-- Implementation of Simplex: reduced, flat representation
--
-- main As bs cs = lists of optimal objective values
-- As are lists of the constraint coefficients (flattened m*n length)
-- bs are lists of the constraint values (m length)
-- cs are lists of the objective coefficients (n length)
--
-- ==
-- nobench input @tests/multi_test_in.txt
-- output @tests/multi_test_out.txt
--
-- compiled input @tests/multi_gen_test_in.txt
-- output @tests/multi_gen_test_out.txt

default(i32)
default(f32)

-- hack
let inf : f32 = 1000000f32

-- segmented scan for first positive number in flat cs with c_inds
let sgmLowestI32 [n] (flg : [n]i32) (arr : [n]i32) (ins_inds_n:[n]i32) (cs:[]f32) (c_inds:[]i32) : [n]i32 =
  let flgs_vals =
    scan ( \ (f1, res, _) (f2, j, ins_i) ->
            let f = f1 | f2 in
            if f2 > 0 then
              if cs[c_inds[ins_i] + j] > 0f32
                then (f, j, 0)
                else (f,-1, 0)
            else if res != (-1)
              then (f,res, 0)
              else if cs[c_inds[ins_i] + j] > 0f32
                then (f, j, 0)
                else (f,-1, 0))
    (0,-1, 0) (zip flg arr ins_inds_n)
  let (_, vals,_) = unzip flgs_vals
  in vals

-- segmented scan for predicate all infinite.
-- Returns 0 on all inf, 1 otherwise
let sgmAllInfI32 [n] (flg : [n]i32) (arr : [n]f32) : [n]f32 =
  let flgs_vals =
    scan ( \ (f1, acc) (f2, b) ->
            let f = f1 | f2 in
            if f2 > 0 then
              if b == inf
                then (f, 0f32)
                else (f, 1f32)
            else if b == inf
              then (f, acc)
              else (f, 1f32))
    (0, 0f32) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

-- segmented scan which finds index with lowest fraction in deltas.
let sgmMinFractI32 [m] (flg : [m]i32) (arr : [m]i32) (ins_inds_m: []i32) (deltas : []f32) (delta_inds : []i32) : []i32 =
  let flgs_vals =
    scan ( \ (f1, min, _) (f2, l, ins_i) ->
            let f = f1 | f2 in
            if f2 > 0
              then (f, l, 0) -- always pick first element in sgm
              else if deltas[delta_inds[ins_i] + l] > deltas[delta_inds[ins_i] + min]
                then (f, min, 0)
                else (f, l, 0))
         (0, -1, 0) (zip flg arr ins_inds_m)
  let (_, vals, _) = unzip flgs_vals
  in vals

-- segmented scan with (+) on integers:
let sgmSumI32 [n] (flg : [n]i32) (arr : [n]i32) : [n]i32 =
  let flgs_vals =
    scan ( \ (f1, x1) (f2,x2) ->
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let sgm_scan_inc_to_exc [n] (flg : [n]i32) (arr:[n]i32) : [n]i32 =
  map (\i -> unsafe if (flg[i]!=0) then 0 else arr[i-1]) (iota n)

let scan_inc_to_exc [n] (arr:[n]i32) : [n]i32 =
  map (\i -> unsafe if (i==0) then 0 else arr[i-1]) (iota n)

let entering_variables (flag_n:[]i32) (iota_ns:[]i32) (ns_scan:[]i32) (ins_inds_n: []i32) (cs:[]f32) (c_inds:[]i32): []i32 =
  let e_scans = sgmLowestI32 flag_n iota_ns ins_inds_n cs c_inds
  let es      = map(\i -> e_scans[i-1]) ns_scan
  in es

let leaving_variables (flag_m:[]i32) (iota_ms:[]i32) (ms_scan:[]i32) (ins_inds_m : []i32) (ns:[]i32) (As:[]f32) (A_inds:[]i32) (bs:[]f32) (b_inds:[]i32) (es:[]i32) : []i32 =
  let deltas  =
    map (\(ins_i, i) -> unsafe if As[A_inds[ins_i] + i*ns[ins_i] + es[ins_i]] > 0f32
      then bs[b_inds[ins_i] + i]/As[A_inds[ins_i] + i*ns[ins_i] + es[ins_i]]
      else inf)
    (zip ins_inds_m iota_ms)
  -- let inf_scan= sgmAllInfF32 flag_m deltas
  -- let infs    = map(\i -> inf_scan[i]) ms_scan
  let l_scans = sgmMinFractI32 flag_m iota_ms ins_inds_m deltas ins_inds_m -- maybe delta_inds is something else
  let ls      = map(\i -> l_scans[i-1]) ms_scan
  in ls

-- input is list of (A,b,c,v,l,e)
let multi_pivot [h] (As:[]f32) (bs:[]f32) (cs:[]f32) (vs:[h]f32) (es:[h]i32) (ls:[h]i32) (ms:[h]i32) (ns:[h]i32) =
  let mns = map (*) ns ms

  let A_inds = scan_inc_to_exc (scan (+) 0 mns)
  let b_inds = scan_inc_to_exc (scan (+) 0 ms)
  let c_inds = scan_inc_to_exc (scan (+) 0 ns)

  let instance_iota = iota h

  let newbs =
    map(\i-> unsafe if es[i] != (-1)
      then bs[b_inds[i] + ls[i]]/As[A_inds[i] + ls[i]*ns[i] + es[i]]
      else 0f32
    ) instance_iota

  -- we should reuse this
  let indsM     = scan_inc_to_exc (scan (+) 0 ms)
  let sizeM     = last indsM + last ms
  let flagM     = scatter (replicate sizeM 0) indsM (replicate h 1)
  let iotaMs    = sgm_scan_inc_to_exc flagM (sgmSumI32 flagM (replicate sizeM 1))

  let indsN     = scan_inc_to_exc (scan (+) 0 ns)
  let sizeN     = last indsN + last ns
  let flagN     = scatter (replicate sizeN 0) indsN (replicate h 1)
  let iotaNs    = sgm_scan_inc_to_exc flagN (sgmSumI32 flagN (replicate sizeN 1))

  let indsMxN   = scan_inc_to_exc (scan (+) 0 mns)
  let sizeMxN   = last indsMxN + last mns
  let flagMxN   = scatter (replicate sizeMxN 0) indsMxN (replicate h 1)
  let iotaMxNs  = sgm_scan_inc_to_exc flagMxN (sgmSumI32 flagMxN (replicate sizeMxN 1))

  let instancesIndsM    = map (\x -> x-1) (scan (+) 0 flagM)
  let instancesIndsN    = map (\x -> x-1) (scan (+) 0 flagN)
  let instancesIndsMxN  = map (\x -> x-1) (scan (+) 0 flagMxN)

  let bHats =
    map(\ins_i i ->
      unsafe
      let n     = ns[ins_i]
      let e     = es[ins_i]
      let l     = ls[ins_i]
      let newb  = newbs[ins_i]
      in if e != (-1) then
        if i == l then newb else bs[b_inds[ins_i] + i] - As[A_inds[ins_i] + i*n + e]*newb
      else
        bs[b_inds[ins_i] + i]
    ) instancesIndsM iotaMs

  let newAles =
    map(\i -> unsafe
      if es[i]!=(-1)
        then 1f32/As[A_inds[i] + ls[i]*ns[i] + es[i]]
        else 0f32
    ) instance_iota

  let AHats =
    map(\ins_i ind ->
      unsafe
      let n       = ns[ins_i]
      let e       = es[ins_i]
      let l       = ls[ins_i]
      let newAle  = newAles[ins_i]
      let (i, j)  = (ind / n, ind % n)
      in if e != (-1)
      then
         if i == l && j == e then newAle
         else if i == l then As[A_inds[ins_i] + i*n + j]/As[A_inds[ins_i] + i*n + e]
         else if j == e then -As[A_inds[ins_i] + i*n + e] * newAle
         else As[A_inds[ins_i] + i*n + j] - As[A_inds[ins_i] + i*n + e] * As[A_inds[ins_i] + l*n + j]/As[A_inds[ins_i] + l*n + e]
      else As[A_inds[ins_i] + i*n + j]
    ) instancesIndsMxN iotaMxNs

  let vHats =
    map(\i -> unsafe if es[i] != (-1)
      then vs[i] + cs[c_inds[i] + es[i]] * newbs[i]
      else vs[i]
    ) instance_iota

  let cHats =
    map(\ins_i i ->
      unsafe
      let n = ns[ins_i]
      let e = es[ins_i]
      let l = ls[ins_i]
      in if e != (-1)
      then
        if i == e
          then -cs[c_inds[ins_i] + e]*AHats[A_inds[ins_i] + l*n + i]
          else cs[c_inds[ins_i] + i]-cs[c_inds[ins_i] + e]*AHats[A_inds[ins_i] + l*n + i]
      else cs[c_inds[ins_i] + i]
    ) instancesIndsN iotaNs

  in (AHats, bHats, cHats, vHats)

let multi_simplex [h] (As:[]f32) (bs:[]f32) (cs:[]f32) (ms:[h]i32) (ns:[h]i32) =
  let vs = replicate h 0f32 --  assume instances start with 0 score
  let mns = map (*) ns ms

  let A_inds = scan_inc_to_exc (scan (+) 0 mns)
  let b_inds = scan_inc_to_exc (scan (+) 0 ms)
  let c_inds = scan_inc_to_exc (scan (+) 0 ns)

  -- es
  let ns_scan = scan (+) 0 ns
  let inds_n  = scan_inc_to_exc ns_scan
  let size_n  = last inds_n + last ns
  let flag_n  = scatter (replicate size_n 0) inds_n (replicate h 1)
  let iota_ns = sgm_scan_inc_to_exc flag_n (sgmSumI32 flag_n (replicate size_n 1))
  let ins_inds_n  = map (\x -> x-1) (scan (+) 0 flag_n)

  let es      = entering_variables flag_n iota_ns ns_scan ins_inds_n cs c_inds
  -- es end

  -- ls
  let ms_scan = scan (+) 0 ms
  let inds_m  = scan_inc_to_exc ms_scan
  let size_m  = last inds_m + last ms
  let flag_m  = scatter (replicate size_m 0) inds_m (replicate h 1)
  let iota_ms = sgm_scan_inc_to_exc flag_m (sgmSumI32 flag_m (replicate size_m 1))
  let ins_inds_m  = map (\x -> x-1) (scan (+) 0 flag_m)

  let ls      = leaving_variables flag_m iota_ms ms_scan ins_inds_m ns As A_inds bs b_inds es
  -- ls end

  let continue = reduce (\res e -> res || e) (false) (map(\e -> e != (-1)) es)
  let (_,_,_,vs,_,_,_)    = loop (As, bs, cs, vs, es, ls, continue) while continue do
    let (As, bs, cs, vs)  = multi_pivot As bs cs vs es ls ms ns
    let es        = entering_variables flag_n iota_ns ns_scan ins_inds_n cs c_inds
    let ls        = leaving_variables flag_m iota_ms ms_scan ins_inds_m ns As A_inds bs b_inds es
    let continue  = reduce (\res e -> res || e) (false) (map(\e -> e != (-1)) es)
    in (As,bs,cs,vs,es,ls, continue)
	in vs

-- As[h][n][m], bs[h][m], cs[h][n]
let main [h] (As:[]f32) (bs:[]f32) (cs:[]f32) (ms:[h]i32) (ns:[h]i32)  =
  let obj = multi_simplex As bs cs ms ns
  in obj
