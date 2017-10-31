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
    scan ( \ (f1, res, _) (f2, j, ins_i) -> unsafe
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
    scan ( \ (f1, min, _) (f2, l, ins_i) -> unsafe
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

-- indices of a flattened array
type flat_index =
  { sizes: []i32            -- array of sizes of each segment (aka shape) [3,2,5,...]
  , segment_indices: []i32  -- array of flat indices of each segment start [0,3,5,...]
  , segment_iotas: []i32    -- array of iotas for each segment [0,1,2,0,1,0,1,...]
  , segment_numbers: []i32  -- array of the instance number for each segment [0,0,0,1,1,2,2,...]
  }

let make_flat_index [h] (sizes: [h]i32) =
  let sizes_scan = scan (+) 0 sizes
  let segment_indices = scan_inc_to_exc sizes_scan
  let full_size = last sizes_scan
  let flags = scatter (replicate full_size 0) segment_indices (replicate h 1)
  let segment_iotas = sgm_scan_inc_to_exc flags (sgmSumI32 flags (replicate full_size 1))
  let segment_numbers = map (\x -> x-1) (scan (+) 0 flags)
  in { sizes=sizes, segment_indices=segment_indices, segment_iotas=segment_iotas, segment_numbers=segment_numbers }

let entering_variables (flag_n:[]i32) (iota_ns:[]i32) (ns_scan:[]i32) (ins_inds_n: []i32) (cs:[]f32) (c_inds:[]i32): []i32 =
  let e_scans = sgmLowestI32 flag_n iota_ns ins_inds_n cs c_inds
  let es      = map(\i -> unsafe e_scans[i-1]) ns_scan
  in es

let leaving_variables (flag_m:[]i32) (iota_ms:[]i32) (ms_scan:[]i32) (ins_inds_m : []i32) (ns:[]i32) (As:[]f32) (A_inds:[]i32) (bs:[]f32) (b_inds:[]i32) (es:[]i32) : []i32 =
  let deltas  =
    map (\(ins_i, i) -> unsafe if As[A_inds[ins_i] + i*ns[ins_i] + es[ins_i]] > 0f32
      then bs[b_inds[ins_i] + i]/As[A_inds[ins_i] + i*ns[ins_i] + es[ins_i]]
      else inf)
    (zip ins_inds_m iota_ms)
  -- let inf_scan= sgmAllInfF32 flag_m deltas
  -- let infs    = map(\i -> inf_scan[i]) ms_scan
  let l_scans = sgmMinFractI32 flag_m iota_ms ins_inds_m deltas b_inds
  let ls      = map(\i -> unsafe l_scans[i-1]) ms_scan
  in ls

let multi_pivot [h]
      (As:[]f32) (bs:[]f32) (cs:[]f32) (vs:[h]f32)
      (es:[h]i32) (ls:[h]i32)
      (m_index: flat_index) (n_index: flat_index) (mxn_index: flat_index) =

  let instance_iota = iota h

  let {sizes=_, segment_indices=b_inds, segment_iotas=iotaMs, segment_numbers=instancesIndsM} = m_index
  let {sizes=ns, segment_indices=c_inds, segment_iotas=iotaNs, segment_numbers=instancesIndsN} = n_index
  let {sizes=_, segment_indices=A_inds, segment_iotas=iotaMxNs, segment_numbers=instancesIndsMxN} = mxn_index

  let newbs =
    map(\i-> unsafe if es[i] != (-1)
      then bs[b_inds[i] + ls[i]]/As[A_inds[i] + ls[i]*ns[i] + es[i]]
      else 0f32
    ) instance_iota

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

  let m_index = make_flat_index ms
  let n_index = make_flat_index ns
  let mxn_index = make_flat_index (map (*) ms ns)

  let A_inds = #segment_indices mxn_index
  let b_inds = #segment_indices m_index
  let c_inds = #segment_indices n_index

  -- es
  let ns_scan = scan (+) 0 ns
  let inds_n  = scan_inc_to_exc ns_scan
  let size_n  = last ns_scan
  let flag_n  = scatter (replicate size_n 0) inds_n (replicate h 1)
  let iota_ns = #segment_iotas n_index
  let ins_inds_n  = #segment_numbers n_index

  let es      = entering_variables flag_n iota_ns ns_scan ins_inds_n cs c_inds
  -- es end

  -- ls
  let ms_scan = scan (+) 0 ms
  let inds_m  = scan_inc_to_exc ms_scan
  let size_m  = last ms_scan
  let flag_m  = scatter (replicate size_m 0) inds_m (replicate h 1)
  let iota_ms = #segment_iotas m_index
  let ins_inds_m = #segment_numbers m_index

  let ls      = leaving_variables flag_m iota_ms ms_scan ins_inds_m ns As A_inds bs b_inds es
  -- ls end

  let continue = reduce (\res e -> res || e) (false) (map(\e -> e != (-1)) es)
  let (_,_,_,vs,_,_,_)    = loop (As, bs, cs, vs, es, ls, continue) while continue do
    let (As, bs, cs, vs)  = multi_pivot As bs cs vs es ls m_index n_index mxn_index
    let es        = entering_variables flag_n iota_ns ns_scan ins_inds_n cs c_inds
    let ls        = leaving_variables flag_m iota_ms ms_scan ins_inds_m ns As A_inds bs b_inds es
    let continue  = reduce (\res e -> res || e) (false) (map(\e -> e != (-1)) es)
    in (As,bs,cs,vs,es,ls, continue)
  in vs

-- As[h][n][m], bs[h][m], cs[h][n]
let main [h] (As:[]f32) (bs:[]f32) (cs:[]f32) (ms:[h]i32) (ns:[h]i32)  =
  multi_simplex As bs cs ms ns
