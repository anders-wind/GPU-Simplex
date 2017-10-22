-- Implementation of Simplex: reduced, flat representation
--
-- main As bs cs vs = lists of optimal objective values
-- As are lists of the constraint coefficients (flattened m*n length)
-- bs are lists of the constraint values (m length)
-- cs are lists of the objective coefficients (n length)
-- vs are lists of the objective values
--
-- ==
-- compiled input @tests/test_in.txt
-- output @tests/test_out.txt

default(i32)
default(f32)

-- hack
let inf : f32 = 1000000f32

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

let entering_variable [n] (c : [n]f32) : i32 =
  reduce
    (\res j -> unsafe if res != -1 then res else if c[j] > 0f32 then j else -1)
    (-1)
    (iota n)

let leaving_variable [m] [mxn] (A : [mxn]f32) (b : [m]f32) (e : i32) (n : i32) : i32 =
  let delta = map (\i -> unsafe if A[i*n+e] > 0f32 then b[i]/A[i*n+e] else inf) (iota m)
  in reduce
       (\min l -> unsafe if min != -1 && delta[l] > delta[min] then min else l)
       (-1)
       (iota m)

let pivot [n] [m] [mxn] (A : [mxn]f32) (b : [m]f32) (c : [n]f32) (v:f32) (l:i32) (e:i32) =
  -- new constraint values
  let newb = b[l]/A[l*n+e]
  let bHat =
    map
      (\i -> unsafe if i == l then newb else b[i]-A[i*n+e]*newb)
      (iota m)

  -- new constraint coefficients
  let newAle = 1f32/A[l*n+e]
  let AHat =
    map
      (\ind ->
         unsafe
         let (i,j) = (ind / n, ind % n)
         in if i == l && j == e then newAle
         else if i == l then A[i*n+j] / A[i*n+e]
         else if j == e then -A[i*n+e] * newAle
         else A[i*n+j] - A[i*n+e] * newAle
      )
      (iota (m*n))

  -- new objective function
  let vHat = v + c[e] * newb
  let cHat =
    map
      (\i -> unsafe if i == e then -c[e]*AHat[l*n+i] else c[i]-c[e]*AHat[l*n+i])
      (iota n)

  in (AHat, bHat, cHat, vHat)

-- input is list of (A,b,c,v,l,e)
let multi_pivot [h] (instancesEL:[h]([]f32, []f32, []f32, f32, i32, i32)) : [h]([]f32, []f32, []f32, f32, i32, i32) =
  -- let instances = filter(\(A, b, c, v, l, e) -> e != (-1)) instancesEL
  let ns  = map(\(_, _, c, _, _, _) -> (shape(c))[0]) instancesEL
  let ms  = map(\(_, b, _, _, _, _) -> (shape(b))[0]) instancesEL
  let mns = map(\n m -> n*m) ns ms

  let instances = map(\((A, b, c, v, l, e), index) -> (A, b, c, v, l, e, index)) (zip instancesEL (iota h))
  let newbs = map(\(A, b, _, _, l, e, i) -> b[l]/A[l*ns[i]+e]) instances

  -- maybe not do concat

  let indsM     = scan (+) 0 ms
  let sizeM     = last indsM + last ms
  let flagM     = scatter (replicate sizeM 0) indsM (replicate h 1) 
  let iotaMs    = sgmSumI32 flagM (replicate sizeM 1)
  
  let indsN     = scan (+) 0 ns
  let sizeN     = last indsN + last ns
  let flagN     = scatter (replicate sizeN 0) indsN (replicate h 1) 
  let iotaNs    = sgmSumI32 flagN (replicate sizeN 1)

  let indsMxN   = scan (+) 0 mns
  let sizeMxN   = last indsMxN + last mns
  let flagMxN   = scatter (replicate sizeMxN 0) indsMxN (replicate h 1) 
  let iotaMxNs  = sgmSumI32 flagMxN (replicate sizeMxN 1)

  let instancesIndsM    = scan (+) 0 flagM
  let instancesIndsN    = scan (+) 0 flagN
  let instancesIndsMxN  = scan (+) 0 flagMxN

  let bHats = 
    map(\instanceInd i -> 
      unsafe
      let A = #1 instancesEL[instanceInd]
      let b = #2 instancesEL[instanceInd]
      let l = #5 instancesEL[instanceInd]
      let e = #6 instancesEL[instanceInd]
      let n = ns[i]
      let newb = newbs[i]
      in if e != (-1) then
        if i == l then newb else b[i]-A[i*n+e]*newb
      else
        b[i]
    ) instancesIndsM iotaMs

  let newAles = map(\(A, _, _, _, l, e, i) -> 1f32/A[l*ns[i]+e]) instances

  let AHats =
    map(\instanceInd ind ->
      unsafe
      let A = #1 instancesEL[instanceInd]
      let l = #5 instancesEL[instanceInd]
      let e = #6 instancesEL[instanceInd]
      let n = ns[instanceInd]
      let newAle = newAles[instanceInd]
      let (i,j) = (ind / n, ind % n)
      in if e != (-1)
      then
         if i == l && j == e then newAle
         else if i == l then A[i*n+j] / A[i*n+e]
         else if j == e then -A[i*n+e] * newAle
         else A[i*n+j] - A[i*n+e] * newAle
      else A[i*n+j]
    ) instancesIndsMxN iotaMxNs

  let vHats = map(\(_, _, c, v, _, e, i) -> if e!=(-1) then v + c[e] * newbs[i] else v) instances

  let cHats = 
    map(\instanceInd i -> 
      unsafe
      let c = #3 instancesEL[instanceInd]
      let l = #5 instancesEL[instanceInd]
      let e = #6 instancesEL[instanceInd]
      let n = ns[instanceInd]
      in if e != (-1)
      then
        if i == e then -c[e]*AHats[instanceInd*h*n+l*n+i] else c[i]-c[e]*AHats[instanceInd*h + l*n+i]
      else c[i]
    ) instancesIndsN iotaNs

  in map(\i -> 
    let A = AHats[i*h*ns[i]:i*h*ns[i]+ns[i]*ms[i]]
    let b = bHats[i*h:i*h+ms[i]]
    let c = cHats[i*h:i*h+ns[i]]
    let v = vHats[i]
    let e = #6 instancesEL[i]
    in if e != (-1)
    then
      let e = entering_variable c
      let l = leaving_variable A b e ns[i]
      in (A,b,c,v,l,e)
    else
      let l = #5 instancesEL[i]
      in (A,b,c,v,l,e)
    ) (iota h)

let simplex [n] [m] [mxn] (A : [mxn]f32) (b : [m]f32) (c : [n]f32) (v : f32) =
  let e = entering_variable c
  let (_,b,_,v,_) = loop (A,b,c,v,e) while e != -1 do
    let l = leaving_variable A b e n
    -- should have a check for if l == -1 here, but it will throw out of
    -- bounds error if not, so that'll do as our "Unbounded" result for now
    let (A,b,c,v) = pivot A b c v l e
    let e = entering_variable c
    in (A,b,c,v,e)
  in (v, b)

let multi_simplex (instances : []([]f32, []f32, []f32, f32)) : []f32 = 
	let instancesEL = map(\(A, b, c, v) -> 
    let n = (shape(c))[0]
    let e = entering_variable c
    let l = leaving_variable A b e n
    in (A, b, c, v, l, e)) instances
  let continue = reduce(\res e -> res || e) (false) (map(\(_,_,_,_,_,e)-> e != (-1)) instancesEL)
  let (res, _) = loop (instancesEL, continue) while continue do
    let res = multi_pivot instancesEL
    let continue = reduce(\res e -> res || e) (false) (map(\(_,_,_,_,_,e)-> e != (-1)) instancesEL)
    in (res, continue)
	in map(\(_,_,_,v,_,_) -> v) res

let main (As:[][][]f32) (bs:[][]f32) (cs:[][]f32) (vs:[]f32) =
  let flatAs =
    map
      (\a ->
        let sh = shape a
        let m = sh[0]
        let n = sh[1]
        in map (\i -> a[i / n, i % n]) (iota (m*n))
      )
      As
  let instances = zip flatAs bs cs vs
  let obj = multi_simplex(instances)
  in obj

