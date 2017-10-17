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
let multipivot [h] (instancesEL:[h]([]f32, []f32, []f32, f32, i32, i32)) : []([]f32, []f32, []f32, f32, i32, i32) =
  let instances = filter(\(A, b, c, v, l, e) -> e != (-1)) instancesEL
  let ns = map(\(A, b, c, v, l, e) -> (shape(c))[0]) instancesEL
  let ms = map(\(A, b, c, v, l, e) -> (shape(b))[0]) instancesEL

  let instances = map(\(A, b, c, v, l, e) index -> (A, b, c, v, l, e, index) (instancesEL iota h)
  let newbs = map(\(A, b, c, v, l, e, i) -> b[l]/A[l*ns[i]+e]) instances

  let iotaMs = map(\i -> (i/m,i%m)) iota (???)
  let bHats = map(\(i,j) -> 
    let specificA = #1 instancesEL[i]
    let specificB = #2 instancesEL[i]
    in unsafe if j == l then newbs[i] else b[i]-A[i*n+e]*newbs[i]
    ) iotaMs

  in instancesEL

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

