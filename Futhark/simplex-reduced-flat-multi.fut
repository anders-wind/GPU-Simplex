-- Implementation of Simplex: reduced, flat representation
--
-- main A b c v = optimal objective value
-- A are the constraint coefficients (flattened m*n length)
-- b are the constraint values (m length)
-- c are the objective coefficients (n length)
-- v is the objective value
--
-- ==
-- compiled input {
--   [[1.0f32, 2.0f32, 3.0f32, 2.0f32, 2.0f32, 5.0f32, 4.0f32, 1.0f32, 2.0f32], [93.44461f32, 71.88667f32, 74.54679f32, 79.33898f32, 75.88172f32, 79.05727f32, 92.16154f32, 57.39885f32, 65.48771f32]] 
--   [[30.0f32,24.0f32,36.0f32], [31.72006f32, 23.99789f32, 47.660904f32]]
--   [[3.0f32,1.0f32,2.0f32], [76.21876f32, 69.50258f32, 46.55636f32]]
--   [0.0f32, 0.0f32]
-- }
-- output { [28.0f32, 23.054108f32] }

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
    let res = map(\(A, b, c, v, l, e) -> 
      if e != (-1)
      then
        let (A,b,c,v) = pivot A b c v l e
        let n = (shape(c))[0]
        let e = entering_variable c
        let l = leaving_variable A b e n
        in (A, b, c, v, l, e)
      else 
        (A, b, c, v, l, e)) instancesEL
    let continue = reduce(\res e -> res || e) (false) (map(\(_,_,_,_,_,e)-> e != (-1)) instancesEL)
    in (res, continue)
	in map(\(_,_,_,v,_,_) -> v) res

let main (As:[][]f32) (bs:[][]f32) (cs:[][]f32) (vs:[]f32) =
  let instances = zip As bs cs vs
  let obj = multi_simplex(instances)
  in obj

