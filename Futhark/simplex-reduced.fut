-- Implementation of Simplex: reduced, nested representation
--
-- main A b c v = optimal objective value
-- A are the constraint coefficients (m rows, n columns)
-- b are the constraint values (m length)
-- c are the objective coefficients (n length)
-- v is the objective value
--
-- ==
-- compiled input {
--   [[1.0f32, 2.0f32, 3.0f32], [2.0f32, 2.0f32, 5.0f32], [ 4.0f32, 1.0f32, 2.0f32]]
--   [30.0f32,24.0f32,36.0f32]
--   [3.0f32,1.0f32,2.0f32]
--   0.0f32
-- }
-- output { 28.0f32 }

default(i32)
default(f32)

-- hack
let inf : f32 = 1000000f32

let pivot [n] [m] (A : [m][n]f32) (b : [m]f32) (c : [n]f32) (v:f32) (l:i32) (e:i32) =
  -- new constraint values
  let newb = b[l]/A[l,e]
  let bHat =
    map
      (\i -> if i == l then newb else b[i]-A[i,e]*newb)
      (iota m)

  -- new constraint coefficients
  let newArowc = 1f32/A[l,e]
  let newArow =
    map
      (\i -> if i == e then newArowc else A[l,i]/A[l,e])
      (iota n)
  let AHat =
    map
     (\i ->
        if i == l
        then newArow
        else map
          (\j ->
            if j == e
            then -A[i,e] * newArowc -- newArow[j]
            else A[i,j] - A[i,e] * newArowc
          )
          (iota n)
     )
     (iota m)

  -- new objective function
  let vHat = v + c[e] * newb -- line 14
  let cHat =
    map
      (\i -> if i == e then -c[e]*AHat[l,i] else c[i]-c[e]*AHat[l,i])
      (iota n)

  in (AHat, bHat, cHat, vHat)

let entering_variable [n] (c : [n]f32) : i32 =
  reduce
    (\res j -> if res != -1 then res else if c[j] > 0f32 then j else -1)
    (-1)
    (iota n)

let leaving_variable [n] [m] (A : [m][n]f32) (b : [m]f32) (e : i32) : i32 =
  let delta = map (\Arow bcon -> if Arow[e] > 0f32 then bcon/Arow[e] else inf) A b
  in reduce
       (\min l -> if min != -1 && delta[l] > delta[min] then min else l)
       (-1)
       (iota m)

let simplex [n] [m] (A : [m][n]f32) (b : [m]f32) (c : [n]f32) (v : f32) =
  let e = entering_variable c
  let (_,b,_,v,_) = loop (A,b,c,v,e) while e != -1 do
    let l = leaving_variable A b e
    -- should have a check for if l == -1 here, but it will throw out of
    -- bounds error if not, so that'll do as our "Unbounded" result for now
    let (A,b,c,v) = pivot A b c v l e
    let e = entering_variable c
    in (A,b,c,v,e)
  in (v, b)

let main [n] [m] (A : [m][n]f32) (b : [m]f32) (c : [n]f32) (v : f32) : f32 =
  let (obj, _) = simplex A b c v in obj
