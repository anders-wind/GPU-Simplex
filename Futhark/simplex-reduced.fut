-- Implementation of Simplex: reduced, nested representation
--
-- main As bs cs vs = list of optimal objective values
-- As are lists of the constraint coefficients (m rows, n columns)
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

let pivot [n] [m] (A : [m][n]f32) (b : [m]f32) (c : [n]f32) (v:f32) (l:i32) (e:i32) =
  -- new constraint values
  let newb = b[l]/A[l,e]
  let bHat =
    map
      (\i -> unsafe if i == l then newb else b[i]-A[i,e]*newb)
      (iota m)

  -- new constraint coefficients
  let newArowc = 1f32/A[l,e]
  let newArow =
    map
      (\i -> unsafe if i == e then newArowc else A[l,i]/A[l,e])
      (iota n)
  let AHat =
    map
     (\i ->
        if i == l
        then newArow
        else map
          (\j ->
            unsafe
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
      (\i -> unsafe if i == e then -c[e]*AHat[l,i] else c[i]-c[e]*AHat[l,i])
      (iota n)

  in (AHat, bHat, cHat, vHat)

let entering_variable [n] (c : [n]f32) : i32 =
  reduce
    (\res j -> unsafe if res != -1 then res else if c[j] > 0f32 then j else -1)
    (-1)
    (iota n)

let leaving_variable [n] [m] (A : [m][n]f32) (b : [m]f32) (e : i32) : i32 =
  let delta = map (\Arow bcon -> unsafe if Arow[e] > 0f32 then bcon/Arow[e] else inf) A b
  in reduce
       (\min l -> unsafe if min != -1 && delta[l] > delta[min] then min else l)
       (-1)
       (iota m)

let swap [mpn] (p : *[mpn]i32) (e : i32) (l : i32) =
  let tmp = p[e] in let p[e] = p[l] in let p[l] = tmp in p

let extract [m] (p : [m]i32) (b : [m]f32) (n:i32) =
  let mapped = map (\i -> if i < m then i else -1) p
  in scatter (replicate n 0f32) mapped b

let simplex [n] [m] (A : [m][n]f32) (b : [m]f32) (c : [n]f32) (v : f32) =
  let p = iota (m+n) -- b variables on n to m-1.
  let e = entering_variable c
  let (_,b,_,v,_,p) = loop (A,b,c,v,e,p) while e != -1 do
    let l = leaving_variable A b e
    -- should have a check for if l == -1 here, but it will throw out of
    -- bounds error if not, so that'll do as our "Unbounded" result for now
    let p = swap p e (l+n)
    let (A,b,c,v) = pivot A b c v l e
    let e = entering_variable c
    in (A,b,c,v,e,p)
  in (v, extract (p[n:m+n]) b n)

let main (As:[][][]f32) (bs:[][]f32) (cs:[][]f32) (vs:[]f32) =
  let instances = zip As bs cs vs
  in map (\(A,b,c,v) -> let (obj,_) = simplex A b c v in obj) instances
