-- Implementation of Simplex: reduced, flat representation
--
-- Only works on regular arrays, i.e. all instances need to be same size
-- main As bs cs = list of optimal objective values
-- As are lists of the constraint coefficients (flattened m*n length)
-- bs are lists of the constraint values (m length)
-- cs are lists of the objective coefficients (n length)
--
-- ==
-- nobench input @tests/flat_test_in.txt
-- output @tests/flat_test_out.txt
--
-- compiled input @tests/flat_gen_test_in.txt
-- output @tests/flat_gen_test_out.txt

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
         else if i == l then A[l*n+j] / A[l*n+e]
         else if j == e then -A[i*n+e] * newAle
         else A[i*n+j] - A[i*n+e] * A[l*n+j] / A[l*n+e]
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
  let (vals,_) =
    reduce
      (\(acc_index,acc_val) (index,vall) ->
        if acc_index < 0
        then
          if vall > 0f32
          then (index,vall)
          else (-1,0f32)
        else (acc_index,acc_val))
    (-1,0f32)
    (zip (iota n) c)
  in vals

let leaving_variable [m] [mxn] (A : [mxn]f32) (b : [m]f32) (e : i32) (n : i32) : i32 =
  let delta = map (\i -> unsafe if A[i*n+e] > 0f32 then b[i]/A[i*n+e] else inf) (iota m)
  let (vals,_) =
    reduce
      (\(acc_index,acc_val) (index,vall) ->
        if acc_val == inf && vall == inf then (-1,inf)
        else if acc_val == inf || vall < acc_val then (index,vall)
        else (acc_index, acc_val))
      (-1,inf)
      (zip (iota m) delta)
  in vals

let simplex [n] [m] [mxn] (A : [mxn]f32) (b : [m]f32) (c : [n]f32) (v : f32) =
  let e = entering_variable c
  let (_,_,_,v,_) = loop (A,b,c,v,e) while e != -1 do
    let l = leaving_variable A b e n
    -- should have a check for if l == -1 here, but it will throw out of
    -- bounds error if not, so that'll do as our "Unbounded" result for now
    let (A,b,c,v) = unsafe pivot A b c v l e
    let e = entering_variable c
    in (A,b,c,v,e)
  in v

let main [h] (As:[h][]f32) (bs:[h][]f32) (cs:[h][]f32) =
  let res = replicate h 0f32
  in loop res for i < h do
    let (A,b,c) = (As[i], bs[i], cs[i])
    let obj = simplex A b c 0f32
    in res with [i] <- obj
