-- Implementation of Simplex
-- ==
-- compiled input {
--   [0, 1, 2]
--   [3 ,4, 5]
--   [ [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--   , [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--   , [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--   , [1.0, 2.0, 3.0, 0.0, 0.0, 0.0]
--   , [2.0, 2.0, 5.0, 0.0, 0.0, 0.0]
--   , [4.0, 1.0, 2.0, 0.0, 0.0, 0.0]]
--   [0.0, 0.0, 0.0, 30.0, 24.0, 36.0]
--   [3.0, 1.0, 2.0, 0.0, 0.0, 0.0]
--   0.0
-- }
-- output { [3.0f32, 0.0f32, -4.0f32, 6.0f32, 9.0f32] }

default(i32)
default(f32)

------------------------
--- Sgm Scan Helpers ---
------------------------

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

-- segmented scan with (+) on floats:
let sgmSumF32 [n] (flg : [n]i32) (arr : [n]f32) : [n]f32 =
  let flgs_vals =
    scan ( \ (f1, x1) (f2,x2) ->
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0.0f32) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals


let index (i:i32) (j:i32) (size:i32) : i32 =
	i * size + j

-- SetContains
let contains [n] (elem:i32)(list:[n]i32) : bool =
	let index = reduce(\res i -> if list[i] == elem then i else res) (-1) (iota n)
	in index != -1

-- SetMinus
let minus [n] (remove:i32)(list:[n]i32) : []i32 =
	filter(\elem -> elem != remove) list

-- SetAdd
let add [n] (new:i32)(list:[n]i32) : []i32 =
	concat [new] list

-- Pivot
let pivot [n] [m] [npm] (N : [n]i32) (B : [m]i32) (A : [npm][npm]f32) (b : [npm]f32) (c : [npm]f32) (v:f32) (l:i32) (e:i32) =
	-- generate the new variables
	let AHat : *[npm][npm]f32 = replicate npm (replicate npm 0f32)
	let bHat = replicate npm 0f32

	-- Compute coefficients of the equation for new basic variables
	let bHat[e] = (b[l]/A[l, e])

	let vals = (map(\j -> if j != e
					then A[l, j] / A[l, e]
					else 1f32/A[l, e])
				N)
	let AHat[e] = scatter (replicate npm 0f32) N vals -- not neccesary to give AHat[e] as argument to scatter since its 0 anyways.


	-- Compute coefficients of the remaining constraints
	let bHat = map(\i -> b[i]-A[i, e] * bHat[e]) (minus l B) -- line 9
	let AHat = map(\i ->
		if contains i B && i != l
		then
			map(\j -> if contains j N && j != e
				then A[i, j] - A[i, e] * AHat[e, j]
				else
					if j == l then -A[i, e] * AHat[e, l] else AHat[i, j]
			) (iota npm)
		else
			AHat[i]
	) (iota npm)

	-- Compute the objective function
	let vHat = v + c[e] * bHat[e] -- line 14
	let cHat = map(\j -> if j != e 
		then c[j] - c[e] * AHat[e, j] 
		else (-c[e]) * AHat[e, l]	) N -- line 15-17

	-- Compute the new sets of basic and non-basic variables.
	let NHat = add l (minus e N)
	let BHat = add e (minus l B)
	in (NHat, BHat, AHat, bHat, cHat, vHat)

-- Simplex
let simplex [n] [m] [npm] (N : [n]i32) (B : [m]i32) (A : [npm][npm]f32) (b : [npm]f32) (c : [npm]f32) (v : f32) =
	let e = reduce(\res j -> if res != -1 then res else if c[j] > 0f32 then j else -1) (-1) N
	let (_,B,_,b,_,v,_) = loop (N,B,A,b,c,v,e) while e != -1 do 
		let delta = map(\i -> if A[i, e] > 0f32 then b[i]/A[i, e] else 1000000f32) B
		let l =
			reduce(\min l -> if min != -1 && delta[l] > delta[min]
				then min
				else l
			) (-1) B
		let (N,B,A,b,c,v) = pivot N B A b c v l e
		let e = reduce(\res j -> if res != -1 then res else if c[j] > 0f32 then j else -1) (-1) N
		in (N,B,A,b,c,v,e)
	in (v, map(\i -> if (contains i B) then b[i] else 0f32) (iota n))

let main [n] [m] [npm] (N : [n]i32) (B : [m]i32) (A : [npm][npm]f32) (b : [npm]f32) (c : [npm]f32) (v:f32) =
  simplex N B A b c v
  