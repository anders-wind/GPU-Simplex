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
-- output { 28.0f32 [3.0f32, 0.0f32, -4.0f32, 6.0f32, 9.0f32] }

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
let pivot [n] [m] [npm] [npmsq] (N : [n]i32) (B : [m]i32) (A : [npmsq]f32) (b : [npm]f32) (c : [npm]f32) (v:f32) (l:i32) (e:i32) =
	-- generate the new variables
	let AHat = replicate npmsq 0f32
	let bHat = replicate npm 0f32
	let cHat = replicate npm 0f32

	-- Compute coefficients of the equation for new basic variables
	let be = (b[l]/A[l*npm + e])
	let bHat[e] = be

	let vals = (map(\j -> unsafe if j != e
					then A[l*npm + j] / A[l*npm + e]
					else 1f32/A[l*npm + e])
				N)

	let AHat = scatter AHat (map(\i -> e*npm + i) N) vals

	-- Compute coefficients of the remaining constraints
	let bHat = scatter bHat (minus l B) (map(\i -> unsafe (b[i]-A[i*npm + e] * be)) (minus l B)) -- line 9

	let AHat = 
		map(\full ->
			unsafe
			let i = full / npm
			let j = full % npm
			in  if contains i B && i != l
				then
					if contains j N && j != e
						then A[i*npm + j] - A[i*npm + e] * AHat[e*npm + j]
						else
							if j == l then -A[i*npm + e] * AHat[e*npm + l] else AHat[i*npm + j]
				else
					AHat[i*npm+j])
		(iota npmsq)

	-- Compute the objective function
	let vHat = v + c[e] * bHat[e] -- line 14
	let cHat = scatter cHat (minus e N) 
		(map(\j -> unsafe (c[j] - c[e] * AHat[e*npm + j])) (minus e N))
	
	let cHat[l] = (-c[e]) * AHat[e*npm + l]

	-- Compute the new sets of basic and non-basic variables.
	let NHat = add l (minus e N)
	let BHat = add e (minus l B)
	in (NHat, BHat, AHat, bHat, cHat, vHat)


let findLeaving [m] [npm] [npmsq] (B : [m]i32) (A : [npmsq]f32) (b : [npm]f32) (e:i32) : i32 =
	let delta = map(\i -> unsafe if A[i*npm + e] > 0f32 then b[i]/A[i*npm + e] else 1000000f32) (iota npm)
	in 	reduce(\min l -> unsafe if min != -1 && delta[l] > delta[min]
			then min
			else l
		) (-1) B

let findEntering [n] [npm] (N : [n]i32) (c : [npm]f32) : i32 = 
	reduce(\res j -> unsafe if res != -1 then res else if c[j] > 0f32 then j else -1) (-1) N

-- Simplex
let simplex [n] [m] [npm] [npmsq] (N : [n]i32) (B : [m]i32) (A : [npmsq]f32) (b : [npm]f32) (c : [npm]f32) (v : f32) =
	let e = findEntering N c
	let (_,B,_,b,_,v,_) = loop (N,B,A,b,c,v,e) while e != -1 do 
		let l = findLeaving B A b e
		in if l == -1
		then -- unbounded
			(N,B,A,b,c,v,-1) 
		else
			let (N,B,A,b,c,v) = pivot N B A b c v l e
			let e = findEntering N c
			in (N,B,A,b,c,v,e)
	in (v, map(\i -> unsafe if (contains i B) then b[i] else 0f32) (iota n))

let main [n] [m] [npm] (N : [n]i32) (B : [m]i32) (A : [npm][npm]f32) (b : [npm]f32) (c : [npm]f32) (v:f32) =
	let flatA = 
		map(\full -> unsafe
			let i = full / npm
			let j = full % npm
			in A[i, j]) (iota (npm*npm))
	in simplex N B flatA b c v
