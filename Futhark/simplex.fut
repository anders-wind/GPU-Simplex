-- ASSIGNMENT 2: Flat-Parallel implementation of Sparse Matrix-Vector Multiplication
-- ==
-- compiled input { 
--   [0,   1,     0,   1,    2,    1,   2,    3,    2,   3,   3  ]
--   [2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, 3.0]
--   [2, 3, 3, 2, 1]
--   [2.0, 1.0, 0.0, 3.0]
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
	reduce(\(i, pred) _> pred || i == elem ) elem


-- SetMinus
let minus [n] (remove:i32)(list:[n]i32) : [n-1]i32 = 
	filter(\elem -> elem != remove) list

-- SetAdd
let add [n] (new:i32)(list:[n]i32) : [n+1]i32 = 
	concat new list

-- Pivot
let pivot [n][m] (N : [n]i32) (B : [m]i32) (A [n+m]) (b:[m]i32) (c:[n]i32) (v:i32) : [n] = 
	-- generate the new variables
	let NHat = N
	let BHat = B
	let AHat = A
	let bHat = b
	let cHat = c
	let vHat = v
	-- the algorithm

	-- Compute coefficients of the equation for new basic variables
	let bHat = write bHat (replicate e 1) (replicate (b[l]/a[l][e]) 1) -- write bHat[e]
	let AHat = write AHat (replicate e m, N) 
		(map(\j -> if(j!=e) 
			then A[l][j] / A[l][e] 
			else 1/A[l][e]) 
		N)

	-- Compute coefficients of the remaining constraints
	let bHat = map(\i -> b[i]-A[i][e]*bHat[e]) (minus l B) -- line 9
	let AHat = 
		map(\i j -> if j != e 
			then A[i][j] - A[i][j] * AHat[e][j] 
			else -A[i][e] * AHat[e][j]) 
		(minus l B) (minus e N) -- line 10-12
	-- Compute the objective function
	let vHat = v + c[e] * bHat[e] -- line 14
	let cHat = 
		map(\j -> if j != e 
			then c[j]-c[e] * aHat[e][j] 
			else -c[e] * aHat[e][l]) 
		N -- line 15-17

	-- Compute the new sets of basic and non-basic variables.
	let NHat = add l (minus e N)
	let BHat = add e (minus l B)
	in (NHat, BHat, AHat, bHat, cHat, vHat) 
	

-- Simplex
let simplex [n][m] (N : [n]i32) (B : [m]i32) (A [n+m][n+m]) (b:[m]i32) (c:[n]i32) (v:i32) : [n]:i32 = 
	if reduce(\j res -> c[j] > 0 || res) false N
	then
		let e = reduce(\j e -> if e!=-1 && c[e] > 0 then e else c[j]) (-1) N
		let delta = map(\i -> if A[i][e] > 0 then b[i]/a[i][e] else i32.inf) B
		let l = reduce(\l min -> if min!=-1 && delta[l] > delta[min] then min else l) -1 B
		in simplex (pivot N B A b c v l e) 
	else -- optimal solution  
		map(\i -> if (contains i B) then b[i] else 0) (iota n)


-- One may run with for example:
-- $ futhark-dataset --i32-bounds=0:9999 -g [1000000]i32 --f32-bounds=-7.0:7.0 -g [1000000]f32 --i32-bounds=100:100 -g [10000]i32 --f32-bounds=-10.0:10.0 -g [10000]f32 | ./spMVmult-seq -t /dev/stderr > /dev/null
let main [n] [m] 
         (mat_inds : [n]i32) (mat_vals : [n]f32) 
         (shp : [m]i32) (vct : []f32) : [m]f32 =
  spMatVctMult (zip mat_inds mat_vals) shp vct
