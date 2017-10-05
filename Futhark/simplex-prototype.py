# python prototype of simplex algorithm (CLRS)

def pivot(N,B,A,b,c,v,l,e):
    size = len(N) + len(B)
    A_ = [[0 for i in range(0,size)] for i in range(0, size)]
    b_ = [0 for i in range(0, size)]
    c_ = [0 for i in range(0,size)]

    # constraint coefficients for entering variable
    b_[e] = b[l] / A[l][e]
    for j in N:
        if j != e:
            A_[e][j] = A[l][j] / A[l][e]
    A_[e][l] = 1.0 / A[l][e]

    # constraint coefficients for remaining variables
    for i in B:
        if i != l:
            b_[i] = b[i] - A[i][e] * b_[e]
            for j in N:
                if j != e:
                    A_[i][j] = A[i][j] - A[i][e] * A_[e][j]
            A_[i][l] = -A[i][e] * A_[e][l]

    v_ = v + c[e]*b_[e]

    # objective function
    for j in N:
        if j != e:
            c_[j] = c[j] - c[e]*A_[e][j]
    c_[l] = -c[e] * A_[e][l]

    # new basic and nonbasic variables
    N_ = N + [l]
    N_.remove(e)
    B_ = B + [e]
    B_.remove(l)
    return (N_, B_, A_, b_, c_, v_)

def get_entering_variable(c, size):
    n = next((i for i in range(0,size) if c[i] > 0), None)
    return n

def get_leaving_variable(B, A, b, evar):
    coefs = map(lambda i: (i, b[i] / A[i][evar]) if A[i][evar] > 0 else None, B)
    coefs = filter(lambda (_,x): x != None, coefs)
    if coefs == []:
        return None
    else:
        return min(coefs, key=lambda x:x[1])[0]

# assume feasible basic solution
# N is a list of n indices
# B is a list of m indices
# A is a matrix of (m+n)*(m+n) elements
# b is a list of m+n elements (corresponding to constraint values, 0 for non-basic)
# c is a list of m+n elements (corresponding to objective coeffs, 0 for basic)
# v is a scalar, objective constant

def simplex(N, B, A, b, c, v):
    size = len(N) + len(B)
    evar = get_entering_variable(c, size)
    if evar == None:
        obj = sum(b[i] * c[i] for i in B)
        return(b, v)
    lvar = get_leaving_variable(B,A,b,evar)
    if lvar == None:
        print "Unbounded"
        return
    (N_, B_, A_, b_, c_, v_) = pivot(N,B,A,b,c,v,lvar,evar)
    return simplex(N_, B_, A_, b_, c_, v_)

def main():
    print ("Running example ... expected obj value: 28.0")
    N = [0, 1, 2]
    B = [3, 4, 5]

    # a_ij = constraint i, nonbasic variable coefficient j
    A = [ [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        , [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        , [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        , [1.0, 2.0, 3.0, 0.0, 0.0, 0.0] # 3
        , [2.0, 2.0, 5.0, 0.0, 0.0, 0.0] # 4
        , [4.0, 1.0, 2.0, 0.0, 0.0, 0.0]]# 5
    b = [0.0, 0.0, 0.0, 30.0, 24.0, 36.0]
    c = [3.0,1.0,2.0,0.0,0.0,0.0]
    v = 0.0

    (sol, obj) = simplex(N,B,A,b,c,v)
    print ("Solution: " + str(sol))
    print ("Objective value: " + str(obj))

main()
