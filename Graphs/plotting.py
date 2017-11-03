import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib

# 0 = inner, 1 = outer, 2 = all, 3 = cplex, 4 = seq

def many_small():
    small = np.genfromtxt('res_small.txt', delimiter=",")
    small = small/1000000
    #iss=[0,1,3]
    #small = smalls[:,iss]
    ns = [1000,2000,3000,4000,5000]
    #ns = [200,400,600,800,1000]
    plt.figure()
    #plt.semilogy(ns, small[0], 'ro',basey=2)
    plt.plot(ns, small[0], 'ro')
    #inner,outer,full,cplex,seq
    line1, = plt.plot(ns, small[0], 'r')
    plt.plot(ns, small[1], 'bo')
    line2, = plt.plot(ns, small[1], 'b')
    plt.plot(ns, small[2], 'go')
    line3, = plt.plot(ns, small[2], 'g')
    plt.plot(ns, small[3], 'yo')
    line4, = plt.plot(ns, small[3], 'y')
    plt.plot(ns, small[4], 'mo')
    line5, = plt.plot(ns, small[4], 'm')
    plt.ylabel('Mean running time in s')
    plt.xlabel('Number of Instances')
    plt.legend([line1, line2, line3,line4,line5], ['inner-parallel', 'outer-parallel', 'all-parllel','cplex','sequentiel'],loc=2)
    plt.title('Average running time on increasing instances with small m and n')
    a = plt.axes([0.65, 0.35, .2, .25])
    plt.plot(ns, small[1], 'bo')
    l2, = plt.plot(ns, small[1], 'b')
    plt.plot(ns, small[2], 'go')
    l3, = plt.plot(ns, small[2], 'g')
    plt.plot(ns, small[4], 'mo')
    l4, = plt.plot(ns, small[4], 'm')
    plt.legend([l2,l3,l4], ['inner-parallel', 'outer-parallel', 'all-parllel','cplex','sequentiel'],loc=2)
    plt.xticks([])
    plt.yticks([0.0,0.005,0.010,0.015])
#    plt.xticks(ns)
    plt.show()

def many_small2():
    small = np.genfromtxt('res_small.txt', delimiter=",")
    small = small/1000000
    #iss=[0,1,3]
    #small = smalls[:,iss]
    ns = [1000,2000,3000,4000,5000]
    #ns = [200,400,600,800,1000]
    plt.figure()
    #plt.semilogy(ns, small[0], 'ro',basey=2)
    #plt.plot(ns, small[0], 'ro')
    #inner,outer,full,cplex,seq
    #line1, = plt.plot(ns, small[0], 'r')
    plt.plot(ns, small[1], 'bo')                  #outer
    line2, = plt.plot(ns, small[1], 'b')
    plt.plot(ns, small[2], 'go')                  #all
    line3, = plt.plot(ns, small[2], 'g')
    #plt.plot(ns, small[3], 'yo')
    #line4, = plt.plot(ns, small[3], 'y')
    plt.plot(ns, small[4], 'mo')
    line5, = plt.plot(ns, small[4], 'm')          #seq
    plt.ylabel('Mean running time in s')
    plt.xlabel('Number of Instances')
    #plt.axes([0, 0.50, 1000, 5000])
    plt.legend([ line2, line3,line5], ['outer-parallel', 'all-parallel', 'sequential'],loc=2)
#    plt.xticks(ns)
    plt.title('Average running time on increasing instances with small m and n')

    a = plt.axes([0.65, 0.11, .2, .25])
    plt.plot(ns, small[0], 'ro')
    l1, = plt.plot(ns, small[0], 'r')             # inner
    plt.plot(ns, small[3], 'yo')
    l2, =plt.plot(ns, small[3], 'y')              # cplex
    plt.plot(ns, small[4], 'mo')
    l3, = plt.plot(ns, small[4], 'm')             # seq
    plt.plot(ns, small[0], 'r')
    plt.legend([l1,l2,l3], ['inner-parallel', 'cplex', 'sequential'],loc=2)
    plt.xticks([])
    plt.yticks([0,1,2.5])
    plt.show()

def many_varying():
    small = np.genfromtxt('res_vary.txt', delimiter=",")
    small = small/1000000
    #ns = [1000,2000,3000,4000,5000]
    #ns = [200,400,600,800,1000]
    ns = [1000,1500,2000,2500,3000]
    plt.figure()
    plt.plot(ns, small[0], 'ro')
    line1, = plt.plot(ns, small[0], 'r')
    plt.plot(ns, small[1], 'bo')
    line2, = plt.plot(ns, small[1], 'b')
    plt.plot(ns, small[2], 'go')
    line3, = plt.plot(ns, small[2], 'g')
    plt.plot(ns, small[3], 'yo')
    line4, = plt.plot(ns, small[3], 'y')
    plt.plot(ns, small[4], 'mo')
    line5, = plt.plot(ns, small[4], 'm')
    plt.ylabel('Mean running time in s')
    plt.xlabel('Number of Instances')
    plt.legend([line1, line2, line3,line4,line5], ['inner-parallel', 'outer-parallel', 'all-parllel','cplex','sequentiel'],loc=2)
    plt.xticks(ns)
    plt.title('Average running time on increasing instances with high variance m and n')
    plt.show()

def many_big():
    small = np.genfromtxt('res_big.txt', delimiter=",")
    small = small/1000000
    #ns = [1000,2000,3000,4000,5000]
    ns = [200,400,600,800,1000]
    plt.figure()
    plt.plot(ns, small[0], 'ro')
    line1, = plt.plot(ns, small[0], 'r')
    plt.plot(ns, small[1], 'bo')
    line2, = plt.plot(ns, small[1], 'b')
    plt.plot(ns, small[2], 'go')
    line3, = plt.plot(ns, small[2], 'g')
    plt.plot(ns, small[3], 'yo')
    line4, = plt.plot(ns, small[3], 'y')
    plt.plot(ns, small[4], 'mo')
    line5, = plt.plot(ns, small[4], 'm')
    plt.ylabel('Mean running time in s')
    plt.xlabel('Number of Instances')
    plt.legend([line1, line2, line3,line4,line5], ['inner-parallel', 'outer-parallel', 'all-parllel','cplex','sequentiel'],loc=2)
    plt.xticks(ns)
    plt.title('Average running time on increasing instances with big m and n')
    plt.show()

def one_big():
    small = np.genfromtxt('res_one.txt', delimiter=",")
    small = small/1000000
    ns = [1000,1500,2000,2500,3000]
    #ns = [200,400,600,800,1000]
    plt.figure()
    #plt.semilogy(ns, small[0], 'ro',basey=2)
    plt.plot(ns, small[0], 'ro')
    line1, = plt.plot(ns, small[0], 'r')
    #plt.plot(ns, small[1], 'bo')
    #line2, = plt.plot(ns, small[1], 'b')
    plt.plot(ns, small[1], 'go')
    line3, = plt.plot(ns, small[1], 'g')
    plt.plot(ns, small[2], 'yo')
    line4, = plt.plot(ns, small[2], 'y')
    #plt.plot(ns, small[2], 'mo')
    #line5, = plt.plot(ns, small[2], 'm')
    plt.ylabel('Mean running time in s')
    plt.xlabel('C and V')
    #plt.legend([line1, line2, line3], ['reduced-flat', 'multi', 'reduced'],loc=2)
   # plt.legend([line1, line2, line4,line5], ['inner-parallel', 'all-parllel','cplex','sequentiel'],loc=2)
    plt.legend([line1,line3,line4], ['inner-parallel', 'all-parllel','cplex','sequentiel'],loc=2)
    plt.xticks(ns)
    plt.title('Average running time on one instance with increasing m and n')
    plt.show()

matplotlib.rcParams.update({'font.size': 16})

##many_small()
many_small2()
#many_varying()
#many_big()
#one_big()
