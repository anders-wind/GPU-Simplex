#!/usr/bin/python
'''The evil test/benchmark script.'''

import subprocess
import argparse
import glob
import os.path

tmp_prefix = 'tmpfutsimplex'
test_in_file = 'tests/gen_test_in.txt'
test_out_file = 'tests/gen_test_out.txt'

def split_data(filename, aname, bname, cname, rname):
    a,b,c,r='[','[','[','['
    with open(filename) as f,\
         open(aname,'a') as aout, open(bname,'a') as bout,\
         open(cname,'a') as cout, open(rname,'a') as rout:
            for line in f:
                aout.write(a.replace('\n', ','))
                bout.write(b.replace('\n', ','))
                cout.write(c.replace('\n', ','))
                rout.write(r.replace('\n', ','))
                a,b,c,r = line,f.next(),f.next(),f.next()
            aout.write(a.replace('\n', ']\n'))
            bout.write(b.replace('\n', ']\n'))
            cout.write(c.replace('\n', ']\n'))
            rout.write(r.replace('\n', ']\n'))

def arrange_test_data(testfile,tin,tout):
    afile = tmp_prefix+'a.file'
    bfile = tmp_prefix+'b.file'
    cfile = tmp_prefix+'c.file'
    rfile = tmp_prefix+'r.file'
    split_data(testfile, afile, bfile, cfile, rfile)
    subprocess.call('cat '+afile+' '+bfile+' '+cfile+' > '+tin, shell=True)
    subprocess.call('mv '+rfile+' '+tout, shell=True)

def generate_test_data(filename,n,v,c):
    strc = 'make -C ../Simplex\ Instance\ Generator -s run N={0} V={1} C={2} > {3}'
    cmd = strc.format(n,v,c,filename)
    subprocess.call(cmd, shell=True)

def run_tests(files):
    for f in files:
        if os.path.isfile(f):
            subprocess.call('futhark-test '+f, shell=True)
        else:
            print ("File doesn't exist: " + f)

def run_benches(files):
    for f in files:
        if os.path.isfile(f):
            subprocess.call('futhark-bench '+f, shell=True)
        else:
            print ("File doesn't exist: " + f)

def cleanup():
    subprocess.call('rm -f '+tmp_prefix+'*', shell=True)

def doit(args):
    try:
        if not(args.no_gen):
            n,v,c=args.number, args.variables,args.constraints
            print ('Generating test data: N={0}, V={1}, C={2}'.format(n,v,c))
            test_file = tmp_prefix+'test.file'
            generate_test_data(test_file,n,v,c)
            arrange_test_data(test_file,test_in_file,test_out_file)

        if args.no_test_bench:
            pass
        elif args.test_all:
            print 'Testing all files'
            run_tests(glob.glob('./*.fut'))
        elif args.test:
            print ('Testing one file: ' + args.test)
            run_tests([args.test])
        elif args.bench_all:
            print 'Benchmarking all files'
            run_benches(glob.glob('./*.fut'))
        elif args.bench:
            print ('Benchmarking one file: ' + args.bench)
            run_benches([args.bench])
        else:
            print 'Default: Testing all files'
            run_tests(glob.glob('./*.fut'))
    finally:
        cleanup()

def main():
    parser = argparse.ArgumentParser(description='Test the Futhark Simplex suite.')
    parser.add_argument('-a','--test-all', help='Test all versions',action='store_true')
    parser.add_argument('-t','--test', help='Test one file')
    parser.add_argument('-e','--bench-all', help='Benchmark all versions',action='store_true')
    parser.add_argument('-b','--bench', help='Benchmark one file')
    parser.add_argument('-n','--number', help='Number of instances to generate.', type=int, default=1)
    parser.add_argument('-v','--variables', help='Number of variables in each instance', type=int, default=5)
    parser.add_argument('-c','--constraints', help='Number of constraints in each instance', type=int, default=5)
    parser.add_argument('-x','--no-test-bench', help='Only generate test data', action='store_true')
    parser.add_argument('-y','--no-gen', help='Do not generate test data', action='store_true')
    # TODO: maybe data gen mode: sparse, etc.
    #       --directory: run tests from directory
    #       variable size of instances generated
    args = parser.parse_args()
    doit(args)

main()
