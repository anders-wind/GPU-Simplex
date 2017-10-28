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

def generate_test_data(filename,n,(vl,vh),(cl,ch)):
    strc = 'make -C ../Simplex\ Instance\ Generator -s run\
            N={0} VLOW={1} VHIGH={2} CLOW={3} CHIGH={4} > {5}'
    cmd = strc.format(n,vl,vh,cl,ch,filename)
    subprocess.call(cmd, shell=True)

def run_futhark(prog, files, compiler):
    for f in files:
        if os.path.isfile(f):
            cmd = 'futhark-{0} --compiler={1} {2}'.format(prog, compiler, f)
            subprocess.call(cmd, shell=True)
        else:
            print("File doesn't exist: {0}".format(f))

def run_tests(files, compiler):
    run_futhark('test', files, compiler)

def run_benches(files, compiler):
    run_futhark('bench', files, compiler)

def cleanup():
    cmd = 'rm -f {0}*'.format(tmp_prefix)
    subprocess.call(cmd, shell=True)

def doit(args):
    try:
        if args.convert:
            print 'Converting Simplex Instance Generator data to futhark simplex data'
            arrange_test_data(args.convert,test_in_file,test_out_file)
            return

        if not(args.no_gen):
            n,v,c=args.number, args.variables,args.constraints
            print ('Generating test data: N={0}, V={1}, C={2}'.format(n,v,c))
            test_file = tmp_prefix+'test.file'
            generate_test_data(test_file,n,v,c)
            arrange_test_data(test_file,test_in_file,test_out_file)

        compiler = args.compiler
        if args.no_test_bench:
            pass
        elif args.test_all:
            print 'Testing all files'
            run_tests(glob.glob('./*.fut'), compiler)
        elif args.test:
            print ('Testing one file: ' + args.test)
            run_tests([args.test], compiler)
        elif args.bench_all:
            print 'Benchmarking all files'
            run_benches(glob.glob('./*.fut'), compiler)
        elif args.bench:
            print ('Benchmarking one file: ' + args.bench)
            run_benches([args.bench], compiler)
        else:
            print 'Default: Testing all files'
            run_tests(glob.glob('./*.fut'), compiler)
    finally:
        cleanup()

def int_tuple(string):
    spl = string.split(",")
    if len(spl) != 2:
        raise argparse.ArgumentTypeError("arg {0} should be a range a,b".format(string))
    try:
        return int(spl[0]),int(spl[1])
    except TypeError:
        raise argparse.ArgumentTypeError("range {0} must be integers".format(string))

def main():
    parser = argparse.ArgumentParser(description='Test the Futhark Simplex suite.')
    parser.add_argument('-a','--test-all', help='Test all versions',action='store_true')
    parser.add_argument('-t','--test', help='Test one file')
    parser.add_argument('-e','--bench-all', help='Benchmark all versions',action='store_true')
    parser.add_argument('-b','--bench', help='Benchmark one file')
    parser.add_argument('-n','--number', help='Number of instances to generate.', type=int, default=5)
    parser.add_argument('-v','--variables', help='Range of variable numbers (n)', type=int_tuple, default=(5,5))
    parser.add_argument('-c','--constants', help='Range of constant numbers (m)', type=int_tuple, default=(5,5))
    parser.add_argument('-x','--no-test-bench', help='Only generate test data', action='store_true')
    parser.add_argument('-y','--no-gen', help='Do not generate test data', action='store_true')
    parser.add_argument('-p','--compiler', help='Which Futhark compiler to use', default='futhark-c')
    parser.add_argument('--convert', help='Given a file with Simplex Instance Generator output, convert to futhark simplex format')
    # TODO: maybe data gen mode: sparse, etc.
    #       --directory: run tests from directory
    #       variable size of instances generated
    args = parser.parse_args()
    doit(args)

main()
