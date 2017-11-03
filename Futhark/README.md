Simplex in Futhark
==================

GPU-parallel implementation of the Simplex algorithm for solving several instances of linear programs. There are three versions:

 - outer parallel: parallelises across the multiple instances, but solves each single instance sequentially.
 - inner parallel: parallelises the multiple instances sequentially, but solves each single instance in parallel.
 - full parallel: fully parallel execution on flattened instances.


Testing and benchmarking
------------------------

There are some test cases in the `tests` directory, written in the format output by the Simplex Instance Generator.
The naming format is `name_n_v_c.test` where n is the number of simplex instances, v is the max number of variables (aka `n`), and c is the max number of constraints (aka `m`) for any instance in the set.

Use the `runtests.py` script to convert them to futhark format (in `generated_tests` directory) and run the tests.
Test all versions on the `basic` case with (this is also the default if `runtests.py` is not given any arguments):

    ./runtests.py --test-all --convert tests/basic_4_3_3.test

Benchmark all versions on the `basic` case with:

    ./runtests.py --bench-all -convert tests/basic_4_3_3.test

For more options:

    ./runtests.py --help
