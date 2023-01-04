## A simple example to show how to call a function written in v from c

### Compile as a shared library

#### On Linux:

Step 1: Compile the v code to a shared library using `v -cc gcc -shared v_test_print.v` or
`v -cc gcc -shared v_test_math.v`.

Step 2: Compile the c file using `gcc test_print.c v_test_print.so -o test_print` or
`gcc test_math.c v_test_math.so -o test_math`.

Step 3: Run the compiled c file using `LD_LIBRARY_PATH=. ./test_print` or
`LD_LIBRARY_PATH=. ./test_math`.

#### On Windows:

Step 1: Compile the v code to a shared library using `v -cc gcc -shared v_test_print.v` or
`v -cc gcc -shared v_test_math.v`.

Step 2: Compile the c file using `gcc test_print.c v_test_print.dll -o test_print.exe` or
`gcc test_math.c v_test_math.dll -o test_math.exe`.

Step 3: Run the compiled c file using `test_print.exe` or `test_math.exe`.

### Compile as a c file

***Requirements: `libgc` must be installed***

Step 1: Compile the v code to a shared library using
`v -shared -cc gcc -o v_test_print.c v_test_print.v` or
`v -shared -cc gcc -o v_test_math.c v_test_math.v`.

*Specifying the output with a `.c` extension will generate the corresponding C source file.*

Step 2: Compile the c file using `gcc test_print.c v_test_print.c -o test_print -lgc` or
`gcc test_math.c v_test_math.c -o test_math -lgc -lm`.

Step 3: Run the compiled c file using `./test_print` or `./test_math`.
