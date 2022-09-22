## A simple example to show how to call a function written in v from c

### Compile as a shared library

#### On Linux:

Step 1: Compile the v code to a shared library using ``v -shared -prod v_test_print.v``

Step 2: Compile the c file using ``gcc test_print.c v_test_print.so -o test_print``

Step 3: Run the compiled c file using ``LD_LIBRARY_PATH=. ./test_print``

#### On Windows:

Step 1: Compile the v code to a shared library using ``v -shared -prod v_test_print.v``

Step 2: Compile the c file using ``gcc test_print.c v_test_print.dll -o test_print.exe``

Step 3: Run the compiled c file using ``test_print.exe``

### Compile as a c file

***Requirements: `libgc` must be installed***

Step 1: Compile the v code to a shared library using ``v -shared -prod v_test_print.v -o v_test_print.c``  
*Specify the output with a `.c` extension will generate the corresponding c code file.*

Step 2: Compile the c file using ``gcc test_print.c v_test_print.c -o test_print -lgc``

Step 3: Run the compiled c file using ``./test_print``

For the `test_math` example, you will need to link with math lib: ``gcc test_math.c v_test_math.c -o test_math -lgc -lm``.
