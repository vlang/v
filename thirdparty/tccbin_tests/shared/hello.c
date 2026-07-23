/* Trivial program, GC-initialized but no real allocation load.
   Exercises the compiler + GC startup path with minimal surface area -
   any platform where this doesn't pass and run cleanly has a basic
   compatibility problem in the prebuilt tcc/libgc pairing itself. */
#include <stdio.h>
#include "gc.h"

int main(void) {
    GC_INIT();
    printf("hello\n");
    return 0;
}
