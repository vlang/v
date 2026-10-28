#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#if defined(FONTSTASH_USE_GCBOEHM)
// Darwin TinyCC defines __APPLE__ without __MACH__.
#if defined(__APPLE__) && !defined(GC_DARWIN_THREADS)
#define GC_DARWIN_THREADS 1
#endif
#if defined(__TINYC__) && defined(__linux__)
#include <gc/gc.h>
#else
#include <gc.h>
#endif
#define FONTSTASH_MALLOC GC_MALLOC
#define FONTSTASH_REALLOC GC_REALLOC
#define FONTSTASH_FREE GC_FREE
#define FONTSTASH_MALLOC_ATOMIC GC_MALLOC_ATOMIC
#endif
#define FONTSTASH_IMPLEMENTATION
#include "fontstash.h"
