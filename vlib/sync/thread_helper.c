#ifdef V_GCBOEHM_NATIVE
#if defined(__APPLE__) && !defined(GC_DARWIN_THREADS)
#define GC_DARWIN_THREADS 1
#endif
#if defined(__TINYC__) && defined(__linux__)
#include <gc/gc.h>
#else
#include <gc.h>
#endif
#endif
#define V_SYNC_THREAD_SCOPE
#include "thread_helper.h"
