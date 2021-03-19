module builtin

#include <gc.h>

#flag -lgc

fn C.GC_MALLOC(n size_t) voidptr

fn C.GC_REALLOC(ptr voidptr, n size_t) voidptr

fn C.GC_FREE(ptr voidptr)
