module builtin

#define GC_THREADS 1

$if windows {
	#flag -I@VROOT/thirdparty/libgc/include
	#flag -L@VROOT/thirdparty/libgc
}

#include <gc.h>

#flag linux -lgc
#flag darwin @VROOT/thirdparty/bdwgc/extra/.libs/gc.o

fn C.GC_MALLOC(n size_t) voidptr

fn C.GC_REALLOC(ptr voidptr, n size_t) voidptr

fn C.GC_FREE(ptr voidptr)

fn C.GC_set_find_leak(int)

// fn C.CHECK_LEAKS()
fn C.GC_gcollect()
