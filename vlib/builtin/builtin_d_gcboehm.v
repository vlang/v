module builtin

#define GC_THREADS 1

$if windows {
	#flag -I@VROOT/thirdparty/libgc/include
	#flag -L@VROOT/thirdparty/libgc
}
$if macos {
	#pkgconfig bdw-gc
}

#include <gc.h>

#flag -lgc

fn C.GC_MALLOC(n size_t) voidptr

fn C.GC_REALLOC(ptr voidptr, n size_t) voidptr

fn C.GC_FREE(ptr voidptr)

fn C.GC_set_find_leak(int)

fn C.GC_gcollect()

fn gcboehm_check_leaks() {
	$if gcboehm_leak ? {
		C.GC_gcollect()
	}
}
