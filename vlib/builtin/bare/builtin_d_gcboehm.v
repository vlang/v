module builtin

#define GC_THREADS 1

$if windows {
	#flag -I@VROOT/thirdparty/libgc/include
	#flag -L@VROOT/thirdparty/libgc
}
$if macos {
	#pkgconfig bdw-gc
}
$if gcboehm_leak ? {
	#define GC_DEBUG
}
#include <gc.h>

#flag -lgc

// replacements for `malloc()/calloc()`, `realloc()` and `free()`
// for use with Boehm-GC
// Do not use them manually. They are automatically chosen when
// compiled with `-gc boehm` or `-gc boehm_leak`.
fn C.GC_MALLOC(n size_t) voidptr

fn C.GC_MALLOC_UNCOLLECTABLE(n size_t) voidptr

fn C.GC_REALLOC(ptr voidptr, n size_t) voidptr

fn C.GC_FREE(ptr voidptr)

// explicitely perform garbage collection now! Garbage collections
// are done automatically when needed, so this function is hardly needed 
fn C.GC_gcollect()

// functions to temporarily suspend/resume garbage collection
fn C.GC_disable()

fn C.GC_enable()

// returns non-zero if GC is disabled
fn C.GC_is_disabled() int

// for leak detection it is advisable to do explicit garbage collections
pub fn gc_check_leaks() {
	$if gcboehm_leak ? {
		C.GC_gcollect()
	}
}
