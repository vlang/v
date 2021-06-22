module builtin

#flag -DGC_THREADS=1
#flag -I@VEXEROOT/thirdparty/libgc

$if windows && tinyc {
	// TODO: windows-tcc cannot compile boehm from source yet
	#flag -L@VEXEROOT/thirdparty/libgc
	#flag -lgc
} $else {
	$if windows {
		#flag -DGC_NOT_DLL=1
	}
	$if tinyc {
		#flag -DIGNORE_DYNAMIC_LOADING
	}
	// we statically link libgc
	#flag @VEXEROOT/thirdparty/libgc/gc.o
}

$if gcboehm_leak ? {
	#flag -DGC_DEBUG
}

#include "gc.h"

// replacements for `malloc()/calloc()`, `realloc()` and `free()`
// for use with Boehm-GC
// Do not use them manually. They are automatically chosen when
// compiled with `-gc boehm` or `-gc boehm_leak`.
fn C.GC_MALLOC(n size_t) voidptr

fn C.GC_MALLOC_ATOMIC(n size_t) voidptr

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

// protect memory block from being freed before this call
fn C.GC_reachable_here(voidptr)

// for leak detection it is advisable to do explicit garbage collections
pub fn gc_check_leaks() {
	$if gcboehm_leak ? {
		C.GC_gcollect()
	}
}
