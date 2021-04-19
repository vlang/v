module builtin

#define GC_THREADS 1

$if static_boehm ? {
	$if macos {
		#flag -I$first_existing("/opt/homebrew/include",     "/usr/local/include")
		#flag   $first_existing("/opt/homebrew/lib/libgc.a", "/usr/local/lib/libgc.a")
	} $else $if linux {
		#flag -l:libgc.a
	} $else {
		#flag -lgc
	}
} $else {
	$if macos {
		#pkgconfig bdw-gc
	}
	$if windows {
		#flag -I@VEXEROOT/thirdparty/libgc/include
		#flag -L@VEXEROOT/thirdparty/libgc
	}
	#flag -lgc
}

$if gcboehm_leak ? {
	#define GC_DEBUG
}

#include <gc.h>

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
