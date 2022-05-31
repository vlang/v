module builtin

// Note: See mkirchner's gc @ https://github.com/mkirchner/gc

//#flag -DGC_THREADS=1
//#flag -DGC_BUILTIN_ATOMIC=1

#flag -L@VEXEROOT/thirdparty/mgc
#include "@VEXEROOT/thirdparty/mgc/gc.h"
#include "@VEXEROOT/thirdparty/mgc/log.h"
#include "@VEXEROOT/thirdparty/mgc/gc.c"
#include "@VEXEROOT/thirdparty/mgc/log.c"
#include <float.h>
#include <stdio.h>
#include <stdlib.h>


// replacements for `malloc()/calloc()`, `realloc()` and `free()`
// for use with mkirchner-GC (see <https://github.com/mkirchner/gc>)
// Do not use them manually. They are automatically chosen when
// compiled with `-gc boehm` or `-gc boehm_leak`.

struct C.GarbageCollector {
}

fn C.GC_MALLOC(n usize) voidptr

//fn C.GC_MALLOC_ATOMIC(n usize) voidptr

//fn C.GC_MALLOC_UNCOLLECTABLE(n usize) voidptr

fn C.GC_REALLOC(ptr voidptr, n usize) voidptr

fn C.GC_FREE(ptr voidptr)

// explicitly perform garbage collection now! Garbage collections
// are done automatically when needed, so this function is hardly needed
fn C.GC_gcollect()

// functions to temporarily suspend/resume garbage collection
// fn C.GC_disable()

// fn C.GC_enable()

// returns non-zero if GC is disabled
// fn C.GC_is_disabled() int

// protect memory block from being freed before this call
// fn C.GC_reachable_here(voidptr)

// for leak detection it is advisable to do explicit garbage collections
/*pub fn gc_check_leaks() {
	$if gcboehm_leak ? {
		C.GC_gcollect()
	}
}*/
