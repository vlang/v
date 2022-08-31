module builtin

#flag -DGC_THREADS=1

$if dynamic_boehm ? {
	$if windows {
		$if tinyc {
			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag -L @VEXEROOT/thirdparty/tcc/lib
			#flag -lgc
		} $else $if msvc {
			#flag -DGC_BUILTIN_ATOMIC=1
			#flag -I @VEXEROOT/thirdparty/libgc/include
		} $else {
			#flag -DGC_WIN32_THREADS=1
			#flag -DGC_BUILTIN_ATOMIC=1
			#flag -I @VEXEROOT/thirdparty/libgc
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		}
	} $else {
		$if $pkgconfig('bdw-gc') {
			#pkgconfig bdw-gc
		} $else {
			$if openbsd || freebsd {
				#flag -I/usr/local/include
				#flag -L/usr/local/lib
			}
			#flag -lgc
		}
	}
} $else {
	$if macos || linux {
		#flag -DGC_BUILTIN_ATOMIC=1
		#flag -I @VEXEROOT/thirdparty/libgc/include
		$if (!macos && prod && !tinyc && !debug) || !(amd64 || arm64 || i386 || arm32) {
			// TODO: replace the architecture check with a `!$exists("@VEXEROOT/thirdparty/tcc/lib/libgc.a")` comptime call
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		} $else {
			#flag @VEXEROOT/thirdparty/tcc/lib/libgc.a
		}
		#flag -ldl
		#flag -lpthread
	} $else $if freebsd {
		// Tested on FreeBSD 13.0-RELEASE-p3, with clang, gcc and tcc:
		#flag -DGC_BUILTIN_ATOMIC=1
		#flag -DBUS_PAGE_FAULT=T_PAGEFLT
		$if !tinyc {
			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		}
		$if tinyc {
			#flag -I/usr/local/include
			#flag $first_existing("/usr/local/lib/libgc.a", "/usr/lib/libgc.a")
			#flag -lgc
		}
		#flag -lpthread
	} $else $if openbsd {
		#flag -DGC_BUILTIN_ATOMIC=1
		#flag -I/usr/local/include
		#flag $first_existing("/usr/local/lib/libgc.a", "/usr/lib/libgc.a")
		#flag -lpthread
	} $else $if windows {
		#flag -DGC_NOT_DLL=1
		#flag -DGC_WIN32_THREADS=1
		$if tinyc {
			#flag -DGC_BUILTIN_ATOMIC=1
			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag @VEXEROOT/thirdparty/tcc/lib/libgc.a
			#flag -luser32
		} $else $if msvc {
			// Build libatomic_ops
			#flag @VEXEROOT/thirdparty/libatomic_ops/atomic_ops.o
			#flag -I  @VEXEROOT/thirdparty/libatomic_ops

			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		} $else {
			#flag -DGC_BUILTIN_ATOMIC=1
			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		}
	} $else $if $pkgconfig('bdw-gc') {
		#flag -DGC_BUILTIN_ATOMIC=1
		#pkgconfig bdw-gc
	} $else {
		#flag -DGC_BUILTIN_ATOMIC=1
		#flag -lgc
	}
}

$if gcboehm_leak ? {
	#flag -DGC_DEBUG=1
}

#include <gc.h>

// replacements for `malloc()/calloc()`, `realloc()` and `free()`
// for use with Boehm-GC
// Do not use them manually. They are automatically chosen when
// compiled with `-gc boehm` or `-gc boehm_leak`.
fn C.GC_MALLOC(n usize) voidptr

fn C.GC_MALLOC_ATOMIC(n usize) voidptr

fn C.GC_MALLOC_UNCOLLECTABLE(n usize) voidptr

fn C.GC_REALLOC(ptr voidptr, n usize) voidptr

fn C.GC_FREE(ptr voidptr)

// explicitly perform garbage collection now! Garbage collections
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
