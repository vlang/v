module builtin

$if !no_gc_threads ? {
	#flag -DGC_THREADS=1
}

$if use_bundled_libgc ? {
	#flag -DGC_BUILTIN_ATOMIC=1
	#flag -I @VEXEROOT/thirdparty/libgc/include
	#flag @VEXEROOT/thirdparty/libgc/gc.o
}

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
			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		}
	} $else {
		$if $pkgconfig('bdw-gc-threaded') {
			#pkgconfig bdw-gc-threaded
		} $else $if $pkgconfig('bdw-gc') {
			#pkgconfig bdw-gc
		} $else {
			$if openbsd || freebsd {
				#flag -I/usr/local/include
				#flag -L/usr/local/lib
			}
			$if freebsd {
				#flag -lgc-threaded
			} $else {
				#flag -lgc
			}
		}
	}
} $else {
	$if macos || linux {
		#flag -DGC_BUILTIN_ATOMIC=1
		#flag -I @VEXEROOT/thirdparty/libgc/include
		$if (prod && !tinyc && !debug) || !(amd64 || arm64 || i386 || arm32 || rv64) {
			// TODO: replace the architecture check with a `!$exists("@VEXEROOT/thirdparty/tcc/lib/libgc.a")` comptime call
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		} $else {
			$if !use_bundled_libgc ? {
				#flag @VEXEROOT/thirdparty/tcc/lib/libgc.a
			}
		}
		$if macos {
			#flag -DMPROTECT_VDB=1
		}
		#flag -ldl
		#flag -lpthread
	} $else $if freebsd {
		// Tested on FreeBSD 13.0-RELEASE-p3, with clang, gcc and tcc
		#flag -DGC_BUILTIN_ATOMIC=1
		#flag -DBUS_PAGE_FAULT=T_PAGEFLT
		$if !tinyc {
			#flag -DUSE_MMAP
			#flag -I @VEXEROOT/thirdparty/libgc/include
			#flag @VEXEROOT/thirdparty/libgc/gc.o
		}
		$if tinyc {
			#flag -I/usr/local/include
			#flag $first_existing("@VEXEROOT/thirdparty/tcc/lib/libgc.a", "/usr/local/lib/libgc-threaded.a", "/usr/lib/libgc-threaded.a")
			#flag -lgc-threaded
		}
		#flag -lpthread
	} $else $if openbsd {
		// Tested on OpenBSD 7.5, with clang, gcc and tcc
		#flag -DGC_BUILTIN_ATOMIC=1
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
	} $else $if windows {
		#flag -DGC_NOT_DLL=1
		#flag -DGC_WIN32_THREADS=1
		#flag -luser32
		$if tinyc {
			#flag -DGC_BUILTIN_ATOMIC=1
			#flag -I @VEXEROOT/thirdparty/libgc/include
			$if !use_bundled_libgc ? {
				#flag @VEXEROOT/thirdparty/tcc/lib/libgc.a
			}
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

// #include <gc/gc_mark.h>

// replacements for `malloc()/calloc()`, `realloc()` and `free()`
// for use with Boehm-GC
// Do not use them manually. They are automatically chosen when
// compiled with `-gc boehm` or `-gc boehm_leak`.
fn C.GC_MALLOC(n usize) voidptr

fn C.GC_MALLOC_ATOMIC(n usize) voidptr

fn C.GC_MALLOC_UNCOLLECTABLE(n usize) voidptr

fn C.GC_REALLOC(ptr voidptr, n usize) voidptr

fn C.GC_FREE(ptr voidptr)

fn C.GC_memalign(align isize, size isize) voidptr

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

// gc_is_enabled() returns true, if the GC is enabled at runtime.
// See also gc_disable() and gc_enable().
pub fn gc_is_enabled() bool {
	return 0 == C.GC_is_disabled()
}

// gc_collect explicitly performs a single garbage collection run.
// Note, that garbage collections, are done automatically, when needed in most cases,
// so usually you should NOT need to call gc_collect() often.
// Note that gc_collect() is a NOP with `-gc none`.
pub fn gc_collect() {
	C.GC_gcollect()
}

// gc_enable explicitly enables the GC.
// Note, that garbage collections are done automatically, when needed in most cases,
// and also that by default the GC is on, so you do not need to enable it.
// See also gc_disable() and gc_collect().
// Note that gc_enable() is a NOP with `-gc none`.
pub fn gc_enable() {
	C.GC_enable()
}

// gc_disable explicitly disables the GC. Do not forget to enable it again by calling gc_enable(), when your program is otherwise idle, and can afford it.
// See also gc_enable() and gc_collect().
// Note that gc_disable() is a NOP with `-gc none`.
pub fn gc_disable() {
	C.GC_disable()
}

// gc_check_leaks is useful for leak detection (it does an explicit garbage collections, but only when a program is compiled with `-gc boehm_leak`).
pub fn gc_check_leaks() {
	$if gcboehm_leak ? {
		C.GC_gcollect()
	}
}

fn C.GC_get_heap_usage_safe(pheap_size &usize, pfree_bytes &usize, punmapped_bytes &usize, pbytes_since_gc &usize,
	ptotal_bytes &usize)
fn C.GC_get_memory_use() usize

pub struct C.GC_stack_base {
	mem_base voidptr
	// reg_base voidptr
}

fn C.GC_get_stack_base(voidptr) int
fn C.GC_register_my_thread(voidptr) int
fn C.GC_unregister_my_thread() int

// fn C.GC_get_my_stackbottom(voidptr) voidptr
fn C.GC_set_stackbottom(voidptr, voidptr)

// fn C.GC_push_all_stacks()

fn C.GC_add_roots(voidptr, voidptr)
fn C.GC_remove_roots(voidptr, voidptr)

// fn C.GC_get_push_other_roots() fn()
// fn C.GC_set_push_other_roots(fn())

fn C.GC_get_sp_corrector() fn (voidptr, voidptr)
fn C.GC_set_sp_corrector(fn (voidptr, voidptr))

// FnGC_WarnCB is the type of the callback, that you have to define, if you want to redirect GC warnings and handle them.
// Note: GC warnings are silenced by default. Use gc_set_warn_proc/1 to set your own handler for them.
pub type FnGC_WarnCB = fn (msg &char, arg usize)

fn C.GC_get_warn_proc() FnGC_WarnCB
fn C.GC_set_warn_proc(cb FnGC_WarnCB)

// gc_get_warn_proc returns the current callback fn, that will be used for printing GC warnings.
pub fn gc_get_warn_proc() FnGC_WarnCB {
	return C.GC_get_warn_proc()
}

// gc_set_warn_proc sets the callback fn, that will be used for printing GC warnings.
pub fn gc_set_warn_proc(cb FnGC_WarnCB) {
	C.GC_set_warn_proc(cb)
}

// used by builtin_init:
fn internal_gc_warn_proc_none(msg &char, arg usize) {}
