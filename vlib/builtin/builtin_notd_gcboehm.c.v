module builtin

// Just define the C functions, so that V does not error because of the missing definitions.

// Note: they will NOT be used, since calls to them are wrapped with `$if gcboehm ? { }`

fn C.GC_MALLOC(n usize) voidptr

fn C.GC_MALLOC_ATOMIC(n usize) voidptr

fn C.GC_MALLOC_UNCOLLECTABLE(n usize) voidptr

fn C.GC_REALLOC(ptr voidptr, n usize) voidptr

fn C.GC_FREE(ptr voidptr)

fn C.GC_memalign(align isize, size isize) voidptr

fn C.GC_get_heap_usage_safe(pheap_size &usize, pfree_bytes &usize, punmapped_bytes &usize, pbytes_since_gc &usize,
	ptotal_bytes &usize)

fn C.GC_get_memory_use() usize

fn C.GC_gcollect()

// gc_check_leaks is useful for detecting leaks, but it needs the GC to run.
// When GC is not used, it is a NOP. When VGC is active, delegates to vgc.
pub fn gc_check_leaks() {}

// gc_is_enabled() returns true, if the GC is enabled at runtime.
// With `-gc none` returns false. With `-gc vgc` delegates to the VGC.
pub fn gc_is_enabled() bool {
	$if vgc ? {
		return C.vgc_atomic_load_u32(unsafe { &vgc_heap.gc_enabled }) != 0
	}
	return false
}

// gc_enable explicitly enables the GC.
pub fn gc_enable() {
	$if vgc ? {
		C.vgc_atomic_store_u32(unsafe { &vgc_heap.gc_enabled }, 1)
	}
}

// gc_disable explicitly disables the GC.
pub fn gc_disable() {
	$if vgc ? {
		C.vgc_atomic_store_u32(unsafe { &vgc_heap.gc_enabled }, 0)
	}
}

// gc_collect explicitly performs a garbage collection.
pub fn gc_collect() {
	$if vgc ? {
		vgc_gc_start()
	}
}

pub type FnGC_WarnCB = fn (msg &char, arg usize)

fn C.GC_get_warn_proc() FnGC_WarnCB
fn C.GC_set_warn_proc(cb FnGC_WarnCB)

// gc_get_warn_proc returns the current callback fn, that will be used for printing GC warnings.
// When the GC is not on, it is a NOP.
pub fn gc_get_warn_proc() {}

// gc_set_warn_proc sets the callback fn, that will be used for printing GC warnings.
// When the GC is not on, it is a NOP.
pub fn gc_set_warn_proc(cb FnGC_WarnCB) {}

// used by builtin_init
fn internal_gc_warn_proc_none(msg &char, arg usize) {}
