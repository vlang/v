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
// When GC is not used, it is a NOP.
pub fn gc_check_leaks() {}

// gc_is_enabled() returns true, if the GC is enabled at runtime.
// It will always return false, with `-gc none`.
// See also gc_disable() and gc_enable().
pub fn gc_is_enabled() bool {
	return false
}

// gc_enable explicitly enables the GC.
// Note, that garbage collections are done automatically, when needed in most cases,
// and also that by default the GC is on, so you do not need to enable it.
// See also gc_disable() and gc_collect().
// Note that gc_enable() is a NOP with `-gc none`.
pub fn gc_enable() {}

// gc_disable explicitly disables the GC.
// Do not forget to enable it again by calling gc_enable(), when your program is otherwise idle, and can afford it.
// See also gc_enable() and gc_collect().
// Note that gc_disable() is a NOP with `-gc none`.
pub fn gc_disable() {}

// gc_collect explicitly performs a garbage collection.
// When the GC is not on, (with `-gc none`), it is a NOP.
pub fn gc_collect() {}

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
