module builtin

// Just define the C functions, so that V does not error because of the missing definitions.

// Note: they will NOT be used, since calls to them are wrapped with `$if gcboehm ? { }`

fn C.GC_MALLOC(n usize) voidptr

fn C.GC_MALLOC_ATOMIC(n usize) voidptr

fn C.GC_MALLOC_UNCOLLECTABLE(n usize) voidptr

fn C.GC_REALLOC(ptr voidptr, n usize) voidptr

fn C.GC_FREE(ptr voidptr)

fn C.GC_get_heap_usage_safe(pheap_size &usize, pfree_bytes &usize, punmapped_bytes &usize, pbytes_since_gc &usize, ptotal_bytes &usize)

fn C.GC_get_memory_use() usize

fn C.GC_gcollect()

// gc_check_leaks is useful for detecting leaks, but it needs the GC to run.
// When GC is not used, it is a NOP.
pub fn gc_check_leaks() {}

// gc_collect explicitly performs a garbage collection.
// When the GC is not on, it is a NOP.
pub fn gc_collect() {}

type FnGC_WarnCB = fn (msg &char, arg usize)

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
