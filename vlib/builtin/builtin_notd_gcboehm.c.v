module builtin

// Just define the C functions, so that V does not error because of the missing definitions.

// Note: they will NOT be used, since calls to them are wrapped with `$if gcboehm ? { }`

fn C.GC_MALLOC(n usize) voidptr

fn C.GC_MALLOC_ATOMIC(n usize) voidptr

fn C.GC_MALLOC_UNCOLLECTABLE(n usize) voidptr

fn C.GC_REALLOC(ptr voidptr, n usize) voidptr

fn C.GC_FREE(ptr voidptr)

fn C.GC_get_heap_size() usize
fn C.GC_get_free_bytes() usize
fn C.GC_get_memory_use() usize
fn C.GC_get_total_bytes() usize

// provide an empty function when manual memory management is used
// to simplify leak detection
//
pub fn gc_check_leaks() {}
