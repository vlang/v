@[has_globals]
module builtin

// The bare libc adapters share one dlmalloc instance per process. Serialize
// initialization and mutations without allocating or depending on pthreads.
// The dlmalloc system callbacks make raw VM syscalls and never enter this lock.
__global g_bare_heap_lock i32

@[inline]
fn bare_heap_lock() {
	for C.v_prealloc_atomic_cas_i32(&g_bare_heap_lock, 0, 1) == 0 {
		for C.v_prealloc_atomic_load_i32(&g_bare_heap_lock) != 0 {
		}
	}
}

@[inline]
fn bare_heap_unlock() {
	// The existing atomic helper supplies a release barrier on every C compiler.
	C.v_prealloc_atomic_cas_i32(&g_bare_heap_lock, 1, 0)
}
