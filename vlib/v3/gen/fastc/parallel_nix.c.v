module fastc

#include <unistd.h>

fn C.sysconf(name int) i64

// fastc_nr_cpus reports the host's online CPU count for parallel generation.
fn fastc_nr_cpus() int {
	// sysconf is a libc call; validate its result before using it as a count.
	count := unsafe { int(C.sysconf(C._SC_NPROCESSORS_ONLN)) }
	if count < 1 {
		return 1
	}
	return count
}

// __atomic_fetch_add is the GCC/Clang atomic builtin (also supported by the
// bundled TinyCC); it needs no header or library, so it stays available in both
// the gen/c host build and the FastC self-host output. `0` is memory_order_relaxed.
fn C.__atomic_fetch_add(voidptr, u32, int) u32

// fastc_atomic_fetch_add_u32 atomically adds `delta` to `*ptr` and returns the
// previous value, used by the work-stealing per-file generator. Windows builds
// use InterlockedExchangeAdd instead (see parallel_windows.c.v); MSVC has no
// __atomic_fetch_add builtin.
fn fastc_atomic_fetch_add_u32(ptr &u32, delta u32) u32 {
	return C.__atomic_fetch_add(ptr, delta, 0)
}
