module fastc

import runtime

#flag -I @VEXEROOT/thirdparty/stdatomic/win

#include "@VEXEROOT/thirdparty/stdatomic/win/atomic.h"

// atomic_fetch_add_u32 is provided by the bundled stdatomic win header, where it
// expands to InterlockedExchangeAdd. MSVC has no GCC/Clang __atomic_fetch_add
// builtin, and the compiler's own atomic lowering uses InterlockedExchangeAdd too.
fn C.atomic_fetch_add_u32(voidptr, u32) u32

// fastc_nr_cpus reports the host's online CPU count for parallel generation.
// Windows hosts are never FastC-selfhost-compiled, so the runtime module is
// safe to use here.
fn fastc_nr_cpus() int {
	return runtime.nr_jobs()
}

// fastc_atomic_fetch_add_u32 atomically adds `delta` to `*ptr` and returns the
// previous value, used by the work-stealing per-file generator (parallel_nix.c.v
// carries the GCC/Clang/TCC counterpart for Unix builds).
fn fastc_atomic_fetch_add_u32(ptr &u32, delta u32) u32 {
	return C.atomic_fetch_add_u32(voidptr(ptr), delta)
}
