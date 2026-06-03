// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

#include <malloc/malloc.h>

struct C.malloc_statistics_t {
	blocks_in_use   u32
	size_in_use     usize
	max_size_in_use usize
	size_allocated  usize
}

fn C.malloc_default_zone() voidptr
fn C.malloc_zone_statistics(zone voidptr, stats &C.malloc_statistics_t)

// darwin_transform_live_mb returns the process's currently live malloc bytes
// (in MB) on macOS. Used by t_print_mem; only referenced from its `$if macos`
// branch, so the macOS-only C calls here never reach other platforms.
fn darwin_transform_live_mb() u64 {
	mut st := C.malloc_statistics_t{}
	C.malloc_zone_statistics(C.malloc_default_zone(), &st)
	return u64(st.size_in_use) / (1024 * 1024)
}
