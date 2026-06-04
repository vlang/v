// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

#include <malloc/malloc.h>

struct C.malloc_statistics_t {
	blocks_in_use   u32
	size_in_use     usize
	max_size_in_use usize
	size_allocated  usize
}

fn C.malloc_default_zone() voidptr
fn C.malloc_zone_statistics(zone voidptr, stats &C.malloc_statistics_t)

// darwin_live_malloc_bytes returns (current live malloc bytes, peak live bytes)
// from the default malloc zone. Under `-gc none` (no frees) the current value
// is monotonic across phases, so per-phase deltas are the exact number of bytes
// each phase allocated and never released. This is the reliable counterpart to
// runtime.used_memory(), whose resident_size reading is distorted by OS paging.
fn darwin_live_malloc_bytes() (u64, u64) {
	mut st := C.malloc_statistics_t{}
	C.malloc_zone_statistics(C.malloc_default_zone(), &st)
	return u64(st.size_in_use), u64(st.max_size_in_use)
}
