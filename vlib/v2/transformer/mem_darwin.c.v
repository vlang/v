// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import os

#include <malloc/malloc.h>

struct C.malloc_statistics_t {
	blocks_in_use   u32
	size_in_use     usize
	max_size_in_use usize
	size_allocated  usize
}

fn C.malloc_default_zone() voidptr
fn C.malloc_zone_statistics(zone voidptr, stats &C.malloc_statistics_t)

// t_print_mem reports live malloc bytes at a transformer sub-phase boundary.
// Gated on V2_MEM. Under -gc none the value is monotonic, so deltas between
// stages are the exact bytes each sub-phase allocated.
fn t_print_mem(stage string) {
	if os.getenv('V2_MEM') == '' {
		return
	}
	mut st := C.malloc_statistics_t{}
	C.malloc_zone_statistics(C.malloc_default_zone(), &st)
	eprintln('  [mem]   transform/${stage}: live ${u64(st.size_in_use) / (1024 * 1024)} MB')
}
