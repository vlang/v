// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module seed

import time

// nr_next returns a next value based on the previous value `prev`.
[inline]
fn nr_next(prev u32) u32 {
	return prev * 1664525 + 1013904223
}

// time_seed_array returns the required number of u32s generated from system time.
[inline]
pub fn time_seed_array(count int) []u32 {
	ctime := time.now()
	mut seed := u32(ctime.unix_time() ^ ctime.microsecond)
	mut seed_data := []u32{cap: count}
	for _ in 0 .. count {
		seed = nr_next(seed)
		seed_data << nr_next(seed)
	}
	return seed_data
}

// time_seed_32 returns a 32-bit seed generated from system time.
[inline]
pub fn time_seed_32() u32 {
	return time_seed_array(1)[0]
}

// time_seed_64 returns a 64-bit seed generated from system time.
[inline]
pub fn time_seed_64() u64 {
	seed_data := time_seed_array(2)
	lower := u64(seed_data[0])
	upper := u64(seed_data[1])
	return lower | (upper << 32)
}
