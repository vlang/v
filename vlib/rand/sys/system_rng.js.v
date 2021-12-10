// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sys

// Until there's a portable, JS has a seeded way to produce random numbers
// and not just Math.random(), use any of the existing implementations
// as the System's RNG
type SysRNG = WyRandRNG

// In the JS version, we simply return the same int as is normally generated.
[inline]
pub fn (r SysRNG) default_rand() int {
	return r.int()
}
