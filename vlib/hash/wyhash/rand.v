// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wyhash

pub fn rand_u64(seed &u64) u64 {
	mut seed0 := seed
	unsafe{
		mut seed1 := *seed0
		seed1 += wyp0
		*seed0 = seed1
		return wymum(seed1^wyp1, seed1)
	}	
	return 0
}
