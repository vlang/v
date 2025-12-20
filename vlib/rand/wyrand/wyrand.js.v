module wyrand

import hash

pub fn (mut r WyRandRNG) free() {}

@[ignore_overflow; inline]
pub fn (mut rng WyRandRNG) u64() u64 {
	unsafe {
		mut seed1 := rng.state
		seed1 += wyp0
		rng.state = seed1
		return hash.wymum(seed1 ^ wyp1, seed1)
	}
	return 0
}
