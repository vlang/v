module wyrand

fn C._wymix(u64, u64) u64

// free should be called when the generator is no longer needed
@[unsafe]
pub fn (mut rng WyRandRNG) free() {
	unsafe { free(rng) }
}

// u64 returns a pseudorandom 64bit int in range `[0, 2⁶⁴)`.
@[ignore_overflow; inline]
pub fn (mut rng WyRandRNG) u64() u64 {
	unsafe {
		mut seed1 := rng.state
		seed1 += wyp0
		rng.state = seed1
		return C._wymix(seed1 ^ wyp1, seed1)
	}
	return 0
}
