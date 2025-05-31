module wyrand

// free should be called when the generator is no longer needed
@[unsafe]
pub fn (mut rng WyRandRNG) free() {
	unsafe { free(rng) }
}
