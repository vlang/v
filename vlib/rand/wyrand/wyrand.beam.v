// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wyrand

// BEAM backend implementation for WyRandRNG.
// Uses a simple LCG (Linear Congruential Generator) since BEAM doesn't have direct
// access to the C _wymix function. In production BEAM code, you would use
// Erlang's :rand module for better randomness.

// free should be called when the generator is no longer needed
@[unsafe]
pub fn (mut rng WyRandRNG) free() {
	// No-op for BEAM - garbage collected
}

// u64 returns a pseudorandom 64bit int in range `[0, 2^64)`.
// This is a simplified implementation using LCG for the BEAM backend.
@[ignore_overflow; inline]
pub fn (mut rng WyRandRNG) u64() u64 {
	// Simple LCG implementation
	// Constants from Knuth's MMIX (using wrapping arithmetic via attribute)
	mut seed1 := rng.state
	seed1 = seed1 * u64(6364136223846793005) + u64(1442695040888963407)
	rng.state = seed1
	// Mix the bits for better distribution (splitmix64 finalizer)
	mut result := seed1
	result ^= result >> 33
	result = result * u64(0xff51afd7ed558ccd)
	result ^= result >> 33
	result = result * u64(0xc4ceb9fe1a85ec53)
	result ^= result >> 33
	return result
}
