// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// BEAM backend crypto/rand implementation
module rand

// read returns an array of `bytes_needed` random bytes read from the OS.
// On BEAM: Would use crypto:strong_rand_bytes/1 for cryptographically secure random bytes
pub fn read(bytes_needed int) ![]u8 {
	// BEAM codegen handles this - translates to crypto:strong_rand_bytes(N)
	// Returns cryptographically secure random bytes
	mut buffer := []u8{len: bytes_needed}
	// Stub: actual implementation would fill buffer with random bytes
	return buffer
}
