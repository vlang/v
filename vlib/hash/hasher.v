module hash

// Hasher is the interface for streaming 64-bit hashers: bytes are fed with
// `write` and the digest is read with `finish`, which must be repeatable
// without consuming state. It is the V-idiomatic equivalent of Rust's
// `std::hash::Hasher`, for use in generic hashed containers.
// See fnv1a.Fnv1a64 for a conforming implementation.
pub interface Hasher {
	// write feeds bytes into the hash state.
mut:
	write(p []u8) !
	// finish returns the current 64-bit digest without consuming state.
	finish() u64
}
