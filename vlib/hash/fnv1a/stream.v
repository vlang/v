module fnv1a

// Fnv1a64 is a streaming FNV-1a 64-bit hasher. It implements hash.Hasher:
// bytes are absorbed with `write` and the digest is read any number of times
// with `finish`.
pub struct Fnv1a64 {
mut:
	state u64
}

// new_fnv1a64 creates an Fnv1a64 in its initial (offset-basis) state.
pub fn new_fnv1a64() &Fnv1a64 {
	return &Fnv1a64{
		state: fnv64_offset_basis
	}
}

// write absorbs bytes into the hash state.
pub fn (mut h Fnv1a64) write(p []u8) ! {
	for b in p {
		h.state = (h.state ^ b) * fnv64_prime
	}
}

// finish returns the current 64-bit digest without consuming state.
pub fn (h &Fnv1a64) finish() u64 {
	return h.state
}

// sum returns the FNV-1a 64-bit digest of data in one shot.
pub fn (h &Fnv1a64) sum(data []u8) u64 {
	mut c := new_fnv1a64()
	c.write(data) or { return c.finish() }
	return c.finish()
}
