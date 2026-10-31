module maphash

import rand

// Seed is an opaque seed value used to initialize a Hasher.
pub struct Seed {
pub:
	s0 u64
	s1 u64
}

// make_seed returns a new random seed.
pub fn make_seed() Seed {
	return Seed{
		s0: rand.u64()
		s1: rand.u64()
	}
}

// Hasher is a hash function seeded with a Seed, suitable for use in hash
// tables. It is not cryptographically secure.
pub struct Hasher {
mut:
	seed    Seed
	state   u64
	written u64
}

// new_hasher returns a Hasher initialized with `seed`.
pub fn new_hasher(seed Seed) Hasher {
	return Hasher{
		seed:  seed
		state: seed.s0 ^ 0x9e3779b97f4a7c15
	}
}

// write adds `data` to the hash.
pub fn (mut h Hasher) write(data []u8) {
	for b in data {
		h.state ^= u64(b) + 0x9e3779b97f4a7c15 + (h.state << 6) + (h.state >> 2)
		h.state = h.state * 0x94d049bb133111b9
		h.written++
	}
}

// write_string adds `s` to the hash.
pub fn (mut h Hasher) write_string(s string) {
	h.write(s.bytes())
}

// write_u64 adds a u64 to the hash.
pub fn (mut h Hasher) write_u64(v u64) {
	mut buf := [u8(0), 0, 0, 0, 0, 0, 0, 0]
	for i in 0 .. 8 {
		buf[i] = u8(v >> u64((7 - i) * 8))
	}
	h.write(buf)
}

// sum64 returns the current 64-bit hash value.
pub fn (h Hasher) sum64() u64 {
	mut x := h.state ^ h.seed.s1
	x ^= x >> 33
	x *= 0xff51afd7ed558ccd
	x ^= x >> 33
	x *= 0xc4ceb9fe1a85ec53
	x ^= x >> 33
	return x
}

// reset resets the hasher to its initial state with the same seed.
pub fn (mut h Hasher) reset() {
	h.state = h.seed.s0 ^ 0x9e3779b97f4a7c15
	h.written = 0
}
