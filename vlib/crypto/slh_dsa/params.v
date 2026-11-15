// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Pure-V implementation of the SLH-DSA stateless hash-based signature
// scheme (FIPS 205). All 12 parameter sets are implemented: the SHAKE
// sets (Section 11.1) and the SHA2 sets (Section 11.2).
module slh_dsa

// ParameterSet identifies one of the 12 approved FIPS 205 parameter sets.
pub enum ParameterSet {
	shake_128s
	shake_128f
	shake_192s
	shake_192f
	shake_256s
	shake_256f
	sha2_128s
	sha2_128f
	sha2_192s
	sha2_192f
	sha2_256s
	sha2_256f
}

// Params holds a single SLH-DSA parameter set (FIPS 205, Table 2).
pub struct Params {
pub:
	id       ParameterSet
	name     string
	n        int  // security parameter: byte length of secret values and hashes
	h        int  // total height of the hypertree
	d        int  // number of XMSS layers in the hypertree
	hp       int  // height of each XMSS tree (h')
	a        int  // height of each FORS tree (t = 2^a leaves)
	k        int  // number of FORS trees
	lgw      int  // Winternitz parameter log2(w); always 4
	m        int  // message-digest length in bytes
	is_shake bool // true for the SHAKE instantiations (Section 11.1)
}

// all_params returns all 12 approved FIPS 205 parameter sets.
pub fn all_params() []Params {
	return [
		Params{ParameterSet.shake_128s, 'SLH-DSA-SHAKE-128s', 16, 63, 7, 9, 12, 14, 4, 30, true},
		Params{ParameterSet.shake_128f, 'SLH-DSA-SHAKE-128f', 16, 66, 22, 3, 6, 33, 4, 34, true},
		Params{ParameterSet.shake_192s, 'SLH-DSA-SHAKE-192s', 24, 63, 7, 9, 14, 17, 4, 39, true},
		Params{ParameterSet.shake_192f, 'SLH-DSA-SHAKE-192f', 24, 66, 22, 3, 8, 33, 4, 42, true},
		Params{ParameterSet.shake_256s, 'SLH-DSA-SHAKE-256s', 32, 64, 8, 8, 14, 22, 4, 47, true},
		Params{ParameterSet.shake_256f, 'SLH-DSA-SHAKE-256f', 32, 68, 17, 4, 9, 35, 4, 49, true},
		Params{ParameterSet.sha2_128s, 'SLH-DSA-SHA2-128s', 16, 63, 7, 9, 12, 14, 4, 30, false},
		Params{ParameterSet.sha2_128f, 'SLH-DSA-SHA2-128f', 16, 66, 22, 3, 6, 33, 4, 34, false},
		Params{ParameterSet.sha2_192s, 'SLH-DSA-SHA2-192s', 24, 63, 7, 9, 14, 17, 4, 39, false},
		Params{ParameterSet.sha2_192f, 'SLH-DSA-SHA2-192f', 24, 66, 22, 3, 8, 33, 4, 42, false},
		Params{ParameterSet.sha2_256s, 'SLH-DSA-SHA2-256s', 32, 64, 8, 8, 14, 22, 4, 47, false},
		Params{ParameterSet.sha2_256f, 'SLH-DSA-SHA2-256f', 32, 68, 17, 4, 9, 35, 4, 49, false},
	]
}

// get_params returns the parameter set for the given identifier.
pub fn get_params(id ParameterSet) Params {
	for p in all_params() {
		if p.id == id {
			return p
		}
	}
	return all_params()[0]
}

// w returns the Winternitz parameter w = 2^lgw (FIPS 205, Eq. 5.1).
pub fn (p Params) w() int {
	return 1 << u32(p.lgw)
}

// len1 returns the WOTS+ message length (FIPS 205, Eq. 5.2).
pub fn (p Params) len1() int {
	return (8 * p.n + p.lgw - 1) / p.lgw
}

// len2 returns the WOTS+ checksum length (FIPS 205, Eq. 5.3).
pub fn (p Params) len2() int {
	mut x := p.len1() * (p.w() - 1)
	mut bits := 0
	for x > 0 {
		bits++
		x >>= 1
	}
	return (bits - 1) / p.lgw + 1
}

// len returns the total WOTS+ length len = len1 + len2 (FIPS 205, Eq. 5.4).
pub fn (p Params) len() int {
	return p.len1() + p.len2()
}

// sk_len returns the private-key length in bytes (4n).
pub fn (p Params) sk_len() int {
	return 4 * p.n
}

// pk_len returns the public-key length in bytes (2n).
pub fn (p Params) pk_len() int {
	return 2 * p.n
}

// sig_len returns the signature length in bytes.
pub fn (p Params) sig_len() int {
	return (1 + p.k * (1 + p.a) + p.h + p.d * p.len()) * p.n
}

// md_len returns the number of digest bytes signed by FORS (ceil(k*a/8)).
pub fn (p Params) md_len() int {
	return (p.k * p.a + 7) / 8
}

// idx_tree_len returns the number of digest bytes selecting the XMSS tree.
pub fn (p Params) idx_tree_len() int {
	return (p.h - p.hp + 7) / 8
}

// idx_leaf_len returns the number of digest bytes selecting the leaf.
pub fn (p Params) idx_leaf_len() int {
	return (p.hp + 7) / 8
}

// AdrsType encodes the seven SLH-DSA address types (FIPS 205, Section 4.2).
pub enum AdrsType as u32 {
	wots_hash  = 0
	wots_pk    = 1
	tree       = 2
	fors_tree  = 3
	fors_roots = 4
	wots_prf   = 5
	fors_prf   = 6
}

// Adrs is a 32-byte SLH-DSA address (FIPS 205, Figure 2): layer address
// (4 bytes), tree address (12 bytes), type (4 bytes) and 12 type-specific
// bytes. It is a value type and is copied whenever the pseudocode copies it.
pub struct Adrs {
mut:
	b [32]u8
}

// new_adrs returns the all-zero address.
fn new_adrs() Adrs {
	return Adrs{}
}

// bytes returns the 32-byte big-endian encoding of the address.
pub fn (a Adrs) bytes() []u8 {
	mut out := []u8{len: 32}
	for i in 0 .. 32 {
		out[i] = a.b[i]
	}
	return out
}

// set_layer_address sets the layer address (bytes 0..4).
pub fn (mut a Adrs) set_layer_address(l u32) {
	put_u32(mut a.b[0..4], l)
}

// set_tree_address sets the tree address (bytes 4..16).
pub fn (mut a Adrs) set_tree_address(t u64) {
	put_u64_12(mut a.b[4..16], t)
}

// set_type_and_clear sets the address type and zeroes the final 12 bytes.
pub fn (mut a Adrs) set_type_and_clear(t AdrsType) {
	put_u32(mut a.b[16..20], u32(t))
	for i in 20 .. 32 {
		a.b[i] = 0
	}
}

// set_key_pair_address sets the key pair address (bytes 20..24).
pub fn (mut a Adrs) set_key_pair_address(i u32) {
	put_u32(mut a.b[20..24], i)
}

// set_chain_address sets the WOTS+ chain address (bytes 24..28).
pub fn (mut a Adrs) set_chain_address(i u32) {
	put_u32(mut a.b[24..28], i)
}

// set_tree_height sets the tree height (bytes 24..28).
pub fn (mut a Adrs) set_tree_height(i u32) {
	put_u32(mut a.b[24..28], i)
}

// set_hash_address sets the hash address within a chain (bytes 28..32).
pub fn (mut a Adrs) set_hash_address(i u32) {
	put_u32(mut a.b[28..32], i)
}

// set_tree_index sets the tree index (bytes 28..32).
pub fn (mut a Adrs) set_tree_index(i u32) {
	put_u32(mut a.b[28..32], i)
}

// get_key_pair_address returns the key pair address (bytes 20..24).
pub fn (a Adrs) get_key_pair_address() u32 {
	return get_u32(a.b[20..24])
}

// get_tree_index returns the tree index (bytes 28..32).
pub fn (a Adrs) get_tree_index() u32 {
	return get_u32(a.b[28..32])
}

fn put_u32(mut b []u8, x u32) {
	b[0] = u8(x >> 24)
	b[1] = u8(x >> 16)
	b[2] = u8(x >> 8)
	b[3] = u8(x)
}

fn get_u32(b []u8) u32 {
	return (u32(b[0]) << 24) | (u32(b[1]) << 16) | (u32(b[2]) << 8) | u32(b[3])
}

fn put_u64_12(mut b []u8, x u64) {
	for i in 0 .. 12 {
		b[11 - i] = u8(x >> u32(8 * i))
	}
}

// to_int converts a byte string to an integer (FIPS 205, Algorithm 2).
fn to_int(x []u8) u64 {
	mut total := u64(0)
	for i in 0 .. x.len {
		total = 256 * total + u64(x[i])
	}
	return total
}

// to_byte converts an integer to an n-byte big-endian string
// (FIPS 205, Algorithm 3).
fn to_byte(x u64, n int) []u8 {
	mut total := x
	mut s := []u8{len: n}
	for i in 0 .. n {
		s[n - 1 - i] = u8(total)
		total >>= 8
	}
	return s
}

// mask64 returns x mod 2^b for b <= 64 (identity for b >= 64).
fn mask64(x u64, b int) u64 {
	if b >= 64 {
		return x
	}
	return x & ((u64(1) << u32(b)) - 1)
}

// base_2b converts a byte string into out_len base-2^b integers
// (FIPS 205, Algorithm 4).
fn base_2b(x []u8, b int, out_len int) []u32 {
	mut baseb := []u32{len: out_len}
	mut i := 0
	mut bits := 0
	mut total := u32(0)
	for out in 0 .. out_len {
		for bits < b {
			total = (total << 8) + u32(x[i])
			i++
			bits += 8
		}
		bits -= b
		baseb[out] = (total >> u32(bits)) & ((u32(1) << u32(b)) - 1)
	}
	return baseb
}
