// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// WOTS+ one-time signatures (FIPS 205, Section 5).
module slh_dsa

// chain iterates F s times starting from index i (FIPS 205, Algorithm 5).
fn chain(x []u8, i u32, s u32, pk_seed []u8, p Params, mut adrs Adrs) []u8 {
	mut tmp := x.clone()
	for j in i .. i + s {
		adrs.set_hash_address(j)
		tmp = f_hash(pk_seed, adrs, tmp, p)
	}
	return tmp
}

// checksum converts a base-w message to its checksum digits.
fn wots_checksum(msg []u32, p Params) []u32 {
	mut csum := u32(0)
	for i in 0 .. p.len1() {
		csum += u32(p.w() - 1) - msg[i]
	}
	shift := (8 - (p.len2() * p.lgw) % 8) % 8
	csum <<= u32(shift)
	csum_bytes := to_byte(u64(csum), (p.len2() * p.lgw + 7) / 8)
	return base_2b(csum_bytes, p.lgw, p.len2())
}

// wots_pkgen generates a WOTS+ public key (FIPS 205, Algorithm 6).
fn wots_pkgen(sk_seed []u8, pk_seed []u8, p Params, adrs Adrs) []u8 {
	mut sk_adrs := adrs
	sk_adrs.set_type_and_clear(.wots_prf)
	sk_adrs.set_key_pair_address(adrs.get_key_pair_address())
	mut w_adrs := adrs
	mut tmp := []u8{cap: p.len() * p.n}
	for i in 0 .. p.len() {
		sk_adrs.set_chain_address(u32(i))
		sk := prf(pk_seed, sk_adrs, sk_seed, p)
		w_adrs.set_chain_address(u32(i))
		tmp << chain(sk, 0, u32(p.w() - 1), pk_seed, p, mut w_adrs)
	}
	mut wotspk_adrs := adrs
	wotspk_adrs.set_type_and_clear(.wots_pk)
	wotspk_adrs.set_key_pair_address(adrs.get_key_pair_address())
	return t_hash(pk_seed, wotspk_adrs, tmp, p)
}

// wots_sign generates a WOTS+ signature on an n-byte message
// (FIPS 205, Algorithm 7).
fn wots_sign(msg []u8, sk_seed []u8, pk_seed []u8, p Params, adrs Adrs) []u8 {
	mut full := base_2b(msg, p.lgw, p.len1())
	full << wots_checksum(full, p)
	mut sk_adrs := adrs
	sk_adrs.set_type_and_clear(.wots_prf)
	sk_adrs.set_key_pair_address(adrs.get_key_pair_address())
	mut w_adrs := adrs
	mut sig := []u8{cap: p.len() * p.n}
	for i in 0 .. p.len() {
		sk_adrs.set_chain_address(u32(i))
		sk := prf(pk_seed, sk_adrs, sk_seed, p)
		w_adrs.set_chain_address(u32(i))
		sig << chain(sk, 0, full[i], pk_seed, p, mut w_adrs)
	}
	return sig
}

// wots_pk_from_sig computes a WOTS+ public key from a signature
// (FIPS 205, Algorithm 8).
fn wots_pk_from_sig(sig []u8, msg []u8, pk_seed []u8, p Params, adrs Adrs) []u8 {
	mut full := base_2b(msg, p.lgw, p.len1())
	full << wots_checksum(full, p)
	mut w_adrs := adrs
	mut tmp := []u8{cap: p.len() * p.n}
	for i in 0 .. p.len() {
		w_adrs.set_chain_address(u32(i))
		elem := sig[i * p.n..(i + 1) * p.n]
		tmp << chain(elem, full[i], u32(p.w() - 1) - full[i], pk_seed, p, mut w_adrs)
	}
	mut wotspk_adrs := adrs
	wotspk_adrs.set_type_and_clear(.wots_pk)
	wotspk_adrs.set_key_pair_address(adrs.get_key_pair_address())
	return t_hash(pk_seed, wotspk_adrs, tmp, p)
}
