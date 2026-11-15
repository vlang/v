// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// SHAKE instantiations of the SLH-DSA tweakable hash functions
// (FIPS 205, Section 11.1) built on crypto.sha3.
module slh_dsa

import crypto.sha3

// h_msg computes the SLH-DSA message digest:
// Hmsg(R, PK.seed, PK.root, M) = SHAKE256(R || PK.seed || PK.root || M, 8m).
fn h_msg(r []u8, pk_seed []u8, pk_root []u8, msg []u8, out_len int) []u8 {
	mut data := []u8{cap: r.len + pk_seed.len + pk_root.len + msg.len}
	data << r
	data << pk_seed
	data << pk_root
	data << msg
	return sha3.shake256(data, out_len)
}

// prf_msg generates the message randomizer:
// PRFmsg(SK.prf, opt_rand, M) = SHAKE256(SK.prf || opt_rand || M, 8n).
fn prf_msg(sk_prf []u8, opt_rand []u8, msg []u8, n int) []u8 {
	mut data := []u8{cap: sk_prf.len + opt_rand.len + msg.len}
	data << sk_prf
	data << opt_rand
	data << msg
	return sha3.shake256(data, n)
}

// prf generates WOTS+ and FORS secret values:
// PRF(PK.seed, SK.seed, ADRS) = SHAKE256(PK.seed || ADRS || SK.seed, 8n).
fn prf(pk_seed []u8, adrs Adrs, sk_seed []u8, n int) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + sk_seed.len}
	data << pk_seed
	data << ab
	data << sk_seed
	return sha3.shake256(data, n)
}

// f_hash is the chaining function:
// F(PK.seed, ADRS, M1) = SHAKE256(PK.seed || ADRS || M1, 8n).
fn f_hash(pk_seed []u8, adrs Adrs, m1 []u8, n int) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + m1.len}
	data << pk_seed
	data << ab
	data << m1
	return sha3.shake256(data, n)
}

// h_hash compresses two child nodes:
// H(PK.seed, ADRS, M2) = SHAKE256(PK.seed || ADRS || M2, 8n).
fn h_hash(pk_seed []u8, adrs Adrs, m2 []u8, n int) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + m2.len}
	data << pk_seed
	data << ab
	data << m2
	return sha3.shake256(data, n)
}

// t_hash compresses len nodes:
// Tl(PK.seed, ADRS, Ml) = SHAKE256(PK.seed || ADRS || Ml, 8n).
fn t_hash(pk_seed []u8, adrs Adrs, ml []u8, n int) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + ml.len}
	data << pk_seed
	data << ab
	data << ml
	return sha3.shake256(data, n)
}
