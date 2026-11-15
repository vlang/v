// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Instantiations of the SLH-DSA tweakable hash functions Hmsg, PRFmsg, PRF,
// F, H and Tl (FIPS 205, Sections 11.1 and 11.2): SHAKE sets use
// crypto.sha3, SHA2 sets use crypto.sha256, crypto.sha512 and crypto.hmac.
// The wrappers dispatch on p.is_shake so all 12 parameter sets share the
// same WOTS+/XMSS/FORS/SLH code paths.
module slh_dsa

import crypto.hmac
import crypto.sha256
import crypto.sha3
import crypto.sha512

// h_msg computes the SLH-DSA message digest (FIPS 205, Algorithms 19-20,
// step Hmsg) and returns p.m bytes.
fn h_msg(r []u8, pk_seed []u8, pk_root []u8, msg []u8, p Params) []u8 {
	if p.is_shake {
		return h_msg_shake(r, pk_seed, pk_root, msg, p)
	}
	return h_msg_sha2(r, pk_seed, pk_root, msg, p)
}

// prf_msg generates the message randomizer (FIPS 205, Algorithm 19)
// and returns p.n bytes.
fn prf_msg(sk_prf []u8, opt_rand []u8, msg []u8, p Params) []u8 {
	if p.is_shake {
		return prf_msg_shake(sk_prf, opt_rand, msg, p)
	}
	return prf_msg_sha2(sk_prf, opt_rand, msg, p)
}

// prf generates WOTS+ and FORS secret values (FIPS 205, Section 4.1)
// and returns p.n bytes.
fn prf(pk_seed []u8, adrs Adrs, sk_seed []u8, p Params) []u8 {
	if p.is_shake {
		return prf_shake(pk_seed, adrs, sk_seed, p)
	}
	return prf_sha2(pk_seed, adrs, sk_seed, p)
}

// f_hash is the chaining function (FIPS 205, Section 4.1)
// and returns p.n bytes.
fn f_hash(pk_seed []u8, adrs Adrs, m1 []u8, p Params) []u8 {
	if p.is_shake {
		return f_shake(pk_seed, adrs, m1, p)
	}
	return f_sha2(pk_seed, adrs, m1, p)
}

// h_hash compresses two child nodes (FIPS 205, Section 4.1)
// and returns p.n bytes.
fn h_hash(pk_seed []u8, adrs Adrs, m2 []u8, p Params) []u8 {
	if p.is_shake {
		return h_shake(pk_seed, adrs, m2, p)
	}
	return h_sha2(pk_seed, adrs, m2, p)
}

// t_hash compresses len nodes (FIPS 205, Section 4.1) and returns p.n bytes.
fn t_hash(pk_seed []u8, adrs Adrs, ml []u8, p Params) []u8 {
	if p.is_shake {
		return t_shake(pk_seed, adrs, ml, p)
	}
	return t_sha2(pk_seed, adrs, ml, p)
}

// h_msg_shake computes the SLH-DSA message digest:
// Hmsg(R, PK.seed, PK.root, M) = SHAKE256(R || PK.seed || PK.root || M, 8m).
fn h_msg_shake(r []u8, pk_seed []u8, pk_root []u8, msg []u8, p Params) []u8 {
	mut data := []u8{cap: r.len + pk_seed.len + pk_root.len + msg.len}
	data << r
	data << pk_seed
	data << pk_root
	data << msg
	return sha3.shake256(data, p.m)
}

// prf_msg_shake generates the message randomizer:
// PRFmsg(SK.prf, opt_rand, M) = SHAKE256(SK.prf || opt_rand || M, 8n).
fn prf_msg_shake(sk_prf []u8, opt_rand []u8, msg []u8, p Params) []u8 {
	mut data := []u8{cap: sk_prf.len + opt_rand.len + msg.len}
	data << sk_prf
	data << opt_rand
	data << msg
	return sha3.shake256(data, p.n)
}

// prf_shake generates WOTS+ and FORS secret values:
// PRF(PK.seed, SK.seed, ADRS) = SHAKE256(PK.seed || ADRS || SK.seed, 8n).
fn prf_shake(pk_seed []u8, adrs Adrs, sk_seed []u8, p Params) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + sk_seed.len}
	data << pk_seed
	data << ab
	data << sk_seed
	return sha3.shake256(data, p.n)
}

// f_shake is the chaining function:
// F(PK.seed, ADRS, M1) = SHAKE256(PK.seed || ADRS || M1, 8n).
fn f_shake(pk_seed []u8, adrs Adrs, m1 []u8, p Params) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + m1.len}
	data << pk_seed
	data << ab
	data << m1
	return sha3.shake256(data, p.n)
}

// h_shake compresses two child nodes:
// H(PK.seed, ADRS, M2) = SHAKE256(PK.seed || ADRS || M2, 8n).
fn h_shake(pk_seed []u8, adrs Adrs, m2 []u8, p Params) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + m2.len}
	data << pk_seed
	data << ab
	data << m2
	return sha3.shake256(data, p.n)
}

// t_shake compresses len nodes:
// Tl(PK.seed, ADRS, Ml) = SHAKE256(PK.seed || ADRS || Ml, 8n).
fn t_shake(pk_seed []u8, adrs Adrs, ml []u8, p Params) []u8 {
	ab := adrs.bytes()
	mut data := []u8{cap: pk_seed.len + ab.len + ml.len}
	data << pk_seed
	data << ab
	data << ml
	return sha3.shake256(data, p.n)
}

// adrs_c returns the 22-byte compressed address used by the SHA2
// instantiations (FIPS 205, Figure 18):
// ADRS[3] || ADRS[8:16] || ADRS[19] || ADRS[20:32].
fn adrs_c(a Adrs) []u8 {
	mut out := []u8{len: 22}
	out[0] = a.b[3]
	for i in 0 .. 8 {
		out[1 + i] = a.b[8 + i]
	}
	out[9] = a.b[19]
	for i in 0 .. 12 {
		out[10 + i] = a.b[20 + i]
	}
	return out
}

// mgf1_256 is MGF1 with SHA-256 (RFC 8017, Appendix B.2.1).
fn mgf1_256(seed []u8, out_len int) []u8 {
	mut out := []u8{cap: out_len}
	mut ctr := u32(0)
	for out.len < out_len {
		mut data := []u8{cap: seed.len + 4}
		data << seed
		data << u8(ctr >> 24)
		data << u8(ctr >> 16)
		data << u8(ctr >> 8)
		data << u8(ctr)
		out << sha256.sum256(data)
		ctr++
	}
	return out[..out_len]
}

// mgf1_512 is MGF1 with SHA-512 (RFC 8017, Appendix B.2.1).
fn mgf1_512(seed []u8, out_len int) []u8 {
	mut out := []u8{cap: out_len}
	mut ctr := u32(0)
	for out.len < out_len {
		mut data := []u8{cap: seed.len + 4}
		data << seed
		data << u8(ctr >> 24)
		data << u8(ctr >> 16)
		data << u8(ctr >> 8)
		data << u8(ctr)
		out << sha512.sum512(data)
		ctr++
	}
	return out[..out_len]
}

// h_msg_sha2 computes the SLH-DSA message digest for the SHA2 sets:
// Hmsg(R, PK.seed, PK.root, M) =
// MGF1-SHA-256(R || PK.seed || SHA-256(R || PK.seed || PK.root || M), m)
// for n = 16 (Section 11.2.1) and the SHA-512 analogue for n = 24, 32
// (Section 11.2.2).
fn h_msg_sha2(r []u8, pk_seed []u8, pk_root []u8, msg []u8, p Params) []u8 {
	mut data := []u8{cap: r.len + pk_seed.len + pk_root.len + msg.len}
	data << r
	data << pk_seed
	data << pk_root
	data << msg
	if p.n == 16 {
		inner := sha256.sum256(data)
		mut seed := []u8{cap: r.len + pk_seed.len + inner.len}
		seed << r
		seed << pk_seed
		seed << inner
		return mgf1_256(seed, p.m)
	}
	inner := sha512.sum512(data)
	mut seed := []u8{cap: r.len + pk_seed.len + inner.len}
	seed << r
	seed << pk_seed
	seed << inner
	return mgf1_512(seed, p.m)
}

// prf_msg_sha2 generates the message randomizer for the SHA2 sets:
// PRFmsg(SK.prf, opt_rand, M) = Trunc_n(HMAC-SHA-256(SK.prf, opt_rand || M))
// for n = 16 (Section 11.2.1) and the HMAC-SHA-512 analogue for n = 24, 32
// (Section 11.2.2).
fn prf_msg_sha2(sk_prf []u8, opt_rand []u8, msg []u8, p Params) []u8 {
	mut data := []u8{cap: opt_rand.len + msg.len}
	data << opt_rand
	data << msg
	if p.n == 16 {
		return hmac.new(sk_prf, data, sha256.sum256, sha256.block_size)[..p.n]
	}
	return hmac.new(sk_prf, data, sha512.sum512, sha512.block_size)[..p.n]
}

// sha256_tweak hashes PK.seed || zeros || ADRS_c || suffix with SHA-256 and
// truncates to n bytes. It implements PRF and F for all SHA2 sets and H and
// Tl for the n = 16 sets (Sections 11.2.1 and 11.2.2).
fn sha256_tweak(pk_seed []u8, adrs Adrs, suffix []u8, p Params) []u8 {
	ac := adrs_c(adrs)
	mut data := []u8{cap: pk_seed.len + (64 - p.n) + ac.len + suffix.len}
	data << pk_seed
	for _ in 0 .. 64 - p.n {
		data << u8(0)
	}
	data << ac
	data << suffix
	return sha256.sum256(data)[..p.n]
}

// sha512_tweak hashes PK.seed || zeros || ADRS_c || suffix with SHA-512 and
// truncates to n bytes. It implements H and Tl for the n = 24, 32 SHA2 sets
// (Section 11.2.2).
fn sha512_tweak(pk_seed []u8, adrs Adrs, suffix []u8, p Params) []u8 {
	ac := adrs_c(adrs)
	mut data := []u8{cap: pk_seed.len + (128 - p.n) + ac.len + suffix.len}
	data << pk_seed
	for _ in 0 .. 128 - p.n {
		data << u8(0)
	}
	data << ac
	data << suffix
	return sha512.sum512(data)[..p.n]
}

// prf_sha2 generates WOTS+ and FORS secret values for the SHA2 sets:
// PRF(PK.seed, SK.seed, ADRS) =
// Trunc_n(SHA-256(PK.seed || toByte(0, 64-n) || ADRS_c || SK.seed)).
fn prf_sha2(pk_seed []u8, adrs Adrs, sk_seed []u8, p Params) []u8 {
	return sha256_tweak(pk_seed, adrs, sk_seed, p)
}

// f_sha2 is the chaining function for the SHA2 sets:
// F(PK.seed, ADRS, M1) =
// Trunc_n(SHA-256(PK.seed || toByte(0, 64-n) || ADRS_c || M1)).
fn f_sha2(pk_seed []u8, adrs Adrs, m1 []u8, p Params) []u8 {
	return sha256_tweak(pk_seed, adrs, m1, p)
}

// h_sha2 compresses two child nodes for the SHA2 sets: SHA-256 as in f_sha2
// for n = 16 (Section 11.2.1) and
// Trunc_n(SHA-512(PK.seed || toByte(0, 128-n) || ADRS_c || M2))
// for n = 24, 32 (Section 11.2.2).
fn h_sha2(pk_seed []u8, adrs Adrs, m2 []u8, p Params) []u8 {
	if p.n == 16 {
		return sha256_tweak(pk_seed, adrs, m2, p)
	}
	return sha512_tweak(pk_seed, adrs, m2, p)
}

// t_sha2 compresses len nodes for the SHA2 sets: SHA-256 as in f_sha2
// for n = 16 (Section 11.2.1) and
// Trunc_n(SHA-512(PK.seed || toByte(0, 128-n) || ADRS_c || Ml))
// for n = 24, 32 (Section 11.2.2).
fn t_sha2(pk_seed []u8, adrs Adrs, ml []u8, p Params) []u8 {
	if p.n == 16 {
		return sha256_tweak(pk_seed, adrs, ml, p)
	}
	return sha512_tweak(pk_seed, adrs, ml, p)
}
