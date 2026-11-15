// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// SLH-DSA key generation, signing and verification
// (FIPS 205, Sections 9 and 10.1-10.3, pure variant).
module slh_dsa

import crypto.rand

// PrivateKey is an SLH-DSA private key: SK.seed, SK.prf, PK.seed, PK.root.
pub struct PrivateKey {
pub:
	params  ParameterSet
	sk_seed []u8
	sk_prf  []u8
	pk_seed []u8
	pk_root []u8
}

// PublicKey is an SLH-DSA public key: PK.seed, PK.root.
pub struct PublicKey {
pub:
	params  ParameterSet
	pk_seed []u8
	pk_root []u8
}

// p returns the parameter set of the private key.
pub fn (sk PrivateKey) p() Params {
	return get_params(sk.params)
}

// public_key returns the public key matching the private key.
pub fn (sk PrivateKey) public_key() PublicKey {
	return PublicKey{
		params:  sk.params
		pk_seed: sk.pk_seed.clone()
		pk_root: sk.pk_root.clone()
	}
}

// bytes serializes the private key as SK.seed || SK.prf || PK.seed || PK.root.
pub fn (sk PrivateKey) bytes() []u8 {
	p := sk.p()
	mut out := []u8{cap: p.sk_len()}
	out << sk.sk_seed
	out << sk.sk_prf
	out << sk.pk_seed
	out << sk.pk_root
	return out
}

// private_key_from_bytes parses a serialized private key.
pub fn private_key_from_bytes(params ParameterSet, b []u8) !PrivateKey {
	p := get_params(params)
	if b.len != p.sk_len() {
		return error('slh_dsa: bad private key length ${b.len}, want ${p.sk_len()}')
	}
	return PrivateKey{
		params:  params
		sk_seed: b[..p.n].clone()
		sk_prf:  b[p.n..2 * p.n].clone()
		pk_seed: b[2 * p.n..3 * p.n].clone()
		pk_root: b[3 * p.n..4 * p.n].clone()
	}
}

// p returns the parameter set of the public key.
pub fn (pk PublicKey) p() Params {
	return get_params(pk.params)
}

// bytes serializes the public key as PK.seed || PK.root.
pub fn (pk PublicKey) bytes() []u8 {
	mut out := []u8{cap: pk.pk_seed.len + pk.pk_root.len}
	out << pk.pk_seed
	out << pk.pk_root
	return out
}

// public_key_from_bytes parses a serialized public key.
pub fn public_key_from_bytes(params ParameterSet, b []u8) !PublicKey {
	p := get_params(params)
	if b.len != p.pk_len() {
		return error('slh_dsa: bad public key length ${b.len}, want ${p.pk_len()}')
	}
	return PublicKey{
		params:  params
		pk_seed: b[..p.n].clone()
		pk_root: b[p.n..].clone()
	}
}

// check_supported reports an error for parameter sets without a pure-V
// instantiation (currently all SLH-DSA-SHA2 sets).
fn check_supported(p Params) ! {
	if !p.is_shake {
		return error('slh_dsa: ${p.name} not implemented (SHA2 instantiation)')
	}
}

// keygen_internal generates a key pair from the given seeds
// (FIPS 205, Algorithm 18).
pub fn keygen_internal(params ParameterSet, sk_seed []u8, sk_prf []u8, pk_seed []u8) !(PrivateKey, PublicKey) {
	p := get_params(params)
	check_supported(p)!
	if sk_seed.len != p.n || sk_prf.len != p.n || pk_seed.len != p.n {
		return error('slh_dsa: bad seed length for ${p.name}')
	}
	mut adrs := new_adrs()
	adrs.set_layer_address(u32(p.d - 1))
	pk_root := xmss_node(sk_seed, 0, u32(p.hp), pk_seed, p, adrs)
	sk := PrivateKey{
		params:  params
		sk_seed: sk_seed.clone()
		sk_prf:  sk_prf.clone()
		pk_seed: pk_seed.clone()
		pk_root: pk_root.clone()
	}
	return sk, sk.public_key()
}

// keygen generates a random SLH-DSA key pair (FIPS 205, Algorithm 21).
pub fn keygen(params ParameterSet) !(PrivateKey, PublicKey) {
	p := get_params(params)
	check_supported(p)!
	sk_seed := rand.bytes(p.n)!
	sk_prf := rand.bytes(p.n)!
	pk_seed := rand.bytes(p.n)!
	return keygen_internal(params, sk_seed, sk_prf, pk_seed)
}

// sign_internal generates a signature on an internal message M.
// With addrnd == none the deterministic variant is used (opt_rand = PK.seed);
// otherwise the hedged variant uses the given n-byte randomness
// (FIPS 205, Algorithm 19).
pub fn sign_internal(msg []u8, sk PrivateKey, addrnd ?[]u8) ![]u8 {
	p := sk.p()
	check_supported(p)!
	mut opt_rand := []u8{}
	if r := addrnd {
		if r.len != p.n {
			return error('slh_dsa: bad addrnd length ${r.len}, want ${p.n}')
		}
		opt_rand = r.clone()
	} else {
		opt_rand = sk.pk_seed.clone()
	}
	r := prf_msg(sk.sk_prf, opt_rand, msg, p.n)
	mut sig := []u8{cap: p.sig_len()}
	sig << r
	digest := h_msg(r, sk.pk_seed, sk.pk_root, msg, p.m)
	md := digest[..p.md_len()].clone()
	mut off := p.md_len()
	tmp_idx_tree := digest[off..off + p.idx_tree_len()].clone()
	off += p.idx_tree_len()
	tmp_idx_leaf := digest[off..off + p.idx_leaf_len()].clone()
	idx_tree := mask64(to_int(tmp_idx_tree), p.h - p.hp)
	idx_leaf := u32(mask64(to_int(tmp_idx_leaf), p.hp))
	mut adrs := new_adrs()
	adrs.set_tree_address(idx_tree)
	adrs.set_type_and_clear(.fors_tree)
	adrs.set_key_pair_address(idx_leaf)
	sig_fors := fors_sign(md, sk.sk_seed, sk.pk_seed, p, adrs)
	sig << sig_fors
	pk_fors := fors_pk_from_sig(sig_fors, md, sk.pk_seed, p, adrs)
	sig << ht_sign(pk_fors, sk.sk_seed, sk.pk_seed, p, idx_tree, idx_leaf)
	return sig
}

// verify_internal verifies a signature on an internal message M
// (FIPS 205, Algorithm 20).
pub fn verify_internal(msg []u8, sig []u8, pk PublicKey) bool {
	p := pk.p()
	if !p.is_shake {
		return false
	}
	if sig.len != p.sig_len() {
		return false
	}
	r := sig[..p.n]
	fors_len := p.k * (1 + p.a) * p.n
	sig_fors := sig[p.n..p.n + fors_len]
	sig_ht := sig[p.n + fors_len..]
	digest := h_msg(r, pk.pk_seed, pk.pk_root, msg, p.m)
	md := digest[..p.md_len()].clone()
	mut off := p.md_len()
	tmp_idx_tree := digest[off..off + p.idx_tree_len()].clone()
	off += p.idx_tree_len()
	tmp_idx_leaf := digest[off..off + p.idx_leaf_len()].clone()
	idx_tree := mask64(to_int(tmp_idx_tree), p.h - p.hp)
	idx_leaf := u32(mask64(to_int(tmp_idx_leaf), p.hp))
	mut adrs := new_adrs()
	adrs.set_tree_address(idx_tree)
	adrs.set_type_and_clear(.fors_tree)
	adrs.set_key_pair_address(idx_leaf)
	pk_fors := fors_pk_from_sig(sig_fors, md, pk.pk_seed, p, adrs)
	return ht_verify(pk_fors, sig_ht, pk.pk_seed, p, idx_tree, idx_leaf, pk.pk_root)
}

// pure_msg builds the internal message M' = 0x00 || len(ctx) || ctx || M
// for pure signing (FIPS 205, Algorithm 22).
fn pure_msg(msg []u8, ctx []u8) ![]u8 {
	if ctx.len > 255 {
		return error('slh_dsa: context string too long (${ctx.len} > 255 bytes)')
	}
	mut m := []u8{cap: 2 + ctx.len + msg.len}
	m << u8(0)
	m << u8(ctx.len)
	m << ctx
	m << msg
	return m
}

// sign generates a hedged pure SLH-DSA signature with the given context
// string (FIPS 205, Algorithm 22).
pub fn sign(msg []u8, ctx []u8, sk PrivateKey) ![]u8 {
	p := sk.p()
	check_supported(p)!
	mut m := pure_msg(msg, ctx)!
	addrnd := rand.bytes(p.n)!
	return sign_internal(m, sk, addrnd)
}

// sign_deterministic generates a deterministic pure SLH-DSA signature with
// the given context string (FIPS 205, Algorithm 22 without addrnd).
pub fn sign_deterministic(msg []u8, ctx []u8, sk PrivateKey) ![]u8 {
	check_supported(sk.p())!
	m := pure_msg(msg, ctx)!
	return sign_internal(m, sk, none)
}

// verify verifies a pure SLH-DSA signature with the given context string
// (FIPS 205, Algorithm 24).
pub fn verify(msg []u8, sig []u8, ctx []u8, pk PublicKey) bool {
	m := pure_msg(msg, ctx) or { return false }
	return verify_internal(m, sig, pk)
}
