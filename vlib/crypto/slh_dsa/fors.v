// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// FORS few-time signatures (FIPS 205, Section 8).
module slh_dsa

// fors_skgen generates a FORS private-key value (FIPS 205, Algorithm 14).
fn fors_skgen(sk_seed []u8, pk_seed []u8, p Params, adrs Adrs, idx u32) []u8 {
	mut sk_adrs := adrs
	sk_adrs.set_type_and_clear(.fors_prf)
	sk_adrs.set_key_pair_address(adrs.get_key_pair_address())
	sk_adrs.set_tree_index(idx)
	return prf(pk_seed, sk_adrs, sk_seed, p.n)
}

// fors_node computes the root of a Merkle subtree of FORS public values
// (FIPS 205, Algorithm 15).
fn fors_node(sk_seed []u8, i u32, z u32, pk_seed []u8, p Params, adrs Adrs) []u8 {
	if z == 0 {
		sk := fors_skgen(sk_seed, pk_seed, p, adrs, i)
		mut l_adrs := adrs
		l_adrs.set_tree_height(0)
		l_adrs.set_tree_index(i)
		return f_hash(pk_seed, l_adrs, sk, p.n)
	}
	mut m_adrs := adrs
	lnode := fors_node(sk_seed, 2 * i, z - 1, pk_seed, p, m_adrs)
	rnode := fors_node(sk_seed, 2 * i + 1, z - 1, pk_seed, p, m_adrs)
	m_adrs.set_tree_height(z)
	m_adrs.set_tree_index(i)
	mut m := []u8{cap: 2 * p.n}
	m << lnode
	m << rnode
	return h_hash(pk_seed, m_adrs, m, p.n)
}

// fors_sign generates a FORS signature on a message digest
// (FIPS 205, Algorithm 16).
fn fors_sign(md []u8, sk_seed []u8, pk_seed []u8, p Params, adrs Adrs) []u8 {
	indices := base_2b(md, p.a, p.k)
	mut sig_fors := []u8{cap: p.k * (p.a + 1) * p.n}
	two_a := u32(1) << u32(p.a)
	for i in 0 .. p.k {
		sig_fors << fors_skgen(sk_seed, pk_seed, p, adrs, u32(i) * two_a + indices[i])
		for j in 0 .. p.a {
			s := (indices[i] >> u32(j)) ^ u32(1)
			sig_fors << fors_node(sk_seed, u32(i) * (two_a >> u32(j)) + s, u32(j),
				pk_seed, p, adrs)
		}
	}
	return sig_fors
}

// fors_pk_from_sig computes a FORS public key from a signature
// (FIPS 205, Algorithm 17).
fn fors_pk_from_sig(sig_fors []u8, md []u8, pk_seed []u8, p Params, adrs Adrs) []u8 {
	indices := base_2b(md, p.a, p.k)
	mut root := []u8{cap: p.k * p.n}
	for i in 0 .. p.k {
		base := i * (p.a + 1) * p.n
		sk := sig_fors[base..base + p.n]
		auth := sig_fors[base + p.n..base + (p.a + 1) * p.n]
		mut v_adrs := adrs
		v_adrs.set_tree_height(0)
		two_a := u32(1) << u32(p.a)
		v_adrs.set_tree_index(u32(i) * two_a + indices[i])
		mut node0 := f_hash(pk_seed, v_adrs, sk, p.n)
		for j in 0 .. p.a {
			v_adrs.set_tree_height(u32(j) + 1)
			mut m := []u8{cap: 2 * p.n}
			if ((indices[i] >> u32(j)) & 1) == 0 {
				v_adrs.set_tree_index(v_adrs.get_tree_index() / 2)
				m << node0
				m << auth[j * p.n..(j + 1) * p.n]
			} else {
				v_adrs.set_tree_index((v_adrs.get_tree_index() - 1) / 2)
				m << auth[j * p.n..(j + 1) * p.n]
				m << node0
			}
			node0 = h_hash(pk_seed, v_adrs, m, p.n)
		}
		root << node0
	}
	mut forspk_adrs := adrs
	forspk_adrs.set_type_and_clear(.fors_roots)
	forspk_adrs.set_key_pair_address(adrs.get_key_pair_address())
	return t_hash(pk_seed, forspk_adrs, root, p.n)
}
