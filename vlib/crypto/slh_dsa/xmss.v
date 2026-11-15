// Copyright (c) 2026 V Language Association. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// XMSS multi-time signatures and the SLH-DSA hypertree
// (FIPS 205, Sections 6 and 7).
module slh_dsa

// xmss_node computes the root of a Merkle subtree of WOTS+ public keys
// (FIPS 205, Algorithm 9).
fn xmss_node(sk_seed []u8, i u32, z u32, pk_seed []u8, p Params, adrs Adrs) []u8 {
	if z == 0 {
		mut l_adrs := adrs
		l_adrs.set_type_and_clear(.wots_hash)
		l_adrs.set_key_pair_address(i)
		return wots_pkgen(sk_seed, pk_seed, p, l_adrs)
	}
	mut m_adrs := adrs
	lnode := xmss_node(sk_seed, 2 * i, z - 1, pk_seed, p, m_adrs)
	rnode := xmss_node(sk_seed, 2 * i + 1, z - 1, pk_seed, p, m_adrs)
	m_adrs.set_type_and_clear(.tree)
	m_adrs.set_tree_height(z)
	m_adrs.set_tree_index(i)
	mut m := []u8{cap: 2 * p.n}
	m << lnode
	m << rnode
	return h_hash(pk_seed, m_adrs, m, p.n)
}

// xmss_sign generates an XMSS signature on an n-byte message
// (FIPS 205, Algorithm 10).
fn xmss_sign(msg []u8, sk_seed []u8, idx u32, pk_seed []u8, p Params, adrs Adrs) []u8 {
	mut w_adrs := adrs
	w_adrs.set_type_and_clear(.wots_hash)
	w_adrs.set_key_pair_address(idx)
	mut sig := wots_sign(msg, sk_seed, pk_seed, p, w_adrs)
	for j in 0 .. p.hp {
		k := (idx >> u32(j)) ^ u32(1)
		sig << xmss_node(sk_seed, k, u32(j), pk_seed, p, adrs)
	}
	return sig
}

// xmss_pk_from_sig computes an XMSS public key from a signature
// (FIPS 205, Algorithm 11).
fn xmss_pk_from_sig(idx u32, sig_xmss []u8, msg []u8, pk_seed []u8, p Params, adrs Adrs) []u8 {
	wots_len := p.len() * p.n
	sig := sig_xmss[..wots_len]
	auth := sig_xmss[wots_len..]
	mut w_adrs := adrs
	w_adrs.set_type_and_clear(.wots_hash)
	w_adrs.set_key_pair_address(idx)
	mut node0 := wots_pk_from_sig(sig, msg, pk_seed, p, w_adrs)
	mut t_adrs := adrs
	t_adrs.set_type_and_clear(.tree)
	t_adrs.set_tree_index(idx)
	for k in 0 .. p.hp {
		t_adrs.set_tree_height(u32(k) + 1)
		mut m := []u8{cap: 2 * p.n}
		if ((u64(idx) >> u32(k)) & 1) == 0 {
			t_adrs.set_tree_index(t_adrs.get_tree_index() / 2)
			m << node0
			m << auth[k * p.n..(k + 1) * p.n]
		} else {
			t_adrs.set_tree_index((t_adrs.get_tree_index() - 1) / 2)
			m << auth[k * p.n..(k + 1) * p.n]
			m << node0
		}
		node0 = h_hash(pk_seed, t_adrs, m, p.n)
	}
	return node0
}

// ht_sign generates a hypertree signature (FIPS 205, Algorithm 12).
fn ht_sign(msg []u8, sk_seed []u8, pk_seed []u8, p Params, idx_tree_in u64, idx_leaf_in u32) []u8 {
	mut idx_tree := idx_tree_in
	mut idx_leaf := idx_leaf_in
	mut adrs := new_adrs()
	adrs.set_tree_address(idx_tree)
	mut sig_ht := xmss_sign(msg, sk_seed, idx_leaf, pk_seed, p, adrs)
	mut root := xmss_pk_from_sig(idx_leaf, sig_ht, msg, pk_seed, p, adrs)
	for j in 1 .. p.d {
		idx_leaf = u32(idx_tree & ((u64(1) << u32(p.hp)) - 1))
		idx_tree >>= u32(p.hp)
		adrs.set_layer_address(u32(j))
		adrs.set_tree_address(idx_tree)
		sig_tmp := xmss_sign(root, sk_seed, idx_leaf, pk_seed, p, adrs)
		sig_ht << sig_tmp
		if j < p.d - 1 {
			root = xmss_pk_from_sig(idx_leaf, sig_tmp, root, pk_seed, p, adrs)
		}
	}
	return sig_ht
}

// ht_verify verifies a hypertree signature (FIPS 205, Algorithm 13).
fn ht_verify(msg []u8, sig_ht []u8, pk_seed []u8, p Params, idx_tree_in u64, idx_leaf_in u32, pk_root []u8) bool {
	mut idx_tree := idx_tree_in
	mut idx_leaf := idx_leaf_in
	xmss_len := (p.hp + p.len()) * p.n
	mut adrs := new_adrs()
	adrs.set_tree_address(idx_tree)
	mut node := xmss_pk_from_sig(idx_leaf, sig_ht[..xmss_len], msg, pk_seed, p, adrs)
	for j in 1 .. p.d {
		idx_leaf = u32(idx_tree & ((u64(1) << u32(p.hp)) - 1))
		idx_tree >>= u32(p.hp)
		adrs.set_layer_address(u32(j))
		adrs.set_tree_address(idx_tree)
		sig_tmp := sig_ht[j * xmss_len..(j + 1) * xmss_len]
		node = xmss_pk_from_sig(idx_leaf, sig_tmp, node, pk_seed, p, adrs)
	}
	return node == pk_root
}
