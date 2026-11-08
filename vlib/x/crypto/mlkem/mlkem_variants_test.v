module mlkem

fn test_variants_roundtrip() {
	for v in [MlKemVariant.ml_kem_512, .ml_kem_768, .ml_kem_1024] {
		pk, sk := generate_keypair(v) or { panic(err) }
		ct, ss1 := encapsulate(pk) or { panic(err) }
		ss2 := decapsulate(ct, sk) or { panic(err) }
		assert ss1 == ss2
		p := params_of(v)
		assert ct.data.len == p.ct_size
		assert pk.data.len == p.ek_size
		assert sk.data.len == p.dk_size
	}
}

fn test_variant_512() {
	pk, sk := generate_keypair(.ml_kem_512) or { panic(err) }
	ct, ss1 := encapsulate(pk) or { panic(err) }
	ss2 := decapsulate(ct, sk) or { panic(err) }
	assert ss1 == ss2
}

fn test_variant_1024() {
	pk, sk := generate_keypair(.ml_kem_1024) or { panic(err) }
	ct, ss1 := encapsulate(pk) or { panic(err) }
	ss2 := decapsulate(ct, sk) or { panic(err) }
	assert ss1 == ss2
}
