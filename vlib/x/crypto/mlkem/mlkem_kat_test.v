module mlkem

fn test_keygen_encaps_decaps() {
	ek, dk := keygen() or { panic(err) }
	ct, ss1 := encaps(ek) or { panic(err) }
	ss2 := decaps(dk, ct) or { panic(err) }
	assert ss1 == ss2
	assert ct.data.len == ct_size
	assert ek.data.len == ek_size
	assert dk.data.len == dk_size
}
