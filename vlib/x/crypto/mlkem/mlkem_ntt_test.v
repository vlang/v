module mlkem

fn test_zeta_table() {
	// Zeta table sanity: zetas[0] = 17^0 = 1, zetas[64] = 17^1 = 17.
	assert zetas[0] == 1
	assert zetas[64] == 17
	assert zetas.len == 128
}

fn test_ntt_roundtrip() {
	mut f := Poly{}
	for i in 0 .. n {
		f[i] = i % q
	}
	orig := f
	ntt(mut f)
	inv_ntt(mut f)
	for i in 0 .. n {
		if f[i] != orig[i] {
			println('mismatch at ${i}: ${f[i]} vs ${orig[i]}')
			assert false
		}
	}
}

fn test_ntt_equiv() {
	mut f := Poly{}
	for i in 0 .. n {
		f[i] = (i * 37 + 11) % q
	}
	orig := f
	ntt(mut f)
	inv_ntt(mut f)
	for i in 0 .. n {
		assert f[i] == orig[i]
	}
}
