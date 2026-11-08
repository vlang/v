module mlkem

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
