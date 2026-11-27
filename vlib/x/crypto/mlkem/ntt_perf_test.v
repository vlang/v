module mlkem

import time

fn test_ntt_performance() {
	mut f := Poly{}
	for i in 0 .. n {
		f[i] = (i * 257 + 13) % q
	}
	// correctness gate first: one round-trip must be exact
	mut g := f
	ntt(mut g)
	inv_ntt(mut g)
	for i in 0 .. n {
		assert g[i] == f[i]
	}
	start := time.now()
	iters := 1000
	for _ in 0 .. iters {
		mut h := f
		ntt(mut h)
		inv_ntt(mut h)
	}
	elapsed := time.since(start)
	ms := elapsed.milliseconds()
	println('ntt roundtrip ${iters}x: ${ms} ms (${ms * 1000 / iters} us/iter)')
	assert ms < 120_000
}
