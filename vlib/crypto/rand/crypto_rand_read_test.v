import crypto.rand

fn test_reading() ? {
	a := rand.read(32) ?
	// dump(a.hex())
	assert a.len == 32
	mut histogram := [256]int{}
	for b in a {
		histogram[b]++
	}
	// dump(histogram)
	for h in histogram {
		assert h < 10
	}
}
