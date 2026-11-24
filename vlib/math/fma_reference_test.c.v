module math

fn C.fma(f64, f64, f64) f64

struct FmaRandom {
mut:
	state u64
}

fn (mut random FmaRandom) next() f64 {
	random.state ^= random.state << 13
	random.state ^= random.state >> 7
	random.state ^= random.state << 17
	return f64_from_bits(random.state)
}

fn assert_fma_matches_reference(x f64, y f64, z f64) {
	want := C.fma(x, y, z)
	got := fma_software(x, y, z)
	if is_nan(want) {
		assert is_nan(got)
	} else {
		assert f64_bits(got) == f64_bits(want), '${f64_bits(x).hex()}, ${f64_bits(y).hex()}, ${f64_bits(z).hex()}: ${f64_bits(got).hex()} != ${f64_bits(want).hex()}'
	}
}

fn test_software_fma_matches_hosted_reference_across_binary64_exponents() {
	$if !freestanding {
		mut random := FmaRandom{ state: 0x937a1bd3c4e56f89 }
		for _ in 0 .. 100000 {
			x := random.next()
			y := random.next()
			z := random.next()
			assert_fma_matches_reference(x, y, z)
		}
		for _ in 0 .. 10000 {
			x := random.next()
			y := random.next()
			product := x * y
			if !is_nan(product) && !is_inf(product, 0) {
				assert_fma_matches_reference(x, y, -product)
			}
		}
	}
}
