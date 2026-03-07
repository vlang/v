module mldsa

import benchmark

const iters = 100

fn test_benchmark_keygen_44() {
	seed := []u8{len: 32, init: u8(index)}
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = PrivateKey.from_seed(seed, .ml_dsa_44) or { panic(err) }
	}
	bmark.measure('${iters}x keygen_44')
}

fn test_benchmark_keygen_65() {
	seed := []u8{len: 32, init: u8(index)}
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = PrivateKey.from_seed(seed, .ml_dsa_65) or { panic(err) }
	}
	bmark.measure('${iters}x keygen_65')
}

fn test_benchmark_keygen_87() {
	seed := []u8{len: 32, init: u8(index)}
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = PrivateKey.from_seed(seed, .ml_dsa_87) or { panic(err) }
	}
	bmark.measure('${iters}x keygen_87')
}

fn test_benchmark_sign_44() {
	seed := []u8{len: 32, init: u8(index)}
	sk := PrivateKey.from_seed(seed, .ml_dsa_44) or { panic(err) }
	msg := 'bench'.bytes()
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = sk.sign(msg) or { panic(err) }
	}
	bmark.measure('${iters}x sign_44')
}

fn test_benchmark_sign_65() {
	seed := []u8{len: 32, init: u8(index)}
	sk := PrivateKey.from_seed(seed, .ml_dsa_65) or { panic(err) }
	msg := 'bench'.bytes()
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = sk.sign(msg) or { panic(err) }
	}
	bmark.measure('${iters}x sign_65')
}

fn test_benchmark_sign_87() {
	seed := []u8{len: 32, init: u8(index)}
	sk := PrivateKey.from_seed(seed, .ml_dsa_87) or { panic(err) }
	msg := 'bench'.bytes()
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = sk.sign(msg) or { panic(err) }
	}
	bmark.measure('${iters}x sign_87')
}

fn test_benchmark_verify_44() {
	seed := []u8{len: 32, init: u8(index)}
	sk := PrivateKey.from_seed(seed, .ml_dsa_44) or { panic(err) }
	pk := sk.public_key()
	msg := 'bench'.bytes()
	sig := sk.sign(msg) or { panic(err) }
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = pk.verify(msg, sig) or { panic(err) }
	}
	bmark.measure('${iters}x verify_44')
}

fn test_benchmark_verify_65() {
	seed := []u8{len: 32, init: u8(index)}
	sk := PrivateKey.from_seed(seed, .ml_dsa_65) or { panic(err) }
	pk := sk.public_key()
	msg := 'bench'.bytes()
	sig := sk.sign(msg) or { panic(err) }
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = pk.verify(msg, sig) or { panic(err) }
	}
	bmark.measure('${iters}x verify_65')
}

fn test_benchmark_verify_87() {
	seed := []u8{len: 32, init: u8(index)}
	sk := PrivateKey.from_seed(seed, .ml_dsa_87) or { panic(err) }
	pk := sk.public_key()
	msg := 'bench'.bytes()
	sig := sk.sign(msg) or { panic(err) }
	mut bmark := benchmark.start()
	for _ in 0 .. iters {
		_ = pk.verify(msg, sig) or { panic(err) }
	}
	bmark.measure('${iters}x verify_87')
}
