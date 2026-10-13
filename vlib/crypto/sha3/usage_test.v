import crypto.sha3

fn test_api_call() {
	input := 'abc'.bytes()

	// Digest generation
	mut digest_224 := sha3.new_digest(sha3.rate_224, sha3.size_224)!
	mut digest_256 := sha3.new_digest(sha3.rate_256, sha3.size_256)!
	mut digest_384 := sha3.new_digest(sha3.rate_384, sha3.size_384)!
	mut digest_512 := sha3.new_digest(sha3.rate_512, sha3.size_512)!
	mut digest_keccak_224 := sha3.new_digest(sha3.rate_224, sha3.size_224, padding: .keccak)!
	mut digest_keccak_256 := sha3.new_digest(sha3.rate_256, sha3.size_256, padding: .keccak)!
	mut digest_keccak_384 := sha3.new_digest(sha3.rate_384, sha3.size_384, padding: .keccak)!
	mut digest_keccak_512 := sha3.new_digest(sha3.rate_512, sha3.size_512, padding: .keccak)!

	// Result should be same as shortened version
	assert digest_224 == sha3.new224()!
	assert digest_256 == sha3.new256()!
	assert digest_384 == sha3.new384()!
	assert digest_512 == sha3.new512()!
	assert digest_keccak_256 == sha3.new256keccak()!
	assert digest_keccak_512 == sha3.new512keccak()!

	// Should not panic
	digest_224.write(input)!
	digest_256.write(input)!
	digest_384.write(input)!
	digest_512.write(input)!
	digest_keccak_224.write(input)!
	digest_keccak_256.write(input)!
	digest_keccak_384.write(input)!
	digest_keccak_512.write(input)!

	// Result should be same as shortened version
	assert digest_224.checksum() == sha3.sum224(input)
	assert digest_256.checksum() == sha3.sum256(input)
	assert digest_384.checksum() == sha3.sum384(input)
	assert digest_512.checksum() == sha3.sum512(input)
	assert digest_keccak_224.checksum().len == sha3.size_224
	assert digest_keccak_256.checksum() == sha3.keccak256(input)
	assert digest_keccak_384.checksum().len == sha3.size_384
	assert digest_keccak_512.checksum() == sha3.keccak512(input)

	for size in 1 .. 65 {
		// Digest generation
		mut digest_xof_128 := sha3.new_digest(sha3.xof_rate_128, size, padding: .xof)!
		mut digest_xof_256 := sha3.new_digest(sha3.xof_rate_256, size, padding: .xof)!

		// Result should be same as shortened version
		assert digest_xof_128 == sha3.new_xof_digest(sha3.xof_rate_128, size)!
		assert digest_xof_256 == sha3.new_xof_digest(sha3.xof_rate_256, size)!
		assert digest_xof_128 == sha3.new128xof(size)!
		assert digest_xof_256 == sha3.new256xof(size)!

		// Should not panic
		digest_xof_128.write(input)!
		digest_xof_256.write(input)!
	}

	// Should be AbsorptionRateError
	if _ := sha3.new_digest(sha3.rate_224, sha3.size_256) {
		assert false, 'new_digest(sha3.rate_224, sha3.size_256) should be error'
	}
	if _ := sha3.new_digest(sha3.rate_224, sha3.size_256, padding: .keccak) {
		assert false, 'new_digest(sha3.rate_224, sha3.size_256, padding: .keccak) should be error'
	}
	if _ := sha3.new_digest(0, sha3.size_256) {
		assert false, 'new_digest(0, sha3.size_256) should be error'
	}
	if _ := sha3.new_digest(0, sha3.size_256, padding: .keccak) {
		assert false, 'new_digest(0, sha3.size_256, padding: .keccak) should be error'
	}

	// Should be HashSizeError
	if _ := sha3.new_digest(sha3.rate_256, -1) {
		assert false, 'new_digest(sha3.rate_256, -1) should be error'
	}
	if _ := sha3.new_digest(sha3.rate_256, -1, padding: .keccak) {
		assert false, 'new_digest(sha3.rate_256, -1, padding: .keccak) should be error'
	}
	if _ := sha3.new_digest(sha3.rate_256, 100) {
		assert false, 'new_digest(sha3.rate_256, -1) should be error'
	}
	if _ := sha3.new_digest(sha3.rate_256, 100, padding: .keccak) {
		assert false, 'new_digest(sha3.rate_256, -1, padding: .keccak) should be error'
	}

	// Should be XOFSizeError
	if _ := sha3.new128xof(-1) {
		assert false, 'new128xof(-1) should be error'
	}
	if _ := sha3.new256xof(-1) {
		assert false, 'new256xof(-1) should be error'
	}
	// Shoud be XOFRateError
	if _ := sha3.new_xof_digest(0, sha3.size_256) {
		assert false, 'new_xof_digest(0, sha3.size_256) should be error'
	}
}

fn test_digest_hashes_a_second_message() {
	// a checksum must leave the digest reusable: the next message hashes
	// on its own, and further writes do not panic
	mut d := sha3.new256() or { panic(err) }
	d.write('abc'.bytes()) or { panic(err) }
	assert d.checksum() == sha3.sum256('abc'.bytes())
	d.write('xyz'.bytes()) or { panic(err) }
	assert d.checksum() == sha3.sum256('xyz'.bytes())
	d.write('a'.bytes()) or { panic(err) }
	d.write('b'.bytes()) or { panic(err) }
	assert d.checksum() == sha3.sum256('ab'.bytes())
	// reset restores the initial state explicitly too
	d.write('abc'.bytes()) or { panic(err) }
	d.reset()
	d.write('abc'.bytes()) or { panic(err) }
	assert d.checksum() == sha3.sum256('abc'.bytes())
}

fn test_digest_reset_boundaries_and_variants() {
	for size, rate in {
		sha3.size_224: sha3.rate_224
		sha3.size_256: sha3.rate_256
		sha3.size_384: sha3.rate_384
		sha3.size_512: sha3.rate_512
	} {
		for padding in [sha3.Padding.sha3, .keccak] {
			mut d := sha3.new_digest(rate, size, padding: padding)!
			for length in [0, rate - 1, rate, rate + 1, rate * 3 + 1] {
				data := []u8{len: length, init: u8(index)}
				mut fresh := sha3.new_digest(rate, size, padding: padding)!
				fresh.write(data)!
				d.write(data[..length / 2])!
				d.write(data[length / 2..])!
				assert d.checksum() == fresh.checksum()
			}
			d.write('discard'.bytes())!
			d.reset()
			mut fresh := sha3.new_digest(rate, size, padding: padding)!
			assert d.checksum() == fresh.checksum()
			assert d.checksum() == fresh.checksum()
		}
	}
	// Extended-output digests preserve their output size and absorption rate on reset.
	mut xof := sha3.new128xof(sha3.xof_rate_128 * 2 + 1)!
	xof.write('message'.bytes())!
	first := xof.checksum()
	xof.write('message'.bytes())!
	assert xof.checksum() == first
}
