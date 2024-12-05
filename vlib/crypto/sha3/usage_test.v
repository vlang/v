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
	digest_keccak_256.write(input)!
	digest_keccak_512.write(input)!

	// Result should be same as shortened version
	assert digest_224.checksum() == sha3.sum224(input)
	assert digest_256.checksum() == sha3.sum256(input)
	assert digest_384.checksum() == sha3.sum384(input)
	assert digest_512.checksum() == sha3.sum512(input)
	assert digest_keccak_256.checksum() == sha3.keccak256(input)
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
	if should_be_error := sha3.new_digest(sha3.rate_224, sha3.size_256) {
		assert false, 'new_digest(sha3.rate_224, sha3.size_256) should be error'
	}
	if should_be_error := sha3.new_digest(sha3.rate_224, sha3.size_256, padding: .keccak) {
		assert false, 'new_digest(sha3.rate_224, sha3.size_256, padding: .keccak) should be error'
	}
	if should_be_error := sha3.new_digest(0, sha3.size_256) {
		assert false, 'new_digest(0, sha3.size_256) should be error'
	}
	if should_be_error := sha3.new_digest(0, sha3.size_256, padding: .keccak) {
		assert false, 'new_digest(0, sha3.size_256, padding: .keccak) should be error'
	}

	// Should be HashSizeError
	if should_be_error := sha3.new_digest(sha3.rate_256, -1) {
		assert false, 'new_digest(sha3.rate_256, -1) should be error'
	}
	if should_be_error := sha3.new_digest(sha3.rate_256, -1, padding: .keccak) {
		assert false, 'new_digest(sha3.rate_256, -1, padding: .keccak) should be error'
	}
	if should_be_error := sha3.new_digest(sha3.rate_256, 100) {
		assert false, 'new_digest(sha3.rate_256, -1) should be error'
	}
	if should_be_error := sha3.new_digest(sha3.rate_256, 100, padding: .keccak) {
		assert false, 'new_digest(sha3.rate_256, -1, padding: .keccak) should be error'
	}

	// Should be XOFSizeError
	if should_be_error := sha3.new128xof(-1) {
		assert false, 'new128xof(-1) should be error'
	}
	if should_be_error := sha3.new256xof(-1) {
		assert false, 'new256xof(-1) should be error'
	}
	// Shoud be XOFRateError
	if should_be_error := sha3.new_xof_digest(0, sha3.size_256) {
		assert false, 'new_xof_digest(0, sha3.size_256) should be error'
	}
}
