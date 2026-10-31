import crypto.blake2s

fn test_checksum_is_repeatable_and_allows_more_writes() {
	for size in [0, 1, 63, 64, 65, 128] {
		input := []u8{len: size, init: u8(index)}
		mut digest := blake2s.new256()!
		digest.write(input)!
		expected := blake2s.sum256(input)
		assert digest.checksum() == expected
		assert digest.checksum() == expected
		digest.write('extra'.bytes())!
		mut combined := input.clone()
		combined << 'extra'.bytes()
		assert digest.checksum() == blake2s.sum256(combined)
	}
}

fn test_keyed_checksum_is_repeatable() {
	mut digest := blake2s.new_pmac256('secret'.bytes())!
	digest.write('message'.bytes())!
	first := digest.checksum()
	assert first.len == 32
	assert digest.checksum() == first
}
