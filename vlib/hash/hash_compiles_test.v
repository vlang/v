import hash

fn test_hash_compiles() {
	assert hash.sum64_string('abc', 5).hex_full() == '4b4b66779c7a16f1'

	// official wyhash test vectors
	assert hash.sum64_string('', 0).hex_full() == '93228a4de0eec5a2'
	assert hash.sum64_string('a', 1).hex_full() == 'c5bac3db178713c4'
	assert hash.sum64_string('abc', 2).hex_full() == 'a97f2f7b1d9b3314'
	assert hash.sum64_string('message digest', 3).hex_full() == '786d1f1df3801df4'
	assert hash.sum64_string('abcdefghijklmnopqrstuvwxyz', 4).hex_full() == 'dca5a8138ad37c87'
	assert hash.sum64_string('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
		5).hex_full() == 'b9e734f117cfaf70'
	assert hash.sum64_string('12345678901234567890123456789012345678901234567890123456789012345678901234567890',
		6).hex_full() == '6cc5eab49a92d617'
	assert hash.wyhash64_c(u64(1234567890), u64(7777777777)) == 13699604260906621654
	assert hash.wymum(u64(1234567890), u64(7777777777)) == 9602194699039780530
}
