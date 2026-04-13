import hash

fn test_hash_compiles() {
	assert hash.sum64_string('abc', 5).hex_full() == 'ecc9659080b91a33'

	// Regression vectors for V's bundled wyhash implementation.
	assert hash.sum64_string('', 0).hex_full() == '93228a4de0eec5a2'
	assert hash.sum64_string('a', 1).hex_full() == 'de7c00cc90a98e24'
	assert hash.sum64_string('abc', 2).hex_full() == '41981296238e0d1d'
	assert hash.sum64_string('message digest', 3).hex_full() == '41bba71e1ae831d7'
	assert hash.sum64_string('abcdefghijklmnopqrstuvwxyz', 4).hex_full() == '065f27868866278a'
	assert hash.sum64_string('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789', 5).hex_full() == 'b9121e0f1a9bdd97'
	assert hash.sum64_string('12345678901234567890123456789012345678901234567890123456789012345678901234567890',
		6).hex_full() == 'a54abb9fbc9e4e82'
	assert hash.sum64([]u8{len: 1}, 0).hex_full() == '34e55bcc2fdda5ac'
	assert hash.sum64([]u8{len: 4}, 0).hex_full() == '58229876e5c11304'
	assert hash.sum64([]u8{len: 8}, 0).hex_full() == '0a4670f5c0e67d5b'
	assert hash.wyhash64_c(u64(1234567890), u64(7777777777)) == 13699604260906621654
	assert hash.wymum(u64(1234567890), u64(7777777777)) == 9602194699039780530
}
