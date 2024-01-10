import hash

fn test_hash_compiles() {
	assert hash.sum64_string('abc', 5).hex_full() == 'ce2703347d216491'
}
