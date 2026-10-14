module crypto

// The `crypto` module declares only the `Hash` enum: vlib/crypto/sha512 stores
// one of these values in its `Digest`, so the variant names and their ordering
// are the public surface worth pinning.

const all_variants = [
	Hash.md4,
	.md5,
	.sha1,
	.sha224,
	.sha256,
	.sha384,
	.sha512,
	.md5sha1,
	.ripemd160,
	.sha3_224,
	.sha3_256,
	.sha3_384,
	.sha3_512,
	.sha512_224,
	.sha512_256,
	.shake128,
	.shake256,
	.blake2s_256,
	.blake2b_256,
	.blake2b_384,
	.blake2b_512,
]

fn test_hash_variants_are_contiguous_and_distinct() {
	assert all_variants.len == 21
	for i, variant in all_variants {
		assert int(variant) == i
	}
}

fn test_hash_variant_names() {
	names := all_variants.map(it.str())
	assert names == [
		'md4',
		'md5',
		'sha1',
		'sha224',
		'sha256',
		'sha384',
		'sha512',
		'md5sha1',
		'ripemd160',
		'sha3_224',
		'sha3_256',
		'sha3_384',
		'sha3_512',
		'sha512_224',
		'sha512_256',
		'shake128',
		'shake256',
		'blake2s_256',
		'blake2b_256',
		'blake2b_384',
		'blake2b_512',
	]
}
