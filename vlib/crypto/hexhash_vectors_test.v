module crypto

import crypto.md5
import crypto.sha1
import crypto.sha256

// The `hexhash` helpers are thin wrappers over the `Digest` API, but they are
// the only entry point a caller reaches for when it wants a hex string, so the
// vectors below pin them against the published digests.

struct Vector {
	msg      string
	hash     string
	hash_224 string
}

// RFC 1321 appendix A.5, the MD5 test suite.
const md5_vectors = [
	Vector{
		msg:  ''
		hash: 'd41d8cd98f00b204e9800998ecf8427e'
	},
	Vector{
		msg:  'abc'
		hash: '900150983cd24fb0d6963f7d28e17f72'
	},
	Vector{
		msg:  'message digest'
		hash: 'f96b697d7cb7938d525a2f31aaf161d0'
	},
]

// FIPS 180-1 / RFC 3174, the SHA-1 examples.
const sha1_vectors = [
	Vector{
		msg:  ''
		hash: 'da39a3ee5e6b4b0d3255bfef95601890afd80709'
	},
	Vector{
		msg:  'abc'
		hash: 'a9993e364706816aba3e25717850c26c9cd0d89d'
	},
	Vector{
		msg:  'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'
		hash: '84983e441c3bd26ebaae4aa1f95129e5e54670f1'
	},
]

// FIPS 180-2 appendix B.1 and B.2, the SHA-256 and SHA-224 examples.
const sha256_vectors = [
	Vector{
		msg:      ''
		hash:     'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
		hash_224: 'd14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f'
	},
	Vector{
		msg:      'abc'
		hash:     'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
		hash_224: '23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7'
	},
]

const million_a = 'a'.repeat(1_000_000)

fn test_md5_hexhash() {
	for v in md5_vectors {
		assert md5.hexhash(v.msg) == v.hash, 'md5(${v.msg.len} bytes)'
	}
}

fn test_sha1_hexhash() {
	for v in sha1_vectors {
		assert sha1.hexhash(v.msg) == v.hash, 'sha1(${v.msg.len} bytes)'
	}
	// The FIPS 180-1 long message: one million 'a' characters.
	assert sha1.hexhash(million_a) == '34aa973cd4c4daa4f61eeb2bdbad27316534016f'
}

fn test_sha256_hexhash() {
	for v in sha256_vectors {
		assert sha256.hexhash(v.msg) == v.hash, 'sha256(${v.msg.len} bytes)'
	}
	assert sha256.hexhash(million_a) == 'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0'
}

fn test_sha256_hexhash_224() {
	for v in sha256_vectors {
		assert sha256.hexhash_224(v.msg) == v.hash_224, 'sha224(${v.msg.len} bytes)'
	}
}
