// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha256
import hash

// verify sha256.Digest implements hash.Hash
fn test_digest_implements_hash() {
	get_digest := fn () hash.Hash {
		return sha256.new()
	}
}

fn test_crypto_sha256() {
	assert sha256.sum('This is a sha256 checksum.'.bytes()).hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}

fn test_crypto_sha256_writer() {
	mut digest := sha256.new()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha256 checksum.'.bytes()) or { assert false }
	mut sum := digest.sum([])
	assert sum.hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
	sum = digest.sum([])
	assert sum.hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}

fn test_crypto_sha256_writer_reset() {
	mut digest := sha256.new()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha256 checksum.'.bytes()) or { assert false }
	_ = digest.sum([])
	digest.reset()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha256 checksum.'.bytes()) or { assert false }
	sum := digest.sum([])
	assert sum.hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}

fn test_crypto_sha256_224() {
	data := 'hello world\n'.bytes()
	mut digest := sha256.new224()
	expected := '95041dd60ab08c0bf5636d50be85fe9790300f39eb84602858a9b430'

	// with sum224 function
	sum224 := sha256.sum224(data)
	assert sum224.hex() == expected

	// with sum
	_ := digest.write(data)!
	sum := digest.sum([])
	assert sum.hex() == expected

	// with checksum
	digest.reset()
	_ := digest.write(data)!
	chksum := digest.sum([])
	assert chksum.hex() == expected
}
