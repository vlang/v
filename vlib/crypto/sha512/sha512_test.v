// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha512
import hash

// verify sha512.Digest implements hash.Hash
fn test_digest_implements_hash() {
	get_digest := fn () hash.Hash {
		return sha512.new()
	}
}

const final_result = '4143e55fcba7e39b20f62a1368e5eb28f64a8859458886117ac66027832e0f9f5263daec688c439d2d0fa07059334668d39e59543039703dbb7e03ec9da7f8d7'

fn test_crypto_sha512() {
	assert sha512.sum512('This is a sha512 checksum.'.bytes()).hex() == final_result
}

fn test_crypto_sha512_writer() {
	mut digest := sha512.new_digest(.sha512)
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha512 checksum.'.bytes()) or { assert false }
	// mut sum := digest.checksum()
	mut sum := digest.sum([])
	assert sum.hex() == final_result
	sum = digest.sum([])
	assert sum.hex() == final_result
}

fn test_crypto_sha512_writer_reset() {
	mut digest := sha512.new_digest(.sha512)
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha512 checksum.'.bytes()) or { assert false }
	_ = digest.sum([])
	digest.reset()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha512 checksum.'.bytes()) or { assert false }
	sum := digest.sum([])
	assert sum.hex() == final_result
}

fn test_crypto_sha512_384() {
	data := 'example bytes'.bytes()
	sum384 := sha512.sum384(data)
	expected := '8004e0038985a2d4dc40802b149f02cdd5868eaa58d87fae02f0cce2a3b566a6af63e34b11d5ba88c6035b96e587a6d6'
	assert sum384.hex() == expected

	mut d := sha512.new384()
	d.write(data) or { assert false }
	sum := d.sum([])
	assert sum.hex() == expected

	d.reset()
	d.write(data) or { assert false }
	chksum := d.sum([])
	assert chksum.hex() == expected
}

fn test_crypto_sha512_224() {
	data := 'example bytes'.bytes()
	sum512_224 := sha512.sum512_224(data)
	expected := '2bcbe17a1c3cb7b2b8b75c3118ed8525b6a4c505f2e59f3dc5dfe462'
	assert sum512_224.hex() == expected

	mut d := sha512.new512_224()
	d.write(data) or { assert false }
	sum := d.sum([])
	assert sum.hex() == expected

	d.reset()
	d.write(data) or { assert false }
	chksum := d.sum([])
	assert chksum.hex() == expected
}

fn test_crypto_sha512_256() {
	data := 'example bytes'.bytes()
	sum512_256 := sha512.sum512_256(data)
	expected := '984512a8f874623cf1e2d5bd85c7d1240214163db0ebd0919922768f94879563'
	assert sum512_256.hex() == expected

	mut d := sha512.new512_256()
	d.write(data) or { assert false }
	sum := d.sum([])
	assert sum.hex() == expected

	d.reset()
	d.write(data) or { assert false }
	chksum := d.sum([])
	assert chksum.hex() == expected
}
