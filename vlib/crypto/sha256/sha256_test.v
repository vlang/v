// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha256

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
