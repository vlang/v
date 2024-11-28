// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.md5
import hash

// verify md5.Digest implements hash.Hash
fn test_digest_implements_hash() {
	get_digest := fn () hash.Hash {
		return md5.new()
	}
}

fn test_crypto_md5() {
	assert md5.sum('this is a md5 checksum.'.bytes()).hex() == '6fb421ff99036547655984da12973431'
}

fn test_crypto_md5_writer() {
	mut digest := md5.new()
	digest.write('this is a'.bytes()) or { assert false }
	digest.write(' md5 checksum.'.bytes()) or { assert false }
	mut sum := digest.sum([])
	assert sum.hex() == '6fb421ff99036547655984da12973431'
	sum = digest.sum([])
	assert sum.hex() == '6fb421ff99036547655984da12973431'
}

fn test_crypto_md5_writer_reset() {
	mut digest := md5.new()
	digest.write('this is a'.bytes()) or { assert false }
	digest.write(' md5 checksum.'.bytes()) or { assert false }
	_ = digest.sum([])
	digest.reset()
	digest.write('this is a'.bytes()) or { assert false }
	digest.write(' md5 checksum.'.bytes()) or { assert false }
	sum := digest.sum([])
	assert sum.hex() == '6fb421ff99036547655984da12973431'
}
