// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.md5

fn test_crypto_md5() {
	assert md5.sum('this is a md5 checksum.'.bytes()).hex() == '6fb421ff99036547655984da12973431'
}

fn test_crypto_md5_writer() {
	mut digest := md5.new()
	digest.write('this is a'.bytes()) or {
		assert false
		return
	}
	digest.write(' md5 checksum.'.bytes()) or {
		assert false
		return
	}
	sum := digest.sum([])
	assert sum.hex() == '6fb421ff99036547655984da12973431'
}

fn test_crypto_md5_writer_reset() {
	mut digest := md5.new()
	digest.write('this is a'.bytes()) or {
		assert false
		return
	}
	digest.write(' md5 checksum.'.bytes()) or {
		assert false
		return
	}
	_ = digest.sum([])
	digest.reset()
	digest.write('this is a'.bytes()) or {
		assert false
		return
	}
	digest.write(' md5 checksum.'.bytes()) or {
		assert false
		return
	}
	sum := digest.sum([])
	assert sum.hex() == '6fb421ff99036547655984da12973431'
}
