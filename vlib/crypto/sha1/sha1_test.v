// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha1

fn test_crypto_sha1() {
	assert sha1.sum('This is a sha1 checksum.'.bytes()).hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
}

fn test_crypto_sha1_writer() {
	mut digest := sha1.new()
	digest.write('This is a'.bytes()) or {
		assert false
		return
	}
	digest.write(' sha1 checksum.'.bytes()) or {
		assert false
		return
	}
	sum := digest.sum([])
	assert sum.hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
}

fn test_crypto_sha1_writer_reset() {
	mut digest := sha1.new()
	digest.write('This is a'.bytes()) or {
		assert false
		return
	}
	digest.write(' sha1 checksum.'.bytes()) or {
		assert false
		return
	}
	_ = digest.sum([])
	digest.reset()
	digest.write('This is a'.bytes()) or {
		assert false
		return
	}
	digest.write(' sha1 checksum.'.bytes()) or {
		assert false
		return
	}
	sum := digest.sum([])
	assert sum.hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
}
