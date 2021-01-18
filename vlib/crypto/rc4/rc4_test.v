// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.rc4

fn test_crypto_rc4() {
	key := 'tthisisourrc4key'.bytes()

	mut c := rc4.new_cipher(key) or {
		println(err)
		return
	}

	mut src := 'toencrypt'.bytes()

	// src & dst same, encrypt in place
	c.xor_key_stream(mut src, mut src) // encrypt data

	c.reset()

	assert src.hex() == '189a39a91aea8afa65'
}
