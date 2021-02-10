// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module aes

// new_cipher_generic creates and returns a new cipher.Block
// this is the generiv v version, no arch optimisations
fn new_cipher_generic(key []byte) AesCipher {
	n := key.len + 28
	mut c := AesCipher{
		enc: []u32{len: (n)}
		dec: []u32{len: (n)}
	}
	expand_key_generic(key, mut c.enc, mut c.dec)
	return c
}
