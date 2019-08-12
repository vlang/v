// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.rand

fn test_crypto_rand() {
	r := rand.read(100) or {
		assert false
		return
	}
	assert r.len == 100
}
