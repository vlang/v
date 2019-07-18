// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.sha256

fn test_crypto_sha256() {
	assert sha256.sum('This is a sha256 checksum.'.bytes()).hex() == 'DC7163299659529EAE29683EB1FFEC50D6C8FC7275ECB10C145FDE0E125B8727'
}
