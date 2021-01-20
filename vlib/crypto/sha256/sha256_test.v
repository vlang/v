// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.sha256

fn test_crypto_sha256() {
	assert sha256.sum('This is a sha256 checksum.'.bytes()).hex() ==
		'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}
