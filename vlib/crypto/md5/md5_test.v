// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.md5

fn test_crypto_md5() {
	assert md5.sum('this is a md5 checksum.'.bytes()).hex() == '6fb421ff99036547655984da12973431'
}
