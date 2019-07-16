// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.sha1

fn test_crypto_md5() {	 
	assert sha1.sum('this is a md5 checksum.'.bytes()).hex() == '6FB421FF99036547655984DA12973431'
}
