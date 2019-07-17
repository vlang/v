// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import crypto.sha512

fn test_crypto_sha512() {	 
	assert sha512.sum512('This is a sha512 checksum.'.bytes()).hex() == '4143E55FCBA7E39B20F62A1368E5EB28F64A8859458886117AC66027832E0F9F5263DAEC688C439D2D0FA07059334668D39E59543039703DBB7E03EC9DA7F8D7'
}
