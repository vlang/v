// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha512

fn test_crypto_sha512() {
	assert sha512.sum512('This is a sha512 checksum.'.bytes()).hex() == '4143e55fcba7e39b20f62a1368e5eb28f64a8859458886117ac66027832e0f9f5263daec688c439d2d0fa07059334668d39e59543039703dbb7e03ec9da7f8d7'
}
