// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha512

fn test_crypto_sha512() {
	assert sha512.sum512('This is a sha512 checksum.'.bytes()).hex() == '4143e55fcba7e39b20f62a1368e5eb28f64a8859458886117ac66027832e0f9f5263daec688c439d2d0fa07059334668d39e59543039703dbb7e03ec9da7f8d7'
}

fn test_crypto_sha512_writer() {
	mut digest := sha512.new_digest(.sha512)
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha512 checksum.'.bytes()) or { assert false }
	mut sum := digest.checksum()
	assert sum.hex() == '4143e55fcba7e39b20f62a1368e5eb28f64a8859458886117ac66027832e0f9f5263daec688c439d2d0fa07059334668d39e59543039703dbb7e03ec9da7f8d7'
	sum = digest.sum([])
	assert sum.hex() == '64922a82583d4e364010e574ca5ce6a15686e0d268bd41ac1003dda924356d3552e276b7baedacd85a6884e744943a92d931cebca4738510b4568c5fb5a49723'
	sum = digest.sum([])
	assert sum.hex() == '64922a82583d4e364010e574ca5ce6a15686e0d268bd41ac1003dda924356d3552e276b7baedacd85a6884e744943a92d931cebca4738510b4568c5fb5a49723'
}

fn test_crypto_sha512_writer_reset() {
	mut digest := sha512.new_digest(.sha512)
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha512 checksum.'.bytes()) or { assert false }
	_ = digest.checksum()
	digest.reset()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha512 checksum.'.bytes()) or { assert false }
	sum := digest.checksum()
	assert sum.hex() == '4143e55fcba7e39b20f62a1368e5eb28f64a8859458886117ac66027832e0f9f5263daec688c439d2d0fa07059334668d39e59543039703dbb7e03ec9da7f8d7'
}
