// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

import crypto.sha512

// test_sign_with_custom_hash_is_not_implemented locks in PrivateKey.sign()'s
// own documented restriction: .with_custom_hash signing is not implemented
// on the default mbedTLS backend (mbedtls_ecdsa_write_signature's RFC 6979
// deterministic-nonce path rejects MBEDTLS_MD_NONE outright -- see sign()'s
// own doc comment). This restriction applies only to the DEFAULT backend --
// under -d use_openssl the same call succeeds instead (see util_test.v's
// test_key_signing_verifying_with_custom_hash), so this test skips itself
// via `$if use_openssl ?` rather than asserting failure when the whole
// crypto.ecdsa directory is run with -d use_openssl explicitly (this file
// carries no vtest vflags of its own to force the flag either way).
fn test_sign_with_custom_hash_is_not_implemented() ! {
	$if use_openssl? {
		eprintln('skipping: .with_custom_hash signing IS implemented under -d use_openssl')
		return
	}
	pv := PrivateKey.new()!
	defer { pv.free() }
	opt := SignerOpts{
		hash_config: .with_custom_hash
		allow_custom_hash: true
		custom_hash: sha512.new()
	}
	pv.sign('a message'.bytes(), opt) or {
		assert err.msg().contains('-d use_openssl')
		return
	}
	assert false, '.with_custom_hash signing should not be implemented on the default backend'
}
