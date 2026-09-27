// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

// Exercises util_notd_use_openssl.v's 3 stub functions -- planned in this
// PR's own Phase 2 but never actually written. Under -d use_openssl,
// pubkey_from_bytes/pubkey_from_string/privkey_from_string resolve to the
// real, working implementations in util_d_use_openssl.v instead, so each
// stub-error test below skips itself via `$if use_openssl ?` rather than
// asserting failure when the whole crypto.ecdsa directory happens to be run
// with -d use_openssl explicitly (this file carries no vtest vflags of its
// own to force the flag either way).

fn test_pubkey_from_bytes_stub_error() {
	$if use_openssl ? {
		eprintln('skipping: pubkey_from_bytes IS implemented under -d use_openssl')
		return
	}
	pubkey_from_bytes([u8(4), 1, 2, 3]) or {
		assert err.msg().contains('-d use_openssl')
		return
	}
	assert false, 'pubkey_from_bytes should not be implemented on the default backend'
}

fn test_pubkey_from_string_stub_error() {
	$if use_openssl ? {
		eprintln('skipping: pubkey_from_string IS implemented under -d use_openssl')
		return
	}
	pubkey_from_string('-----BEGIN PUBLIC KEY-----\n-----END PUBLIC KEY-----') or {
		assert err.msg().contains('-d use_openssl')
		return
	}
	assert false, 'pubkey_from_string should not be implemented on the default backend'
}

fn test_privkey_from_string_stub_error() {
	$if use_openssl ? {
		eprintln('skipping: privkey_from_string IS implemented under -d use_openssl')
		return
	}
	privkey_from_string('-----BEGIN PRIVATE KEY-----\n-----END PRIVATE KEY-----') or {
		assert err.msg().contains('-d use_openssl')
		return
	}
	assert false, 'privkey_from_string should not be implemented on the default backend'
}

fn test_pubkey_bytes_delegates_to_uncompressed_bytes() ! {
	// PublicKey.bytes() (util_notd_use_openssl.v) is the one REAL
	// implementation in this file -- it delegates to uncompressed_bytes()
	// rather than duplicating the mbedTLS call sequence.
	_, priv_key := generate_key() or { panic(err) }
	defer { priv_key.free() }
	pub_key := priv_key.public_key()!
	defer { pub_key.free() }
	assert pub_key.bytes()! == pub_key.uncompressed_bytes()!
}
