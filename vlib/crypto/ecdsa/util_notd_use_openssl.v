// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

// This is the DEFAULT backend (flag `use_openssl` absent) -- see
// util_d_use_openssl.v for the real, OpenSSL-backed PEM/DER loaders. None of
// pubkey_from_bytes/pubkey_from_string/privkey_from_string have any external
// caller in this repo (grep-confirmed) -- deferred here as clear-error
// stubs, mirroring vlib/net/http/server_tls_d_use_openssl.v's own
// "not supported on this backend yet" stub precedent, rather than silently
// missing (an undefined-symbol compile error) or ported without the format-
// parsing work that's genuinely out of this cut's scope.

const notd_use_openssl_pem_error = 'crypto.ecdsa: PEM/DER key loading is not implemented for the default mbedTLS backend yet; build with -d use_openssl'

// pubkey_from_bytes loads a PublicKey from DER-encoded bytes in der.
// Not implemented for the default mbedTLS backend -- see this file's own
// module doc comment.
pub fn pubkey_from_bytes(bytes []u8) !PublicKey {
	return error(notd_use_openssl_pem_error)
}

// bytes gets the public key as an uncompressed EC point -- identical to
// PublicKey.uncompressed_bytes() (ecdsa_notd_use_openssl.v), which IS
// implemented for this backend; delegates rather than duplicating the
// mbedTLS call sequence.
pub fn (pbk PublicKey) bytes() ![]u8 {
	return pbk.uncompressed_bytes()
}

// pubkey_from_string loads a PublicKey from a PEM-formatted string.
// Not implemented for the default mbedTLS backend -- see this file's own
// module doc comment.
pub fn pubkey_from_string(s string) !PublicKey {
	return error(notd_use_openssl_pem_error)
}

// privkey_from_string loads a PrivateKey from a PEM-formatted string.
// Not implemented for the default mbedTLS backend -- see this file's own
// module doc comment.
pub fn privkey_from_string(s string) !PrivateKey {
	return error(notd_use_openssl_pem_error)
}
