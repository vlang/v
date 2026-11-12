// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

// This is the DEFAULT backend (flag `use_openssl` absent): the PEM/DER key
// loaders parse with mbedTLS's pk module (pkparse.c, already linked by this
// backend, see ecdsa_notd_use_openssl.c.v), then rebuild the key through the
// backend's own paths: a private key from its scalar (as new_key_from_seed
// does), a public key from its uncompressed point (as
// PublicKey.from_uncompressed_bytes does). Only EC keys on the curves `Nid`
// names are accepted, as on the OpenSSL backend.

// pubkey_from_bytes loads a PublicKey from DER-encoded bytes in der
// (a SubjectPublicKeyInfo).
pub fn pubkey_from_bytes(bytes []u8) !PublicKey {
	if bytes.len == 0 {
		return error('crypto.ecdsa: empty public key bytes')
	}
	return public_key_from_pk(bytes)
}

// bytes gets the public key as an uncompressed EC point -- identical to
// PublicKey.uncompressed_bytes() (ecdsa_notd_use_openssl.v), which IS
// implemented for this backend; delegates rather than duplicating the
// mbedTLS call sequence.
pub fn (pbk PublicKey) bytes() ![]u8 {
	return pbk.uncompressed_bytes()
}

// pubkey_from_string loads a PublicKey from a PEM-formatted string
// (`-----BEGIN PUBLIC KEY-----`).
pub fn pubkey_from_string(s string) !PublicKey {
	if s.len == 0 {
		return error('crypto.ecdsa: empty PEM string')
	}
	// mbedTLS reads PEM from a NUL-terminated buffer whose length counts the NUL.
	mut buf := s.bytes()
	buf << 0
	return public_key_from_pk(buf)
}

// privkey_from_string loads a PrivateKey from a PEM-formatted string: an
// unencrypted SEC1 (`-----BEGIN EC PRIVATE KEY-----`) or PKCS#8
// (`-----BEGIN PRIVATE KEY-----`) key.
pub fn privkey_from_string(s string) !PrivateKey {
	if s.len == 0 {
		return error('crypto.ecdsa: empty PEM string')
	}
	mut buf := s.bytes()
	buf << 0
	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	init_rng(mut ctr_drbg, mut entropy)!
	defer {
		free_rng(mut ctr_drbg, mut entropy)
	}
	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}
	ret := C.mbedtls_pk_parse_key(&pk, buf.data, uint(buf.len), unsafe { nil }, 0, C.mbedtls_ctr_drbg_random,
		voidptr(&ctr_drbg))
	if ret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_pk_parse_key failed (not an unencrypted EC private key?)',
			ret)
	}
	keypair := ec_keypair(&pk)!
	nid := nid_of_group(C.mbedtls_ecp_keypair_get_group_id(keypair))!
	mut scalar := []u8{len: nid.byte_size()}
	mut olen := uint(0)
	wret := C.mbedtls_ecp_write_key_ext(keypair, &olen, scalar.data, uint(scalar.len))
	if wret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_write_key_ext failed', wret)
	}
	defer {
		unsafe { vmemset(scalar.data, 0, scalar.len) }
	}
	return new_key_from_seed(scalar[..int(olen)], nid: nid, fixed_size: true)
}

// public_key_from_pk parses a SubjectPublicKeyInfo, DER or NUL-terminated PEM.
fn public_key_from_pk(buf []u8) !PublicKey {
	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}
	ret := C.mbedtls_pk_parse_public_key(&pk, buf.data, uint(buf.len))
	if ret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_pk_parse_public_key failed (not an EC public key?)',
			ret)
	}
	keypair := ec_keypair(&pk)!
	nid := nid_of_group(C.mbedtls_ecp_keypair_get_group_id(keypair))!
	// An uncompressed point is 0x04 followed by both coordinates.
	mut point := []u8{len: 1 + 2 * nid.byte_size()}
	mut olen := uint(0)
	wret := C.mbedtls_ecp_write_public_key(keypair, C.MBEDTLS_ECP_PF_UNCOMPRESSED, &olen,
		point.data, uint(point.len))
	if wret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_write_public_key failed', wret)
	}
	return PublicKey.from_uncompressed_bytes(point[..int(olen)], nid: nid)
}

// ec_keypair is the EC key a parsed pk context holds; any other key type is refused.
fn ec_keypair(pk &C.mbedtls_pk_context) !&C.mbedtls_ecdsa_context {
	if C.mbedtls_pk_get_type(pk) != C.MBEDTLS_PK_ECKEY {
		return error('crypto.ecdsa: the key is not an EC key')
	}
	return C.mbedtls_pk_ec(*pk)
}

// nid_of_group maps an mbedTLS group id back to the curve `Nid` names.
fn nid_of_group(group int) !Nid {
	for nid in [Nid.prime256v1, .secp384r1, .secp521r1, .secp256k1] {
		if nid.mbedtls_group_id() == group {
			return nid
		}
	}
	return error('crypto.ecdsa: unsupported curve')
}
