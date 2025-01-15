// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

import hash
import crypto
import crypto.sha256
import crypto.sha512

#flag darwin -L /opt/homebrew/opt/openssl/lib -I /opt/homebrew/opt/openssl/include

#flag -I/usr/include/openssl
#flag -lcrypto
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib
#include <openssl/ecdsa.h>
#include <openssl/obj_mac.h>
#include <openssl/objects.h>
#include <openssl/bn.h>

// C function declarations
fn C.EC_KEY_new_by_curve_name(nid int) &C.EC_KEY
fn C.EC_KEY_generate_key(key &C.EC_KEY) int
fn C.EC_KEY_free(key &C.EC_KEY)
fn C.EC_KEY_set_public_key(key &C.EC_KEY, &C.EC_POINT) int
fn C.EC_KEY_set_private_key(key &C.EC_KEY, prv &C.BIGNUM) int
fn C.EC_KEY_get0_group(key &C.EC_KEY) &C.EC_GROUP
fn C.EC_KEY_get0_private_key(key &C.EC_KEY) &C.BIGNUM
fn C.EC_KEY_get0_public_key(key &C.EC_KEY) &C.EC_POINT
fn C.EC_KEY_check_key(key &C.EC_KEY) int
fn C.EC_KEY_up_ref(key &C.EC_KEY) int
fn C.EC_POINT_new(group &C.EC_GROUP) &C.EC_POINT
fn C.EC_POINT_mul(group &C.EC_GROUP, r &C.EC_POINT, n &C.BIGNUM, q &C.EC_POINT, m &C.BIGNUM, ctx &C.BN_CTX) int
fn C.EC_POINT_cmp(group &C.EC_GROUP, a &C.EC_POINT, b &C.EC_POINT, ctx &C.BN_CTX) int
fn C.EC_POINT_free(point &C.EC_POINT)
fn C.BN_num_bits(a &C.BIGNUM) int
fn C.BN_bn2bin(a &C.BIGNUM, to &u8) int
fn C.BN_cmp(a &C.BIGNUM, b &C.BIGNUM) int
fn C.BN_CTX_new() &C.BN_CTX
fn C.BN_CTX_free(ctx &C.BN_CTX)
fn C.BN_bin2bn(s &u8, len int, ret &C.BIGNUM) &C.BIGNUM
fn C.BN_free(a &C.BIGNUM)
fn C.ECDSA_size(key &C.EC_KEY) u32
fn C.ECDSA_sign(type_ int, dgst &u8, dgstlen int, sig &u8, siglen &u32, eckey &C.EC_KEY) int
fn C.ECDSA_verify(type_ int, dgst &u8, dgstlen int, sig &u8, siglen int, eckey &C.EC_KEY) int

// NID constants
//
// NIST P-256 is refered to as secp256r1 and prime256v1, defined as #define NID_X9_62_prime256v1 415
// Different names, but they are all the same.
// https://www.rfc-editor.org/rfc/rfc4492.html#appendix-A
const nid_prime256v1 = C.NID_X9_62_prime256v1

// NIST P-384, ie, secp384r1 curve, defined as #define NID_secp384r1 715
const nid_secp384r1 = C.NID_secp384r1

// NIST P-521, ie, secp521r1 curve, defined as #define NID_secp521r1 716
const nid_secp521r1 = C.NID_secp521r1

// Bitcoin curve, defined as #define NID_secp256k1 714
const nid_secp256k1 = C.NID_secp256k1

// The list of supported curve(s)
pub enum Nid {
	prime256v1
	secp384r1
	secp521r1
	secp256k1
}

@[params]
pub struct CurveOptions {
pub mut:
	nid Nid = .prime256v1 // default to NIST P-256 curve
}

@[typedef]
struct C.EC_KEY {}

@[typedef]
struct C.EC_GROUP {}

@[typedef]
struct C.BIGNUM {}

@[typedef]
struct C.EC_POINT {}

@[typedef]
struct C.ECDSA_SIG {}

@[typedef]
struct C.BN_CTX {}

pub struct PrivateKey {
	key &C.EC_KEY
}

pub struct PublicKey {
	key &C.EC_KEY
}

// generate_key generates a new key pair. If opt was not provided, its default to prime256v1 curve.
// If you want another curve, use in the following manner: `pubkey, pivkey := ecdsa.generate_key(nid: .secp384r1)!`
pub fn generate_key(opt CurveOptions) !(PublicKey, PrivateKey) {
	ec_key := new_curve(opt)
	if ec_key == 0 {
		C.EC_KEY_free(ec_key)
		return error('Failed to create new EC_KEY')
	}
	res := C.EC_KEY_generate_key(ec_key)
	if res != 1 {
		C.EC_KEY_free(ec_key)
		return error('Failed to generate EC_KEY')
	}

	priv_key := PrivateKey{
		key: ec_key
	}
	pub_key := PublicKey{
		key: ec_key
	}
	return pub_key, priv_key
}

// new_key_from_seed creates a new private key from the seed bytes. If opt was not provided,
// its default to prime256v1 curve.
//
// Notes on the seed:
// You should make sure, the seed bytes was comes from cryptographically secure random generators,
// likes the `crypto.rand` or other trusted sources.
// Internally, the seed size's would be checked to not exceed the key size of underlying curve,
// ie, 32 bytes length for p-256 and secp256k1, 48 bytes length for p-384 and 64 bytes length for p-521.
// Its recommended to use seed with bytes length matching with underlying curve key size.
pub fn new_key_from_seed(seed []u8, opt CurveOptions) !PrivateKey {
	// Early exit check
	if seed.len == 0 {
		return error('Seed with null-length was not allowed')
	}
	// Create a new EC_KEY object with the specified curve
	ec_key := new_curve(opt)
	if ec_key == 0 {
		C.EC_KEY_free(ec_key)
		return error('Failed to create new EC_KEY')
	}
	// Retrieve the EC_GROUP object associated with the EC_KEY
	// Note:
	// Its cast-ed with voidptr() to workaround the strictness of the type system,
	// ie, cc backend with `-cstrict` option behaviour. Without this cast,
	// C.EC_KEY_get0_group expected to return `const EC_GROUP *`,
	// ie expected to return pointer into constant of EC_GROUP on C parts,
	// so, its make cgen not happy with this and would fail with error.
	group := voidptr(C.EC_KEY_get0_group(ec_key))
	if group == 0 {
		C.EC_KEY_free(ec_key)
		return error('Unable to load group')
	}
	// Adds early check for upper size, so, we dont hit unnecessary
	// call to math intensive calculation, conversion and checking routines.
	num_bits := C.EC_GROUP_get_degree(group)
	key_size := (num_bits + 7) / 8
	if seed.len > key_size {
		C.EC_KEY_free(ec_key)
		return error('Seed length exceeds key size')
	}

	// Convert the seed bytes into a BIGNUM
	bn := C.BN_bin2bn(seed.data, seed.len, 0)
	if bn == 0 {
		C.EC_KEY_free(ec_key)
		return error('Failed to create BIGNUM from seed')
	}
	// Set the BIGNUM as the private key in the EC_KEY object
	mut res := C.EC_KEY_set_private_key(ec_key, bn)
	if res != 1 {
		C.BN_free(bn)
		C.EC_KEY_free(ec_key)
		return error('Failed to set private key')
	}
	// Now compute the public key
	//
	// Create a new EC_POINT object for the public key
	pub_key_point := C.EC_POINT_new(group)
	// Create a new BN_CTX object for efficient BIGNUM operations
	ctx := C.BN_CTX_new()
	if ctx == 0 {
		C.EC_POINT_free(pub_key_point)
		C.BN_free(bn)
		C.EC_KEY_free(ec_key)
		return error('Failed to create BN_CTX')
	}
	defer {
		C.BN_CTX_free(ctx)
	}
	// Perform the point multiplication to compute the public key: pub_key_point = bn * G
	res = C.EC_POINT_mul(group, pub_key_point, bn, 0, 0, ctx)
	if res != 1 {
		C.EC_POINT_free(pub_key_point)
		C.BN_free(bn)
		C.EC_KEY_free(ec_key)
		return error('Failed to compute public key')
	}
	// Set the computed public key in the EC_KEY object
	res = C.EC_KEY_set_public_key(ec_key, pub_key_point)
	if res != 1 {
		C.EC_POINT_free(pub_key_point)
		C.BN_free(bn)
		C.EC_KEY_free(ec_key)
		return error('Failed to set public key')
	}
	// Add key check
	// EC_KEY_check_key return 1 on success or 0 on error.
	chk := C.EC_KEY_check_key(ec_key)
	if chk == 0 {
		key_free(ec_key)
		return error('EC_KEY_check_key failed')
	}
	C.EC_POINT_free(pub_key_point)
	C.BN_free(bn)
	return PrivateKey{
		key: ec_key
	}
}

// sign performs signing the message with the options. By default, the options was using `.with_no_hash` options,
// so, its would not precompute the the digest (hash) of message before signing.
// If you wish to use with recommended hash function, you should pass `hash_config: .with_recommended_hash`
// to precompute the digest and and then sign the hash value.
// TODO: maybe changed to use .with_recommended_hash in the future to align with recommended way for signing.
pub fn (pv PrivateKey) sign(message []u8, opt SignerOpts) ![]u8 {
	digest := calc_digest(pv.key, message, opt)!
	return pv.sign_message(digest)!
}

// sign_message sign a message with private key
fn (priv_key PrivateKey) sign_message(message []u8) ![]u8 {
	if message.len == 0 {
		return error('Message cannot be null or empty')
	}
	mut sig_len := u32(0)
	sig_size := C.ECDSA_size(priv_key.key)
	sig := unsafe { malloc(int(sig_size)) }
	res := C.ECDSA_sign(0, message.data, message.len, sig, &sig_len, priv_key.key)
	if res != 1 {
		unsafe { free(sig) }
		return error('Failed to sign message')
	}
	signed_data := unsafe { sig.vbytes(int(sig_len)) }
	unsafe { free(sig) }
	return signed_data.clone()
}

// verify verifies a message with the signature are valid with public key provided .
// You should provide it with the same SignerOpts used with the `.sign()` call.
// or verify would fail (false).
pub fn (pub_key PublicKey) verify(message []u8, sig []u8, opt SignerOpts) !bool {
	digest := calc_digest(pub_key.key, message, opt)!
	res := C.ECDSA_verify(0, digest.data, digest.len, sig.data, sig.len, pub_key.key)
	if res == -1 {
		return error('Failed to verify signature')
	}
	return res == 1
}

// seed gets the seed (private key bytes).
// Notes:
// Generally, if the private key generated from the seed with `new_key_from_seed(seed)`
// the call to `.seed()` would produce original seed bytes except your original seed bytes
// contains leading zeros bytes, internally its would be chopped off by underlying wrapper,
// so, its would produces unmatching length with the original seed.
pub fn (priv_key PrivateKey) seed() ![]u8 {
	bn := voidptr(C.EC_KEY_get0_private_key(priv_key.key))
	if bn == 0 {
		return error('Failed to get private key BIGNUM')
	}
	num_bytes := (C.BN_num_bits(bn) + 7) / 8

	mut buf := []u8{len: int(num_bytes)}
	res := C.BN_bn2bin(bn, buf.data)
	if res == 0 {
		return error('Failed to convert BIGNUM to bytes')
	}
	return buf
}

// Get the public key from private key
pub fn (priv_key PrivateKey) public_key() !PublicKey {
	// Increase reference count
	res := C.EC_KEY_up_ref(priv_key.key)
	if res != 1 {
		return error('Failed to increment EC_KEY reference count')
	}
	return PublicKey{
		key: priv_key.key
	}
}

// EC_GROUP_cmp() for comparing two group (curve).
// EC_GROUP_cmp returns 0 if the curves are equal, 1 if they are not equal, or -1 on error.
fn C.EC_GROUP_cmp(a &C.EC_GROUP, b &C.EC_GROUP, ctx &C.BN_CTX) int

// equal compares two private keys was equal. Its checks for two things, ie:
//
// - whether both of private keys lives under the same group (curve),
// - compares if two private key bytes was equal.
pub fn (priv_key PrivateKey) equal(other PrivateKey) bool {
	group1 := voidptr(C.EC_KEY_get0_group(priv_key.key))
	group2 := voidptr(C.EC_KEY_get0_group(other.key))
	ctx := C.BN_CTX_new()
	if ctx == 0 {
		return false
	}
	defer {
		C.BN_CTX_free(ctx)
	}
	gres := C.EC_GROUP_cmp(group1, group2, ctx)
	// Its lives on the same group
	if gres == 0 {
		bn1 := voidptr(C.EC_KEY_get0_private_key(priv_key.key))
		bn2 := voidptr(C.EC_KEY_get0_private_key(other.key))
		res := C.BN_cmp(bn1, bn2)
		return res == 0
	}
	return false
}

// Compare two public keys
pub fn (pub_key PublicKey) equal(other PublicKey) bool {
	// TODO: check validity of the group
	group1 := voidptr(C.EC_KEY_get0_group(pub_key.key))
	group2 := voidptr(C.EC_KEY_get0_group(other.key))
	if group1 == 0 || group2 == 0 {
		return false
	}
	ctx := C.BN_CTX_new()
	if ctx == 0 {
		return false
	}
	defer {
		C.BN_CTX_free(ctx)
	}
	gres := C.EC_GROUP_cmp(group1, group2, ctx)
	// Its lives on the same group
	if gres == 0 {
		point1 := voidptr(C.EC_KEY_get0_public_key(pub_key.key))
		point2 := voidptr(C.EC_KEY_get0_public_key(other.key))
		if point1 == 0 || point2 == 0 {
			return false
		}
		res := C.EC_POINT_cmp(group1, point1, point2, ctx)
		return res == 0
	}

	return false
}

// Helpers
//
// new_curve creates a new empty curve based on curve NID, default to prime256v1 (or secp256r1).
fn new_curve(opt CurveOptions) &C.EC_KEY {
	mut nid := nid_prime256v1
	match opt.nid {
		.prime256v1 {
			// do nothing
		}
		.secp384r1 {
			nid = nid_secp384r1
		}
		.secp521r1 {
			nid = nid_secp521r1
		}
		.secp256k1 {
			nid = nid_secp256k1
		}
	}
	return C.EC_KEY_new_by_curve_name(nid)
}

// Gets recommended hash function of the key.
// Its purposes for hashing message to be signed.
fn recommended_hash(key &C.EC_KEY) !crypto.Hash {
	group := voidptr(C.EC_KEY_get0_group(key))
	if group == 0 {
		return error('Unable to load group')
	}
	// gets the bits size of private key group
	num_bits := C.EC_GROUP_get_degree(group)
	match true {
		// use sha256
		num_bits <= 256 {
			return .sha256
		}
		num_bits > 256 && num_bits <= 384 {
			return .sha384
		}
		// TODO: what hash should be used if the size is over > 512 bits
		num_bits > 384 {
			return .sha512
		}
		else {
			return error('Unsupported bits size')
		}
	}
}

pub enum HashConfig {
	with_recommended_hash
	with_no_hash
	with_custom_hash
}

@[params]
pub struct SignerOpts {
pub mut:
	// default to .with_no_hash to align with the current behaviour of signing.
	hash_config HashConfig = .with_no_hash
	// make sense when HashConfig != with_recommended_hash
	allow_smaller_size bool
	allow_custom_hash  bool
	// set to non-nil if allow_custom_hash was true
	custom_hash &hash.Hash = unsafe { nil }
}

// calc_digest tries to calculates digest (hash) of the message based on options provided.
// If the options was with_no_hash, its return default message without hashing.
fn calc_digest(key &C.EC_KEY, message []u8, opt SignerOpts) ![]u8 {
	if message.len == 0 {
		return error('null-length messages')
	}
	// we're working on mutable copy of SignerOpts, with some issues when make it as a mutable.
	// ie, declaring a mutable parameter that accepts a struct with the `@[params]` attribute is not allowed.
	mut cfg := opt
	match cfg.hash_config {
		.with_no_hash {
			// return original message
			return message
		}
		.with_recommended_hash {
			h := recommended_hash(key)!
			match h {
				.sha256 {
					return sha256.sum256(message)
				}
				.sha384 {
					return sha512.sum384(message)
				}
				.sha512 {
					return sha512.sum512(message)
				}
				else {
					return error('Unsupported hash')
				}
			}
		}
		.with_custom_hash {
			if !cfg.allow_custom_hash {
				return error('custom hash was not allowed, set it into true')
			}
			if cfg.custom_hash == unsafe { nil } {
				return error('Custom hasher was not defined')
			}
			// check key size bits
			group := voidptr(C.EC_KEY_get0_group(key))
			if group == 0 {
				return error('fail to load group')
			}
			num_bits := C.EC_GROUP_get_degree(group)
			// check for key size matching
			key_size := (num_bits + 7) / 8
			// If current Private Key size is bigger then current hash output size,
			// by default its not allowed, until set the allow_smaller_size into true
			if key_size > cfg.custom_hash.size() {
				if !cfg.allow_smaller_size {
					return error('Hash into smaller size than current key size was not allowed')
				}
			}
			digest := cfg.custom_hash.sum(message)
			defer { unsafe { cfg.custom_hash.free() } }
			return digest
		}
	}
	return error('Not should be here')
}

// Clear allocated memory for key
fn key_free(ec_key &C.EC_KEY) {
	C.EC_KEY_free(ec_key)
}

// free clears out allocated memory for PublicKey.
// Dont use PublicKey after calling `.free()`
pub fn (pb &PublicKey) free() {
	C.EC_KEY_free(pb.key)
}

// free clears out allocated memory for PrivateKey
// Dont use PrivateKey after calling `.free()`
pub fn (pv &PrivateKey) free() {
	C.EC_KEY_free(pv.key)
}
