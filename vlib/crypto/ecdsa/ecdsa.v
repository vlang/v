// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

import hash
import crypto
import crypto.sha256
import crypto.sha512

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

// #define NID_X9_62_id_ecPublicKey   408
const nid_ec_publickey = C.NID_X9_62_id_ecPublicKey
// C.EVP_PKEY_EC = NID_X9_62_id_ecPublicKey
const nid_evp_pkey_ec = C.EVP_PKEY_EC
// we only support this
const openssl_ec_named_curve = C.OPENSSL_EC_NAMED_CURVE

// Nid is an enumeration of the supported curves
pub enum Nid {
	prime256v1
	secp384r1
	secp521r1
	secp256k1
}

@[params]
pub struct CurveOptions {
pub mut:
	// default to NIST P-256 curve
	nid Nid = .prime256v1
	// by default, allow arbitrary size of seed bytes as key.
	// Set it to `true` when you need fixed size, using the curve key size.
	// Its main purposes is to support the `.new_key_from_seed` call.
	fixed_size bool
}

// HashConfig is an enumeration of the possible options for key signing (verifying).
pub enum HashConfig {
	with_recommended_hash
	with_no_hash
	with_custom_hash
}

// SignerOpts represents configuration options to drive signing and verifying process.
@[params]
pub struct SignerOpts {
pub mut:
	// default to .with_recommended_hash
	hash_config HashConfig = .with_recommended_hash
	// make sense when HashConfig != with_recommended_hash
	allow_smaller_size bool
	allow_custom_hash  bool
	// set to non-nil if allow_custom_hash was true
	custom_hash &hash.Hash = unsafe { nil }
}

// KeyFlag is an enumeration of possible options to support flexible of PrivateKey key size.
enum KeyFlag {
	// flexible flag to allow flexible-size of seed bytes
	flexible
	// fixed flag for using underlying curve key size
	fixed
}

// generate_key generates a new key pair. If opt was not provided, its default to prime256v1 curve.
// If you want another curve, use in the following manner: `pubkey, pivkey := ecdsa.generate_key(nid: .secp384r1)!`
pub fn generate_key(opt CurveOptions) !(PublicKey, PrivateKey) {
	// creates new empty key
	ec_key := new_curve(opt)
	if ec_key == 0 {
		C.EC_KEY_free(ec_key)
		return error('Failed to create new EC_KEY')
	}
	// we duplicate the empty ec_key and shares similiar curve infos
	// and used this as public key
	pbkey := C.EC_KEY_dup(ec_key)
	if pbkey == 0 {
		C.EC_KEY_free(ec_key)
		C.EC_KEY_free(pbkey)
		return error('Failed on EC_KEY_dup')
	}
	res := C.EC_KEY_generate_key(ec_key)
	if res != 1 {
		C.EC_KEY_free(ec_key)
		C.EC_KEY_free(pbkey)
		return error('Failed to generate EC_KEY')
	}
	// we take public key bits from above generated key
	// and stored in duplicated public key object before.
	pubkey_point := voidptr(C.EC_KEY_get0_public_key(ec_key))
	if pubkey_point == 0 {
		C.EC_POINT_free(pubkey_point)
		C.EC_KEY_free(ec_key)
		C.EC_KEY_free(pbkey)
		return error('Failed to get public key BIGNUM')
	}
	np := C.EC_KEY_set_public_key(pbkey, pubkey_point)
	if np != 1 {
		C.EC_POINT_free(pubkey_point)
		C.EC_KEY_free(ec_key)
		C.EC_KEY_free(pbkey)
		return error('Failed to set public key')
	}
	// when using default generate_key, its using underlying curve key size
	// and discarded opt.fixed_size flag when its not set.
	priv_key := PrivateKey{
		key:     ec_key
		ks_flag: .fixed
	}
	pub_key := PublicKey{
		key: pbkey
	}
	return pub_key, priv_key
}

// new_key_from_seed creates a new private key from the seed bytes. If opt was not provided,
// its default to prime256v1 curve.
//
// Notes on the seed:
// You should make sure, the seed bytes come from a cryptographically secure random generator,
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
	// Note: cast with voidptr() to allow -cstrict checks to pass
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
	// Check if its using fixed key size or flexible one
	if opt.fixed_size {
		if seed.len != key_size {
			C.EC_KEY_free(ec_key)
			return error('seed size doesnt match with curve key size')
		}
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
		C.EC_KEY_free(ec_key)
		return error('EC_KEY_check_key failed')
	}
	C.EC_POINT_free(pub_key_point)
	C.BN_free(bn)

	mut pvkey := PrivateKey{
		key: ec_key
	}
	// we set the flag information on the key
	if opt.fixed_size {
		// using fixed one
		pvkey.ks_flag = .fixed
		pvkey.ks_size = key_size
	} else {
		pvkey.ks_size = seed.len
	}

	return pvkey
}

// PrivateKey represents ECDSA private key. Actually its a key pair,
// contains private key and public key parts.
pub struct PrivateKey {
	// The new high level of keypair opaque, set to nil now.
	evpkey &C.EVP_PKEY = unsafe { nil }
	// TODO: when all has been migrated to the new one,
	// removes this low level declarations.
	key &C.EC_KEY
mut:
	// ks_flag with .flexible value allowing
	// flexible-size seed bytes as key.
	// When it is `.fixed`, it will use the underlying key size.
	ks_flag KeyFlag = .flexible
	// ks_size stores size of the seed bytes when ks_flag was .flexible.
	// You should set it to a non zero value
	ks_size int
}

// PrivateKey.new creates a new key pair. By default, it would create a prime256v1 based key.
// Dont forget to call `.free()` after finish with your key.
pub fn PrivateKey.new(opt CurveOptions) !PrivateKey {
	// Default to prime256v1 based key
	mut group_nid := nid_prime256v1
	match opt.nid {
		.prime256v1 {}
		.secp384r1 {
			group_nid = nid_secp384r1
		}
		.secp521r1 {
			group_nid = nid_secp521r1
		}
		.secp256k1 {
			group_nid = nid_secp256k1
		}
	}
	// New high level keypair generator
	evpkey := C.EVP_PKEY_new()
	pctx := C.EVP_PKEY_CTX_new_id(nid_evp_pkey_ec, 0)
	if pctx == 0 {
		C.EVP_PKEY_free(evpkey)
		C.EVP_PKEY_CTX_free(pctx)
		return error('C.EVP_PKEY_CTX_new_id failed')
	}
	nt := C.EVP_PKEY_keygen_init(pctx)
	if nt <= 0 {
		C.EVP_PKEY_free(evpkey)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_keygen_init failed')
	}
	// set the group (curve)
	cn := C.EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx, group_nid)
	if cn <= 0 {
		C.EVP_PKEY_free(evpkey)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_set_ec_paramgen_curve_nid')
	}
	// explicitly only allowing named curve, likely its the default on 3.0.
	pn := C.EVP_PKEY_CTX_set_ec_param_enc(pctx, openssl_ec_named_curve)
	if pn <= 0 {
		C.EVP_PKEY_free(evpkey)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_set_ec_param_enc failed')
	}
	// generates keypair
	nr := C.EVP_PKEY_keygen(pctx, &evpkey)
	if nr <= 0 {
		C.EVP_PKEY_free(evpkey)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_keygen failed')
	}

	// EVP_PKEY_get1_EC_KEY was deprecated in 3.0. Its used here for compatibility purposes
	// to support the old key function.
	// TODO: removes this when its ready to obsolete.
	eckey := C.EVP_PKEY_get1_EC_KEY(evpkey)
	if eckey == 0 {
		C.EVP_PKEY_CTX_free(pctx)
		C.EC_KEY_free(eckey)
		C.EVP_PKEY_free(evpkey)
		return error('EVP_PKEY_get1_EC_KEY failed')
	}
	// Cleans up the context
	C.EVP_PKEY_CTX_free(pctx)
	// when using default this function, its using underlying curve key size
	// and discarded opt.fixed_size flag when its not set.
	priv_key := PrivateKey{
		evpkey:  evpkey
		key:     eckey
		ks_flag: .fixed
	}
	return priv_key
}

// sign performs signing the message with the options. By default options,
// it will perform hashing before signing the message.
pub fn (pv PrivateKey) sign(message []u8, opt SignerOpts) ![]u8 {
	digest := calc_digest(pv.key, message, opt)!
	return pv.sign_message(digest)!
}

// sign_with_options signs message with the options. It will be deprecated,
// Use `PrivateKey.sign()` instead.
@[deprecated: 'use PrivateKey.sign() instead']
pub fn (pv PrivateKey) sign_with_options(message []u8, opt SignerOpts) ![]u8 {
	return pv.sign(message, opt)
}

// sign_message sign a message with private key.
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

// bytes represent private key as bytes.
pub fn (priv_key PrivateKey) bytes() ![]u8 {
	bn := voidptr(C.EC_KEY_get0_private_key(priv_key.key))
	if bn == 0 {
		return error('Failed to get private key BIGNUM')
	}
	num_bytes := (C.BN_num_bits(bn) + 7) / 8
	// Get the buffer size to store the seed.
	size := if priv_key.ks_flag == .flexible {
		// should be non zero
		priv_key.ks_size
	} else {
		num_bytes
	}
	mut buf := []u8{len: int(size)}
	res := C.BN_bn2binpad(bn, buf.data, size)
	if res == 0 {
		return error('Failed to convert BIGNUM to bytes')
	}
	return buf
}

// seed gets the seed (private key bytes). It will be deprecated.
// Use `PrivateKey.bytes()` instead.
@[deprecated: 'use PrivateKey.bytes() instead']
pub fn (priv_key PrivateKey) seed() ![]u8 {
	return priv_key.bytes()
}

// Get the public key from private key.
pub fn (priv_key PrivateKey) public_key() !PublicKey {
	// There are some issues concerned when returning PublicKey directly using underlying
	// `PrivateKey.key`. This private key containing sensitive information inside it, so return
	// this without care maybe can lead to some serious security impact.
	// See https://discord.com/channels/592103645835821068/592320321995014154/1329261267965448253
	// So, we instead return a new EC_KEY opaque based information availables on private key object
	// without private key bits has been set on this new opaque.
	group := voidptr(C.EC_KEY_get0_group(priv_key.key))
	if group == 0 {
		return error('Failed to load group from priv_key')
	}
	nid := C.EC_GROUP_get_curve_name(group)
	if nid != nid_prime256v1 && nid != nid_secp384r1 && nid != nid_secp521r1 && nid != nid_secp256k1 {
		return error('Get unsupported curve nid')
	}
	// get public key point from private key opaque
	pubkey_point := voidptr(C.EC_KEY_get0_public_key(priv_key.key))
	if pubkey_point == 0 {
		// C.EC_POINT_free(pubkey_point)
		// todo: maybe its not set, just calculates new one
		return error('Failed to get public key BIGNUM')
	}
	// creates a new EC_KEY opaque based on the same NID with private key and
	// sets public key on it.
	pub_key := C.EC_KEY_new_by_curve_name(nid)
	np := C.EC_KEY_set_public_key(pub_key, pubkey_point)
	if np != 1 {
		// C.EC_POINT_free(pubkey_point)
		C.EC_KEY_free(pub_key)
		return error('Failed to set public key')
	}
	// performs explicit check
	chk := C.EC_KEY_check_key(pub_key)
	if chk == 0 {
		C.EC_KEY_free(pub_key)
		return error('EC_KEY_check_key failed')
	}
	// OK ?
	return PublicKey{
		key: pub_key
	}
}

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

// free clears out allocated memory for PrivateKey
// Dont use PrivateKey after calling `.free()`
pub fn (pv &PrivateKey) free() {
	C.EC_KEY_free(pv.key)
	C.EVP_PKEY_free(pv.evpkey)
}

// PublicKey represents ECDSA public key for verifying message.
pub struct PublicKey {
	// The new high level of keypair opaque, set to nil now.
	evpkey &C.EVP_PKEY = unsafe { nil }
	// Remove this when its fully obsoleted by the new one.
	key &C.EC_KEY
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

// free clears out allocated memory for PublicKey.
// Dont use PublicKey after calling `.free()`
pub fn (pb &PublicKey) free() {
	C.EC_KEY_free(pb.key)
	C.EVP_PKEY_free(pb.evpkey)
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

fn calc_digest_with_recommended_hash(key &C.EC_KEY, msg []u8) ![]u8 {
	h := recommended_hash(key)!
	match h {
		.sha256 {
			return sha256.sum256(msg)
		}
		.sha384 {
			return sha512.sum384(msg)
		}
		.sha512 {
			return sha512.sum512(msg)
		}
		else {
			return error('Unsupported hash')
		}
	}
}

// calc_digest tries to calculates digest (hash) of the message based on options provided.
// If the options was .with_no_hash, its has the same behaviour with .with_recommended_hash.
fn calc_digest(key &C.EC_KEY, message []u8, opt SignerOpts) ![]u8 {
	if message.len == 0 {
		return error('null-length messages')
	}
	// check key size bits
	group := voidptr(C.EC_KEY_get0_group(key))
	if group == 0 {
		return error('fail to load group')
	}
	num_bits := C.EC_GROUP_get_degree(group)
	key_size := (num_bits + 7) / 8
	// we're working on mutable copy of SignerOpts, with some issues when make it as a mutable.
	// ie, declaring a mutable parameter that accepts a struct with the `@[params]` attribute is not allowed.
	mut cfg := opt
	match cfg.hash_config {
		// There are issues when your message size was exceeds the current key size with .with_no_hash options.
		// See https://discord.com/channels/592103645835821068/592114487759470596/1334319744098107423
		// So, we aliased it into .with_recommended_hash
		.with_no_hash, .with_recommended_hash {
			return calc_digest_with_recommended_hash(key, message)!
		}
		.with_custom_hash {
			if !cfg.allow_custom_hash {
				return error('custom hash was not allowed, set it into true')
			}
			if cfg.custom_hash == unsafe { nil } {
				return error('Custom hasher was not defined')
			}
			// If current Private Key size is bigger then current hash output size,
			// by default its not allowed, until set the allow_smaller_size into true
			if key_size > cfg.custom_hash.size() {
				if !cfg.allow_smaller_size {
					return error('Hash into smaller size than current key size was not allowed')
				}
			}
			// we need to reset the custom hash before writes message
			cfg.custom_hash.reset()
			_ := cfg.custom_hash.write(message)!
			digest := cfg.custom_hash.sum([]u8{})
			// NOTES:
			// NIST FIPS 186-4 at the end of section 6.4 states that:
			// When the length of the output of the hash function is greater than the bit length of n,
			// then the leftmost n bits of the hash function output block shall be used in any calculation
			// using the hash function output during the generation or verification of a digital signature
			// with output of custom_hash was bigger than bit length (key size)
			// TODO:
			// Maybe better to pick up only required bytes from digest, ie,
			// out := digest[..key_size].clone()
			// unsafe { digest.free() }
			// return out
			// Currently, just boildown to the caller
			return digest
		}
	}
	return error('Not should be here')
}

// Clear allocated memory for key
fn key_free(ec_key &C.EC_KEY) {
	C.EC_KEY_free(ec_key)
}
