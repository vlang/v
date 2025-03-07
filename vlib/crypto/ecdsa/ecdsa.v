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

// https://docs.openssl.org/3.0/man3/EVP_PKEY_fromdata/#selections
const evp_pkey_keypair = C.EVP_PKEY_KEYPAIR

// POINT_CONVERSION FORAMT
const point_conversion_uncompressed = 4

// Nid is an enumeration of the supported curves
pub enum Nid {
	prime256v1 = C.NID_X9_62_prime256v1
	secp384r1  = C.NID_secp384r1
	secp521r1  = C.NID_secp521r1
	secp256k1  = C.NID_secp256k1
}

// we need this group (cruve) name representation to pass them into needed routines
fn (nid Nid) str() string {
	match nid {
		// TODO: maybe better relies on info from underlying C defined constants,
		// ie, #define SN_X9_62_prime256v1     "prime256v1" etc
		.prime256v1 { return 'prime256v1' }
		.secp384r1 { return 'secp384r1' }
		.secp521r1 { return 'secp521r1' }
		.secp256k1 { return 'secp256k1' }
	}
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
	// This can be simplified to just more simpler one
	pv := PrivateKey.new(opt)!
	pb := pv.public_key()!

	return pb, pv
}

// new_key_from_seed creates a new private key from the seed bytes. If opt was not provided,
// its default to prime256v1 curve.
//
// Notes on the seed:
//
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
	evpkey := evpkey_from_seed(seed, opt)!
	num_bits := C.EVP_PKEY_get_bits(evpkey)
	key_size := (num_bits + 7) / 8
	if seed.len > key_size {
		C.EVP_PKEY_free(evpkey)
		return error('Seed length exceeds key size')
	}
	// Check if its using fixed key size or flexible one
	if opt.fixed_size {
		if seed.len != key_size {
			C.EVP_PKEY_free(evpkey)
			return error('seed size doesnt match with curve key size')
		}
	}
	// TODO: remove this when its ready to go out
	eckey := C.EVP_PKEY_get1_EC_KEY(evpkey)
	if eckey == 0 {
		C.EC_KEY_free(eckey)
		C.EVP_PKEY_free(evpkey)
		return error('EVP_PKEY_get1_EC_KEY failed')
	}
	mut pvkey := PrivateKey{
		key:    eckey
		evpkey: evpkey
	}

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
	if pv.evpkey != unsafe { nil } {
		digest := calc_digest_with_evpkey(pv.evpkey, message, opt)!
		return sign_digest(pv.evpkey, digest)!
	}
	digest := calc_digest_with_eckey(pv.key, message, opt)!
	return pv.sign_digest(digest)!
}

// sign_with_options signs message with the options. It will be deprecated,
// Use `PrivateKey.sign()` instead.
@[deprecated: 'use PrivateKey.sign() instead']
pub fn (pv PrivateKey) sign_with_options(message []u8, opt SignerOpts) ![]u8 {
	return pv.sign(message, opt)
}

// sign_digest sign a digest with private key.
fn (pv PrivateKey) sign_digest(digest []u8) ![]u8 {
	if digest.len == 0 {
		return error('Digest cannot be null or empty')
	}
	mut sig_len := u32(0)
	sig_size := C.ECDSA_size(pv.key)
	sig := unsafe { malloc(int(sig_size)) }
	res := C.ECDSA_sign(0, digest.data, digest.len, sig, &sig_len, pv.key)
	if res != 1 {
		unsafe { free(sig) }
		return error('Failed to sign digest')
	}
	signed_data := unsafe { sig.vbytes(int(sig_len)) }
	unsafe { free(sig) }
	return signed_data.clone()
}

// bytes represent private key as bytes.
pub fn (pv PrivateKey) bytes() ![]u8 {
	// This is the old one
	bn := voidptr(C.EC_KEY_get0_private_key(pv.key))
	if bn == 0 {
		return error('Failed to get private key BIGNUM')
	}
	num_bytes := (C.BN_num_bits(bn) + 7) / 8
	// Get the buffer size to store the seed.
	size := if pv.ks_flag == .flexible {
		// should be non zero
		pv.ks_size
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
pub fn (pv PrivateKey) seed() ![]u8 {
	return pv.bytes()
}

// public_key gets the PublicKey from private key.
pub fn (pv PrivateKey) public_key() !PublicKey {
	// Check if EVP_PKEY opaque was availables or not.
	// TODO: removes this check when its ready
	if pv.evpkey != unsafe { nil } {
		bo := C.BIO_new(C.BIO_s_mem())
		n := C.i2d_PUBKEY_bio(bo, pv.evpkey)
		assert n != 0
		// stores this bio as another key
		pbkey := C.d2i_PUBKEY_bio(bo, 0)

		// TODO: removes this when its ready to obsolete.
		eckey := C.EVP_PKEY_get1_EC_KEY(pbkey)
		if eckey == 0 {
			C.EC_KEY_free(eckey)
			C.EVP_PKEY_free(pbkey)
			C.BIO_free_all(bo)
			return error('EVP_PKEY_get1_EC_KEY failed')
		}
		C.BIO_free_all(bo)

		return PublicKey{
			evpkey: pbkey
			key:    eckey
		}
	}
	// Otherwise, use the old EC_KEY opaque.
	// TODO: removes this when its ready to obsolete
	//
	// There are some issues concerned when returning PublicKey directly using underlying
	// `PrivateKey.key`. This private key containing sensitive information inside it, so return
	// this without care maybe can lead to some serious security impact.
	// See https://discord.com/channels/592103645835821068/592320321995014154/1329261267965448253
	// So, we instead return a new EC_KEY opaque based information availables on private key object
	// without private key bits has been set on this new opaque.
	group := voidptr(C.EC_KEY_get0_group(pv.key))
	if group == 0 {
		return error('Failed to load group from priv_key')
	}
	nid := C.EC_GROUP_get_curve_name(group)
	if nid != nid_prime256v1 && nid != nid_secp384r1 && nid != nid_secp521r1 && nid != nid_secp256k1 {
		return error('Get unsupported curve nid')
	}
	// get public key point from private key opaque
	pubkey_point := voidptr(C.EC_KEY_get0_public_key(pv.key))
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
	if priv_key.evpkey != unsafe { nil } && other.evpkey != unsafe { nil } {
		eq := C.EVP_PKEY_eq(voidptr(priv_key.evpkey), voidptr(other.evpkey))
		return eq == 1
	}
	// TODO: remove this when its ready
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
pub fn (pb PublicKey) verify(message []u8, sig []u8, opt SignerOpts) !bool {
	if pb.evpkey != unsafe { nil } {
		digest := calc_digest_with_evpkey(pb.evpkey, message, opt)!
		return verify_signature(pb.evpkey, sig, digest)
	}
	digest := calc_digest_with_eckey(pb.key, message, opt)!
	res := C.ECDSA_verify(0, digest.data, digest.len, sig.data, sig.len, pb.key)
	if res == -1 {
		return error('Failed to verify signature')
	}
	return res == 1
}

// Compare two public keys
pub fn (pub_key PublicKey) equal(other PublicKey) bool {
	if pub_key.evpkey != unsafe { nil } && other.evpkey != unsafe { nil } {
		eq := C.EVP_PKEY_eq(voidptr(pub_key.evpkey), voidptr(other.evpkey))
		return eq == 1
	}
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

// calc_digest_with_eckey tries to calculates digest (hash) of the message based on options provided.
// If the options was .with_no_hash, its has the same behaviour with .with_recommended_hash.
fn calc_digest_with_eckey(key &C.EC_KEY, message []u8, opt SignerOpts) ![]u8 {
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

// calc_digest_with_evpkey get the digest of the messages under the EVP_PKEY and options
fn calc_digest_with_evpkey(key &C.EVP_PKEY, message []u8, opt SignerOpts) ![]u8 {
	if message.len == 0 {
		return error('null-length messages')
	}
	bits_size := C.EVP_PKEY_get_bits(key)
	if bits_size <= 0 {
		return error(' bits_size was invalid')
	}
	key_size := (bits_size + 7) / 8

	match opt.hash_config {
		.with_no_hash, .with_recommended_hash {
			md := default_digest(key)!
			return calc_digest_with_md(message, md)!
		}
		.with_custom_hash {
			mut cfg := opt
			if !cfg.allow_custom_hash {
				return error('custom hash was not allowed, set it into true')
			}
			if cfg.custom_hash == unsafe { nil } {
				return error('Custom hasher was not defined')
			}
			if key_size > cfg.custom_hash.size() {
				if !cfg.allow_smaller_size {
					return error('Hash into smaller size than current key size was not allowed')
				}
			}
			// we need to reset the custom hash before writes message
			cfg.custom_hash.reset()
			_ := cfg.custom_hash.write(message)!
			digest := cfg.custom_hash.sum([]u8{})

			return digest
		}
	}
	return error('Not should be here')
}

// sign_digest signs the digest with the key. Under the hood, EVP_PKEY_sign() does not
// hash the data to be signed, and therefore is normally used to sign digests.
fn sign_digest(key &C.EVP_PKEY, digest []u8) ![]u8 {
	ctx := C.EVP_PKEY_CTX_new(key, 0)
	if ctx == 0 {
		C.EVP_PKEY_CTX_free(ctx)
		return error('EVP_PKEY_CTX_new failed')
	}
	sin := C.EVP_PKEY_sign_init(ctx)
	if sin != 1 {
		C.EVP_PKEY_CTX_free(ctx)
		return error('EVP_PKEY_sign_init failed')
	}
	// siglen was used to store the size of the signature output. When EVP_PKEY_sign
	// was called with NULL signature buffer, siglen will tell maximum size of signature.
	siglen := usize(C.EVP_PKEY_size(key))
	st := C.EVP_PKEY_sign(ctx, 0, &siglen, digest.data, digest.len)
	if st <= 0 {
		C.EVP_PKEY_CTX_free(ctx)
		return error('Get null buffer length on EVP_PKEY_sign')
	}
	sig := []u8{len: int(siglen)}
	do := C.EVP_PKEY_sign(ctx, sig.data, &siglen, digest.data, digest.len)
	if do <= 0 {
		C.EVP_PKEY_CTX_free(ctx)
		return error('EVP_PKEY_sign fails to sign message')
	}
	// siglen now contains actual length of the signature buffer.
	signed := sig[..siglen].clone()

	// Cleans up
	unsafe { sig.free() }
	C.EVP_PKEY_CTX_free(ctx)

	return signed
}

// verify_signature verifies the signature for the digest under the provided key.
fn verify_signature(key &C.EVP_PKEY, sig []u8, digest []u8) bool {
	ctx := C.EVP_PKEY_CTX_new(key, 0)
	if ctx == 0 {
		C.EVP_PKEY_CTX_free(ctx)
		return false
	}
	vinit := C.EVP_PKEY_verify_init(ctx)
	if vinit != 1 {
		C.EVP_PKEY_CTX_free(ctx)
		return false
	}
	res := C.EVP_PKEY_verify(ctx, sig.data, sig.len, digest.data, digest.len)
	if res <= 0 {
		C.EVP_PKEY_CTX_free(ctx)
		return false
	}
	C.EVP_PKEY_CTX_free(ctx)
	return res == 1
}

// calc_digest_with_md get the digest of the msg using md digest algorithm
fn calc_digest_with_md(msg []u8, md &C.EVP_MD) ![]u8 {
	ctx := C.EVP_MD_CTX_new()
	if ctx == 0 {
		C.EVP_MD_CTX_free(ctx)
		return error('EVP_MD_CTX_new failed')
	}
	nt := C.EVP_DigestInit(ctx, md)
	assert nt == 1
	upd := C.EVP_DigestUpdate(ctx, msg.data, msg.len)
	assert upd == 1

	size := u32(C.EVP_MD_get_size(md))
	out := []u8{len: int(size)}

	fin := C.EVP_DigestFinal(ctx, out.data, &size)
	assert fin == 1

	digest := out[..size].clone()
	// cleans up
	unsafe { out.free() }
	C.EVP_MD_CTX_free(ctx)

	return digest
}

// default_digest gets the default digest (hash) algorithm for this key.
fn default_digest(key &C.EVP_PKEY) !&C.EVP_MD {
	// get bits size of this key
	bits_size := C.EVP_PKEY_get_bits(key)
	if bits_size <= 0 {
		return error(' this size isnt available.')
	}
	// based on this bits_size, choose appropriate digest algorithm
	match true {
		bits_size <= 256 {
			return voidptr(C.EVP_sha256())
		}
		bits_size > 256 && bits_size <= 384 {
			return voidptr(C.EVP_sha384())
		}
		bits_size > 384 {
			return voidptr(C.EVP_sha512())
		}
		else {
			return error('Unsupported bits size')
		}
	}
	return error('should not here')
}

// Build EVP_PKEY from raw seed of bytes and options.
fn evpkey_from_seed(seed []u8, opt CurveOptions) !&C.EVP_PKEY {
	// This routine mostly comes from the official docs with adds some checking at
	// https://docs.openssl.org/3.0/man3/EVP_PKEY_fromdata/#creating-an-ecc-keypair-using-raw-key-data
	//
	// convert the seed bytes to BIGNUM.
	bn := C.BN_bin2bn(seed.data, seed.len, 0)
	if bn == 0 {
		C.BN_free(bn)
		return error('BN_bin2bn failed from seed')
	}
	// build the group (curve) from the options.
	group := C.EC_GROUP_new_by_curve_name(int(opt.nid))
	if group == 0 {
		C.EC_GROUP_free(group)
		C.BN_free(bn)
		return error('EC_GROUP_new_by_curve_name failed')
	}
	// Build EC_POINT from this BIGNUM and gets bytes represantion of this point
	// in uncompressed format.
	point := ec_point_mult(group, bn)!
	pub_bytes := point_2_buf(group, point, point_conversion_uncompressed)!

	// Lets build params builder
	param_bld := C.OSSL_PARAM_BLD_new()
	assert param_bld != 0

	// push the group, private and public key bytes infos into the builder
	n := C.OSSL_PARAM_BLD_push_utf8_string(param_bld, c'group', opt.nid.str().str, 0)
	m := C.OSSL_PARAM_BLD_push_BN(param_bld, c'priv', bn)
	o := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'pub', pub_bytes.data, pub_bytes.len)
	if n <= 0 || m <= 0 || o <= 0 {
		C.EC_POINT_free(point)
		C.BN_free(bn)
		C.EC_GROUP_free(group)
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push FAILED')
	}
	// Setup the new key
	mut pkey := C.EVP_PKEY_new()
	assert pkey != 0

	// build parameter, initialize and build the key from params
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	pctx := C.EVP_PKEY_CTX_new_id(nid_evp_pkey_ec, 0)
	if params == 0 || pctx == 0 {
		C.EC_POINT_free(point)
		C.BN_free(bn)
		C.EC_GROUP_free(group)
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_free(pkey)
		if pctx == 0 {
			C.EVP_PKEY_CTX_free(pctx)
		}
		return error('EVP_PKEY_CTX_new or OSSL_PARAM_BLD_to_param failed')
	}
	// initialize key and build the key from builded params context.
	p := C.EVP_PKEY_fromdata_init(pctx)
	q := C.EVP_PKEY_fromdata(pctx, &pkey, evp_pkey_keypair, params)
	if p <= 0 || q <= 0 {
		C.EC_POINT_free(point)
		C.BN_free(bn)
		C.EC_GROUP_free(group)
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_free(pkey)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_fromdata failed')
	}
	// After this step, we have build the key in pkey
	// TODO: right way to check the builded key

	// Cleans up
	C.EC_POINT_free(point)
	C.BN_free(bn)
	C.EC_GROUP_free(group)
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)

	return pkey
}

// ec_point_mult performs point multiplications, point = bn * generator
fn ec_point_mult(group &C.EC_GROUP, bn &C.BIGNUM) !&C.EC_POINT {
	// Create a new EC_POINT object for the public key
	point := C.EC_POINT_new(group)
	// Create a new BN_CTX object for efficient BIGNUM operations
	ctx := C.BN_CTX_new()
	if ctx == 0 {
		C.EC_POINT_free(point)
		C.BN_CTX_free(ctx)
		return error('Failed to create BN_CTX')
	}

	// Perform the point multiplication to compute the public key: point = bn * G
	res := C.EC_POINT_mul(group, point, bn, 0, 0, ctx)
	if res != 1 {
		C.EC_POINT_free(point)
		C.BN_CTX_free(ctx)
		return error('Failed to compute public key')
	}
	C.BN_CTX_free(ctx)
	return point
}

// maximum key size we supported was 64 bytes.
const default_point_bufsize = 160 // 2 * 64 + 1 + extra

// point_2_buf gets bytes representation of the EC_POINT
fn point_2_buf(group &C.EC_GROUP, point &C.EC_POINT, fmt int) ![]u8 {
	ctx := C.BN_CTX_new()
	pbuf := []u8{len: default_point_bufsize}
	// Notes from the docs:
	// EC_POINT_point2buf() allocates a buffer of suitable length and writes an EC_POINT to it in octet format.
	// The allocated buffer is written to *pbuf and its length is returned.
	// The caller must free up the allocated buffer with a call to OPENSSL_free().
	// Since the allocated buffer value is written to *pbuf the pbuf parameter MUST NOT be NULL.
	// So, we explicitly call `.OPENSSL_free` on the allocated buffer.
	n := C.EC_POINT_point2buf(group, point, fmt, voidptr(&pbuf.data), ctx)
	if n <= 0 {
		C.BN_CTX_free(ctx)
		C.OPENSSL_free(voidptr(&pbuf.data))
		return error('Get null length of buf')
	}
	// Gets the copy of the result with the correct length
	result := pbuf[..n].clone()

	C.OPENSSL_free(voidptr(pbuf.data))
	C.BN_CTX_free(ctx)

	return result
}
