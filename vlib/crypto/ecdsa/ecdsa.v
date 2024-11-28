// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

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
fn C.BN_bin2bn(s &u8, len int, ret &C.BIGNUM) &C.BIGNUM
fn C.EC_KEY_set_private_key(key &C.EC_KEY, prv &C.BIGNUM) int
fn C.EC_KEY_get0_group(key &C.EC_KEY) &C.EC_GROUP
fn C.EC_POINT_new(group &C.EC_GROUP) &C.EC_POINT
fn C.EC_POINT_mul(group &C.EC_GROUP, r &C.EC_POINT, n &C.BIGNUM, q &C.EC_POINT, m &C.BIGNUM, ctx &C.BN_CTX) int
fn C.EC_KEY_set_public_key(key &C.EC_KEY, &C.EC_POINT) int
fn C.EC_POINT_free(point &C.EC_POINT)
fn C.BN_free(a &C.BIGNUM)
fn C.ECDSA_size(key &C.EC_KEY) u32
fn C.ECDSA_sign(type_ int, dgst &u8, dgstlen int, sig &u8, siglen &u32, eckey &C.EC_KEY) int
fn C.ECDSA_verify(type_ int, dgst &u8, dgstlen int, sig &u8, siglen int, eckey &C.EC_KEY) int
fn C.EC_KEY_get0_private_key(key &C.EC_KEY) &C.BIGNUM
fn C.BN_num_bits(a &C.BIGNUM) int
fn C.BN_bn2bin(a &C.BIGNUM, to &u8) int
fn C.EC_KEY_up_ref(key &C.EC_KEY) int
fn C.BN_cmp(a &C.BIGNUM, b &C.BIGNUM) int
fn C.EC_KEY_get0_public_key(key &C.EC_KEY) &C.EC_POINT
fn C.EC_POINT_cmp(group &C.EC_GROUP, a &C.EC_POINT, b &C.EC_POINT, ctx &C.BN_CTX) int
fn C.BN_CTX_new() &C.BN_CTX
fn C.BN_CTX_free(ctx &C.BN_CTX)

// NID constants
const nid_prime256v1 = C.NID_X9_62_prime256v1

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

// Generate a new key pair
pub fn generate_key() !(PublicKey, PrivateKey) {
	nid := nid_prime256v1 // Using NIST P-256 curve
	ec_key := C.EC_KEY_new_by_curve_name(nid)
	if ec_key == 0 {
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

// Create a new private key from a seed
pub fn new_key_from_seed(seed []u8) !PrivateKey {
	nid := nid_prime256v1
	// Create a new EC_KEY object with the specified curve
	ec_key := C.EC_KEY_new_by_curve_name(nid)
	if ec_key == 0 {
		return error('Failed to create new EC_KEY')
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
	// Retrieve the EC_GROUP object associated with the EC_KEY
	group := C.EC_KEY_get0_group(ec_key)
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
	C.EC_POINT_free(pub_key_point)
	C.BN_free(bn)
	return PrivateKey{
		key: ec_key
	}
}

// Sign a message with private key
pub fn (priv_key PrivateKey) sign(message []u8) ![]u8 {
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

// Verify a signature with public key
pub fn (pub_key PublicKey) verify(message []u8, sig []u8) !bool {
	res := C.ECDSA_verify(0, message.data, message.len, sig.data, sig.len, pub_key.key)
	if res == -1 {
		return error('Failed to verify signature')
	}
	return res == 1
}

// Get the seed (private key bytes)
pub fn (priv_key PrivateKey) seed() ![]u8 {
	bn := C.EC_KEY_get0_private_key(priv_key.key)
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

// Compare two private keys
pub fn (priv_key PrivateKey) equal(other PrivateKey) bool {
	bn1 := C.EC_KEY_get0_private_key(priv_key.key)
	bn2 := C.EC_KEY_get0_private_key(other.key)
	res := C.BN_cmp(bn1, bn2)
	return res == 0
}

// Compare two public keys
pub fn (pub_key PublicKey) equal(other PublicKey) bool {
	group := C.EC_KEY_get0_group(pub_key.key)
	point1 := C.EC_KEY_get0_public_key(pub_key.key)
	point2 := C.EC_KEY_get0_public_key(other.key)
	ctx := C.BN_CTX_new()
	if ctx == 0 {
		return false
	}
	defer {
		C.BN_CTX_free(ctx)
	}
	res := C.EC_POINT_cmp(group, point1, point2, ctx)
	return res == 0
}
