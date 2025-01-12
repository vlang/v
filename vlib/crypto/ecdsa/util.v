module ecdsa

#include <openssl/evp.h>
#include <openssl/err.h>
#include <openssl/x509.h>

// #define NID_X9_62_id_ecPublicKey   408
const nid_ec_publickey = C.NID_X9_62_id_ecPublicKey

@[typedef]
struct C.EVP_PKEY {}

// EVP_PKEY *EVP_PKEY_new(void);
fn C.EVP_PKEY_new() &C.EVP_PKEY

// EVP_PKEY_free(EVP_PKEY *key);
fn C.EVP_PKEY_free(key &C.EVP_PKEY)

// EC_KEY *EVP_PKEY_get1_EC_KEY(EVP_PKEY *pkey);
fn C.EVP_PKEY_get1_EC_KEY(pkey &C.EVP_PKEY) &C.EC_KEY

// EVP_PKEY *d2i_PUBKEY(EVP_PKEY **a, const unsigned char **pp, long length);
fn C.d2i_PUBKEY(k &&C.EVP_PKEY, pp &&u8, length u32) &C.EVP_PKEY

// point_conversion_form_t EC_KEY_get_conv_form(const EC_KEY *key);
fn C.EC_KEY_get_conv_form(k &C.EC_KEY) int

// EC_GROUP_get_degree
fn C.EC_GROUP_get_degree(g &C.EC_GROUP) int

// const EC_POINT *EC_KEY_get0_public_key(const EC_KEY *key);
fn C.EC_KEY_get0_public_key(key &C.EC_KEY) &C.EC_POINT

// size_t EC_POINT_point2oct(const EC_GROUP *group, const EC_POINT *point, point_conversion_form_t form, uint8_t *buf, size_t max_out, BN_CTX *ctx);
fn C.EC_POINT_point2oct(g &C.EC_GROUP, p &C.EC_POINT, form int, buf &u8, max_out int, ctx &C.BN_CTX) int

// int EVP_PKEY_get_base_id(const EVP_PKEY *pkey);
fn C.EVP_PKEY_base_id(key &C.EVP_PKEY) int

// int EC_GROUP_get_curve_name(const EC_GROUP *group);
fn C.EC_GROUP_get_curve_name(g &C.EC_GROUP) int
fn C.EC_GROUP_free(group &C.EC_GROUP)

// pubkey_from_bytes loads ECDSA Public Key from bytes array.
// The bytes of data should be a valid of ASN.1 DER serialized SubjectPublicKeyInfo structrue of RFC 5480.
// Otherwise, its should an error.
// Typically, you can load the bytes from pem formatted of ecdsa public key.
//
// Examples:
// ```codeblock
// import crypto.pem
//
// const pubkey_sample = '-----BEGIN PUBLIC KEY-----
// MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAE+P3rhFkT1fXHYbY3CpcBdh6xTC74MQFx
// cftNVD3zEPVzo//OalIVatY162ksg8uRWBdvFFuHZ9OMVXkbjwWwhcXP7qmI9rOS
// LR3AGUldy+bBpV2nT306qCIwgUAMeOJP
// -----END PUBLIC KEY-----'
//
// block, _ := pem.decode(pubkey_sample) or { panic(err) }
// pubkey := pubkey_from_bytes(block.data)!
// ```
pub fn pubkey_from_bytes(bytes []u8) !PublicKey {
	if bytes.len == 0 {
		return error('Invalid bytes')
	}
	mut pub_key := C.EVP_PKEY_new()
	pub_key = C.d2i_PUBKEY(&pub_key, voidptr(&bytes.data), bytes.len)
	if pub_key == 0 {
		C.EVP_PKEY_free(pub_key)
		return error('Error loading public key')
	}
	// Get the NID of this pubkey, and check if the pubkey object was
	// have the correct NID of ec public key type, ie, NID_X9_62_id_ecPublicKey
	nid := C.EVP_PKEY_base_id(pub_key)
	if nid != nid_ec_publickey {
		C.EVP_PKEY_free(pub_key)
		return error('Get an nid of non ecPublicKey')
	}

	eckey := C.EVP_PKEY_get1_EC_KEY(pub_key)
	if eckey == 0 {
		key_free(eckey)
		return error('Failed to get ec key')
	}
	// check the group for the supported curve(s)
	group := voidptr(C.EC_KEY_get0_group(eckey))
	if group == 0 {
		C.EC_GROUP_free(group)
		return error('Failed to load group from key')
	}
	nidgroup := C.EC_GROUP_get_curve_name(group)
	if nidgroup != nid_prime256v1 && nidgroup != nid_secp384r1 && nidgroup != nid_secp521r1
		&& nidgroup != nid_secp256k1 {
		return error('Unsupported group')
	}
	C.EVP_PKEY_free(pub_key)
	// Its OK to return
	return PublicKey{
		key: eckey
	}
}

// bytes gets the bytes of public key parts of this keypair.
pub fn (pbk PublicKey) bytes() ![]u8 {
	point := voidptr(C.EC_KEY_get0_public_key(pbk.key))
	if point == 0 {
		C.EC_POINT_free(point)
		return error('Failed to get public key BIGNUM')
	}

	group := voidptr(C.EC_KEY_get0_group(pbk.key))
	num_bits := C.EC_GROUP_get_degree(group)
	// 1 byte of conversion format || x || y of EC_POINT
	num_bytes := 1 + 2 * ((num_bits + 7) / 8)

	ctx := C.BN_CTX_new()
	defer {
		C.BN_CTX_free(ctx)
	}

	if ctx == 0 {
		C.EC_POINT_free(point)
		C.BN_CTX_free(ctx)
		return error('Failed to create BN_CTX')
	}
	mut buf := []u8{len: num_bytes}

	// Get conversion format.
	// The uncompressed form is indicated by 0x04 and the compressed form is indicated
	// by either 0x02 or 0x03, hybrid 0x06
	// The public key MUST be rejected if any other value is included in the first octet.
	conv_form := C.EC_KEY_get_conv_form(pbk.key)
	if conv_form !in [2, 3, 4, 6] {
		return error('bad conversion format')
	}
	n := C.EC_POINT_point2oct(group, point, conv_form, buf.data, buf.len, ctx)

	// returns the clone of the buffer[..n]
	return buf[..n].clone()
}
