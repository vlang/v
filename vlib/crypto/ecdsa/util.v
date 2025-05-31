module ecdsa

// pubkey_from_bytes loads ECDSA Public Key from bytes array.
// The bytes of data should be a valid of ASN.1 DER serialized SubjectPublicKeyInfo structrue of RFC 5480.
// Otherwise, its should an error.
// Typically, you can load the bytes from pem formatted of ecdsa public key.
//
// Examples:
// ```codeblock
// import crypto.pem
// import crypto.ecdsa
//
// const pubkey_sample = '-----BEGIN PUBLIC KEY-----
// MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAE+P3rhFkT1fXHYbY3CpcBdh6xTC74MQFx
// cftNVD3zEPVzo//OalIVatY162ksg8uRWBdvFFuHZ9OMVXkbjwWwhcXP7qmI9rOS
// LR3AGUldy+bBpV2nT306qCIwgUAMeOJP
// -----END PUBLIC KEY-----'
//
// block, _ := pem.decode(pubkey_sample) or { panic(err) }
// pubkey := ecdsa.pubkey_from_bytes(block.data)!
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

	// check the key
	pctx := C.EVP_PKEY_CTX_new(pub_key, 0)
	if pctx == 0 {
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pub_key)
		return error('EVP_PKEY_CTX_new failed')
	}
	// performs public-only evpkey check
	nck := C.EVP_PKEY_public_check(pctx)
	if nck != 1 {
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pub_key)
		return error('EVP_PKEY_check failed')
	}
	// Matching the supported group
	gn := key_group_name(pub_key)!
	// TODO: using shortname constant
	if gn != 'secp256k1' && gn != 'secp384r1' && gn != 'secp521r1' && gn != 'prime256v1' {
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pub_key)
		return error('Unsupported group')
	}
	// Cleans up
	C.EVP_PKEY_CTX_free(pctx)

	// Its OK to return
	return PublicKey{
		evpkey: pub_key
	}
}

// bytes gets the bytes of public key.
pub fn (pbk PublicKey) bytes() ![]u8 {
	ppub := []u8{len: default_point_bufsize}
	n := C.EVP_PKEY_get1_encoded_public_key(pbk.evpkey, voidptr(&ppub.data))
	if n <= 0 {
		C.OPENSSL_free(voidptr(ppub.data))
		return error('EVP_PKEY_get1_encoded_public_key failed')
	}
	out := ppub[..n].clone()
	// ppub should be freed by calling `OPENSSL_free` or memleak happens.
	C.OPENSSL_free(voidptr(ppub.data))

	return out
}

// pubkey_from_string loads a PublicKey from valid PEM-formatted string in s.
pub fn pubkey_from_string(s string) !PublicKey {
	if s.len == 0 {
		return error('Null length string was not allowed')
	}
	mut evpkey := C.EVP_PKEY_new()
	bo := C.BIO_new(C.BIO_s_mem())
	if bo == 0 {
		C.EVP_PKEY_free(evpkey)
		C.BIO_free_all(bo)
		return error('Failed to create BIO_new')
	}
	n := C.BIO_write(bo, s.str, s.len)
	if n <= 0 {
		C.EVP_PKEY_free(evpkey)
		C.BIO_free_all(bo)
		return error('BIO_write failed')
	}
	evpkey = C.PEM_read_bio_PUBKEY(bo, &evpkey, 0, 0)
	if evpkey == 0 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_free(evpkey)
		return error('Error loading key')
	}
	// Get the NID of this key, and check if the key object was
	// have the correct NID of ec public key type, ie, NID_X9_62_id_ecPublicKey
	nid := C.EVP_PKEY_base_id(evpkey)
	if nid != nid_ec_publickey {
		C.BIO_free_all(bo)
		C.EVP_PKEY_free(evpkey)
		return error('Get an nid of non ecPublicKey')
	}
	// check the key
	pctx := C.EVP_PKEY_CTX_new(evpkey, 0)
	if pctx == 0 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(evpkey)
		return error('EVP_PKEY_CTX_new failed')
	}
	// performs public-only evpkey check
	nck := C.EVP_PKEY_public_check(pctx)
	if nck != 1 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(evpkey)
		return error('EVP_PKEY_check failed')
	}
	// Matching the supported group
	gn := key_group_name(evpkey)!
	// TODO: using shortname constant
	if gn != 'secp256k1' && gn != 'secp384r1' && gn != 'secp521r1' && gn != 'prime256v1' {
		C.BIO_free_all(bo)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(evpkey)
		return error('Unsupported group')
	}
	// Cleans up
	C.BIO_free_all(bo)
	C.EVP_PKEY_CTX_free(pctx)

	// Its OK to return
	return PublicKey{
		evpkey: evpkey
	}
}

// privkey_from_string loads a PrivateKey from valid PEM-formatted string in s.
// Underlying wrapper support for old SECG and PKCS8 private key format, but this was not heavily tested.
// This routine does not support for the PKCS8 EncryptedPrivateKeyInfo format.
// See [ecdsa_seed_test.v](https://github.com/vlang/v/blob/master/vlib/crypto/ecdsa/example/ecdsa_seed_test.v) file
// for example of usage.
pub fn privkey_from_string(s string) !PrivateKey {
	if s.len == 0 {
		return error('null string was not allowed')
	}
	mut evpkey := C.EVP_PKEY_new()
	bo := C.BIO_new(C.BIO_s_mem())
	if bo == 0 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_free(evpkey)
		return error('Failed to create BIO_new')
	}
	n := C.BIO_write(bo, s.str, s.len)
	if n <= 0 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_free(evpkey)
		return error('BIO_write failed')
	}
	evpkey = C.PEM_read_bio_PrivateKey(bo, &evpkey, 0, 0)
	if evpkey == 0 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_free(evpkey)
		return error('Error loading key')
	}

	// Get the NID of this key, and check if the key object was
	// have the correct NID of ec public key type, ie, NID_X9_62_id_ecPublicKey
	nid := C.EVP_PKEY_base_id(evpkey)
	if nid != nid_ec_publickey {
		C.BIO_free_all(bo)
		C.EVP_PKEY_free(evpkey)
		return error('Get an nid of non ecPublicKey')
	}
	pctx := C.EVP_PKEY_CTX_new(evpkey, 0)
	if pctx == 0 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(evpkey)
		return error('EVP_PKEY_CTX_new failed')
	}
	// performs evpkey check
	nck := C.EVP_PKEY_check(pctx)
	if nck != 1 {
		C.BIO_free_all(bo)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(evpkey)
		return error('EVP_PKEY_check failed')
	}
	// Matching the supported group
	gn := key_group_name(evpkey)!
	// TODO: using shortname constant
	if gn != 'secp256k1' && gn != 'secp384r1' && gn != 'secp521r1' && gn != 'prime256v1' {
		C.BIO_free_all(bo)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(evpkey)
		return error('Unsupported group')
	}
	// Cleans up
	C.BIO_free_all(bo)
	C.EVP_PKEY_CTX_free(pctx)

	// Its OK to return
	return PrivateKey{
		evpkey:  evpkey
		ks_flag: .fixed
	}
}

// evp_key_size get the key size of this ec key
fn evp_key_size(key &C.EVP_PKEY) !int {
	num_bits := C.EVP_PKEY_get_bits(key)
	key_size := (num_bits + 7) / 8

	return key_size
}

const default_groupname_size = 25 // short name commonly only take 10-15 length

// key_group_name returns underlying group name of the key as a string.
fn key_group_name(key &C.EVP_PKEY) !string {
	gname := []u8{len: default_groupname_size}
	gname_len := usize(0)
	mut s := C.EVP_PKEY_get_group_name(key, gname.data, u32(gname.len), &gname_len)
	if s == 0 {
		unsafe { gname.free() }
		return error('fail to get group name')
	}
	group := gname[..gname_len].bytestr()
	unsafe { gname.free() }
	return group
}
