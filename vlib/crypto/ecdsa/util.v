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

	eckey := C.EVP_PKEY_get1_EC_KEY(pub_key)
	if eckey == 0 {
		C.EC_KEY_free(eckey)
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

// bytes gets the bytes of public key.
pub fn (pbk PublicKey) bytes() ![]u8 {
	point := voidptr(C.EC_KEY_get0_public_key(pbk.key))
	// defer { C.EC_POINT_free(point)}
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
	//
	// The uncompressed form is indicated by 0x04 and the compressed form is indicated
	// by either 0x02 or 0x03, hybrid 0x06
	// The public key MUST be rejected if any other value is included in the first octet.
	conv_form := C.EC_KEY_get_conv_form(pbk.key)
	if conv_form !in [2, 3, 4, 6] {
		return error('bad conversion format')
	}
	n := C.EC_POINT_point2oct(group, point, conv_form, buf.data, buf.len, ctx)
	if n == 0 {
		return error('EC_POINT_point2oct failed')
	}
	// returns the clone of the buffer[..n]
	return buf[..n].clone()
}

// pubkey_from_string loads a PublicKey from valid PEM-formatted string in s.
pub fn pubkey_from_string(s string) !PublicKey {
	if s.len == 0 {
		return error('Null length string was not allowed')
	}
	mut evpkey := C.EVP_PKEY_new()
	bo := C.BIO_new(C.BIO_s_mem())
	if bo == 0 {
		return error('Failed to create BIO_new')
	}
	n := C.BIO_write(bo, s.str, s.len)
	if n <= 0 {
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
	// Gets the ec key
	eckey := C.EVP_PKEY_get1_EC_KEY(evpkey)
	if eckey == 0 {
		C.BIO_free_all(bo)
		C.EC_KEY_free(eckey)
		C.EVP_PKEY_free(evpkey)
		return error('Failed to get ec key')
	}
	// check the group for the supported curve(s)
	if !is_valid_supported_group(eckey) {
		C.BIO_free_all(bo)
		C.EC_KEY_free(eckey)
		C.EVP_PKEY_free(evpkey)
		return error('Unsupported group')
	}
	chk := C.EC_KEY_check_key(eckey)
	if chk == 0 {
		C.EC_KEY_free(eckey)
		return error('EC_KEY_check_key failed')
	}
	C.EVP_PKEY_free(evpkey)
	C.BIO_free_all(bo)
	// Its OK to return
	return PublicKey{
		key: eckey
	}
}

// privkey_from_string loads a PrivateKey from valid PEM-formatted string in s.
// Underlying wrapper support for old secg and pkcs8 private key format, but this was not heavily tested.
// This routine does not support for the pkcs8 EncryptedPrivateKeyInfo format.
// See [usage_test.v](https://github.com/vlang/v/blob/master/vlib/crypto/ecdsa/example/ecdsa_seed_test.v) file
// for example of usage.
pub fn privkey_from_string(s string) !PrivateKey {
	if s.len == 0 {
		return error('null string was not allowed')
	}
	mut evpkey := C.EVP_PKEY_new()
	bo := C.BIO_new(C.BIO_s_mem())
	if bo == 0 {
		return error('Failed to create BIO_new')
	}
	n := C.BIO_write(bo, s.str, s.len)
	if n <= 0 {
		C.BIO_free_all(bo)
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

	eckey := C.EVP_PKEY_get1_EC_KEY(evpkey)
	if eckey == 0 {
		C.BIO_free_all(bo)
		C.EC_KEY_free(eckey)
		C.EVP_PKEY_free(evpkey)
		return error('Failed to get ec key')
	}
	// check the group for the supported curve(s)
	if !is_valid_supported_group(eckey) {
		C.BIO_free_all(bo)
		C.EC_KEY_free(eckey)
		C.EVP_PKEY_free(evpkey)
		return error('Unsupported group')
	}

	chk := C.EC_KEY_check_key(eckey)
	if chk == 0 {
		C.EC_KEY_free(eckey)
		return error('EC_KEY_check_key failed')
	}
	ksize := ec_key_size(eckey)!

	C.EVP_PKEY_free(evpkey)
	C.BIO_free_all(bo)

	// Its OK to return
	return PrivateKey{
		key:     eckey
		ks_flag: .fixed
		ks_size: ksize
	}
}

// Helpers
//
// is_valid_supported_group checks whether this eckey has valid group of supported curve.
@[inline]
fn is_valid_supported_group(eckey &C.EC_KEY) bool {
	group := voidptr(C.EC_KEY_get0_group(eckey))
	if group == 0 {
		return false
	}
	nidgroup := C.EC_GROUP_get_curve_name(group)
	if nidgroup == nid_prime256v1 || nidgroup == nid_secp384r1 || nidgroup == nid_secp521r1
		|| nidgroup == nid_secp256k1 {
		return true
	}
	return false
}

// key_size get the key size of this ec key
fn ec_key_size(ec_key &C.EC_KEY) !int {
	group := voidptr(C.EC_KEY_get0_group(ec_key))
	if group == 0 {
		return error('Unable to load group')
	}
	num_bits := C.EC_GROUP_get_degree(group)
	key_size := (num_bits + 7) / 8
	return key_size
}
