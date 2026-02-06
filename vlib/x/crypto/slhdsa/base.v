// Copyright (c) blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module slhdsa

// Configuration options used in SLH-DSA key generation.
@[params]
pub struct KeyOpts {
pub mut:
	// An opaque represents the kind of SLH-DSA keys want to built.
	// See `enum Kind` for available options.
	kind Kind = .sha2_128s

	// flag, 0=random (default), 1= use seed bytes, 2 = use priv bytes, otherwise error
	flag int
	// when you set flag=1, builder will use seed bytes as a params,
	// so, make sure to pass seed bytes length != 0
	seed []u8
	// when flag=2, you should ensure private bytes length != 0
	priv []u8

	// This option below was not supported yet.
	//
	// Sets properties to be used when fetching algorithm implementations
	// used for SLH-DSA hashing operations.
	propq string
}

// Configurations parameters used for signing (and or verifying)
@[params]
pub struct SignerOpts {
pub mut:
	// optional context string up to 255 length, used in signing (verifying)
	context string

	// "message-encoding"
	// The default value of 1 uses 'Pure SLH-DSA Signature Generation'.
	// Setting it to 0 does not encode the message, which is used for testing,
	// but can also be used for 'Pre Hash SLH-DSA Signature Generation'.
	// If you set encoding to 0, you should provide the entropy.
	encoding int = 1

	// "test-entropy" used for testing to pass a optional random value.
	entropy []u8 // octet-string

	// "deterministic" integer option.
	// The default value of 0 generates a random value (using a DRBG) this is used when
	// processing the message. Setting this to 1 causes the private key seed to be used instead.
	// This value is ignored if "test-entropy" is set.
	deterministic int
}

// from_seed creates a new SLH-DSA PrivateKey from seed bytes and kind options.
// If the seed length was zero, it will create a key based on randomly generated seed.
// You should make sure, the seed bytes comes from trusted cryptographic secure source.
// The seed size was 3 times of `𝑛` parameter defined in the standard.
// The `𝑛` size maybe 16, 24 or 32 bytes length, depend on the chosen type.
fn PrivateKey.from_seed(seed []u8, kind Kind) !PrivateKey {
	// when seed bytes length was zero, build the key based on the random ones
	if seed.len == 0 {
		return PrivateKey.new(kind: kind)!
	}
	// Get the n size parameter set from the options
	nsize := kind.nsize()
	// The length of the seed bytes supplied must be 3 * nsize.
	if seed.len != 3 * nsize {
		return error('Unmatching seed length with kind supplied, need ${3 * nsize} bytes')
	}

	param_bld := C.OSSL_PARAM_BLD_new()
	assert param_bld != 0

	m := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'seed', seed.data, seed.len)
	if m <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push failed')
	}

	// build params
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	// create a desired context
	pctx := C.EVP_PKEY_CTX_new_from_name(0, kind.long_name(), 0)
	if params == 0 || pctx == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_new or OSSL_PARAM_BLD_to_param failed')
	}
	pkey := C.EVP_PKEY_new()
	assert pkey != 0
	ke := C.EVP_PKEY_keygen_init(pctx)
	if ke <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_keygen_init failed')
	}
	// Use EVP_PKEY_CTX_set_params() after calling EVP_PKEY_keygen_init().
	s := C.EVP_PKEY_CTX_set_params(pctx, params)
	if s != 1 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_CTX_set_params failed')
	}

	ss := C.EVP_PKEY_keygen(pctx, &pkey)
	if ss <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_keygen failed')
	}
	// TODO: right way to check the key
	pvkey := PrivateKey{
		key: pkey
	}
	// Cleans up
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)

	return pvkey
}

fn PrivateKey.from_bytes(bytes []u8, kind Kind) !PrivateKey {
	// when bytes length was zero, build the key based on the random ones
	if bytes.len == 0 {
		return PrivateKey.new(kind: kind)!
	}
	// Get the n size parameter set from the options
	nsize := kind.nsize()
	// The private key has a size of 4 * n bytes.
	if bytes.len != 4 * nsize {
		return error('Unmatching private bytes length with kind supplied, need ${4 * nsize} bytes')
	}

	param_bld := C.OSSL_PARAM_BLD_new()
	assert param_bld != 0

	m := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'priv', bytes.data, bytes.len)
	if m <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push FAILED')
	}

	// build params
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	// create a desired context
	pctx := C.EVP_PKEY_CTX_new_from_name(0, kind.long_name(), 0)
	if params == 0 || pctx == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_new or OSSL_PARAM_BLD_to_param failed')
	}

	pkey := C.EVP_PKEY_new()
	assert pkey != 0

	s := C.EVP_PKEY_fromdata_init(pctx)
	if s <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata_init failed')
	}

	ss := C.EVP_PKEY_fromdata(pctx, &pkey, evp_pkey_keypair, params)
	if ss <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata failed')
	}

	pvkey := PrivateKey{
		key: pkey
	}
	// Cleans up
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)

	return pvkey
}

// from_bytes creates a new PublicKey with type of supported key and bytes array.
// If you dont provide the bytes, ie, supplied with zero-length bytes,
// it will be generated with random bytes for you.
pub fn PublicKey.from_bytes(bytes []u8, kind Kind) !PublicKey {
	// when bytes length was zero, build the key based on the random ones
	if bytes.len == 0 {
		pv := PrivateKey.new(kind: kind)!
		pbk := pv.public_key()!
		return pbk
	}
	// Get the n size parameter set from the options
	nsize := kind.nsize()
	// The public key has a size of 2 * n bytes.
	if bytes.len != 2 * nsize {
		return error('Unmatching public bytes length with kind supplied, need ${2 * nsize} bytes')
	}

	param_bld := C.OSSL_PARAM_BLD_new()
	assert param_bld != 0

	m := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'pub', bytes.data, bytes.len)
	if m <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push FAILED')
	}

	// build params
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	// create a desired context
	pctx := C.EVP_PKEY_CTX_new_from_name(0, kind.long_name(), 0)
	if params == 0 || pctx == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_new or OSSL_PARAM_BLD_to_param failed')
	}

	pkey := C.EVP_PKEY_new()
	assert pkey != 0

	s := C.EVP_PKEY_fromdata_init(pctx)
	if s <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata failed')
	}

	ss := C.EVP_PKEY_fromdata(pctx, &pkey, evp_pkey_public_key, params)
	if ss <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata failed')
	}

	pbkey := PublicKey{
		key: pkey
	}
	// Cleans up
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)

	return pbkey
}

// The enumeration of NID of SLHDSA parameters set. <br>
// See Table 2. SLH-DSA parameter sets of the Chapter 11. Parameter Sets<br>
// Each sets name indicates:
//
// 	- the hash function family (SHA2 or SHAKE) that is used to instantiate the hash functions.
//	- the length in bits of the security parameter, in the 128, 192, and 256 respectives number.
//	- the mnemonic name indicates parameter to create relatively small signatures (`s`)
//	  or to have relatively fast signature generation (`f`).
pub enum Kind {
	// SHA2-based family
	sha2_128s = C.NID_SLH_DSA_SHA2_128s
	sha2_128f = C.NID_SLH_DSA_SHA2_128f
	sha2_192s = C.NID_SLH_DSA_SHA2_192s
	sha2_192f = C.NID_SLH_DSA_SHA2_192f
	sha2_256s = C.NID_SLH_DSA_SHA2_256s
	sha2_256f = C.NID_SLH_DSA_SHA2_256f
	// SHAKE-based family
	shake_128s = C.NID_SLH_DSA_SHAKE_128s
	shake_128f = C.NID_SLH_DSA_SHAKE_128f
	shake_192s = C.NID_SLH_DSA_SHAKE_192s
	shake_192f = C.NID_SLH_DSA_SHAKE_192f
	shake_256s = C.NID_SLH_DSA_SHAKE_256s
	shake_256f = C.NID_SLH_DSA_SHAKE_256f
}

// nsize returns the size of underlying n parameter from current type.
@[inline]
fn (n Kind) nsize() int {
	match n {
		.sha2_128s, .sha2_128f, .shake_128s, .shake_128f { return 16 }
		.sha2_192s, .sha2_192f, .shake_192s, .shake_192f { return 24 }
		.sha2_256s, .sha2_256f, .shake_256s, .shake_256f { return 32 }
	}
}

fn (n Kind) str() string {
	match n {
		// vfmt off
		// SHA2-based family
		.sha2_128s { return "sha2_128s" }
		.sha2_128f { return "sha2_128f" }
		.sha2_192s { return "sha2_192s" }
		.sha2_192f { return "sha2_192f" }
		.sha2_256s { return "sha2_256s" }
		.sha2_256f { return "sha2_256f" }
		// SHAKE-based family
		.shake_128s { return "shake_128s" }
		.shake_128f { return "shake_128f" }
		.shake_192s { return "shake_192s" }
		.shake_192f { return "shake_192f" }
		.shake_256s { return "shake_256s" }
		.shake_256f { return "shake_256f" }
		// vfmt on
	}
}

// Kind long name as v string
@[inline]
fn (n Kind) ln_to_vstr() string {
	out := unsafe { n.long_name().vstring() }
	return out
}

// Kind long name as c-style string
@[inline]
fn (n Kind) long_name() &char {
	match n {
		// vfmt off
		// SHA2-based family
		.sha2_128s { return ln_slhdsa_sha2_128s }
		.sha2_128f { return ln_slhdsa_sha2_128f }
		.sha2_192s { return ln_slhdsa_sha2_192s }
		.sha2_192f { return ln_slhdsa_sha2_192f }
		.sha2_256s { return ln_slhdsa_sha2_256s }
		.sha2_256f { return ln_slhdsa_sha2_256f }
		// SHAKE-based family
		.shake_128s { return ln_slhdsa_shake_128s }
		.shake_128f { return ln_slhdsa_shake_128f }
		.shake_192s { return ln_slhdsa_shake_192s }
		.shake_192f { return ln_slhdsa_shake_192f }
		.shake_256s { return ln_slhdsa_shake_256s }
		.shake_256f { return ln_slhdsa_shake_256f }
		// vfmt on
	}
}

// Chapter 11. Parameters Set
struct ParamSet {
	// Algorithm name
	id  Kind
	n   int
	h   int
	d   int
	hp  int
	a   int
	k   int
	lgw int = 4
	m   int
	sc  int
	pkb int
	sig int
}

// Table 2. SLH-DSA parameter sets
const paramset = {
	// 						     id 	𝑛 	ℎ 	𝑑 	ℎ′  𝑎 	𝑘 	𝑙𝑔𝑤 𝑚  sc pkb  sig
	'sha2_128s':  ParamSet{.sha2_128s, 16, 63, 7, 9, 12, 14, 4, 30, 1, 32, 7856}
	'sha2_128f':  ParamSet{.sha2_128f, 16, 66, 22, 3, 6, 33, 4, 34, 1, 32, 17088}
	'sha2_192s':  ParamSet{.sha2_192s, 24, 63, 7, 9, 14, 17, 4, 39, 3, 48, 16224}
	'sha2_192f':  ParamSet{.sha2_192f, 24, 66, 22, 3, 8, 33, 4, 42, 3, 48, 35664}
	'sha2_256s':  ParamSet{.sha2_256s, 32, 64, 8, 8, 14, 22, 4, 47, 5, 64, 29792}
	'sha2_256f':  ParamSet{.sha2_256f, 32, 68, 17, 4, 9, 35, 4, 49, 5, 64, 49856}
	// SHAKE family
	'shake_128s': ParamSet{.shake_128s, 16, 63, 7, 9, 12, 14, 4, 30, 1, 32, 7856}
	'shake_128f': ParamSet{.shake_128f, 16, 66, 22, 3, 6, 33, 4, 34, 1, 32, 17088}
	'shake_192s': ParamSet{.shake_192s, 24, 63, 7, 9, 14, 17, 4, 39, 3, 48, 16224}
	'shake_192f': ParamSet{.shake_192f, 24, 66, 22, 3, 8, 33, 4, 42, 3, 48, 35664}
	'shake_256s': ParamSet{.shake_256s, 32, 64, 8, 8, 14, 22, 4, 47, 5, 64, 29792}
	'shake_256f': ParamSet{.shake_256f, 32, 68, 17, 4, 9, 35, 4, 49, 5, 64, 49856}
}

fn ParamSet.from_kind(k Kind) ParamSet {
	return paramset[k.str()]
}

// Some helpers
//
fn key_algo_name(key &C.EVP_PKEY) &char {
	name := voidptr(C.EVP_PKEY_get0_type_name(key))
	assert name != 0
	return name
}

fn key_type_name(key &C.EVP_PKEY) &char {
	kn := voidptr(C.EVP_PKEY_get0_type_name(key))
	assert kn != 0
	return kn
}

fn key_description(key &C.EVP_PKEY) &char {
	kd := voidptr(C.EVP_PKEY_get0_description(key))
	assert kd != 0
	return kd
}
