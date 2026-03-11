// Copyright (c) V contributors. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module mldsa

const min_supported_openssl_version = u64(0x030500000)

const evp_pkey_keypair = C.EVP_PKEY_KEYPAIR
const evp_pkey_public_key = C.EVP_PKEY_PUBLIC_KEY

const ln_mldsa_44 = &char(C.LN_ML_DSA_44)
const ln_mldsa_65 = &char(C.LN_ML_DSA_65)
const ln_mldsa_87 = &char(C.LN_ML_DSA_87)

const max_seed_size = 32
const max_context_size = 255
const max_public_key_size = 2592
const max_private_key_size = 4896

// init rejects runtimes that do not provide OpenSSL 3.5 ML-DSA support.
fn init() {
	if C.OpenSSL_version_num() < min_supported_openssl_version {
		panic('Your OpenSSL version ${C.OpenSSL_version_num().hex()} does not support ML-DSA')
	}
}

// Kind identifies one of the OpenSSL ML-DSA parameter sets.
pub enum Kind {
	ml_dsa_44 = C.NID_ML_DSA_44
	ml_dsa_65 = C.NID_ML_DSA_65
	ml_dsa_87 = C.NID_ML_DSA_87
}

// long_name returns the OpenSSL long algorithm name for the selected parameter set.
@[inline]
fn (k Kind) long_name() &char {
	return match k {
		.ml_dsa_44 { ln_mldsa_44 }
		.ml_dsa_65 { ln_mldsa_65 }
		.ml_dsa_87 { ln_mldsa_87 }
	}
}

// public_key_size returns the raw public key size for this ML-DSA parameter set.
@[inline]
pub fn (k Kind) public_key_size() int {
	return match k {
		.ml_dsa_44 { 1312 }
		.ml_dsa_65 { 1952 }
		.ml_dsa_87 { 2592 }
	}
}

// private_key_size returns the raw private key size for this ML-DSA parameter set.
@[inline]
pub fn (k Kind) private_key_size() int {
	return match k {
		.ml_dsa_44 { 2560 }
		.ml_dsa_65 { 4032 }
		.ml_dsa_87 { 4896 }
	}
}

// KeyOpts configures ML-DSA key creation.
@[params]
pub struct KeyOpts {
pub mut:
	// kind selects the ML-DSA parameter set. The default is ML-DSA-44.
	kind Kind = .ml_dsa_44
	// seed imports a deterministic 32-byte seed. It is mutually exclusive with `priv`.
	seed []u8
	// priv imports a raw private key. It is mutually exclusive with `seed`.
	priv []u8
}

// SignerOpts configures ML-DSA one-shot signing and verification.
@[params]
pub struct SignerOpts {
pub mut:
	// context is an optional context string with a maximum length of 255 bytes.
	context string
	// encoding selects OpenSSL's message encoding mode. The default value of 1 enables Pure ML-DSA.
	encoding int = 1
	// entropy provides optional 32-byte deterministic per-message randomness for tests.
	entropy []u8
	// deterministic switches OpenSSL to all-zero per-message randomness when set to 1.
	deterministic int
	// mu tells OpenSSL to treat the input as the FIPS 204 mu value when set to 1.
	mu int
}

// PrivateKey stores an OpenSSL ML-DSA private keypair.
pub struct PrivateKey {
	key &C.EVP_PKEY
}

// new creates a new ML-DSA private key.
// When `seed` is set, the key is generated deterministically from that 32-byte value.
// When `priv` is set, the key is imported from the raw private key bytes.
// `seed` and `priv` are mutually exclusive.
pub fn PrivateKey.new(opt KeyOpts) !PrivateKey {
	if opt.seed.len > 0 && opt.priv.len > 0 {
		return error('KeyOpts.seed and KeyOpts.priv are mutually exclusive')
	}
	if opt.priv.len > 0 {
		return PrivateKey.from_bytes(opt.priv, opt.kind)!
	}
	if opt.seed.len > 0 {
		return PrivateKey.from_seed(opt.seed, opt.kind)!
	}
	key := C.EVP_PKEY_Q_keygen(0, 0, opt.kind.long_name())
	if key == 0 {
		return error('EVP_PKEY_Q_keygen failed')
	}
	return PrivateKey{
		key: key
	}
}

// from_seed creates a new ML-DSA private key from a deterministic 32-byte seed.
pub fn PrivateKey.from_seed(seed []u8, kind Kind) !PrivateKey {
	if seed.len != max_seed_size {
		return error('ML-DSA seed length must be ${max_seed_size} bytes')
	}
	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		return error('OSSL_PARAM_BLD_new failed')
	}
	if C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'seed', seed.data, usize(seed.len)) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push_octet_string seed failed')
	}
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	pctx := C.EVP_PKEY_CTX_new_from_name(0, kind.long_name(), 0)
	if params == 0 || pctx == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_new_from_name or OSSL_PARAM_BLD_to_param failed')
	}
	mut pkey := C.EVP_PKEY_new()
	if pkey == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_new failed')
	}
	if C.EVP_PKEY_keygen_init(pctx) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_keygen_init failed')
	}
	if C.EVP_PKEY_CTX_set_params(pctx, params) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_CTX_set_params failed')
	}
	if C.EVP_PKEY_keygen(pctx, &pkey) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_keygen failed')
	}
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)
	return PrivateKey{
		key: pkey
	}
}

// from_bytes imports a new ML-DSA private key from raw private key bytes.
pub fn PrivateKey.from_bytes(bytes []u8, kind Kind) !PrivateKey {
	if bytes.len != kind.private_key_size() {
		return error('ML-DSA private key length must be ${kind.private_key_size()} bytes')
	}
	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		return error('OSSL_PARAM_BLD_new failed')
	}
	if C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'priv', bytes.data, usize(bytes.len)) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push_octet_string priv failed')
	}
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	pctx := C.EVP_PKEY_CTX_new_from_name(0, kind.long_name(), 0)
	if params == 0 || pctx == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_new_from_name or OSSL_PARAM_BLD_to_param failed')
	}
	mut pkey := C.EVP_PKEY_new()
	if pkey == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_new failed')
	}
	if C.EVP_PKEY_fromdata_init(pctx) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata_init failed')
	}
	if C.EVP_PKEY_fromdata(pctx, &pkey, evp_pkey_keypair, params) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata failed')
	}
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)
	return PrivateKey{
		key: pkey
	}
}

// sign signs `msg` using this ML-DSA private key.
pub fn (pv PrivateKey) sign(msg []u8, opt SignerOpts) ![]u8 {
	validate_signer_opts(opt)!
	return mldsa_do_sign(pv.key, msg, opt)!
}

// verify verifies `sig` for `msg` using this ML-DSA keypair.
pub fn (pv PrivateKey) verify(sig []u8, msg []u8, opt SignerOpts) !bool {
	validate_signer_opts(opt)!
	return mldsa_do_verify(pv.key, sig, msg, opt)!
}

// public_key extracts the public key component from this ML-DSA private keypair.
pub fn (pv PrivateKey) public_key() !PublicKey {
	pbkey := C.EVP_PKEY_dup(pv.key)
	if pbkey == 0 {
		return error('EVP_PKEY_dup failed')
	}
	if C.EVP_PKEY_set_octet_string_param(pbkey, c'priv', 0, 0) <= 0 {
		C.EVP_PKEY_free(pbkey)
		return error('EVP_PKEY_set_octet_string_param failed')
	}
	return PublicKey{
		key: pbkey
	}
}

// bytes returns the raw ML-DSA private key bytes.
pub fn (pv PrivateKey) bytes() ![]u8 {
	return extract_octets(pv.key, c'priv', max_private_key_size)
}

// seed returns the retained ML-DSA key generation seed.
pub fn (pv PrivateKey) seed() ![]u8 {
	return extract_octets(pv.key, c'seed', max_seed_size)
}

// public_bytes returns the raw ML-DSA public key bytes for this keypair.
pub fn (pv PrivateKey) public_bytes() ![]u8 {
	return extract_octets(pv.key, c'pub', max_public_key_size)
}

// free releases the OpenSSL resources held by this private key.
pub fn (mut pv PrivateKey) free() {
	C.EVP_PKEY_free(pv.key)
}

// PublicKey stores an OpenSSL ML-DSA public key.
pub struct PublicKey {
	key &C.EVP_PKEY
}

// from_bytes imports a new ML-DSA public key from raw public key bytes.
pub fn PublicKey.from_bytes(bytes []u8, kind Kind) !PublicKey {
	if bytes.len != kind.public_key_size() {
		return error('ML-DSA public key length must be ${kind.public_key_size()} bytes')
	}
	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		return error('OSSL_PARAM_BLD_new failed')
	}
	if C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'pub', bytes.data, usize(bytes.len)) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		return error('OSSL_PARAM_BLD_push_octet_string pub failed')
	}
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	pctx := C.EVP_PKEY_CTX_new_from_name(0, kind.long_name(), 0)
	if params == 0 || pctx == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_new_from_name or OSSL_PARAM_BLD_to_param failed')
	}
	mut pkey := C.EVP_PKEY_new()
	if pkey == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_new failed')
	}
	if C.EVP_PKEY_fromdata_init(pctx) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata_init failed')
	}
	if C.EVP_PKEY_fromdata(pctx, &pkey, evp_pkey_public_key, params) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_PKEY_CTX_free(pctx)
		C.EVP_PKEY_free(pkey)
		return error('EVP_PKEY_fromdata failed')
	}
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_PKEY_CTX_free(pctx)
	return PublicKey{
		key: pkey
	}
}

// verify verifies `sig` for `msg` using this ML-DSA public key.
pub fn (pb PublicKey) verify(sig []u8, msg []u8, opt SignerOpts) !bool {
	validate_signer_opts(opt)!
	return mldsa_do_verify(pb.key, sig, msg, opt)!
}

// bytes returns the raw ML-DSA public key bytes.
pub fn (pb PublicKey) bytes() ![]u8 {
	return extract_octets(pb.key, c'pub', max_public_key_size)
}

// free releases the OpenSSL resources held by this public key.
pub fn (mut pb PublicKey) free() {
	C.EVP_PKEY_free(pb.key)
}

fn validate_signer_opts(opt SignerOpts) ! {
	if opt.context.len > max_context_size {
		return error('ML-DSA context length must be <= ${max_context_size} bytes')
	}
	if opt.encoding !in [0, 1] {
		return error('ML-DSA message encoding must be 0 or 1')
	}
	if opt.deterministic !in [0, 1] {
		return error('ML-DSA deterministic flag must be 0 or 1')
	}
	if opt.mu !in [0, 1] {
		return error('ML-DSA mu flag must be 0 or 1')
	}
	if opt.entropy.len != 0 && opt.entropy.len != max_seed_size {
		return error('ML-DSA test entropy length must be ${max_seed_size} bytes')
	}
}

fn extract_octets(key &C.EVP_PKEY, name &char, max_len int) ![]u8 {
	mut out_len := usize(0)
	mut buf := []u8{len: max_len}
	if C.EVP_PKEY_get_octet_string_param(key, name, buf.data, usize(buf.len), &out_len) <= 0 {
		unsafe { buf.free() }
		return error('EVP_PKEY_get_octet_string_param failed')
	}
	out := buf[..int(out_len)].clone()
	unsafe { buf.free() }
	return out
}

fn key_type_name(key &C.EVP_PKEY) &char {
	name := C.EVP_PKEY_get0_type_name(key)
	assert name != 0
	return name
}

fn push_signer_params(param_bld &C.OSSL_PARAM_BLD, opt SignerOpts, signing bool) ! {
	if opt.context.len > 0 {
		if C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'context-string', opt.context.str,
			usize(opt.context.len)) <= 0 {
			return error('OSSL_PARAM_BLD_push_octet_string context-string failed')
		}
	}
	if opt.encoding != 1 {
		if C.OSSL_PARAM_BLD_push_int(param_bld, c'message-encoding', opt.encoding) <= 0 {
			return error('OSSL_PARAM_BLD_push_int message-encoding failed')
		}
	}
	if opt.mu != 0 {
		if C.OSSL_PARAM_BLD_push_int(param_bld, c'mu', opt.mu) <= 0 {
			return error('OSSL_PARAM_BLD_push_int mu failed')
		}
	}
	if !signing {
		return
	}
	if opt.entropy.len > 0 {
		if C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'test-entropy', opt.entropy.data,
			usize(opt.entropy.len)) <= 0 {
			return error('OSSL_PARAM_BLD_push_octet_string test-entropy failed')
		}
	}
	if opt.deterministic != 0 {
		if C.OSSL_PARAM_BLD_push_int(param_bld, c'deterministic', opt.deterministic) <= 0 {
			return error('OSSL_PARAM_BLD_push_int deterministic failed')
		}
	}
}

fn mldsa_do_sign(key &C.EVP_PKEY, msg []u8, opt SignerOpts) ![]u8 {
	sctx := C.EVP_PKEY_CTX_new_from_pkey(0, key, 0)
	if sctx == 0 {
		return error('EVP_PKEY_CTX_new_from_pkey failed')
	}
	algo_name := key_type_name(key)
	sig_alg := C.EVP_SIGNATURE_fetch(0, algo_name, 0)
	if sig_alg == 0 {
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_SIGNATURE_fetch failed')
	}
	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_new failed')
	}
	push_signer_params(param_bld, opt, true) or {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return err
	}
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	if params == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_to_param failed')
	}
	if C.EVP_PKEY_sign_message_init(sctx, sig_alg, params) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_sign_message_init failed')
	}
	mut sig_len := usize(C.EVP_PKEY_size(key))
	mut buf := []u8{len: int(sig_len)}
	if C.EVP_PKEY_sign(sctx, buf.data, &sig_len, msg.data, usize(msg.len)) <= 0 {
		unsafe { buf.free() }
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_sign failed')
	}
	sig := buf[..int(sig_len)].clone()
	unsafe { buf.free() }
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_SIGNATURE_free(sig_alg)
	C.EVP_PKEY_CTX_free(sctx)
	return sig
}

fn mldsa_do_verify(key &C.EVP_PKEY, sig []u8, msg []u8, opt SignerOpts) !bool {
	sctx := C.EVP_PKEY_CTX_new_from_pkey(0, key, 0)
	if sctx == 0 {
		return error('EVP_PKEY_CTX_new_from_pkey failed')
	}
	algo_name := key_type_name(key)
	sig_alg := C.EVP_SIGNATURE_fetch(0, algo_name, 0)
	if sig_alg == 0 {
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_SIGNATURE_fetch failed')
	}
	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_new failed')
	}
	push_signer_params(param_bld, opt, false) or {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return err
	}
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	if params == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_to_param failed')
	}
	if C.EVP_PKEY_verify_message_init(sctx, sig_alg, params) <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_verify_message_init failed')
	}
	res := C.EVP_PKEY_verify(sctx, sig.data, usize(sig.len), msg.data, usize(msg.len))
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_SIGNATURE_free(sig_alg)
	C.EVP_PKEY_CTX_free(sctx)
	return res == 1
}
