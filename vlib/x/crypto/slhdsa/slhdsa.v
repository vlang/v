// Copyright (c) blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module slhdsa

// PrivateKey represents SLH-DSA keypair.
pub struct PrivateKey {
	key &C.EVP_PKEY
}

// new creates a new SLH-DSA PrivateKey based on the supplied options.
// By default, it will create a `SLH-DSA-SHA2-128s` key with random generator.
// See `enum Kind` for all of availables choice's type (kind) of key.
// Its also support to generate keys based on the seed or private bytes through
// provided options. See available options in `KeyOpts`.
// Example:
// ```
// PrivateKey.new()!
// PrivateKey.new(kind: .sha2_128s)
// ```
pub fn PrivateKey.new(opt KeyOpts) !PrivateKey {
	match opt.flag {
		// default random generated key, its recommended way
		0 {
			kind := opt.kind.long_name()
			key := C.EVP_PKEY_Q_keygen(0, 0, kind)
			if key == 0 {
				return error('EVP_PKEY_Q_keygen failed')
			}
			return PrivateKey{
				key: key
			}
		}
		// use seed bytes, usually for testing purposes
		1 {
			// if you dont provides seed bytes, it will discarded, and use random one
			return PrivateKey.from_seed(opt.seed, opt.kind)!
		}
		// use private bytes, usually for testing purposes
		2 {
			// if you dont provides private bytes, it will discarded, and use random one
			return PrivateKey.from_bytes(opt.priv, opt.kind)!
		}
		else {
			return error('Unsupported flag')
		}
	}
}

// sign signs the message using this key under desired options in opt.
pub fn (pv PrivateKey) sign(msg []u8, opt SignerOpts) ![]u8 {
	if msg.len == 0 {
		return error('Zero length of messages')
	}
	if opt.context.len > 255 {
		return error('The context string size was over than 255')
	}
	out := slhdsa_do_sign(pv.key, msg, opt)!
	return out
}

// verify verifies signature for the message msg under provided options.
// Its possible because of under the hood, private key is a key pair.
pub fn (pv PrivateKey) verify(sig []u8, msg []u8, opt SignerOpts) !bool {
	if opt.context.len > 255 {
		return error('The context string size was over than 255')
	}
	return slhdsa_do_verify(pv.key, sig, msg, opt)!
}

const default_bioread_bufsize = 4096

// dump_key represents PrivateKey in human readable string.
pub fn (pv PrivateKey) dump_key() !string {
	bo := C.BIO_new(C.BIO_s_mem())
	if bo == 0 {
		C.BIO_free_all(bo)
		return error('BIO_new failed')
	}
	n := C.EVP_PKEY_print_private(bo, pv.key, 2, 0)
	if n <= 0 {
		C.BIO_free_all(bo)
		return error('print private failed')
	}
	size := usize(0)
	buf := []u8{len: default_bioread_bufsize}
	m := C.BIO_read_ex(bo, buf.data, buf.len, &size)

	if m <= 0 {
		unsafe { buf.free() }
		C.BIO_free_all(bo)
		return error('BIO_read_ex failed')
	}
	output := buf[..size].bytestr()

	// Cleans up and return the result
	unsafe { buf.free() }
	C.BIO_free_all(bo)

	return output
}

// public_key gets the public part of this private key as a PublicKey.
pub fn (pv PrivateKey) public_key() !PublicKey {
	pbkey := C.EVP_PKEY_dup(pv.key)
	// we clears out the private bits from the key
	// by setting it into null
	n := C.EVP_PKEY_set_octet_string_param(pbkey, c'priv', 0, 0)
	if n <= 0 {
		C.EVP_PKEY_free(pbkey)
		return error('EVP_PKEY_set_octet_string_param')
	}

	return PublicKey{
		key: pbkey
	}
}

// free releases memory occupied by this key.
pub fn (mut pv PrivateKey) free() {
	C.EVP_PKEY_free(pv.key)
}

// PublicKey represents public key part from the key.
pub struct PublicKey {
	key &C.EVP_PKEY
}

// verify verifies signature sig for messages msg under options provided.
pub fn (pb PublicKey) verify(sig []u8, msg []u8, opt SignerOpts) !bool {
	if opt.context.len > 255 {
		return error('The context string size was over than 255')
	}
	return slhdsa_do_verify(pb.key, sig, msg, opt)!
}

// dump_key dumps this public key as a human readable string.
pub fn (pb PublicKey) dump_key() !string {
	bo := C.BIO_new(C.BIO_s_mem())
	n := C.EVP_PKEY_print_public(bo, pb.key, 2, 0)
	if n <= 0 {
		C.BIO_free_all(bo)
		return error('EVP_PKEY_print_public failed')
	}
	size := usize(0)
	mut m := C.BIO_read_ex(bo, 0, default_bioread_bufsize, &size)
	mut buf := []u8{len: int(size)}
	m = C.BIO_read_ex(bo, buf.data, buf.len, &size)
	if m <= 0 {
		C.BIO_free_all(bo)
		return error('BIO_read_ex failed')
	}

	output := buf[..size].bytestr()
	// Cleans up
	unsafe { buf.free() }
	C.BIO_free_all(bo)

	return output
}

// free releases memory occupied by this public key
pub fn (mut pb PublicKey) free() {
	C.EVP_PKEY_free(pb.key)
}

// the biggest supported size of seed was 32 bytes length, where private key contains 4*32 bytes
const default_privkey_buffer = 128

fn (pv PrivateKey) bytes() ![]u8 {
	priv_len := usize(0)
	priv := []u8{len: default_privkey_buffer}

	n := C.EVP_PKEY_get_octet_string_param(pv.key, c'priv', priv.data, priv.len, &priv_len)
	if n <= 0 {
		return error('EVP_PKEY_get_octet_string_param failed')
	}
	out := priv[..int(priv_len)].clone()
	unsafe { priv.free() }
	return out
}

// the biggest public key size was 64 bytes length, where public key
// was contains 2*n, so we relaxed it to 2*72 bytes
const default_pubkey_buffer = 144

fn (pv PrivateKey) public_bytes() ![]u8 {
	pblen := usize(0)
	buf := []u8{len: default_pubkey_buffer}
	n := C.EVP_PKEY_get_octet_string_param(pv.key, c'pub', buf.data, buf.len, &pblen)
	if n <= 0 {
		return error('EVP_PKEY_get_octet_string_param failed')
	}

	out := buf[..pblen].clone()
	unsafe { buf.free() }

	return out
}

// Signing and verifying Helpers
//
// slhdsa_do_sign performs signing msg with SLH-DSA key.
@[inline]
fn slhdsa_do_sign(key &C.EVP_PKEY, msg []u8, opt SignerOpts) ![]u8 {
	sctx := C.EVP_PKEY_CTX_new_from_pkey(0, key, 0)
	if sctx == 0 {
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_CTX_new_from_pkey failed')
	}

	// gets algo name from key
	algo_name := key_type_name(key)
	sig_alg := C.EVP_SIGNATURE_fetch(0, algo_name, 0)
	if sig_alg == 0 {
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_SIGNATURE_fetch failed')
	}

	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_new failed')
	}
	// if context string was set into non-null string, then we set
	// `context-string` params into context key generator.
	if opt.context.len > 0 {
		// OSSL_PARAM_octet_string("context-string", (unsigned char *)"A context string", 33),
		o := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'context-string', opt.context.str,
			opt.context.len)
		if o <= 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('OSSL_PARAM_BLD_push_octet_string FAILED')
		}
	}
	// handle entropy testing
	if opt.entropy.len > 0 {
		if opt.encoding != 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('encoding need 0 for testing')
		}
		o := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'test-entropy', opt.entropy.data,
			opt.entropy.len)
		if o <= 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('OSSL_PARAM_BLD_push_octet_string failed')
		}
		oo := C.OSSL_PARAM_BLD_push_int(param_bld, c'message-encoding', opt.encoding)
		if oo <= 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('OSSL_PARAM_BLD_push_int FAILED')
		}
	}
	if opt.encoding == 0 {
		oo := C.OSSL_PARAM_BLD_push_int(param_bld, c'message-encoding', opt.encoding)
		if oo <= 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('OSSL_PARAM_BLD_push_int FAILED')
		}
	}
	// build params
	params := C.OSSL_PARAM_BLD_to_param(param_bld)

	// Then init the context with this params
	x := C.EVP_PKEY_sign_message_init(sctx, sig_alg, params)
	if x <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_sign_message_init failed')
	}

	sig_len := usize(C.EVP_PKEY_size(key))
	buf := []u8{len: int(sig_len)}
	// Do signing the msg and updates the sig_len.
	m := C.EVP_PKEY_sign(sctx, buf.data, &sig_len, msg.data, msg.len)
	if m <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_sign_message_init failed')
	}

	// return the copy of the sig
	sig := buf[..sig_len].clone()

	// cleans up
	unsafe { buf.free() }
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)
	C.EVP_SIGNATURE_free(sig_alg)
	C.EVP_PKEY_CTX_free(sctx)

	return sig
}

// slhdsa_do_verify performs verifying of provided signature in sign for the msg message.
@[inline]
fn slhdsa_do_verify(key &C.EVP_PKEY, sign []u8, msg []u8, opt SignerOpts) !bool {
	sctx := C.EVP_PKEY_CTX_new_from_pkey(0, key, 0)
	if sctx == 0 {
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_PKEY_CTX_new_from_pkey failed')
	}

	// get the algo signature name from the key
	algo_name := key_type_name(key)
	sig_alg := C.EVP_SIGNATURE_fetch(0, algo_name, 0)
	if sig_alg == 0 {
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('EVP_SIGNATURE_fetch failed')
	}

	param_bld := C.OSSL_PARAM_BLD_new()
	if param_bld == 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_new failed')
	}

	// handle context option
	if opt.context.len > 0 {
		o := C.OSSL_PARAM_BLD_push_octet_string(param_bld, c'context-string', opt.context.str,
			opt.context.len)
		if o <= 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('OSSL_PARAM_BLD_push failed')
		}
	}
	// for testing
	if opt.encoding == 0 {
		oo := C.OSSL_PARAM_BLD_push_int(param_bld, c'message-encoding', opt.encoding)
		if oo <= 0 {
			C.OSSL_PARAM_BLD_free(param_bld)
			C.EVP_SIGNATURE_free(sig_alg)
			C.EVP_PKEY_CTX_free(sctx)
			return error('OSSL_PARAM_BLD_push FAILED')
		}
	}
	// build params
	params := C.OSSL_PARAM_BLD_to_param(param_bld)
	n := C.EVP_PKEY_verify_message_init(sctx, sig_alg, params)
	if n <= 0 {
		C.OSSL_PARAM_BLD_free(param_bld)
		C.OSSL_PARAM_free(params)
		C.EVP_SIGNATURE_free(sig_alg)
		C.EVP_PKEY_CTX_free(sctx)
		return error('OSSL_PARAM_BLD_push FAILED')
	}

	// do verifying of the signature.
	m := C.EVP_PKEY_verify(sctx, sign.data, sign.len, msg.data, msg.len)

	// Cleans up
	C.EVP_SIGNATURE_free(sig_alg)
	C.EVP_PKEY_CTX_free(sctx)
	C.OSSL_PARAM_BLD_free(param_bld)
	C.OSSL_PARAM_free(params)

	return m == 1
}
