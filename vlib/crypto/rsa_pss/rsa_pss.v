// Copyright (c) 2019-2026 the V contributors. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rsa_pss

// HashAlg selects the digest used both for hashing the message and for PSS's
// internal MGF1/salt computation — RFC 8446 §4.2.3 ties these together for
// its three `rsa_pss_rsae_*` schemes (sha256/384/512 each use the
// same-named hash throughout, never a mix).
pub enum HashAlg {
	sha256
	sha384
	sha512
}

fn (h HashAlg) evp_md() &C.EVP_MD {
	return match h {
		.sha256 { C.EVP_sha256() }
		.sha384 { C.EVP_sha384() }
		.sha512 { C.EVP_sha512() }
	}
}

// KeyOptions configures key generation.
@[params]
pub struct KeyOptions {
pub mut:
	// RSA key size in bits. 2048 is the practical minimum for real-world
	// certificate interop in 2026.
	bits int = 2048
}

// PrivateKey represents an RSA private key (and its paired public key).
pub struct PrivateKey {
	evpkey &C.EVP_PKEY
}

// PrivateKey.new generates a new RSA key pair.
pub fn PrivateKey.new(opt KeyOptions) !PrivateKey {
	pctx := C.EVP_PKEY_CTX_new_id(C.EVP_PKEY_RSA, 0)
	if pctx == 0 {
		return error('EVP_PKEY_CTX_new_id failed')
	}
	if C.EVP_PKEY_keygen_init(pctx) <= 0 {
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_keygen_init failed')
	}
	if C.EVP_PKEY_CTX_set_rsa_keygen_bits(pctx, opt.bits) <= 0 {
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_CTX_set_rsa_keygen_bits failed')
	}
	mut evpkey := &C.EVP_PKEY(unsafe { nil })
	if C.EVP_PKEY_keygen(pctx, &evpkey) <= 0 {
		C.EVP_PKEY_CTX_free(pctx)
		return error('EVP_PKEY_keygen failed')
	}
	C.EVP_PKEY_CTX_free(pctx)
	return PrivateKey{
		evpkey: evpkey
	}
}

// public_key returns the PublicKey half of this key pair. OpenSSL's EVP_PKEY
// carries both halves in one opaque object, so this simply wraps the same
// handle — do not `free()` the result independently of the PrivateKey it came
// from more than once.
pub fn (pv PrivateKey) public_key() PublicKey {
	return PublicKey{
		evpkey: pv.evpkey
	}
}

// sign signs `message` (hashing it internally with `hash`) using RSA-PSS with
// a salt length equal to the digest length, matching RFC 8446 §4.2.3's
// deterministic `rsae` parameterization.
pub fn (pv PrivateKey) sign(message []u8, hash HashAlg) ![]u8 {
	ctx := C.EVP_MD_CTX_new()
	if ctx == 0 {
		return error('EVP_MD_CTX_new failed')
	}
	defer {
		C.EVP_MD_CTX_free(ctx)
	}
	mut pctx := &C.EVP_PKEY_CTX(unsafe { nil })
	if C.EVP_DigestSignInit(ctx, &pctx, hash.evp_md(), unsafe { nil }, pv.evpkey) <= 0 {
		return error('EVP_DigestSignInit failed')
	}
	if C.EVP_PKEY_CTX_set_rsa_padding(pctx, C.RSA_PKCS1_PSS_PADDING) <= 0 {
		return error('EVP_PKEY_CTX_set_rsa_padding failed')
	}
	if C.EVP_PKEY_CTX_set_rsa_pss_saltlen(pctx, C.RSA_PSS_SALTLEN_DIGEST) <= 0 {
		return error('EVP_PKEY_CTX_set_rsa_pss_saltlen failed')
	}
	if C.EVP_DigestSignUpdate(ctx, message.data, message.len) <= 0 {
		return error('EVP_DigestSignUpdate failed')
	}
	mut siglen := usize(0)
	if C.EVP_DigestSignFinal(ctx, unsafe { nil }, &siglen) <= 0 {
		return error('EVP_DigestSignFinal (length query) failed')
	}
	mut sig := []u8{len: int(siglen)}
	if C.EVP_DigestSignFinal(ctx, sig.data, &siglen) <= 0 {
		unsafe { sig.free() }
		return error('EVP_DigestSignFinal failed')
	}
	result := sig[..int(siglen)].clone()
	unsafe { sig.free() }
	return result
}

// free releases the underlying OpenSSL key. Do not use the PrivateKey (or any
// PublicKey obtained from it via `.public_key()`) after calling this.
pub fn (pv &PrivateKey) free() {
	C.EVP_PKEY_free(pv.evpkey)
}

// PublicKey represents an RSA public key, used to verify RSA-PSS signatures.
pub struct PublicKey {
	evpkey &C.EVP_PKEY
}

// verify checks that `sig` is a valid RSA-PSS signature (salt length equal to
// the digest length, per RFC 8446 §4.2.3) over `message` under this key.
pub fn (pb PublicKey) verify(message []u8, sig []u8, hash HashAlg) !bool {
	ctx := C.EVP_MD_CTX_new()
	if ctx == 0 {
		return error('EVP_MD_CTX_new failed')
	}
	defer {
		C.EVP_MD_CTX_free(ctx)
	}
	mut pctx := &C.EVP_PKEY_CTX(unsafe { nil })
	if C.EVP_DigestVerifyInit(ctx, &pctx, hash.evp_md(), unsafe { nil }, pb.evpkey) <= 0 {
		return error('EVP_DigestVerifyInit failed')
	}
	if C.EVP_PKEY_CTX_set_rsa_padding(pctx, C.RSA_PKCS1_PSS_PADDING) <= 0 {
		return error('EVP_PKEY_CTX_set_rsa_padding failed')
	}
	if C.EVP_PKEY_CTX_set_rsa_pss_saltlen(pctx, C.RSA_PSS_SALTLEN_DIGEST) <= 0 {
		return error('EVP_PKEY_CTX_set_rsa_pss_saltlen failed')
	}
	if C.EVP_DigestVerifyUpdate(ctx, message.data, message.len) <= 0 {
		return error('EVP_DigestVerifyUpdate failed')
	}
	res := C.EVP_DigestVerifyFinal(ctx, sig.data, sig.len)
	return res == 1
}

// free releases the underlying OpenSSL key handle. If this PublicKey came
// from `PrivateKey.public_key()`, do not also call `.free()` on the
// PrivateKey (or vice versa) — they share one handle.
pub fn (pb &PublicKey) free() {
	C.EVP_PKEY_free(pb.evpkey)
}
