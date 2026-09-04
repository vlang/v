// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

import hash
import crypto.sha256
import crypto.sha512

// This is the DEFAULT backend (flag `use_openssl` absent) -- see
// ecdsa_d_use_openssl.v for the opt-in OpenSSL backend, ecdsa_notd_use_openssl.c.v
// for the raw mbedTLS C bindings this file calls into, and README.md for the
// full rationale. Public API matches ecdsa_d_use_openssl.v's exactly, so
// every caller (net.quic, this module's own tests) compiles unchanged
// regardless of which backend is selected.

// Nid is an enumeration of the supported curves. Independent of
// ecdsa_d_use_openssl.v's own Nid (only one of the two files is ever
// compiled at once, so there's no conflict) -- this one carries no C.*
// value binding, since mbedTLS's curve identifiers are a different integer
// space (mbedtls_ecp_group_id) than OpenSSL's NIDs.
pub enum Nid {
	prime256v1
	secp384r1
	secp521r1
	secp256k1
}

// mbedtls_group_id maps this curve to mbedTLS's own mbedtls_ecp_group_id
// enum value (ecp.h), for mbedtls_ecp_group_load/mbedtls_ecdsa_genkey/
// mbedtls_ecp_set_public_key's `grp_id`/`gid` parameter.
fn (nid Nid) mbedtls_group_id() int {
	match nid {
		.prime256v1 {
			return C.MBEDTLS_ECP_DP_SECP256R1
		}
		.secp384r1 {
			return C.MBEDTLS_ECP_DP_SECP384R1
		}
		.secp521r1 {
			return C.MBEDTLS_ECP_DP_SECP521R1
		}
		.secp256k1 {
			return C.MBEDTLS_ECP_DP_SECP256K1
		}
	}
}

// byte_size returns this curve's fixed SEC1 field-element byte length --
// 32 for P-256/secp256k1, 48 for P-384, 66 for P-521. Used for buffer
// sizing (private-key bytes, ECDH shared secret) exactly where
// ecdsa_d_use_openssl.v instead derives the same number from
// `EVP_PKEY_get_bits`/`BN_num_bits` at runtime -- mbedTLS's raw API needs
// the curve remembered on the V side (see PrivateKey/PublicKey's own `nid`
// field), so it's simpler to have this as a direct lookup.
fn (nid Nid) byte_size() int {
	match nid {
		.prime256v1 {
			return 32
		}
		.secp384r1 {
			return 48
		}
		.secp521r1 {
			return 66
		}
		.secp256k1 {
			return 32
		}
	}
}

// CurveOptions represents configuration options to drive keypair generation.
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
// If you want another curve, use `pubkey, pivkey := ecdsa.generate_key(nid: .secp384r1)!` instead.
pub fn generate_key(opt CurveOptions) !(PublicKey, PrivateKey) {
	pv := PrivateKey.new(opt)!
	pb := pv.public_key() or {
		// pv was never returned to a caller who could free it -- free it here
		// before propagating the error, or its mbedTLS-allocated group/scalar/
		// point buffers leak.
		pv.free()
		return err
	}
	return pb, pv
}

// new_key_from_seed is NOT implemented for the default mbedTLS backend: unlike
// OpenSSL's current implementation (raw `BN_bin2bn(seed)`, no range check),
// mbedTLS's closest equivalent validates the resulting scalar is in
// `[1, curve_order-1]` and rejects it otherwise -- a seed that deterministically
// "works" under `-d use_openssl` today could be rejected here. That's a real
// behavioral divergence, not just missing plumbing, and needs its own design
// pass (reduce mod n first? accept the stricter rejection and document it?)
// before porting -- not silently reimplemented as if the two were equivalent.
// Build with `-d use_openssl` if you need this today.
pub fn new_key_from_seed(seed []u8, opt CurveOptions) !PrivateKey {
	return error('crypto.ecdsa: new_key_from_seed is not implemented for the default mbedTLS backend yet (OpenSSL and mbedTLS diverge on out-of-range-scalar handling here); build with -d use_openssl')
}

// PrivateKey represents ECDSA private key. Actually its a key pair,
// contains private key and public key parts.
pub struct PrivateKey {
	// mbedtls_ecdsa_context is mbedTLS's own typedef of mbedtls_ecp_keypair --
	// one opaque type covers both PrivateKey (grp+d+Q all set) and PublicKey
	// (grp+Q set, d left zero), mirroring how the OpenSSL backend reuses one
	// &C.EVP_PKEY for both.
	ctx &C.mbedtls_ecdsa_context
mut:
	// nid records which curve this key uses -- mbedTLS's raw ecp/ecdsa/ecdh
	// API (unlike OpenSSL's self-describing EVP_PKEY) needs the caller to
	// remember and re-supply the curve id on several calls.
	nid Nid
	// ks_flag with .flexible value allowing
	// flexible-size seed bytes as key.
	// When it is `.fixed`, it will use the underlying key size.
	ks_flag KeyFlag = .fixed
	// ks_size stores size of the seed bytes when ks_flag was .flexible.
	// You should set it to a non zero value
	ks_size int
}

// PrivateKey.new creates a new key pair. By default, it would create a prime256v1 based key.
// Dont forget to call `.free()` after finish with your key.
pub fn PrivateKey.new(opt CurveOptions) !PrivateKey {
	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	init_rng(mut ctr_drbg, mut entropy)!
	defer {
		free_rng(mut ctr_drbg, mut entropy)
	}

	mut ctx := &C.mbedtls_ecdsa_context{}
	C.mbedtls_ecdsa_init(ctx)
	ret := C.mbedtls_ecdsa_genkey(ctx, opt.nid.mbedtls_group_id(), C.mbedtls_ctr_drbg_random, voidptr(&ctr_drbg))
	if ret != 0 {
		C.mbedtls_ecdsa_free(ctx)
		return error_with_code('crypto.ecdsa: mbedtls_ecdsa_genkey failed', ret)
	}
	return PrivateKey{
		ctx: ctx
		nid: opt.nid
		ks_flag: .fixed
	}
}

// sign performs signing the message with the options. By default options,
// it will perform hashing before signing the message.
//
// `hash_config: .with_custom_hash` is NOT implemented for the default
// mbedTLS backend: `mbedtls_ecdsa_write_signature`'s RFC 6979 deterministic-
// nonce path (compiled in for this build) needs a real `mbedtls_md_type_t`
// to seed its internal HMAC-DRBG, and `hash.Hash` (an arbitrary caller-
// supplied hasher) carries no such identifier -- passing MBEDTLS_MD_NONE
// through, which an earlier version of this file's own doc comments claimed
// was harmless, is actually rejected outright by
// mbedtls_md_info_from_type()/mbedtls_ecdsa_sign_det_restartable(). Verify()
// has no equivalent problem (mbedtls_ecdsa_read_signature takes no md_alg
// parameter at all), so only signing is restricted here. Build with
// `-d use_openssl` if you need custom-hash signing today.
pub fn (pv PrivateKey) sign(message []u8, opt SignerOpts) ![]u8 {
	if opt.hash_config == .with_custom_hash {
		return error('crypto.ecdsa: hash_config: .with_custom_hash is not implemented for signing on the default mbedTLS backend yet (RFC 6979 deterministic nonces need a real digest algorithm identifier, which an arbitrary hash.Hash does not carry); build with -d use_openssl')
	}
	md_alg, digest := calc_digest(pv.nid, message, opt)!
	return sign_digest(pv.ctx, md_alg, digest)!
}

// sign_with_options signs message with the options. It will be deprecated,
// Use `PrivateKey.sign()` instead.
@[deprecated: 'use PrivateKey.sign() instead']
pub fn (pv PrivateKey) sign_with_options(message []u8, opt SignerOpts) ![]u8 {
	return pv.sign(message, opt)
}

// bytes represent private key as bytes.
pub fn (pv PrivateKey) bytes() ![]u8 {
	mut d := C.mbedtls_mpi{}
	C.mbedtls_mpi_init(&d)
	defer {
		C.mbedtls_mpi_free(&d)
	}
	ret := C.mbedtls_ecp_export(pv.ctx, unsafe { nil }, &d, unsafe { nil })
	if ret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_export failed', ret)
	}
	size := if pv.ks_flag == .flexible { pv.ks_size } else { pv.nid.byte_size() }
	mut buf := []u8{len: size}
	wret := C.mbedtls_mpi_write_binary(&d, buf.data, usize(size))
	if wret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_mpi_write_binary failed', wret)
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
	mut q := C.mbedtls_ecp_point{}
	C.mbedtls_ecp_point_init(&q)
	defer {
		C.mbedtls_ecp_point_free(&q)
	}
	ret := C.mbedtls_ecp_export(pv.ctx, unsafe { nil }, unsafe { nil }, &q)
	if ret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_export failed', ret)
	}
	mut pub_ctx := &C.mbedtls_ecdsa_context{}
	C.mbedtls_ecdsa_init(pub_ctx)
	// Only the public point is copied in -- the fresh context's own private
	// scalar is never set, mirroring the OpenSSL backend's own "duplicate
	// then scrub the private component" contract for PublicKey.
	sret := C.mbedtls_ecp_set_public_key(pv.nid.mbedtls_group_id(), pub_ctx, &q)
	if sret != 0 {
		C.mbedtls_ecdsa_free(pub_ctx)
		return error_with_code('crypto.ecdsa: mbedtls_ecp_set_public_key failed', sret)
	}
	return PublicKey{
		ctx: pub_ctx
		nid: pv.nid
	}
}

// derive_shared_secret performs ECDH key agreement between this private key
// and a peer's public key, returning the raw shared secret (the X-coordinate
// of the resulting curve point only, per SEC1 — this is what mbedTLS's
// mbedtls_ecdh_compute_shared already returns, and is exactly what TLS 1.3's
// key schedule expects as ECDHE input; it is NOT the full uncompressed point).
// Both keys must use the same curve.
pub fn (pv PrivateKey) derive_shared_secret(peer PublicKey) ![]u8 {
	if pv.nid != peer.nid {
		return error('crypto.ecdsa: derive_shared_secret requires both keys to use the same curve')
	}
	mut grp := C.mbedtls_ecp_group{}
	mut d := C.mbedtls_mpi{}
	mut peer_q := C.mbedtls_ecp_point{}
	mut z := C.mbedtls_mpi{}
	C.mbedtls_ecp_group_init(&grp)
	C.mbedtls_mpi_init(&d)
	C.mbedtls_ecp_point_init(&peer_q)
	C.mbedtls_mpi_init(&z)
	defer {
		C.mbedtls_ecp_group_free(&grp)
		C.mbedtls_mpi_free(&d)
		C.mbedtls_ecp_point_free(&peer_q)
		C.mbedtls_mpi_free(&z)
	}
	eret := C.mbedtls_ecp_export(pv.ctx, &grp, &d, unsafe { nil })
	if eret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_export (own key) failed', eret)
	}
	peret := C.mbedtls_ecp_export(peer.ctx, unsafe { nil }, unsafe { nil }, &peer_q)
	if peret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_export (peer key) failed', peret)
	}
	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	init_rng(mut ctr_drbg, mut entropy)!
	defer {
		free_rng(mut ctr_drbg, mut entropy)
	}
	cret := C.mbedtls_ecdh_compute_shared(&grp, &z, &peer_q, &d, C.mbedtls_ctr_drbg_random, voidptr(&ctr_drbg))
	if cret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecdh_compute_shared failed (mismatched curve or invalid peer key?)', cret)
	}
	size := pv.nid.byte_size()
	mut secret := []u8{len: size}
	wret := C.mbedtls_mpi_write_binary(&z, secret.data, usize(size))
	if wret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_mpi_write_binary failed', wret)
	}
	return secret
}

// equal compares two private keys was equal. Delegates to the public
// components' equality, matching OpenSSL's own EVP_PKEY_eq semantics of
// comparing public (not raw private-scalar) material.
pub fn (priv_key PrivateKey) equal(other PrivateKey) bool {
	// Each defer is registered immediately after its own allocation succeeds,
	// not after both -- registering both together after the second fallible
	// call would leak pa's mbedTLS context if other.public_key() then failed
	// (defer only arms once the defer statement itself is reached).
	mut pa := priv_key.public_key() or { return false }
	defer {
		pa.free()
	}
	mut pb := other.public_key() or { return false }
	defer {
		pb.free()
	}
	return pa.equal(pb)
}

// free clears out allocated memory for PrivateKey. Dont use PrivateKey after calling `.free()`
//
// Double-free behavior is backend-dependent: `-d use_openssl` aborts the
// process on a true double-free (OpenSSL's EVP_PKEY_free); this default
// mbedTLS backend's mbedtls_ecdsa_free is documented safe on an
// already-freed/zeroed context (a silent no-op). Never rely on either --
// call `.free()` exactly once.
pub fn (pv &PrivateKey) free() {
	C.mbedtls_ecdsa_free(pv.ctx)
}

// PublicKey represents ECDSA public key for verifying message.
pub struct PublicKey {
	ctx &C.mbedtls_ecdsa_context
mut:
	nid Nid
}

// verify verifies a message with the signature are valid with public key provided .
// You should provide it with the same SignerOpts used with the `.sign()` call.
// or verify would fail (false).
pub fn (pb PublicKey) verify(message []u8, sig []u8, opt SignerOpts) !bool {
	// mbedtls_ecdsa_read_signature takes no md_alg parameter (unlike
	// write_signature/sign) -- it verifies against whatever digest bytes are
	// given, algorithm-agnostic.
	_, digest := calc_digest(pb.nid, message, opt)!
	ret := C.mbedtls_ecdsa_read_signature(pb.ctx, digest.data, usize(digest.len), sig.data, usize(sig.len))
	return ret == 0
}

// equal compares two public keys was equal.
pub fn (pub_key PublicKey) equal(other PublicKey) bool {
	mut qa := C.mbedtls_ecp_point{}
	mut qb := C.mbedtls_ecp_point{}
	C.mbedtls_ecp_point_init(&qa)
	C.mbedtls_ecp_point_init(&qb)
	defer {
		C.mbedtls_ecp_point_free(&qa)
		C.mbedtls_ecp_point_free(&qb)
	}
	if C.mbedtls_ecp_export(pub_key.ctx, unsafe { nil }, unsafe { nil }, &qa) != 0 {
		return false
	}
	if C.mbedtls_ecp_export(other.ctx, unsafe { nil }, unsafe { nil }, &qb) != 0 {
		return false
	}
	return C.mbedtls_ecp_point_cmp(&qa, &qb) == 0
}

// free clears out allocated memory for PublicKey. Dont use PublicKey after calling `.free()`
//
// See PrivateKey.free()'s own doc comment for this backend's double-free
// behavior (a silent no-op here, unlike `-d use_openssl`'s abort).
pub fn (pb &PublicKey) free() {
	C.mbedtls_ecdsa_free(pb.ctx)
}

// uncompressed_bytes returns the public key as an uncompressed EC point,
// `0x04 || X || Y` (SEC1 §2.3.3) — the wire format TLS 1.3's key_share
// extension uses for the secp256r1 (P-256) group (RFC 8446 §4.2.8.2).
pub fn (pb PublicKey) uncompressed_bytes() ![]u8 {
	mut grp := C.mbedtls_ecp_group{}
	mut q := C.mbedtls_ecp_point{}
	C.mbedtls_ecp_group_init(&grp)
	C.mbedtls_ecp_point_init(&q)
	defer {
		C.mbedtls_ecp_group_free(&grp)
		C.mbedtls_ecp_point_free(&q)
	}
	eret := C.mbedtls_ecp_export(pb.ctx, &grp, unsafe { nil }, &q)
	if eret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_export failed', eret)
	}
	mut buf := []u8{len: int(C.MBEDTLS_ECP_MAX_PT_LEN)}
	mut olen := usize(0)
	wret := C.mbedtls_ecp_point_write_binary(&grp, &q, C.MBEDTLS_ECP_PF_UNCOMPRESSED, &olen, buf.data, usize(buf.len))
	if wret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_point_write_binary failed', wret)
	}
	return buf[..int(olen)].clone()
}

// PublicKey.from_uncompressed_bytes reconstructs a public key (no private
// component) from an uncompressed EC point `0x04 || X || Y`, as received on
// the wire in a TLS 1.3 key_share extension. `opt.nid` must match the curve
// the peer actually used (v1 callers of net.quic only ever use `.prime256v1`
// here, since X25519 is handled entirely by crypto.x25519 instead).
pub fn PublicKey.from_uncompressed_bytes(bytes []u8, opt CurveOptions) !PublicKey {
	if bytes.len == 0 {
		return error('empty public key bytes')
	}
	if bytes[0] != 0x04 {
		return error('only uncompressed EC points (0x04 prefix) are supported, got tag ${bytes[0]:02x}')
	}
	mut grp := C.mbedtls_ecp_group{}
	C.mbedtls_ecp_group_init(&grp)
	defer {
		C.mbedtls_ecp_group_free(&grp)
	}
	gret := C.mbedtls_ecp_group_load(&grp, opt.nid.mbedtls_group_id())
	if gret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_group_load failed', gret)
	}
	mut q := C.mbedtls_ecp_point{}
	C.mbedtls_ecp_point_init(&q)
	defer {
		C.mbedtls_ecp_point_free(&q)
	}
	rret := C.mbedtls_ecp_point_read_binary(&grp, &q, bytes.data, usize(bytes.len))
	if rret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecp_point_read_binary failed', rret)
	}
	// mbedtls_ecp_point_read_binary does not itself validate the point is on
	// the curve (per its own doc comment) -- check explicitly, matching the
	// implicit trust boundary OpenSSL's EVP_PKEY_fromdata enforces internally.
	cret := C.mbedtls_ecp_check_pubkey(&grp, &q)
	if cret != 0 {
		return error_with_code('crypto.ecdsa: point is not a valid point on the curve', cret)
	}
	mut pub_ctx := &C.mbedtls_ecdsa_context{}
	C.mbedtls_ecdsa_init(pub_ctx)
	sret := C.mbedtls_ecp_set_public_key(opt.nid.mbedtls_group_id(), pub_ctx, &q)
	if sret != 0 {
		C.mbedtls_ecdsa_free(pub_ctx)
		return error_with_code('crypto.ecdsa: mbedtls_ecp_set_public_key failed', sret)
	}
	return PublicKey{
		ctx: pub_ctx
		nid: opt.nid
	}
}

// Helpers
//
// init_rng seeds a fresh ctr_drbg/entropy pair for one top-level operation
// (keygen/sign/ECDH) -- per-call lifecycle, not a persistent global, mirroring
// how the OpenSSL backend also creates fresh EVP_PKEY_CTX state per call.
// Mirrors net.mbedtls's own ssl_connection.c.v init_rng/free_rng exactly;
// duplicated locally rather than imported since crypto.* never imports
// net.* in this repo.
fn init_rng(mut ctr_drbg C.mbedtls_ctr_drbg_context, mut entropy C.mbedtls_entropy_context) ! {
	C.mbedtls_ctr_drbg_init(&ctr_drbg)
	C.mbedtls_entropy_init(&entropy)
	ret := C.mbedtls_ctr_drbg_seed(&ctr_drbg, C.mbedtls_entropy_func, voidptr(&entropy), unsafe { nil }, 0)
	if ret != 0 {
		C.mbedtls_ctr_drbg_free(&ctr_drbg)
		C.mbedtls_entropy_free(&entropy)
		return error_with_code('crypto.ecdsa: failed to seed RNG', ret)
	}
}

fn free_rng(mut ctr_drbg C.mbedtls_ctr_drbg_context, mut entropy C.mbedtls_entropy_context) {
	C.mbedtls_ctr_drbg_free(&ctr_drbg)
	C.mbedtls_entropy_free(&entropy)
}

// calc_digest hashes `message` per `opt`, choosing SHA-256/384/512 by curve
// bit-size using the exact same <=256/<=384/else thresholds
// ecdsa_d_use_openssl.v's own default_digest() uses -- but computes the hash
// in pure V (crypto.sha256/crypto.sha512, already used elsewhere in this
// repo, e.g. net.quic's own TLS 1.3 code) rather than via mbedTLS's md.h, so
// this file's C surface stays limited to the EC primitives themselves.
// Returns the mbedtls_md_type_t the digest corresponds to (needed by
// mbedtls_ecdsa_write_signature/read_signature) alongside the digest bytes.
fn calc_digest(nid Nid, message []u8, opt SignerOpts) !(int, []u8) {
	if message.len == 0 {
		return error('null-length messages')
	}
	match opt.hash_config {
		.with_no_hash, .with_recommended_hash {
			bits := nid.byte_size() * 8
			return match true {
				bits <= 256 { C.MBEDTLS_MD_SHA256, sha256.sum256(message) }
				bits <= 384 { C.MBEDTLS_MD_SHA384, sha512.sum384(message) }
				else { C.MBEDTLS_MD_SHA512, sha512.sum512(message) }
			}
		}
		.with_custom_hash {
			mut cfg := opt
			if !cfg.allow_custom_hash {
				return error('custom hash was not allowed, set it into true')
			}
			if cfg.custom_hash == unsafe { nil } {
				return error('Custom hasher was not defined')
			}
			key_size := nid.byte_size()
			if key_size > cfg.custom_hash.size() {
				if !cfg.allow_smaller_size {
					return error('Hash into smaller size than current key size was not allowed')
				}
			}
			cfg.custom_hash.reset()
			_ := cfg.custom_hash.write(message)!
			digest := cfg.custom_hash.sum([]u8{})
			// Custom-hash callers don't get to pick an mbedtls_md_type_t
			// (they're bypassing the recommended-digest path entirely), so
			// MBEDTLS_MD_NONE (0) is returned as a placeholder. This is fine
			// for verify() (mbedtls_ecdsa_read_signature takes no md_alg
			// parameter at all) but NOT for sign() -- PrivateKey.sign()
			// rejects .with_custom_hash outright before ever reaching
			// sign_digest(), because mbedtls_ecdsa_write_signature's RFC
			// 6979 deterministic-nonce path needs a real digest-algorithm
			// identifier to seed its HMAC-DRBG and hard-rejects
			// MBEDTLS_MD_NONE (mbedtls_md_info_from_type returns nil for
			// it) -- see PrivateKey.sign()'s own doc comment.
			return 0, digest
		}
	}
}

// sign_digest signs an already-hashed digest with the key, producing a DER-
// encoded Ecdsa-Sig-Value (SEQUENCE { r INTEGER, s INTEGER }) -- the same
// encoding OpenSSL's EVP_PKEY_sign produces for EC keys, so external
// verifiers don't need to know which backend signed. Uses RFC 6979
// deterministic nonces automatically (MBEDTLS_ECDSA_DETERMINISTIC is
// compiled into the vendored mbedTLS build) -- f_rng below is used only for
// blinding, per mbedtls_ecdsa_write_signature's own doc comment.
fn sign_digest(ctx &C.mbedtls_ecdsa_context, md_alg int, digest []u8) ![]u8 {
	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	init_rng(mut ctr_drbg, mut entropy)!
	defer {
		free_rng(mut ctr_drbg, mut entropy)
	}
	mut sig := []u8{len: int(C.MBEDTLS_ECDSA_MAX_LEN)}
	mut slen := usize(0)
	ret := C.mbedtls_ecdsa_write_signature(ctx, md_alg, digest.data, usize(digest.len), sig.data, usize(sig.len), &slen, C.mbedtls_ctr_drbg_random, voidptr(&ctr_drbg))
	if ret != 0 {
		return error_with_code('crypto.ecdsa: mbedtls_ecdsa_write_signature failed', ret)
	}
	return sig[..int(slen)].clone()
}
