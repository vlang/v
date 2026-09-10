// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

// This is the DEFAULT backend (flag `use_openssl` absent) -- see
// ecdsa_d_use_openssl.c.v for the opt-in OpenSSL backend and README.md for
// the full rationale. `crypto.ecdsa` deliberately does NOT `import
// net.mbedtls` (crypto.* never imports net.* anywhere in this repo) -- this
// file declares its own independent set of mbedTLS C bindings, self-
// contained exactly like the OpenSSL file is.
//
// The `@VEXEROOT/thirdparty/mbedtls/library/*.o` list below MUST stay an
// EXACT, COMPLETE, byte-for-byte mirror of net.mbedtls's own list in
// mbedtls.c.v -- not a trimmed subset, even though most of the ssl_*.o/
// debug.o files are never called by this file's own code. This isn't just
// about link-time dedup (vlib/v/ast/cflags.v's add_unique_cflags/has_cflag
// dedupe identical `(os, name, value)` #flag strings so the linker only
// ever sees one copy of a given .o). It's also about which MODULE a given
// #flag string's attribution lands on: when both this file and
// mbedtls.c.v declare the *same* -I/.o strings, whichever module's
// declaration gets registered first in the parse order "wins" that
// string's `mod` field in the compiler's cflag table, and V's
// build_thirdparty_obj_file()/get_rest_of_module_cflags() looks up a .o's
// needed -I flags ONLY among flags sharing that same winning `mod`. A
// PARTIAL mirror (originally missing debug.o and the 14 ssl_*.o files)
// let this file's own -I declarations "win" attribution while
// net.mbedtls's debug.o -- which this file never declared, so
// net.mbedtls uniquely owned it -- ended up needing -I flags that had all
// been attributed to crypto.ecdsa instead: net.quic's full test suite
// failed 54/54 with `mbedtls/build_info.h: No such file or directory`
// while trying to compile debug.c with zero -I flags on the command line.
// Keeping every single object identical between the two files makes
// parse order irrelevant: whichever module wins any one flag consistently
// wins the *whole* overlapping set, so a .o's needed -I flags are always
// attributed to the same module that owns the .o itself. A hand-picked
// minimal subset was tried first and separately ran into repeated link
// errors from transitive internals this file never calls directly
// (ctr_drbg's own AES-based derivation function, md.c's PSA
// status-translation helper, and likely more not yet hit) -- mbedTLS's
// internal call graph between .o files isn't fully reflected in the
// headers this file binds against, so re-deriving a minimal list is
// whack-a-mole on two independent axes, not one.

#flag -I @VEXEROOT/thirdparty/mbedtls/library

#flag -I @VEXEROOT/thirdparty/mbedtls/include

#flag windows -DWIN32_LEAN_AND_MEAN

#flag windows -lws2_32

#flag -I @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/include

#flag -I @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/include/everest

#flag -I @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/include/everest/kremlib

#flag @VEXEROOT/thirdparty/mbedtls/library/aes.o

#flag @VEXEROOT/thirdparty/mbedtls/library/aesce.o

#flag @VEXEROOT/thirdparty/mbedtls/library/aesni.o

#flag @VEXEROOT/thirdparty/mbedtls/library/aria.o

#flag @VEXEROOT/thirdparty/mbedtls/library/asn1parse.o

#flag @VEXEROOT/thirdparty/mbedtls/library/asn1write.o

#flag @VEXEROOT/thirdparty/mbedtls/library/base64.o

#flag @VEXEROOT/thirdparty/mbedtls/library/bignum.o

#flag @VEXEROOT/thirdparty/mbedtls/library/bignum_core.o

#flag @VEXEROOT/thirdparty/mbedtls/library/bignum_mod.o

#flag @VEXEROOT/thirdparty/mbedtls/library/bignum_mod_raw.o

#flag @VEXEROOT/thirdparty/mbedtls/library/block_cipher.o

#flag @VEXEROOT/thirdparty/mbedtls/library/camellia.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ccm.o

#flag @VEXEROOT/thirdparty/mbedtls/library/chacha20.o

#flag @VEXEROOT/thirdparty/mbedtls/library/chachapoly.o

#flag @VEXEROOT/thirdparty/mbedtls/library/cipher.o

#flag @VEXEROOT/thirdparty/mbedtls/library/cipher_wrap.o

#flag @VEXEROOT/thirdparty/mbedtls/library/cmac.o

#flag @VEXEROOT/thirdparty/mbedtls/library/constant_time.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ctr_drbg.o

#flag @VEXEROOT/thirdparty/mbedtls/library/debug.o

#flag @VEXEROOT/thirdparty/mbedtls/library/des.o

#flag @VEXEROOT/thirdparty/mbedtls/library/dhm.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ecdh.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ecdsa.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ecjpake.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ecp.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ecp_curves.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ecp_curves_new.o

#flag @VEXEROOT/thirdparty/mbedtls/library/entropy.o

#flag @VEXEROOT/thirdparty/mbedtls/library/entropy_poll.o

#flag @VEXEROOT/thirdparty/mbedtls/library/error.o

#flag @VEXEROOT/thirdparty/mbedtls/library/gcm.o

#flag @VEXEROOT/thirdparty/mbedtls/library/hkdf.o

#flag @VEXEROOT/thirdparty/mbedtls/library/hmac_drbg.o

#flag @VEXEROOT/thirdparty/mbedtls/library/lmots.o

#flag @VEXEROOT/thirdparty/mbedtls/library/lms.o

#flag @VEXEROOT/thirdparty/mbedtls/library/md5.o

#flag @VEXEROOT/thirdparty/mbedtls/library/md.o

#flag @VEXEROOT/thirdparty/mbedtls/library/memory_buffer_alloc.o

#flag @VEXEROOT/thirdparty/mbedtls/library/mps_reader.o

#flag @VEXEROOT/thirdparty/mbedtls/library/mps_trace.o

#flag @VEXEROOT/thirdparty/mbedtls/library/net_sockets.o

#flag @VEXEROOT/thirdparty/mbedtls/library/nist_kw.o

#flag @VEXEROOT/thirdparty/mbedtls/library/oid.o

#flag @VEXEROOT/thirdparty/mbedtls/library/padlock.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pem.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pk.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pkcs12.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pkcs5.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pkcs7.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pk_ecc.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pkparse.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pk_wrap.o

#flag @VEXEROOT/thirdparty/mbedtls/library/pkwrite.o

#flag @VEXEROOT/thirdparty/mbedtls/library/platform.o

#flag @VEXEROOT/thirdparty/mbedtls/library/platform_util.o

#flag @VEXEROOT/thirdparty/mbedtls/library/poly1305.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_aead.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_cipher.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_client.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_driver_wrappers_no_static.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_ecp.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_ffdh.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_hash.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_mac.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_pake.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_random.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_rsa.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_se.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_slot_management.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_crypto_storage.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_its_file.o

#flag @VEXEROOT/thirdparty/mbedtls/library/psa_util.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ripemd160.o

#flag @VEXEROOT/thirdparty/mbedtls/library/rsa_alt_helpers.o

#flag @VEXEROOT/thirdparty/mbedtls/library/rsa.o

#flag @VEXEROOT/thirdparty/mbedtls/library/sha1.o

#flag @VEXEROOT/thirdparty/mbedtls/library/sha256.o

#flag @VEXEROOT/thirdparty/mbedtls/library/sha3.o

#flag @VEXEROOT/thirdparty/mbedtls/library/sha512.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_cache.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_ciphersuites.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_client.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_cookie.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_debug_helpers_generated.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_msg.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_ticket.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls12_client.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls12_server.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_client.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_generic.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_keys.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls13_server.o

#flag @VEXEROOT/thirdparty/mbedtls/library/ssl_tls.o

#flag @VEXEROOT/thirdparty/mbedtls/library/threading.o

#flag @VEXEROOT/thirdparty/mbedtls/library/timing.o

#flag @VEXEROOT/thirdparty/mbedtls/library/version.o

#flag @VEXEROOT/thirdparty/mbedtls/library/version_features.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509_create.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509_crl.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509_crt.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509_csr.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509write.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509write_crt.o

#flag @VEXEROOT/thirdparty/mbedtls/library/x509write_csr.o

#flag @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/library/Hacl_Curve25519_joined.o

#flag @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/library/everest.o

#flag @VEXEROOT/thirdparty/mbedtls/3rdparty/everest/library/x25519.o

#include <mbedtls/ecdsa.h>

#include <mbedtls/ecdh.h>

#include <mbedtls/ecp.h>

#include <mbedtls/bignum.h>

#include <mbedtls/ctr_drbg.h>

#include <mbedtls/entropy.h>

#include <mbedtls/error.h>

#include <mbedtls/threading.h>

#insert "@VEXEROOT/vlib/net/mbedtls/mbedtls_threading.h"

// v_mbedtls_threading_setup installs the mutex callbacks mbedtls needs when
// built with MBEDTLS_THREADING_ALT (Windows) -- a no-op elsewhere. Safe to
// call even if net.mbedtls's own init() already called it (idempotent per
// its own doc comment); this file cannot assume that happened, since a
// program may import crypto.ecdsa without ever importing net.mbedtls.
fn C.v_mbedtls_threading_setup()

fn init() {
	C.v_mbedtls_threading_setup()
}

// Curve/group identifiers (mbedtls_ecp_group_id, ecp.h).
pub const C.MBEDTLS_ECP_DP_SECP256R1 int
pub const C.MBEDTLS_ECP_DP_SECP384R1 int
pub const C.MBEDTLS_ECP_DP_SECP521R1 int
pub const C.MBEDTLS_ECP_DP_SECP256K1 int

// Point format (mbedtls_ecp_point_write_binary's `format` param, ecp.h).
pub const C.MBEDTLS_ECP_PF_UNCOMPRESSED int

// Message-digest type identifiers (mbedtls_md_type_t, md.h) -- only the
// three this module's own curve->hash mapping needs.
pub const C.MBEDTLS_MD_SHA256 int
pub const C.MBEDTLS_MD_SHA384 int
pub const C.MBEDTLS_MD_SHA512 int

// Upper bounds for stack/heap buffer sizing (ecp.h/ecdsa.h): the largest
// possible uncompressed SEC1 point (1 tag byte + 2*66 for P-521) and the
// largest possible DER Ecdsa-Sig-Value for any curve this module supports.
pub const C.MBEDTLS_ECP_MAX_PT_LEN int
pub const C.MBEDTLS_ECDSA_MAX_LEN int

@[typedef]
struct C.mbedtls_ecp_group {}

@[typedef]
struct C.mbedtls_ecp_point {}

// mbedtls_ecdsa_context is mbedTLS's own typedef of mbedtls_ecp_keypair
// (ecdsa.h) -- one opaque type covers both PrivateKey (grp+d+Q all set) and
// PublicKey (grp+Q set, d left zero) on the V side, mirroring how
// crypto.ecdsa's OpenSSL backend reuses one &C.EVP_PKEY for both.
@[typedef]
struct C.mbedtls_ecdsa_context {}

@[typedef]
struct C.mbedtls_mpi {}

@[typedef]
struct C.mbedtls_ctr_drbg_context {}

@[typedef]
struct C.mbedtls_entropy_context {}

fn C.mbedtls_ecdsa_init(ctx &C.mbedtls_ecdsa_context)

fn C.mbedtls_ecdsa_free(ctx &C.mbedtls_ecdsa_context)

fn C.mbedtls_ecdsa_genkey(ctx &C.mbedtls_ecdsa_context, gid int, f_rng fn (voidptr, &u8, usize) int, p_rng voidptr) int

// mbedtls_ecdsa_write_signature/read_signature operate on a whole context
// (not bare grp/d/Q like the lower-level mbedtls_ecdsa_sign/verify) and
// produce/consume the same ASN.1 DER Ecdsa-Sig-Value OpenSSL's
// EVP_PKEY_sign/verify already does -- DER compatibility between the two
// backends' signatures is automatic, not something this module has to
// arrange. write_signature uses RFC 6979 deterministic nonces automatically
// when MBEDTLS_ECDSA_DETERMINISTIC is compiled in (it is, in this vendored
// build) -- f_rng is then used only for blinding, per its own doc comment.
// `md_alg` is NOT a free-form label in this configuration: the
// deterministic-nonce path (mbedtls_ecdsa_sign_det_restartable, ecdsa.c)
// calls mbedtls_md_info_from_type(md_alg) to seed its internal HMAC-DRBG
// and hard-rejects with MBEDTLS_ERR_ECP_BAD_INPUT_DATA when that lookup
// fails -- which it always does for MBEDTLS_MD_NONE (0), since md.c's own
// switch has no case for it. PrivateKey.sign() (ecdsa_notd_use_openssl.v)
// relies on this: it never calls write_signature with md_alg 0.
fn C.mbedtls_ecdsa_write_signature(ctx &C.mbedtls_ecdsa_context, md_alg int, hash &u8, hlen usize, sig &u8, sig_size usize, slen &usize, f_rng fn (voidptr, &u8, usize) int, p_rng voidptr) int

fn C.mbedtls_ecdsa_read_signature(ctx &C.mbedtls_ecdsa_context, hash &u8, hlen usize, sig &u8, slen usize) int

fn C.mbedtls_ecp_group_init(grp &C.mbedtls_ecp_group)

fn C.mbedtls_ecp_group_free(grp &C.mbedtls_ecp_group)

fn C.mbedtls_ecp_group_load(grp &C.mbedtls_ecp_group, id int) int

fn C.mbedtls_ecp_point_init(pt &C.mbedtls_ecp_point)

fn C.mbedtls_ecp_point_free(pt &C.mbedtls_ecp_point)

fn C.mbedtls_ecp_point_write_binary(grp &C.mbedtls_ecp_group, pt &C.mbedtls_ecp_point, format int, olen &usize, buf &u8, buflen usize) int

fn C.mbedtls_ecp_point_read_binary(grp &C.mbedtls_ecp_group, pt &C.mbedtls_ecp_point, buf &u8, ilen usize) int

fn C.mbedtls_ecp_point_cmp(p &C.mbedtls_ecp_point, q &C.mbedtls_ecp_point) int

fn C.mbedtls_ecp_check_pubkey(grp &C.mbedtls_ecp_group, pt &C.mbedtls_ecp_point) int

// mbedtls_ecp_export reads out a keypair's group/private-scalar/public-point
// in one call -- the load-bearing primitive this whole backend relies on to
// avoid ANY C shim: mbedtls_ecp_point/mbedtls_ecp_keypair/mbedtls_mpi are
// all fully MBEDTLS_PRIVATE-wrapped internally (unreachable by hand-
// replicating a struct layout from V, the way net.mbedtls's
// mbedtls_pk_rsassa_pss_options deliberately does for a non-private-field
// struct), but this one exported function does the extraction in real C
// where the true layout is known. Any of grp/d/Q may be passed nil to skip
// that output.
fn C.mbedtls_ecp_export(key &C.mbedtls_ecdsa_context, grp &C.mbedtls_ecp_group, d &C.mbedtls_mpi, q &C.mbedtls_ecp_point) int

fn C.mbedtls_ecp_set_public_key(grp_id int, key &C.mbedtls_ecdsa_context, q &C.mbedtls_ecp_point) int

fn C.mbedtls_mpi_init(x &C.mbedtls_mpi)

fn C.mbedtls_mpi_free(x &C.mbedtls_mpi)

fn C.mbedtls_mpi_write_binary(x &C.mbedtls_mpi, buf &u8, buflen usize) int

fn C.mbedtls_mpi_cmp_mpi(x &C.mbedtls_mpi, y &C.mbedtls_mpi) int

// mbedtls_ecdh_compute_shared derives a raw ECDH shared secret (the X-
// coordinate only, matching derive_shared_secret's documented SEC1 output)
// directly from bare grp/d/Q -- the lower-level API, not the stateful
// mbedtls_ecdh_context one, since this module never needs to hold ECDH
// state across calls the way a TLS handshake would.
fn C.mbedtls_ecdh_compute_shared(grp &C.mbedtls_ecp_group, z &C.mbedtls_mpi, q &C.mbedtls_ecp_point, d &C.mbedtls_mpi, f_rng fn (voidptr, &u8, usize) int, p_rng voidptr) int

fn C.mbedtls_ctr_drbg_init(ctx &C.mbedtls_ctr_drbg_context)

fn C.mbedtls_ctr_drbg_free(ctx &C.mbedtls_ctr_drbg_context)

fn C.mbedtls_ctr_drbg_seed(ctx &C.mbedtls_ctr_drbg_context, f_entropy fn (voidptr, &u8, usize), p_entropy voidptr, custom &u8, len usize) int

fn C.mbedtls_ctr_drbg_random(p_rng voidptr, output &u8, output_len usize) int

fn C.mbedtls_entropy_init(ctx &C.mbedtls_entropy_context)

fn C.mbedtls_entropy_free(ctx &C.mbedtls_entropy_context)

fn C.mbedtls_entropy_func(data voidptr, output &u8, len usize)
