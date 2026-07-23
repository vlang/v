// Copyright (c) 2019-2026 the V contributors. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rsa_pss

// RSA-PSS sign/verify, added for net.quic's TLS 1.3 CertificateVerify support
// (RFC 8446 §4.2.3's `rsa_pss_rsae_sha256/384/512` mandatory-to-implement
// signature schemes) — see https://github.com/vlang/v/issues/27675. No RSA of
// any kind previously existed anywhere in V's stdlib.
//
// Follows the same OpenSSL EVP high-level API binding pattern already used by
// vlib/crypto/ecdsa, reusing the same `-lcrypto` linkage and per-platform
// `#flag` search paths.

#flag darwin -L/opt/homebrew/opt/openssl/lib
#flag darwin -I/opt/homebrew/opt/openssl/include
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib

// -I points at the PARENT of the openssl/ header dir (matching the darwin
// flags above), not at .../openssl itself -- #include <openssl/evp.h> below
// already spells out the openssl/ prefix, so an -I ending in .../openssl
// would make the compiler look for .../openssl/openssl/evp.h. This was
// silently masked on every OTHER compiler by /usr/include being implicitly
// on the default search path regardless of this flag being wrong --
// musl-gcc's -nostdinc removes that implicit fallback, and was the first to
// surface it (confirmed via an actual CI failure: "Header file
// <openssl/evp.h> ... was not found" on docker-ubuntu-musl, even with
// libssl-dev/openssl/evp.h genuinely present on disk). Same bug as
// vlib/crypto/ecdsa/ecdsa.c.v, which this module's #flag setup was copied
// from.
#flag linux -I/usr/local/include
#flag linux -L/usr/local/lib64/

#flag openbsd -I/usr/local/include/eopenssl35
#flag openbsd -L/usr/local/lib/eopenssl35 -Wl,-rpath,/usr/local/lib/eopenssl35

// Installed through choco:
#flag windows -IC:/Program Files/OpenSSL-Win64/include
#flag windows -LC:/Program Files/OpenSSL-Win64/lib/VC/x64/MD

// Installed on the CI:
#flag windows -IC:/Program Files/OpenSSL/include
#flag windows -LC:/Program Files/OpenSSL/lib/VC/x64/MD

#flag -I/usr/include

#flag -lcrypto

#include <openssl/evp.h>
#include <openssl/rsa.h>

pub const C.EVP_PKEY_RSA int
pub const C.RSA_PKCS1_PSS_PADDING int // RSA_PSS_SALTLEN_DIGEST (-1): salt length equals the digest length — the
// deterministic choice RFC 8446 §4.2.3 requires for the `rsae` schemes
// (as opposed to OpenSSL's own default, "maximum possible salt length").

pub const C.RSA_PSS_SALTLEN_DIGEST int

@[typedef]
struct C.EVP_PKEY {}

fn C.EVP_PKEY_new() &C.EVP_PKEY
fn C.EVP_PKEY_free(key &C.EVP_PKEY)

@[typedef]
struct C.EVP_PKEY_CTX {}

fn C.EVP_PKEY_CTX_new_id(id i32, e voidptr) &C.EVP_PKEY_CTX
fn C.EVP_PKEY_CTX_new(pkey &C.EVP_PKEY, e voidptr) &C.EVP_PKEY_CTX
fn C.EVP_PKEY_CTX_free(ctx &C.EVP_PKEY_CTX)
fn C.EVP_PKEY_keygen_init(ctx &C.EVP_PKEY_CTX) i32
fn C.EVP_PKEY_keygen(ctx &C.EVP_PKEY_CTX, ppkey &&C.EVP_PKEY) i32
fn C.EVP_PKEY_CTX_set_rsa_keygen_bits(ctx &C.EVP_PKEY_CTX, bits i32) i32
fn C.EVP_PKEY_CTX_set_rsa_padding(ctx &C.EVP_PKEY_CTX, pad i32) i32
fn C.EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx &C.EVP_PKEY_CTX, saltlen i32) i32

@[typedef]
struct C.EVP_MD_CTX {}

fn C.EVP_MD_CTX_new() &C.EVP_MD_CTX
fn C.EVP_MD_CTX_free(ctx &C.EVP_MD_CTX)

@[typedef]
struct C.EVP_MD {}

fn C.EVP_sha256() &C.EVP_MD
fn C.EVP_sha384() &C.EVP_MD
fn C.EVP_sha512() &C.EVP_MD

fn C.EVP_DigestSignInit(ctx &C.EVP_MD_CTX, pctx &&C.EVP_PKEY_CTX, tipe &C.EVP_MD, e voidptr, pkey &C.EVP_PKEY) i32
fn C.EVP_DigestSignUpdate(ctx &C.EVP_MD_CTX, d voidptr, cnt i32) i32
fn C.EVP_DigestSignFinal(ctx &C.EVP_MD_CTX, sig &u8, siglen &usize) i32

fn C.EVP_DigestVerifyInit(ctx &C.EVP_MD_CTX, pctx &&C.EVP_PKEY_CTX, tipe &C.EVP_MD, e voidptr, pkey &C.EVP_PKEY) i32
fn C.EVP_DigestVerifyUpdate(ctx &C.EVP_MD_CTX, d voidptr, cnt i32) i32
fn C.EVP_DigestVerifyFinal(ctx &C.EVP_MD_CTX, sig &u8, siglen i32) i32
