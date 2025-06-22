// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

// See https://docs.openssl.org/master/man7/openssl_user_macros/#description
// should be 0x30000000L, but a lot of EC_KEY method was deprecated on version 3.0
// #define OPENSSL_API_COMPAT 0x10100000L

#flag darwin -L/opt/homebrew/opt/openssl/lib
#flag darwin -I/opt/homebrew/opt/openssl/include
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib

#flag linux -I/usr/local/include/openssl
#flag linux -L/usr/local/lib64/

#flag openbsd -I/usr/local/include/eopenssl34
#flag openbsd -L/usr/local/lib/eopenssl34 -Wl,-rpath,/usr/local/lib/eopenssl34

// Installed through choco:
#flag windows -IC:/Program Files/OpenSSL-Win64/include
#flag windows -LC:/Program Files/OpenSSL-Win64/lib/VC/x64/MD

// Installed on the CI:
#flag windows -IC:/Program Files/OpenSSL/include
#flag windows -LC:/Program Files/OpenSSL/lib/VC/x64/MD

#flag -I/usr/include/openssl

#flag -lcrypto

#include <openssl/ecdsa.h>
#include <openssl/obj_mac.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/x509.h>
#include <openssl/bio.h>
#include <openssl/pem.h>
#include <openssl/param_build.h>

// The following header is available on OpenSSL 3.0, but not in OpenSSL 1.1.1f
//#include <openssl/core.h>

// The new opaque of public key pair high level API
@[typedef]
struct C.EVP_PKEY {}

fn C.EVP_PKEY_new() &C.EVP_PKEY
fn C.EVP_PKEY_free(key &C.EVP_PKEY)
fn C.EVP_PKEY_base_id(key &C.EVP_PKEY) int
fn C.EVP_PKEY_bits(pkey &C.EVP_PKEY) int
fn C.EVP_PKEY_size(key &C.EVP_PKEY) int
fn C.EVP_PKEY_eq(a &C.EVP_PKEY, b &C.EVP_PKEY) int
fn C.EVP_PKEY_check(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_public_check(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_dup(key &C.EVP_PKEY) &C.EVP_PKEY
fn C.EVP_PKEY_set_bn_param(pkey &C.EVP_PKEY, key_name &char, bn &C.BIGNUM) int

fn C.EVP_PKEY_get_group_name(pkey &C.EVP_PKEY, gname &u8, gname_sz u32, gname_len &usize) int
fn C.EVP_PKEY_get1_encoded_public_key(pkey &C.EVP_PKEY, ppub &&u8) int
fn C.EVP_PKEY_get_bn_param(pkey &C.EVP_PKEY, key_name &u8, bn &&C.BIGNUM) int
fn C.EVP_PKEY_fromdata_init(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_fromdata(ctx &C.EVP_PKEY_CTX, ppkey &&C.EVP_PKEY, selection int, params &C.OSSL_PARAM) int

// no-prehash signing (verifying)
fn C.EVP_PKEY_sign(ctx &C.EVP_PKEY_CTX, sig &u8, siglen &usize, tbs &u8, tbslen int) int
fn C.EVP_PKEY_sign_init(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_verify_init(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_verify(ctx &C.EVP_PKEY_CTX, sig &u8, siglen int, tbs &u8, tbslen int) int

// single shoot digest signing (verifying) routine
fn C.EVP_DigestSign(ctx &C.EVP_MD_CTX, sig &u8, siglen &usize, tbs &u8, tbslen int) int
fn C.EVP_DigestVerify(ctx &C.EVP_MD_CTX, sig &u8, siglen int, tbs &u8, tbslen int) int

// Message digest routines
fn C.EVP_DigestInit(ctx &C.EVP_MD_CTX, md &C.EVP_MD) int
fn C.EVP_DigestUpdate(ctx &C.EVP_MD_CTX, d voidptr, cnt int) int
fn C.EVP_DigestFinal(ctx &C.EVP_MD_CTX, md &u8, s &u32) int

// Recommended hashed signing/verifying routines
fn C.EVP_DigestSignInit(ctx &C.EVP_MD_CTX, pctx &&C.EVP_PKEY_CTX, tipe &C.EVP_MD, e voidptr, pkey &C.EVP_PKEY) int
fn C.EVP_DigestSignUpdate(ctx &C.EVP_MD_CTX, d voidptr, cnt int) int
fn C.EVP_DigestSignFinal(ctx &C.EVP_MD_CTX, sig &u8, siglen &usize) int
fn C.EVP_DigestVerifyInit(ctx &C.EVP_MD_CTX, pctx &&C.EVP_PKEY_CTX, tipe &C.EVP_MD, e voidptr, pkey &C.EVP_PKEY) int
fn C.EVP_DigestVerifyUpdate(ctx &C.EVP_MD_CTX, d voidptr, cnt int) int
fn C.EVP_DigestVerifyFinal(ctx &C.EVP_MD_CTX, sig &u8, siglen int) int

// EVP_PKEY Context
@[typedef]
struct C.EVP_PKEY_CTX {}

fn C.EVP_PKEY_CTX_new(pkey &C.EVP_PKEY, e voidptr) &C.EVP_PKEY_CTX
fn C.EVP_PKEY_CTX_new_id(id int, e voidptr) &C.EVP_PKEY_CTX
fn C.EVP_PKEY_keygen_init(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_keygen(ctx &C.EVP_PKEY_CTX, ppkey &&C.EVP_PKEY) int
fn C.EVP_PKEY_CTX_set_ec_paramgen_curve_nid(ctx &C.EVP_PKEY_CTX, nid int) int
fn C.EVP_PKEY_CTX_set_ec_param_enc(ctx &C.EVP_PKEY_CTX, param_enc int) int
fn C.EVP_PKEY_CTX_free(ctx &C.EVP_PKEY_CTX)

fn C.EVP_PKEY_get_bits(pkey &C.EVP_PKEY) int

// BIO input output declarations.
@[typedef]
struct C.BIO_METHOD {}

@[typedef]
pub struct C.BIO {}

fn C.BIO_new(t &C.BIO_METHOD) &C.BIO
fn C.BIO_free_all(a &C.BIO)
fn C.BIO_s_mem() &C.BIO_METHOD
fn C.BIO_write(b &C.BIO, buf &u8, length int) int
fn C.PEM_read_bio_PrivateKey(bp &C.BIO, x &&C.EVP_PKEY, cb int, u &voidptr) &C.EVP_PKEY
fn C.PEM_read_bio_PUBKEY(bp &C.BIO, x &&C.EVP_PKEY, cb int, u &voidptr) &C.EVP_PKEY
fn C.PEM_write_bio_PUBKEY(bp &C.BIO, x &C.EVP_PKEY) int
fn C.d2i_PUBKEY(k &&C.EVP_PKEY, pp &&u8, length u32) &C.EVP_PKEY
fn C.i2d_PUBKEY_bio(bo &C.BIO, pkey &C.EVP_PKEY) int
fn C.d2i_PUBKEY_bio(bo &C.BIO, key &&C.EVP_PKEY) &C.EVP_PKEY

// Elliptic curve point related declarations.
@[typedef]
struct C.EC_POINT {}

fn C.EC_POINT_new(group &C.EC_GROUP) &C.EC_POINT
fn C.EC_POINT_mul(group &C.EC_GROUP, r &C.EC_POINT, n &C.BIGNUM, q &C.EC_POINT, m &C.BIGNUM, ctx &C.BN_CTX) int
fn C.EC_POINT_point2buf(group &C.EC_GROUP, point &C.EC_POINT, form int, pbuf &&u8, ctx &C.BN_CTX) int
fn C.EC_POINT_free(point &C.EC_POINT)

// Elliptic group (curve) related declarations.
@[typedef]
struct C.EC_GROUP {}

fn C.EC_GROUP_free(group &C.EC_GROUP)
fn C.EC_GROUP_new_by_curve_name(nid int) &C.EC_GROUP

// Elliptic BIGNUM related declarations.
@[typedef]
struct C.BIGNUM {}

fn C.BN_new() &C.BIGNUM
fn C.BN_num_bits(a &C.BIGNUM) int
fn C.BN_bn2bin(a &C.BIGNUM, to &u8) int
fn C.BN_bn2binpad(a &C.BIGNUM, to &u8, tolen int) int
fn C.BN_cmp(a &C.BIGNUM, b &C.BIGNUM) int
fn C.BN_bin2bn(s &u8, len int, ret &C.BIGNUM) &C.BIGNUM
fn C.BN_free(a &C.BIGNUM)

// BIGNUM context
@[typedef]
struct C.BN_CTX {}

fn C.BN_CTX_new() &C.BN_CTX
fn C.BN_CTX_free(ctx &C.BN_CTX)

@[typedef]
struct C.EVP_MD_CTX {}

fn C.EVP_MD_CTX_new() &C.EVP_MD_CTX
fn C.EVP_MD_CTX_free(ctx &C.EVP_MD_CTX)

// Wrapper of digest and signing related of the C opaque and functions.
@[typedef]
struct C.EVP_MD {}

fn C.EVP_sha256() &C.EVP_MD
fn C.EVP_sha384() &C.EVP_MD
fn C.EVP_sha512() &C.EVP_MD
fn C.EVP_MD_get_size(md &C.EVP_MD) int // -1 failure

fn C.OPENSSL_free(addr voidptr)

@[typedef]
struct C.OSSL_PARAM {}

@[typedef]
struct C.OSSL_PARAM_BLD {}

fn C.OSSL_PARAM_free(params &C.OSSL_PARAM)
fn C.OSSL_PARAM_BLD_free(param_bld &C.OSSL_PARAM_BLD)
fn C.OSSL_PARAM_BLD_new() &C.OSSL_PARAM_BLD
fn C.OSSL_PARAM_BLD_push_utf8_string(bld &C.OSSL_PARAM_BLD, key &char, buf &char, bsize int) int
fn C.OSSL_PARAM_BLD_push_BN(bld &C.OSSL_PARAM_BLD, key &u8, bn &C.BIGNUM) int
fn C.OSSL_PARAM_BLD_push_octet_string(bld &C.OSSL_PARAM_BLD, key &u8, buf voidptr, bsize int) int
fn C.OSSL_PARAM_BLD_to_param(bld &C.OSSL_PARAM_BLD) &C.OSSL_PARAM
