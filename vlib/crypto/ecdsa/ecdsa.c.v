// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ecdsa

// See https://docs.openssl.org/master/man7/openssl_user_macros/#description
// should be 0x30000000L, but a lot of EC_KEY method was deprecated on version 3.0
// #define OPENSSL_API_COMPAT 0x10100000L

#flag darwin -L /opt/homebrew/opt/openssl/lib -I /opt/homebrew/opt/openssl/include

#flag -I/usr/include/openssl
#flag -lcrypto
#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib
#include <openssl/ecdsa.h>
#include <openssl/obj_mac.h>
#include <openssl/objects.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/x509.h>
#include <openssl/bio.h>
#include <openssl/pem.h>

// The new opaque of public key pair high level API
@[typedef]
struct C.EVP_PKEY {}

fn C.EVP_PKEY_new() &C.EVP_PKEY
fn C.EVP_PKEY_free(key &C.EVP_PKEY)
fn C.EVP_PKEY_get1_EC_KEY(pkey &C.EVP_PKEY) &C.EC_KEY
fn C.EVP_PKEY_base_id(key &C.EVP_PKEY) int

// EVP_PKEY Context
@[typedef]
struct C.EVP_PKEY_CTX {}

fn C.EVP_PKEY_CTX_new_id(id int, e voidptr) &C.EVP_PKEY_CTX
fn C.EVP_PKEY_keygen_init(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_keygen(ctx &C.EVP_PKEY_CTX, ppkey &&C.EVP_PKEY) int
fn C.EVP_PKEY_CTX_set_ec_paramgen_curve_nid(ctx &C.EVP_PKEY_CTX, nid int) int
fn C.EVP_PKEY_CTX_set_ec_param_enc(ctx &C.EVP_PKEY_CTX, param_enc int) int
fn C.EVP_PKEY_CTX_free(ctx &C.EVP_PKEY_CTX)

// Elliptic curve keypair declarations
@[typedef]
struct C.EC_KEY {}

fn C.EC_KEY_new_by_curve_name(nid int) &C.EC_KEY
fn C.EC_KEY_generate_key(key &C.EC_KEY) int
fn C.EC_KEY_dup(src &C.EC_KEY) &C.EC_KEY
fn C.EC_KEY_free(key &C.EC_KEY)
fn C.EC_KEY_set_public_key(key &C.EC_KEY, &C.EC_POINT) int
fn C.EC_KEY_set_private_key(key &C.EC_KEY, prv &C.BIGNUM) int
fn C.EC_KEY_get0_group(key &C.EC_KEY) &C.EC_GROUP
fn C.EC_KEY_get0_private_key(key &C.EC_KEY) &C.BIGNUM
fn C.EC_KEY_get0_public_key(key &C.EC_KEY) &C.EC_POINT
fn C.EC_KEY_get_conv_form(k &C.EC_KEY) int
fn C.EC_KEY_check_key(key &C.EC_KEY) int
fn C.EC_KEY_up_ref(key &C.EC_KEY) int

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
fn C.d2i_PUBKEY(k &&C.EVP_PKEY, pp &&u8, length u32) &C.EVP_PKEY

// Elliptic curve point related declarations.
@[typedef]
struct C.EC_POINT {}

fn C.EC_POINT_new(group &C.EC_GROUP) &C.EC_POINT
fn C.EC_POINT_mul(group &C.EC_GROUP, r &C.EC_POINT, n &C.BIGNUM, q &C.EC_POINT, m &C.BIGNUM, ctx &C.BN_CTX) int
fn C.EC_POINT_point2oct(g &C.EC_GROUP, p &C.EC_POINT, form int, buf &u8, max_out int, ctx &C.BN_CTX) int
fn C.EC_POINT_cmp(group &C.EC_GROUP, a &C.EC_POINT, b &C.EC_POINT, ctx &C.BN_CTX) int
fn C.EC_POINT_free(point &C.EC_POINT)

// Elliptic group (curve) related declarations.
@[typedef]
struct C.EC_GROUP {}

fn C.EC_GROUP_free(group &C.EC_GROUP)
fn C.EC_GROUP_get_degree(g &C.EC_GROUP) int
fn C.EC_GROUP_get_curve_name(g &C.EC_GROUP) int
fn C.EC_GROUP_cmp(a &C.EC_GROUP, b &C.EC_GROUP, ctx &C.BN_CTX) int

// Elliptic BIGNUM related declarations.
@[typedef]
struct C.BIGNUM {}

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

// ELliptic ECDSA signing and verifying related declarations.
@[typedef]
struct C.ECDSA_SIG {}

fn C.ECDSA_size(key &C.EC_KEY) u32
fn C.ECDSA_sign(type_ int, dgst &u8, dgstlen int, sig &u8, siglen &u32, eckey &C.EC_KEY) int
fn C.ECDSA_verify(type_ int, dgst &u8, dgstlen int, sig &u8, siglen int, eckey &C.EC_KEY) int
