// Copyright (c) blackshirt. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module slhdsa

// TODO: remove this when the 3.5 version was commonly availables on the stock.
#flag linux -I/opt/ssl/include/openssl
#flag linux -I/opt/ssl/include/crypto
#flag linux -I/opt/ssl/include
#flag linux -L/opt/ssl/lib64

// Standard path
#flag linux -I/usr/local/include/openssl
#flag linux -I/usr/local/include/crypto
#flag linux -L/usr/local/lib64

#flag darwin -L /opt/homebrew/opt/openssl/lib -I /opt/homebrew/opt/openssl/include

#flag -I/usr/include/openssl
#flag -lcrypto

#flag darwin -I/usr/local/opt/openssl/include
#flag darwin -L/usr/local/opt/openssl/lib

#include <openssl/obj_mac.h>
#include <openssl/evp.h>
#include <openssl/bio.h>
#include <openssl/param_build.h>

@[typedef]
struct C.EVP_PKEY {}

fn C.EVP_PKEY_new() &C.EVP_PKEY
fn C.EVP_PKEY_Q_keygen(ctx voidptr, propq &char, tipe &char) &C.EVP_PKEY
fn C.EVP_PKEY_is_a(pkey &C.EVP_PKEY, name &char) int
fn C.EVP_PKEY_size(key &C.EVP_PKEY) int
fn C.EVP_PKEY_dup(key &C.EVP_PKEY) &C.EVP_PKEY
fn C.EVP_PKEY_free(key &C.EVP_PKEY)
fn C.EVP_PKEY_get_octet_string_param(pkey &C.EVP_PKEY, key_name &char, buf &u8, max_buf_sz int, out_len &usize) int
fn C.EVP_PKEY_print_private(out &C.BIO, pkey &C.EVP_PKEY, indent int, pctx voidptr) int
fn C.EVP_PKEY_print_public(out &C.BIO, pkey &C.EVP_PKEY, indent int, pctx voidptr) int
fn C.EVP_PKEY_keygen_init(ctx &C.EVP_PKEY_CTX) int // 1 success
fn C.EVP_PKEY_keygen(ctx &C.EVP_PKEY_CTX, ppkey &&C.EVP_PKEY) int
fn C.EVP_PKEY_CTX_set_params(ctx &C.EVP_PKEY_CTX, params &C.OSSL_PARAM) int
fn C.EVP_PKEY_get0_type_name(key &C.EVP_PKEY) &char
fn C.EVP_PKEY_get0_description(key &C.EVP_PKEY) &char
fn C.EVP_PKEY_get_bits(pkey &C.EVP_PKEY) int
fn C.EVP_PKEY_get_security_bits(pkey &C.EVP_PKEY) int
fn C.EVP_PKEY_sign_init_ex(ctx &C.EVP_PKEY_CTX, params &C.OSSL_PARAM) int
fn C.EVP_PKEY_sign_message_init(ctx &C.EVP_PKEY_CTX, algo &C.EVP_SIGNATURE, params &C.OSSL_PARAM) int
fn C.EVP_PKEY_sign(ctx &C.EVP_PKEY_CTX, sig &char, siglen &usize, tbs &char, tbslen int) int
fn C.EVP_PKEY_verify_message_init(ctx &C.EVP_PKEY_CTX, algo &C.EVP_SIGNATURE, params &C.OSSL_PARAM) int
fn C.EVP_PKEY_verify(ctx &C.EVP_PKEY_CTX, sig &char, siglen &int, tbs &char, tbslen int) int
fn C.EVP_PKEY_set_octet_string_param(key &C.EVP_PKEY, key_name &u8, buf &u8, bsize int) int
fn C.EVP_PKEY_fromdata_init(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_fromdata(ctx &C.EVP_PKEY_CTX, ppkey &&C.EVP_PKEY, selection int, params &C.OSSL_PARAM) int
fn C.EVP_PKEY_check(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_private_check(ctx &C.EVP_PKEY_CTX) int
fn C.EVP_PKEY_public_check(ctx &C.EVP_PKEY_CTX) int

@[typedef]
struct C.EVP_PKEY_CTX {}

fn C.EVP_PKEY_CTX_free(ctx &C.EVP_PKEY_CTX)
fn C.EVP_PKEY_CTX_new_from_name(libctx voidptr, name &char, pq voidptr) &C.EVP_PKEY_CTX
fn C.EVP_PKEY_CTX_new_from_pkey(libctx voidptr, pkey &C.EVP_PKEY, pq voidptr) &C.EVP_PKEY_CTX

fn C.OpenSSL_version_num() u64

@[typedef]
struct C.EVP_SIGNATURE {}

fn C.EVP_SIGNATURE_free(signature &C.EVP_SIGNATURE)
fn C.EVP_SIGNATURE_fetch(ctx voidptr, algorithm &char, properties voidptr) &C.EVP_SIGNATURE
fn C.EVP_SIGNATURE_get0_name(signature &C.EVP_SIGNATURE) &char

@[typedef]
struct C.OSSL_PARAM {}

@[typedef]
struct C.OSSL_PARAM_BLD {}

fn C.OSSL_PARAM_free(params &C.OSSL_PARAM)
fn C.OSSL_PARAM_BLD_free(param_bld &C.OSSL_PARAM_BLD)
fn C.OSSL_PARAM_BLD_new() &C.OSSL_PARAM_BLD
fn C.OSSL_PARAM_BLD_push_int(bld &C.OSSL_PARAM_BLD, key &u8, val int) int
fn C.OSSL_PARAM_BLD_push_octet_string(bld &C.OSSL_PARAM_BLD, key &u8, buf voidptr, bsize int) int
fn C.OSSL_PARAM_BLD_to_param(bld &C.OSSL_PARAM_BLD) &C.OSSL_PARAM

@[typedef]
struct C.BIO_METHOD {}

@[typedef]
struct C.BIO {}

fn C.BIO_new(t &C.BIO_METHOD) &C.BIO
fn C.BIO_read_ex(b &C.BIO, data voidptr, dlen int, readbytes &usize) int
fn C.BIO_free_all(a &C.BIO)
fn C.BIO_s_mem() &C.BIO_METHOD
