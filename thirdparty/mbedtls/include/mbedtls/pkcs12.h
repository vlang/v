/**
 * \file pkcs12.h
 *
 * \brief PKCS#12 Personal Information Exchange Syntax
 */
/*
 *  Copyright The Mbed TLS Contributors
 *  SPDX-License-Identifier: Apache-2.0
 *
 *  Licensed under the Apache License, Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
#ifndef MBEDTLS_PKCS12_H
#define MBEDTLS_PKCS12_H

#include "mbedtls/build_info.h"

#include "mbedtls/md.h"
#include "mbedtls/cipher.h"
#include "mbedtls/asn1.h"

#include <stddef.h>

/** Bad input parameters to function. */
#define MBEDTLS_ERR_PKCS12_BAD_INPUT_DATA                 -0x1F80
/** Feature not available, e.g. unsupported encryption scheme. */
#define MBEDTLS_ERR_PKCS12_FEATURE_UNAVAILABLE            -0x1F00
/** PBE ASN.1 data not as expected. */
#define MBEDTLS_ERR_PKCS12_PBE_INVALID_FORMAT             -0x1E80
/** Given private key password does not allow for correct decryption. */
#define MBEDTLS_ERR_PKCS12_PASSWORD_MISMATCH              -0x1E00

#define MBEDTLS_PKCS12_DERIVE_KEY       1   /**< encryption/decryption key */
#define MBEDTLS_PKCS12_DERIVE_IV        2   /**< initialization vector     */
#define MBEDTLS_PKCS12_DERIVE_MAC_KEY   3   /**< integrity / MAC key       */

#define MBEDTLS_PKCS12_PBE_DECRYPT      0
#define MBEDTLS_PKCS12_PBE_ENCRYPT      1

#ifdef __cplusplus
extern "C" {
#endif

#if defined(MBEDTLS_ASN1_PARSE_C)

/**
 * \brief            PKCS12 Password Based function (encryption / decryption)
 *                   for cipher-based and mbedtls_md-based PBE's
 *
 * \param pbe_params an ASN1 buffer containing the pkcs-12 PbeParams structure
 * \param mode       either #MBEDTLS_PKCS12_PBE_ENCRYPT or
 *                   #MBEDTLS_PKCS12_PBE_DECRYPT
 * \param cipher_type the cipher used
 * \param md_type    the mbedtls_md used
 * \param pwd        Latin1-encoded password used. This may only be \c NULL when
 *                   \p pwdlen is 0. No null terminator should be used.
 * \param pwdlen     length of the password (may be 0)
 * \param input      the input data
 * \param len        data length
 * \param output     the output buffer
 *
 * \return           0 if successful, or a MBEDTLS_ERR_XXX code
 */
int mbedtls_pkcs12_pbe( mbedtls_asn1_buf *pbe_params, int mode,
                mbedtls_cipher_type_t cipher_type, mbedtls_md_type_t md_type,
                const unsigned char *pwd,  size_t pwdlen,
                const unsigned char *input, size_t len,
                unsigned char *output );

#endif /* MBEDTLS_ASN1_PARSE_C */

/**
 * \brief            The PKCS#12 derivation function uses a password and a salt
 *                   to produce pseudo-random bits for a particular "purpose".
 *
 *                   Depending on the given id, this function can produce an
 *                   encryption/decryption key, an initialization vector or an
 *                   integrity key.
 *
 * \param data       buffer to store the derived data in
 * \param datalen    length of buffer to fill
 * \param pwd        The password to use. For compliance with PKCS#12 §B.1, this
 *                   should be a BMPString, i.e. a Unicode string where each
 *                   character is encoded as 2 bytes in big-endian order, with
 *                   no byte order mark and with a null terminator (i.e. the
 *                   last two bytes should be 0x00 0x00).
 * \param pwdlen     length of the password (may be 0).
 * \param salt       Salt buffer to use. This may only be \c NULL when
 *                   \p saltlen is 0.
 * \param saltlen    length of the salt (may be zero)
 * \param mbedtls_md mbedtls_md type to use during the derivation
 * \param id         id that describes the purpose (can be
 *                   #MBEDTLS_PKCS12_DERIVE_KEY, #MBEDTLS_PKCS12_DERIVE_IV or
 *                   #MBEDTLS_PKCS12_DERIVE_MAC_KEY)
 * \param iterations number of iterations
 *
 * \return          0 if successful, or a MD, BIGNUM type error.
 */
int mbedtls_pkcs12_derivation( unsigned char *data, size_t datalen,
                       const unsigned char *pwd, size_t pwdlen,
                       const unsigned char *salt, size_t saltlen,
                       mbedtls_md_type_t mbedtls_md, int id, int iterations );

#ifdef __cplusplus
}
#endif

#endif /* pkcs12.h */
