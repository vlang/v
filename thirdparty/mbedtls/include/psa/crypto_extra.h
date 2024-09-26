/**
 * \file psa/crypto_extra.h
 *
 * \brief PSA cryptography module: Mbed TLS vendor extensions
 *
 * \note This file may not be included directly. Applications must
 * include psa/crypto.h.
 *
 * This file is reserved for vendor-specific definitions.
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

#ifndef PSA_CRYPTO_EXTRA_H
#define PSA_CRYPTO_EXTRA_H
#include "mbedtls/private_access.h"

#include "mbedtls/platform_util.h"

#include "crypto_types.h"
#include "crypto_compat.h"

#ifdef __cplusplus
extern "C" {
#endif

/* UID for secure storage seed */
#define PSA_CRYPTO_ITS_RANDOM_SEED_UID 0xFFFFFF52

/* See mbedtls_config.h for definition */
#if !defined(MBEDTLS_PSA_KEY_SLOT_COUNT)
#define MBEDTLS_PSA_KEY_SLOT_COUNT 32
#endif

/** \addtogroup attributes
 * @{
 */

/** \brief Declare the enrollment algorithm for a key.
 *
 * An operation on a key may indifferently use the algorithm set with
 * psa_set_key_algorithm() or with this function.
 *
 * \param[out] attributes       The attribute structure to write to.
 * \param alg2                  A second algorithm that the key may be used
 *                              for, in addition to the algorithm set with
 *                              psa_set_key_algorithm().
 *
 * \warning Setting an enrollment algorithm is not recommended, because
 *          using the same key with different algorithms can allow some
 *          attacks based on arithmetic relations between different
 *          computations made with the same key, or can escalate harmless
 *          side channels into exploitable ones. Use this function only
 *          if it is necessary to support a protocol for which it has been
 *          verified that the usage of the key with multiple algorithms
 *          is safe.
 */
static inline void psa_set_key_enrollment_algorithm(
    psa_key_attributes_t *attributes,
    psa_algorithm_t alg2)
{
    attributes->MBEDTLS_PRIVATE(core).MBEDTLS_PRIVATE(policy).MBEDTLS_PRIVATE(alg2) = alg2;
}

/** Retrieve the enrollment algorithm policy from key attributes.
 *
 * \param[in] attributes        The key attribute structure to query.
 *
 * \return The enrollment algorithm stored in the attribute structure.
 */
static inline psa_algorithm_t psa_get_key_enrollment_algorithm(
    const psa_key_attributes_t *attributes)
{
    return( attributes->MBEDTLS_PRIVATE(core).MBEDTLS_PRIVATE(policy).MBEDTLS_PRIVATE(alg2) );
}

#if defined(MBEDTLS_PSA_CRYPTO_SE_C)

/** Retrieve the slot number where a key is stored.
 *
 * A slot number is only defined for keys that are stored in a secure
 * element.
 *
 * This information is only useful if the secure element is not entirely
 * managed through the PSA Cryptography API. It is up to the secure
 * element driver to decide how PSA slot numbers map to any other interface
 * that the secure element may have.
 *
 * \param[in] attributes        The key attribute structure to query.
 * \param[out] slot_number      On success, the slot number containing the key.
 *
 * \retval #PSA_SUCCESS
 *         The key is located in a secure element, and \p *slot_number
 *         indicates the slot number that contains it.
 * \retval #PSA_ERROR_NOT_PERMITTED
 *         The caller is not permitted to query the slot number.
 *         Mbed Crypto currently does not return this error.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         The key is not located in a secure element.
 */
psa_status_t psa_get_key_slot_number(
    const psa_key_attributes_t *attributes,
    psa_key_slot_number_t *slot_number );

/** Choose the slot number where a key is stored.
 *
 * This function declares a slot number in the specified attribute
 * structure.
 *
 * A slot number is only meaningful for keys that are stored in a secure
 * element. It is up to the secure element driver to decide how PSA slot
 * numbers map to any other interface that the secure element may have.
 *
 * \note Setting a slot number in key attributes for a key creation can
 *       cause the following errors when creating the key:
 *       - #PSA_ERROR_NOT_SUPPORTED if the selected secure element does
 *         not support choosing a specific slot number.
 *       - #PSA_ERROR_NOT_PERMITTED if the caller is not permitted to
 *         choose slot numbers in general or to choose this specific slot.
 *       - #PSA_ERROR_INVALID_ARGUMENT if the chosen slot number is not
 *         valid in general or not valid for this specific key.
 *       - #PSA_ERROR_ALREADY_EXISTS if there is already a key in the
 *         selected slot.
 *
 * \param[out] attributes       The attribute structure to write to.
 * \param slot_number           The slot number to set.
 */
static inline void psa_set_key_slot_number(
    psa_key_attributes_t *attributes,
    psa_key_slot_number_t slot_number )
{
    attributes->MBEDTLS_PRIVATE(core).MBEDTLS_PRIVATE(flags) |= MBEDTLS_PSA_KA_FLAG_HAS_SLOT_NUMBER;
    attributes->MBEDTLS_PRIVATE(slot_number) = slot_number;
}

/** Remove the slot number attribute from a key attribute structure.
 *
 * This function undoes the action of psa_set_key_slot_number().
 *
 * \param[out] attributes       The attribute structure to write to.
 */
static inline void psa_clear_key_slot_number(
    psa_key_attributes_t *attributes )
{
    attributes->MBEDTLS_PRIVATE(core).MBEDTLS_PRIVATE(flags) &= ~MBEDTLS_PSA_KA_FLAG_HAS_SLOT_NUMBER;
}

/** Register a key that is already present in a secure element.
 *
 * The key must be located in a secure element designated by the
 * lifetime field in \p attributes, in the slot set with
 * psa_set_key_slot_number() in the attribute structure.
 * This function makes the key available through the key identifier
 * specified in \p attributes.
 *
 * \param[in] attributes        The attributes of the existing key.
 *
 * \retval #PSA_SUCCESS
 *         The key was successfully registered.
 *         Note that depending on the design of the driver, this may or may
 *         not guarantee that a key actually exists in the designated slot
 *         and is compatible with the specified attributes.
 * \retval #PSA_ERROR_ALREADY_EXISTS
 *         There is already a key with the identifier specified in
 *         \p attributes.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         The secure element driver for the specified lifetime does not
 *         support registering a key.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         The identifier in \p attributes is invalid, namely the identifier is
 *         not in the user range, or
 *         \p attributes specifies a lifetime which is not located
 *         in a secure element, or no slot number is specified in \p attributes,
 *         or the specified slot number is not valid.
 * \retval #PSA_ERROR_NOT_PERMITTED
 *         The caller is not authorized to register the specified key slot.
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 * \retval #PSA_ERROR_INSUFFICIENT_STORAGE
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_DATA_INVALID
 * \retval #PSA_ERROR_DATA_CORRUPT
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_BAD_STATE
 *         The library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t mbedtls_psa_register_se_key(
    const psa_key_attributes_t *attributes);

#endif /* MBEDTLS_PSA_CRYPTO_SE_C */

/**@}*/

/**
 * \brief Library deinitialization.
 *
 * This function clears all data associated with the PSA layer,
 * including the whole key store.
 *
 * This is an Mbed TLS extension.
 */
void mbedtls_psa_crypto_free( void );

/** \brief Statistics about
 * resource consumption related to the PSA keystore.
 *
 * \note The content of this structure is not part of the stable API and ABI
 *       of Mbed Crypto and may change arbitrarily from version to version.
 */
typedef struct mbedtls_psa_stats_s
{
    /** Number of slots containing key material for a volatile key. */
    size_t MBEDTLS_PRIVATE(volatile_slots);
    /** Number of slots containing key material for a key which is in
     * internal persistent storage. */
    size_t MBEDTLS_PRIVATE(persistent_slots);
    /** Number of slots containing a reference to a key in a
     * secure element. */
    size_t MBEDTLS_PRIVATE(external_slots);
    /** Number of slots which are occupied, but do not contain
     * key material yet. */
    size_t MBEDTLS_PRIVATE(half_filled_slots);
    /** Number of slots that contain cache data. */
    size_t MBEDTLS_PRIVATE(cache_slots);
    /** Number of slots that are not used for anything. */
    size_t MBEDTLS_PRIVATE(empty_slots);
    /** Number of slots that are locked. */
    size_t MBEDTLS_PRIVATE(locked_slots);
    /** Largest key id value among open keys in internal persistent storage. */
    psa_key_id_t MBEDTLS_PRIVATE(max_open_internal_key_id);
    /** Largest key id value among open keys in secure elements. */
    psa_key_id_t MBEDTLS_PRIVATE(max_open_external_key_id);
} mbedtls_psa_stats_t;

/** \brief Get statistics about
 * resource consumption related to the PSA keystore.
 *
 * \note When Mbed Crypto is built as part of a service, with isolation
 *       between the application and the keystore, the service may or
 *       may not expose this function.
 */
void mbedtls_psa_get_stats( mbedtls_psa_stats_t *stats );

/**
 * \brief Inject an initial entropy seed for the random generator into
 *        secure storage.
 *
 * This function injects data to be used as a seed for the random generator
 * used by the PSA Crypto implementation. On devices that lack a trusted
 * entropy source (preferably a hardware random number generator),
 * the Mbed PSA Crypto implementation uses this value to seed its
 * random generator.
 *
 * On devices without a trusted entropy source, this function must be
 * called exactly once in the lifetime of the device. On devices with
 * a trusted entropy source, calling this function is optional.
 * In all cases, this function may only be called before calling any
 * other function in the PSA Crypto API, including psa_crypto_init().
 *
 * When this function returns successfully, it populates a file in
 * persistent storage. Once the file has been created, this function
 * can no longer succeed.
 *
 * If any error occurs, this function does not change the system state.
 * You can call this function again after correcting the reason for the
 * error if possible.
 *
 * \warning This function **can** fail! Callers MUST check the return status.
 *
 * \warning If you use this function, you should use it as part of a
 *          factory provisioning process. The value of the injected seed
 *          is critical to the security of the device. It must be
 *          *secret*, *unpredictable* and (statistically) *unique per device*.
 *          You should be generate it randomly using a cryptographically
 *          secure random generator seeded from trusted entropy sources.
 *          You should transmit it securely to the device and ensure
 *          that its value is not leaked or stored anywhere beyond the
 *          needs of transmitting it from the point of generation to
 *          the call of this function, and erase all copies of the value
 *          once this function returns.
 *
 * This is an Mbed TLS extension.
 *
 * \note This function is only available on the following platforms:
 * * If the compile-time option MBEDTLS_PSA_INJECT_ENTROPY is enabled.
 *   Note that you must provide compatible implementations of
 *   mbedtls_nv_seed_read and mbedtls_nv_seed_write.
 * * In a client-server integration of PSA Cryptography, on the client side,
 *   if the server supports this feature.
 * \param[in] seed          Buffer containing the seed value to inject.
 * \param[in] seed_size     Size of the \p seed buffer.
 *                          The size of the seed in bytes must be greater
 *                          or equal to both #MBEDTLS_ENTROPY_BLOCK_SIZE
 *                          and the value of \c MBEDTLS_ENTROPY_MIN_PLATFORM
 *                          in `library/entropy_poll.h` in the Mbed TLS source
 *                          code.
 *                          It must be less or equal to
 *                          #MBEDTLS_ENTROPY_MAX_SEED_SIZE.
 *
 * \retval #PSA_SUCCESS
 *         The seed value was injected successfully. The random generator
 *         of the PSA Crypto implementation is now ready for use.
 *         You may now call psa_crypto_init() and use the PSA Crypto
 *         implementation.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         \p seed_size is out of range.
 * \retval #PSA_ERROR_STORAGE_FAILURE
 *         There was a failure reading or writing from storage.
 * \retval #PSA_ERROR_NOT_PERMITTED
 *         The library has already been initialized. It is no longer
 *         possible to call this function.
 */
psa_status_t mbedtls_psa_inject_entropy(const uint8_t *seed,
                                        size_t seed_size);

/** \addtogroup crypto_types
 * @{
 */

/** DSA public key.
 *
 * The import and export format is the
 * representation of the public key `y = g^x mod p` as a big-endian byte
 * string. The length of the byte string is the length of the base prime `p`
 * in bytes.
 */
#define PSA_KEY_TYPE_DSA_PUBLIC_KEY                 ((psa_key_type_t)0x4002)

/** DSA key pair (private and public key).
 *
 * The import and export format is the
 * representation of the private key `x` as a big-endian byte string. The
 * length of the byte string is the private key size in bytes (leading zeroes
 * are not stripped).
 *
 * Deterministic DSA key derivation with psa_generate_derived_key follows
 * FIPS 186-4 &sect;B.1.2: interpret the byte string as integer
 * in big-endian order. Discard it if it is not in the range
 * [0, *N* - 2] where *N* is the boundary of the private key domain
 * (the prime *p* for Diffie-Hellman, the subprime *q* for DSA,
 * or the order of the curve's base point for ECC).
 * Add 1 to the resulting integer and use this as the private key *x*.
 *
 */
#define PSA_KEY_TYPE_DSA_KEY_PAIR                    ((psa_key_type_t)0x7002)

/** Whether a key type is a DSA key (pair or public-only). */
#define PSA_KEY_TYPE_IS_DSA(type)                                       \
    (PSA_KEY_TYPE_PUBLIC_KEY_OF_KEY_PAIR(type) == PSA_KEY_TYPE_DSA_PUBLIC_KEY)

#define PSA_ALG_DSA_BASE                        ((psa_algorithm_t)0x06000400)
/** DSA signature with hashing.
 *
 * This is the signature scheme defined by FIPS 186-4,
 * with a random per-message secret number (*k*).
 *
 * \param hash_alg      A hash algorithm (\c PSA_ALG_XXX value such that
 *                      #PSA_ALG_IS_HASH(\p hash_alg) is true).
 *                      This includes #PSA_ALG_ANY_HASH
 *                      when specifying the algorithm in a usage policy.
 *
 * \return              The corresponding DSA signature algorithm.
 * \return              Unspecified if \p hash_alg is not a supported
 *                      hash algorithm.
 */
#define PSA_ALG_DSA(hash_alg)                             \
    (PSA_ALG_DSA_BASE | ((hash_alg) & PSA_ALG_HASH_MASK))
#define PSA_ALG_DETERMINISTIC_DSA_BASE          ((psa_algorithm_t)0x06000500)
#define PSA_ALG_DSA_DETERMINISTIC_FLAG PSA_ALG_ECDSA_DETERMINISTIC_FLAG
/** Deterministic DSA signature with hashing.
 *
 * This is the deterministic variant defined by RFC 6979 of
 * the signature scheme defined by FIPS 186-4.
 *
 * \param hash_alg      A hash algorithm (\c PSA_ALG_XXX value such that
 *                      #PSA_ALG_IS_HASH(\p hash_alg) is true).
 *                      This includes #PSA_ALG_ANY_HASH
 *                      when specifying the algorithm in a usage policy.
 *
 * \return              The corresponding DSA signature algorithm.
 * \return              Unspecified if \p hash_alg is not a supported
 *                      hash algorithm.
 */
#define PSA_ALG_DETERMINISTIC_DSA(hash_alg)                             \
    (PSA_ALG_DETERMINISTIC_DSA_BASE | ((hash_alg) & PSA_ALG_HASH_MASK))
#define PSA_ALG_IS_DSA(alg)                                             \
    (((alg) & ~PSA_ALG_HASH_MASK & ~PSA_ALG_DSA_DETERMINISTIC_FLAG) ==  \
     PSA_ALG_DSA_BASE)
#define PSA_ALG_DSA_IS_DETERMINISTIC(alg)               \
    (((alg) & PSA_ALG_DSA_DETERMINISTIC_FLAG) != 0)
#define PSA_ALG_IS_DETERMINISTIC_DSA(alg)                       \
    (PSA_ALG_IS_DSA(alg) && PSA_ALG_DSA_IS_DETERMINISTIC(alg))
#define PSA_ALG_IS_RANDOMIZED_DSA(alg)                          \
    (PSA_ALG_IS_DSA(alg) && !PSA_ALG_DSA_IS_DETERMINISTIC(alg))


/* We need to expand the sample definition of this macro from
 * the API definition. */
#undef PSA_ALG_IS_VENDOR_HASH_AND_SIGN
#define PSA_ALG_IS_VENDOR_HASH_AND_SIGN(alg)    \
    PSA_ALG_IS_DSA(alg)

/**@}*/

/** \addtogroup attributes
 * @{
 */

/** Custom Diffie-Hellman group.
 *
 * For keys of type #PSA_KEY_TYPE_DH_PUBLIC_KEY(#PSA_DH_FAMILY_CUSTOM) or
 * #PSA_KEY_TYPE_DH_KEY_PAIR(#PSA_DH_FAMILY_CUSTOM), the group data comes
 * from domain parameters set by psa_set_key_domain_parameters().
 */
#define PSA_DH_FAMILY_CUSTOM             ((psa_dh_family_t) 0x7e)


/**
 * \brief Set domain parameters for a key.
 *
 * Some key types require additional domain parameters in addition to
 * the key type identifier and the key size. Use this function instead
 * of psa_set_key_type() when you need to specify domain parameters.
 *
 * The format for the required domain parameters varies based on the key type.
 *
 * - For RSA keys (#PSA_KEY_TYPE_RSA_PUBLIC_KEY or #PSA_KEY_TYPE_RSA_KEY_PAIR),
 *   the domain parameter data consists of the public exponent,
 *   represented as a big-endian integer with no leading zeros.
 *   This information is used when generating an RSA key pair.
 *   When importing a key, the public exponent is read from the imported
 *   key data and the exponent recorded in the attribute structure is ignored.
 *   As an exception, the public exponent 65537 is represented by an empty
 *   byte string.
 * - For DSA keys (#PSA_KEY_TYPE_DSA_PUBLIC_KEY or #PSA_KEY_TYPE_DSA_KEY_PAIR),
 *   the `Dss-Params` format as defined by RFC 3279 &sect;2.3.2.
 *   ```
 *   Dss-Params ::= SEQUENCE  {
 *      p       INTEGER,
 *      q       INTEGER,
 *      g       INTEGER
 *   }
 *   ```
 * - For Diffie-Hellman key exchange keys
 *   (#PSA_KEY_TYPE_DH_PUBLIC_KEY(#PSA_DH_FAMILY_CUSTOM) or
 *   #PSA_KEY_TYPE_DH_KEY_PAIR(#PSA_DH_FAMILY_CUSTOM)), the
 *   `DomainParameters` format as defined by RFC 3279 &sect;2.3.3.
 *   ```
 *   DomainParameters ::= SEQUENCE {
 *      p               INTEGER,                    -- odd prime, p=jq +1
 *      g               INTEGER,                    -- generator, g
 *      q               INTEGER,                    -- factor of p-1
 *      j               INTEGER OPTIONAL,           -- subgroup factor
 *      validationParams ValidationParams OPTIONAL
 *   }
 *   ValidationParams ::= SEQUENCE {
 *      seed            BIT STRING,
 *      pgenCounter     INTEGER
 *   }
 *   ```
 *
 * \note This function may allocate memory or other resources.
 *       Once you have called this function on an attribute structure,
 *       you must call psa_reset_key_attributes() to free these resources.
 *
 * \note This is an experimental extension to the interface. It may change
 *       in future versions of the library.
 *
 * \param[in,out] attributes    Attribute structure where the specified domain
 *                              parameters will be stored.
 *                              If this function fails, the content of
 *                              \p attributes is not modified.
 * \param type                  Key type (a \c PSA_KEY_TYPE_XXX value).
 * \param[in] data              Buffer containing the key domain parameters.
 *                              The content of this buffer is interpreted
 *                              according to \p type as described above.
 * \param data_length           Size of the \p data buffer in bytes.
 *
 * \retval #PSA_SUCCESS
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 * \retval #PSA_ERROR_NOT_SUPPORTED
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 */
psa_status_t psa_set_key_domain_parameters(psa_key_attributes_t *attributes,
                                           psa_key_type_t type,
                                           const uint8_t *data,
                                           size_t data_length);

/**
 * \brief Get domain parameters for a key.
 *
 * Get the domain parameters for a key with this function, if any. The format
 * of the domain parameters written to \p data is specified in the
 * documentation for psa_set_key_domain_parameters().
 *
 * \note This is an experimental extension to the interface. It may change
 *       in future versions of the library.
 *
 * \param[in] attributes        The key attribute structure to query.
 * \param[out] data             On success, the key domain parameters.
 * \param data_size             Size of the \p data buffer in bytes.
 *                              The buffer is guaranteed to be large
 *                              enough if its size in bytes is at least
 *                              the value given by
 *                              PSA_KEY_DOMAIN_PARAMETERS_SIZE().
 * \param[out] data_length      On success, the number of bytes
 *                              that make up the key domain parameters data.
 *
 * \retval #PSA_SUCCESS
 * \retval #PSA_ERROR_BUFFER_TOO_SMALL
 */
psa_status_t psa_get_key_domain_parameters(
    const psa_key_attributes_t *attributes,
    uint8_t *data,
    size_t data_size,
    size_t *data_length);

/** Safe output buffer size for psa_get_key_domain_parameters().
 *
 * This macro returns a compile-time constant if its arguments are
 * compile-time constants.
 *
 * \warning This function may call its arguments multiple times or
 *          zero times, so you should not pass arguments that contain
 *          side effects.
 *
 * \note This is an experimental extension to the interface. It may change
 *       in future versions of the library.
 *
 * \param key_type  A supported key type.
 * \param key_bits  The size of the key in bits.
 *
 * \return If the parameters are valid and supported, return
 *         a buffer size in bytes that guarantees that
 *         psa_get_key_domain_parameters() will not fail with
 *         #PSA_ERROR_BUFFER_TOO_SMALL.
 *         If the parameters are a valid combination that is not supported
 *         by the implementation, this macro shall return either a
 *         sensible size or 0.
 *         If the parameters are not valid, the
 *         return value is unspecified.
 */
#define PSA_KEY_DOMAIN_PARAMETERS_SIZE(key_type, key_bits)              \
    (PSA_KEY_TYPE_IS_RSA(key_type) ? sizeof(int) :                      \
     PSA_KEY_TYPE_IS_DH(key_type) ? PSA_DH_KEY_DOMAIN_PARAMETERS_SIZE(key_bits) : \
     PSA_KEY_TYPE_IS_DSA(key_type) ? PSA_DSA_KEY_DOMAIN_PARAMETERS_SIZE(key_bits) : \
     0)
#define PSA_DH_KEY_DOMAIN_PARAMETERS_SIZE(key_bits)     \
    (4 + (PSA_BITS_TO_BYTES(key_bits) + 5) * 3 /*without optional parts*/)
#define PSA_DSA_KEY_DOMAIN_PARAMETERS_SIZE(key_bits)    \
    (4 + (PSA_BITS_TO_BYTES(key_bits) + 5) * 2 /*p, g*/ + 34 /*q*/)

/**@}*/

/** \defgroup psa_tls_helpers TLS helper functions
 * @{
 */

#if defined(MBEDTLS_ECP_C)
#include <mbedtls/ecp.h>

/** Convert an ECC curve identifier from the Mbed TLS encoding to PSA.
 *
 * \note This function is provided solely for the convenience of
 *       Mbed TLS and may be removed at any time without notice.
 *
 * \param grpid         An Mbed TLS elliptic curve identifier
 *                      (`MBEDTLS_ECP_DP_xxx`).
 * \param[out] bits     On success, the bit size of the curve.
 *
 * \return              The corresponding PSA elliptic curve identifier
 *                      (`PSA_ECC_FAMILY_xxx`).
 * \return              \c 0 on failure (\p grpid is not recognized).
 */
static inline psa_ecc_family_t mbedtls_ecc_group_to_psa( mbedtls_ecp_group_id grpid,
                                                        size_t *bits )
{
    switch( grpid )
    {
        case MBEDTLS_ECP_DP_SECP192R1:
            *bits = 192;
            return( PSA_ECC_FAMILY_SECP_R1 );
        case MBEDTLS_ECP_DP_SECP224R1:
            *bits = 224;
            return( PSA_ECC_FAMILY_SECP_R1 );
        case MBEDTLS_ECP_DP_SECP256R1:
            *bits = 256;
            return( PSA_ECC_FAMILY_SECP_R1 );
        case MBEDTLS_ECP_DP_SECP384R1:
            *bits = 384;
            return( PSA_ECC_FAMILY_SECP_R1 );
        case MBEDTLS_ECP_DP_SECP521R1:
            *bits = 521;
            return( PSA_ECC_FAMILY_SECP_R1 );
        case MBEDTLS_ECP_DP_BP256R1:
            *bits = 256;
            return( PSA_ECC_FAMILY_BRAINPOOL_P_R1 );
        case MBEDTLS_ECP_DP_BP384R1:
            *bits = 384;
            return( PSA_ECC_FAMILY_BRAINPOOL_P_R1 );
        case MBEDTLS_ECP_DP_BP512R1:
            *bits = 512;
            return( PSA_ECC_FAMILY_BRAINPOOL_P_R1 );
        case MBEDTLS_ECP_DP_CURVE25519:
            *bits = 255;
            return( PSA_ECC_FAMILY_MONTGOMERY );
        case MBEDTLS_ECP_DP_SECP192K1:
            *bits = 192;
            return( PSA_ECC_FAMILY_SECP_K1 );
        case MBEDTLS_ECP_DP_SECP224K1:
            *bits = 224;
            return( PSA_ECC_FAMILY_SECP_K1 );
        case MBEDTLS_ECP_DP_SECP256K1:
            *bits = 256;
            return( PSA_ECC_FAMILY_SECP_K1 );
        case MBEDTLS_ECP_DP_CURVE448:
            *bits = 448;
            return( PSA_ECC_FAMILY_MONTGOMERY );
        default:
            *bits = 0;
            return( 0 );
    }
}

/** Convert an ECC curve identifier from the PSA encoding to Mbed TLS.
 *
 * \note This function is provided solely for the convenience of
 *       Mbed TLS and may be removed at any time without notice.
 *
 * \param curve         A PSA elliptic curve identifier
 *                      (`PSA_ECC_FAMILY_xxx`).
 * \param bits          The bit-length of a private key on \p curve.
 * \param bits_is_sloppy If true, \p bits may be the bit-length rounded up
 *                      to the nearest multiple of 8. This allows the caller
 *                      to infer the exact curve from the length of a key
 *                      which is supplied as a byte string.
 *
 * \return              The corresponding Mbed TLS elliptic curve identifier
 *                      (`MBEDTLS_ECP_DP_xxx`).
 * \return              #MBEDTLS_ECP_DP_NONE if \c curve is not recognized.
 * \return              #MBEDTLS_ECP_DP_NONE if \p bits is not
 *                      correct for \p curve.
 */
mbedtls_ecp_group_id mbedtls_ecc_group_of_psa( psa_ecc_family_t curve,
                                               size_t bits,
                                               int bits_is_sloppy );
#endif /* MBEDTLS_ECP_C */

/**@}*/

/** \defgroup psa_external_rng External random generator
 * @{
 */

#if defined(MBEDTLS_PSA_CRYPTO_EXTERNAL_RNG)
/** External random generator function, implemented by the platform.
 *
 * When the compile-time option #MBEDTLS_PSA_CRYPTO_EXTERNAL_RNG is enabled,
 * this function replaces Mbed TLS's entropy and DRBG modules for all
 * random generation triggered via PSA crypto interfaces.
 *
 * \note This random generator must deliver random numbers with cryptographic
 *       quality and high performance. It must supply unpredictable numbers
 *       with a uniform distribution. The implementation of this function
 *       is responsible for ensuring that the random generator is seeded
 *       with sufficient entropy. If you have a hardware TRNG which is slow
 *       or delivers non-uniform output, declare it as an entropy source
 *       with mbedtls_entropy_add_source() instead of enabling this option.
 *
 * \param[in,out] context       Pointer to the random generator context.
 *                              This is all-bits-zero on the first call
 *                              and preserved between successive calls.
 * \param[out] output           Output buffer. On success, this buffer
 *                              contains random data with a uniform
 *                              distribution.
 * \param output_size           The size of the \p output buffer in bytes.
 * \param[out] output_length    On success, set this value to \p output_size.
 *
 * \retval #PSA_SUCCESS
 *         Success. The output buffer contains \p output_size bytes of
 *         cryptographic-quality random data, and \c *output_length is
 *         set to \p output_size.
 * \retval #PSA_ERROR_INSUFFICIENT_ENTROPY
 *         The random generator requires extra entropy and there is no
 *         way to obtain entropy under current environment conditions.
 *         This error should not happen under normal circumstances since
 *         this function is responsible for obtaining as much entropy as
 *         it needs. However implementations of this function may return
 *         #PSA_ERROR_INSUFFICIENT_ENTROPY if there is no way to obtain
 *         entropy without blocking indefinitely.
 * \retval #PSA_ERROR_HARDWARE_FAILURE
 *         A failure of the random generator hardware that isn't covered
 *         by #PSA_ERROR_INSUFFICIENT_ENTROPY.
 */
psa_status_t mbedtls_psa_external_get_random(
    mbedtls_psa_external_random_context_t *context,
    uint8_t *output, size_t output_size, size_t *output_length );
#endif /* MBEDTLS_PSA_CRYPTO_EXTERNAL_RNG */

/**@}*/

/** \defgroup psa_builtin_keys Built-in keys
 * @{
 */

/** The minimum value for a key identifier that is built into the
 * implementation.
 *
 * The range of key identifiers from #MBEDTLS_PSA_KEY_ID_BUILTIN_MIN
 * to #MBEDTLS_PSA_KEY_ID_BUILTIN_MAX within the range from
 * #PSA_KEY_ID_VENDOR_MIN and #PSA_KEY_ID_VENDOR_MAX and must not intersect
 * with any other set of implementation-chosen key identifiers.
 *
 * This value is part of the library's ABI since changing it would invalidate
 * the values of built-in key identifiers in applications.
 */
#define MBEDTLS_PSA_KEY_ID_BUILTIN_MIN          ((psa_key_id_t)0x7fff0000)

/** The maximum value for a key identifier that is built into the
 * implementation.
 *
 * See #MBEDTLS_PSA_KEY_ID_BUILTIN_MIN for more information.
 */
#define MBEDTLS_PSA_KEY_ID_BUILTIN_MAX          ((psa_key_id_t)0x7fffefff)

/** A slot number identifying a key in a driver.
 *
 * Values of this type are used to identify built-in keys.
 */
typedef uint64_t psa_drv_slot_number_t;

#if defined(MBEDTLS_PSA_CRYPTO_BUILTIN_KEYS)
/** Test whether a key identifier belongs to the builtin key range.
 *
 * \param key_id  Key identifier to test.
 *
 * \retval 1
 *         The key identifier is a builtin key identifier.
 * \retval 0
 *         The key identifier is not a builtin key identifier.
 */
static inline int psa_key_id_is_builtin( psa_key_id_t key_id )
{
    return( ( key_id >= MBEDTLS_PSA_KEY_ID_BUILTIN_MIN ) &&
            ( key_id <= MBEDTLS_PSA_KEY_ID_BUILTIN_MAX ) );
}

/** Platform function to obtain the location and slot number of a built-in key.
 *
 * An application-specific implementation of this function must be provided if
 * #MBEDTLS_PSA_CRYPTO_BUILTIN_KEYS is enabled. This would typically be provided
 * as part of a platform's system image.
 *
 * #MBEDTLS_SVC_KEY_ID_GET_KEY_ID(\p key_id) needs to be in the range from
 * #MBEDTLS_PSA_KEY_ID_BUILTIN_MIN to #MBEDTLS_PSA_KEY_ID_BUILTIN_MAX.
 *
 * In a multi-application configuration
 * (\c MBEDTLS_PSA_CRYPTO_KEY_ID_ENCODES_OWNER is defined),
 * this function should check that #MBEDTLS_SVC_KEY_ID_GET_OWNER_ID(\p key_id)
 * is allowed to use the given key.
 *
 * \param key_id                The key ID for which to retrieve the
 *                              location and slot attributes.
 * \param[out] lifetime         On success, the lifetime associated with the key
 *                              corresponding to \p key_id. Lifetime is a
 *                              combination of which driver contains the key,
 *                              and with what persistence level the key is
 *                              intended to be used. If the platform
 *                              implementation does not contain specific
 *                              information about the intended key persistence
 *                              level, the persistence level may be reported as
 *                              #PSA_KEY_PERSISTENCE_DEFAULT.
 * \param[out] slot_number      On success, the slot number known to the driver
 *                              registered at the lifetime location reported
 *                              through \p lifetime which corresponds to the
 *                              requested built-in key.
 *
 * \retval #PSA_SUCCESS
 *         The requested key identifier designates a built-in key.
 *         In a multi-application configuration, the requested owner
 *         is allowed to access it.
 * \retval #PSA_ERROR_DOES_NOT_EXIST
 *         The requested key identifier is not a built-in key which is known
 *         to this function. If a key exists in the key storage with this
 *         identifier, the data from the storage will be used.
 * \return (any other error)
 *         Any other error is propagated to the function that requested the key.
 *         Common errors include:
 *         - #PSA_ERROR_NOT_PERMITTED: the key exists but the requested owner
 *           is not allowed to access it.
 */
psa_status_t mbedtls_psa_platform_get_builtin_key(
    mbedtls_svc_key_id_t key_id,
    psa_key_lifetime_t *lifetime,
    psa_drv_slot_number_t *slot_number );
#endif /* MBEDTLS_PSA_CRYPTO_BUILTIN_KEYS */

/** @} */

/** \addtogroup crypto_types
 * @{
 */

#define PSA_ALG_CATEGORY_PAKE                   ((psa_algorithm_t)0x0a000000)

/** Whether the specified algorithm is a password-authenticated key exchange.
 *
 * \param alg An algorithm identifier (value of type #psa_algorithm_t).
 *
 * \return 1 if \p alg is a password-authenticated key exchange (PAKE)
 *         algorithm, 0 otherwise.
 *         This macro may return either 0 or 1 if \p alg is not a supported
 *         algorithm identifier.
 */
#define PSA_ALG_IS_PAKE(alg)                                        \
    (((alg) & PSA_ALG_CATEGORY_MASK) == PSA_ALG_CATEGORY_PAKE)

/** The Password-authenticated key exchange by juggling (J-PAKE) algorithm.
 *
 * This is J-PAKE as defined by RFC 8236, instantiated with the following
 * parameters:
 *
 * - The group can be either an elliptic curve or defined over a finite field.
 * - Schnorr NIZK proof as defined by RFC 8235 and using the same group as the
 *   J-PAKE algorithm.
 * - A cryptographic hash function.
 *
 * To select these parameters and set up the cipher suite, call these functions
 * in any order:
 *
 * \code
 * psa_pake_cs_set_algorithm(cipher_suite, PSA_ALG_JPAKE);
 * psa_pake_cs_set_primitive(cipher_suite,
 *                           PSA_PAKE_PRIMITIVE(type, family, bits));
 * psa_pake_cs_set_hash(cipher_suite, hash);
 * \endcode
 *
 * For more information on how to set a specific curve or field, refer to the
 * documentation of the individual \c PSA_PAKE_PRIMITIVE_TYPE_XXX constants.
 *
 * After initializing a J-PAKE operation, call
 *
 * \code
 * psa_pake_setup(operation, cipher_suite);
 * psa_pake_set_user(operation, ...);
 * psa_pake_set_peer(operation, ...);
 * psa_pake_set_password_key(operation, ...);
 * \endcode
 *
 * The password is provided as a key. This can be the password text itself,
 * in an agreed character encoding, or some value derived from the password
 * as required by a higher level protocol.
 *
 * (The implementation converts the key material to a number as described in
 * Section 2.3.8 of _SEC 1: Elliptic Curve Cryptography_
 * (https://www.secg.org/sec1-v2.pdf), before reducing it modulo \c q. Here
 * \c q is order of the group defined by the primitive set in the cipher suite.
 * The \c psa_pake_set_password_key() function returns an error if the result
 * of the reduction is 0.)
 *
 * The key exchange flow for J-PAKE is as follows:
 * -# To get the first round data that needs to be sent to the peer, call
 *    \code
 *    // Get g1
 *    psa_pake_output(operation, #PSA_PAKE_STEP_KEY_SHARE, ...);
 *    // Get the ZKP public key for x1
 *    psa_pake_output(operation, #PSA_PAKE_STEP_ZK_PUBLIC, ...);
 *    // Get the ZKP proof for x1
 *    psa_pake_output(operation, #PSA_PAKE_STEP_ZK_PROOF, ...);
 *    // Get g2
 *    psa_pake_output(operation, #PSA_PAKE_STEP_KEY_SHARE, ...);
 *    // Get the ZKP public key for x2
 *    psa_pake_output(operation, #PSA_PAKE_STEP_ZK_PUBLIC, ...);
 *    // Get the ZKP proof for x2
 *    psa_pake_output(operation, #PSA_PAKE_STEP_ZK_PROOF, ...);
 *    \endcode
 * -# To provide the first round data received from the peer to the operation,
 *    call
 *    \code
 *    // Set g3
 *    psa_pake_input(operation, #PSA_PAKE_STEP_KEY_SHARE, ...);
 *    // Set the ZKP public key for x3
 *    psa_pake_input(operation, #PSA_PAKE_STEP_ZK_PUBLIC, ...);
 *    // Set the ZKP proof for x3
 *    psa_pake_input(operation, #PSA_PAKE_STEP_ZK_PROOF, ...);
 *    // Set g4
 *    psa_pake_input(operation, #PSA_PAKE_STEP_KEY_SHARE, ...);
 *    // Set the ZKP public key for x4
 *    psa_pake_input(operation, #PSA_PAKE_STEP_ZK_PUBLIC, ...);
 *    // Set the ZKP proof for x4
 *    psa_pake_input(operation, #PSA_PAKE_STEP_ZK_PROOF, ...);
 *    \endcode
 * -# To get the second round data that needs to be sent to the peer, call
 *    \code
 *    // Get A
 *    psa_pake_output(operation, #PSA_PAKE_STEP_KEY_SHARE, ...);
 *    // Get ZKP public key for x2*s
 *    psa_pake_output(operation, #PSA_PAKE_STEP_ZK_PUBLIC, ...);
 *    // Get ZKP proof for x2*s
 *    psa_pake_output(operation, #PSA_PAKE_STEP_ZK_PROOF, ...);
 *    \endcode
 * -# To provide the second round data received from the peer to the operation,
 *    call
 *    \code
 *    // Set B
 *    psa_pake_input(operation, #PSA_PAKE_STEP_KEY_SHARE, ...);
 *    // Set ZKP public key for x4*s
 *    psa_pake_input(operation, #PSA_PAKE_STEP_ZK_PUBLIC, ...);
 *    // Set ZKP proof for x4*s
 *    psa_pake_input(operation, #PSA_PAKE_STEP_ZK_PROOF, ...);
 *    \endcode
 * -# To access the shared secret call
 *    \code
 *    // Get Ka=Kb=K
 *    psa_pake_get_implicit_key()
 *    \endcode
 *
 * For more information consult the documentation of the individual
 * \c PSA_PAKE_STEP_XXX constants.
 *
 * At this point there is a cryptographic guarantee that only the authenticated
 * party who used the same password is able to compute the key. But there is no
 * guarantee that the peer is the party it claims to be and was able to do so.
 *
 * That is, the authentication is only implicit (the peer is not authenticated
 * at this point, and no action should be taken that assume that they are - like
 * for example accessing restricted files).
 *
 * To make the authentication explicit there are various methods, see Section 5
 * of RFC 8236 for two examples.
 *
 */
#define PSA_ALG_JPAKE                   ((psa_algorithm_t)0x0a000100)

/** @} */

/** \defgroup pake Password-authenticated key exchange (PAKE)
 *
 * This is a proposed PAKE interface for the PSA Crypto API. It is not part of
 * the official PSA Crypto API yet.
 *
 * \note The content of this section is not part of the stable API and ABI
 *       of Mbed Crypto and may change arbitrarily from version to version.
 *       Same holds for the corresponding macros #PSA_ALG_CATEGORY_PAKE and
 *       #PSA_ALG_JPAKE.
 * @{
 */

/** \brief Encoding of the application role of PAKE
 *
 * Encodes the application's role in the algorithm is being executed. For more
 * information see the documentation of individual \c PSA_PAKE_ROLE_XXX
 * constants.
 */
typedef uint8_t psa_pake_role_t;

/** Encoding of input and output indicators for PAKE.
 *
 * Some PAKE algorithms need to exchange more data than just a single key share.
 * This type is for encoding additional input and output data for such
 * algorithms.
 */
typedef uint8_t psa_pake_step_t;

/** Encoding of the type of the PAKE's primitive.
 *
 * Values defined by this standard will never be in the range 0x80-0xff.
 * Vendors who define additional types must use an encoding in this range.
 *
 * For more information see the documentation of individual
 * \c PSA_PAKE_PRIMITIVE_TYPE_XXX constants.
 */
typedef uint8_t psa_pake_primitive_type_t;

/** \brief Encoding of the family of the primitive associated with the PAKE.
 *
 * For more information see the documentation of individual
 * \c PSA_PAKE_PRIMITIVE_TYPE_XXX constants.
 */
typedef uint8_t psa_pake_family_t;

/** \brief Encoding of the primitive associated with the PAKE.
 *
 * For more information see the documentation of the #PSA_PAKE_PRIMITIVE macro.
 */
typedef uint32_t psa_pake_primitive_t;

/** A value to indicate no role in a PAKE algorithm.
 * This value can be used in a call to psa_pake_set_role() for symmetric PAKE
 * algorithms which do not assign roles.
 */
#define PSA_PAKE_ROLE_NONE                  ((psa_pake_role_t)0x00)

/** The first peer in a balanced PAKE.
 *
 * Although balanced PAKE algorithms are symmetric, some of them needs an
 * ordering of peers for the transcript calculations. If the algorithm does not
 * need this, both #PSA_PAKE_ROLE_FIRST and #PSA_PAKE_ROLE_SECOND are
 * accepted.
 */
#define PSA_PAKE_ROLE_FIRST                ((psa_pake_role_t)0x01)

/** The second peer in a balanced PAKE.
 *
 * Although balanced PAKE algorithms are symmetric, some of them needs an
 * ordering of peers for the transcript calculations. If the algorithm does not
 * need this, either #PSA_PAKE_ROLE_FIRST or #PSA_PAKE_ROLE_SECOND are
 * accepted.
 */
#define PSA_PAKE_ROLE_SECOND                ((psa_pake_role_t)0x02)

/** The client in an augmented PAKE.
 *
 * Augmented PAKE algorithms need to differentiate between client and server.
 */
#define PSA_PAKE_ROLE_CLIENT                ((psa_pake_role_t)0x11)

/** The server in an augmented PAKE.
 *
 * Augmented PAKE algorithms need to differentiate between client and server.
 */
#define PSA_PAKE_ROLE_SERVER                ((psa_pake_role_t)0x12)

/** The PAKE primitive type indicating the use of elliptic curves.
 *
 * The values of the \c family and \c bits fields of the cipher suite identify a
 * specific elliptic curve, using the same mapping that is used for ECC
 * (::psa_ecc_family_t) keys.
 *
 * (Here \c family means the value returned by psa_pake_cs_get_family() and
 * \c bits means the value returned by psa_pake_cs_get_bits().)
 *
 * Input and output during the operation can involve group elements and scalar
 * values:
 * -# The format for group elements is the same as for public keys on the
 *  specific curve would be. For more information, consult the documentation of
 *  psa_export_public_key().
 * -# The format for scalars is the same as for private keys on the specific
 *  curve would be. For more information, consult the documentation of
 *  psa_export_key().
 */
#define PSA_PAKE_PRIMITIVE_TYPE_ECC       ((psa_pake_primitive_type_t)0x01)

/** The PAKE primitive type indicating the use of Diffie-Hellman groups.
 *
 * The values of the \c family and \c bits fields of the cipher suite identify
 * a specific Diffie-Hellman group, using the same mapping that is used for
 * Diffie-Hellman (::psa_dh_family_t) keys.
 *
 * (Here \c family means the value returned by psa_pake_cs_get_family() and
 * \c bits means the value returned by psa_pake_cs_get_bits().)
 *
 * Input and output during the operation can involve group elements and scalar
 * values:
 * -# The format for group elements is the same as for public keys on the
 *  specific group would be. For more information, consult the documentation of
 *  psa_export_public_key().
 * -# The format for scalars is the same as for private keys on the specific
 *  group would be. For more information, consult the documentation of
 *  psa_export_key().
 */
#define PSA_PAKE_PRIMITIVE_TYPE_DH       ((psa_pake_primitive_type_t)0x02)

/** Construct a PAKE primitive from type, family and bit-size.
 *
 * \param pake_type     The type of the primitive
 *                      (value of type ::psa_pake_primitive_type_t).
 * \param pake_family   The family of the primitive
 *                      (the type and interpretation of this parameter depends
 *                      on \p type, for more information consult the
 *                      documentation of individual ::psa_pake_primitive_type_t
 *                      constants).
 * \param pake_bits     The bit-size of the primitive
 *                      (Value of type \c size_t. The interpretation
 *                      of this parameter depends on \p family, for more
 *                      information consult the documentation of individual
 *                      ::psa_pake_primitive_type_t constants).
 *
 * \return The constructed primitive value of type ::psa_pake_primitive_t.
 *         Return 0 if the requested primitive can't be encoded as
 *         ::psa_pake_primitive_t.
 */
#define PSA_PAKE_PRIMITIVE(pake_type, pake_family, pake_bits) \
    ((pake_bits & 0xFFFF) != pake_bits) ? 0 :                 \
    ((psa_pake_primitive_t) (((pake_type) << 24 |             \
            (pake_family) << 16) | (pake_bits)))

/** The key share being sent to or received from the peer.
 *
 * The format for both input and output at this step is the same as for public
 * keys on the group determined by the primitive (::psa_pake_primitive_t) would
 * be.
 *
 * For more information on the format, consult the documentation of
 * psa_export_public_key().
 *
 * For information regarding how the group is determined, consult the
 * documentation #PSA_PAKE_PRIMITIVE.
 */
#define PSA_PAKE_STEP_KEY_SHARE                 ((psa_pake_step_t)0x01)

/** A Schnorr NIZKP public key.
 *
 * This is the ephemeral public key in the Schnorr Non-Interactive
 * Zero-Knowledge Proof (the value denoted by the letter 'V' in RFC 8235).
 *
 * The format for both input and output at this step is the same as for public
 * keys on the group determined by the primitive (::psa_pake_primitive_t) would
 * be.
 *
 * For more information on the format, consult the documentation of
 * psa_export_public_key().
 *
 * For information regarding how the group is determined, consult the
 * documentation #PSA_PAKE_PRIMITIVE.
 */
#define PSA_PAKE_STEP_ZK_PUBLIC                 ((psa_pake_step_t)0x02)

/** A Schnorr NIZKP proof.
 *
 * This is the proof in the Schnorr Non-Interactive Zero-Knowledge Proof (the
 * value denoted by the letter 'r' in RFC 8235).
 *
 * Both for input and output, the value at this step is an integer less than
 * the order of the group selected in the cipher suite. The format depends on
 * the group as well:
 *
 * - For Montgomery curves, the encoding is little endian.
 * - For everything else the encoding is big endian (see Section 2.3.8 of
 *   _SEC 1: Elliptic Curve Cryptography_ at https://www.secg.org/sec1-v2.pdf).
 *
 * In both cases leading zeroes are allowed as long as the length in bytes does
 * not exceed the byte length of the group order.
 *
 * For information regarding how the group is determined, consult the
 * documentation #PSA_PAKE_PRIMITIVE.
 */
#define PSA_PAKE_STEP_ZK_PROOF                  ((psa_pake_step_t)0x03)

/** The type of the data structure for PAKE cipher suites.
 *
 * This is an implementation-defined \c struct. Applications should not
 * make any assumptions about the content of this structure.
 * Implementation details can change in future versions without notice.
 */
typedef struct psa_pake_cipher_suite_s psa_pake_cipher_suite_t;

/** Return an initial value for a PAKE cipher suite object.
 */
static psa_pake_cipher_suite_t psa_pake_cipher_suite_init( void );

/** Retrieve the PAKE algorithm from a PAKE cipher suite.
 *
 * \param[in] cipher_suite     The cipher suite structure to query.
 *
 * \return The PAKE algorithm stored in the cipher suite structure.
 */
static psa_algorithm_t psa_pake_cs_get_algorithm(
                           const psa_pake_cipher_suite_t *cipher_suite );

/** Declare the PAKE algorithm for the cipher suite.
 *
 * This function overwrites any PAKE algorithm
 * previously set in \p cipher_suite.
 *
 * \param[out] cipher_suite    The cipher suite structure to write to.
 * \param algorithm            The PAKE algorithm to write.
 *                             (`PSA_ALG_XXX` values of type ::psa_algorithm_t
 *                             such that #PSA_ALG_IS_PAKE(\c alg) is true.)
 *                             If this is 0, the PAKE algorithm in
 *                             \p cipher_suite becomes unspecified.
 */
static void psa_pake_cs_set_algorithm( psa_pake_cipher_suite_t *cipher_suite,
                                       psa_algorithm_t algorithm );

/** Retrieve the primitive from a PAKE cipher suite.
 *
 * \param[in] cipher_suite     The cipher suite structure to query.
 *
 * \return The primitive stored in the cipher suite structure.
 */
static psa_pake_primitive_t psa_pake_cs_get_primitive(
                           const psa_pake_cipher_suite_t *cipher_suite );

/** Declare the primitive for a PAKE cipher suite.
 *
 * This function overwrites any primitive previously set in \p cipher_suite.
 *
 * \param[out] cipher_suite    The cipher suite structure to write to.
 * \param primitive            The primitive to write. If this is 0, the
 *                             primitive type in \p cipher_suite becomes
 *                             unspecified.
 */
static void psa_pake_cs_set_primitive( psa_pake_cipher_suite_t *cipher_suite,
                                       psa_pake_primitive_t primitive );

/** Retrieve the PAKE family from a PAKE cipher suite.
 *
 * \param[in] cipher_suite     The cipher suite structure to query.
 *
 * \return The PAKE family stored in the cipher suite structure.
 */
static psa_pake_family_t psa_pake_cs_get_family(
                           const psa_pake_cipher_suite_t *cipher_suite );

/** Retrieve the PAKE primitive bit-size from a PAKE cipher suite.
 *
 * \param[in] cipher_suite     The cipher suite structure to query.
 *
 * \return The PAKE primitive bit-size stored in the cipher suite structure.
 */
static uint16_t psa_pake_cs_get_bits(
                           const psa_pake_cipher_suite_t *cipher_suite );

/** Retrieve the hash algorithm from a PAKE cipher suite.
 *
 * \param[in] cipher_suite      The cipher suite structure to query.
 *
 * \return The hash algorithm stored in the cipher suite structure. The return
 *         value is 0 if the PAKE is not parametrised by a hash algorithm or if
 *         the hash algorithm is not set.
 */
static psa_algorithm_t psa_pake_cs_get_hash(
                           const psa_pake_cipher_suite_t *cipher_suite );

/** Declare the hash algorithm for a PAKE cipher suite.
 *
 * This function overwrites any hash algorithm
 * previously set in \p cipher_suite.
 *
 * Refer to the documentation of individual PAKE algorithm types (`PSA_ALG_XXX`
 * values of type ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true)
 * for more information.
 *
 * \param[out] cipher_suite     The cipher suite structure to write to.
 * \param hash                  The hash involved in the cipher suite.
 *                              (`PSA_ALG_XXX` values of type ::psa_algorithm_t
 *                              such that #PSA_ALG_IS_HASH(\c alg) is true.)
 *                              If this is 0, the hash algorithm in
 *                              \p cipher_suite becomes unspecified.
 */
static void psa_pake_cs_set_hash( psa_pake_cipher_suite_t *cipher_suite,
                                  psa_algorithm_t hash );

/** The type of the state data structure for PAKE operations.
 *
 * Before calling any function on a PAKE operation object, the application
 * must initialize it by any of the following means:
 * - Set the structure to all-bits-zero, for example:
 *   \code
 *   psa_pake_operation_t operation;
 *   memset(&operation, 0, sizeof(operation));
 *   \endcode
 * - Initialize the structure to logical zero values, for example:
 *   \code
 *   psa_pake_operation_t operation = {0};
 *   \endcode
 * - Initialize the structure to the initializer #PSA_PAKE_OPERATION_INIT,
 *   for example:
 *   \code
 *   psa_pake_operation_t operation = PSA_PAKE_OPERATION_INIT;
 *   \endcode
 * - Assign the result of the function psa_pake_operation_init()
 *   to the structure, for example:
 *   \code
 *   psa_pake_operation_t operation;
 *   operation = psa_pake_operation_init();
 *   \endcode
 *
 * This is an implementation-defined \c struct. Applications should not
 * make any assumptions about the content of this structure.
 * Implementation details can change in future versions without notice. */
typedef struct psa_pake_operation_s psa_pake_operation_t;

/** Return an initial value for a PAKE operation object.
 */
static psa_pake_operation_t psa_pake_operation_init( void );

/** Set the session information for a password-authenticated key exchange.
 *
 * The sequence of operations to set up a password-authenticated key exchange
 * is as follows:
 * -# Allocate an operation object which will be passed to all the functions
 *    listed here.
 * -# Initialize the operation object with one of the methods described in the
 *    documentation for #psa_pake_operation_t, e.g.
 *    #PSA_PAKE_OPERATION_INIT.
 * -# Call psa_pake_setup() to specify the cipher suite.
 * -# Call \c psa_pake_set_xxx() functions on the operation to complete the
 *    setup. The exact sequence of \c psa_pake_set_xxx() functions that needs
 *    to be called depends on the algorithm in use.
 *
 * Refer to the documentation of individual PAKE algorithm types (`PSA_ALG_XXX`
 * values of type ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true)
 * for more information.
 *
 * A typical sequence of calls to perform a password-authenticated key
 * exchange:
 * -# Call psa_pake_output(operation, #PSA_PAKE_STEP_KEY_SHARE, ...) to get the
 *    key share that needs to be sent to the peer.
 * -# Call psa_pake_input(operation, #PSA_PAKE_STEP_KEY_SHARE, ...) to provide
 *    the key share that was received from the peer.
 * -# Depending on the algorithm additional calls to psa_pake_output() and
 *    psa_pake_input() might be necessary.
 * -# Call psa_pake_get_implicit_key() for accessing the shared secret.
 *
 * Refer to the documentation of individual PAKE algorithm types (`PSA_ALG_XXX`
 * values of type ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true)
 * for more information.
 *
 * If an error occurs at any step after a call to psa_pake_setup(),
 * the operation will need to be reset by a call to psa_pake_abort(). The
 * application may call psa_pake_abort() at any time after the operation
 * has been initialized.
 *
 * After a successful call to psa_pake_setup(), the application must
 * eventually terminate the operation. The following events terminate an
 * operation:
 * - A call to psa_pake_abort().
 * - A successful call to psa_pake_get_implicit_key().
 *
 * \param[in,out] operation     The operation object to set up. It must have
 *                              been initialized but not set up yet.
 * \param[in] cipher_suite      The cipher suite to use. (A cipher suite fully
 *                              characterizes a PAKE algorithm and determines
 *                              the algorithm as well.)
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         The algorithm in \p cipher_suite is not a PAKE algorithm, or the
 *         PAKE primitive in \p cipher_suite is not compatible with the
 *         PAKE algorithm, or the hash algorithm in \p cipher_suite is invalid
 *         or not compatible with the PAKE algorithm and primitive.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         The algorithm in \p cipher_suite is not a supported PAKE algorithm,
 *         or the PAKE primitive in \p cipher_suite is not supported or not
 *         compatible with the PAKE algorithm, or the hash algorithm in
 *         \p cipher_suite is not supported or not compatible with the PAKE
 *         algorithm and primitive.
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_BAD_STATE
 *         The operation state is not valid, or
 *         the library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_setup( psa_pake_operation_t *operation,
                             const psa_pake_cipher_suite_t *cipher_suite );

/** Set the password for a password-authenticated key exchange from key ID.
 *
 * Call this function when the password, or a value derived from the password,
 * is already present in the key store.
 *
 * \param[in,out] operation     The operation object to set the password for. It
 *                              must have been set up by psa_pake_setup() and
 *                              not yet in use (neither psa_pake_output() nor
 *                              psa_pake_input() has been called yet). It must
 *                              be on operation for which the password hasn't
 *                              been set yet (psa_pake_set_password_key()
 *                              hasn't been called yet).
 * \param password              Identifier of the key holding the password or a
 *                              value derived from the password (eg. by a
 *                              memory-hard function).  It must remain valid
 *                              until the operation terminates. It must be of
 *                              type #PSA_KEY_TYPE_PASSWORD or
 *                              #PSA_KEY_TYPE_PASSWORD_HASH. It has to allow
 *                              the usage #PSA_KEY_USAGE_DERIVE.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_HANDLE
 *         \p password is not a valid key identifier.
 * \retval #PSA_ERROR_NOT_PERMITTED
 *         The key does not have the #PSA_KEY_USAGE_DERIVE flag, or it does not
 *         permit the \p operation's algorithm.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         The key type for \p password is not #PSA_KEY_TYPE_PASSWORD or
 *         #PSA_KEY_TYPE_PASSWORD_HASH, or \p password is not compatible with
 *         the \p operation's cipher suite.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         The key type or key size of \p password is not supported with the
 *         \p operation's cipher suite.
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_STORAGE_FAILURE
 * \retval #PSA_ERROR_DATA_CORRUPT
 * \retval #PSA_ERROR_DATA_INVALID
 * \retval #PSA_ERROR_BAD_STATE
 *         The operation state is not valid (it must have been set up.), or
 *         the library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_set_password_key( psa_pake_operation_t *operation,
                                        mbedtls_svc_key_id_t password );

/** Set the user ID for a password-authenticated key exchange.
 *
 * Call this function to set the user ID. For PAKE algorithms that associate a
 * user identifier with each side of the session you need to call
 * psa_pake_set_peer() as well. For PAKE algorithms that associate a single
 * user identifier with the session, call psa_pake_set_user() only.
 *
 * Refer to the documentation of individual PAKE algorithm types (`PSA_ALG_XXX`
 * values of type ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true)
 * for more information.
 *
 * \param[in,out] operation     The operation object to set the user ID for. It
 *                              must have been set up by psa_pake_setup() and
 *                              not yet in use (neither psa_pake_output() nor
 *                              psa_pake_input() has been called yet). It must
 *                              be on operation for which the user ID hasn't
 *                              been set (psa_pake_set_user() hasn't been
 *                              called yet).
 * \param[in] user_id           The user ID to authenticate with.
 * \param user_id_len           Size of the \p user_id buffer in bytes.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         \p user_id is not valid for the \p operation's algorithm and cipher
 *         suite.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         The value of \p user_id is not supported by the implementation.
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_BAD_STATE
 *         The operation state is not valid, or
 *         the library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_set_user( psa_pake_operation_t *operation,
                                const uint8_t *user_id,
                                size_t user_id_len );

/** Set the peer ID for a password-authenticated key exchange.
 *
 * Call this function in addition to psa_pake_set_user() for PAKE algorithms
 * that associate a user identifier with each side of the session. For PAKE
 * algorithms that associate a single user identifier with the session, call
 * psa_pake_set_user() only.
 *
 * Refer to the documentation of individual PAKE algorithm types (`PSA_ALG_XXX`
 * values of type ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true)
 * for more information.
 *
 * \param[in,out] operation     The operation object to set the peer ID for. It
 *                              must have been set up by psa_pake_setup() and
 *                              not yet in use (neither psa_pake_output() nor
 *                              psa_pake_input() has been called yet). It must
 *                              be on operation for which the peer ID hasn't
 *                              been set (psa_pake_set_peer() hasn't been
 *                              called yet).
 * \param[in] peer_id           The peer's ID to authenticate.
 * \param peer_id_len           Size of the \p peer_id buffer in bytes.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         \p user_id is not valid for the \p operation's algorithm and cipher
 *         suite.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         The algorithm doesn't associate a second identity with the session.
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_BAD_STATE
 *         Calling psa_pake_set_peer() is invalid with the \p operation's
 *         algorithm, the operation state is not valid, or the library has not
 *         been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_set_peer( psa_pake_operation_t *operation,
                                const uint8_t *peer_id,
                                size_t peer_id_len );

/** Set the application role for a password-authenticated key exchange.
 *
 * Not all PAKE algorithms need to differentiate the communicating entities.
 * It is optional to call this function for PAKEs that don't require a role
 * to be specified. For such PAKEs the application role parameter is ignored,
 * or #PSA_PAKE_ROLE_NONE can be passed as \c role.
 *
 * Refer to the documentation of individual PAKE algorithm types (`PSA_ALG_XXX`
 * values of type ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true)
 * for more information.
 *
 * \param[in,out] operation     The operation object to specify the
 *                              application's role for. It must have been set up
 *                              by psa_pake_setup() and not yet in use (neither
 *                              psa_pake_output() nor psa_pake_input() has been
 *                              called yet). It must be on operation for which
 *                              the application's role hasn't been specified
 *                              (psa_pake_set_role() hasn't been called yet).
 * \param role                  A value of type ::psa_pake_role_t indicating the
 *                              application's role in the PAKE the algorithm
 *                              that is being set up. For more information see
 *                              the documentation of \c PSA_PAKE_ROLE_XXX
 *                              constants.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         The \p role is not a valid PAKE role in the \p operation’s algorithm.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         The \p role for this algorithm is not supported or is not valid.
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_BAD_STATE
 *         The operation state is not valid, or
 *         the library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_set_role( psa_pake_operation_t *operation,
                                psa_pake_role_t role );

/** Get output for a step of a password-authenticated key exchange.
 *
 * Depending on the algorithm being executed, you might need to call this
 * function several times or you might not need to call this at all.
 *
 * The exact sequence of calls to perform a password-authenticated key
 * exchange depends on the algorithm in use.  Refer to the documentation of
 * individual PAKE algorithm types (`PSA_ALG_XXX` values of type
 * ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true) for more
 * information.
 *
 * If this function returns an error status, the operation enters an error
 * state and must be aborted by calling psa_pake_abort().
 *
 * \param[in,out] operation    Active PAKE operation.
 * \param step                 The step of the algorithm for which the output is
 *                             requested.
 * \param[out] output          Buffer where the output is to be written in the
 *                             format appropriate for this \p step. Refer to
 *                             the documentation of the individual
 *                             \c PSA_PAKE_STEP_XXX constants for more
 *                             information.
 * \param output_size          Size of the \p output buffer in bytes. This must
 *                             be at least #PSA_PAKE_OUTPUT_SIZE(\p alg, \p
 *                             primitive, \p step) where \p alg and
 *                             \p primitive are the PAKE algorithm and primitive
 *                             in the operation's cipher suite, and \p step is
 *                             the output step.
 *
 * \param[out] output_length   On success, the number of bytes of the returned
 *                             output.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_BUFFER_TOO_SMALL
 *         The size of the \p output buffer is too small.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         \p step is not compatible with the operation's algorithm.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         \p step is not supported with the operation's algorithm.
 * \retval #PSA_ERROR_INSUFFICIENT_ENTROPY
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_STORAGE_FAILURE
 * \retval #PSA_ERROR_DATA_CORRUPT
 * \retval #PSA_ERROR_DATA_INVALID
 * \retval #PSA_ERROR_BAD_STATE
 *         The operation state is not valid (it must be active, and fully set
 *         up, and this call must conform to the algorithm's requirements
 *         for ordering of input and output steps), or
 *         the library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_output( psa_pake_operation_t *operation,
                              psa_pake_step_t step,
                              uint8_t *output,
                              size_t output_size,
                              size_t *output_length );

/** Provide input for a step of a password-authenticated key exchange.
 *
 * Depending on the algorithm being executed, you might need to call this
 * function several times or you might not need to call this at all.
 *
 * The exact sequence of calls to perform a password-authenticated key
 * exchange depends on the algorithm in use.  Refer to the documentation of
 * individual PAKE algorithm types (`PSA_ALG_XXX` values of type
 * ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true) for more
 * information.
 *
 * If this function returns an error status, the operation enters an error
 * state and must be aborted by calling psa_pake_abort().
 *
 * \param[in,out] operation    Active PAKE operation.
 * \param step                 The step for which the input is provided.
 * \param[in] input            Buffer containing the input in the format
 *                             appropriate for this \p step. Refer to the
 *                             documentation of the individual
 *                             \c PSA_PAKE_STEP_XXX constants for more
 *                             information.
 * \param input_length         Size of the \p input buffer in bytes.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_SIGNATURE
 *         The verification fails for a #PSA_PAKE_STEP_ZK_PROOF input step.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         \p is not compatible with the \p operation’s algorithm, or the
 *         \p input is not valid for the \p operation's algorithm, cipher suite
 *         or \p step.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         \p step p is not supported with the \p operation's algorithm, or the
 *         \p input is not supported for the \p operation's algorithm, cipher
 *         suite or \p step.
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_STORAGE_FAILURE
 * \retval #PSA_ERROR_DATA_CORRUPT
 * \retval #PSA_ERROR_DATA_INVALID
 * \retval #PSA_ERROR_BAD_STATE
 *         The operation state is not valid (it must be active, and fully set
 *         up, and this call must conform to the algorithm's requirements
 *         for ordering of input and output steps), or
 *         the library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_input( psa_pake_operation_t *operation,
                             psa_pake_step_t step,
                             const uint8_t *input,
                             size_t input_length );

/** Get implicitly confirmed shared secret from a PAKE.
 *
 * At this point there is a cryptographic guarantee that only the authenticated
 * party who used the same password is able to compute the key. But there is no
 * guarantee that the peer is the party it claims to be and was able to do so.
 *
 * That is, the authentication is only implicit. Since the peer is not
 * authenticated yet, no action should be taken yet that assumes that the peer
 * is who it claims to be. For example, do not access restricted files on the
 * peer's behalf until an explicit authentication has succeeded.
 *
 * This function can be called after the key exchange phase of the operation
 * has completed. It imports the shared secret output of the PAKE into the
 * provided derivation operation. The input step
 * #PSA_KEY_DERIVATION_INPUT_SECRET is used when placing the shared key
 * material in the key derivation operation.
 *
 * The exact sequence of calls to perform a password-authenticated key
 * exchange depends on the algorithm in use.  Refer to the documentation of
 * individual PAKE algorithm types (`PSA_ALG_XXX` values of type
 * ::psa_algorithm_t such that #PSA_ALG_IS_PAKE(\c alg) is true) for more
 * information.
 *
 * When this function returns successfully, \p operation becomes inactive.
 * If this function returns an error status, both \p operation
 * and \p key_derivation operations enter an error state and must be aborted by
 * calling psa_pake_abort() and psa_key_derivation_abort() respectively.
 *
 * \param[in,out] operation    Active PAKE operation.
 * \param[out] output          A key derivation operation that is ready
 *                             for an input step of type
 *                             #PSA_KEY_DERIVATION_INPUT_SECRET.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_INVALID_ARGUMENT
 *         #PSA_KEY_DERIVATION_INPUT_SECRET is not compatible with the
 *         algorithm in the \p output key derivation operation.
 * \retval #PSA_ERROR_NOT_SUPPORTED
 *         Input from a PAKE is not supported by the algorithm in the \p output
 *         key derivation operation.
 * \retval #PSA_ERROR_INSUFFICIENT_MEMORY
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_STORAGE_FAILURE
 * \retval #PSA_ERROR_DATA_CORRUPT
 * \retval #PSA_ERROR_DATA_INVALID
 * \retval #PSA_ERROR_BAD_STATE
 *         The PAKE operation state is not valid (it must be active, but beyond
 *         that validity is specific to the algorithm), or
 *         the library has not been previously initialized by psa_crypto_init(),
 *         or the state of \p output is not valid for
 *         the #PSA_KEY_DERIVATION_INPUT_SECRET step. This can happen if the
 *         step is out of order or the application has done this step already
 *         and it may not be repeated.
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_get_implicit_key( psa_pake_operation_t *operation,
                                        psa_key_derivation_operation_t *output );

/** Abort a PAKE operation.
 *
 * Aborting an operation frees all associated resources except for the \c
 * operation structure itself. Once aborted, the operation object can be reused
 * for another operation by calling psa_pake_setup() again.
 *
 * This function may be called at any time after the operation
 * object has been initialized as described in #psa_pake_operation_t.
 *
 * In particular, calling psa_pake_abort() after the operation has been
 * terminated by a call to psa_pake_abort() or psa_pake_get_implicit_key()
 * is safe and has no effect.
 *
 * \param[in,out] operation    The operation to abort.
 *
 * \retval #PSA_SUCCESS
 *         Success.
 * \retval #PSA_ERROR_COMMUNICATION_FAILURE
 * \retval #PSA_ERROR_CORRUPTION_DETECTED
 * \retval #PSA_ERROR_BAD_STATE
 *         The library has not been previously initialized by psa_crypto_init().
 *         It is implementation-dependent whether a failure to initialize
 *         results in this error code.
 */
psa_status_t psa_pake_abort( psa_pake_operation_t * operation );

/**@}*/

/** A sufficient output buffer size for psa_pake_output().
 *
 * If the size of the output buffer is at least this large, it is guaranteed
 * that psa_pake_output() will not fail due to an insufficient output buffer
 * size. The actual size of the output might be smaller in any given call.
 *
 * See also #PSA_PAKE_OUTPUT_MAX_SIZE
 *
 * \param alg           A PAKE algorithm (\c PSA_ALG_XXX value such that
 *                      #PSA_ALG_IS_PAKE(\p alg) is true).
 * \param primitive     A primitive of type ::psa_pake_primitive_t that is
 *                      compatible with algorithm \p alg.
 * \param output_step   A value of type ::psa_pake_step_t that is valid for the
 *                      algorithm \p alg.
 * \return              A sufficient output buffer size for the specified
 *                      PAKE algorithm, primitive, and output step. If the
 *                      PAKE algorithm, primitive, or output step is not
 *                      recognized, or the parameters are incompatible,
 *                      return 0.
 */
#define PSA_PAKE_OUTPUT_SIZE(alg, primitive, output_step)               \
    ( alg == PSA_ALG_JPAKE &&                                           \
      primitive == PSA_PAKE_PRIMITIVE(PSA_PAKE_PRIMITIVE_TYPE_ECC,      \
                                      PSA_ECC_FAMILY_SECP_R1, 256) ?    \
      (                                                                 \
        output_step == PSA_PAKE_STEP_KEY_SHARE ? 65 :                   \
        output_step == PSA_PAKE_STEP_ZK_PUBLIC ? 65 :                   \
        32                                                              \
      ) :                                                               \
      0 )

/** A sufficient input buffer size for psa_pake_input().
 *
 * The value returned by this macro is guaranteed to be large enough for any
 * valid input to psa_pake_input() in an operation with the specified
 * parameters.
 *
 * See also #PSA_PAKE_INPUT_MAX_SIZE
 *
 * \param alg           A PAKE algorithm (\c PSA_ALG_XXX value such that
 *                      #PSA_ALG_IS_PAKE(\p alg) is true).
 * \param primitive     A primitive of type ::psa_pake_primitive_t that is
 *                      compatible with algorithm \p alg.
 * \param input_step    A value of type ::psa_pake_step_t that is valid for the
 *                      algorithm \p alg.
 * \return              A sufficient input buffer size for the specified
 *                      input, cipher suite and algorithm. If the cipher suite,
 *                      the input type or PAKE algorithm is not recognized, or
 *                      the parameters are incompatible, return 0.
 */
#define PSA_PAKE_INPUT_SIZE(alg, primitive, input_step)                 \
    ( alg == PSA_ALG_JPAKE &&                                           \
      primitive == PSA_PAKE_PRIMITIVE(PSA_PAKE_PRIMITIVE_TYPE_ECC,      \
                                      PSA_ECC_FAMILY_SECP_R1, 256) ?    \
      (                                                                 \
        input_step == PSA_PAKE_STEP_KEY_SHARE ? 65 :                    \
        input_step == PSA_PAKE_STEP_ZK_PUBLIC ? 65 :                    \
        32                                                              \
      ) :                                                               \
      0 )

/** Output buffer size for psa_pake_output() for any of the supported PAKE
 * algorithm and primitive suites and output step.
 *
 * This macro must expand to a compile-time constant integer.
 *
 * See also #PSA_PAKE_OUTPUT_SIZE(\p alg, \p primitive, \p step).
 */
#define PSA_PAKE_OUTPUT_MAX_SIZE 65

/** Input buffer size for psa_pake_input() for any of the supported PAKE
 * algorithm and primitive suites and input step.
 *
 * This macro must expand to a compile-time constant integer.
 *
 * See also #PSA_PAKE_INPUT_SIZE(\p alg, \p primitive, \p step).
 */
#define PSA_PAKE_INPUT_MAX_SIZE 65

/** Returns a suitable initializer for a PAKE cipher suite object of type
 * psa_pake_cipher_suite_t.
 */
#define PSA_PAKE_CIPHER_SUITE_INIT {PSA_ALG_NONE, 0, 0, 0, PSA_ALG_NONE}

/** Returns a suitable initializer for a PAKE operation object of type
 * psa_pake_operation_t.
 */
#if defined(MBEDTLS_PSA_BUILTIN_PAKE)
#define PSA_PAKE_OPERATION_INIT {PSA_ALG_NONE, 0, 0, 0, 0,              \
                                 NULL, 0                ,               \
                                 PSA_PAKE_ROLE_NONE, {0}, 0, 0,         \
                                 {.dummy = 0}}
#else
#define PSA_PAKE_OPERATION_INIT {PSA_ALG_NONE, 0, 0, {0}}
#endif

struct psa_pake_cipher_suite_s
{
    psa_algorithm_t algorithm;
    psa_pake_primitive_type_t type;
    psa_pake_family_t family;
    uint16_t  bits;
    psa_algorithm_t hash;
};

static inline psa_algorithm_t psa_pake_cs_get_algorithm(
                        const psa_pake_cipher_suite_t *cipher_suite )
{
    return( cipher_suite->algorithm );
}

static inline void psa_pake_cs_set_algorithm(
    psa_pake_cipher_suite_t *cipher_suite,
    psa_algorithm_t algorithm)
{
    if( !PSA_ALG_IS_PAKE( algorithm ) )
        cipher_suite->algorithm = 0;
    else
        cipher_suite->algorithm = algorithm;
}

static inline psa_pake_primitive_t psa_pake_cs_get_primitive(
                        const psa_pake_cipher_suite_t *cipher_suite )
{
    return( PSA_PAKE_PRIMITIVE( cipher_suite->type, cipher_suite->family,
                                cipher_suite->bits ) );
}

static inline void psa_pake_cs_set_primitive(
                        psa_pake_cipher_suite_t *cipher_suite,
                        psa_pake_primitive_t primitive )
{
    cipher_suite->type = (psa_pake_primitive_type_t) (primitive >> 24);
    cipher_suite->family = (psa_pake_family_t) (0xFF & (primitive >> 16));
    cipher_suite->bits = (uint16_t) (0xFFFF & primitive);
}

static inline psa_pake_family_t psa_pake_cs_get_family(
                        const psa_pake_cipher_suite_t *cipher_suite )
{
    return( cipher_suite->family );
}

static inline uint16_t psa_pake_cs_get_bits(
                        const psa_pake_cipher_suite_t *cipher_suite )
{
    return( cipher_suite->bits );
}

static inline psa_algorithm_t psa_pake_cs_get_hash(
                        const psa_pake_cipher_suite_t *cipher_suite )
{
    return( cipher_suite->hash );
}

static inline void psa_pake_cs_set_hash( psa_pake_cipher_suite_t *cipher_suite,
                                         psa_algorithm_t hash )
{
    if( !PSA_ALG_IS_HASH( hash ) )
        cipher_suite->hash = 0;
    else
        cipher_suite->hash = hash;
}

#if defined(MBEDTLS_PSA_BUILTIN_ALG_JPAKE)
#include <mbedtls/ecjpake.h>
/* Note: the format for mbedtls_ecjpake_read/write function has an extra
 * length byte for each step, plus an extra 3 bytes for ECParameters in the
 * server's 2nd round. */
#define MBEDTLS_PSA_PAKE_BUFFER_SIZE ( ( 3 + 1 + 65 + 1 + 65 + 1 + 32 ) * 2 )
#endif

struct psa_pake_operation_s
{
    psa_algorithm_t MBEDTLS_PRIVATE(alg);
    unsigned int MBEDTLS_PRIVATE(state);
    unsigned int MBEDTLS_PRIVATE(sequence);
#if defined(MBEDTLS_PSA_BUILTIN_PAKE)
    unsigned int MBEDTLS_PRIVATE(input_step);
    unsigned int MBEDTLS_PRIVATE(output_step);
    uint8_t* MBEDTLS_PRIVATE(password);
    size_t MBEDTLS_PRIVATE(password_len);
    psa_pake_role_t MBEDTLS_PRIVATE(role);
    uint8_t MBEDTLS_PRIVATE(buffer[MBEDTLS_PSA_PAKE_BUFFER_SIZE]);
    size_t MBEDTLS_PRIVATE(buffer_length);
    size_t MBEDTLS_PRIVATE(buffer_offset);
#endif
    union
    {
#if defined(MBEDTLS_PSA_BUILTIN_ALG_JPAKE)
        mbedtls_ecjpake_context ecjpake;
#endif
        /* Make the union non-empty even with no supported algorithms. */
        uint8_t dummy;
    } MBEDTLS_PRIVATE(ctx);
};

static inline struct psa_pake_cipher_suite_s psa_pake_cipher_suite_init( void )
{
    const struct psa_pake_cipher_suite_s v = PSA_PAKE_CIPHER_SUITE_INIT;
    return( v );
}

static inline struct psa_pake_operation_s psa_pake_operation_init( void )
{
    const struct psa_pake_operation_s v = PSA_PAKE_OPERATION_INIT;
    return( v );
}

#ifdef __cplusplus
}
#endif

#endif /* PSA_CRYPTO_EXTRA_H */
