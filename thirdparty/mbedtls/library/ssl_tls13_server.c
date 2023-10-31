/*
 *  TLS 1.3 server-side functions
 *
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

#include "common.h"

#if defined(MBEDTLS_SSL_SRV_C) && defined(MBEDTLS_SSL_PROTO_TLS1_3)

#include "mbedtls/debug.h"
#include "mbedtls/error.h"
#include "mbedtls/platform.h"

#include "ssl_misc.h"
#include "ssl_tls13_keys.h"
#include "ssl_debug_helpers.h"

#if defined(MBEDTLS_ECP_C)
#include "mbedtls/ecp.h"
#endif /* MBEDTLS_ECP_C */

#if defined(MBEDTLS_PLATFORM_C)
#include "mbedtls/platform.h"
#else
#include <stdlib.h>
#define mbedtls_calloc    calloc
#define mbedtls_free       free
#endif /* MBEDTLS_PLATFORM_C */

#include "ssl_misc.h"
#include "ssl_tls13_keys.h"
#include "ssl_debug_helpers.h"

/* From RFC 8446:
 *   struct {
 *          ProtocolVersion versions<2..254>;
 *   } SupportedVersions;
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_parse_supported_versions_ext( mbedtls_ssl_context *ssl,
                                                   const unsigned char *buf,
                                                   const unsigned char *end )
{
    const unsigned char *p = buf;
    size_t versions_len;
    const unsigned char *versions_end;
    uint16_t tls_version;
    int tls13_supported = 0;

    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, 1 );
    versions_len = p[0];
    p += 1;

    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, versions_len );
    versions_end = p + versions_len;
    while( p < versions_end )
    {
        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, versions_end, 2 );
        tls_version = mbedtls_ssl_read_version( p, ssl->conf->transport );
        p += 2;

        /* In this implementation we only support TLS 1.3 and DTLS 1.3. */
        if( tls_version == MBEDTLS_SSL_VERSION_TLS1_3 )
        {
            tls13_supported = 1;
            break;
        }
    }

    if( !tls13_supported )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "TLS 1.3 is not supported by the client" ) );

        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_PROTOCOL_VERSION,
                                      MBEDTLS_ERR_SSL_BAD_PROTOCOL_VERSION );
        return( MBEDTLS_ERR_SSL_BAD_PROTOCOL_VERSION );
    }

    MBEDTLS_SSL_DEBUG_MSG( 1, ( "Negotiated version. Supported is [%04x]",
                              (unsigned int)tls_version ) );

    return( 0 );
}

#if defined(MBEDTLS_ECDH_C)
/*
 *
 * From RFC 8446:
 *   enum {
 *       ... (0xFFFF)
 *   } NamedGroup;
 *   struct {
 *       NamedGroup named_group_list<2..2^16-1>;
 *   } NamedGroupList;
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_parse_supported_groups_ext( mbedtls_ssl_context *ssl,
                                                 const unsigned char *buf,
                                                 const unsigned char *end )
{
    const unsigned char *p = buf;
    size_t named_group_list_len;
    const unsigned char *named_group_list_end;

    MBEDTLS_SSL_DEBUG_BUF( 3, "supported_groups extension", p, end - buf );
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, 2 );
    named_group_list_len = MBEDTLS_GET_UINT16_BE( p, 0 );
    p += 2;
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, named_group_list_len );
    named_group_list_end = p + named_group_list_len;
    ssl->handshake->hrr_selected_group = 0;

    while( p < named_group_list_end )
    {
        uint16_t named_group;
        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, named_group_list_end, 2 );
        named_group = MBEDTLS_GET_UINT16_BE( p, 0 );
        p += 2;

        MBEDTLS_SSL_DEBUG_MSG( 2,
                               ( "got named group: %s(%04x)",
                                 mbedtls_ssl_named_group_to_str( named_group ),
                                 named_group ) );

        if( ! mbedtls_ssl_named_group_is_offered( ssl, named_group ) ||
            ! mbedtls_ssl_named_group_is_supported( named_group ) ||
            ssl->handshake->hrr_selected_group != 0 )
        {
            continue;
        }

        MBEDTLS_SSL_DEBUG_MSG( 2,
                               ( "add named group %s(%04x) into received list.",
                                 mbedtls_ssl_named_group_to_str( named_group ),
                                 named_group ) );

        ssl->handshake->hrr_selected_group = named_group;
    }

    return( 0 );

}
#endif /* MBEDTLS_ECDH_C */

#define SSL_TLS1_3_PARSE_KEY_SHARES_EXT_NO_MATCH 1

#if defined(MBEDTLS_ECDH_C)
/*
 *  ssl_tls13_parse_key_shares_ext() verifies whether the information in the
 *  extension is correct and stores the first acceptable key share and its associated group.
 *
 *  Possible return values are:
 *  - 0: Successful processing of the client provided key share extension.
 *  - SSL_TLS1_3_PARSE_KEY_SHARES_EXT_NO_MATCH: The key shares provided by the client
 *    does not match a group supported by the server. A HelloRetryRequest will
 *    be needed.
 *  - A negative value for fatal errors.
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_parse_key_shares_ext( mbedtls_ssl_context *ssl,
                                           const unsigned char *buf,
                                           const unsigned char *end )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char const *p = buf;
    unsigned char const *client_shares_end;
    size_t client_shares_len;

    /* From RFC 8446:
     *
     * struct {
     *     KeyShareEntry client_shares<0..2^16-1>;
     * } KeyShareClientHello;
     *
     */

    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, 2 );
    client_shares_len = MBEDTLS_GET_UINT16_BE( p, 0 );
    p += 2;
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, client_shares_len );

    ssl->handshake->offered_group_id = 0;
    client_shares_end = p + client_shares_len;

    /* We try to find a suitable key share entry and copy it to the
     * handshake context. Later, we have to find out whether we can do
     * something with the provided key share or whether we have to
     * dismiss it and send a HelloRetryRequest message.
     */

    while( p < client_shares_end )
    {
        uint16_t group;
        size_t key_exchange_len;
        const unsigned char *key_exchange;

        /*
         * struct {
         *    NamedGroup group;
         *    opaque key_exchange<1..2^16-1>;
         * } KeyShareEntry;
         */
        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, client_shares_end, 4 );
        group = MBEDTLS_GET_UINT16_BE( p, 0 );
        key_exchange_len = MBEDTLS_GET_UINT16_BE( p, 2 );
        p += 4;
        key_exchange = p;
        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, client_shares_end, key_exchange_len );
        p += key_exchange_len;

        /* Continue parsing even if we have already found a match,
         * for input validation purposes.
         */
        if( ! mbedtls_ssl_named_group_is_offered( ssl, group ) ||
            ! mbedtls_ssl_named_group_is_supported( group ) ||
            ssl->handshake->offered_group_id != 0 )
        {
            continue;
        }

        /*
         * For now, we only support ECDHE groups.
         */
        if( mbedtls_ssl_tls13_named_group_is_ecdhe( group ) )
        {
            MBEDTLS_SSL_DEBUG_MSG( 2, ( "ECDH group: %s (%04x)",
                                        mbedtls_ssl_named_group_to_str( group ),
                                        group ) );
            ret = mbedtls_ssl_tls13_read_public_ecdhe_share(
                      ssl, key_exchange - 2, key_exchange_len + 2 );
            if( ret != 0 )
                return( ret );

        }
        else
        {
            MBEDTLS_SSL_DEBUG_MSG( 4, ( "Unrecognized NamedGroup %u",
                                        (unsigned) group ) );
            continue;
        }

        ssl->handshake->offered_group_id = group;
    }


    if( ssl->handshake->offered_group_id == 0 )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "no matching key share" ) );
        return( SSL_TLS1_3_PARSE_KEY_SHARES_EXT_NO_MATCH );
    }
    return( 0 );
}
#endif /* MBEDTLS_ECDH_C */

#if defined(MBEDTLS_DEBUG_C)
static void ssl_tls13_debug_print_client_hello_exts( mbedtls_ssl_context *ssl )
{
    ((void) ssl);

    MBEDTLS_SSL_DEBUG_MSG( 3, ( "Supported Extensions:" ) );
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- KEY_SHARE_EXTENSION ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_KEY_SHARE ) > 0 ) ? "TRUE" : "FALSE" ) );
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- PSK_KEY_EXCHANGE_MODES_EXTENSION ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_PSK_KEY_EXCHANGE_MODES ) > 0 ) ?
                "TRUE" : "FALSE" ) );
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- PRE_SHARED_KEY_EXTENSION ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_PRE_SHARED_KEY ) > 0 ) ? "TRUE" : "FALSE" ) );
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- SIGNATURE_ALGORITHM_EXTENSION ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_SIG_ALG ) > 0 ) ? "TRUE" : "FALSE" ) );
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- SUPPORTED_GROUPS_EXTENSION ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_SUPPORTED_GROUPS ) >0 ) ?
                "TRUE" : "FALSE" ) );
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- SUPPORTED_VERSION_EXTENSION ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_SUPPORTED_VERSIONS ) > 0 ) ?
                "TRUE" : "FALSE" ) );
#if defined ( MBEDTLS_SSL_SERVER_NAME_INDICATION )
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- SERVERNAME_EXTENSION    ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_SERVERNAME ) > 0 ) ?
                "TRUE" : "FALSE" ) );
#endif /* MBEDTLS_SSL_SERVER_NAME_INDICATION */
#if defined ( MBEDTLS_SSL_ALPN )
    MBEDTLS_SSL_DEBUG_MSG( 3,
            ( "- ALPN_EXTENSION   ( %s )",
            ( ( ssl->handshake->extensions_present
                & MBEDTLS_SSL_EXT_ALPN ) > 0 ) ?
                "TRUE" : "FALSE" ) );
#endif /* MBEDTLS_SSL_ALPN */
}
#endif /* MBEDTLS_DEBUG_C */

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_client_hello_has_exts( mbedtls_ssl_context *ssl,
                                            int exts_mask )
{
    int masked = ssl->handshake->extensions_present & exts_mask;
    return( masked == exts_mask );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_client_hello_has_exts_for_ephemeral_key_exchange(
        mbedtls_ssl_context *ssl )
{
    return( ssl_tls13_client_hello_has_exts( ssl,
                          MBEDTLS_SSL_EXT_SUPPORTED_GROUPS |
                          MBEDTLS_SSL_EXT_KEY_SHARE        |
                          MBEDTLS_SSL_EXT_SIG_ALG ) );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_check_ephemeral_key_exchange( mbedtls_ssl_context *ssl )
{
    if( !mbedtls_ssl_conf_tls13_ephemeral_enabled( ssl ) )
        return( 0 );

    if( !ssl_tls13_client_hello_has_exts_for_ephemeral_key_exchange( ssl ) )
        return( 0 );

    ssl->handshake->tls13_kex_modes =
        MBEDTLS_SSL_TLS1_3_KEY_EXCHANGE_MODE_EPHEMERAL;
    return( 1 );
}

#if defined(MBEDTLS_X509_CRT_PARSE_C) && \
    defined(MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED)
/*
 * Pick best ( private key, certificate chain ) pair based on the signature
 * algorithms supported by the client.
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_pick_key_cert( mbedtls_ssl_context *ssl )
{
    mbedtls_ssl_key_cert *key_cert, *key_cert_list;
    const uint16_t *sig_alg = ssl->handshake->received_sig_algs;

#if defined(MBEDTLS_SSL_SERVER_NAME_INDICATION)
    if( ssl->handshake->sni_key_cert != NULL )
        key_cert_list = ssl->handshake->sni_key_cert;
    else
#endif /* MBEDTLS_SSL_SERVER_NAME_INDICATION */
        key_cert_list = ssl->conf->key_cert;

    if( key_cert_list == NULL )
    {
        MBEDTLS_SSL_DEBUG_MSG( 3, ( "server has no certificate" ) );
        return( -1 );
    }

    for( ; *sig_alg != MBEDTLS_TLS1_3_SIG_NONE; sig_alg++ )
    {
        for( key_cert = key_cert_list; key_cert != NULL;
             key_cert = key_cert->next )
        {
            MBEDTLS_SSL_DEBUG_CRT( 3, "certificate (chain) candidate",
                                   key_cert->cert );

            /*
            * This avoids sending the client a cert it'll reject based on
            * keyUsage or other extensions.
            */
            if( mbedtls_x509_crt_check_key_usage(
                    key_cert->cert, MBEDTLS_X509_KU_DIGITAL_SIGNATURE ) != 0 ||
                mbedtls_x509_crt_check_extended_key_usage(
                    key_cert->cert, MBEDTLS_OID_SERVER_AUTH,
                    MBEDTLS_OID_SIZE( MBEDTLS_OID_SERVER_AUTH ) ) != 0 )
            {
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "certificate mismatch: "
                                       "(extended) key usage extension" ) );
                continue;
            }

            MBEDTLS_SSL_DEBUG_MSG( 3,
                                   ( "ssl_tls13_pick_key_cert:"
                                     "check signature algorithm %s [%04x]",
                                     mbedtls_ssl_sig_alg_to_str( *sig_alg ),
                                     *sig_alg ) );
            if( mbedtls_ssl_tls13_check_sig_alg_cert_key_match(
                                            *sig_alg, &key_cert->cert->pk ) )
            {
                ssl->handshake->key_cert = key_cert;
                MBEDTLS_SSL_DEBUG_MSG( 3,
                                       ( "ssl_tls13_pick_key_cert:"
                                         "selected signature algorithm"
                                         " %s [%04x]",
                                         mbedtls_ssl_sig_alg_to_str( *sig_alg ),
                                         *sig_alg ) );
                MBEDTLS_SSL_DEBUG_CRT(
                        3, "selected certificate (chain)",
                        ssl->handshake->key_cert->cert );
                return( 0 );
            }
        }
    }

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "ssl_tls13_pick_key_cert:"
                                "no suitable certificate found" ) );
    return( -1 );
}
#endif /* MBEDTLS_X509_CRT_PARSE_C &&
          MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED */

/*
 *
 * STATE HANDLING: ClientHello
 *
 * There are three possible classes of outcomes when parsing the ClientHello:
 *
 * 1) The ClientHello was well-formed and matched the server's configuration.
 *
 *    In this case, the server progresses to sending its ServerHello.
 *
 * 2) The ClientHello was well-formed but didn't match the server's
 *    configuration.
 *
 *    For example, the client might not have offered a key share which
 *    the server supports, or the server might require a cookie.
 *
 *    In this case, the server sends a HelloRetryRequest.
 *
 * 3) The ClientHello was ill-formed
 *
 *    In this case, we abort the handshake.
 *
 */

/*
 * Structure of this message:
 *
 * uint16 ProtocolVersion;
 * opaque Random[32];
 * uint8 CipherSuite[2];    // Cryptographic suite selector
 *
 * struct {
 *      ProtocolVersion legacy_version = 0x0303;    // TLS v1.2
 *      Random random;
 *      opaque legacy_session_id<0..32>;
 *      CipherSuite cipher_suites<2..2^16-2>;
 *      opaque legacy_compression_methods<1..2^8-1>;
 *      Extension extensions<8..2^16-1>;
 * } ClientHello;
 */

#define SSL_CLIENT_HELLO_OK           0
#define SSL_CLIENT_HELLO_HRR_REQUIRED 1

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_parse_client_hello( mbedtls_ssl_context *ssl,
                                         const unsigned char *buf,
                                         const unsigned char *end )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    const unsigned char *p = buf;
    size_t legacy_session_id_len;
    const unsigned char *cipher_suites;
    size_t cipher_suites_len;
    const unsigned char *cipher_suites_end;
    size_t extensions_len;
    const unsigned char *extensions_end;
    int hrr_required = 0;

    const mbedtls_ssl_ciphersuite_t* ciphersuite_info;

    ssl->handshake->extensions_present = MBEDTLS_SSL_EXT_NONE;

    /*
     * ClientHello layout:
     *     0  .   1   protocol version
     *     2  .  33   random bytes
     *    34  .  34   session id length ( 1 byte )
     *    35  . 34+x  session id
     *    ..  .  ..   ciphersuite list length ( 2 bytes )
     *    ..  .  ..   ciphersuite list
     *    ..  .  ..   compression alg. list length ( 1 byte )
     *    ..  .  ..   compression alg. list
     *    ..  .  ..   extensions length ( 2 bytes, optional )
     *    ..  .  ..   extensions ( optional )
     */

    /*
     * Minimal length ( with everything empty and extensions omitted ) is
     * 2 + 32 + 1 + 2 + 1 = 38 bytes. Check that first, so that we can
     * read at least up to session id length without worrying.
     */
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, 38 );

    /* ...
     * ProtocolVersion legacy_version = 0x0303; // TLS 1.2
     * ...
     * with ProtocolVersion defined as:
     * uint16 ProtocolVersion;
     */
    if( mbedtls_ssl_read_version( p, ssl->conf->transport ) !=
          MBEDTLS_SSL_VERSION_TLS1_2 )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "Unsupported version of TLS." ) );
        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_PROTOCOL_VERSION,
                                      MBEDTLS_ERR_SSL_BAD_PROTOCOL_VERSION );
        return ( MBEDTLS_ERR_SSL_BAD_PROTOCOL_VERSION );
    }
    p += 2;

    /*
     * Only support TLS 1.3 currently, temporarily set the version.
     */
    ssl->tls_version = MBEDTLS_SSL_VERSION_TLS1_3;

    /* ...
     * Random random;
     * ...
     * with Random defined as:
     * opaque Random[32];
     */
    MBEDTLS_SSL_DEBUG_BUF( 3, "client hello, random bytes",
                           p, MBEDTLS_CLIENT_HELLO_RANDOM_LEN );

    memcpy( &ssl->handshake->randbytes[0], p, MBEDTLS_CLIENT_HELLO_RANDOM_LEN );
    p += MBEDTLS_CLIENT_HELLO_RANDOM_LEN;

    /* ...
     * opaque legacy_session_id<0..32>;
     * ...
     */
    legacy_session_id_len = p[0];
    p++;

    if( legacy_session_id_len > sizeof( ssl->session_negotiate->id ) )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "bad client hello message" ) );
        return( MBEDTLS_ERR_SSL_DECODE_ERROR );
    }

    ssl->session_negotiate->id_len = legacy_session_id_len;
    MBEDTLS_SSL_DEBUG_BUF( 3, "client hello, session id",
                           p, legacy_session_id_len );
    /*
     * Check we have enough data for the legacy session identifier
     * and the ciphersuite list  length.
     */
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, legacy_session_id_len + 2 );

    memcpy( &ssl->session_negotiate->id[0], p, legacy_session_id_len );
    p += legacy_session_id_len;

    cipher_suites_len = MBEDTLS_GET_UINT16_BE( p, 0 );
    p += 2;

    /* Check we have enough data for the ciphersuite list, the legacy
     * compression methods and the length of the extensions.
     */
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, cipher_suites_len + 2 + 2 );

   /* ...
    * CipherSuite cipher_suites<2..2^16-2>;
    * ...
    * with CipherSuite defined as:
    * uint8 CipherSuite[2];
    */
    cipher_suites = p;
    cipher_suites_end = p + cipher_suites_len;
    MBEDTLS_SSL_DEBUG_BUF( 3, "client hello, ciphersuitelist",
                          p, cipher_suites_len );
    /*
     * Search for a matching ciphersuite
     */
    int ciphersuite_match = 0;
    for ( ; p < cipher_suites_end; p += 2 )
    {
        uint16_t cipher_suite;
        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, cipher_suites_end, 2 );
        cipher_suite = MBEDTLS_GET_UINT16_BE( p, 0 );
        ciphersuite_info = mbedtls_ssl_ciphersuite_from_id( cipher_suite );
        /*
         * Check whether this ciphersuite is valid and offered.
         */
        if( ( mbedtls_ssl_validate_ciphersuite(
                ssl, ciphersuite_info, ssl->tls_version,
                ssl->tls_version ) != 0 ) ||
            ! mbedtls_ssl_tls13_cipher_suite_is_offered( ssl, cipher_suite ) )
        {
            continue;
        }

        ssl->session_negotiate->ciphersuite = cipher_suite;
        ssl->handshake->ciphersuite_info = ciphersuite_info;
        ciphersuite_match = 1;

        break;

    }

    if( ! ciphersuite_match )
    {
        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_HANDSHAKE_FAILURE,
                                      MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
        return ( MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
    }

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "selected ciphersuite: %s",
                                ciphersuite_info->name ) );

    p = cipher_suites + cipher_suites_len;

    /* ...
     * opaque legacy_compression_methods<1..2^8-1>;
     * ...
     */
    if( p[0] != 1 || p[1] != MBEDTLS_SSL_COMPRESS_NULL )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "bad legacy compression method" ) );
        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_ILLEGAL_PARAMETER,
                                      MBEDTLS_ERR_SSL_ILLEGAL_PARAMETER );
        return ( MBEDTLS_ERR_SSL_ILLEGAL_PARAMETER );
    }
    p += 2;

    /* ...
     * Extension extensions<8..2^16-1>;
     * ...
     * with Extension defined as:
     * struct {
     *    ExtensionType extension_type;
     *    opaque extension_data<0..2^16-1>;
     * } Extension;
     */
    extensions_len = MBEDTLS_GET_UINT16_BE( p, 0 );
    p += 2;
    MBEDTLS_SSL_CHK_BUF_READ_PTR( p, end, extensions_len );
    extensions_end = p + extensions_len;

    MBEDTLS_SSL_DEBUG_BUF( 3, "client hello extensions", p, extensions_len );

    while( p < extensions_end )
    {
        unsigned int extension_type;
        size_t extension_data_len;
        const unsigned char *extension_data_end;

        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, extensions_end, 4 );
        extension_type = MBEDTLS_GET_UINT16_BE( p, 0 );
        extension_data_len = MBEDTLS_GET_UINT16_BE( p, 2 );
        p += 4;

        MBEDTLS_SSL_CHK_BUF_READ_PTR( p, extensions_end, extension_data_len );
        extension_data_end = p + extension_data_len;

        switch( extension_type )
        {
#if defined(MBEDTLS_SSL_SERVER_NAME_INDICATION)
            case MBEDTLS_TLS_EXT_SERVERNAME:
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "found ServerName extension" ) );
                ret = mbedtls_ssl_parse_server_name_ext( ssl, p,
                                                         extension_data_end );
                if( ret != 0 )
                {
                    MBEDTLS_SSL_DEBUG_RET(
                            1, "mbedtls_ssl_parse_servername_ext", ret );
                    return( ret );
                }
                ssl->handshake->extensions_present |= MBEDTLS_SSL_EXT_SERVERNAME;
                break;
#endif /* MBEDTLS_SSL_SERVER_NAME_INDICATION */

#if defined(MBEDTLS_ECDH_C)
            case MBEDTLS_TLS_EXT_SUPPORTED_GROUPS:
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "found supported group extension" ) );

                /* Supported Groups Extension
                 *
                 * When sent by the client, the "supported_groups" extension
                 * indicates the named groups which the client supports,
                 * ordered from most preferred to least preferred.
                 */
                ret = ssl_tls13_parse_supported_groups_ext(
                          ssl, p, extension_data_end );
                if( ret != 0 )
                {
                    MBEDTLS_SSL_DEBUG_RET( 1,
                                "mbedtls_ssl_parse_supported_groups_ext", ret );
                    return( ret );
                }

                ssl->handshake->extensions_present |= MBEDTLS_SSL_EXT_SUPPORTED_GROUPS;
                break;
#endif /* MBEDTLS_ECDH_C */

#if defined(MBEDTLS_ECDH_C)
            case MBEDTLS_TLS_EXT_KEY_SHARE:
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "found key share extension" ) );

                /*
                 * Key Share Extension
                 *
                 * When sent by the client, the "key_share" extension
                 * contains the endpoint's cryptographic parameters for
                 * ECDHE/DHE key establishment methods.
                 */
                ret = ssl_tls13_parse_key_shares_ext(
                          ssl, p, extension_data_end );
                if( ret == SSL_TLS1_3_PARSE_KEY_SHARES_EXT_NO_MATCH )
                {
                    MBEDTLS_SSL_DEBUG_MSG( 2, ( "HRR needed " ) );
                    hrr_required = 1;
                }

                if( ret < 0 )
                {
                    MBEDTLS_SSL_DEBUG_RET(
                        1, "ssl_tls13_parse_key_shares_ext", ret );
                    return( ret );
                }

                ssl->handshake->extensions_present |= MBEDTLS_SSL_EXT_KEY_SHARE;
                break;
#endif /* MBEDTLS_ECDH_C */

            case MBEDTLS_TLS_EXT_SUPPORTED_VERSIONS:
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "found supported versions extension" ) );

                ret = ssl_tls13_parse_supported_versions_ext(
                          ssl, p, extension_data_end );
                if( ret != 0 )
                {
                    MBEDTLS_SSL_DEBUG_RET( 1,
                                ( "ssl_tls13_parse_supported_versions_ext" ), ret );
                    return( ret );
                }
                ssl->handshake->extensions_present |= MBEDTLS_SSL_EXT_SUPPORTED_VERSIONS;
                break;

#if defined(MBEDTLS_SSL_ALPN)
            case MBEDTLS_TLS_EXT_ALPN:
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "found alpn extension" ) );

                ret = mbedtls_ssl_parse_alpn_ext( ssl, p, extension_data_end );
                if( ret != 0 )
                {
                    MBEDTLS_SSL_DEBUG_RET(
                            1, ( "mbedtls_ssl_parse_alpn_ext" ), ret );
                    return( ret );
                }
                ssl->handshake->extensions_present |= MBEDTLS_SSL_EXT_ALPN;
                break;
#endif /* MBEDTLS_SSL_ALPN */

#if defined(MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED)
            case MBEDTLS_TLS_EXT_SIG_ALG:
                MBEDTLS_SSL_DEBUG_MSG( 3, ( "found signature_algorithms extension" ) );

                ret = mbedtls_ssl_parse_sig_alg_ext(
                          ssl, p, extension_data_end );
                if( ret != 0 )
                {
                    MBEDTLS_SSL_DEBUG_MSG( 1,
                    ( "ssl_parse_supported_signature_algorithms_server_ext ( %d )",
                      ret ) );
                    return( ret );
                }
                ssl->handshake->extensions_present |= MBEDTLS_SSL_EXT_SIG_ALG;
                break;
#endif /* MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED */

            default:
                MBEDTLS_SSL_DEBUG_MSG( 3,
                        ( "unknown extension found: %ud ( ignoring )",
                          extension_type ) );
        }

        p += extension_data_len;
    }

    /* Update checksum with either
     * - The entire content of the CH message, if no PSK extension is present
     * - The content up to but excluding the PSK extension, if present.
     */
    mbedtls_ssl_add_hs_msg_to_checksum( ssl, MBEDTLS_SSL_HS_CLIENT_HELLO,
                                        buf, p - buf );

    /* List all the extensions we have received */
#if defined(MBEDTLS_DEBUG_C)
    ssl_tls13_debug_print_client_hello_exts( ssl );
#endif /* MBEDTLS_DEBUG_C */

    return( hrr_required ? SSL_CLIENT_HELLO_HRR_REQUIRED : SSL_CLIENT_HELLO_OK );
}

/* Update the handshake state machine */

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_postprocess_client_hello( mbedtls_ssl_context* ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

    /*
     * Here we only support the ephemeral or (EC)DHE key echange mode
     */
    if( !ssl_tls13_check_ephemeral_key_exchange( ssl ) )
    {
        MBEDTLS_SSL_DEBUG_MSG(
                1,
                ( "ClientHello message misses mandatory extensions." ) );
        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_MISSING_EXTENSION ,
                                      MBEDTLS_ERR_SSL_ILLEGAL_PARAMETER );
        return( MBEDTLS_ERR_SSL_ILLEGAL_PARAMETER );
    }

    /*
     * Server certificate selection
     */
    if( ssl->conf->f_cert_cb && ( ret = ssl->conf->f_cert_cb( ssl ) ) != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET( 1, "f_cert_cb", ret );
        return( ret );
    }
#if defined(MBEDTLS_SSL_SERVER_NAME_INDICATION)
    ssl->handshake->sni_name = NULL;
    ssl->handshake->sni_name_len = 0;
#endif /* MBEDTLS_SSL_SERVER_NAME_INDICATION */

    ret = mbedtls_ssl_tls13_key_schedule_stage_early( ssl );
    if( ret != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET( 1,
             "mbedtls_ssl_tls1_3_key_schedule_stage_early", ret );
        return( ret );
    }

    return( 0 );

}

/*
 * Main entry point from the state machine; orchestrates the otherfunctions.
 */

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_process_client_hello( mbedtls_ssl_context *ssl )
{

    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char* buf = NULL;
    size_t buflen = 0;
    int parse_client_hello_ret;

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "=> parse client hello" ) );

    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_tls13_fetch_handshake_msg(
                          ssl, MBEDTLS_SSL_HS_CLIENT_HELLO,
                          &buf, &buflen ) );

    MBEDTLS_SSL_PROC_CHK_NEG( ssl_tls13_parse_client_hello( ssl, buf,
                                                            buf + buflen ) );
    parse_client_hello_ret = ret; /* Store return value of parse_client_hello,
                                   * only SSL_CLIENT_HELLO_OK or
                                   * SSL_CLIENT_HELLO_HRR_REQUIRED at this
                                   * stage as negative error codes are handled
                                   * by MBEDTLS_SSL_PROC_CHK_NEG. */

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_postprocess_client_hello( ssl ) );

    if( parse_client_hello_ret == SSL_CLIENT_HELLO_OK )
        mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_SERVER_HELLO );
    else
        mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_HELLO_RETRY_REQUEST );

cleanup:

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "<= parse client hello" ) );
    return( ret );
}

/*
 * Handler for MBEDTLS_SSL_SERVER_HELLO
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_prepare_server_hello( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *server_randbytes =
                    ssl->handshake->randbytes + MBEDTLS_CLIENT_HELLO_RANDOM_LEN;
    if( ssl->conf->f_rng == NULL )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "no RNG provided" ) );
        return( MBEDTLS_ERR_SSL_NO_RNG );
    }

    if( ( ret = ssl->conf->f_rng( ssl->conf->p_rng, server_randbytes,
                                  MBEDTLS_SERVER_HELLO_RANDOM_LEN ) ) != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET( 1, "f_rng", ret );
        return( ret );
    }

    MBEDTLS_SSL_DEBUG_BUF( 3, "server hello, random bytes", server_randbytes,
                           MBEDTLS_SERVER_HELLO_RANDOM_LEN );

#if defined(MBEDTLS_HAVE_TIME)
    ssl->session_negotiate->start = time( NULL );
#endif /* MBEDTLS_HAVE_TIME */

    return( ret );
}

/*
 * ssl_tls13_write_server_hello_supported_versions_ext ():
 *
 * struct {
 *      ProtocolVersion selected_version;
 * } SupportedVersions;
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_server_hello_supported_versions_ext(
                                                mbedtls_ssl_context *ssl,
                                                unsigned char *buf,
                                                unsigned char *end,
                                                size_t *out_len )
{
    *out_len = 0;

    MBEDTLS_SSL_DEBUG_MSG( 3, ( "server hello, write selected version" ) );

    /* Check if we have space to write the extension:
     * - extension_type         (2 bytes)
     * - extension_data_length  (2 bytes)
     * - selected_version       (2 bytes)
     */
    MBEDTLS_SSL_CHK_BUF_PTR( buf, end, 6 );

    MBEDTLS_PUT_UINT16_BE( MBEDTLS_TLS_EXT_SUPPORTED_VERSIONS, buf, 0 );

    MBEDTLS_PUT_UINT16_BE( 2, buf, 2 );

    mbedtls_ssl_write_version( buf + 4,
                               ssl->conf->transport,
                               ssl->tls_version );

    MBEDTLS_SSL_DEBUG_MSG( 3, ( "supported version: [%04x]",
                                ssl->tls_version ) );

    *out_len = 6;

    return( 0 );
}



/* Generate and export a single key share. For hybrid KEMs, this can
 * be called multiple times with the different components of the hybrid. */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_generate_and_write_key_share( mbedtls_ssl_context *ssl,
                                                   uint16_t named_group,
                                                   unsigned char *buf,
                                                   unsigned char *end,
                                                   size_t *out_len )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

    *out_len = 0;

#if defined(MBEDTLS_ECDH_C)
    if( mbedtls_ssl_tls13_named_group_is_ecdhe( named_group ) )
    {
        ret = mbedtls_ssl_tls13_generate_and_write_ecdh_key_exchange(
                                        ssl, named_group, buf, end, out_len );
        if( ret != 0 )
        {
            MBEDTLS_SSL_DEBUG_RET(
                1, "mbedtls_ssl_tls13_generate_and_write_ecdh_key_exchange",
                ret );
            return( ret );
        }
    }
    else
#endif /* MBEDTLS_ECDH_C */
    if( 0 /* Other kinds of KEMs */ )
    {
    }
    else
    {
        ((void) ssl);
        ((void) named_group);
        ((void) buf);
        ((void) end);
        ret = MBEDTLS_ERR_SSL_INTERNAL_ERROR;
    }

    return( ret );
}

/*
 * ssl_tls13_write_key_share_ext
 *
 * Structure of key_share extension in ServerHello:
 *
 * struct {
 *     NamedGroup group;
 *     opaque key_exchange<1..2^16-1>;
 * } KeyShareEntry;
 * struct {
 *     KeyShareEntry server_share;
 * } KeyShareServerHello;
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_key_share_ext( mbedtls_ssl_context *ssl,
                                          unsigned char *buf,
                                          unsigned char *end,
                                          size_t *out_len )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *p = buf;
    uint16_t group = ssl->handshake->offered_group_id;
    unsigned char *server_share = buf + 4;
    size_t key_exchange_length;

    *out_len = 0;

    MBEDTLS_SSL_DEBUG_MSG( 3, ( "server hello, adding key share extension" ) );

    /* Check if we have space for header and length fields:
     * - extension_type         (2 bytes)
     * - extension_data_length  (2 bytes)
     * - group                  (2 bytes)
     * - key_exchange_length    (2 bytes)
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 8 );
    MBEDTLS_PUT_UINT16_BE( MBEDTLS_TLS_EXT_KEY_SHARE, p, 0 );
    MBEDTLS_PUT_UINT16_BE( group, server_share, 0 );
    p += 8;

    /* When we introduce PQC-ECDHE hybrids, we'll want to call this
     * function multiple times. */
    ret = ssl_tls13_generate_and_write_key_share(
              ssl, group, server_share + 4, end, &key_exchange_length );
    if( ret != 0 )
        return( ret );
    p += key_exchange_length;

    MBEDTLS_PUT_UINT16_BE( key_exchange_length, server_share + 2, 0 );

    MBEDTLS_PUT_UINT16_BE( p - server_share, buf, 2 );

    *out_len = p - buf;

    return( 0 );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_hrr_key_share_ext( mbedtls_ssl_context *ssl,
                                              unsigned char *buf,
                                              unsigned char *end,
                                              size_t *out_len )
{
    uint16_t selected_group = ssl->handshake->hrr_selected_group;
    /* key_share Extension
     *
     *  struct {
     *    select (Handshake.msg_type) {
     *      ...
     *      case hello_retry_request:
     *          NamedGroup selected_group;
     *      ...
     *    };
     * } KeyShare;
     */

    *out_len = 0;

    /*
     * For a pure PSK key exchange, there is no group to agree upon. The purpose
     * of the HRR is then to transmit a cookie to force the client to demonstrate
     * reachability at their apparent network address (primarily useful for DTLS).
     */
    if( ! mbedtls_ssl_tls13_some_ephemeral_enabled( ssl ) )
        return( 0 );

    /* We should only send the key_share extension if the client's initial
     * key share was not acceptable. */
    if( ssl->handshake->offered_group_id != 0 )
    {
        MBEDTLS_SSL_DEBUG_MSG( 4, ( "Skip key_share extension in HRR" ) );
        return( 0 );
    }

    if( selected_group == 0 )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "no matching named group found" ) );
        return( MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
    }

    /* Check if we have enough space:
     * - extension_type         (2 bytes)
     * - extension_data_length  (2 bytes)
     * - selected_group         (2 bytes)
     */
    MBEDTLS_SSL_CHK_BUF_PTR( buf, end, 6 );

    MBEDTLS_PUT_UINT16_BE( MBEDTLS_TLS_EXT_KEY_SHARE, buf, 0 );
    MBEDTLS_PUT_UINT16_BE( 2, buf, 2 );
    MBEDTLS_PUT_UINT16_BE( selected_group, buf, 4 );

    MBEDTLS_SSL_DEBUG_MSG( 3,
        ( "HRR selected_group: %s (%x)",
            mbedtls_ssl_named_group_to_str( selected_group ),
            selected_group ) );

    *out_len = 6;

    return( 0 );
}

/*
 * Structure of ServerHello message:
 *
 *     struct {
 *        ProtocolVersion legacy_version = 0x0303;    // TLS v1.2
 *        Random random;
 *        opaque legacy_session_id_echo<0..32>;
 *        CipherSuite cipher_suite;
 *        uint8 legacy_compression_method = 0;
 *        Extension extensions<6..2^16-1>;
 *    } ServerHello;
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_server_hello_body( mbedtls_ssl_context *ssl,
                                              unsigned char *buf,
                                              unsigned char *end,
                                              size_t *out_len,
                                              int is_hrr )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *p = buf;
    unsigned char *p_extensions_len;
    size_t output_len;

    *out_len = 0;

    /* ...
     * ProtocolVersion legacy_version = 0x0303; // TLS 1.2
     * ...
     * with ProtocolVersion defined as:
     * uint16 ProtocolVersion;
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 2 );
    MBEDTLS_PUT_UINT16_BE( 0x0303, p, 0 );
    p += 2;

    /* ...
     * Random random;
     * ...
     * with Random defined as:
     * opaque Random[MBEDTLS_SERVER_HELLO_RANDOM_LEN];
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, MBEDTLS_SERVER_HELLO_RANDOM_LEN );
    if( is_hrr )
    {
        memcpy( p, mbedtls_ssl_tls13_hello_retry_request_magic,
                MBEDTLS_SERVER_HELLO_RANDOM_LEN );
    }
    else
    {
        memcpy( p, &ssl->handshake->randbytes[MBEDTLS_CLIENT_HELLO_RANDOM_LEN],
                MBEDTLS_SERVER_HELLO_RANDOM_LEN );
    }
    MBEDTLS_SSL_DEBUG_BUF( 3, "server hello, random bytes",
                           p, MBEDTLS_SERVER_HELLO_RANDOM_LEN );
    p += MBEDTLS_SERVER_HELLO_RANDOM_LEN;

    /* ...
     * opaque legacy_session_id_echo<0..32>;
     * ...
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 1 + ssl->session_negotiate->id_len );
    *p++ = (unsigned char)ssl->session_negotiate->id_len;
    if( ssl->session_negotiate->id_len > 0 )
    {
        memcpy( p, &ssl->session_negotiate->id[0],
                ssl->session_negotiate->id_len );
        p += ssl->session_negotiate->id_len;

        MBEDTLS_SSL_DEBUG_BUF( 3, "session id", ssl->session_negotiate->id,
                               ssl->session_negotiate->id_len );
    }

    /* ...
     * CipherSuite cipher_suite;
     * ...
     * with CipherSuite defined as:
     * uint8 CipherSuite[2];
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 2 );
    MBEDTLS_PUT_UINT16_BE( ssl->session_negotiate->ciphersuite, p, 0 );
    p += 2;
    MBEDTLS_SSL_DEBUG_MSG( 3,
        ( "server hello, chosen ciphersuite: %s ( id=%d )",
          mbedtls_ssl_get_ciphersuite_name(
            ssl->session_negotiate->ciphersuite ),
          ssl->session_negotiate->ciphersuite ) );

    /* ...
     * uint8 legacy_compression_method = 0;
     * ...
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 1 );
    *p++ = 0x0;

    /* ...
     * Extension extensions<6..2^16-1>;
     * ...
     * struct {
     *      ExtensionType extension_type; (2 bytes)
     *      opaque extension_data<0..2^16-1>;
     * } Extension;
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 2 );
    p_extensions_len = p;
    p += 2;

    if( ( ret = ssl_tls13_write_server_hello_supported_versions_ext(
                                            ssl, p, end, &output_len ) ) != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET(
            1, "ssl_tls13_write_server_hello_supported_versions_ext", ret );
        return( ret );
    }
    p += output_len;

    if( mbedtls_ssl_conf_tls13_some_ephemeral_enabled( ssl ) )
    {
        if( is_hrr )
            ret = ssl_tls13_write_hrr_key_share_ext( ssl, p, end, &output_len );
        else
            ret = ssl_tls13_write_key_share_ext( ssl, p, end, &output_len );
        if( ret != 0 )
            return( ret );
        p += output_len;
    }

    MBEDTLS_PUT_UINT16_BE( p - p_extensions_len - 2, p_extensions_len, 0 );

    MBEDTLS_SSL_DEBUG_BUF( 4, "server hello extensions",
                           p_extensions_len, p - p_extensions_len );

    *out_len = p - buf;

    MBEDTLS_SSL_DEBUG_BUF( 3, "server hello", buf, *out_len );

    return( ret );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_finalize_write_server_hello( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    ret = mbedtls_ssl_tls13_compute_handshake_transform( ssl );
    if( ret != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET( 1,
                               "mbedtls_ssl_tls13_compute_handshake_transform",
                               ret );
        return( ret );
    }

    return( ret );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_server_hello( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *buf;
    size_t buf_len, msg_len;

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "=> write server hello" ) );

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_prepare_server_hello( ssl ) );

    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_start_handshake_msg( ssl,
                                MBEDTLS_SSL_HS_SERVER_HELLO, &buf, &buf_len ) );

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_write_server_hello_body( ssl, buf,
                                                             buf + buf_len,
                                                             &msg_len,
                                                             0 ) );

    mbedtls_ssl_add_hs_msg_to_checksum(
        ssl, MBEDTLS_SSL_HS_SERVER_HELLO, buf, msg_len );

    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_finish_handshake_msg(
                              ssl, buf_len, msg_len ) );

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_finalize_write_server_hello( ssl ) );

#if defined(MBEDTLS_SSL_TLS1_3_COMPATIBILITY_MODE)
    /* The server sends a dummy change_cipher_spec record immediately
     * after its first handshake message. This may either be after
     * a ServerHello or a HelloRetryRequest.
     */
    mbedtls_ssl_handshake_set_state(
            ssl, MBEDTLS_SSL_SERVER_CCS_AFTER_SERVER_HELLO );
#else
    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_ENCRYPTED_EXTENSIONS );
#endif /* MBEDTLS_SSL_TLS1_3_COMPATIBILITY_MODE */

cleanup:

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "<= write server hello" ) );
    return( ret );
}


/*
 * Handler for MBEDTLS_SSL_HELLO_RETRY_REQUEST
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_prepare_hello_retry_request( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    if( ssl->handshake->hello_retry_request_count > 0 )
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "Too many HRRs" ) );
        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_HANDSHAKE_FAILURE,
                                      MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
        return( MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
    }

    /*
     * Create stateless transcript hash for HRR
     */
    MBEDTLS_SSL_DEBUG_MSG( 4, ( "Reset transcript for HRR" ) );
    ret = mbedtls_ssl_reset_transcript_for_hrr( ssl );
    if( ret != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET( 1, "mbedtls_ssl_reset_transcript_for_hrr", ret );
        return( ret );
    }
    mbedtls_ssl_session_reset_msg_layer( ssl, 0 );

    return( 0 );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_hello_retry_request( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *buf;
    size_t buf_len, msg_len;

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "=> write hello retry request" ) );

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_prepare_hello_retry_request( ssl ) );

    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_start_handshake_msg(
                              ssl, MBEDTLS_SSL_HS_SERVER_HELLO,
                              &buf, &buf_len ) );

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_write_server_hello_body( ssl, buf,
                                                             buf + buf_len,
                                                             &msg_len,
                                                             1 ) );
    mbedtls_ssl_add_hs_msg_to_checksum(
        ssl, MBEDTLS_SSL_HS_SERVER_HELLO, buf, msg_len );


    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_finish_handshake_msg( ssl, buf_len,
                                                            msg_len ) );

    ssl->handshake->hello_retry_request_count++;

#if defined(MBEDTLS_SSL_TLS1_3_COMPATIBILITY_MODE)
    /* The server sends a dummy change_cipher_spec record immediately
     * after its first handshake message. This may either be after
     * a ServerHello or a HelloRetryRequest.
     */
    mbedtls_ssl_handshake_set_state(
            ssl, MBEDTLS_SSL_SERVER_CCS_AFTER_HELLO_RETRY_REQUEST );
#else
    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CLIENT_HELLO );
#endif /* MBEDTLS_SSL_TLS1_3_COMPATIBILITY_MODE */

cleanup:
    MBEDTLS_SSL_DEBUG_MSG( 2, ( "<= write hello retry request" ) );
    return( ret );
}

/*
 * Handler for MBEDTLS_SSL_ENCRYPTED_EXTENSIONS
 */

/*
 * struct {
 *    Extension extensions<0..2 ^ 16 - 1>;
 * } EncryptedExtensions;
 *
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_encrypted_extensions_body( mbedtls_ssl_context *ssl,
                                                      unsigned char *buf,
                                                      unsigned char *end,
                                                      size_t *out_len )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *p = buf;
    size_t extensions_len = 0;
    unsigned char *p_extensions_len;
    size_t output_len;

    *out_len = 0;

    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 2 );
    p_extensions_len = p;
    p += 2;

    ((void) ssl);
    ((void) ret);
    ((void) output_len);

#if defined(MBEDTLS_SSL_ALPN)
    ret = mbedtls_ssl_write_alpn_ext( ssl, p, end, &output_len );
    if( ret != 0 )
        return( ret );
    p += output_len;
#endif /* MBEDTLS_SSL_ALPN */

    extensions_len = ( p - p_extensions_len ) - 2;
    MBEDTLS_PUT_UINT16_BE( extensions_len, p_extensions_len, 0 );

    *out_len = p - buf;

    MBEDTLS_SSL_DEBUG_BUF( 4, "encrypted extensions", buf, *out_len );

    return( 0 );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_encrypted_extensions( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *buf;
    size_t buf_len, msg_len;

    mbedtls_ssl_set_outbound_transform( ssl,
                                        ssl->handshake->transform_handshake );
    MBEDTLS_SSL_DEBUG_MSG(
        3, ( "switching to handshake transform for outbound data" ) );

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "=> write encrypted extensions" ) );

    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_start_handshake_msg( ssl,
                       MBEDTLS_SSL_HS_ENCRYPTED_EXTENSIONS, &buf, &buf_len ) );

    MBEDTLS_SSL_PROC_CHK( ssl_tls13_write_encrypted_extensions_body(
                              ssl, buf, buf + buf_len, &msg_len ) );

    mbedtls_ssl_add_hs_msg_to_checksum(
        ssl, MBEDTLS_SSL_HS_ENCRYPTED_EXTENSIONS, buf, msg_len );

    MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_finish_handshake_msg(
                              ssl, buf_len, msg_len ) );

#if defined(MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED)
    if( mbedtls_ssl_tls13_some_psk_enabled( ssl ) )
        mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_SERVER_FINISHED );
    else
        mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CERTIFICATE_REQUEST );
#else
    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_SERVER_FINISHED );
#endif

cleanup:

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "<= write encrypted extensions" ) );
    return( ret );
}

#if defined(MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED)
#define SSL_CERTIFICATE_REQUEST_SEND_REQUEST 0
#define SSL_CERTIFICATE_REQUEST_SKIP         1
/* Coordination:
 * Check whether a CertificateRequest message should be written.
 * Returns a negative code on failure, or
 * - SSL_CERTIFICATE_REQUEST_SEND_REQUEST
 * - SSL_CERTIFICATE_REQUEST_SKIP
 * indicating if the writing of the CertificateRequest
 * should be skipped or not.
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_certificate_request_coordinate( mbedtls_ssl_context *ssl )
{
    int authmode;

#if defined(MBEDTLS_SSL_SERVER_NAME_INDICATION)
    if( ssl->handshake->sni_authmode != MBEDTLS_SSL_VERIFY_UNSET )
        authmode = ssl->handshake->sni_authmode;
    else
#endif
    authmode = ssl->conf->authmode;

    if( authmode == MBEDTLS_SSL_VERIFY_NONE )
        return( SSL_CERTIFICATE_REQUEST_SKIP );

    ssl->handshake->certificate_request_sent = 1;

    return( SSL_CERTIFICATE_REQUEST_SEND_REQUEST );
}

/*
 * struct {
 *   opaque certificate_request_context<0..2^8-1>;
 *   Extension extensions<2..2^16-1>;
 * } CertificateRequest;
 *
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_certificate_request_body( mbedtls_ssl_context *ssl,
                                                     unsigned char *buf,
                                                     const unsigned char *end,
                                                     size_t *out_len )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;
    unsigned char *p = buf;
    size_t output_len = 0;
    unsigned char *p_extensions_len;

    *out_len = 0;

    /* Check if we have enough space:
     * - certificate_request_context (1 byte)
     * - extensions length           (2 bytes)
     */
    MBEDTLS_SSL_CHK_BUF_PTR( p, end, 3 );

    /*
     * Write certificate_request_context
     */
    /*
     * We use a zero length context for the normal handshake
     * messages. For post-authentication handshake messages
     * this request context would be set to a non-zero value.
     */
    *p++ = 0x0;

    /*
     * Write extensions
     */
    /* The extensions must contain the signature_algorithms. */
    p_extensions_len = p;
    p += 2;
    ret = mbedtls_ssl_write_sig_alg_ext( ssl, p, end, &output_len );
    if( ret != 0 )
        return( ret );

    p += output_len;
    MBEDTLS_PUT_UINT16_BE( p - p_extensions_len - 2, p_extensions_len, 0 );

    *out_len = p - buf;

    return( 0 );
}

MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_certificate_request( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "=> write certificate request" ) );

    MBEDTLS_SSL_PROC_CHK_NEG( ssl_tls13_certificate_request_coordinate( ssl ) );

    if( ret == SSL_CERTIFICATE_REQUEST_SEND_REQUEST )
    {
        unsigned char *buf;
        size_t buf_len, msg_len;

        MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_start_handshake_msg( ssl,
                MBEDTLS_SSL_HS_CERTIFICATE_REQUEST, &buf, &buf_len ) );

        MBEDTLS_SSL_PROC_CHK( ssl_tls13_write_certificate_request_body(
                                  ssl, buf, buf + buf_len, &msg_len ) );

        mbedtls_ssl_add_hs_msg_to_checksum(
            ssl, MBEDTLS_SSL_HS_CERTIFICATE_REQUEST, buf, msg_len );

        MBEDTLS_SSL_PROC_CHK( mbedtls_ssl_finish_handshake_msg(
                                  ssl, buf_len, msg_len ) );
    }
    else if( ret == SSL_CERTIFICATE_REQUEST_SKIP )
    {
        MBEDTLS_SSL_DEBUG_MSG( 2, ( "<= skip write certificate request" ) );
        ret = 0;
    }
    else
    {
        MBEDTLS_SSL_DEBUG_MSG( 1, ( "should never happen" ) );
        ret = MBEDTLS_ERR_SSL_INTERNAL_ERROR;
        goto cleanup;
    }

    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_SERVER_CERTIFICATE );
cleanup:

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "<= write certificate request" ) );
    return( ret );
}

/*
 * Handler for MBEDTLS_SSL_SERVER_CERTIFICATE
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_server_certificate( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

#if defined(MBEDTLS_X509_CRT_PARSE_C)
    if( ( ssl_tls13_pick_key_cert( ssl ) != 0 ) ||
          mbedtls_ssl_own_cert( ssl ) == NULL )
    {
        MBEDTLS_SSL_DEBUG_MSG( 2, ( "No certificate available." ) );
        MBEDTLS_SSL_PEND_FATAL_ALERT( MBEDTLS_SSL_ALERT_MSG_HANDSHAKE_FAILURE,
                                      MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE);
        return( MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
    }
#endif /* MBEDTLS_X509_CRT_PARSE_C */

    ret = mbedtls_ssl_tls13_write_certificate( ssl );
    if( ret != 0 )
        return( ret );
    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CERTIFICATE_VERIFY );
    return( 0 );
}

/*
 * Handler for MBEDTLS_SSL_CERTIFICATE_VERIFY
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_certificate_verify( mbedtls_ssl_context *ssl )
{
    int ret = mbedtls_ssl_tls13_write_certificate_verify( ssl );
    if( ret != 0 )
        return( ret );
    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_SERVER_FINISHED );
    return( 0 );
}
#endif /* MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED */

/*
 * Handler for MBEDTLS_SSL_SERVER_FINISHED
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_write_server_finished( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

    ret = mbedtls_ssl_tls13_write_finished_message( ssl );
    if( ret != 0 )
        return( ret );

    ret = mbedtls_ssl_tls13_compute_application_transform( ssl );
    if( ret != 0 )
    {
        MBEDTLS_SSL_PEND_FATAL_ALERT(
                MBEDTLS_SSL_ALERT_MSG_HANDSHAKE_FAILURE,
                MBEDTLS_ERR_SSL_HANDSHAKE_FAILURE );
        return( ret );
    }

    MBEDTLS_SSL_DEBUG_MSG( 1, ( "Switch to handshake keys for inbound traffic" ) );
    mbedtls_ssl_set_inbound_transform( ssl, ssl->handshake->transform_handshake );

    if( ssl->handshake->certificate_request_sent )
        mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CLIENT_CERTIFICATE );
    else
    {
        MBEDTLS_SSL_DEBUG_MSG( 2, ( "skip parse certificate" ) );
        MBEDTLS_SSL_DEBUG_MSG( 2, ( "skip parse certificate verify" ) );
        mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CLIENT_FINISHED );
    }

    return( 0 );
}

/*
 * Handler for MBEDTLS_SSL_CLIENT_FINISHED
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_process_client_finished( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

    ret = mbedtls_ssl_tls13_process_finished_message( ssl );
    if( ret != 0 )
        return( ret );

    ret = mbedtls_ssl_tls13_generate_resumption_master_secret( ssl );
    if( ret != 0 )
    {
        MBEDTLS_SSL_DEBUG_RET( 1,
            "mbedtls_ssl_tls13_generate_resumption_master_secret ", ret );
    }

    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_HANDSHAKE_WRAPUP );
    return( 0 );
}

/*
 * Handler for MBEDTLS_SSL_HANDSHAKE_WRAPUP
 */
MBEDTLS_CHECK_RETURN_CRITICAL
static int ssl_tls13_handshake_wrapup( mbedtls_ssl_context *ssl )
{
    MBEDTLS_SSL_DEBUG_MSG( 2, ( "handshake: done" ) );

    mbedtls_ssl_tls13_handshake_wrapup( ssl );
    mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_HANDSHAKE_OVER );
    return( 0 );
}

/*
 * TLS 1.3 State Machine -- server side
 */
int mbedtls_ssl_tls13_handshake_server_step( mbedtls_ssl_context *ssl )
{
    int ret = MBEDTLS_ERR_ERROR_CORRUPTION_DETECTED;

    if( ssl->state == MBEDTLS_SSL_HANDSHAKE_OVER || ssl->handshake == NULL )
        return( MBEDTLS_ERR_SSL_BAD_INPUT_DATA );

    MBEDTLS_SSL_DEBUG_MSG( 2, ( "tls13 server state: %s(%d)",
                                mbedtls_ssl_states_str( ssl->state ),
                                ssl->state ) );

    switch( ssl->state )
    {
        /* start state */
        case MBEDTLS_SSL_HELLO_REQUEST:
            mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CLIENT_HELLO );
            ret = 0;
            break;

        case MBEDTLS_SSL_CLIENT_HELLO:
            ret = ssl_tls13_process_client_hello( ssl );
            if( ret != 0 )
                MBEDTLS_SSL_DEBUG_RET( 1, "ssl_tls13_process_client_hello", ret );
            break;

        case MBEDTLS_SSL_HELLO_RETRY_REQUEST:
            ret = ssl_tls13_write_hello_retry_request( ssl );
            if( ret != 0 )
            {
                MBEDTLS_SSL_DEBUG_RET( 1, "ssl_tls13_write_hello_retry_request", ret );
                return( ret );
            }
            break;

        case MBEDTLS_SSL_SERVER_HELLO:
            ret = ssl_tls13_write_server_hello( ssl );
            break;

        case MBEDTLS_SSL_ENCRYPTED_EXTENSIONS:
            ret = ssl_tls13_write_encrypted_extensions( ssl );
            if( ret != 0 )
            {
                MBEDTLS_SSL_DEBUG_RET( 1, "ssl_tls13_write_encrypted_extensions", ret );
                return( ret );
            }
            break;

#if defined(MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED)
        case MBEDTLS_SSL_CERTIFICATE_REQUEST:
            ret = ssl_tls13_write_certificate_request( ssl );
            break;

        case MBEDTLS_SSL_SERVER_CERTIFICATE:
            ret = ssl_tls13_write_server_certificate( ssl );
            break;

        case MBEDTLS_SSL_CERTIFICATE_VERIFY:
            ret = ssl_tls13_write_certificate_verify( ssl );
            break;
#endif /* MBEDTLS_KEY_EXCHANGE_WITH_CERT_ENABLED */

        /*
         * Injection of dummy-CCS's for middlebox compatibility
         */
#if defined(MBEDTLS_SSL_TLS1_3_COMPATIBILITY_MODE)
        case MBEDTLS_SSL_SERVER_CCS_AFTER_HELLO_RETRY_REQUEST:
            ret = mbedtls_ssl_tls13_write_change_cipher_spec( ssl );
            if( ret == 0 )
                mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_CLIENT_HELLO );
            break;

        case MBEDTLS_SSL_SERVER_CCS_AFTER_SERVER_HELLO:
            ret = mbedtls_ssl_tls13_write_change_cipher_spec( ssl );
            if( ret == 0 )
                mbedtls_ssl_handshake_set_state( ssl, MBEDTLS_SSL_ENCRYPTED_EXTENSIONS );
            break;
#endif /* MBEDTLS_SSL_TLS1_3_COMPATIBILITY_MODE */

        case MBEDTLS_SSL_SERVER_FINISHED:
            ret = ssl_tls13_write_server_finished( ssl );
            break;

        case MBEDTLS_SSL_CLIENT_FINISHED:
            ret = ssl_tls13_process_client_finished( ssl );
            break;

        case MBEDTLS_SSL_HANDSHAKE_WRAPUP:
            ret = ssl_tls13_handshake_wrapup( ssl );
            break;

        case MBEDTLS_SSL_CLIENT_CERTIFICATE:
            ret = mbedtls_ssl_tls13_process_certificate( ssl );
            if( ret == 0 )
            {
                if( ssl->session_negotiate->peer_cert != NULL )
                {
                    mbedtls_ssl_handshake_set_state(
                        ssl, MBEDTLS_SSL_CLIENT_CERTIFICATE_VERIFY );
                }
                else
                {
                    MBEDTLS_SSL_DEBUG_MSG( 2, ( "skip parse certificate verify" ) );
                    mbedtls_ssl_handshake_set_state(
                        ssl, MBEDTLS_SSL_CLIENT_FINISHED );
                }
            }
            break;

        case MBEDTLS_SSL_CLIENT_CERTIFICATE_VERIFY:
            ret = mbedtls_ssl_tls13_process_certificate_verify( ssl );
            if( ret == 0 )
            {
                mbedtls_ssl_handshake_set_state(
                    ssl, MBEDTLS_SSL_CLIENT_FINISHED );
            }
            break;

        default:
            MBEDTLS_SSL_DEBUG_MSG( 1, ( "invalid state %d", ssl->state ) );
            return( MBEDTLS_ERR_SSL_FEATURE_UNAVAILABLE );
    }

    return( ret );
}

#endif /* MBEDTLS_SSL_SRV_C && MBEDTLS_SSL_PROTO_TLS1_3 */
