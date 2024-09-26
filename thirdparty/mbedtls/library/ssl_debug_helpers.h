/**
 * \file ssl_debug_helpers.h
 *
 * \brief Automatically generated helper functions for debugging
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

#ifndef MBEDTLS_SSL_DEBUG_HELPERS_H
#define MBEDTLS_SSL_DEBUG_HELPERS_H

#include "common.h"

#if defined(MBEDTLS_DEBUG_C)

#include "mbedtls/ssl.h"
#include "ssl_misc.h"


const char *mbedtls_ssl_states_str( mbedtls_ssl_states in );

const char *mbedtls_ssl_protocol_version_str( mbedtls_ssl_protocol_version in );

const char *mbedtls_tls_prf_types_str( mbedtls_tls_prf_types in );

const char *mbedtls_ssl_key_export_type_str( mbedtls_ssl_key_export_type in );

const char *mbedtls_ssl_sig_alg_to_str( uint16_t in );

const char *mbedtls_ssl_named_group_to_str( uint16_t in );

const char *mbedtls_ssl_get_extension_name( unsigned int extension_type );

void mbedtls_ssl_print_extensions( const mbedtls_ssl_context *ssl,
                                   int level, const char *file, int line,
                                   int hs_msg_type, uint32_t extensions_mask,
                                   const char *extra );

void mbedtls_ssl_print_extension( const mbedtls_ssl_context *ssl,
                                  int level, const char *file, int line,
                                  int hs_msg_type, unsigned int extension_type,
                                  const char *extra_msg0, const char *extra_msg1 );

#define MBEDTLS_SSL_PRINT_EXTS( level, hs_msg_type, extensions_mask )            \
            mbedtls_ssl_print_extensions( ssl, level, __FILE__, __LINE__,       \
                                          hs_msg_type, extensions_mask, NULL )

#define MBEDTLS_SSL_PRINT_EXT( level, hs_msg_type, extension_type, extra )      \
            mbedtls_ssl_print_extension( ssl, level, __FILE__, __LINE__,        \
                                         hs_msg_type, extension_type,           \
                                         extra, NULL )
#else

#define MBEDTLS_SSL_PRINT_EXTS( level, hs_msg_type, extension_mask )

#define MBEDTLS_SSL_PRINT_EXT( level, hs_msg_type, extension_type, extra )

#endif /* MBEDTLS_DEBUG_C */

#endif /* MBEDTLS_SSL_DEBUG_HELPERS_H */
