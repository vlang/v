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

#endif /* MBEDTLS_DEBUG_C */

#endif /* SSL_DEBUG_HELPERS_H */
