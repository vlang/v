// Native mbedTLS helpers and checked opaque storage ABI for macOS.
#include <mbedtls/net_sockets.h>
#include <mbedtls/ssl.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/error.h>
#include <mbedtls/threading.h>
#include <mbedtls/oid.h>
#include <mbedtls/ecdsa.h>
#include <mbedtls/ecdh.h>
#include <mbedtls/ecp.h>
#include <mbedtls/bignum.h>

#define V_MBEDTLS_HELPER_LINKAGE
#include "../../vlib/net/mbedtls/mbedtls_helpers.h"
#include "../../vlib/net/mbedtls/mbedtls_threading.h"

_Static_assert(sizeof(mbedtls_ctr_drbg_context) == 416, "V mbedtls_ctr_drbg_context storage size");
_Static_assert(_Alignof(mbedtls_ctr_drbg_context) == 8, "V mbedtls_ctr_drbg_context storage alignment");
_Static_assert(sizeof(mbedtls_ecdsa_context) == 256, "V mbedtls_ecdsa_context storage size");
_Static_assert(_Alignof(mbedtls_ecdsa_context) == 8, "V mbedtls_ecdsa_context storage alignment");
_Static_assert(sizeof(mbedtls_ecp_group) == 192, "V mbedtls_ecp_group storage size");
_Static_assert(_Alignof(mbedtls_ecp_group) == 8, "V mbedtls_ecp_group storage alignment");
_Static_assert(sizeof(mbedtls_ecp_point) == 48, "V mbedtls_ecp_point storage size");
_Static_assert(_Alignof(mbedtls_ecp_point) == 8, "V mbedtls_ecp_point storage alignment");
_Static_assert(sizeof(mbedtls_entropy_context) == 904, "V mbedtls_entropy_context storage size");
_Static_assert(_Alignof(mbedtls_entropy_context) == 8, "V mbedtls_entropy_context storage alignment");
_Static_assert(sizeof(mbedtls_mpi) == 16, "V mbedtls_mpi storage size");
_Static_assert(_Alignof(mbedtls_mpi) == 8, "V mbedtls_mpi storage alignment");
_Static_assert(sizeof(mbedtls_net_context) == 4, "V mbedtls_net_context storage size");
_Static_assert(_Alignof(mbedtls_net_context) == 4, "V mbedtls_net_context storage alignment");
_Static_assert(sizeof(mbedtls_pk_context) == 16, "V mbedtls_pk_context storage size");
_Static_assert(_Alignof(mbedtls_pk_context) == 8, "V mbedtls_pk_context storage alignment");
_Static_assert(sizeof(mbedtls_pk_rsassa_pss_options) == 8, "V mbedtls_pk_rsassa_pss_options storage size");
_Static_assert(_Alignof(mbedtls_pk_rsassa_pss_options) == 4, "V mbedtls_pk_rsassa_pss_options storage alignment");
_Static_assert(sizeof(mbedtls_ssl_config) == 392, "V mbedtls_ssl_config storage size");
_Static_assert(_Alignof(mbedtls_ssl_config) == 8, "V mbedtls_ssl_config storage alignment");
_Static_assert(sizeof(mbedtls_ssl_context) == 568, "V mbedtls_ssl_context storage size");
_Static_assert(_Alignof(mbedtls_ssl_context) == 8, "V mbedtls_ssl_context storage alignment");
_Static_assert(sizeof(mbedtls_x509_crl) == 416, "V mbedtls_x509_crl storage size");
_Static_assert(_Alignof(mbedtls_x509_crl) == 8, "V mbedtls_x509_crl storage alignment");
_Static_assert(sizeof(mbedtls_x509_crt) == 744, "V mbedtls_x509_crt storage size");
_Static_assert(_Alignof(mbedtls_x509_crt) == 8, "V mbedtls_x509_crt storage alignment");
_Static_assert(MBEDTLS_ECDSA_MAX_LEN == (139), "V MBEDTLS_ECDSA_MAX_LEN value");
_Static_assert(MBEDTLS_ECP_DP_SECP256K1 == (12), "V MBEDTLS_ECP_DP_SECP256K1 value");
_Static_assert(MBEDTLS_ECP_DP_SECP256R1 == (3), "V MBEDTLS_ECP_DP_SECP256R1 value");
_Static_assert(MBEDTLS_ECP_DP_SECP384R1 == (4), "V MBEDTLS_ECP_DP_SECP384R1 value");
_Static_assert(MBEDTLS_ECP_DP_SECP521R1 == (5), "V MBEDTLS_ECP_DP_SECP521R1 value");
_Static_assert(MBEDTLS_ECP_MAX_PT_LEN == (133), "V MBEDTLS_ECP_MAX_PT_LEN value");
_Static_assert(MBEDTLS_ECP_PF_UNCOMPRESSED == (0), "V MBEDTLS_ECP_PF_UNCOMPRESSED value");
_Static_assert(MBEDTLS_ERR_CTR_DRBG_ENTROPY_SOURCE_FAILED == (-52), "V MBEDTLS_ERR_CTR_DRBG_ENTROPY_SOURCE_FAILED value");
_Static_assert(MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY == (-30848), "V MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY value");
_Static_assert(MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET == (-31488), "V MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET value");
_Static_assert(MBEDTLS_ERR_SSL_TIMEOUT == (-26624), "V MBEDTLS_ERR_SSL_TIMEOUT value");
_Static_assert(MBEDTLS_ERR_SSL_WANT_READ == (-26880), "V MBEDTLS_ERR_SSL_WANT_READ value");
_Static_assert(MBEDTLS_ERR_SSL_WANT_WRITE == (-26752), "V MBEDTLS_ERR_SSL_WANT_WRITE value");
_Static_assert(MBEDTLS_ERR_X509_CERT_VERIFY_FAILED == (-9984), "V MBEDTLS_ERR_X509_CERT_VERIFY_FAILED value");
_Static_assert(MBEDTLS_MD_SHA256 == (9), "V MBEDTLS_MD_SHA256 value");
_Static_assert(MBEDTLS_MD_SHA384 == (10), "V MBEDTLS_MD_SHA384 value");
_Static_assert(MBEDTLS_MD_SHA512 == (11), "V MBEDTLS_MD_SHA512 value");
_Static_assert(MBEDTLS_NET_LISTEN_BACKLOG == (10), "V MBEDTLS_NET_LISTEN_BACKLOG value");
_Static_assert(MBEDTLS_NET_PROTO_TCP == (0), "V MBEDTLS_NET_PROTO_TCP value");
_Static_assert(MBEDTLS_SSL_IS_CLIENT == (0), "V MBEDTLS_SSL_IS_CLIENT value");
_Static_assert(MBEDTLS_SSL_IS_SERVER == (1), "V MBEDTLS_SSL_IS_SERVER value");
_Static_assert(MBEDTLS_SSL_PRESET_DEFAULT == (0), "V MBEDTLS_SSL_PRESET_DEFAULT value");
_Static_assert(MBEDTLS_SSL_TRANSPORT_STREAM == (0), "V MBEDTLS_SSL_TRANSPORT_STREAM value");
_Static_assert(MBEDTLS_SSL_VERIFY_OPTIONAL == (1), "V MBEDTLS_SSL_VERIFY_OPTIONAL value");
_Static_assert(MBEDTLS_SSL_VERIFY_REQUIRED == (2), "V MBEDTLS_SSL_VERIFY_REQUIRED value");
_Static_assert(MBEDTLS_X509_BADCERT_NOT_TRUSTED == (8), "V MBEDTLS_X509_BADCERT_NOT_TRUSTED value");

#include <stdint.h>

size_t v_mbedtls_context_size(int type_id) {
	switch (type_id) {
	case 0: return sizeof(mbedtls_ctr_drbg_context);
	case 1: return sizeof(mbedtls_ecdsa_context);
	case 2: return sizeof(mbedtls_ecp_group);
	case 3: return sizeof(mbedtls_ecp_point);
	case 4: return sizeof(mbedtls_entropy_context);
	case 5: return sizeof(mbedtls_mpi);
	case 6: return sizeof(mbedtls_net_context);
	case 7: return sizeof(mbedtls_pk_context);
	case 8: return sizeof(mbedtls_pk_rsassa_pss_options);
	case 9: return sizeof(mbedtls_ssl_config);
	case 10: return sizeof(mbedtls_ssl_context);
	case 11: return sizeof(mbedtls_x509_crl);
	case 12: return sizeof(mbedtls_x509_crt);
	default: return 0;
	}
}

int v_mbedtls_context_aligned(int type_id, void *context) {
	size_t alignment;
	switch (type_id) {
	case 0: alignment = _Alignof(mbedtls_ctr_drbg_context); break;
	case 1: alignment = _Alignof(mbedtls_ecdsa_context); break;
	case 2: alignment = _Alignof(mbedtls_ecp_group); break;
	case 3: alignment = _Alignof(mbedtls_ecp_point); break;
	case 4: alignment = _Alignof(mbedtls_entropy_context); break;
	case 5: alignment = _Alignof(mbedtls_mpi); break;
	case 6: alignment = _Alignof(mbedtls_net_context); break;
	case 7: alignment = _Alignof(mbedtls_pk_context); break;
	case 8: alignment = _Alignof(mbedtls_pk_rsassa_pss_options); break;
	case 9: alignment = _Alignof(mbedtls_ssl_config); break;
	case 10: alignment = _Alignof(mbedtls_ssl_context); break;
	case 11: alignment = _Alignof(mbedtls_x509_crl); break;
	case 12: alignment = _Alignof(mbedtls_x509_crt); break;
	default: return 0;
	}
	return ((uintptr_t)context % alignment) == 0;
}
