#ifndef V_NET_MBEDTLS_HELPERS_H
#define V_NET_MBEDTLS_HELPERS_H

static inline void v_mbedtls_ssl_set_bio_nonblocking(mbedtls_ssl_context *ssl, mbedtls_net_context *net)
{
	mbedtls_ssl_set_bio(ssl, net, mbedtls_net_send, mbedtls_net_recv, NULL);
}

/* v_mbedtls_x509_crt_get_pk returns a pointer to crt's embedded public-key
 * context. mbedtls_x509_crt's `pk` field is a plain (non-MBEDTLS_PRIVATE)
 * member, but the struct as a whole is still kept opaque on the V side
 * (too risky to hand-replicate in full, per this module's own convention
 * for mbedtls_x509_crt/mbedtls_pk_context elsewhere) -- this shim is the
 * one place that needs real field access, resolved by the real C compiler
 * against the real struct layout rather than a V-side guess at the offset.
 */
static inline mbedtls_pk_context *v_mbedtls_x509_crt_get_pk(mbedtls_x509_crt *crt)
{
	return &crt->pk;
}

/* v_mbedtls_pk_ec_group_id returns the EC group ID of an EC public-key
 * context (MBEDTLS_ECP_DP_NONE if pk is not an EC key), so a caller can
 * confirm a certificate's ACTUAL curve matches what a TLS 1.3
 * SignatureScheme name like ecdsa_secp256r1_sha256 specifically claims.
 * mbedtls_pk_verify_ext only checks the key TYPE (EC vs RSA), never the
 * curve, so without this a P-384/P-521 (or any other EC curve) certificate
 * could otherwise be accepted under a scheme name that names one specific
 * curve. mbedtls_pk_ec() takes its mbedtls_pk_context argument BY VALUE
 * (real mbedTLS API, not a V-side assumption) -- resolved here in C, where
 * the struct's size/layout are fully known, rather than attempting a V-side
 * by-value pass of an intentionally-opaque struct type.
 */
static inline mbedtls_ecp_group_id v_mbedtls_pk_ec_group_id(const mbedtls_pk_context *pk)
{
	const mbedtls_ecp_keypair *kp = mbedtls_pk_ec(*pk);
	if (kp == NULL) {
		return MBEDTLS_ECP_DP_NONE;
	}
	return mbedtls_ecp_keypair_get_group_id(kp);
}

/* v_mbedtls_check_server_cert_usage verifies the leaf certificate's
 * keyUsage (if present) allows digitalSignature and its extendedKeyUsage
 * (if present) allows serverAuth -- the two checks a full TLS handshake's
 * own mbedtls_ssl_check_cert_usage() performs internally that
 * mbedtls_x509_crt_verify() alone does NOT (chain trust + hostname only).
 * Standalone callers that never construct an mbedtls_ssl_context (this
 * module's whole reason to exist) never get that internal check run for
 * them. Returns 0 if both checks pass; both
 * mbedtls_x509_crt_check_key_usage/check_extended_key_usage already treat
 * an ABSENT extension as "allowed" (X.509's own "absent means
 * unrestricted" convention), so this correctly does not over-reject a
 * certificate with no KeyUsage/EKU extensions at all. Non-zero (the first
 * failing check's own mbedTLS error code) if either restricts the
 * certificate's use to something other than a TLS server signing key.
 */
static inline int v_mbedtls_check_server_cert_usage(mbedtls_x509_crt *crt)
{
	int ku_ret = mbedtls_x509_crt_check_key_usage(crt, MBEDTLS_X509_KU_DIGITAL_SIGNATURE);
	if (ku_ret != 0) {
		return ku_ret;
	}
	return mbedtls_x509_crt_check_extended_key_usage(crt, MBEDTLS_OID_SERVER_AUTH,
		MBEDTLS_OID_SIZE(MBEDTLS_OID_SERVER_AUTH));
}

#endif
