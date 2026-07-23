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

#endif
