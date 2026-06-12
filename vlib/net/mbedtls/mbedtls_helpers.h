#ifndef V_NET_MBEDTLS_HELPERS_H
#define V_NET_MBEDTLS_HELPERS_H

static inline void v_mbedtls_ssl_set_bio_nonblocking(mbedtls_ssl_context *ssl, mbedtls_net_context *net)
{
	mbedtls_ssl_set_bio(ssl, net, mbedtls_net_send, mbedtls_net_recv, NULL);
}

#endif
