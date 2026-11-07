#ifndef V_MBEDTLS_SERVER_READ_TIMEOUT_TEST_H
#define V_MBEDTLS_SERVER_READ_TIMEOUT_TEST_H

#include <mbedtls/ssl.h>

static uint32_t v_mbedtls_test_config_read_timeout(mbedtls_ssl_config *conf)
{
	return conf->MBEDTLS_PRIVATE(read_timeout);
}

static uint32_t v_mbedtls_test_active_read_timeout(mbedtls_ssl_context *ssl)
{
	return mbedtls_ssl_context_get_config(ssl)->MBEDTLS_PRIVATE(read_timeout);
}

static void v_mbedtls_test_remove_rng(mbedtls_ssl_config *conf)
{
	mbedtls_ssl_conf_rng(conf, NULL, NULL);
}

#endif
