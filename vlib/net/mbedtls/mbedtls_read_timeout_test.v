// vtest build: !(windows && tinyc) // TODO: fix these by adding declarations for the missing functions in the prebuilt tcc
module mbedtls

import time

fn test_ssl_conn_read_timeout_can_be_configured_at_runtime() ! {
	config := SSLConnectConfig{}
	assert config.read_timeout == default_mbedtls_client_read_timeout

	mut conn := new_ssl_conn(config)!
	assert conn.read_timeout() == default_mbedtls_client_read_timeout

	timeout := 5 * time.minute
	conn.set_read_timeout(timeout)
	assert conn.read_timeout() == timeout
}
