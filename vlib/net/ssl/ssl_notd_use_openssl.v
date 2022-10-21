module ssl

import net.mbedtls

pub struct SSLConn {
	mbedtls.SSLConn
}

[params]
pub struct SSLConnectConfig {
	mbedtls.SSLConnectConfig
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	c := mbedtls.new_ssl_conn(config.SSLConnectConfig) or { return err }
	return &SSLConn{c}
}
