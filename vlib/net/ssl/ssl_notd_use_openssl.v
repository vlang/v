module ssl

import net.mbedtls

pub type SSLConn = mbedtls.SSLConn
pub type SSLListener = mbedtls.SSLListener

@[params]
pub struct SSLConnectConfig {
	mbedtls.SSLConnectConfig
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	return mbedtls.new_ssl_conn(config.SSLConnectConfig) or { return err }
}

// new_ssl_listener returns a new SSLListener with the given config.
pub fn new_ssl_listener(saddr string, config SSLConnectConfig) !&SSLListener {
	return mbedtls.new_ssl_listener(saddr, config.SSLConnectConfig) or { return err }
}
