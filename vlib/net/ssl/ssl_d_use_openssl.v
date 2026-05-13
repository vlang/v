module ssl

import net.openssl

pub type SSLConn = openssl.SSLConn

@[params]
pub struct SSLConnectConfig {
	openssl.SSLConnectConfig
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	return openssl.new_ssl_conn(config.SSLConnectConfig) or { return err }
}
