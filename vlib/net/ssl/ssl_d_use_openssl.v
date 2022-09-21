module ssl

import net.openssl

pub struct SSLConn {
	openssl.SSLConn
}

[params]
pub struct SSLConnectConfig {
	openssl.SSLConnectConfig
}

pub fn new_ssl_conn(config SSLConnectConfig) ?&SSLConn {
	c := openssl.new_ssl_conn(config.SSLConnectConfig) or { return err }
	return &SSLConn{c}
}
