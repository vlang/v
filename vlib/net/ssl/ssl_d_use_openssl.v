module ssl

import net.openssl

pub type SSLConn = openssl.SSLConn
pub type SSLListener = openssl.SSLListener
pub type SSLListenerOptions = openssl.SSLListenerOptions

@[params]
pub struct SSLConnectConfig {
	openssl.SSLConnectConfig
}

// new_ssl_conn returns a new SSLConn with the given config.
pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
	return openssl.new_ssl_conn(config.SSLConnectConfig) or { return err }
}

// new_ssl_listener returns a new SSLListener with the given config.
pub fn new_ssl_listener(saddr string, config SSLConnectConfig, options SSLListenerOptions) !&SSLListener {
	return openssl.new_ssl_listener(saddr, config.SSLConnectConfig, options) or { return err }
}
