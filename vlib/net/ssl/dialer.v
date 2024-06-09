module ssl

import net

pub struct SSLDialer {
	config SSLConnectConfig
}

// create_ssl_dialer creates a dialer that will initiate SSL secured
// connections.
pub fn new_ssl_dialer(config SSLConnectConfig) net.IDialer {
	return &SSLDialer{
		config: config
	}
}

// dial initiates a new SSL connection.
pub fn (d SSLDialer) dial(address string) !net.IConn {
	return new_ssl_conn(d.config)!
}
