module ssl

$if tinyc && (freebsd || openbsd) {
	import net.openssl
} $else {
	import net.mbedtls
}

$if tinyc && (freebsd || openbsd) {
	// TinyCC hangs in the bundled mbedtls path on FreeBSD/OpenBSD.
	// Prefer the OpenSSL backend there, which does not exhibit the issue.
	pub struct SSLConn {
		openssl.SSLConn
	}

	pub struct SSLConnectConfig {
		openssl.SSLConnectConfig
	}

	// new_ssl_conn returns a new SSLConn with the given config.
	@[params]
	pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
		c := openssl.new_ssl_conn(config.SSLConnectConfig) or { return err }
		return &SSLConn{c}
	}
} $else {
	pub struct SSLConn {
		mbedtls.SSLConn
	}

	pub struct SSLConnectConfig {
		mbedtls.SSLConnectConfig
	}

	// new_ssl_conn returns a new SSLConn with the given config.
	@[params]
	pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
		c := mbedtls.new_ssl_conn(config.SSLConnectConfig) or { return err }
		return &SSLConn{c}
	}
}
