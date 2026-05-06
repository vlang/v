module ssl

$if tinyc && (freebsd || openbsd) {
	import net.openssl
} $else {
	import net.mbedtls
}

$if tinyc && (freebsd || openbsd) {
	// TinyCC hangs in the bundled mbedtls path on FreeBSD/OpenBSD.
	// Prefer the OpenSSL backend there, which does not exhibit the issue.
	pub type SSLConn = openssl.SSLConn

	@[params]
	pub struct SSLConnectConfig {
		openssl.SSLConnectConfig
	}

	// new_ssl_conn returns a new SSLConn with the given config.
	pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
		return openssl.new_ssl_conn(config.SSLConnectConfig) or { return err }
	}
} $else {
	pub type SSLConn = mbedtls.SSLConn

	@[params]
	pub struct SSLConnectConfig {
		mbedtls.SSLConnectConfig
	}

	// new_ssl_conn returns a new SSLConn with the given config.
	pub fn new_ssl_conn(config SSLConnectConfig) !&SSLConn {
		return mbedtls.new_ssl_conn(config.SSLConnectConfig) or { return err }
	}
}
