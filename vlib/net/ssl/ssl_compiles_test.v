import net.ssl

fn test_ssl_compiles() {
	assert sizeof(ssl.SSLConn) > 0
}
