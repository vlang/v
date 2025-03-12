// vtest build: present_openssl? && !(windows && tinyc) // TODO: fix these by adding declarations for the missing functions in the prebuilt tcc
import net.ssl

fn test_ssl_compiles() {
	assert sizeof(ssl.SSLConn) > 0
}
