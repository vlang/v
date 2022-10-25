import net.openssl

fn test_openssl_compiles() {
	assert openssl.is_used == 1
	assert true
}
