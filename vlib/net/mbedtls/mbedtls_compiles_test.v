import net.mbedtls

fn test_mbedtls_compiles() {
	assert mbedtls.is_used == 1
	assert true
}
