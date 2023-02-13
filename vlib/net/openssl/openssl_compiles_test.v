import net.openssl

struct Abc {
	x &C.SSL_CTX
}

fn test_printing_struct_with_reference_field_of_type_ssl_ctx() {
	a := Abc{&C.SSL_CTX(123)}
	dump(a)
	sa := a.str()
	assert sa.contains('&C.SSL_CTX(0x7b)')
}

fn test_openssl_compiles() {
	assert openssl.is_used == 1
	assert true
}
