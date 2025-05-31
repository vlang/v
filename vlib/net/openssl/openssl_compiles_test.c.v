// vtest build: present_openssl?
import net.openssl as _

struct Abc {
	x &C.SSL_CTX
}

fn test_printing_struct_with_reference_field_of_type_ssl_ctx() {
	a := unsafe { Abc{&C.SSL_CTX(123)} }
	dump(a)
	sa := a.str()
	assert sa.contains('&C.SSL_CTX(0x7b)')
}
