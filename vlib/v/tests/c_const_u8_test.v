import net

const C.AF_UNIX u16

fn x16(n u16) bool {
	return true
}

fn xint(n int) bool {
	return true
}

fn test_const() {
	assert xint(C.EOF) == true // a random libc const is int by default

	assert x16(u16(C.AF_INET)) == true // defined in V's net module
}
