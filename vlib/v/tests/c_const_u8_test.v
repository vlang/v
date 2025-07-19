const C.AF_INET u16

fn x(n u16) bool {
	return true
}

fn test_const() {
	assert x(C.AF_INET) == true
}
