import time

fn test_module_type_cast() {
	a := time.Duration(5)
	b := time.Duration(6)
	// println(a+b)
	assert a + b == 11
}
