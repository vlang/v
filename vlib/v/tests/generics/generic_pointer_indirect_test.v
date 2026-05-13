fn assert_generic_pointer_value[T](t T) {
	println(t)
	dump(t)
	assert *t == 'lalala'
}

fn test_generic_pointer_indirect() {
	la := 'lalala'
	assert_generic_pointer_value(&la)
}
