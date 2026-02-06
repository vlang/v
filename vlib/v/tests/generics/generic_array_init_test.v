fn decode[T]() {
	a := T{123}
	b := T{u8(123)}
	assert a == b
}

fn test_main() {
	decode[[]u8]()
}
