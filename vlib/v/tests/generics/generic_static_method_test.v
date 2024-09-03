struct SomeStruct {
	name string
}

fn SomeStruct.static_method[T]() string {
	return 'hello'
}

fn test_main() {
	val := SomeStruct.static_method[string]()
	assert val == 'hello'
}
