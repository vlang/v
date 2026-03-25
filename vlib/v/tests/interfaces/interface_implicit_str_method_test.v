interface Stringy {
	str() string
}

struct SomeStruct {}

fn stringify(value Stringy) string {
	return value.str()
}

fn join(values ...Stringy) string {
	return values.map(it.str()).join(' ')
}

fn test_interface_implicit_str_method() {
	some_struct := SomeStruct{}
	assert stringify('hi there') == 'hi there'
	assert stringify(1337) == '1337'
	assert stringify(true) == 'true'
	assert stringify(some_struct) == 'SomeStruct{}'
	assert join('hi', 42, false, some_struct) == 'hi 42 false SomeStruct{}'
}
