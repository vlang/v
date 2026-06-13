import datatypes

struct MyNode[T] {
	val T
}

struct MyContainer[T] {
	lst datatypes.LinkedList[MyNode[T]]
}

fn test_auto_str_registers_nested_generic_field_str_method() {
	c := MyContainer[string]{}
	assert '${c}' == 'MyContainer[string]{
    lst: []
}'
}
