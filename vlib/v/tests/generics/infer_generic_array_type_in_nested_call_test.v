fn unmarshal_any[T]() T {
	mut typ := T{}
	$if T is $array {
		unmarshal_array(mut typ)
	}
	return typ
}

fn unmarshal_array[T](mut typ []T) {
	typ << 42
}

fn test_infer_generic_array_type_in_nested_call() {
	ret := unmarshal_any[[]int]()
	println(ret)
	assert ret == [42]
}
