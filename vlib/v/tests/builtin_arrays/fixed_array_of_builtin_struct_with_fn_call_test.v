fn get_foo_array(arr [2]VAttribute) []VAttribute {
	return [arr[0], arr[1]]
}

fn test_main() {
	arr := get_foo_array([VAttribute{}, VAttribute{}]!)
	assert arr == get_foo_array([VAttribute{}, VAttribute{}]!)
}
