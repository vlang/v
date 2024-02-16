fn test_main() {
	foo := spawn get_pointer()
	ret := foo.wait()
	assert *ret == 42
}

fn test_opt() {
	foo := spawn get_pointer_opt()
	ret := foo.wait()
	assert *ret? == 42
}

fn get_pointer() &int {
	val := 42
	return &val
}

fn get_pointer_opt() ?&int {
	val := 42
	return &val
}
