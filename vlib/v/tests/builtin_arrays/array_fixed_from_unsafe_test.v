@[unsafe]
fn test() [5]int {
	return [5]int{}
}

fn test_main() {
	foo := unsafe { test() }
	assert foo.len == 5
	assert foo == unsafe { test() }
}
