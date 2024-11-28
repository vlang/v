const some_arr = make_array()
const some_arr2 = make_array2()

fn make_array() [64]u8 {
	mut arr := [64]u8{}
	return arr
}

fn make_array2() []u8 {
	mut arr := []u8{}
	return arr
}

fn test_main() {
	assert some_arr.len == 64
	assert some_arr2.len == 0
}
