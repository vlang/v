const kb = 1024

const buf_siz = 2 * kb

fn test_consts() {
	assert kb == 1024
	assert buf_siz == 2 * kb
	assert buf_siz == 2048
	println(buf_siz)
}

fn test_fixed_size_array_can_use_a_known_comptime_const_as_its_size() {
	buf := [buf_siz]u8{}
	println(buf.len)
	assert buf.len == 2048
}

fn test_fixed_size_array_using_a_known_int_expression_directly_as_its_size() {
	zbuf := [5 + 20 * 10]u8{}
	assert zbuf.len == 205
}

fn test_fixed_size_array_using_a_known_int_expression_with_const_as_its_size() {
	zbuf := [2 * kb]u8{}
	assert zbuf.len == 2048
}
