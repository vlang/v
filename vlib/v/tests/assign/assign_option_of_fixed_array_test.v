type Addr = [4]u8

fn test_assign_option_of_fixed_array() {
	mut addr := ?Addr(none)
	addr = Addr([u8(1), 2, 3, 4]!)
	println(addr)
	assert '${addr}' == 'Option(Addr([1, 2, 3, 4]))'
}

fn make_fixed_array() [2][3]u8 {
	return [[u8(1), 2, 3]!, [u8(4), 5, 6]!]!
}

struct FixedArrayOptions {
mut:
	one_dimensional ?[4]u8
	two_dimensional ?[2][3]u8
}

fn test_assign_fixed_array_values_to_option_fields() {
	mut options := FixedArrayOptions{}
	value := [u8(7), 8, 9, 10]!
	options.one_dimensional = value
	options.two_dimensional = make_fixed_array()
	assert options.one_dimensional? == [u8(7), 8, 9, 10]!
	assert options.two_dimensional? == [[u8(1), 2, 3]!, [u8(4), 5, 6]!]!
}
