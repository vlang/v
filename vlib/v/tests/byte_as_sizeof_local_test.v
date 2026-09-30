// `sizeof` of a local or parameter named `byte` measures the variable's type.
struct Pair {
	a u8
	b u64
}

fn param_width(byte u16) usize {
	return sizeof(byte)
}

fn test_sizeof_byte_local_and_param_use_their_types() {
	byte := Pair{}
	assert sizeof(byte) == sizeof(Pair)
	assert byte.a == 0
	assert param_width(1) == 2
}
