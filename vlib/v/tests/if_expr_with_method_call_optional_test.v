fn to_bit_string(i rune) []u8 {
	return if i == 0 { []u8{} } else { '${i:b}'.split('').map(it.u8()) }
}

fn from_bit_string(bits []u8) rune {
	return if bits == [] { 0 } else { bits.map(it.str()).join('').parse_uint(2, 8) or { 0 } }
}

fn test_if_expr_with_method_call_optional() {
	a := from_bit_string(to_bit_string(u32(100)))
	println(a)
	assert a == `d`
}
