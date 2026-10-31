const sizeof_addr4 = 1 + 4 + 2
const base_len = 1 + 2
const sizeof_chain = base_len

struct SizeofTimesConst {
	b [sizeof(u32) * base_len]u8
}

struct ConstTimesSizeof {
	b [base_len * sizeof(u32)]u8
}

fn times_ten(sizeof_addr4 int) int {
	return sizeof_addr4 * 10
}

fn test_sizeof_named_const_keeps_precedence() {
	system_addr_size := sizeof_addr4 * 20
	assert sizeof_addr4 == 7
	assert system_addr_size == 140
	assert -sizeof_addr4 == -7
	assert sizeof_chain * 3 == 9
}

fn test_param_shadowing_sizeof_named_const() {
	assert times_ten(5) == 50
}

fn test_fixed_array_len_with_sizeof_and_const_expr() {
	a := SizeofTimesConst{}
	b := ConstTimesSizeof{}
	assert a.b.len == 12
	assert b.b.len == 12
	assert sizeof(SizeofTimesConst) == 12
	assert sizeof(ConstTimesSizeof) == 12
}
