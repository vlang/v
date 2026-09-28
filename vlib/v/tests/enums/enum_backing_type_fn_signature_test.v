// Function parameters and return values of an enum with a backing type must keep
// its storage: a 64 bit value used to be truncated to `int`, and a `mut` parameter
// of a `u8` enum used to write an `int` over the fields after it.

enum Wide as u64 {
	low  = 1
	high = 9223372036854775808
}

enum Small as u8 {
	a = 1
	b = 200
}

struct Fields {
mut:
	small Small
	next  u8 = 7
	more  u8 = 9
}

fn wide_value(w Wide) u64 {
	return u64(w)
}

fn wide_result() Wide {
	return .high
}

fn set_wide(mut w Wide) {
	w = .high
}

fn set_small(mut s Small) {
	s = .b
}

fn generic_value[T](value T) u64 {
	return u64(value)
}

fn test_wide_enum_params_and_results() {
	assert wide_value(.high) == 9223372036854775808
	assert wide_result() == .high
	mut w := Wide.low
	set_wide(mut w)
	assert w == .high
	assert generic_value(Wide.high) == 9223372036854775808
}

fn test_mut_param_of_narrow_enum_keeps_the_next_fields() {
	mut fields := Fields{}
	set_small(mut fields.small)
	assert fields.small == .b
	assert fields.next == 7
	assert fields.more == 9
}
