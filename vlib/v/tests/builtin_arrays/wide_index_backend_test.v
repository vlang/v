fn test_dynamic_array_wide_and_negative_indexes() {
	mut values := [10, 20, 30]
	signed := i64(1)
	unsigned := u64(2)
	assert values[signed] == 20
	assert values[unsigned] == 30
	values[signed] = 21
	values[unsigned] = 31
	assert values == [10, 21, 31]
	assert (values[i64(-1)] or { -1 }) == -1
	assert (values[u64(1) << 63] or { -1 }) == -1
	last := i64(-1)
	assert values#[last] == 31
	values#[last] = 32
	assert values[2] == 32
	assert (values#[i64(-4)] or { -1 }) == -1
}

fn test_string_wide_and_negative_indexes() {
	value := 'abc'
	signed := i64(1)
	unsigned := u64(2)
	assert value[signed] == `b`
	assert value[unsigned] == `c`
	assert (value[i64(-1)] or { u8(0) }) == 0
	assert (value[u64(1) << 63] or { u8(0) }) == 0
	assert value#[i64(-1)] == `c`
	assert (value#[i64(-4)] or { u8(0) }) == 0
}

fn test_fixed_array_wide_indexes_keep_their_bounds_helpers() {
	values := [10, 20, 30]!
	signed := i64(1)
	unsigned := u64(2)
	assert values[signed] == 20
	assert values[unsigned] == 30
}
