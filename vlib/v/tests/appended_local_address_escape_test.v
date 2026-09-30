fn append_address(mut values []&char, data int) {
	if data != 0 {
		num := u64(data) * 3
		values << &char(&num)
	}
}

@[noinline]
fn use_the_stack(n int) int {
	mut buf := [64]u64{}
	for i in 0 .. 64 {
		buf[i] = u64(i + n)
	}
	return if n > 0 { use_the_stack(n - 1) + int(buf[n % 64]) } else { 0 }
}

// The address of a local appended to an array outlives the function: the local is
// allocated on the heap (db.pg's ORM binds its parameters this way).
fn test_address_of_a_local_appended_to_a_mut_array_parameter() {
	mut values := []&char{}
	append_address(mut values, 31)
	append_address(mut values, 45)
	_ = use_the_stack(10)
	assert unsafe { *(&u64(values[0])) } == 93
	assert unsafe { *(&u64(values[1])) } == 135
}

type Scalar = i16 | i64 | string

fn append_scalar(mut values []&char, data Scalar) {
	match data {
		i16 {
			num := u16(data)
			values << &char(&num)
		}
		i64 {
			num := u64(data)
			values << &char(&num)
		}
		string {
			values << &char(data.str)
		}
	}
}

// Each branch declares its own `num`: every one of them is moved to the heap.
fn test_same_named_locals_in_match_branches() {
	mut values := []&char{}
	append_scalar(mut values, Scalar(i64(45)))
	append_scalar(mut values, Scalar(i16(7)))
	append_scalar(mut values, Scalar(i64(31)))
	_ = use_the_stack(10)
	assert unsafe { *(&u64(values[0])) } == 45
	assert unsafe { *(&u16(values[1])) } == 7
	assert unsafe { *(&u64(values[2])) } == 31
}

fn append_through_values(mut values []&u64, data int) {
	a := u64(data)
	b := u64(data) * 2
	values << unsafe { &a }
	values << if data > 0 { &b } else { &a }
	values << match data {
		0 { &a }
		else { &b }
	}
}

// The appended address can be the value of a block or of an `if` or `match`.
fn test_address_appended_through_a_value_block_or_branch() {
	mut values := []&u64{}
	append_through_values(mut values, 21)
	_ = use_the_stack(10)
	assert *values[0] == 21
	assert *values[1] == 42
	assert *values[2] == 42
}

fn address_result(value &u64, success bool) !&u64 {
	if !success {
		return error('no address')
	}
	return unsafe { value }
}

fn append_through_or_values(mut values []&u64, data int) {
	option_source := u64(data)
	option_fallback := u64(data) * 2
	result_source := u64(data) * 3
	result_fallback := u64(data) * 4
	alias_fallback := u64(data) * 5
	missing := ?&u64(none)
	candidate := ?&u64(&option_source)
	values << (candidate or { &option_fallback })
	values << (missing or { &option_fallback })
	values << (address_result(&result_source, true) or { &result_fallback })
	values << (address_result(&result_source, false) or { &result_fallback })
	alias := missing or { &alias_fallback }
	values << alias
}

fn test_option_and_result_appends_keep_operand_and_fallback_addresses() {
	mut values := []&u64{}
	append_through_or_values(mut values, 21)
	_ = use_the_stack(10)
	assert *values[0] == 21
	assert *values[1] == 42
	assert *values[2] == 63
	assert *values[3] == 84
	assert *values[4] == 105
}

fn append_through_branch_aliases(mut values []&u64, data u64, choose_first bool) {
	block_local := data
	if_first := data * 2
	if_second := data * 3
	match_first := data * 4
	match_second := data * 5
	block_alias := unsafe { &block_local }
	if_alias := if choose_first { &if_first } else { &if_second }
	match_alias := match choose_first {
		true { &match_first }
		false { &match_second }
	}
	mut assigned_alias := &u64(unsafe { nil })
	assigned_alias = if choose_first { &if_first } else { &if_second }
	values << block_alias
	values << if_alias
	values << match_alias
	values << assigned_alias
}

fn test_value_block_if_and_match_aliases_keep_appended_addresses() {
	mut values := []&u64{}
	append_through_branch_aliases(mut values, 21, true)
	append_through_branch_aliases(mut values, 14, false)
	_ = use_the_stack(10)
	assert *values[0] == 21
	assert *values[1] == 42
	assert *values[2] == 84
	assert *values[3] == 42
	assert *values[4] == 14
	assert *values[5] == 42
	assert *values[6] == 70
	assert *values[7] == 42
}

fn copy_c_bytes(bytes &u8) string {
	return unsafe { tos_clone(bytes) }
}

fn fixed_buffer_text() (string, int) {
	mut buf := [16]u8{}
	for i in 0 .. int(sizeof(buf)) - 1 {
		buf[i] = `x`
	}
	return copy_c_bytes(unsafe { &buf[0] }), int(sizeof(buf))
}

// A fixed array retains its value size when its elements' addresses are passed in a
// `return` (`time.strftime` returns `cstring_to_vstring(&buf[0])`), including when the
// original storage moves to the heap.
fn test_fixed_array_in_a_return_keeps_its_size() {
	text, size := fixed_buffer_text()
	assert size == 16
	assert text == 'x'.repeat(15)
}

@[noinline]
fn append_fixed_element_addresses(mut values []&u64) (int, int) {
	mut items := [u64(11), 22, 33, 44]!
	pointer := unsafe { &items }
	alias := unsafe { &items[1] }
	values << unsafe { &items[0] }
	values << alias
	items[0] = 55
	items[1] = 66
	return int(sizeof(items)), int(sizeof(pointer))
}

fn test_appended_fixed_array_element_addresses_share_durable_storage_and_value_size() {
	mut values := []&u64{}
	size, pointer_size := append_fixed_element_addresses(mut values)
	assert size == 4 * int(sizeof(u64))
	assert pointer_size == int(sizeof(voidptr))
	_ = use_the_stack(10)
	assert *values[0] == 55
	assert *values[1] == 66
}
