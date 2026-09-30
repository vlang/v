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

fn sum_items(items [3]u64) u64 {
	return items[0] + items[1] + items[2]
}

fn bump_items(mut items [3]u64) {
	items[1] += 100
}

fn append_fixed_array_elements(mut values []&u64, data u64) (u64, u64, u64) {
	mut items := [data, data + 1, data + 2]!
	values << unsafe { &items[0] }
	values << unsafe { &items[2] }
	items[2] += 10
	mut total := u64(0)
	for item in items {
		total += item
	}
	bump_items(mut items)
	snapshot := items
	return sum_items(items), total, snapshot[1]
}

// The addresses of a fixed array's elements outlive the function: the array is moved to
// the heap, and its other uses still see one array.
fn test_element_addresses_of_a_fixed_array_appended() {
	mut values := []&u64{}
	sum, total, second := append_fixed_array_elements(mut values, 30)
	_ = use_the_stack(10)
	assert sum == 203
	assert total == 103
	assert second == 131
	assert values.map(*it) == [u64(30), 42]
}

fn append_through_wrapper_aliases(mut values []&u64, data u64, c bool) {
	first := data
	p := unsafe {
		q := &first
		q
	}
	values << p
	second := data + 1
	r := if c {
		s := &second
		s
	} else {
		unsafe { nil }
	}
	values << r
}

// The appended pointer can be an alias declared inside the block or branch that gives
// another alias its value.
fn test_address_appended_through_an_alias_declared_in_a_value_wrapper() {
	mut values := []&u64{}
	append_through_wrapper_aliases(mut values, 50, true)
	_ = use_the_stack(10)
	assert values.map(*it) == [u64(50), 51]
}
