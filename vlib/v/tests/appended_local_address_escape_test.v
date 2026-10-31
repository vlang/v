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

fn escape_decl_pair() (u64, u64) {
	return 81, 82
}

@[noinline]
fn append_multi_decl_addresses(mut out []&u64, first bool) {
	a, b := escape_decl_pair()
	out << &a
	out << &b
	c, d := u64(83), u64(84)
	out << &c
	out << &d
	e, f := if first { u64(85), u64(86) } else { u64(87), u64(88) }
	out << &e
	out << &f
	g, h := match first {
		true { u64(89), u64(90) }
		else { u64(91), u64(92) }
	}
	out << &g
	out << &h
}

// V has no shadowing, so the name is declared again in a sibling scope: there it is a
// pointer and must not inherit the heap storage of the earlier value.
@[noinline]
fn append_multi_reused_name_addresses(mut out []&u64, target &u64) {
	{
		x := u64(93)
		out << &x
	}
	{
		x, other := target, u64(95)
		assert *x == 94
		out << &other
	}
}

fn test_multi_declarations_keep_retained_binding_addresses() {
	mut out := []&u64{}
	append_multi_decl_addresses(mut out, true)
	append_multi_decl_addresses(mut out, false)
	target := u64(94)
	append_multi_reused_name_addresses(mut out, &target)
	_ = use_the_stack(10)
	assert out.map(*it) == [u64(81), 82, 83, 84, 85, 86, 89, 90, 81, 82, 83, 84, 87, 88, 91, 92,
		93, 95]
}

struct EscapingValueIterator {
	values []u64
mut:
	index int
}

fn (mut iter EscapingValueIterator) next() ?u64 {
	if iter.index == iter.values.len {
		return none
	}
	defer { iter.index++ }
	return iter.values[iter.index]
}

@[noinline]
fn append_for_in_binding_addresses(mut out []&u64, mut indices []&int) {
	mut input := [u64(101), 102]
	for i, value in input {
		out << &value
		indices << &i
	}
	for i, value in EscapingValueIterator{ values: [u64(103), 104] } {
		out << &value
		indices << &i
	}
	for value in 105 .. 107 {
		indices << &value
	}
	for mut value in input {
		out << &value
		value += 10
	}
	assert input == [u64(111), 112]
}

fn test_for_in_values_and_indices_keep_retained_binding_addresses() {
	mut out := []&u64{}
	mut indices := []&int{}
	append_for_in_binding_addresses(mut out, mut indices)
	_ = use_the_stack(10)
	assert out.map(*it) == [u64(101), 102, 103, 104, 111, 112]
	assert indices.map(*it) == [0, 1, 0, 1, 105, 106]
	assert out[0] != out[1]
	assert indices[0] != indices[1]
}

@[noinline]
fn append_select_binding_address(mut out []&u64, values chan u64) {
	select {
		value := <-values {
			out << &value
		}
	}
}

fn test_select_receive_values_keep_retained_binding_addresses() {
	mut out := []&u64{}
	values := chan u64{cap: 2}
	values <- 121
	values <- 122
	append_select_binding_address(mut out, values)
	append_select_binding_address(mut out, values)
	_ = use_the_stack(10)
	assert out.map(*it) == [u64(121), 122]
	assert out[0] != out[1]
}

struct HeapCaptureValue {
mut:
	value u64
}

fn test_heap_value_captures_snapshot_the_semantic_value() {
	mut scalar := u64(131)
	mut addresses := []&u64{}
	addresses << &scalar
	read_scalar := fn [scalar] () u64 {
		return scalar
	}
	scalar = 132
	assert read_scalar() == 131
	assert *addresses[0] == 132
	mut record := HeapCaptureValue{ value: 133 }
	mut records := []&HeapCaptureValue{}
	records << &record
	read_record := fn [record] () u64 {
		return record.value
	}
	record.value = 134
	assert read_record() == 133
	assert records[0].value == 134
	mut buffer := [u64(141), 142]!
	addresses << unsafe { &buffer[0] }
	bump_buffer := fn [mut buffer] () u64 {
		buffer[0]++
		return buffer[0]
	}
	// A mutable capture of a fixed array shares its storage.
	buffer[0] = 143
	assert bump_buffer() == 144
	assert bump_buffer() == 145
	assert buffer[0] == 145
	assert *addresses[1] == 145
}

@[noinline]
fn append_map_binding_addresses(mut out []&u64, mut keys []&int) {
	values := {
		7: u64(151)
		8: u64(152)
	}
	for key, value in values {
		keys << &key
		out << &value
	}
	rows := {
		'one': [u64(153), 154]!
	}
	for _, row in rows {
		out << unsafe { &row[0] }
		out << unsafe { &row[1] }
	}
	mut borrowed := {
		'one': u64(155)
	}
	for _, mut value in borrowed {
		out << &value
		value++
	}
	assert borrowed['one'] == 156
}

fn test_map_values_and_keys_keep_retained_binding_addresses() {
	mut out := []&u64{}
	mut keys := []&int{}
	append_map_binding_addresses(mut out, mut keys)
	_ = use_the_stack(10)
	mut copied := out.map(*it)
	copied.sort()
	assert copied == [u64(151), 152, 153, 154, 156]
	mut copied_keys := keys.map(*it)
	copied_keys.sort()
	assert copied_keys == [7, 8]
}

@[noinline]
fn append_fixed_mutable_iteration_addresses(mut out []&u64) {
	mut first := [u64(161), 162]!
	for mut item in first {
		out << &item
		item += 10
	}
	mut second := [u64(163), 164]!
	for mut item in second {
		out << &item
		item += 10
	}
	mut third := [u64(165), 166]!
	mut delayed := unsafe { &u64(nil) }
	for mut item in third {
		delayed = &item
		item += 10
	}
	out << delayed
	assert first == [u64(171), 172]!
	assert second == [u64(173), 174]!
	assert third == [u64(175), 176]!
	assert sizeof(first) == 2 * sizeof(u64)
	mut fourth := [u64(167), 168, 169]!
	for mut item in fourth[1..] {
		out << &item
		item += 10
	}
	assert fourth == [u64(167), 178, 179]!
	for mut item in escaping_fixed_iteration_values() {
		out << &item
		item += 10
	}
}

fn escaping_fixed_iteration_values() [2]u64 {
	return [u64(177), 178]!
}

fn test_mutable_fixed_iteration_retains_backing_array_storage() {
	mut out := []&u64{}
	append_fixed_mutable_iteration_addresses(mut out)
	_ = use_the_stack(10)
	assert out.map(*it) == [u64(171), 172, 173, 174, 176, 178, 179, 187, 188]
}

@[noinline]
fn append_pointer_arithmetic_addresses(mut out []&u64) {
	mut items := [u64(191), 192, 193]!
	out << unsafe { &items[0] + 1 }
	out << unsafe { 1 + &items[1] }
	out << unsafe { &items[2] - 2 }
	base := unsafe { &items[0] }
	advanced := unsafe { (base + 1) + 1 }
	out << advanced
	items[1] = 194
	assert sizeof(items) == 3 * sizeof(u64)
}

fn test_pointer_arithmetic_retains_backing_array_storage() {
	mut out := []&u64{}
	append_pointer_arithmetic_addresses(mut out)
	_ = use_the_stack(10)
	assert out.map(*it) == [u64(194), 193, 191, 193]
}

fn failing_retained_error(message string) !u64 {
	return error(message)
}

@[noinline]
fn append_implicit_error_addresses(mut out []&IError) {
	err := u64(201)
	failing_retained_error('outer') or {
		out << &err
		assert typeof(err).name == 'IError'
		failing_retained_error('inner') or {
			out << &err
			assert typeof(err).name == 'IError'
			0
		}
		out << &err
		0
	}
	if value := failing_retained_error('guard') {
		_ = value
	} else {
		out << &err
	}
	value := if value := failing_retained_error('value guard') {
		value
	} else {
		out << &err
		u64(0)
	}
	assert value == 0
	assert err == 201
}

@[noinline]
fn returned_implicit_error_address() &IError {
	failing_retained_error('returned') or { return &err }
	panic('unexpected success')
}

fn test_implicit_result_errors_keep_retained_binding_addresses() {
	mut out := []&IError{}
	append_implicit_error_addresses(mut out)
	out << returned_implicit_error_address()
	_ = use_the_stack(10)
	assert out.map((*it).msg()) == ['outer', 'inner', 'outer', 'guard', 'value guard', 'returned']
	assert out[0] == out[2]
	assert out[0] != out[1]
}

struct RetainedMethodRecord {
mut:
	value u64
}

type RetainedMethodAlias = RetainedMethodRecord

struct RetainedMethodHolder {
	record RetainedMethodRecord
}

fn (record &RetainedMethodRecord) identity() &RetainedMethodRecord {
	return record
}

fn (record &RetainedMethodRecord) field_addresses() []&u64 {
	return [&record.value]
}

@[noinline]
fn append_reference_method_results(mut out []&RetainedMethodRecord, mut fields []&u64) {
	mut local := RetainedMethodRecord{ value: 211 }
	out << local.identity()
	fields << local.field_addresses()
	local.value = 214
	alias := RetainedMethodAlias(RetainedMethodRecord{ value: 212 })
	out << alias.identity()
	explicit := &RetainedMethodRecord{ value: 213 }
	out << explicit.identity()
	holder := RetainedMethodHolder{ record: RetainedMethodRecord{ value: 215 } }
	out << holder.record.identity()
}

fn test_reference_method_results_keep_receiver_storage() {
	mut out := []&RetainedMethodRecord{}
	mut fields := []&u64{}
	append_reference_method_results(mut out, mut fields)
	_ = use_the_stack(10)
	assert out.map(it.value) == [u64(214), 212, 213, 215]
	assert fields.map(*it) == [u64(214)]
}
