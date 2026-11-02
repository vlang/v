import constevalhelper as evaluated

@[comptime]
fn const_crc32_table(poly u32) [256]u32 {
	mut table := [256]u32{}
	for i in 0 .. 256 {
		mut crc := u32(i)
		for _ in 0 .. 8 {
			crc = if crc & 1 == 1 { (crc >> 1) ^ poly } else { crc >> 1 }
		}
		table[i] = crc
	}
	return table
}

const imported_pair = evaluated.make_pair(10)

const computed_crc = const_crc32_table(0xedb88320)

@[comptime]
fn const_size(value int) int {
	mut result := 0
	for i := 0; i < value; i++ {
		if i == 2 { continue }
		result += i
		if result > 10 { break }
	}
	return result
}

const computed_size = const_size(5)

@[comptime]
fn const_wrap8(a u8, b u8) u8 { return a + b }

@[comptime]
fn const_half8(a u8, b u8) u8 { return (a + b) / 2 }

@[comptime]
fn const_widen8(a u8, b u8) int { return int(a + b) }

@[comptime]
fn const_invert8(a u8) int { return int(~a) / 2 }

@[comptime]
fn const_local_invert8(a u8) int {
	converted := ~a
	return int(converted) / 2
}

@[comptime]
fn const_unsigned(a u64) u64 { return (a >> 1) / 3 }

const computed_wrap = const_wrap8(250, 10)
const computed_half = const_half8(250, 10)
const computed_widen = const_widen8(250, 10)
const computed_invert = const_invert8(128)
const computed_local_invert = const_local_invert8(128)
const computed_unsigned = const_unsigned(0xffffffffffffffff)

struct ConstRecord {
mut:
	name  string
	flags [2]bool
	table [2]u32
}

@[comptime]
fn const_record(enabled bool, suffix string) ConstRecord {
	mut record := ConstRecord{
		name:  'a\n' + suffix
		flags: [false, true]!
		table: [
			u32(1),
			2,
		]!
	}
	record.flags[0] = !enabled
	mut copy := record
	copy.table[0] = 99
	return if enabled { record } else { copy }
}

const computed_record = const_record(true, 'b')

@[comptime]
fn const_fib(n int) int {
	if n <= 1 { return n }
	return const_fib(n - 1) + const_fib(n - 2)
}

const computed_fib = const_fib(10)

@[comptime]
fn const_shift(a u32, n int) u32 { return (a << n) ^ (a >> n) }

@[comptime]
fn const_guard(a bool) bool {
	if a { return true }
	panic('unreachable during evaluation')
}

const computed_shift_negative = const_shift(0x80000001, -1)
const computed_shift_zero = const_shift(0x80000001, 0)
const computed_shift_last = const_shift(0x80000001, 31)
const computed_shift_width = const_shift(0x80000001, 32)
const computed_guard = const_guard(true)

@[comptime]
fn const_compare8(a u8, b u8) bool { return a + b == u8(4) }

@[comptime]
fn const_mixed_compare(a u64, b int) bool { return a > b }

@[comptime]
fn const_size_compare(a int, b uint) bool { return a < b }

@[comptime]
fn const_rune_half(a rune, b rune) rune { return (a + b) / 2 }

@[comptime]
fn const_literal_compare(a u32) bool { return a < 0xffffffffffffffff }

@[comptime]
fn const_complement_literal(a int) bool { return a < ~0xffffffff }

struct ConstArithmeticState {
mut:
	value u64
}

@[comptime]
fn const_aggregate_compound() u64 {
	mut values := [u64(0xffffffffffffffff)]!
	mut state := ConstArithmeticState{ value: u64(0xffffffffffffffff) }
	values[0] /= 3
	state.value /= 3
	return values[0] + state.value
}

@[comptime]
fn const_aggregate_division() u64 {
	values := [u64(0xffffffffffffffff)]!
	return values[0] / 3
}

@[comptime]
fn const_aggregate_comparison() bool {
	values := [u64(0xffffffffffffffff)]!
	return (values[0] + 0) > 0
}

@[comptime]
fn const_conditional_division(flag bool) u64 {
	return (if flag { u64(0) } else { 0xffffffffffffffff }) / 3
}

type ConstInteger = int

@[comptime]
fn const_alias(value ConstInteger) ConstInteger {
	mut n := value
	for i in ConstInteger(0) .. ConstInteger(2) { n += i }
	n++
	return -n
}

const computed_compare = const_compare8(250, 10)
const computed_mixed_compare = const_mixed_compare(0xffffffffffffffff, -1)
const computed_size_compare = const_size_compare(-1, 1)
const computed_rune_half = const_rune_half(rune(0xffffffff), rune(1))
const computed_literal_compare = const_literal_compare(0xffffffff)
const computed_complement_literal = const_complement_literal(-1)
const computed_aggregate_compound = const_aggregate_compound()
const computed_aggregate_division = const_aggregate_division()
const computed_aggregate_comparison = const_aggregate_comparison()
const computed_conditional_division = const_conditional_division(false)
const computed_alias = const_alias(ConstInteger(3))

@[comptime]
fn const_unsigned_range(low u64, high u64) u64 {
	mut count := u64(0)
	for i in low .. high { count += i >> 63 }
	return count
}

const computed_unsigned_range = const_unsigned_range(0xfffffffffffffffd, 0xffffffffffffffff)

const stored_byte = u8(250) + u8(10)
const stored_chain = stored_byte + u8(253)

@[comptime]
fn const_read_stored_values() int {
	return int(stored_byte) * 1000 + int(stored_chain)
}

const computed_stored_values = const_read_stored_values()

@[comptime]
fn const_wide_seed() int {
	return 256
}

const computed_direct_bound = u8(const_wide_seed()) + 1
const computed_bound_source = const_wide_seed()
const computed_indirect_bound = u8(computed_bound_source) + 1
const computed_bound_alias = computed_indirect_bound
const computed_bound_quotient = 256 / (256 - u8(computed_bound_source))

@[comptime]
fn const_symbolic_lengths() int {
	a := [computed_direct_bound]u8{}
	b := [computed_indirect_bound + 1]u8{}
	c := [(computed_bound_alias)]u8{}
	return a.len + b.len + c.len
}

const computed_symbolic_lengths = const_symbolic_lengths()
const imported_symbolic_length = evaluated.symbolic_length()

@[comptime]
fn const_else_if_division(flag int) u64 {
	return (if flag == 0 {
		u64(0)
	} else if flag == 1 {
		u64(0xffffffffffffffff)
	} else {
		0xffffffffffffffff
	}) / 3
}

const computed_else_if_middle = const_else_if_division(1)
const computed_else_if_final = const_else_if_division(2)

@[comptime]
fn const_stored_sizeof_complement() u64 {
	n := sizeof(u8)
	return u64(~n)
}

@[comptime]
fn const_direct_sizeof_complement() u64 {
	return u64(~sizeof(u8))
}

@[comptime]
fn const_stored_sizeof_infix() u64 {
	n := sizeof(u8) + u32(1)
	return u64(~n)
}

@[comptime]
fn const_direct_sizeof_infix() u64 {
	return u64(~(sizeof(u8) + u32(1)))
}

const computed_stored_sizeof = const_stored_sizeof_complement()
const computed_direct_sizeof = const_direct_sizeof_complement()
const computed_stored_sizeof_infix = const_stored_sizeof_infix()
const computed_direct_sizeof_infix = const_direct_sizeof_infix()
const fixed_width = 2
const computed_array_table = const_array_table()

@[comptime]
fn const_array_table() [fixed_width][fixed_width]u8 {
	return [[u8(1), 2]!, [u8(3), 4]!]!
}

const symbolic_width = 3
const computed_imported_arrays = const_imported_array_values()

@[comptime]
fn const_imported_array_values() int {
	mut local := evaluated.symbolic_table
	local = evaluated.make_symbolic_table()
	aliased := evaluated.SymbolicPair([u8(1), 2]!)
	mut values := [symbolic_width]u8{}
	values[2] = 7
	return local[1] + aliased[0] + evaluated.last_symbolic(values)
}

const computed_imported_record_length = evaluated.record_length(evaluated.SymbolicRecord{})
const computed_imported_nested_length = evaluated.nested_length(evaluated.SymbolicOuter{})
const computed_imported_explicit_record = const_imported_record_value(5)

@[comptime]
fn const_imported_record_value(symbolic_width int) int {
	value := evaluated.SymbolicRecord{ value: symbolic_width }
	return evaluated.record_length(value) * 10 + value.value
}

const widened_shift_count = 31
const ordinary_shift_count = u8(250 + 37)
const required_shift_count = const_shift_count()

@[comptime]
fn const_shift_count() u8 {
	return u8(250 + 37)
}

@[comptime]
fn const_shift_width_cases(bound int, count int) [18]bool {
	stored := 1 << 31
	return [
		(1 << 31) > bound,
		(0 << 31) > bound,
		(1 << (30 + 1)) > bound,
		(1 << widened_shift_count) > bound,
		(1 << required_shift_count) > bound,
		((1 << 31) + 0) > bound,
		((1 << 31) & 1) > bound,
		(1 << 63) > 0,
		~(1 << 31) > 0,
		((1) << 31) > bound,
		(1 << (1 << 5)) > bound,
		(1 << int(31)) > bound,
		(1 << const_shift_count()) > bound,
		(1 << count) > bound,
		(1 << ordinary_shift_count) > bound,
		(u64(1) << 31) > bound,
		stored > bound,
		(1 << ~(-32)) > bound,
	]!
}

@[comptime]
fn const_shadowed_shift_count(widened_shift_count int, bound int) bool {
	return (1 << widened_shift_count) > bound
}

@[comptime]
fn const_large_literal_shifts() [2]u64 {
	return [u64(1 << 32), u64(4294967297 << 31)]!
}

const computed_shift_width_cases = const_shift_width_cases(-1, 31)
const computed_shadowed_shift_count = const_shadowed_shift_count(30, -1)
const computed_large_literal_shifts = const_large_literal_shifts()

fn test_comptime_struct_and_shift_parity() {
	assert computed_imported_record_length == 2
	assert computed_imported_record_length == evaluated.record_length(evaluated.SymbolicRecord{})
	assert computed_imported_nested_length == 2
	assert computed_imported_nested_length == evaluated.nested_length(evaluated.SymbolicOuter{})
	assert computed_imported_explicit_record == 25
	assert computed_imported_explicit_record == const_imported_record_value(5)
	assert !computed_shift_width_cases[0]
	assert computed_shift_width_cases[7]
	assert computed_shift_width_cases == const_shift_width_cases(-1, 31)
	assert !computed_shadowed_shift_count
	assert computed_shadowed_shift_count == const_shadowed_shift_count(30, -1)
	assert computed_large_literal_shifts == [u64(4294967296), u64(9223372039002259456)]!
	assert computed_large_literal_shifts == const_large_literal_shifts()
}

const computed_selector_local = evaluated.local_selector()
const computed_selector_parameter = evaluated.parameter_selector(evaluated.SelectorCell{ value: 11 })
const computed_selector_qualified = evaluated.qualified_selector()

fn test_comptime_selector_namespace_parity() {
	assert computed_selector_local == 7
	assert computed_selector_local == evaluated.local_selector()
	assert computed_selector_parameter == 11
	assert computed_selector_parameter == evaluated.parameter_selector(evaluated.SelectorCell{ value: 11 })
	assert computed_selector_qualified == 7
	assert computed_selector_qualified == evaluated.qualified_selector()
}

const computed_literal_logical_shifts = const_literal_logical_shifts(1)
const ordinary_literal_logical_shift = -5 >>> 1
const ordinary_literal_logical_table = [u64(-5 >>> 0), u64(-5 >>> 31), u64(-5 >>> 32)]!

@[comptime]
fn const_literal_logical_shifts(count int) [12]u64 {
	stored := -5 >>> 1
	named := -5
	return [u64(stored), u64((-5) >>> count), u64(-5 >>> 0), u64(-5 >>> 31), u64(-5 >>> 32),
		u64(-5 >>> -1), u64(~(5 >>> 1)), u64(int(-5) >>> 1), u64(named >>> 1), u64(i64(-5) >>> 1),
		u64(i32(-5) >>> 1), u64(i8(-5) >>> 1)]!
}

const computed_array_once = const_array_once()
const computed_nested_array_once = const_nested_array_once()
const computed_array_narrowing = const_array_narrowing()

@[comptime]
fn const_array_once() [3]int {
	mut n := 0
	a := [if true {
		n += 1
		n
	} else {
		0
	}, if true {
		n += 1
		n
	} else {
		0
	}]!
	return [a[0], a[1], n]!
}

@[comptime]
fn const_nested_array_once() int {
	mut n := 0
	a := [[if true {
		n += 1
		n
	} else {
		0
	}]!]!
	return a[0][0] * 10 + n
}

@[comptime]
fn const_array_narrowing() int {
	n := u8(128)
	a := [~n]!
	return int(a[0])
}

fn test_comptime_literal_shifts_and_array_evaluation_order() {
	assert computed_literal_logical_shifts[0] == u64(2147483645)
	assert computed_literal_logical_shifts[4] == 0
	assert computed_literal_logical_shifts[6] == u64(4294967293)
	assert computed_literal_logical_shifts == const_literal_logical_shifts(1)
	assert ordinary_literal_logical_shift == u32(2147483645)
	assert ordinary_literal_logical_table == [u64(4294967291), 1, 0]!
	assert computed_array_once == [1, 2, 2]!
	assert computed_array_once == const_array_once()
	assert computed_nested_array_once == 11
	assert computed_nested_array_once == const_nested_array_once()
	assert computed_array_narrowing == 127
	assert computed_array_narrowing == const_array_narrowing()
}

struct ConstMutationCell {
mut:
	value  int
	values [3]int
}

struct ConstMutationBox {
mut:
	cells [3]ConstMutationCell
}

const computed_mutation_targets = const_mutation_targets()

@[comptime]
fn const_mutation_targets() [11]int {
	mut compound := [3]int{}
	mut index := 0
	compound[if true {
		index += 1
		index
	} else {
		0
	}] += 7
	compound_result := compound[1] * 100 + compound[2] * 10 + index
	mut postfix := [3]int{}
	index = 0
	postfix[if true {
		index += 1
		index
	} else {
		0
	}]++
	postfix_result := postfix[1] * 100 + postfix[2] * 10 + index
	mut decrement := [0, 7, 0]!
	index = 0
	decrement[if true {
		index += 1
		index
	} else {
		0
	}]--
	decrement_result := decrement[1] * 100 + index
	mut nested := [3][3]int{}
	index = 0
	nested[if true {
		index += 1
		index
	} else {
		0
	}][1] = 7
	nested_result := nested[1][1] * 100 + nested[2][1] * 10 + index
	mut same_root := [3]int{}
	same_root[if true {
		same_root[0] = 5
		1
	} else {
		0
	}] = 7
	same_root_result := same_root[0] * 10 + same_root[1]
	mut read_root := [2]int{}
	read_result := read_root[if true {
		read_root[0] = 5
		0
	} else {
		1
	}]
	mut cells := [3]ConstMutationCell{}
	copy := cells
	index = 0
	cells[if true {
		index += 1
		index
	} else {
		0
	}].value += 7
	copy_result := copy[1].value * 1000 + cells[1].value * 100 + index
	mut box := ConstMutationBox{}
	index = 0
	box.cells[if true {
		index += 1
		index
	} else {
		0
	}].values[if true {
		index += 1
		index
	} else {
		0
	}]++
	nested_postfix_result := box.cells[1].values[2] * 100 + index
	mut narrow := [u8(0), 255, 0]!
	index = 0
	narrow[if true {
		index += 1
		index
	} else {
		0
	}]++
	narrow_result := int(narrow[1]) * 100 + index
	mut live_index := [3]int{}
	index = 1
	live_index[index] += if true {
		index = 2
		live_index[0] = 5
		7
	} else {
		0
	}
	live_index_result := live_index[0] * 1000 + live_index[1] * 100 + live_index[2] * 10 + index
	mut current_leaf := [3]int{}
	current_leaf[1] += if true {
		current_leaf[1] = 5
		7
	} else {
		0
	}
	return [compound_result, postfix_result, decrement_result, nested_result, same_root_result,
		copy_result, nested_postfix_result, narrow_result, live_index_result, read_result,
		current_leaf[1]]!
}

fn test_comptime_mutation_targets_match_runtime() {
	assert computed_mutation_targets == [701, 101, 601, 701, 57, 701, 102, 1, 5072, 5, 12]!
	assert computed_mutation_targets == const_mutation_targets()
}

const computed_local_promotion = const_local_promotion(255)
const computed_range_bounds = const_range_bounds()

@[comptime]
fn const_local_promotion(input int) int {
	value := input + u8(1)
	return value
}

@[comptime]
fn const_range_bounds() [3]int {
	mut n := 3
	mut live := 0
	for _ in 0 .. n {
		n = 1
		live++
	}
	n = 3
	mut expression := 0
	for _ in 0 .. (n + 0) {
		n = 1
		expression++
	}
	n = 3
	mut conditional := 0
	for _ in 0 .. if true { n } else { 0 } {
		n = 1
		conditional++
	}
	return [live, expression, conditional]!
}

fn test_comptime_local_promotion_and_range_bounds_match_runtime() {
	assert computed_local_promotion == 256
	assert computed_local_promotion == const_local_promotion(255)
	assert computed_range_bounds == [1, 3, 3]!
	assert computed_range_bounds == const_range_bounds()
}

const computed_typed_range_bounds = const_typed_range_bounds()

@[comptime]
fn const_typed_range_bounds() [9]int {
	mut results := [9]int{}
	for _ in u8(0) .. u16(256) {
		results[0] = 1
		break
	}
	negative := i16(-1)
	for _ in u8(0) .. negative {
		results[1] = 1
		break
	}
	for _ in i64(-1) .. u64(1) {
		results[2] = 1
		break
	}
	for _ in u64(0) .. 1 {
		results[3] = 1
		break
	}
	for _ in int(0) .. u8(255) + u8(1) {
		results[4] = 1
		break
	}
	mut limit := u16(2)
	for _ in u8(0) .. limit {
		results[5]++
		limit = 256
		if results[5] == 2 {
			break
		}
	}
	for _ in i64(-1) .. sizeof(u8) {
		results[6] = 1
		break
	}
	for _ in i64(-1) .. (sizeof(u8)) {
		results[7] = 1
		break
	}
	maximum := u64(0xffffffffffffffff)
	for _ in i64(-1) .. maximum {
		results[8] = 1
		break
	}
	return results
}

fn test_comptime_range_bound_types_match_runtime() {
	assert computed_typed_range_bounds == [1, 0, 1, 1, 0, 2, 1, 1, 1]!
	assert computed_typed_range_bounds == const_typed_range_bounds()
}

const computed_staging_controls = const_staging_controls(250, 10)
const computed_staging_fits = const_staging_small(1, 2)
const computed_staging_signed = const_staging_signed(0)

@[comptime]
fn const_staging_controls(a u8, b u8) [5]int {
	stored := a + b
	explicit := int(stored + (if true { u8(1) } else { u8(1) }))
	compile_time := int((a + b) + ($if true { u8(1) } $else { u8(1) }))
	values := [a + b, if true { u8(1) } else { u8(1) }]!
	mut table := [300]int{}
	table[u8(250) + u8(10)] = if true { 7 } else { 0 }
	mut nested := [300][1]int{}
	nested[u8(250) + u8(10)][if true { 0 } else { 0 }] = 7
	return [explicit, compile_time, int(values[0]) + int(values[1]), table[4], nested[4][0]]!
}

@[comptime]
fn const_staging_small(a u8, b u8) int {
	return int((a + b) + (if true { u8(1) } else { u8(1) }))
}

@[comptime]
fn const_staging_signed(offset int) bool {
	stored := (1 << 40) + offset
	return stored < (if true { i64(-1) } else { i64(-1) })
}

fn test_comptime_staging_controls_match_runtime() {
	assert computed_staging_controls == [5, 261, 5, 0, 0]!
	assert computed_staging_controls == const_staging_controls(250, 10)
	assert computed_staging_fits == 4
	assert computed_staging_fits == const_staging_small(1, 2)
	assert !computed_staging_signed
	assert computed_staging_signed == const_staging_signed(0)
}

fn test_comptime_constants_match_runtime() {
	assert imported_pair == evaluated.make_pair(10)
	assert imported_pair == [u32(10), 17]!
	assert computed_crc == const_crc32_table(0xedb88320)
	assert computed_crc[1] == 0x77073096
	assert computed_crc[255] == 0x2d02ef8d
	assert computed_size == const_size(5)
	assert [computed_size]u8{}.len == 8
	assert computed_wrap == const_wrap8(250, 10)
	assert computed_half == const_half8(250, 10)
	assert computed_widen == const_widen8(250, 10)
	assert computed_invert == const_invert8(128)
	assert computed_local_invert == const_local_invert8(128)
	assert computed_unsigned == const_unsigned(0xffffffffffffffff)
	assert computed_record == const_record(true, 'b')
	assert computed_record.name == 'a\nb'
	assert computed_record.table[0] == 1
	assert computed_fib == const_fib(10)
	assert computed_shift_negative == const_shift(0x80000001, -1)
	assert computed_shift_zero == const_shift(0x80000001, 0)
	assert computed_shift_last == const_shift(0x80000001, 31)
	assert computed_shift_width == const_shift(0x80000001, 32)
	assert computed_unsigned_range == 2
	assert computed_unsigned_range == const_unsigned_range(0xfffffffffffffffd, 0xffffffffffffffff)
	assert computed_guard
	assert computed_compare
	assert computed_compare == const_compare8(250, 10)
	assert computed_mixed_compare
	assert computed_mixed_compare == const_mixed_compare(0xffffffffffffffff, -1)
	assert !computed_size_compare
	assert computed_size_compare == const_size_compare(-1, 1)
	assert computed_rune_half == 0
	assert computed_rune_half == const_rune_half(rune(0xffffffff), rune(1))
	assert computed_literal_compare
	assert computed_literal_compare == const_literal_compare(0xffffffff)
	assert computed_complement_literal
	assert computed_complement_literal == const_complement_literal(-1)
	assert computed_aggregate_compound == u64(12297829382473034410)
	assert computed_aggregate_compound == const_aggregate_compound()
	assert computed_aggregate_division == u64(6148914691236517205)
	assert computed_aggregate_division == const_aggregate_division()
	assert computed_aggregate_comparison
	assert computed_aggregate_comparison == const_aggregate_comparison()
	assert computed_conditional_division == u64(6148914691236517205)
	assert computed_conditional_division == const_conditional_division(false)
	assert computed_alias == const_alias(ConstInteger(3))
	assert computed_alias == ConstInteger(-5)
	assert computed_stored_values == 4001
	assert computed_stored_values == const_read_stored_values()
	assert computed_direct_bound == 1
	assert [computed_direct_bound]u8{}.len == computed_direct_bound
	assert computed_indirect_bound == 1
	assert [computed_indirect_bound]u8{}.len == computed_indirect_bound
	assert [computed_bound_alias]u8{}.len == 1
	assert computed_bound_quotient == 1
	assert [computed_bound_quotient]u8{}.len == computed_bound_quotient
	assert computed_symbolic_lengths == 4
	assert computed_symbolic_lengths == const_symbolic_lengths()
	assert imported_symbolic_length == 8
	assert imported_symbolic_length == evaluated.symbolic_length()
	cast_before_required := [u8(250 + 50) + computed_bound_source]u8{}
	assert cast_before_required.len == 300
	quotient_before_required := [256 / (256 - u8(250 + 6)) + computed_bound_source]u8{}
	assert quotient_before_required.len == 257
	imported_bound := [evaluated.symbolic_bound]u8{}
	assert imported_bound.len == 1
	assert computed_else_if_middle == u64(6148914691236517205)
	assert computed_else_if_middle == const_else_if_division(1)
	assert computed_else_if_final == u64(6148914691236517205)
	assert computed_else_if_final == const_else_if_division(2)
	assert computed_stored_sizeof == u64(4294967294)
	assert computed_stored_sizeof == const_stored_sizeof_complement()
	assert computed_direct_sizeof == const_direct_sizeof_complement()
	assert computed_stored_sizeof_infix == u64(4294967293)
	assert computed_stored_sizeof_infix == const_stored_sizeof_infix()
	assert computed_direct_sizeof_infix == const_direct_sizeof_infix()
	assert computed_array_table == [[u8(1), 2]!, [u8(3), 4]!]!
	assert computed_array_table == const_array_table()
	assert computed_imported_arrays == 10
	assert computed_imported_arrays == const_imported_array_values()
}
