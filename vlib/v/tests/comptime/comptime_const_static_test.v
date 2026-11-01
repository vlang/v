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
