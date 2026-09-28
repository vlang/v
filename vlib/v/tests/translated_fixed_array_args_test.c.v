@[translated]
module main

#include <string.h>
#include "@VMODROOT/vlib/v/tests/testdata/translated_fixed_array_args.h"

fn C.memset(dest voidptr, value int, count usize) voidptr

fn sum(values &int) int { return unsafe { values[0] + values[1] } }

fn first_byte(values &u8) u8 { return unsafe { values[0] } }

fn first_byte_row(values &[2]u8) u8 { return unsafe { values[0] } }

fn test_translated_fixed_array_arguments() {
	mut values := [20, 22]!
	assert sum(values) == 42
	C.memset(values, 0, sizeof(values))
	assert values == [0, 0]!
	chars := [char(65), char(66)]!
	assert first_byte(chars) == 65
	assert first_byte([char(65), char(66)]!) == 65
	rows := [[char(65), char(66)]!, [char(67), char(68)]!]!
	assert first_byte_row(rows) == 65
}

fn test_translated_fixed_array_voidptr_assignment() {
	mut pointer := unsafe { voidptr(nil) }
	pointer = [20, 22]!
	assert unsafe { (&int(pointer))[0] } == 20
}

type TranslatedVoidPtr = voidptr

fn test_translated_fixed_array_typed_pointer_assignments() {
	mut typed := unsafe { &int(nil) }
	typed = [20, 22]!
	assert unsafe { typed[0] } == 20
	mut aliased := unsafe { TranslatedVoidPtr(nil) }
	aliased = [20, 22]!
	assert unsafe { (&int(aliased))[0] } == 20
	mut bytes := unsafe { &u8(nil) }
	bytes = [char(65), char(66)]!
	assert first_byte(bytes) == 65
	mut byte_row := unsafe { &[2]u8(nil) }
	byte_row = [[char(65), char(66)]!, [char(67), char(68)]!]!
	assert first_byte_row(byte_row) == 65
	chars := [char(67), char(68)]!
	bytes = chars
	assert first_byte(bytes) == 67
	char_rows := [[char(69), char(70)]!, [char(71), char(72)]!]!
	byte_row = char_rows
	assert first_byte_row(byte_row) == 69
	mut named_byte_row := unsafe { &[row_len]u8(nil) }
	named_byte_row = char_rows
	assert first_byte_row(named_byte_row) == 69
}

fn make_runtime_row(value int) [2]int {
	return [value, value + 1]!
}

fn test_translated_nested_literal_pointer_storage_survives_assignment() {
	mut row := unsafe { &[2]int(nil) }
	row = [make_runtime_row(20), make_runtime_row(30)]!
	assert sum_row(row) == 41
	assert row != [make_runtime_row(1), make_runtime_row(2)]!
	mut single_row := unsafe { &int(nil) }
	single_row = make_runtime_row(10)
	assert sum(single_row) == 21
}

fn test_translated_fixed_array_comparisons() {
	values := [char(3), char(5)]!
	pointer := unsafe { &values[0] }
	end := unsafe { &values[1] }
	assert pointer == values
	assert values == pointer
	assert end != values
	assert values != end
	assert pointer != [char(3), char(5)]!
	assert [char(3), char(5)]! != pointer
}

type DecayedCell = int

type Row = [2]int

type RowAlias = Row

const row_len = 2

fn sum_row(values &[2]int) int {
	return unsafe { values[0] + values[1] }
}

fn sum_alias_row(values &[2]DecayedCell) int {
	return unsafe { values[0] + values[1] }
}

fn sum_named_row(values &[row_len]int) int {
	return unsafe { values[0] + values[1] }
}

fn first_named_deep_row(values &[row_len][row_len]int) int {
	return unsafe { values[0][0] }
}

fn first_pointer_value(values &&int) int {
	return unsafe { *values[0] }
}

fn test_translated_array_decay_resolves_nested_aliases() {
	rows := [[DecayedCell(20), 22]!, [DecayedCell(3), 5]!]!
	assert sum_row(rows) == 42
	mut pointer := unsafe { &[2]int(nil) }
	pointer = rows
	assert sum_row(pointer) == 42
	assert pointer == rows
	assert rows == pointer
	assert pointer != [[20, 22]!, [3, 5]!]!
	copy := rows
	assert pointer != copy
	assert copy != pointer
	plain_rows := [[20, 22]!, [3, 5]!]!
	assert sum_alias_row(plain_rows) == 42
	assert sum_named_row(plain_rows) == 42
	mut alias_rows := [2]RowAlias{}
	alias_rows[0][0] = 20
	alias_rows[0][1] = 22
	alias_rows[1][0] = 3
	alias_rows[1][1] = 5
	assert sum_row(alias_rows) == 42
	assert sum_row([[20, 22]!, [3, 5]!]!) == 42
	assert sum_row([make_runtime_row(20), make_runtime_row(3)]!) == 41
	deep_rows := [[[20, 22]!, [3, 5]!]!, [[7, 11]!, [13, 17]!]!]!
	assert first_named_deep_row(deep_rows) == 20
	mut named_pointer := unsafe { &[row_len]int(nil) }
	named_pointer = plain_rows
	assert named_pointer == plain_rows
	cell := DecayedCell(42)
	pointers := [&cell]!
	assert first_pointer_value(pointers) == 42
}

type ByteRows = [2][2]char

type ByteRowsAlias = ByteRows

type ByteRowsAliasChain = ByteRowsAlias

fn first_alias_byte_row(rows ByteRowsAliasChain) u8 {
	mut pointer := unsafe { &[2]u8(nil) }
	pointer = rows
	assert first_byte_row(pointer) == first_byte_row(rows)
	return first_byte_row(rows)
}

fn test_translated_array_decay_unwraps_outer_alias_chains() {
	rows := [[char(65), char(66)]!, [char(67), char(68)]!]!
	assert first_alias_byte_row(rows) == 65
}

type DecayedIntPtr = &int

type DecayedIntPtrAlias = DecayedIntPtr

type DecayedRowPtr = &[2]int

type DecayedRowPtrAlias = DecayedRowPtr

interface DecaySink {
	sum(values DecayedIntPtrAlias) int
	sum_row(values DecayedRowPtrAlias) int
}

struct FixedArrayDecaySink {}

fn (sink FixedArrayDecaySink) sum(values DecayedIntPtrAlias) int {
	return sum(&int(values))
}

fn (sink FixedArrayDecaySink) sum_row(values DecayedRowPtrAlias) int {
	return sum_row(&[2]int(values))
}

fn test_translated_interface_pointer_alias_arguments_decay_fixed_arrays() {
	sink := DecaySink(FixedArrayDecaySink{})
	values := [20, 22]!
	assert sink.sum(values) == 42
	assert sink.sum([20, 22]!) == 42
	assert sink.sum(make_runtime_row(20)) == 41
	rows := [[20, 22]!, [3, 5]!]!
	assert sink.sum_row(rows) == 42
	assert sink.sum_row([[20, 22]!, [3, 5]!]!) == 42
	assert sink.sum_row([make_runtime_row(20), make_runtime_row(3)]!) == 41
}

fn C.translated_first_byte(values &u8) u8
fn C.translated_first_char(values &char) char
fn C.translated_first_byte_row(values &[2]u8) u8

fn test_translated_c_calls_cast_byte_compatible_fixed_arrays() {
	chars := [char(65), char(66)]!
	assert C.translated_first_byte(chars) == 65
	assert C.translated_first_byte([char(67), char(68)]!) == 67
	bytes := [u8(69), 70]!
	assert C.translated_first_char(bytes) == char(69)
	assert C.translated_first_char([u8(71), 72]!) == char(71)
	rows := [[char(73), char(74)]!, [char(75), char(76)]!]!
	assert C.translated_first_byte_row(rows) == 73
	assert C.translated_first_byte_row([[char(77), char(78)]!, [char(79), char(80)]!]!) == 77
}

interface VoidDecaySink {
	sum(values voidptr) int
	sum_alias(values TranslatedVoidPtr) int
}

struct FixedArrayVoidDecaySink {}

fn (sink FixedArrayVoidDecaySink) sum(values voidptr) int {
	return sum(unsafe { &int(values) })
}

fn (sink FixedArrayVoidDecaySink) sum_alias(values TranslatedVoidPtr) int {
	return sum(unsafe { &int(values) })
}

fn test_translated_interface_void_pointer_uses_source_array_type() {
	sink := VoidDecaySink(FixedArrayVoidDecaySink{})
	assert sink.sum_alias([20, 22]!) == 42
	assert sink.sum_alias(make_runtime_row(20)) == 41
	values := [20, 22]!
	assert sink.sum(values) == 42
	assert sink.sum([20, 22]!) == 42
	assert sink.sum(make_runtime_row(20)) == 41
	rows := [[20, 22]!, [3, 5]!]!
	assert sink.sum(rows) == 42
	assert sink.sum([[20, 22]!, [3, 5]!]!) == 42
	assert sink.sum([make_runtime_row(20), make_runtime_row(3)]!) == 41
	assert sink.sum_alias(values) == 42
	assert sink.sum_alias(rows) == 42
	assert sink.sum_alias([[20, 22]!, [3, 5]!]!) == 42
	assert sink.sum_alias([make_runtime_row(20), make_runtime_row(3)]!) == 41
}

fn C.translated_sum_ints(values &int) int
fn C.translated_mutate_ints(values DecayedIntPtrAlias)
fn C.translated_increment_int(value &int)

fn test_translated_c_int_pointer_arguments_convert_array_storage() {
	mut values := [20, 22]!
	assert C.translated_sum_ints(values) == 42
	assert C.translated_sum_ints([21, 22]!) == 43
	assert C.translated_sum_ints(make_runtime_row(22)) == 45
	C.translated_mutate_ints(values)
	assert values == [-7, 42]!
	mut rows := [[1, 2]!, [3, 4]!]!
	C.translated_mutate_ints(rows[1])
	assert rows == [[1, 2]!, [-7, 42]!]!
	mut scalar := 40
	C.translated_increment_int(&scalar)
	assert scalar == 41
	mut dynamic := [1, 2]
	C.translated_mutate_ints(dynamic.data)
	assert dynamic == [-7, 42]
}
