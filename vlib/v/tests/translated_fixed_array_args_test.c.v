@[translated]
module main

#include <string.h>

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
	mut named_pointer := unsafe { &[row_len]int(nil) }
	named_pointer = plain_rows
	assert named_pointer == plain_rows
	cell := DecayedCell(42)
	pointers := [&cell]!
	assert first_pointer_value(pointers) == 42
}
