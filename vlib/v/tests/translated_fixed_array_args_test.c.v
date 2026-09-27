@[translated]
module main

#include <string.h>

fn C.memset(dest voidptr, value int, count usize) voidptr

fn sum(values &int) int { return unsafe { values[0] + values[1] } }

fn first_byte(values &u8) u8 { return unsafe { values[0] } }

fn test_translated_fixed_array_arguments() {
	mut values := [20, 22]!
	assert sum(values) == 42
	C.memset(values, 0, sizeof(values))
	assert values == [0, 0]!
	chars := [char(65), char(66)]!
	assert first_byte(chars) == 65
}

fn test_translated_fixed_array_comparisons() {
	values := [char(3), char(5)]!
	pointer := unsafe { &values[0] }
	end := unsafe { &values[1] }
	assert pointer == values
	assert values == pointer
	assert end != values
	assert values != end
}

type DecayedCell = int

fn sum_row(values &[2]int) int {
	return unsafe { values[0] + values[1] }
}

fn sum_alias_row(values &[2]DecayedCell) int {
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
	copy := rows
	assert pointer != copy
	assert copy != pointer
	plain_rows := [[20, 22]!, [3, 5]!]!
	assert sum_alias_row(plain_rows) == 42
	cell := DecayedCell(42)
	pointers := [&cell]!
	assert first_pointer_value(pointers) == 42
}
