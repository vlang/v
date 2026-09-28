@[translated]
module main

#include <string.h>

fn C.memset(dest voidptr, value int, count usize) voidptr

fn write_array_value(values &int, value int) {
	unsafe { *values = value }
}

struct ArrayPointerWriter {}

fn (_ ArrayPointerWriter) write(values &int, value int) {
	write_array_value(values, value)
}

fn read_array_value(values [2]int, ignored int) int {
	return values[0] + ignored
}

fn test_array_pointer_arguments_keep_the_original_storage() {
	flag := true
	mut values := [0, 0]!
	write_array_value(values, if flag { 42 } else { 1 })
	assert values[0] == 42
	writer := ArrayPointerWriter{}
	writer.write(values, if flag { 43 } else { 1 })
	assert values[0] == 43
	callback := write_array_value
	callback(values, if flag { 44 } else { 1 })
	assert values[0] == 44

	mut bytes := [4]u8{}
	C.memset(bytes, 23, if flag { usize(4) } else { usize(1) })
	assert bytes == [u8(23), 23, 23, 23]!
}

fn test_array_pointer_argument_keeps_its_original_index() {
	flag := true
	mut rows := [[0, 0]!, [0, 0]!]!
	mut index := 0
	write_array_value(rows[index], if flag {
		index = 1
		33
	} else {
		1
	})
	assert index == 1
	assert rows[0] == [33, 0]!
	assert rows[1] == [0, 0]!
}

fn test_array_value_argument_still_captures_its_original_value() {
	flag := true
	mut values := [7, 8]!
	result := read_array_value(values, if flag {
		values[0] = 99
		1
	} else {
		0
	})
	assert result == 8
	assert values[0] == 99
}
