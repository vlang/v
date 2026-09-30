#include <string.h>

fn C.memset(dest voidptr, value int, count usize) voidptr

fn expression_byte_count(value usize) usize {
	return value
}

struct SizeReceiver {}

fn (receiver SizeReceiver) byte_count(value usize) usize {
	return value
}

fn test_sizeof_expression_in_function_arguments() {
	mut values := [20, 22]!
	pointer := unsafe { &values }
	assert expression_byte_count(sizeof(*pointer)) == sizeof([2]int)
	assert SizeReceiver{}.byte_count(sizeof(*pointer)) == sizeof([2]int)
	C.memset(unsafe { &values[0] }, 0, sizeof(*pointer))
	assert values == [0, 0]!
	assert expression_byte_count(sizeof(int)) == sizeof(int)
}
