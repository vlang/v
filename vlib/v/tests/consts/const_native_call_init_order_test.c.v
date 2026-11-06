#include <stdlib.h>

fn C.abs(i32) i32

const native_call_dependent = native_call_seed + 1
const native_call_seed = native_call_build_seed()

fn native_call_build_seed() int {
	return int(C.abs(-41))
}

fn abs() int {
	return native_call_dependent
}

fn test_native_call_does_not_read_same_named_v_function_constants() {
	assert abs() == 42
}
