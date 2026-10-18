#include "@DIR/c_immutable_out_param_test.h"

fn C.immutable_out_param_write_one(&int) int
fn C.immutable_out_param_write_i32(&i32) int

fn immutable_c_out_param_compared() bool {
	value := 0
	C.immutable_out_param_write_one(&value)
	return value != 0
}

fn immutable_c_out_param_in_condition() bool {
	value := 0
	if C.immutable_out_param_write_one(&value) != 0 {
		return false
	}
	return value == 1
}

fn immutable_c_out_param_alias() bool {
	value := 0
	pointer := &value
	C.immutable_out_param_write_one(pointer)
	return value > 0
}

fn test_c_out_param_writes_invalidate_immutable_scalar_folding() {
	assert immutable_c_out_param_compared()
	assert immutable_c_out_param_in_condition()
	assert immutable_c_out_param_alias()
	mut value := 0
	C.immutable_out_param_write_one(&value)
	assert value == 1
	narrow := i32(0)
	C.immutable_out_param_write_i32(&narrow)
	assert narrow == 1
}
