#include "@VMODROOT/vlib/v/tests/testdata/c_struct_array_fields.h"

const c_array_field_size = 8

@[typedef]
struct C.CArrayFieldRecord {
mut:
	Data4    [8]u8
	Named    [c_array_field_size]u8
	Computed [c_array_field_size + 1]u8
	Matrix   [2][4]u8
}

@[typedef]
union C.CArrayFieldUnion {
mut:
	Bytes [8]u8
	Words [2]u32
}

fn C.c_array_field_sum(&C.CArrayFieldRecord) int

fn test_c_struct_uppercase_fixed_array_fields() {
	mut record := C.CArrayFieldRecord{}
	assert sizeof(C.CArrayFieldRecord) == 33
	record.Data4[7] = 11
	record.Named[7] = 13
	record.Computed[8] = 17
	record.Matrix[1][3] = 19
	assert record.Data4[7] == 11
	assert record.Named[7] == 13
	assert record.Computed[8] == 17
	assert record.Matrix[1][3] == 19
	assert C.c_array_field_sum(&record) == 60
}

fn test_c_union_uppercase_fixed_array_fields() {
	mut value := C.CArrayFieldUnion{}
	assert sizeof(C.CArrayFieldUnion) == 8
	// C union access is unsafe; read only the member most recently written.
	unsafe {
		value.Bytes[7] = 23
	}
	assert unsafe { value.Bytes[7] } == 23
	unsafe {
		value.Words[1] = 29
	}
	assert unsafe { value.Words[1] } == 29
}
