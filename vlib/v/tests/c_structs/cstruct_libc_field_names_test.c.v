#include "@VEXEROOT/vlib/v/tests/testdata/c_libc_fields.h"

@[typedef]
struct C.LibcFieldRecord {
pub mut:
	index   u64
	select  u64
	malloc  u64
	exit    u64
	byte    u64
	int_str u64
	v_index u64
}

@[typedef]
union C.LibcFieldUnion {
pub mut:
	index  u64
	select u64
}

type LibcFieldAlias = C.LibcFieldRecord

type LibcFieldPointer = &LibcFieldAlias

struct VCollisionFields {
mut:
	index   int
	select  int
	malloc  int
	exit    int
	byte    int
	int_str int
}

struct CFieldWrapper {
	C.LibcFieldRecord
}

fn read_c_field_pointer(value LibcFieldPointer) u64 {
	return value.index
}

fn test_c_field_initializer_and_alias_pointer_access() {
	mut value := LibcFieldAlias{
		index:   11
		select:  13
		malloc:  17
		exit:    19
		byte:    23
		int_str: 29
		v_index: 31
	}
	assert value.index == 11
	assert value.select == 13
	assert value.malloc == 17
	assert value.exit == 19
	assert value.byte == 23
	assert value.int_str == 29
	assert value.v_index == 31
	value.index += 1
	value.select = 37
	mut pointer := &value
	pointer.index = 41
	assert read_c_field_pointer(pointer) == 41
	assert pointer.select == 37
	assert sizeof(value.index) == sizeof(u64)
	assert sizeof(pointer.index) == sizeof(u64)
	heap := &C.LibcFieldRecord{ index: 43, select: 47 }
	assert heap.index == 43 && heap.select == 47
}

fn test_c_union_field_initializer_and_write() {
	mut value := C.LibcFieldUnion{ index: 53 }
	// Union fields share storage and require unsafe access.
	assert unsafe { value.index } == 53
	unsafe {
		value.select = 59
	}
	assert unsafe { value.index } == 59
}

fn test_promoted_c_member_spelling_and_v_member_mangling() {
	mut wrapper := CFieldWrapper{ index: 61 }
	assert wrapper.index == 61
	wrapper.index = 67
	assert wrapper.LibcFieldRecord.index == 67
	mut value := VCollisionFields{ index: 71, select: 73, malloc: 79, exit: 83, byte: 89, int_str: 97 }
	value.index++
	assert value.index == 72 && value.select == 73 && value.malloc == 79
	assert value.exit == 83 && value.byte == 89 && value.int_str == 97
}
