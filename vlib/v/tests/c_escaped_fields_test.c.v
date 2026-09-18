#include "@VMODROOT/vlib/v/tests/testdata/c_escaped_fields.h"

@[typedef]
union C.EscapedFieldEvent {
pub mut:
	@type   u32
	padding [16]u8
}

@[typedef]
struct C.EscapedFieldRecord {
pub mut:
	@type   u32
	@module u32
	bytes   [4]u8
}

type EscapedRecordAlias = C.EscapedFieldRecord

struct EscapedFieldHolder {
mut:
	record C.EscapedFieldRecord
}

struct EscapedVFields {
mut:
	@type   int
	@struct int
}

fn read_escaped_event_type(event &C.EscapedFieldEvent) u32 {
	return unsafe { event.@type }
}

fn test_escaped_c_union_field_read_and_write() {
	mut event := C.EscapedFieldEvent{}
	unsafe {
		event.@type = 7
	}
	assert read_escaped_event_type(&event) == 7
	initialized := C.EscapedFieldEvent{
		@type: 9
	}
	assert read_escaped_event_type(&initialized) == 9
}

fn test_escaped_c_struct_field_initializers_and_pointers() {
	mut record := C.EscapedFieldRecord{
		@type:   11
		@module: 13
	}
	record.@type += 1
	assert record.@type == 12
	assert record.@module == 13
	pointer := &record
	assert pointer.@type == 12
	assert sizeof(record.@type) == sizeof(u32)

	heap := &C.EscapedFieldRecord{
		@type:   17
		@module: 19
	}
	assert heap.@type == 17
	assert heap.@module == 19
}

fn test_escaped_c_struct_field_with_fixed_array_initializer() {
	bytes := [u8(1), 2, 3, 4]!
	record := C.EscapedFieldRecord{
		@type:   23
		@module: 29
		bytes:   bytes
	}
	assert record.@type == 23
	assert record.@module == 29
	assert record.bytes == bytes
}

fn test_escaped_c_fields_through_aliases_and_nested_values() {
	alias := EscapedRecordAlias{
		@type: 31
	}
	assert alias.@type == 31
	mut holder := EscapedFieldHolder{
		record: C.EscapedFieldRecord{
			@type: 37
		}
	}
	holder.record.@type = 41
	assert holder.record.@type == 41
}

fn test_escaped_v_fields_are_unchanged() {
	mut value := EscapedVFields{
		@type:   43
		@struct: 47
	}
	value.@type++
	assert value.@type == 44
	assert value.@struct == 47
}
