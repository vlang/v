#flag -I @VEXEROOT/vlib/v/tests/pointers
#include "c_pointer_output_cast.h"

fn C.pointer_output_cast_probe(&voidptr)
fn C.pointer_output_pair_cast_probe(&voidptr, &voidptr) bool
fn C.pointer_output_pair_cast_valid(voidptr, voidptr) bool

struct PointerOutputCastHandles {
mut:
	child_stdout_read  &u32 = unsafe { nil }
	child_stdout_write &u32 = unsafe { nil }
}

fn test_c_pointer_output_cast_updates_caller_storage() {
	mut output := voidptr(unsafe { nil })
	C.pointer_output_cast_probe(voidptr(&output))
	assert !isnil(output)
	if !isnil(output) {
		assert unsafe { *&i64(output) } == 37
	}
}

fn test_c_pointer_output_pair_cast_updates_local_handles() {
	mut read_handle := voidptr(unsafe { nil })
	mut write_handle := voidptr(unsafe { nil })
	assert C.pointer_output_pair_cast_probe(voidptr(&read_handle), voidptr(&write_handle))
	assert C.pointer_output_pair_cast_valid(read_handle, write_handle)
}

fn test_c_pointer_output_pair_cast_updates_value_struct_fields() {
	mut handles := PointerOutputCastHandles{}
	assert C.pointer_output_pair_cast_probe(voidptr(&handles.child_stdout_read), voidptr(&handles.child_stdout_write))
	assert C.pointer_output_pair_cast_valid(handles.child_stdout_read, handles.child_stdout_write)
}

fn test_c_pointer_output_pair_cast_updates_heap_struct_fields() {
	mut handles := &PointerOutputCastHandles{}
	assert C.pointer_output_pair_cast_probe(voidptr(&handles.child_stdout_read), voidptr(&handles.child_stdout_write))
	assert C.pointer_output_pair_cast_valid(handles.child_stdout_read, handles.child_stdout_write)
}
