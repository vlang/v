#include "@DIR/c_implicit_reference_branch_arg_test.h"

@[typedef]
struct C.OrderedBuffer {}

fn C.ordered_buffer_set(&C.OrderedBuffer, i32)
fn C.ordered_buffer_get(&C.OrderedBuffer) i32

struct OrderedBufferHolder {
	buffer C.OrderedBuffer
}

fn test_c_array_typedef_reference_before_branch_argument() {
	holder := OrderedBufferHolder{}
	condition := true
	C.ordered_buffer_set(holder.buffer, if condition { i32(42) } else { i32(1) })
	assert C.ordered_buffer_get(holder.buffer) == 42
}

fn test_c_array_typedef_reference_keeps_original_pointer_target() {
	mut first := OrderedBufferHolder{}
	mut second := OrderedBufferHolder{}
	mut selected := &first
	condition := true
	C.ordered_buffer_set(selected.buffer, if condition {
		selected = &second
		i32(43)
	} else {
		i32(1)
	})
	assert C.ordered_buffer_get(first.buffer) == 43
	assert C.ordered_buffer_get(second.buffer) == 0
	assert selected == &second
}

fn test_c_array_typedef_reference_keeps_original_index() {
	holders := [OrderedBufferHolder{}, OrderedBufferHolder{}]
	mut index := 0
	C.ordered_buffer_set(holders[index].buffer, match index {
		0 {
			index = 1
			i32(44)
		}
		else {
			i32(1)
		}
	})
	assert C.ordered_buffer_get(holders[0].buffer) == 44
	assert C.ordered_buffer_get(holders[1].buffer) == 0
	assert index == 1
}
