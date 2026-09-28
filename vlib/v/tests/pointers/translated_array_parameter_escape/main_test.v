@[has_globals]
module main

import decay
import forwarding as wrappers

__global saved_parameter_pointer = decay.pick(make_values())
__global saved_block_parameter_pointer = decay.pick(unsafe { make_values() })
__global saved_forwarded_pointer = forward_twice(make_values())

fn make_values() [3]int {
	return [3, 5, 7]!
}

fn pick_from_temporary() &int {
	return decay.pick(make_values())
}

fn pick_from_branch_argument(flag bool) &int {
	return decay.pick_offset(make_values(), if flag { 1 } else { 2 })
}

fn pick_from_local() &int {
	mut values := make_values()
	pointer := decay.pick(values)
	values[1] = 11
	return pointer
}

fn pick_from_local_branch(flag bool) &int {
	mut values := make_values()
	pointer := decay.pick_offset(values, if flag { 1 } else { 2 })
	values[1] = 29
	values[2] = 31
	return pointer
}

fn read_pointer(pointer &int) int {
	return unsafe { *pointer }
}

fn test_ordinary_caller_keeps_translated_array_parameter_storage() {
	returned := decay.pick(make_values())
	assert read_pointer(returned) == 5
	assert read_pointer(pick_from_temporary()) == 5
	assert read_pointer(pick_from_branch_argument(true)) == 5
	assert read_pointer(pick_from_branch_argument(false)) == 7
	assert read_pointer(pick_from_local()) == 11
	assert read_pointer(pick_from_local_branch(true)) == 29
	assert read_pointer(pick_from_local_branch(false)) == 31
	assert read_pointer(saved_parameter_pointer) == 5
	mut outer := unsafe { &int(nil) }
	if outer == unsafe { nil } {
		outer = decay.pick([13, 17, 19]!)
	}
	assert read_pointer(outer) == 17
	mut values := make_values()
	local := decay.pick(values)
	values[1] = 23
	assert read_pointer(local) == 23
	picker := decay.Picker{}
	method := picker.pick(make_values())
	assert read_pointer(method) == 5
}

fn pick_from_conditional_return(flag bool) &int {
	values := make_values()
	return if flag { decay.pick(values) } else { decay.pick_offset(values, 2) }
}

fn pick_from_array_branch(flag bool) &int {
	return decay.pick(if flag { make_values() } else { [11, 13, 17]! })
}

fn logged_prefix(mut trace []int) int {
	trace << 1
	return 17
}

fn logged_values(mut trace []int) [3]int {
	trace << 2
	return make_values()
}

fn pick_from_unsafe_argument(flag bool) &int {
	mut trace := []int{}
	pointer := decay.pick_after(logged_prefix(mut trace), unsafe { logged_values(mut trace) }, if flag {
		trace << 3
		1
	} else {
		trace << 4
		2
	})
	assert trace == if flag { [1, 2, 3] } else { [1, 2, 4] }
	return pointer
}

fn pick_from_scoped_unsafe_argument() &int {
	return decay.pick(unsafe {
		values := make_values()
		values
	})
}

fn retain_local_argument() {
	mut values := make_values()
	decay.retain(values)
	values[1] = 41
}

fn test_translated_parameters_keep_branch_and_retained_storage() {
	assert read_pointer(saved_block_parameter_pointer) == 5
	assert read_pointer(pick_from_conditional_return(true)) == 5
	assert read_pointer(pick_from_conditional_return(false)) == 7
	assert read_pointer(pick_from_array_branch(true)) == 5
	assert read_pointer(pick_from_array_branch(false)) == 13
	assert read_pointer(pick_from_unsafe_argument(true)) == 5
	assert read_pointer(pick_from_unsafe_argument(false)) == 7
	assert read_pointer(pick_from_scoped_unsafe_argument()) == 5
	retain_local_argument()
	assert decay.read_retained() == 41
	decay.retain(make_values())
	assert decay.read_retained() == 5
}

fn forward_values(values [3]int) &int {
	return decay.pick(values)
}

fn forward_twice(values [3]int) &int {
	return forward_values(values)
}

fn forward_temporary() &int {
	return forward_twice(make_values())
}

fn forward_local() &int {
	mut values := make_values()
	pointer := forward_twice(values)
	values[1] = 47
	return pointer
}

fn forward_retained(values [3]int) {
	decay.retain(values)
}

fn forward_retained_local() {
	mut values := make_values()
	forward_retained(values)
	values[1] = 53
}

fn forward_imported() &int {
	return wrappers.forward(make_values())
}

fn forward_generic() &int {
	return wrappers.forward_generic('marker', make_values())
}

fn forward_method() &int {
	forwarder := wrappers.Forwarder{}
	return forwarder.forward(make_values())
}

fn forward_recursive(values [3]int, remaining int) &int {
	if remaining == 0 {
		return decay.pick(values)
	}
	return forward_recursive(values, remaining - 1)
}

fn forward_recursive_temporary() &int {
	return forward_recursive(make_values(), 2)
}

fn test_ordinary_wrappers_preserve_translated_array_storage() {
	assert read_pointer(forward_temporary()) == 5
	assert read_pointer(forward_local()) == 47
	assert read_pointer(saved_forwarded_pointer) == 5
	assert read_pointer(forward_imported()) == 5
	assert read_pointer(forward_generic()) == 5
	assert read_pointer(forward_method()) == 5
	assert read_pointer(forward_recursive_temporary()) == 5
	forward_retained_local()
	assert decay.read_retained() == 53
	forward_retained(make_values())
	assert decay.read_retained() == 5
}
