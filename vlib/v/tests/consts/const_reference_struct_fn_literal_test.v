module main

struct ReferenceConstCallback {
	callback fn (int) string = unsafe { nil }
}

const reference_const_callback = &ReferenceConstCallback{
	callback: fn (value int) string { return 'reference${value}' }
}
const value_const_callback = ReferenceConstCallback{
	callback: fn (value int) string { return 'value${value}' }
}

const parenthesized_const_callback = (&ReferenceConstCallback{
	callback: fn (value int) string { return 'parenthesized${value}' }
})

fn test_function_literal_in_reference_const_struct() {
	assert reference_const_callback.callback(3) == 'reference3'
	assert reference_const_callback.callback(7) == 'reference7'
	assert parenthesized_const_callback.callback(9) == 'parenthesized9'
}

fn test_value_const_and_local_reference_function_literals() {
	assert value_const_callback.callback(4) == 'value4'
	local := &ReferenceConstCallback{
		callback: fn (value int) string { return 'local${value}' }
	}
	assert local.callback(5) == 'local5'
}
