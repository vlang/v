@[has_globals]
module main

__global escape_binding_global = [0, 0]!

fn escape_binding_overwrite_stack() int {
	b := [7, 7, 7, 7, 7, 7, 7, 7]!
	return b[3]
}

@[noinline]
fn scalar_reference_after_lowering_heap_value_capture() &int {
	mut value := 291
	read_capture := fn [value] () int {
		p := &value
		return *p
	}
	mut read_mutable_capture := fn [mut value] () int {
		value++
		return value
	}
	assert read_capture() == 291
	value = 292
	assert read_capture() == 291
	assert read_mutable_capture() == 292
	assert read_mutable_capture() == 293
	assert value == 292
	value = 291
	return &value
}

fn test_heap_value_captures_keep_value_snapshots() {
	kept := scalar_reference_after_lowering_heap_value_capture()
	assert escape_binding_overwrite_stack() == 7
	assert *kept == 291
}

@[noinline]
fn reference_from_return_map_guard_with_promoted_outer_binding(key string) &int {
	x := 231
	values := {
		'hit': &escape_binding_global[0]
	}
	return if x := values[key] { x } else { &x }
}

fn test_return_map_guards_preserve_inner_reference_and_outer_heap_bindings() {
	escape_binding_global[0] = 232
	for key, expected in {
		'hit':  232
		'miss': 231
	} {
		kept := reference_from_return_map_guard_with_promoted_outer_binding(key)
		assert escape_binding_overwrite_stack() == 7
		assert *kept == expected
	}
}

fn failing_retained_error(message string) !u64 {
	return error(message)
}

@[noinline]
fn append_implicit_error_addresses(mut out []&IError) {
	err := u64(201)
	failing_retained_error('outer') or {
		out << &err
		assert typeof(err).name == 'IError'
		failing_retained_error('inner') or {
			out << &err
		assert typeof(err).name == 'IError'
			0
		}
		out << &err
		assert typeof(err).name == 'IError'
		0
	}
	if value := failing_retained_error('guard') {
		_ = value
	} else {
		out << &err
		assert typeof(err).name == 'IError'
	}
	value := if value := failing_retained_error('value guard') {
		value
	} else {
		out << &err
		assert typeof(err).name == 'IError'
		u64(0)
	}
	assert value == 0
	assert err == 201
}

@[noinline]
fn returned_implicit_error_address() &IError {
	failing_retained_error('returned') or { return &err }
	panic('unexpected success')
}

fn test_implicit_result_errors_keep_retained_binding_addresses() {
	mut out := []&IError{}
	append_implicit_error_addresses(mut out)
	out << returned_implicit_error_address()
	assert escape_binding_overwrite_stack() == 7
	assert out.map((*it).msg()) == ['outer', 'inner', 'outer', 'guard', 'value guard', 'returned']
	assert out[0] == out[2]
	assert out[0] != out[1]
}
