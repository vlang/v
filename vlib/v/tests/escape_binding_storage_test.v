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
	return if found := values[key] { found } else { &x }
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

@[noinline]
fn single_scoped_binding_references() []&int {
	mut x := 251
	outer := &x
	mut inner := unsafe { &int(nil) }
	// Shadowing itself is covered by the flat-AST transform tests; these exercise retained copies.
	unsafe {
		{
			pointer := &x
			assert pointer == outer
			assert *pointer == 251
		}
		{
			mut copy := x
			copy++
			inner = &copy
		}
	}
	x += 2
	return [outer, inner]
}

fn scoped_binding_pair(value int) (int, int) {
	return value, value + 1
}

@[noinline]
fn tuple_scoped_binding_references() []&int {
	mut x := 271
	mut kept := [&x]
	unsafe {
		{
			copy, next := scoped_binding_pair(x)
			kept << &copy
			kept << &next
		}
	}
	x += 2
	return kept
}

struct ScopedBindingRecord {
mut:
	value int
}

@[noinline]
fn struct_scoped_binding_references() []&ScopedBindingRecord {
	mut x := ScopedBindingRecord{ value: 281 }
	mut kept := [&x]
	unsafe {
		{
			copy := ScopedBindingRecord{ value: x.value + 1 }
			kept << &copy
		}
	}
	x.value += 2
	return kept
}

fn test_scoped_initializers_read_incoming_heap_storage() {
	single := single_scoped_binding_references()
	tuple := tuple_scoped_binding_references()
	records := struct_scoped_binding_references()
	assert escape_binding_overwrite_stack() == 7
	assert single.map(*it) == [253, 252]
	assert tuple.map(*it) == [273, 271, 272]
	assert records.map(it.value) == [283, 282]
	assert single[0] != single[1]
	assert records[0] != records[1]
}

@[noinline]
fn block_scope_preserves_outer_heap_type() &int {
	mut x := 301
	kept := &x
	unsafe {
		{
			inner := 'inner'
			assert inner == 'inner'
			assert typeof(inner).name == 'string'
		}
	}
	assert typeof(x).name == 'int'
	x = 303
	return kept
}

@[noinline]
fn loop_scope_preserves_outer_heap_type() &int {
	mut x := 311
	kept := &x
	unsafe {
		{
			for _ in 0 .. 1 {
				inner := 'inner'
				assert inner == 'inner'
				assert typeof(inner).name == 'string'
			}
			assert typeof(x).name == 'int'
			x = 313
		}
	}
	return kept
}

fn test_nested_scopes_restore_outer_heap_binding_types() {
	block := block_scope_preserves_outer_heap_type()
	loop := loop_scope_preserves_outer_heap_type()
	assert escape_binding_overwrite_stack() == 7
	assert *block == 303
	assert *loop == 313
}
