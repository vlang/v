module types

import v.flat

fn test_transform_static_method_list_agrees_with_the_general_lookup() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Box')] = []StructField{}
	make_key := flat.encode_static_type_method_name('Box', 'make')
	tc.fn_ret_types[make_key] = Type(int_)
	tc.fn_param_types[make_key] = []Type{}
	tc.fn_ret_types['Box.size'] = Type(int_)
	tc.fn_param_types['Box.size'] = []Type{}
	tc.resolution_type_mode = true
	assert tc.transform_static_method_count == -1
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'make') or { '' } == make_key
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'size') == none
	tc.refresh_transform_static_method_names()
	assert tc.transform_static_method_count == tc.fn_ret_types.len
	assert tc.transform_static_method_names == {
		'make': true
	}
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'make') or { '' } == make_key
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'size') == none
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'open') == none
	// A signature registered after the list was taken is still found.
	open_key := flat.encode_static_type_method_name('Box', 'open')
	tc.fn_ret_types[open_key] = Type(int_)
	tc.fn_param_types[open_key] = []Type{}
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'open') or { '' } == open_key
	tc.refresh_transform_static_method_names()
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'open') or { '' } == open_key
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'shut') == none
	// The forks of a checker read its list; a removal retires it.
	forked := tc.fork_for_parallel_transform(&a)
	assert forked.transform_static_method_count == tc.transform_static_method_count
	assert forked.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'shut') == none
	assert forked.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'open') or { '' } == open_key
	tc.forget_transform_static_method_names()
	assert tc.transform_static_method_count == -1
	assert tc.transform_static_method_names.len == 0
	assert tc.static_assoc_fn_key_for_base(flat.source_type_id('Box'), 'open') or { '' } == open_key
}
