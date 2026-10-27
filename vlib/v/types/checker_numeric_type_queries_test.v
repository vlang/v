module types

import v.flat

fn test_struct_field_recent_queries_preserve_hits_misses_and_collisions() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	owner_id := flat.source_type_id('RecentFields')
	tc.structs[flat.source_type_id('RecentFields')] = [
		StructField{ name: 'abax', typ: builtin_int_type },
		StructField{ name: 'abbx', typ: builtin_string_type },
	]
	// The sampled bytes select one slot, but every hit must verify the whole name.
	assert struct_field_cache_slot(owner_id, 'abax') == struct_field_cache_slot(owner_id, 'abbx')
	assert struct_field_cache_slot(owner_id, 'abax') == struct_field_cache_slot(owner_id, 'abzx')
	for _ in 0 .. 3 {
		assert tc.struct_field_type(owner_id, 'abax')? == builtin_int_type
		assert tc.struct_field_type(owner_id, 'abax')? == builtin_int_type
		assert tc.struct_field_type(owner_id, 'abzx') == none
		assert tc.struct_field_type(owner_id, 'abzx') == none
		assert tc.struct_field_type(owner_id, 'abbx')? == builtin_string_type
		assert tc.struct_field_type(owner_id, 'abbx')? == builtin_string_type
	}
	// Invalidating declarations also invalidates a remembered negative lookup.
	tc.structs[flat.source_type_id('RecentFields')] << StructField{ name: 'abzx', typ: builtin_bool_type }
	tc.clear_field_lookup_cache()
	assert tc.struct_field_type(owner_id, 'abzx')? == builtin_bool_type
	assert tc.struct_field_type(owner_id, 'abzx')? == builtin_bool_type
}

fn test_struct_field_recent_queries_keep_c_module_precedence() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	owner_id := flat.source_type_id('C.RecentMirror')
	tc.structs[flat.source_type_id('C.RecentMirror')] = [StructField{ name: 'value', typ: builtin_int_type }]
	tc.cur_module = 'fallback'
	assert tc.struct_field_type(owner_id, 'value')? == builtin_int_type
	assert tc.struct_field_type(owner_id, 'value')? == builtin_int_type
	tc.c_struct_scoped_fields[c_struct_module_key('local', 'C.RecentMirror')] = [
		StructField{ name: 'value', typ: builtin_string_type },
	]
	tc.cur_module = 'local'
	assert tc.struct_field_type(owner_id, 'value')? == builtin_string_type
	tc.cur_module = 'fallback'
	assert tc.struct_field_type(owner_id, 'value')? == builtin_int_type
}

fn test_struct_field_recent_miss_keeps_escaped_keyword_precedence() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	owner_id := flat.source_type_id('RecentEscaped')
	tc.structs[flat.source_type_id('RecentEscaped')] = []StructField{}
	assert tc.struct_field_type(owner_id, '@type') == none
	assert tc.struct_field_type(owner_id, '@type') == none
	// Exact escaped keyword fields are checked before a cached ordinary miss.
	tc.structs[flat.source_type_id('RecentEscaped')] << StructField{ name: '@type', typ: builtin_bool_type }
	assert tc.struct_field_type(owner_id, '@type')? == builtin_bool_type
	assert tc.struct_field_type(owner_id, '@type')? == builtin_bool_type
}

fn test_void_pointer_queries_keep_all_nominal_spelling_variants() {
	for spelling in ['voidptr', '&void'] {
		id := flat.source_type_id(spelling)
		for typ in [Type(Struct{ name_id: id }), Type(Enum{ name_id: id }),
			Type(Interface{ name_id: id }), Type(SumType{ name_id: id }),
			Type(Alias{ name_id: id, base_type: &builtin_int_type })] {
			assert fn_param_is_voidptr_type(typ), typ.name()
		}
	}
	for typ in [Type(Struct{ name_id: flat.source_type_void }),
		Type(Enum{ name_id: flat.source_type_void }),
		Type(Interface{ name_id: flat.source_type_void }),
		Type(SumType{ name_id: flat.source_type_void }),
		Type(Alias{ name_id: flat.source_type_void, base_type: &builtin_int_type })] {
		assert !fn_param_is_voidptr_type(typ), typ.name()
		pointer := Type(Pointer{ base_type: new_type_reference(typ) })
		assert fn_param_is_voidptr_type(pointer), pointer.name()
	}
}

fn test_generic_method_owner_candidates_keep_qualification_context() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.qualify_name_cache = &QualifyNameCache{}
	owner_id := flat.source_type_id('RecentBox')
	for module_name in ['one', 'two', 'one'] {
		tc.cur_module = module_name
		tc.cur_file = '${module_name}.v'
		expected := [owner_id, flat.source_type_id('${module_name}.RecentBox')]
		assert tc.generic_struct_method_base_candidates(owner_id) == expected
		assert tc.generic_struct_method_base_candidates(owner_id) == expected
	}
	tc.resolution_type_mode = true
	assert tc.generic_struct_method_base_candidates(owner_id) == [owner_id]
	tc.structs[flat.source_type_id('one.RecentBox')] = []StructField{}
	assert tc.generic_struct_method_base_candidates(owner_id) == [owner_id,
		flat.source_type_id('one.RecentBox')]
	assert tc.generic_struct_method_base_candidates(flat.source_type_int) == [flat.source_type_int]
}
