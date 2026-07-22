module types

import v3.flat

fn test_collect_resets_function_context_before_type_cache_keys() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert tc.fn_context.generic_params.len == 0
	tc.fn_context.generic_params = ['T']

	tc.collect(&a)
	assert tc.fn_context.generic_params.len == 0
	assert tc.parse_type('voidptr').name() == '&void'
}

fn test_parse_type_cache_keeps_context_components_without_joined_keys() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.cur_file = 'one.v'
	tc.cur_module = 'one'
	assert tc.parse_type('int').name() == 'int'
	assert tc.parse_type('int').name() == 'int'
	assert tc.type_cache.parse_entries.len == 1

	tc.cur_module = 'two'
	assert tc.parse_type('int').name() == 'int'
	assert tc.type_cache.parse_entries.len == 2

	tc.cur_file = 'two.v'
	assert tc.parse_type('int').name() == 'int'
	assert tc.type_cache.parse_entries.len == 3
}

fn test_c_type_cache_uses_existing_named_type_identity() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	typ := Type(Struct{
		name: 'sample.Item'
	})
	assert tc.c_type(typ) == 'sample__Item'
	assert tc.c_type(typ) == 'sample__Item'
	assert tc.type_cache.c_entries.len == 1
}

fn test_semantic_type_interner_uses_structural_identity() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	first_id, first := tc.intern_type(Type(Map{
		key_type:   Type(string_)
		value_type: Type(Array{
			elem_type: Type(int_)
		})
	}))
	second_id, second := tc.intern_type(Type(Map{
		key_type:   Type(string_)
		value_type: Type(Array{
			elem_type: Type(int_)
		})
	}))
	assert first_id == second_id
	assert semantic_types_equal(first, second)

	int_alias, _ := tc.intern_type(Type(Alias{
		name:      'sample.Number'
		base_type: Type(int_)
	}))
	string_alias, _ := tc.intern_type(Type(Alias{
		name:      'sample.Number'
		base_type: Type(string_)
	}))
	assert int_alias != string_alias
}

fn test_c_type_cache_keys_composite_types_by_type_id() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	typ := Type(Pointer{
		base_type: Type(Struct{
			name: 'sample.Item'
		})
	})
	assert tc.c_type(typ) == 'sample__Item*'
	entries_after_first := tc.type_cache.c_entries.len
	assert entries_after_first >= 2
	assert tc.c_type(typ) == 'sample__Item*'
	assert tc.type_cache.c_entries.len == entries_after_first
}

fn test_type_name_is_lazily_cached_by_type_id() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	typ := Type(Map{
		key_type:   Type(string_)
		value_type: Type(Array{
			elem_type: Type(int_)
		})
	})
	first := tc.type_name(typ)
	second := tc.type_name(typ)
	assert first == 'map[string][]int'
	assert first.str == second.str
}

fn test_postfix_fixed_array_of_generic_struct_parses_before_generic_application() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	typ := tc.parse_type('arc.Arc[Resource][2]')
	assert typ is ArrayFixed
	fixed := typ as ArrayFixed
	assert fixed.len == 2
	assert fixed.elem_type.name() == 'arc.Arc[Resource]'
}

fn test_resolution_function_type_keeps_concrete_caller_type() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'decode.v'
	tc.cur_module = 'json2'
	tc.structs['Event'] = []StructField{}

	assert tc.qualify_type_text('fn (voidptr, &Event)') == 'fn(voidptr, &json2.Event)'
	assert tc.qualify_resolution_type_text('fn (voidptr, &Event)') == 'fn(voidptr, &Event)'
	resolved := tc.parse_resolution_type('fn (voidptr, &Event)')
	assert resolved is FnType
	fn_type := resolved as FnType
	assert fn_type.params.len == 2
	assert fn_type.params[1] is Pointer
	assert (fn_type.params[1] as Pointer).base_type.name() == 'Event'
	cached := tc.resolution_type_views.by_file['decode.v'] or { panic('missing cached view') }
	assert cached.type_interner == tc.type_interner
	assert tc.parse_resolution_type('fn (voidptr, &Event)').name() == resolved.name()
}

fn test_resolved_symbols_have_stable_ids_and_storage() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	first := tc.canonical_symbol('sample.run')
	second := tc.canonical_symbol('sample.' + 'run')
	assert first == second
	assert first.str == second.str
	assert tc.symbol_count() == 1
}
