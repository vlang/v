module types

import v3.flat

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

fn test_recursive_callback_alias_parses_once_and_keeps_its_abi() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases['Handlers'] = 'map[string]fn (Handlers) int'
	tc.type_cache.parse_enabled = true

	typ := tc.parse_type('Handlers')
	assert typ is Alias
	base := (typ as Alias).base_type
	assert base is Map
	callback := (base as Map).value_type
	assert callback is FnType
	param := (callback as FnType).params[0]
	assert param is Alias
	assert (param as Alias).name == 'Handlers'
	assert tc.c_type(param) == 'map'
	assert tc.parse_type('Handlers').name() == 'Handlers'
	assert tc.type_cache.alias_parse_stack.len == 0
}

fn test_fn_type_with_spaced_empty_parameter_list_has_no_void_parameter() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)

	typ := tc.parse_type('fn ( ) int')
	assert typ is FnType
	fn_typ := typ as FnType
	assert fn_typ.params.len == 0
	assert fn_typ.return_type.name() == 'int'
	assert Type(fn_typ).name() == 'fn() int'
}

fn test_postfix_fixed_array_of_generic_struct_parses_before_generic_application() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	generic := tc.parse_type('arc.Arc[Resource]')
	assert generic is Struct
	assert generic.name() == 'arc.Arc[Resource]'
	nested_map := tc.parse_type('map[string]map[string]arc.Arc[Resource]')
	assert nested_map is Map
	inner_map := (nested_map as Map).value_type
	assert inner_map is Map
	inner_value := (inner_map as Map).value_type
	assert inner_value is Struct
	assert inner_value.name() == 'arc.Arc[Resource]'
	typ := tc.parse_type('arc.Arc[Resource][2]')
	assert typ is ArrayFixed
	fixed := typ as ArrayFixed
	assert fixed.len == 2
	assert fixed.elem_type.name() == 'arc.Arc[Resource]'
}

fn test_generic_text_substitution_recurses_through_wrappers() {
	assert subst_generic_text('chan T', ['int'], ['T']) == 'chan int'
	assert subst_generic_text('thread T', ['string'], ['T']) == 'thread string'
	assert subst_generic_text('atomic T', ['u64'], ['T']) == 'atomic u64'
	assert subst_generic_text('chan ?[]T', ['i16'], ['T']) == 'chan ?[]i16'
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
