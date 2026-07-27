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

fn test_type_cache_overlay_rebinds_resolution_type_views() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.cur_file = 'main.v'
	tc.cur_module = 'main'

	assert tc.parse_resolution_type('int').name() == 'int'
	base := tc.type_cache
	base_view := tc.resolution_type_views.by_file['main.v'] or { panic('missing base view') }
	assert base_view.type_cache == base

	tc.freeze_type_cache_for_forks()
	overlay := tc.type_cache
	assert overlay != base
	assert overlay.base == base
	assert tc.resolution_type_views.by_file.len == 0
	assert tc.parse_resolution_type('string').name() == 'string'
	overlay_view := tc.resolution_type_views.by_file['main.v'] or { panic('missing overlay view') }
	assert overlay_view.type_cache == overlay

	tc.unfreeze_type_cache_after_forks()
	assert tc.type_cache == base
	assert tc.resolution_type_views.by_file.len == 0
	assert tc.parse_resolution_type('bool').name() == 'bool'
	restored_view := tc.resolution_type_views.by_file['main.v'] or {
		panic('missing restored view')
	}
	assert restored_view.type_cache == base
}

fn test_type_cache_restore_preserves_disabled_resolution_type_views() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)

	tc.disable_resolution_type_view_cache()
	tc.freeze_type_cache_for_forks()
	assert isnil(tc.resolution_type_views)
	tc.unfreeze_type_cache_after_forks()
	assert isnil(tc.resolution_type_views)

	tc.reset_resolution_type_view_cache()
	tc.freeze_type_cache_for_forks()
	assert !isnil(tc.resolution_type_views)
	tc.disable_resolution_type_view_cache()
	assert isnil(tc.resolution_type_views)

	tc.unfreeze_type_cache_after_forks()
	assert isnil(tc.resolution_type_views)
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

fn test_voidptr_is_not_implicitly_compatible_with_callback_type() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)

	void_pointer := tc.parse_type('voidptr')
	callback := tc.parse_type('fn ()')
	assert !tc.type_compatible(void_pointer, callback)
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
