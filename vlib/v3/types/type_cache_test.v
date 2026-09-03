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

fn test_parse_resolution_type_prefers_file_import_over_known_short_symbol() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['token.Pos'] = []StructField{}
	tc.structs['v.token.Pos'] = []StructField{}
	tc.structs['Box'] = []StructField{}
	tc.cur_file = 'ast.v'
	tc.cur_module = 'v.ast'
	tc.register_file_import('token', 'v.token')

	assert tc.parse_resolution_type('token.Pos').name() == 'v.token.Pos'
	assert tc.parse_resolution_type('[]token.Pos').name() == '[]v.token.Pos'
	assert tc.parse_resolution_type('?token.Pos').name() == '?v.token.Pos'
	assert tc.parse_resolution_type('Box[token.Pos]').name() == 'Box[v.token.Pos]'
}

fn test_parse_thread_type_qualifies_concrete_payloads() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'fixturetest.v'
	tc.cur_module = 'fixturetest'
	tc.structs['fixturetest.FixtureResult'] = []StructField{}

	assert tc.parse_type('thread FixtureResult').name() == 'thread fixturetest.FixtureResult'
	assert tc.parse_type('thread ?FixtureResult').name() == 'thread ?fixturetest.FixtureResult'
	assert tc.parse_type('thread T').name() == 'thread T'
}

fn test_parse_resolution_fn_type_preserves_nested_main_type_lock() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Context'] = []StructField{}
	tc.structs['veb.Context'] = []StructField{}
	tc.cur_file = 'veb/middleware.v'
	tc.cur_module = 'veb'

	locked := tc.parse_resolution_type('fn (mut main.Context) bool')
	assert locked is FnType
	assert locked.params.len == 1
	locked_param := locked.params[0]
	if locked_param is Pointer {
		assert locked_param.base_type.name() == 'Context'
	} else {
		assert false, locked_param.name()
	}
	assert locked.params_mut == [true]

	local := tc.parse_resolution_type('fn (mut Context) bool')
	assert local is FnType
	local_param := local.params[0]
	if local_param is Pointer {
		assert local_param.base_type.name() == 'veb.Context'
	} else {
		assert false, local_param.name()
	}
}

fn test_parse_resolution_main_alias_uses_alias_declaration_scope() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Context'] = []StructField{}
	tc.struct_modules['Context'] = 'main'
	tc.structs['veb.Context'] = []StructField{}
	tc.struct_modules['veb.Context'] = 'veb'
	tc.type_aliases['AliasContext'] = 'Context'
	tc.type_alias_modules['AliasContext'] = 'main'
	tc.cur_file = 'veb/veb.v'
	tc.cur_module = 'veb'

	locked := tc.parse_resolution_type('main.AliasContext')
	if locked is Alias {
		assert locked.name == 'AliasContext'
		assert locked.base_type is Struct
		assert locked.base_type.name() == 'Context'
	} else {
		assert false, locked.name()
	}
	assert tc.c_type(locked) == 'main__Context'
}

fn test_embedded_field_type_trusts_collected_embed_metadata() {
	field := StructField{
		name:     'Middleware[Context]'
		typ:      Type(Struct{
			name: 'veb.Middleware[veb.Context]'
		})
		is_embed: true
	}
	embedded := embedded_field_type(field) or { panic('missing embedded field type') }
	assert embedded.name() == 'veb.Middleware[veb.Context]'
}

fn test_receiver_embeds_through_alias() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Context'] = [
		StructField{
			name:     'Context'
			typ:      Type(Struct{
				name: 'veb.Context'
			})
			is_embed: true
		},
	]
	actual := Type(Alias{
		name:      'AliasContext'
		base_type: Type(Struct{
			name: 'Context'
		})
	})
	expected := Type(Pointer{
		base_type: Type(Struct{
			name: 'veb.Context'
		})
	})
	assert tc.receiver_embeds(actual, expected)
}

fn test_parse_resolution_type_handles_locked_main_generic_application() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.struct_generic_params['StructType'] = ['T']
	tc.cur_file = 'decode.v'
	tc.cur_module = 'json2'

	assert tc.parse_resolution_type('main.StructType[string]').name() == 'StructType[string]'
	assert tc.parse_resolution_type('main.StructType[main.string]').name() == 'StructType[string]'
	assert tc.parse_type('main.StructType[string]').name() == 'StructType[string]'
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

fn test_type_cache_overlay_can_be_discarded_without_publishing_entries() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.cur_file = 'main.v'
	tc.cur_module = 'main'

	assert tc.parse_type('int').name() == 'int'
	base := tc.type_cache
	base_entries := base.parse_entries.len
	tc.freeze_type_cache_for_forks()
	assert tc.parse_type('string').name() == 'string'
	assert tc.type_cache.parse_entries.len > 0

	tc.discard_type_cache_overlay_after_forks()
	assert tc.type_cache == base
	assert base.parse_entries.len == base_entries
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

fn test_c_type_recent_cache_distinguishes_reassigned_sum_payloads() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	mut typ := Type(Struct{
		name: 'mcp.Request'
	})
	assert tc.c_type(typ) == 'mcp__Request'
	typ = Type(Struct{
		name: 'http.Request'
	})
	assert tc.c_type(typ) == 'http__Request'
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

fn test_fn_param_mutability_participates_in_type_identity() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	immutable := Type(FnType{
		params:      [Type(int_)]
		params_mut:  [false]
		return_type: Type(void_)
	})
	mutable := Type(FnType{
		params:      [Type(int_)]
		params_mut:  [true]
		return_type: Type(void_)
	})
	legacy_immutable := Type(FnType{
		params:      [Type(int_)]
		return_type: Type(void_)
	})

	assert immutable.name() == 'fn(int)'
	assert mutable.name() == 'fn(mut int)'
	assert semantic_types_equal(immutable, legacy_immutable)
	assert semantic_type_hash(immutable) == semantic_type_hash(legacy_immutable)
	assert !semantic_types_equal(immutable, mutable)
	assert semantic_type_hash(immutable) != semantic_type_hash(mutable)
	immutable_id, _ := tc.intern_type(immutable)
	mutable_id, _ := tc.intern_type(mutable)
	assert immutable_id != mutable_id
	assert tc.c_type(immutable) == 'fn_ptr:void|i64'
	assert tc.c_type(mutable) == 'fn_ptr:void|i64*'

	cloned := clone_owned_type(mutable)
	assert cloned is FnType
	cloned_fn := cloned as FnType
	assert cloned_fn.params_mut == [true]
	assert cloned.name() == 'fn(mut int)'
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

fn test_concrete_generic_method_signature_candidates_flatten_nested_pointer_args() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	candidates := tc.concrete_generic_method_signature_candidates('SimpleCache[string, &CacheItem[string, int]]',
		'set')
	assert 'SimpleCache[string, ptr_CacheItem_string_int].set' in candidates
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
