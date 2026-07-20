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
