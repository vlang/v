module types

import v.flat

fn test_receiver_pattern_cache_keys_keep_source_argument_order_and_context() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'main'
	tc.fn_context.generic_params = ['T']
	first := flat.source_type_id('Item[int]')
	second := flat.source_type_id('Item[string]')
	ordered := tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [
		first,
		second,
	], 'take')
	reversed := tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [
		second,
		first,
	], 'take')
	shorter := tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [first], 'take')
	empty := tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), []u32{}, 'take')
	empty_arg := tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [u32(0)], 'take')
	keys := {
		ordered:   1
		reversed:  2
		shorter:   3
		empty:     4
		empty_arg: 5
	}
	assert keys.len == 5
	assert keys[ordered] == 1
	assert keys[reversed] == 2
	tc.cur_file = 'other.v'
	assert tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [first, second], 'take') != ordered
	tc.cur_file = 'caller.v'
	tc.cur_module = 'other'
	assert tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [first, second], 'take') != ordered
	tc.cur_module = 'main'
	tc.fn_context.generic_params = ['U']
	assert tc.generic_receiver_pattern_cache_key(flat.source_type_id('Box'), [first, second], 'take') != ordered
}

fn test_wrapped_single_letter_concrete_types_preserve_their_semantic_type() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'single'
	tc.cur_file = 'single.v'
	tc.structs['single.M'] = []StructField{}
	for prefix in ['!', '?', '&', '[]', 'chan ', 'thread '] {
		for _ in 0 .. 2 {
			parsed := tc.parse_resolution_type(flat.source_type_id('${prefix}M'))
			assert parsed.name() == '${prefix}single.M'
			assert parsed !is Unknown
		}
	}
	assert tc.parse_type(flat.source_type_id('!single.M')) is ResultType
	assert tc.parse_type(flat.source_type_id('?single.M')) is OptionType
	assert tc.parse_type(flat.source_type_id('[2]single.M')) is ArrayFixed
	assert tc.parse_type(flat.source_type_id('single.T')) is Unknown
	assert tc.parse_type(flat.source_type_id('!single.T')) is ResultType
	assert tc.parse_type(flat.source_type_id('?single.T')) is OptionType
}

fn test_node_cache_reset_clears_string_slots_and_set_bits() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for n in [0, 1, 257, 33] {
		tc.reset_node_caches(n)
		assert tc.resolved_call_names.len == n
		assert tc.resolved_call_set.len == n
		for i in 0 .. n {
			assert isnil(tc.resolved_call_names[i])
			assert !tc.resolved_call_set[i]
			tc.resolved_call_names[i] = cached_name('previous.call')
			tc.resolved_call_set[i] = true
		}
	}
}

fn test_seeded_field_types_preserve_direct_precedence_and_generic_substitution() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Inner'] = [StructField{ name: 'value', typ: Type(bool_) }]
	tc.structs['Outer'] = [
		StructField{ name: 'Inner', typ: Type(Struct{ name_id: flat.source_type_id('Inner') }), is_embed: true },
		StructField{ name: 'value', typ: Type(int_) },
		StructField{ name: 'value', typ: Type(string_) },
	]
	tc.structs['Box'] = [StructField{ name: 'item', typ: unknown_type('T') }]
	tc.structs['Promoted'] = [
		StructField{ name: 'Inner', typ: Type(Struct{ name_id: flat.source_type_id('Inner') }) },
	]
	tc.struct_generic_params['Box'] = ['T']
	mut expected := []string{}
	for owner in ['Outer', 'Box[int]', 'Box[string]'] {
		field := if owner == 'Outer' { 'value' } else { 'item' }
		expected << flat.source_type_name(tc.struct_field_type_name(flat.source_type_id(owner), field)?)
	}
	tc.clear_field_lookup_cache()
	tc.cache_direct_struct_field_types()
	tc.scope_parallel_check_workers = true
	mut worker := tc.fork_for_parallel_check()
	assert worker.type_cache.struct_field_shared.len == tc.type_cache.struct_field_shared.len
	assert worker.type_cache.struct_field_shared.len > 0
	assert worker.type_cache.struct_field_complete[flat.source_type_id('Inner')]
	assert !worker.type_cache.struct_field_complete[flat.source_type_id('Outer')]
	assert !worker.type_cache.struct_field_complete[flat.source_type_id('Promoted')]
	assert worker.struct_field_type(flat.source_type_id('Inner'), 'missing') == none
	assert worker.struct_field_type(flat.source_type_id('Promoted'), 'value')? == Type(bool_)
	for i, owner in ['Outer', 'Box[int]', 'Box[string]'] {
		field := if owner == 'Outer' { 'value' } else { 'item' }
		assert flat.source_type_name(worker.struct_field_type_name(flat.source_type_id(owner), field)?) == expected[i]
	}
	worker.clear_field_lookup_cache()
	assert tc.type_cache.struct_field_shared.len > 0
	assert tc.type_cache.struct_field_complete[flat.source_type_id('Inner')]
	tc.structs['Inner'] << StructField{ name: 'missing', typ: Type(string_) }
	tc.clear_field_lookup_cache()
	assert tc.struct_field_type(flat.source_type_id('Inner'), 'missing')? == Type(string_)
}

fn test_field_cache_handles_collisions_missing_fields_and_invalidation() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Box'] = [
		StructField{ name: 'abba', typ: Type(int_) },
		StructField{ name: 'acca', typ: Type(string_) },
	]
	for _ in 0 .. 3 {
		assert tc.struct_field_type(flat.source_type_id('Box'.clone()), 'abba'.clone())? == Type(int_)
		assert tc.struct_field_type(flat.source_type_id('Box'), 'acca')? == Type(string_)
		assert tc.struct_field_type(flat.source_type_id('Box'), 'adda') == none
	}
	tc.structs['Box'] = [StructField{ name: 'adda', typ: Type(bool_) }]
	tc.clear_field_lookup_cache()
	assert tc.struct_field_type(flat.source_type_id('Box'), 'adda')? == Type(bool_)
	assert tc.struct_field_type(flat.source_type_id('Box'), 'acca') == none
}

fn test_type_text_id_cache_keeps_colliding_ids_and_file_context_distinct() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.cur_module = 'app'
	tc.cur_file = 'one.v'
	tc.structs['one.Item'] = []StructField{}
	tc.structs['two.Item'] = []StructField{}
	tc.register_file_import('dep', 'one')
	item_id := flat.source_type_id('dep.Item')
	mut collision_id := u32(0)
	mut collision_name := ''
	// Spellings now own their IDs. Find a real collision in the bounded front cache.
	for i in 0 .. 8192 {
		name := 'collision.Item_${item_id}_${i}'
		id := flat.source_type_id(name)
		if id != item_id && id & 4095 == item_id & 4095 {
			collision_id = id
			collision_name = name
			break
		}
	}
	assert collision_id != 0
	tc.structs[collision_name] = []StructField{}
	for _ in 0 .. 3 {
		assert tc.parse_type_ref(item_id).name() == 'one.Item'
		assert tc.parse_type_ref(collision_id).name() == collision_name
		assert tc.parse_type_ref(flat.source_type_string).name() == 'string'
		assert tc.parse_type_ref(flat.source_type_bool).name() == 'bool'
	}
	tc.cur_file = 'two.v'
	tc.register_file_import('dep', 'two')
	assert tc.parse_type_ref(item_id).name() == 'two.Item'
	tc.cur_file = 'one.v'
	assert tc.parse_type_ref(item_id).name() == 'one.Item'
}

fn test_parse_type_cache_keeps_context_components_without_joined_keys() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.cur_file = 'one.v'
	tc.cur_module = 'one'
	// Nominal names require the source context; builtin scalars do not.
	tc.structs['one.Item'] = []StructField{}
	tc.structs['two.Item'] = []StructField{}
	assert tc.parse_type(flat.source_type_id('Item')).name() == 'one.Item'
	assert tc.parse_type(flat.source_type_id('Item')).name() == 'one.Item'
	assert tc.type_cache.parse_entries.len == 1

	tc.cur_module = 'two'
	assert tc.parse_type(flat.source_type_id('Item')).name() == 'two.Item'
	assert tc.type_cache.parse_entries.len == 2

	tc.cur_file = 'two.v'
	assert tc.parse_type(flat.source_type_id('Item')).name() == 'two.Item'
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

	assert tc.parse_resolution_type(flat.source_type_id('token.Pos')).name() == 'v.token.Pos'
	assert tc.parse_resolution_type(flat.source_type_id('[]token.Pos')).name() == '[]v.token.Pos'
	assert tc.parse_resolution_type(flat.source_type_id('?token.Pos')).name() == '?v.token.Pos'
	assert tc.parse_resolution_type(flat.source_type_id('Box[token.Pos]')).name() == 'Box[v.token.Pos]'
}

fn test_parse_resolution_type_in_file_does_not_require_an_active_file_cursor() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['token.Pos'] = []StructField{}
	tc.structs['v.token.Pos'] = []StructField{}
	tc.cur_file = 'parser.v'
	tc.cur_module = 'parser'
	tc.register_file_import('token', 'v.token')
	tc.file_modules['parser.v'] = 'parser'
	tc.cur_file = ''
	tc.cur_module = ''

	assert tc.parse_resolution_type_in_file(flat.source_type_id('token.Pos'), 'parser.v').name() == 'v.token.Pos'
	tc.fn_type_files['parser.read'] = 'parser.v'
	tc.fn_type_modules['parser.read'] = 'parser'
	for text in ['', '!', '?', 'int', 'bool', 'string', 'voidptr', '&[]u8', '?int', '![]string'] {
		fresh := tc.fork_type_parse_view('parser.v', 'parser')
		expected := fresh.parse_resolution_type(flat.source_type_id(text))
		assert tc.parse_resolution_type_in_file(flat.source_type_id(text), 'parser.v') == expected
		assert tc.fn_signature_type('parser.read', flat.source_type_id(text)) == expected
	}
	assert tc.fn_signature_type('parser.read', flat.source_type_id('token.Pos')).name() == 'v.token.Pos'
}

fn test_parse_thread_type_qualifies_concrete_payloads() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'fixturetest.v'
	tc.cur_module = 'fixturetest'
	tc.structs['fixturetest.FixtureResult'] = []StructField{}

	assert tc.parse_type(flat.source_type_id('thread FixtureResult')).name() == 'thread fixturetest.FixtureResult'
	assert tc.parse_type(flat.source_type_id('thread ?FixtureResult')).name() == 'thread ?fixturetest.FixtureResult'
	assert tc.parse_type(flat.source_type_id('thread T')).name() == 'thread T'
}

fn test_alias_target_parsing_preserves_declaration_module() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'caller'
	tc.cur_file = 'caller.v'
	tc.type_alias_modules['dep.Alias'] = 'dep'
	tc.structs['caller.Item'] = []StructField{}
	tc.structs['dep.Item'] = []StructField{}
	for text in ['int', 'string', ' &[]u8 ', '?int', '![]string', 'shared int', 'Item', '&Item'] {
		fresh := tc.fork_type_parse_view('caller.v', 'dep')
		expected := if text == 'shared int' {
			Type(Pointer{ base_type: &Type(int_) })
		} else {
			fresh.parse_type(flat.source_type_id(text))
		}
		parsed := tc.parse_alias_type('dep.Alias', flat.source_type_id(text))
		assert parsed is Alias
		assert parsed.name() == 'dep.Alias'
		assert *parsed.base_type == expected
	}
	assert tc.parse_alias_target_type('dep.Alias', flat.source_type_id('Item')).name() == 'dep.Item'
	assert tc.cur_module == 'caller'
}

fn test_context_independent_container_types_match_declaration_views() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'caller'
	tc.file_modules['dep.v'] = 'dep'
	tc.type_alias_modules['dep.Alias'] = 'dep'
	tc.fn_type_files['dep.read'] = 'dep.v'
	tc.fn_type_modules['dep.read'] = 'dep'
	assert context_independent_type(0)
	for text in ['map[string]int', '?map[string][]u8', '[]map[int]&string', '[32]u8', '&[2][3]int',
		'map[[4]u8][]map[string]bool'] {
		assert context_independent_type(flat.source_type_id(text))
		fresh := tc.fork_type_parse_view('dep.v', 'dep')
		expected := fresh.parse_resolution_type(flat.source_type_id(text))
		assert tc.parse_resolution_type_in_file(flat.source_type_id(text), 'dep.v') == expected
		assert tc.fn_signature_type('dep.read', flat.source_type_id(text)) == expected
		alias_typ := tc.parse_alias_type('dep.Alias', flat.source_type_id(text))
		assert alias_typ is Alias
		assert *alias_typ.base_type == fresh.parse_type(flat.source_type_id(text))
	}
	for text in ['map', 'array', 'map[string]Item', 'map[Key]int', 'map[string]int ', '[size]int',
		'[2 + 3]int', '[0x10]int', 'map[string', '[2', '[2]'] {
		assert !context_independent_type(flat.source_type_id(text))
	}
}

fn test_parse_resolution_fn_type_preserves_nested_main_type_lock() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Context'] = []StructField{}
	tc.structs['veb.Context'] = []StructField{}
	tc.cur_file = 'veb/middleware.v'
	tc.cur_module = 'veb'

	locked := tc.parse_resolution_type(flat.source_type_id('fn (mut main.Context) bool'))
	assert locked is FnType
	assert locked.params.len == 1
	locked_param := locked.params[0].typ
	if locked_param is Pointer {
		assert locked_param.base_type.name() == 'Context'
	} else {
		assert false, locked_param.name()
	}
	assert locked.params.map(it.is_mut) == [true]

	local := tc.parse_resolution_type(flat.source_type_id('fn (mut Context) bool'))
	assert local is FnType
	local_param := local.params[0].typ
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
	tc.type_aliases.set_id('AliasContext', flat.source_type_id('Context'))
	tc.type_alias_modules['AliasContext'] = 'main'
	tc.cur_file = 'veb/veb.v'
	tc.cur_module = 'veb'

	locked := tc.parse_resolution_type(flat.source_type_id('main.AliasContext'))
	if locked is Alias {
		assert locked.name() == 'AliasContext'
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
			name_id: flat.source_type_id('veb.Middleware[veb.Context]')
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
				name_id: flat.source_type_id('veb.Context')
			})
			is_embed: true
		},
	]
	actual := Type(Alias{
		name_id:   flat.source_type_id('AliasContext')
		base_type: &Type(Struct{
			name_id: flat.source_type_id('Context')
		})
	})
	expected := Type(Pointer{
		base_type: &Type(Struct{
			name_id: flat.source_type_id('veb.Context')
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

	assert tc.parse_resolution_type(flat.source_type_id('main.StructType[string]')).name() == 'StructType[string]'
	assert tc.parse_resolution_type(flat.source_type_id('main.StructType[main.string]')).name() == 'StructType[string]'
	assert tc.parse_type(flat.source_type_id('main.StructType[string]')).name() == 'StructType[string]'
}

fn test_type_cache_overlay_rebinds_resolution_type_views() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.cur_file = 'main.v'
	tc.cur_module = 'main'
	tc.structs['Item'] = []StructField{}

	assert tc.parse_resolution_type(flat.source_type_id('Item')).name() == 'Item'
	base := tc.type_cache
	base_view := tc.resolution_type_views.by_file['main.v'] or { panic('missing base view') }
	assert base_view.type_cache == base

	tc.freeze_type_cache_for_forks()
	overlay := tc.type_cache
	assert overlay != base
	assert overlay.base == base
	assert tc.resolution_type_views.by_file.len == 0
	assert tc.parse_resolution_type(flat.source_type_id('[]Item')).name() == '[]Item'
	overlay_view := tc.resolution_type_views.by_file['main.v'] or { panic('missing overlay view') }
	assert overlay_view.type_cache == overlay

	tc.unfreeze_type_cache_after_forks()
	assert tc.type_cache == base
	assert tc.resolution_type_views.by_file.len == 0
	assert tc.parse_resolution_type(flat.source_type_id('?Item')).name() == '?Item'
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

	assert tc.parse_type(flat.source_type_int).name() == 'int'
	base := tc.type_cache
	base_entries := base.parse_entries.len
	tc.freeze_type_cache_for_forks()
	assert tc.parse_type(flat.source_type_string).name() == 'string'
	assert tc.type_cache.parse_entries.len > 0

	tc.discard_type_cache_overlay_after_forks()
	assert tc.type_cache == base
	assert base.parse_entries.len == base_entries
}

fn test_c_type_cache_uses_existing_named_type_identity() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	typ := Type(Struct{
		name_id: flat.source_type_id('sample.Item')
	})
	assert tc.c_type(typ) == 'sample__Item'
	assert tc.c_type(typ) == 'sample__Item'
	assert tc.type_cache.c_entries.len == 1
}

fn test_c_type_recent_cache_distinguishes_reassigned_sum_payloads() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	mut typ := Type(Struct{
		name_id: flat.source_type_id('mcp.Request')
	})
	assert tc.c_type(typ) == 'mcp__Request'
	typ = Type(Struct{
		name_id: flat.source_type_id('http.Request')
	})
	assert tc.c_type(typ) == 'http__Request'
}

fn test_semantic_type_interner_uses_structural_identity() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	first_id, first := tc.intern_type(Type(Map{
		key_type:   &Type(string_)
		value_type: &Type(Array{
			elem_type: &Type(int_)
		})
	}))
	second_id, second := tc.intern_type(Type(Map{
		key_type:   &Type(string_)
		value_type: &Type(Array{
			elem_type: &Type(int_)
		})
	}))
	assert first_id == second_id
	assert semantic_types_equal(first, second)

	int_alias, _ := tc.intern_type(Type(Alias{
		name_id:   flat.source_type_id('sample.Number')
		base_type: &Type(int_)
	}))
	string_alias, _ := tc.intern_type(Type(Alias{
		name_id:   flat.source_type_id('sample.Number')
		base_type: &Type(string_)
	}))
	assert int_alias != string_alias
}

fn test_fn_param_mutability_participates_in_type_identity() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	immutable := Type(tc.fn_type([Type(int_)], &Type(void_), [false]))
	mutable := Type(tc.fn_type([Type(int_)], &Type(void_), [true]))
	default_immutable := Type(tc.fn_type([Type(int_)], &Type(void_), []bool{}))

	assert immutable.name() == 'fn(int)'
	assert mutable.name() == 'fn(mut int)'
	assert semantic_types_equal(immutable, default_immutable)
	assert semantic_type_hash(immutable) == semantic_type_hash(default_immutable)
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
	assert cloned_fn.params.map(it.is_mut) == [true]
	assert cloned.name() == 'fn(mut int)'
}

fn test_c_type_cache_keys_composite_types_by_type_id() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	typ := Type(Pointer{
		base_type: &Type(Struct{
			name_id: flat.source_type_id('sample.Item')
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
		key_type:   &Type(string_)
		value_type: &Type(Array{
			elem_type: &Type(int_)
		})
	})
	first := tc.type_name(typ)
	second := tc.type_name(typ)
	assert first == flat.source_type_id('map[string][]int')
	assert first == second
}

fn test_recursive_callback_alias_parses_once_and_keeps_its_abi() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set_id('Handlers', flat.source_type_id('map[string]fn (Handlers) int'))
	tc.type_cache.parse_enabled = true

	typ := tc.parse_type(flat.source_type_id('Handlers'))
	assert typ is Alias
	base := (typ as Alias).base_type
	assert base is Map
	callback := (base as Map).value_type
	assert callback is FnType
	param := (callback as FnType).params[0].typ
	assert param is Alias
	assert (param as Alias).name() == 'Handlers'
	assert tc.c_type(param) == 'map'
	assert tc.parse_type(flat.source_type_id('Handlers')).name() == 'Handlers'
	assert tc.type_cache.alias_parse_stack.len == 0
}

fn test_fn_type_with_spaced_empty_parameter_list_has_no_void_parameter() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)

	typ := tc.parse_type(flat.source_type_id('fn ( ) int'))
	assert typ is FnType
	fn_typ := typ as FnType
	assert fn_typ.params.len == 0
	assert fn_typ.return_type.name() == 'int'
	assert Type(fn_typ).name() == 'fn() int'
}

fn test_voidptr_is_not_implicitly_compatible_with_callback_type() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)

	void_pointer := tc.parse_type(flat.source_type_voidptr)
	callback := tc.parse_type(flat.source_type_id('fn ()'))
	assert !tc.type_compatible(void_pointer, callback)
}

fn test_postfix_fixed_array_of_generic_struct_parses_before_generic_application() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	generic := tc.parse_type(flat.source_type_id('arc.Arc[Resource]'))
	assert generic is Struct
	assert generic.name() == 'arc.Arc[Resource]'
	nested_map := tc.parse_type(flat.source_type_id('map[string]map[string]arc.Arc[Resource]'))
	assert nested_map is Map
	inner_map := (nested_map as Map).value_type
	assert inner_map is Map
	inner_value := (inner_map as Map).value_type
	assert inner_value is Struct
	assert inner_value.name() == 'arc.Arc[Resource]'
	typ := tc.parse_type(flat.source_type_id('arc.Arc[Resource][2]'))
	assert typ is ArrayFixed
	fixed := typ as ArrayFixed
	assert fixed.len == 2
	assert fixed.elem_type.name() == 'arc.Arc[Resource]'
}

fn test_generic_text_substitution_recurses_through_wrappers() {
	assert flat.source_type_name(subst_generic_text(flat.source_type_id('chan T'), flat.TypeTextList.from_names(['int']).ids, ['T'])) == 'chan int'
	assert flat.source_type_name(subst_generic_text(flat.source_type_id('thread T'), flat.TypeTextList.from_names(['string']).ids, ['T'])) == 'thread string'
	assert flat.source_type_name(subst_generic_text(flat.source_type_id('atomic T'), flat.TypeTextList.from_names(['u64']).ids, ['T'])) == 'atomic u64'
	assert flat.source_type_name(subst_generic_text(flat.source_type_id('chan ?[]T'), flat.TypeTextList.from_names(['i16']).ids, ['T'])) == 'chan ?[]i16'
}

fn test_generic_type_substitution_preserves_padded_source_argument_semantics() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	typ := tc.parse_type(flat.source_type_id('fn (T) ?[]T'))
	args := [flat.source_type_id(' \tint\n')]
	substituted := tc.substitute_generic_type(typ, args, ['T'])
	expected := tc.parse_type(flat.source_type_id('fn (int) ?[]int'))
	assert substituted.name() == expected.name()
	assert flat.source_type_name(args[0]) == ' \tint\n'
}

fn test_generic_type_substitution_updates_fixed_array_length_expr() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	typ := tc.parse_type(flat.source_type_id('[cache_line_size - sizeof(T)]u8'))
	assert typ is ArrayFixed

	text_substituted := tc.substitute_generic_type(typ, flat.TypeTextList.from_names(['int']).ids, ['T'])
	assert text_substituted is ArrayFixed
	assert (text_substituted as ArrayFixed).len_expr == 'cache_line_size - sizeof(int)'

	value_substituted := tc.substitute_generic_type_values(typ, [Type(int_)], ['T'])
	assert value_substituted is ArrayFixed
	assert (value_substituted as ArrayFixed).len_expr == 'cache_line_size - sizeof(int)'
	assert subst_generic_const_expr('sizeof(T) + T_SIZE', flat.TypeTextList.from_names(['u64']).ids, ['T']) ==
		'sizeof(u64) + T_SIZE'
}

fn test_generic_text_substitution_preserves_mut_fn_pointer_params() {
	substituted := flat.source_type_name(subst_generic_text(flat.source_type_id('fn (mut T) string'), flat.TypeTextList.from_names(['&Dog']).ids, ['T']))
	assert substituted == 'fn(mut &Dog) string'

	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	callback_type := tc.parse_type(flat.source_type_id(substituted))
	assert callback_type is FnType
	fn_type := callback_type as FnType
	assert fn_type.params.map(it.is_mut) == [true]
	assert fn_compatible_param_type(fn_type, 0).name() == '&&Dog'
	assert tc.c_type(callback_type) == 'fn_ptr:string|Dog**'
	assert subst_generic_signature_param_text('f fn (mut it T) string', flat.TypeTextList.from_names(['&Dog']).ids, ['T']) == 'fn(mut &Dog) string'
}

fn test_mut_callback_param_keeps_declared_and_slot_pointers_apart() {
	assert normalize_fn_type_param_text('Dog') == 'Dog'
	assert normalize_fn_type_param_text('mut Dog') == '&Dog'
	assert normalize_fn_type_param_text('&Dog') == '&Dog'
	assert normalize_fn_type_param_text('mut &Dog') == '&&Dog'
	assert normalize_fn_type_param_text('mut it &Dog') == '&&Dog'

	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	assert tc.c_type(tc.parse_type(flat.source_type_id('fn (Dog)'))) == 'fn_ptr:void|Dog'
	assert tc.c_type(tc.parse_type(flat.source_type_id('fn (mut Dog)'))) == 'fn_ptr:void|Dog*'
	assert tc.c_type(tc.parse_type(flat.source_type_id('fn (&Dog)'))) == 'fn_ptr:void|Dog*'
	assert tc.c_type(tc.parse_type(flat.source_type_id('fn (mut &Dog)'))) == 'fn_ptr:void|Dog**'

	for text in ['fn(Dog)', 'fn(mut Dog)', 'fn(&Dog)', 'fn(mut &Dog)'] {
		parsed := tc.parse_type(flat.source_type_id(text))
		assert parsed.name() == text
		assert tc.c_type(tc.parse_type(parsed.source_type_id())) == tc.c_type(parsed)
	}
}

fn test_concrete_generic_method_signature_candidates_flatten_nested_pointer_args() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	candidates := tc.concrete_generic_method_signature_candidates(flat.source_type_id('SimpleCache[string, &CacheItem[string, int]]'), 'set')
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

fn test_node_cache_reset_drops_stale_fn_values() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(8)
	tc.set_resolved_fn_value(3, 'previous.callback')
	assert tc.resolved_fn_value_name(3)? == 'previous.callback'
	// A checker reused on another AST must not resolve recycled ids to the
	// previous program's targets.
	tc.reset_node_caches(8)
	assert tc.resolved_fn_value_name(3) == none
}

fn test_caller_type_name_qualification_preserves_enclosing_generic_parameters() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'T')
	mut tc := TypeChecker.new(&a)
	tc.structs['consumer.T'] = []StructField{}
	tc.structs['consumer.Payload'] = []StructField{}
	tc.cur_module = 'unrelated'
	tc.enclosing_generic_param_masks = []u32{len: a.nodes.len, init: u32(1) << u32(`T` - `A`)}
	assert flat.source_type_name(tc.qualify_type_name_at(flat.source_type_id('T'), id, 'consumer')) == 'T'
	assert flat.source_type_name(tc.qualify_type_name_at(flat.source_type_id('Payload'), id, 'consumer')) == 'consumer.Payload'
	assert flat.source_type_name(tc.qualify_type_name_at(flat.source_type_id('int'), id, 'consumer')) == 'int'
	assert flat.source_type_name(tc.qualify_type_name_at(flat.source_type_id('UnknownName'), id, 'consumer')) == 'UnknownName'
	assert tc.cur_module == 'unrelated'
}

fn test_node_types_share_stable_canonical_storage() {
	ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.reset_node_caches(4096)
	first := Type(Array{ elem_type: &Type(int_) })
	second := Type(Array{ elem_type: &Type(int_) })
	tc.remember_expr_type(flat.NodeId(0), first)
	tc.remember_expr_type(flat.NodeId(1), second)
	stored := tc.expr_type_values[0]
	assert voidptr(stored) == voidptr(tc.expr_type_values[1])
	for i in 2 .. 4096 {
		typ := Type(Struct{ name_id: flat.source_type_id('Item${i}') })
		tc.remember_expr_type(flat.NodeId(i), typ)
	}
	assert voidptr(stored) == voidptr(tc.expr_type_values[0])
	assert stored.name() == '[]int'
	assert tc.cached_expr_type(flat.NodeId(4095))? == Type(Struct{ name_id: flat.source_type_id('Item4095') })
}

fn test_integer_widening_uses_semantic_types_and_operand_positions() {
	mut a := flat.FlatAst.new()
	left := a.add_node(flat.Node{ kind: .ident, value: 'left' })
	right := a.add_node(flat.Node{ kind: .ident, value: 'right' })
	a.children << [left, right]
	expr := a.add_node(flat.Node{
		kind:           .infix
		op:             .plus
		children_count: 2
	})
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.trust_checked_expr_types = true
	tc.remember_expr_type(left, Type(u64_))
	tc.remember_expr_type(right, Type(u128_))
	narrow := [Type(int_), Type(u64_), Type(usize_), Type(rune_)]
	for typ in narrow {
		assert tc.widen_mixed_integer_expr_type(expr, typ) == Type(u128_)
	}
	alias := Type(Alias{ name_id: flat.source_type_id('user.int'), base_type: &Type(int_) })
	unchanged := [Type(bool_), Type(f64_), Type(string_), alias]
	for typ in unchanged {
		assert tc.widen_mixed_integer_expr_type(expr, typ) == typ
	}
	for op in [flat.Op.eq, .logical_and, .left_shift, .right_shift] {
		a.nodes[int(expr)].op = op
		assert tc.widen_mixed_integer_expr_type(expr, Type(u64_)) == Type(u64_)
	}
	tc.remember_expr_type(left, Type(i128_))
	assert tc.widen_mixed_integer_expr_type(expr, Type(int_)) == Type(i128_)
}

fn test_parsed_descriptors_borrow_canonical_storage() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.fast_type_text_refs = true
	parsed := tc.parse_type(flat.source_type_id('[]int'))
	annotated := tc.parse_type_ref(flat.source_type_id('[]int'))
	canonical := tc.parse_canonical_type_cached(flat.source_type_id('[]int'))
	assert voidptr(parsed) == voidptr(annotated)
	assert voidptr(parsed) == voidptr(canonical)
	for i in 1 .. 1024 {
		tc.parse_type(flat.source_type_id('[${i}]int'))
	}
	assert voidptr(parsed) == voidptr(tc.parse_type(flat.source_type_id('[]int')))
	mut value := *parsed
	value = Type(string_)
	assert value.name() == 'string'
	assert parsed.name() == '[]int'
}

fn test_field_caches_share_canonical_descriptors() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	integer := tc.parse_type(flat.source_type_id('int'))
	fields := [
		StructField{ name: 'left', typ: *integer },
		StructField{ name: 'right', typ: *integer },
	]
	owner := flat.source_type_id('Pair')
	tc.structs['Pair'] = fields
	tc.cache_direct_struct_field_types()
	shared := tc.type_cache.struct_field_shared[owner] or { panic('missing Pair') }
	left := shared.values['left'] or { panic('missing left') }
	right := shared.values['right'] or { panic('missing right') }
	assert voidptr(left) == voidptr(integer)
	assert voidptr(right) == voidptr(integer)
	assert tc.struct_field_type(owner, 'left')? == Type(int_)
	slot := struct_field_cache_slot(owner, 'left')
	assert voidptr(tc.type_cache.struct_field_recent_values[slot]) == voidptr(integer)
	tc.clear_field_lookup_cache()
	// A computed lookup must repopulate the private table with the same descriptor.
	tc.type_cache.struct_field_shared.clear()
	tc.type_cache.struct_field_complete.clear()
	assert tc.struct_field_type(owner, 'right')? == Type(int_)
	private := tc.type_cache.struct_field_entries[owner] or { panic('missing Pair') }
	cached := private.values['right'] or { panic('missing right') }
	assert voidptr(cached) == voidptr(integer)
}

fn test_function_signature_owns_canonical_parameter_edges() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	integer := tc.parse_type(flat.source_type_id('int'))
	mut parameters := [Type(int_), Type(int_)]
	result := Type(string_)
	signature := tc.fn_type(parameters, result, [false, true])
	parameters[0] = Type(bool_)
	assert signature.params.len == 2
	assert signature.params[0].typ.name() == 'int'
	assert !signature.params[0].is_mut
	assert signature.params[1].is_mut
	assert voidptr(signature.params[0].typ) == voidptr(integer)
	assert voidptr(signature.params[1].typ) == voidptr(integer)
	assert voidptr(signature.return_type) == voidptr(tc.parse_type(flat.source_type_id('string')))
}

fn test_type_traversal_borrows_each_recursive_edge() {
	integer := &Type(int_)
	mut aliases := integer
	mut pointers := integer
	mut mixed := integer
	for i in 0 .. 128 {
		aliases = &Type(Alias{ name_id: flat.source_type_id('Alias${i}'), base_type: aliases })
		pointers = &Type(Pointer{ base_type: pointers })
		parent := &Type(Pointer{ base_type: mixed })
		mixed = &Type(Alias{ name_id: flat.source_type_id('Mixed${i}'), base_type: parent })
	}
	assert *unalias_type(aliases) == *integer
	assert unwrap_all_pointers(pointers) == *integer
	assert unwrap_pointer(pointers) is Pointer
	assert unalias_and_unwrap_pointer_type(mixed) == *integer
	assert aliases is Alias
	assert pointers is Pointer
	assert mixed is Alias
}

fn test_unalias_borrows_the_original_descriptor() {
	base := Type(Array{ elem_type: &Type(int_) })
	inner := Type(Alias{ name_id: flat.source_type_id('Inner'), base_type: &base })
	outer := Type(Alias{ name_id: flat.source_type_id('Outer'), base_type: &inner })
	assert voidptr(unalias_type(&base)) == voidptr(&base)
	assert voidptr(unalias_type(&inner)) == voidptr(&base)
	assert voidptr(unalias_type(&outer)) == voidptr(&base)
	mut replacement := *unalias_type(&outer)
	replacement = Type(string_)
	assert replacement is String
	assert base is Array
	assert outer.name() == 'Outer'
}

fn retained_type_reference() &Type {
	local := Type(Primitive{ props: .integer, size: 16 })
	return new_type_reference(local)
}

fn test_new_type_reference_owns_its_descriptor_copy() {
	reference := retained_type_reference()
	mut original := Type(int_)
	copy := new_type_reference(original)
	original = Type(string_)
	for i in 0 .. 128 {
		other := new_type_reference(Type(Primitive{ props: .integer, size: u8(i) }))
		assert other is Primitive
		assert other.size == u8(i)
	}
	assert original is String
	assert copy is Primitive
	assert copy.size == 0
	assert reference is Primitive
	assert reference.size == 16
}

fn test_cached_expr_type_references_preserve_storage_and_visibility() {
	ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.reset_node_caches(8)
	tc.remember_expr_type(flat.NodeId(1), Type(int_))
	tc.remember_expr_type(flat.NodeId(5), Type(string_))
	dense := tc.cached_expr_type_ref(flat.NodeId(1))
	assert !isnil(dense)
	assert voidptr(dense) == voidptr(tc.expr_type_values[1])
	tc.parallel_check_sparse = true
	tc.check_range_lo = 0
	tc.check_range_hi = 4
	assert isnil(tc.cached_expr_type_ref(flat.NodeId(5)))
	tc.sparse_expr_type_values[5] = Type(bool_)
	sparse := tc.cached_expr_type_ref(flat.NodeId(5))
	assert !isnil(sparse)
	assert *sparse == Type(bool_)
	for i in 8 .. 4096 {
		tc.sparse_expr_type_values[i] = Type(int_)
	}
	tc.sparse_expr_type_values[5] = Type(string_)
	assert *sparse == Type(bool_)
	assert *dense == Type(int_)
	assert tc.cached_expr_type(flat.NodeId(5))? == Type(string_)

	mut probe := tc.fork_storage_observation_view()
	assert *probe.cached_expr_type_ref(flat.NodeId(1)) == Type(int_)
	assert *probe.cached_expr_type_ref(flat.NodeId(5)) == Type(string_)
	probe.sparse_expr_type_values[5] = Type(u64_)
	assert *probe.cached_expr_type_ref(flat.NodeId(5)) == Type(u64_)
	assert *tc.cached_expr_type_ref(flat.NodeId(5)) == Type(string_)
	tc.invalidate_checked_expr_type(1)
	assert isnil(tc.cached_expr_type_ref(flat.NodeId(1)))
	assert *dense == Type(int_)
}

fn test_expr_type_references_preserve_call_precedence_and_canonical_storage() {
	mut ast := flat.FlatAst.new()
	ident := ast.add_node(flat.Node{ kind: .ident, value: 'value' })
	call := ast.add_node(flat.Node{ kind: .call, typ: flat.source_type_id('string') })
	mut tc := TypeChecker.new(&ast)
	tc.reset_node_caches(ast.nodes.len)
	assert tc.expr_type_reference(ident) == none
	assert tc.expr_type_reference(flat.NodeId(-1)) == none
	tc.remember_expr_type(ident, Type(int_))
	assert voidptr(tc.expr_type_reference(ident)?) == voidptr(tc.expr_type_values[int(ident)])
	assert *tc.expr_type_reference(ident)? == tc.expr_type(ident)?
	assert *tc.expr_type_reference(call)? == Type(string_)
	tc.remember_resolved_call(call, 'main.target')
	tc.fn_ret_types['main.target'] = Type(bool_)
	resolved := tc.expr_type_reference(call)?
	assert *resolved == Type(bool_)
	tc.fn_ret_types['main.target'] = Type(string_)
	assert *resolved == Type(bool_)
	tc.remember_expr_type(call, Type(int_))
	assert voidptr(tc.expr_type_reference(call)?) == voidptr(tc.expr_type_values[int(call)])
	assert *tc.expr_type_reference(call)? == tc.expr_type(call)?
	// Legacy call metadata can retain Void even though new writes skip it.
	tc.expr_type_values[int(call)] = tc.intern_type_reference(Type(void_))
	assert *tc.expr_type_reference(call)? == Type(string_)
	tc.fn_ret_types.delete('main.target')
	assert *tc.expr_type_reference(call)? == Type(string_)
	ast.nodes[int(call)].typ = flat.source_type_int
	assert *tc.expr_type_reference(call)? == Type(void_)
	tc.invalidate_checked_expr_type(int(call))
	assert tc.expr_type_reference(call) == none
	for placeholder in [u32(0), flat.source_type_int, flat.source_type_unknown,
		qualify_source_type_array, qualify_source_type_map] {
		ast.nodes[int(call)].typ = placeholder
		assert tc.expr_type(call) == none
		assert tc.expr_type_reference(call) == none
	}
	tc.parallel_check_sparse = true
	tc.check_range_lo = int(call)
	tc.check_range_hi = int(call)
	assert tc.expr_type_reference(ident) == none
	tc.sparse_expr_type_values[int(ident)] = Type(bool_)
	sparse := tc.expr_type_reference(ident)?
	for i in 8 .. 4096 {
		tc.sparse_expr_type_values[i] = Type(string_)
	}
	tc.sparse_expr_type_values[int(ident)] = Type(string_)
	assert *sparse == Type(bool_)
	assert *tc.expr_type_reference(ident)? == tc.expr_type(ident)?
}
