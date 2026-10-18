module fastc

import v.flat
import v.pref
import v.scanner

fn direct_member32_parser() Parser {
	prefs := pref.new_preferences()
	mut record_fields := flat.TypeTextBindings{}
	record_fields.set('count', 'int')
	record_fields.set('text', 'string')
	record_fields.set('next', 'Record*')
	record_fields.set('optional', 'Option')
	record_fields.set('callback', 'voidptr')
	mut leaf_fields := flat.TypeTextBindings{}
	leaf_fields.set('count', 'bool')
	mut array_fields := flat.TypeTextBindings{}
	array_fields.set('len', 'int')
	array_fields.set('data', 'voidptr')
	mut map_fields := flat.TypeTextBindings{}
	map_fields.set('key_values', 'voidptr')
	mut aliases := flat.TypeTextBindings{}
	aliases.set('AliasMode', 'Mode')
	declared := {
		'Record':    true
		'Leaf':      true
		'Mode':      true
		'AliasMode': true
		'Iface':     true
	}
	return Parser{
		prefs:                     prefs
		s:                         scanner.new_scanner(prefs, .normal)
		selfhost:                  true
		module_name:               'fixture'
		declared_types:            declared
		declared_type_c_names:     fastc_declared_type_c_names(declared)
		declared_type_key_by_name: fastc_declared_type_key_by_name(declared)
		declared_kinds:            {
			'Record':    .struct_
			'Leaf':      .struct_
			'Mode':      .enum_
			'AliasMode': .alias_
			'Iface':     .interface_
		}
		alias_base_types:          aliases
		struct_fields:             {
			'Record': record_fields
			'Leaf':   leaf_fields
			'array':  array_fields
			'map':    map_fields
		}
		struct_field_info:         {
			'Record': [FastcStructField{ name: '__embedded_0', typ: flat.source_type_id('Leaf') }]
			'Leaf':   [FastcStructField{ name: 'promoted', typ: flat.source_type_int }]
		}
		interface_fields:          {
			'Iface.id': FastcInterfaceField{ name: 'id', typ: flat.source_type_int }
		}
		locals:                    {
			'value':     FastcLocal{ typ: flat.source_type_id('Record') }
			'pointer':   FastcLocal{ typ: flat.source_type_id('Record*'), is_mut: true, is_reference: true, c_name: '__receiver' }
			'items':     FastcLocal{ typ: flat.source_type_id('Array_int') }
			'lookup':    FastcLocal{ typ: flat.source_type_id('Map_string_int') }
			'iface':     FastcLocal{ typ: flat.source_type_id('Iface') }
			'fixed':     FastcLocal{ typ: flat.source_type_id('FixedArray_3_int') }
			'Mode':      FastcLocal{ typ: flat.source_type_id('Record') }
			'AliasMode': FastcLocal{ typ: flat.source_type_id('Record') }
		}
	}
}

fn direct_member32_tokens(root string, field string) []FastcExpressionToken {
	return [FastcExpressionToken{ tok: .name, lit: root }, FastcExpressionToken{ tok: .dot, lit: '.' },
		FastcExpressionToken{ tok: .name, lit: field }]
}

fn direct_member32_result(g &Parser, tokens []FastcExpressionToken, start int, end int, legacy bool) string {
	if legacy {
		// These pure member expressions ignore argument markers in legacy inference.
		// Mark every token so parentheses cannot hide the marker from the shortcut.
		mut fallback := tokens.clone()
		for i in 0 .. fallback.len {
			fallback[i] = FastcExpressionToken{ ...fallback[i], is_mut_argument: true }
		}
		value := g.infer_expression_type_range_impl(fallback, start, end) or { return 'error: ' + err.msg() }
		return '${value}'
	}
	value := g.infer_expression_type_range_impl(tokens, start, end) or { return 'error: ' + err.msg() }
	return '${value}'
}

fn direct_member32_assert_legacy(g &Parser, tokens []FastcExpressionToken) {
	assert direct_member32_result(g, tokens, 0, tokens.len, false) == direct_member32_result(g, tokens, 0, tokens.len, true), '${tokens}'
}

fn test_direct_member32_accepts_exact_existing_direct_lookup() {
	g := direct_member32_parser()
	expected_fields := {
		'count':    flat.source_type_int
		'text':     flat.source_type_string
		'next':     flat.source_type_id('Record*')
		'optional': flat.source_type_id('Option')
		'callback': flat.source_type_voidptr
	}
	for root in ['value', 'pointer'] {
		for field in ['count', 'text', 'next', 'optional', 'callback'] {
			tokens := direct_member32_tokens(root, field)
			expected := expected_fields[field]
			assert g.infer_local_direct_member_type(tokens, 0, tokens.len) == expected
			assert g.infer_expression_type(tokens) or { panic(err) } == expected
			direct_member32_assert_legacy(&g, tokens)
			mut wrapped := [FastcExpressionToken{ tok: .lpar, lit: '(' }]
			wrapped << tokens
			wrapped << FastcExpressionToken{ tok: .rpar, lit: ')' }
			direct_member32_assert_legacy(&g, wrapped)
			assert g.infer_expression_type(wrapped) or { panic(err) } == expected
			mut subrange := [FastcExpressionToken{ tok: .number, lit: '99' }]
			subrange << tokens
			subrange << FastcExpressionToken{ tok: .key_true }
			assert direct_member32_result(&g, subrange, 1, 4, false) == direct_member32_result(&g, subrange, 1, 4, true)
		}
	}
	expected_containers := {
		'items.len':         flat.source_type_int
		'items.data':        flat.source_type_voidptr
		'lookup.len':        flat.source_type_int
		'lookup.key_values': flat.source_type_voidptr
		'iface.id':          flat.source_type_int
	}
	for pair in [['items', 'len'], ['items', 'data'], ['lookup', 'len'], ['lookup', 'key_values'],
		['iface', 'id']] {
		tokens := direct_member32_tokens(pair[0], pair[1])
		expected := expected_containers['${pair[0]}.${pair[1]}']
		assert g.infer_local_direct_member_type(tokens, 0, 3) == expected
		assert g.infer_expression_type(tokens) or { panic(err) } == expected
		direct_member32_assert_legacy(&g, tokens)
	}
	assert g.temp_id == 0
}

fn test_direct_member32_preserves_ambiguous_roots_and_missing_direct_fields() {
	mut g := direct_member32_parser()
	for pair in [['Mode', 'count'], ['AliasMode', 'count'], ['value', 'promoted'], ['value', 'absent'],
		['fixed', 'len'], ['unknown', 'count'], ['C', 'count'], ['@value', 'count'],
		['value', '@count']] {
		tokens := direct_member32_tokens(pair[0], pair[1])
		assert g.infer_local_direct_member_type(tokens, 0, 3) == 0
		direct_member32_assert_legacy(&g, tokens)
	}
	g.imports['value'] = 'external'
	mut types := flat.TypeTextBindings{}
	types.set('external.count', 'string')
	g.constant_types = types
	direct_member32_assert_legacy(&g, direct_member32_tokens('value', 'count'))
	assert g.infer_local_direct_member_type(direct_member32_tokens('value', 'count'), 0, 3) == 0
	g.imports.clear()
	g.reset_lookup_memos()
	g.member_smartcasts['value.count'] = FastcMemberSmartcast{ typ: flat.source_type_string }
	direct_member32_assert_legacy(&g, direct_member32_tokens('value', 'count'))
	assert g.infer_local_direct_member_type(direct_member32_tokens('value', 'count'), 0, 3) == 0
	g.member_smartcasts.clear()
	for local in [FastcLocal{},
		FastcLocal{ typ: flat.source_type_id('Record'), option_value_type: flat.source_type_int },
		FastcLocal{ typ: flat.source_type_id('Record*'), is_reference: true, smartcast_origin_type: flat.source_type_int },
		FastcLocal{ typ: flat.source_type_id('Record'), smartcast_origin_source: '__origin' }] {
		g.locals['candidate'] = local
		tokens := direct_member32_tokens('candidate', 'count')
		assert g.infer_local_direct_member_type(tokens, 0, 3) == 0
		direct_member32_assert_legacy(&g, tokens)
	}
}

fn test_direct_member32_rejects_synthetic_metadata_and_contexts() {
	mut g := direct_member32_parser()
	base := direct_member32_tokens('value', 'count')
	for index in 0 .. 3 {
		for changed in [FastcExpressionToken{ ...base[index], source: '__rendered' },
			FastcExpressionToken{ ...base[index], is_mut_argument: true },
			FastcExpressionToken{ ...base[index], is_statement: true },
			FastcExpressionToken{ ...base[index], is_capturing_closure: true },
			FastcExpressionToken{ ...base[index], is_storage: true },
			FastcExpressionToken{ ...base[index], chan_element_type: flat.source_type_int },
			FastcExpressionToken{ ...base[index], option_value_type: flat.source_type_int }] {
			mut tokens := base.clone()
			tokens[index] = changed
			assert g.infer_local_direct_member_type(tokens, 0, 3) == 0
			direct_member32_assert_legacy(&g, tokens)
		}
	}
	for index in 0 .. 3 {
		mut tokens := base.clone()
		tokens[index].typ = flat.source_type_id('Record*')
		if index > 0 || tokens[index].typ != g.locals['value'].typ {
			assert g.infer_local_direct_member_type(tokens, 0, 3) == 0
		}
		direct_member32_assert_legacy(&g, tokens)
	}
	for mode in 0 .. 5 {
		mut context := Parser{ ...g, selfhost: mode != 0, translated: mode == 1, in_mono_drain: mode == 2, in_generic_placeholder: mode == 3 }
		if mode == 4 {
			context.member_smartcasts['unrelated'] = FastcMemberSmartcast{ typ: flat.source_type_int }
		}
		assert context.infer_local_direct_member_type(base, 0, 3) == 0
		direct_member32_assert_legacy(&context, base)
	}
}

fn test_direct_member32_preserves_explicit_old_precedence() {
	mut g := direct_member32_parser()
	for name in ['Mode', 'AliasMode'] {
		tokens := direct_member32_tokens(name, 'count')
		// The enum type wins over the conflicting local Record.count : int.
		assert g.infer_expression_type(tokens) or { panic(err) } == flat.source_type_id('Mode')
		direct_member32_assert_legacy(&g, tokens)
	}
	g.imports['value'] = 'external'
	g.constant_types.set('external.count', 'string')
	g.reset_lookup_memos()
	tokens := direct_member32_tokens('value', 'count')
	// The imported constant wins over the conflicting local Record.count : int.
	assert g.infer_expression_type(tokens) or { panic(err) } == flat.source_type_string
	direct_member32_assert_legacy(&g, tokens)
	mut imported_types := g.declared_types.clone()
	imported_types['external.count'] = true
	imported_type := Parser{
		...g
		// A fresh declaration context cannot keep expression/name memo entries.
		type_memo:                 map[i64]u32{}
		declared_type_key_memo:    map[string]FastcMemoEntry{}
		declared_types:            imported_types
		declared_type_c_names:     fastc_declared_type_c_names(imported_types)
		declared_type_key_by_name: fastc_declared_type_key_by_name(imported_types)
	}
	// A module-qualified type takes precedence even over the imported constant.
	assert imported_type.infer_expression_type(tokens) or { panic(err) } == flat.source_type_id('external__count')
	direct_member32_assert_legacy(&imported_type, tokens)
	g.imports.clear()
	g.reset_lookup_memos()
	g.member_smartcasts['value.count'] = FastcMemberSmartcast{ typ: flat.source_type_string }
	assert g.infer_expression_type(tokens) or { panic(err) } == flat.source_type_string
	direct_member32_assert_legacy(&g, tokens)
	g.member_smartcasts.clear()
	g.member_smartcasts['value'] = FastcMemberSmartcast{ typ: flat.source_type_id('Leaf') }
	assert g.infer_expression_type(tokens) or { panic(err) } == flat.source_type_bool
	direct_member32_assert_legacy(&g, tokens)
	g.member_smartcasts.clear()
	mut annotated := tokens.clone()
	annotated[0].typ = flat.source_type_id('Leaf')
	// An explicit receiver annotation takes precedence over the local's Record type.
	assert g.infer_expression_type(annotated) or { panic(err) } == flat.source_type_bool
	direct_member32_assert_legacy(&g, annotated)
	for index in [1, 2] {
		mut ignored_annotation := tokens.clone()
		ignored_annotation[index].typ = flat.source_type_string
		// Dot/member annotations do not replace the declared field's type.
		assert g.infer_expression_type(ignored_annotation) or { panic(err) } == flat.source_type_int
		direct_member32_assert_legacy(&g, ignored_annotation)
	}
}
