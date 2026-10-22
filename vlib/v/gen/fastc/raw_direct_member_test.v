module fastc

import strings
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_raw_direct_member29_parser(source string) Parser {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('atomic_member.v', source.len), source)
	first := scan.scan()
	mut record_types := flat.TypeTextBindings{}
	for name in ['count', 'short'] { record_types.set(name, 'int') }
	record_types.set('items', 'Array_int')
	record_types.set('next', 'Record*')
	mut array_types := flat.TypeTextBindings{}
	array_types.set('len', 'int')
	array_types.set('cap', 'int')
	mut string_types := flat.TypeTextBindings{}
	string_types.set('len', 'int')
	fields := {
		'Record': [
			FastcStructField{ name: 'count', typ: flat.source_type_int },
			FastcStructField{ name: 'short', typ: flat.source_type_int },
			FastcStructField{ name: 'items', typ: flat.source_type_id('Array_int') },
			FastcStructField{ name: 'next', typ: flat.source_type_id('Record*') },
			FastcStructField{ name: 'promoted', typ: flat.source_type_int, storage_path: ['__embedded_0'] },
			FastcStructField{ name: 'shared', typ: flat.source_type_int, is_shared_pointer: true },
			FastcStructField{ name: 'callback', typ: flat.source_type_voidptr, is_function: true },
			FastcStructField{ name: 'optional', typ: fastc_type_option, option_value_type: flat.source_type_int },
			FastcStructField{ name: 'generic', typ: flat.source_type_int, generic_argument_type: flat.source_type_int },
			FastcStructField{ name: 'channel', typ: flat.source_type_id('chan'), chan_element_type: flat.source_type_int },
			FastcStructField{ name: 'fixed', typ: flat.source_type_id('FixedArray_2_int') },
			FastcStructField{ name: 'alias', typ: flat.source_type_id('Alias') },
		]
		'array':  [FastcStructField{ name: 'len', typ: flat.source_type_int },
			FastcStructField{ name: 'cap', typ: flat.source_type_int }]
		'string': [FastcStructField{ name: 'len', typ: flat.source_type_int }]
	}
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Alias', 'Record')
	aliases.set('Shade', 'Mode')
	aliases.set('PointerShade', 'Mode')
	return Parser{
		prefs:                    prefs
		s:                        scan
		tok:                      first
		lit:                      scan.lit
		selfhost:                 true
		module_name:              'fixture'
		unsafe_depth:             2
		expected_expression_type: flat.source_type_id('Iface')
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_expression_type:     flat.source_type_void
		last_option_value_type:   flat.source_type_string
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		declared_kinds:           {
			'Record':       .struct_
			'string':       .struct_
			'Alias':        .alias_
			'Iface':        .interface_
			'Mode':         .enum_
			'Shade':        .alias_
			'PointerShade': .alias_
		}
		declared_types:           {
			'Mode':         true
			'Shade':        true
			'PointerShade': true
		}
		alias_base_types:         aliases
		sum_types:                {
			'Choice': true
		}
		struct_fields:            {
			'Record': record_types
			'array':  array_types
			'string': string_types
		}
		struct_field_info:        fields
		struct_field_lookup:      fastc_build_struct_field_lookup(fields)
		generic_method_names:     {
			'generic_field': true
		}
		locals:                   {
			'value':        FastcLocal{ typ: flat.source_type_id('Record') }
			'Mode':         FastcLocal{ typ: flat.source_type_id('Record') }
			'Shade':        FastcLocal{ typ: flat.source_type_id('Record'), c_name: 'Shade__short' }
			'PointerShade': FastcLocal{ typ: flat.source_type_id('Record*'), c_name: 'PointerShade__short' }
			'count':        FastcLocal{ typ: flat.source_type_id('Record*') }
			'mutable':      FastcLocal{ typ: flat.source_type_id('Record'), is_mut: true }
			'pointer':      FastcLocal{ typ: flat.source_type_id('Record*') }
			'reference':    FastcLocal{ typ: flat.source_type_id('Record*'), is_reference: true, is_mut: true }
			'renamed':      FastcLocal{ typ: flat.source_type_id('Record*'), c_name: '__record_value' }
			'double':       FastcLocal{ typ: flat.source_type_id('Record**') }
			'items':        FastcLocal{ typ: flat.source_type_id('Array_int') }
			'item_ref':     FastcLocal{ typ: flat.source_type_id('Array_int*'), is_reference: true }
			'text':         FastcLocal{ typ: flat.source_type_string }
			'map':          FastcLocal{ typ: flat.source_type_id('Map_string_int') }
			'fixed':        FastcLocal{ typ: flat.source_type_id('FixedArray_2_int') }
			'alias':        FastcLocal{ typ: flat.source_type_id('Alias') }
			'boxed':        FastcLocal{ typ: flat.source_type_id('Choice') }
			'iface':        FastcLocal{ typ: flat.source_type_id('Iface') }
			'option':       FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_id('Record') }
			'origin':       FastcLocal{ typ: flat.source_type_id('Record'), smartcast_origin_type: flat.source_type_id('Choice') }
			'C':            FastcLocal{ typ: flat.source_type_id('Record') }
		}
	}
}

fn raw_direct_member29_tokens(root string, field string) []FastcExpressionToken {
	return [FastcExpressionToken{ tok: .name, lit: root }, FastcExpressionToken{ tok: .dot, lit: '.' },
		FastcExpressionToken{ tok: .name, lit: field }]
}

fn test_fastc_raw_direct_member_matches_frozen_general_renderer() {
	mut accepted := 0
	for root in ['value', 'mutable', 'pointer', 'reference', 'renamed', 'double', 'items', 'item_ref',
		'text', 'map', 'fixed', 'alias', 'boxed', 'iface', 'option', 'origin', 'count'] {
		for field in ['count', 'short', 'items', 'next', 'promoted', 'shared', 'callback', 'optional',
			'generic', 'channel', 'fixed', 'alias', 'len', 'cap', 'float', 'int', 'type', 'unknown',
			'generic_field'] {
			for typed in [false, true] {
				g := fastc_raw_direct_member29_parser('')
				legacy := fastc_raw_direct_member29_parser('')
				mut tokens := raw_direct_member29_tokens(root, field)
				if typed { tokens[0].typ = g.locals[root].typ }
				tokens_before := tokens.clone()
				expected := legacy.raw_direct_member29_legacy(tokens) or { panic('legacy ${root}.${field}') }
				actual := g.render_raw_expression_tokens(tokens) or { panic('new ${root}.${field}') }
				assert actual == expected, '${root}.${field}: ${actual} != ${expected}'
				assert tokens == tokens_before
				if direct := g.render_raw_local_direct_member(tokens) {
					accepted++
					assert direct == expected
				} else {
					assert root == 'map' && field == 'len'
				}
			}
		}
	}
	assert accepted > 500
}

fn test_fastc_raw_direct_member_preserves_rejected_cases() {
	for root in ['C', 'Mode', 'Shade', 'PointerShade', 'unknown', '@OS'] {
		g := fastc_raw_direct_member29_parser('')
		legacy := fastc_raw_direct_member29_parser('')
		tokens := raw_direct_member29_tokens(root, 'short')
		assert g.render_raw_local_direct_member(tokens) == none
		assert g.render_raw_expression_tokens(tokens) == legacy.raw_direct_member29_legacy(tokens)
	}
	for at in 0 .. 3 {
		for with_source in [false, true] {
			g := fastc_raw_direct_member29_parser('')
			legacy := fastc_raw_direct_member29_parser('')
			mut tokens := raw_direct_member29_tokens('value', 'count')
			old := tokens[at]
			tokens[at] = FastcExpressionToken{
				...old
				source:          fastc_token_source(if with_source {
					'rendered_piece'
				} else {
					''
				})
				is_mut_argument: !with_source
			}
			assert g.render_raw_local_direct_member(tokens) == none
			assert g.render_raw_expression_tokens(tokens) == legacy.raw_direct_member29_legacy(tokens)
		}
	}
	g := fastc_raw_direct_member29_parser('')
	for empty_dot in [false, true] {
		mut tokens := raw_direct_member29_tokens('renamed', 'short')
		if empty_dot { tokens[1] = FastcExpressionToken{ tok: .dot } }
		assert g.render_raw_local_direct_member(tokens) == g.raw_direct_member29_legacy(tokens)
	}
}

fn test_fastc_raw_direct_member_preserves_contextual_len_and_import_shadows() {
	for root in ['value', 'map'] {
		for token_type in ['Record', 'Map_string_int', 'Map_string_int*'] {
			mut g := fastc_raw_direct_member29_parser('')
			mut legacy := fastc_raw_direct_member29_parser('')
			g.imports[root] = 'shadowed.module'
			legacy.imports[root] = 'shadowed.module'
			mut tokens := raw_direct_member29_tokens(root, 'len')
			tokens[0].typ = flat.source_type_id(token_type)
			expected := legacy.raw_direct_member29_legacy(tokens) or { panic('legacy len') }
			assert g.render_raw_expression_tokens(tokens) or { panic('new len') } == expected
			if direct := g.render_raw_local_direct_member(tokens) {
				assert direct == expected
			}
		}
	}
	for c_name in ['renamed_record', '(*renamed_record)', '((Record*)raw_record)', 'Mode__short'] {
		mut g := fastc_raw_direct_member29_parser('')
		mut legacy := fastc_raw_direct_member29_parser('')
		g.locals['value'] = FastcLocal{ typ: flat.source_type_id('Record*'), c_name: c_name }
		legacy.locals['value'] = FastcLocal{ typ: flat.source_type_id('Record*'), c_name: c_name }
		tokens := raw_direct_member29_tokens('value', 'float')
		assert g.render_raw_local_direct_member(tokens) == legacy.raw_direct_member29_legacy(tokens)
	}
}

// Frozen pre-shortcut raw renderer used as an independent spelling oracle.
fn (g &Parser) raw_direct_member29_legacy(tokens []FastcExpressionToken) ?string {
	if tokens.len == 1 {
		item := tokens[0]
		if item.has_source() {
			return item.source_text()
		}
		return match item.tok {
			.name { g.resolved_expression_name(item.lit, .unknown) }
			.number {
				if g.selfhost {
					fastc_c_selfhost_number(item.lit)
				} else {
					fastc_c_number(item.lit) or { return none }
				}
			}
			.string {
				literal := fastc_c_string(item.lit) or { return none }
				if g.selfhost { '_S(${literal})' } else { literal }
			}
			.char {
				if item.lit.starts_with('c:') {
					fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
				} else {
					fastc_c_rune(item.lit) or { return none }
				}
			}
			.key_true { '((bool)true)' }
			.key_false { '((bool)false)' }
			.key_nil { 'NULL' }
			.key_none { '(Option){.state=2}' }
			else {
				if item.lit == '' { item.tok.str() } else { item.lit }
			}
		}
	}
	mut result := strings.new_builder(32)
	mut cast_closes := []int{}
	mut cast_open := -1
	mut previous_module_separator := false
	mut skip_until := -1
	for i, item in tokens {
		if i <= skip_until {
			continue
		}
		mut piece := item.lit
		map_length_receiver_type := if item.tok == .dot {
			flat.source_type_name(g.map_length_receiver_type(tokens, i) or { u32(0) })
		} else {
			''
		}
		if item.tok == .right_shift_unsigned {
			// V's logical right shift `>>>` has no C spelling; on the unsigned operand it is
			// always applied to (V code casts to an unsigned type first, e.g. `u64(x) >>> n`)
			// a plain C `>>` is the logical shift.
			piece = '>>'
		}
		module_separator := item.tok == .dot && g.expression_dot_is_module_separator(tokens, i)
		if item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) {
			mut cast_open_index := i + 1
			for cast_open_index < tokens.len && tokens[cast_open_index].tok in [.amp, .and, .name,
				.dot] {
				cast_open_index++
			}
			if cast_open_index < tokens.len && tokens[cast_open_index].tok == .lpar {
				if cast_end := fastc_matching_rpar(tokens, cast_open_index) {
					if cast := g.render_cast_expression(tokens[i..cast_end + 1]) {
						result.write_string(cast.source)
						skip_until = cast_end
						continue
					}
				}
			}
		}
		is_direct_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) && i + 2 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 2].tok == .lpar && (fastc_primitive_c_type(tokens[i + 1].lit) != none || g.resolve_declared_type_key(tokens[i + 1].lit) != none)
		is_c_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) && i + 4 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 1].lit == 'C' && tokens[i + 2].tok == .dot && tokens[i + 3].tok == .name && tokens[i + 4].tok == .lpar
		if item.tok == .key_sizeof && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			piece = item.tok.str()
			if close := fastc_matching_rpar(tokens, i + 1) {
				if pointer_sizeof := g.render_pointer_type_sizeof(tokens[i..close + 1]) {
					piece = pointer_sizeof.source
					skip_until = close
				}
			}
		} else if item.has_source() {
			// Synthetic expression atoms (an `or` unwrap, interpolation, anonymous
			// function, etc.) carry their complete C spelling in `source`. Preserve it
			// when the atom is nested inside a larger binary or call expression.
			piece = item.source_text()
		} else if is_direct_pointer_cast || is_c_pointer_cast {
			piece = ''
		} else if item.tok == .name && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			is_member_call := i > 0 && tokens[i - 1].tok == .dot
			mut cast_type := if is_member_call {
				''
			} else {
				fastc_primitive_c_type(item.lit) or { '' }
			}
			is_c_cast := i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' && tokens[i - 1].tok == .dot && item.lit.len > 0 && 'C.${item.lit}' !in g.functions && fastc_call_has_one_argument(tokens, i + 1) && (item.lit[0].is_capital() || '#Cstruct#${item.lit}' in g.declared_types)
			if is_c_cast {
				cast_type = if '#Cstruct#${item.lit}' in g.declared_types {
					'struct ${item.lit}'
				} else {
					item.lit
				}
			}
			if cast_type == '' && !is_member_call {
				if type_key := g.resolve_declared_type_key(item.lit) {
					cast_type = fastc_c_declared_type_name(type_key)
				}
			}
			if cast_type != '' {
				pointer_token := if i > 0 && tokens[i - 1].tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i - 1) {
					tokens[i - 1].tok
				} else if is_c_cast && i >= 3 && tokens[i - 3].tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i - 3) {
					tokens[i - 3].tok
				} else {
					token.Token.unknown
				}
				pointer_suffix := '*'.repeat(if pointer_token == .and {
					2
				} else if pointer_token == .amp {
					1
				} else {
					0
				})
				physical_cast_type := if is_c_cast {
					cast_type
				} else {
					fastc_output_c_type(cast_type)
				}
				piece = '((${physical_cast_type}${pointer_suffix})('
				close := fastc_matching_rpar(tokens, i + 1) or { return none }
				cast_open = i + 1
				cast_closes << close
			} else {
				previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
				piece = if previous == .dot {
					item.lit
				} else {
					g.resolved_expression_name(item.lit, previous)
				}
			}
		} else if item.tok == .lpar && i == cast_open {
			piece = ''
		} else if item.tok == .rpar && i in cast_closes {
			piece = '))'
		} else if item.tok == .number {
			piece = if g.selfhost {
				fastc_c_selfhost_number(item.lit)
			} else {
				fastc_c_number(item.lit) or { return none }
			}
		} else if item.tok == .string {
			if item.has_source() {
				piece = item.source_text()
			} else {
				literal := fastc_c_string(item.lit) or { return none }
				piece = if g.selfhost { '_S(${literal})' } else { literal }
			}
		} else if item.tok == .char {
			piece = if item.lit.starts_with('c:') {
				fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
			} else {
				fastc_c_rune(item.lit) or { return none }
			}
		} else if item.tok == .key_true {
			piece = '((bool)true)'
		} else if item.tok == .key_false {
			piece = '((bool)false)'
		} else if item.tok == .key_nil {
			piece = 'NULL'
		} else if item.tok == .key_none {
			piece = '(Option){.state=2}'
		} else if item.tok == .name {
			previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
			piece = if previous == .dot && item.lit == 'len'
				&& g.map_length_receiver_type(tokens, i - 1) != none {
				'count'
			} else if previous == .dot && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
				item.lit
			} else if previous == .dot && previous_module_separator {
				// The module prefix already makes a qualified keyword-named constant safe
				// (`orm.float` -> `orm__float`), so do not sanitize the member by itself.
				item.lit
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name && g.is_enum_type_name(tokens[i - 2].lit) {
				// An enum type prefix likewise makes keyword-named fields safe
				// (`TokenKind.float` -> `TokenKind__float`).
				item.lit
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' {
				item.lit
			} else if previous == .dot {
				fastc_c_identifier(item.lit)
			} else {
				g.resolved_expression_name(item.lit, previous)
			}
		} else if item.tok == .dot && map_length_receiver_type != '' {
			piece = if map_length_receiver_type.ends_with('*') { '->data->' } else { '.data->' }
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit in g.imports && tokens[i - 1].lit !in g.locals && (i < 2 || tokens[i - 2].tok != .dot) {
			// An imported module name qualifies only at the start of a chain; `v.pref.os.str()`
			// accesses the field `os`, not the `os` module, even when that module is imported. A
			// LOCAL of the same name (`for token in … { token.starts_with(…) }`) shadows the module.
			piece = '__'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit == 'C' {
			piece = ''
		} else if item.tok == .dot && !module_separator && i >= 3 && tokens[i - 2].tok == .dot {
			start := fastc_method_receiver_start(tokens, i)
			receiver := tokens[start..i]
			if receiver_type_source_id := g.infer_member_access_type(receiver, 0, receiver.len) {
				receiver_type := receiver_type_source_id

				piece = if fastc_is_pointer_type(g.underlying_alias_type(receiver_type)) {
					'->'
				} else {
					'.'
				}
			} else {
				piece = '.'
			}
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && g.local_is_pointer(tokens[i - 1].lit) {
			piece = '->'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit !in g.locals && g.is_enum_type_name(tokens[i - 1].lit) {
			piece = '__'
		} else if item.tok == .dot && module_separator {
			piece = '__'
		} else if piece == '' {
			piece = item.tok.str()
		}
		if previous_module_separator && item.tok == .name && i + 1 < tokens.len
			&& tokens[i + 1].tok == .lpar {
			if compact_name, prefix_len := g.compact_qualified_function_call(tokens, i) {
				result.go_back(prefix_len)
				piece = compact_name
			}
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator && !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
		previous_module_separator = module_separator
	}
	return g.render_enum_alias_member_references(tokens, fastc_take_string(mut result))
}
