module fastc

import v.flat
import v.token

fn fastc_match_common_numeric_variant(variants []u32) u32 {
	if variants.len < 2 { return u32(0) }
	mut has_float := false
	for variant in variants {
		if variant != flat.source_type_bool && !fastc_is_numeric_expression_type(variant) {
			return u32(0)
		}
		has_float = has_float || variant in [flat.source_type_f32, flat.source_type_f64]
	}
	return if has_float { flat.source_type_f64 } else { flat.source_type_u64 }
}

fn fastc_match_multi_variant_value(subject string, access string, variants []u32, common_type u32) string {
	common_name := fastc_output_c_type(flat.source_type_name(common_type))
	mut source := '((${common_name})0)'
	for i := variants.len - 1; i >= 0; i-- {
		variant := flat.source_type_name(variants[i])
		variant_c_type := fastc_output_c_type(variant)
		source = '((${subject}${access}_typ == __v_typeid_${variant}) ? ((${common_name})(*(${variant_c_type} *)${subject}${access}_object)) : (${source}))'
	}
	return source
}

fn (mut g Parser) read_match_expression() !string {
	outer_expected_type_source_id := g.expected_expression_type
	outer_expected_type := outer_expected_type_source_id
	g.expect(.key_match)!
	g.expected_expression_type = u32(0)
	// `read_expression` consumes the `mut` modifier and keeps only the referenced
	// name in `last_expression`, so remember it before parsing the subject.
	subject_is_mut := g.tok == .key_mut
	mut subject := g.read_expression([token.Token.lcbr])!
	subject_wrapper := g.last_expression_type
	subject_payload := if subject_wrapper in [fastc_type_option, fastc_type_result] {
		g.option_value_type_for_expression(g.last_expression)
	} else {
		u32(0)
	}
	subject_type_source_id := if subject_payload != 0 { subject_payload } else { subject_wrapper }
	subject_type := subject_type_source_id
	if subject == '' || subject_type_source_id == u32(0) {
		return g.unsupported('unverifiable match expression subject')
	}
	subject_tokens := g.last_expression.clone()
	if g.member_smartcasts.len > 0 {
		if path := fastc_indexed_member_chain_path(subject_tokens) {
			if cast := g.member_smartcasts[path] { subject = cast.source }
		}
	}
	smartcast_is_reference := subject_is_mut || flat.source_type_query(subject_type).has(.c_pointer_suffix)
	// A sum-type / interface subject dispatches on the boxed `_typ` tag; each branch
	// names a variant type and (for a plain-local subject) is smart-cast inside its
	// value expression.
	is_boxed := g.is_boxed_type(fastc_normalize_inferred_type(subject_type_source_id))
	boxed_access := if flat.source_type_query(subject_type).has(.c_pointer_suffix) {
		'->'
	} else {
		'.'
	}
	mut subject_local := ''
	if is_boxed && subject_tokens.len == 1 && subject_tokens[0].tok == .name && subject_tokens[0].lit in g.locals {
		subject_local = subject_tokens[0].lit
	} else if is_boxed && subject_tokens.len == 2 && subject_tokens[0].tok in [
		.key_mut,
		.amp,
	] && subject_tokens[1].tok == .name && subject_tokens[1].lit in g.locals {
		subject_local = subject_tokens[1].lit
	}
	// A boxed member subject (`match sym.info { ast.Struct { sym.info.name } }`, or
	// `match mut sym.info { … }`) cannot be shadowed like a plain local, so each branch
	// narrows the member path through the same branch-scoped rewrite the `if x.f is T`
	// smart-casts use. A leading `mut`/`&` renders away, so skip it before the chain scan.
	member_start := if subject_tokens.len > 0 && subject_tokens[0].tok in [.key_mut, .amp] {
		1
	} else {
		0
	}
	mut subject_member_path := ''
	if is_boxed && subject_local == '' {
		if path := fastc_indexed_member_chain_path(subject_tokens[member_start..]) {
			subject_member_path = path
		}
	}

	// The boxed member itself, which an assignment to the narrowed member writes.
	mut subject_member_source := ''
	mut subject_member_type := u32(0)
	if subject_member_path.contains('.') {
		subject_member_source = g.render_member_receiver(subject_tokens[member_start..]) or { '' }
		subject_member_type = fastc_normalize_inferred_type(g.infer_expression_type(subject_tokens[member_start..]) or {
			u32(0)
		})
	}
	g.expect(.lcbr)!
	temporary := g.temporary_name('match')
	mut conditions := []string{}
	mut values := []string{}
	mut result_type := u32(0)
	mut result_option_type := u32(0)
	mut fallback := ''
	mut has_else := false
	mut handled_cases := map[string]bool{}
	g.skip_semicolons()
	for g.tok != .rcbr && g.tok != .eof {
		mut is_else := false
		mut branch_conditions := []string{}
		mut branch_variant := u32(0)
		mut branch_variants := []u32{}
		mut variant_count := 0
		mut branch_all_arrays := true
		if g.tok == .key_else {
			is_else = true
			g.next()
		} else if is_boxed {
			for {
				variant_key := g.read_match_type_key() or {
					return g.unsupported('match expression type value')
				}
				variant_cname := fastc_c_declared_type_name(variant_key)
				if variant_cname in handled_cases {
					return g.unsupported('duplicate match case `${variant_cname}`')
				}
				handled_cases[variant_cname] = true
				branch_conditions << '(${temporary}${boxed_access}_typ == __v_typeid_${variant_cname})'
				branch_variant = flat.source_type_id(variant_cname)
				branch_variants << branch_variant
				if !variant_cname.starts_with('Array_') {
					branch_all_arrays = false
				}
				variant_count++
				if g.tok != .comma {
					break
				}
				g.next()
			}
		} else {
			for {
				g.expected_expression_type = subject_type_source_id
				start := g.read_expression([token.Token.comma, token.Token.lcbr, token.Token.dotdot,
					token.Token.ellipsis])!
				start_tokens := g.last_expression.clone()
				mut case_key := g.normalized_match_case_key(start_tokens, start)
				mut case_source := start
				// A match range case uses `...` (inclusive, `.ellipsis`); `..` (`.dotdot`) is
				// also accepted defensively.
				if g.tok in [token.Token.dotdot, token.Token.ellipsis] {
					g.next()
					finish := g.read_expression([token.Token.comma, token.Token.lcbr])!
					finish_tokens := g.last_expression.clone()
					case_key = 'range:${case_key}..${g.normalized_match_case_key(finish_tokens, finish)}'
					case_source = '${start}..${finish}'
					branch_conditions << '((${temporary}) >= (${start}) && (${temporary}) <= (${finish}))'
				} else {
					branch_conditions << if fastc_trim_pointer_type(g.underlying_alias_type(subject_type_source_id)) == flat.source_type_string {
						'builtin__string_eq(${temporary}, ${start})'
					} else {
						'((${temporary}) == (${start}))'
					}
				}
				if case_key in handled_cases {
					return g.unsupported('duplicate match case `${case_source}`')
				}
				handled_cases[case_key] = true
				if g.tok != .comma {
					break
				}
				g.next()
			}
		}
		g.expect(.lcbr)!
		g.expected_expression_type = if result_type != 0 && outer_expected_type_source_id in [
			u32(0),
			fastc_type_option,
			fastc_type_result,
			flat.source_type_voidptr,
		] {
			result_type
		} else {
			outer_expected_type_source_id
		}
		// A single-type branch smart-casts to that variant. A branch listing several
		// array variants (`[]int, []string { value.len }`) cannot pick one concrete
		// element type, but every V array shares the generic `array` layout, so casting
		// to `array` still exposes the common fields (`len`, `cap`, `data`).
		common_numeric_type := fastc_match_common_numeric_variant(branch_variants)
		smartcast_type := if variant_count == 1 {
			branch_variant
		} else if variant_count > 1 && branch_all_arrays {
			fastc_type_array
		} else if common_numeric_type != 0 {
			common_numeric_type
		} else if branch_variants.len > 0 {
			// Several struct variants in one arm (`SizeOf, IsRefType { x.is_type }`) may
			// only touch fields common to every variant, which V lays out compatibly, so
			// reading them through the first variant is correct.
			branch_variants[0]
		} else {
			u32(0)
		}
		multi_struct_smartcast := branch_variants.len > 1 && !branch_all_arrays && common_numeric_type == 0
		mut smartcast_saved := FastcLocal{}
		smartcast_active := is_boxed && !is_else && smartcast_type != 0 && subject_local != '' && !multi_struct_smartcast
		if smartcast_active {
			smartcast_saved = g.locals[subject_local] or { FastcLocal{} }
			g.locals[subject_local] = FastcLocal{
				is_mut:       smartcast_saved.is_mut
				is_reference: smartcast_is_reference
				typ:          (if smartcast_is_reference {
					flat.source_type_composition_two(.concat, smartcast_type, flat.source_type_id('*'))
				} else {
					smartcast_type
				})
			}
		}
		projection_path := if subject_member_path != '' {
			subject_member_path
		} else if multi_struct_smartcast {
			subject_local
		} else {
			''
		}
		member_smartcast_active := is_boxed && !is_else && smartcast_type != 0 && projection_path != ''
		mut member_smartcast_saved := FastcMemberSmartcast{}
		mut member_smartcast_present := false
		if member_smartcast_active {
			member_smartcast_present = projection_path in g.member_smartcasts
			member_smartcast_saved = g.member_smartcasts[projection_path] or {
				FastcMemberSmartcast{}
			}
			pointer := '((${fastc_output_c_type(flat.source_type_name(smartcast_type))} *)${temporary}${boxed_access}_object)'
			mut view_type, mut view_source := g.narrowed_member_view(smartcast_type, u32(0),
				pointer)
			if common_numeric_type != 0 {
				// Numeric variants of different widths are read as their common type.
				view_type = common_numeric_type
				view_source = fastc_match_multi_variant_value(temporary, boxed_access,
					branch_variants, common_numeric_type)
			}
			g.member_smartcasts[projection_path] = FastcMemberSmartcast{
				typ:            view_type
				source:         view_source
				variants:       if multi_struct_smartcast {
					flat.TypeTextList{ ids: branch_variants.clone() }
				} else {
					flat.TypeTextList{}
				}
				tag_source:     '${temporary}${boxed_access}_typ'
				object_source:  '${temporary}${boxed_access}_object'
				subject_source: subject_member_source
				subject_type:   subject_member_type
			}
		}
		mut value := g.read_match_block_expression_value()!
		mut value_type := fastc_normalize_inferred_type(g.last_expression_type)
		if member_smartcast_active && !fastc_is_pointer_type(smartcast_type)
			&& fastc_is_pointer_type(value_type) && fastc_trim_pointer_type(value_type) == smartcast_type
			&& !fastc_is_pointer_type(outer_expected_type_source_id) {
			if path := fastc_indexed_member_chain_path(g.last_expression) {
				if path == projection_path {
					value = '*(${value})'
					value_type = smartcast_type
				}
			}
		}
		if value_type == 0 && g.declared_kinds[g.semantic_type_key(outer_expected_type_source_id)] == .enum_ {
			value_type = outer_expected_type_source_id
		}
		if value_type in [fastc_type_option, fastc_type_result] {
			payload := if g.last_expression.len > 0 {
				g.option_value_type_for_expression(g.last_expression)
			} else {
				g.last_option_value_type
			}
			if payload != 0 { result_option_type = payload }
		}
		if member_smartcast_active {
			if member_smartcast_present && projection_path in g.member_smartcasts {
				g.member_smartcasts[projection_path] = member_smartcast_saved
			} else {
				g.member_smartcasts.delete(projection_path)
			}
		}
		if smartcast_active {
			g.locals[subject_local] = smartcast_saved
			// The payload is boxed as its C output type (a V `int` is 64 bits wide).
			smartcast_c_type := fastc_output_c_type(flat.source_type_name(smartcast_type))
			smartcast_value := if smartcast_is_reference {
				'(${smartcast_c_type} *)${temporary}${boxed_access}_object'
			} else if common_numeric_type != 0 {
				fastc_match_multi_variant_value(temporary, boxed_access, branch_variants, common_numeric_type)
			} else {
				'*(${smartcast_c_type} *)${temporary}${boxed_access}_object'
			}
			declaration_type := if smartcast_is_reference {
				smartcast_c_type + '*'
			} else {
				smartcast_c_type
			}
			value = '({ ${declaration_type} ${fastc_c_identifier(subject_local)} = ${smartcast_value}; (${value}); })'
		}
		if g.selfhost && value_type != u32(0) && outer_expected_type_source_id != u32(0) && g.should_box_variant(outer_expected_type, value_type) {
			// A branch value whose type is a variant of the match's (boxed) expected
			// type must be boxed, so every branch shares the ternary's result type
			// (e.g. a `[]Primitive` branch returning the smart-cast array as a `Primitive`).
			value = g.interface_value_expression(outer_expected_type, value_type, value)
			value_type = outer_expected_type
		}
		g.skip_semicolons()
		if g.tok != .rcbr {
			return g.unsupported('match branch `${value}` left `${g.token_source()}` (`${g.tok.str()}`)')
		}
		g.expect(.rcbr)!
		g.skip_semicolons()
		if g.selfhost && fastc_trim_pointer_type(value_type) == fastc_type_ierror && g.return_type == fastc_type_result {
			error_result_type := (if result_type != 0 { result_type } else { outer_expected_type })
			if error_result_type != 0 && error_result_type !in [fastc_type_option, fastc_type_result] {
				value = '({ return (__v_result){.err=${value}, .state=1}; (${flat.source_type_name(fastc_normalize_inferred_type(error_result_type))}){0}; })'
				value_type = error_result_type
			}
		}
		if g.selfhost && result_type in [fastc_type_option, fastc_type_result] && value_type !in [
			u32(0),
			fastc_type_option,
			fastc_type_result,
		] {
			value = g.option_success_expression(result_type, value_type, value)
			value_type = result_type
		} else if g.selfhost && value_type in [fastc_type_option, fastc_type_result] && result_type !in [
			u32(0),
			fastc_type_option,
			fastc_type_result,
		] {
			for i, previous_value in values {
				values[i] = g.option_success_expression(value_type, result_type, previous_value)
			}
			if fallback != '' {
				fallback = g.option_success_expression(value_type, result_type, fallback)
			}
			result_option_type = result_type
			result_type = value_type
		}
		if g.selfhost && value_type in [u32(0), flat.source_type_void] && result_type != 0 {
			// A diverging arm (a `@[noreturn]` call such as `eprintln_exit(...)`) yields no
			// usable value; run it, then fall through to a zeroed result so the branch still
			// matches the ternary's result type.
			zero_type := fastc_normalize_inferred_type(result_type)
			value = '({ (void)(${value}); (${flat.source_type_name(zero_type)}){0}; })'
			value_type = result_type
		}
		if result_type == 0 && value_type !in [u32(0), flat.source_type_void] {
			if g.selfhost {
				zero_type := fastc_normalize_inferred_type(value_type)
				for i, previous_value in values {
					values[i] = '({ (void)(${previous_value}); (${flat.source_type_name(zero_type)}){0}; })'
				}
				if fallback != '' {
					fallback = '({ (void)(${fallback}); (${flat.source_type_name(zero_type)}){0}; })'
				}
			}
			result_type = value_type
		}
		if is_else {
			fallback = value
			has_else = true
		} else {
			conditions << '(${branch_conditions.join(' || ')})'
			values << value
		}
	}
	g.expect(.rcbr)!
	g.expected_expression_type = outer_expected_type_source_id
	if !has_else && !g.selfhost {
		return g.unsupported('non-exhaustive match expression without `else`')
	}
	if !has_else {
		fallback = '(${flat.source_type_name(result_type)}){0}'
	}
	mut expression := fallback
	if conditions.len > 16 {
		// Flat branches keep large enum matches within native C parsers' stack limits.
		// A label preserves any break/continue in an arm's enclosing V loop.
		result_name := g.temporary_name('match_result')
		done := g.temporary_name('match_done')
		storage_type := if result_type in [u32(0), flat.source_type_void] {
			flat.source_type_int
		} else {
			result_type
		}
		mut statements := ['${fastc_output_c_type(flat.source_type_name(storage_type))} ${result_name};']
		for i, condition in conditions {
			statements << 'if (${condition}) { ${result_name} = (${values[i]}); goto ${done}; }'
		}
		statements << '${result_name} = (${fallback}); ${done}: ${result_name};'
		expression = '({ ${statements.join(' ')} })'
	} else {
		for i := conditions.len - 1; i >= 0; i-- {
			expression = '((${conditions[i]}) ? (${values[i]}) : (${expression}))'
		}
	}
	g.last_expression_type = (if result_type == 0 {
		outer_expected_type_source_id
	} else {
		result_type
	})
	g.last_expression = []FastcExpressionToken{}
	g.last_option_value_type = result_option_type
	subject_decl_type := g.declaration_c_type(subject_type, subject)
	if subject_payload != 0 {
		if result_type !in [fastc_type_option, fastc_type_result] {
			result_option_type = result_type
			expression = g.option_success_expression(subject_wrapper, result_type, expression)
		}
		g.last_expression_type = subject_wrapper
		g.last_option_value_type = result_option_type
		return '({ ${flat.source_type_name(subject_wrapper)} __vf_match_option = (${subject}); __vf_match_option.state ? (${flat.source_type_name(subject_wrapper)}){.state=2} : ({ ${subject_decl_type} ${temporary} = ${g.option_value_expression(subject_payload, '__vf_match_option')}; ${expression}; }); })'
	}
	return '({ ${subject_decl_type} ${temporary} = (${subject}); ${expression}; })'
}

fn (g &Parser) normalized_match_case_key(tokens []FastcExpressionToken, rendered string) string {
	if value := fastc_integer_literal_value(tokens) {
		return 'integer:${value}'
	}
	mut key := rendered.trim_space()
	mut seen := map[string]bool{}
	for key !in seen {
		seen[key] = true
		key = g.constant_values[key] or { break }
		key = key.trim_space()
	}
	if value := fastc_decimal_integer_value(key) {
		return 'integer:${value}'
	}
	return key
}

fn (mut g Parser) read_match_block_expression_value() !string {
	if g.tok == .dollar && g.dollar_keyword_is('if') {
		// A `$if … { value } $else { value }` arm is a compile-time expression: select
		// the taken branch's value rather than executing it as a statement.
		return g.read_comptime_if_expression()!
	}
	if g.tok == .key_if || g.or_block_has_statements() {
		return g.read_block_expression_value()!
	}
	mut values := []string{}
	mut value_types := []u32{}
	for {
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		values << value
		value_types << fastc_normalize_inferred_type(g.last_expression_type)
		if g.tok != .comma {
			break
		}
		g.next()
	}
	if values.len == 1 {
		return values[0]
	}
	mut packed_values := []string{cap: values.len}
	for value in values {
		packed_values << 'V_FASTC_MULTI_VALUE((${value}))'
	}
	g.last_expression_type = flat.source_type_id('MultiReturn')
	g.last_expression = []FastcExpressionToken{}
	g.last_multi_return_types = flat.TypeTextList{ ids: value_types.clone() }
	return fastc_multi_return_literal(packed_values)
}

fn (mut g Parser) read_block_expression_value() !string {
	g.skip_semicolons()
	if g.tok == .dollar && g.dollar_keyword_is('if') {
		// `$if … { value } $else { value }` as the block's value: select the taken
		// branch's value rather than executing it as a statement.
		return g.read_comptime_if_expression()!
	}
	if g.tok == .key_if && g.if_starts_final_block_expression() {
		return g.read_if_expression()!
	}
	if !g.or_block_has_statements() {
		first := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		if g.tok != .comma {
			return first
		}
		mut values := [first]
		mut value_types := [fastc_normalize_inferred_type(g.last_expression_type)]
		for g.tok == .comma {
			g.next()
			values << g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			value_types << fastc_normalize_inferred_type(g.last_expression_type)
		}
		mut packed_values := []string{cap: values.len}
		for value in values {
			packed_values << 'V_FASTC_MULTI_VALUE((${value}))'
		}
		g.last_expression_type = flat.source_type_id('MultiReturn')
		g.last_expression = []FastcExpressionToken{}
		g.last_multi_return_types = flat.TypeTextList{ ids: value_types.clone() }
		return fastc_multi_return_literal(packed_values)
	}
	previous_capture := g.capturing_defer
	previous_lines := g.captured_defer_lines.clone()
	g.capturing_defer = true
	g.captured_defer_lines = []string{}
	// Restore through a defer so a `parse_statement` error that a speculative caller
	// swallows cannot leave `capturing_defer` set for the rest of the function.
	defer {
		g.capturing_defer = previous_capture
		g.captured_defer_lines = previous_lines.clone()
	}
	for g.or_block_has_statements() {
		if g.tok == .key_if && g.if_starts_final_block_expression() {
			break
		}
		if g.tok == .key_match && g.match_starts_final_block_expression() {
			break
		}
		_ = g.parse_statement()!
		g.skip_semicolons()
	}
	statements := g.captured_defer_lines.clone()
	if g.tok == .rcbr {
		g.last_expression_type = u32(0)
		g.last_expression = []FastcExpressionToken{}
		return '({ ${statements.join(' ')} 0; })'
	}
	g.skip_semicolons()
	mut value := if g.tok == .name {
		prefix := g.lit
		g.next()
		g.read_expression_with_prefix(prefix, [token.Token.comma, token.Token.semicolon,
			token.Token.rcbr])!
	} else {
		g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
	}
	if g.tok == .comma {
		// A multi-return block value (`{ c := ...; c, '.zst', 'zstd' }`, e.g. veb's
		// compressed-static match): pack the comma-separated values into a MultiReturn,
		// mirroring the single-line branch path above.
		mut values := [value]
		mut value_types := [fastc_normalize_inferred_type(g.last_expression_type)]
		for g.tok == .comma {
			g.next()
			part := if g.tok == .name {
				prefix := g.lit
				g.next()
				g.read_expression_with_prefix(prefix, [token.Token.comma, token.Token.semicolon,
					token.Token.rcbr])!
			} else {
				g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			}
			values << part
			value_types << fastc_normalize_inferred_type(g.last_expression_type)
		}
		mut packed_values := []string{cap: values.len}
		for packed_value in values {
			packed_values << 'V_FASTC_MULTI_VALUE((${packed_value}))'
		}
		g.last_expression_type = flat.source_type_id('MultiReturn')
		g.last_expression = []FastcExpressionToken{}
		g.last_multi_return_types = flat.TypeTextList{ ids: value_types.clone() }
		return '({ ${statements.join(' ')} ${fastc_multi_return_literal(packed_values)}; })'
	}
	if g.tok == .name {
		prefix := g.lit
		g.next()
		final_value := g.read_expression_with_prefix(prefix, [token.Token.semicolon, token.Token.rcbr])!
		value = if value.trim_space() in ['', ';'] {
			final_value
		} else {
			value + final_value
		}
	}
	return '({ ${statements.join(' ')} ${value}; })'
}
