module fastc

import v3.token

fn fastc_match_common_numeric_variant(variants []string) string {
	if variants.len < 2 {
		return ''
	}
	mut has_float := false
	for variant in variants {
		if variant != 'bool' && !fastc_is_numeric_expression_type(variant) {
			return ''
		}
		has_float = has_float || variant in ['f32', 'f64']
	}
	return if has_float { 'f64' } else { 'u64' }
}

fn fastc_match_multi_variant_value(subject string, access string, variants []string, common_type string) string {
	mut source := '((${common_type})0)'
	for i := variants.len - 1; i >= 0; i-- {
		variant := variants[i]
		source = '((${subject}${access}_typ == __v_typeid_${variant}) ? ((${common_type})(*(${variant} *)${subject}${access}_object)) : (${source}))'
	}
	return source
}

fn (mut g Parser) read_match_expression() !string {
	outer_expected_type := g.expected_expression_type
	g.expect(.key_match)!
	g.expected_expression_type = ''
	// `read_expression` consumes the `mut` modifier and keeps only the referenced
	// name in `last_expression`, so remember it before parsing the subject.
	subject_is_mut := g.tok == .key_mut
	subject := g.read_expression([token.Token.lcbr])!
	subject_type := g.last_expression_type
	if subject == '' || subject_type == '' {
		return g.unsupported('unverifiable match expression subject')
	}
	subject_tokens := g.last_expression.clone()
	smartcast_is_reference := subject_is_mut || subject_type.ends_with('*')
	// A sum-type / interface subject dispatches on the boxed `_typ` tag; each branch
	// names a variant type and (for a plain-local subject) is smart-cast inside its
	// value expression.
	is_boxed := g.is_boxed_type(fastc_normalize_inferred_type(subject_type))
	boxed_access := if subject_type.ends_with('*') { '->' } else { '.' }
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
		if path := fastc_member_chain_path(subject_tokens, member_start, subject_tokens.len) {
			subject_member_path = path
		}
	}
	g.expect(.lcbr)!
	temporary := g.temporary_name('match')
	mut conditions := []string{}
	mut values := []string{}
	mut result_type := ''
	mut fallback := ''
	mut has_else := false
	mut handled_cases := map[string]bool{}
	g.skip_semicolons()
	for g.tok != .rcbr && g.tok != .eof {
		mut is_else := false
		mut branch_conditions := []string{}
		mut branch_variant := ''
		mut branch_variants := []string{}
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
				branch_variant = variant_cname
				branch_variants << variant_cname
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
				g.expected_expression_type = subject_type
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
					branch_conditions << if g.underlying_alias_type(subject_type).trim_right('*') == 'string' {
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
		g.expected_expression_type = outer_expected_type
		// A single-type branch smart-casts to that variant. A branch listing several
		// array variants (`[]int, []string { value.len }`) cannot pick one concrete
		// element type, but every V array shares the generic `array` layout, so casting
		// to `array` still exposes the common fields (`len`, `cap`, `data`).
		common_numeric_type := fastc_match_common_numeric_variant(branch_variants)
		smartcast_type := if variant_count == 1 {
			branch_variant
		} else if variant_count > 1 && branch_all_arrays {
			'array'
		} else if common_numeric_type != '' {
			common_numeric_type
		} else if branch_variants.len > 0 {
			// Several struct variants in one arm (`SizeOf, IsRefType { x.is_type }`) may
			// only touch fields common to every variant, which V lays out compatibly, so
			// reading them through the first variant is correct.
			branch_variants[0]
		} else {
			''
		}
		multi_struct_smartcast := branch_variants.len > 1 && !branch_all_arrays && common_numeric_type == ''
		mut smartcast_saved := FastcLocal{}
		smartcast_active := is_boxed && !is_else && smartcast_type != '' && subject_local != '' && !multi_struct_smartcast
		if smartcast_active {
			smartcast_saved = g.locals[subject_local] or { FastcLocal{} }
			g.locals[subject_local] = FastcLocal{
				is_mut: smartcast_saved.is_mut
				is_reference: smartcast_is_reference
				typ: if smartcast_is_reference {
					smartcast_type + '*'
				} else {
					smartcast_type
				}
			}
		}
		projection_path := if subject_member_path != '' {
			subject_member_path
		} else if multi_struct_smartcast {
			subject_local
		} else {
			''
		}
		member_smartcast_active := is_boxed && !is_else && smartcast_type != '' && projection_path != ''
		mut member_smartcast_saved := FastcMemberSmartcast{}
		mut member_smartcast_present := false
		if member_smartcast_active {
			member_smartcast_present = projection_path in g.member_smartcasts
			member_smartcast_saved = g.member_smartcasts[projection_path] or {
				FastcMemberSmartcast{}
			}
			g.member_smartcasts[projection_path] = FastcMemberSmartcast{
				typ: smartcast_type + '*'
				source: '((${smartcast_type} *)${temporary}${boxed_access}_object)'
				variants: if multi_struct_smartcast { branch_variants.clone() } else { [] }
				tag_source: '${temporary}${boxed_access}_typ'
				object_source: '${temporary}${boxed_access}_object'
			}
		}
		mut value := g.read_match_block_expression_value()!
		mut value_type := g.last_expression_type
		if member_smartcast_active {
			if member_smartcast_present {
				g.member_smartcasts[projection_path] = member_smartcast_saved
			} else {
				g.member_smartcasts.delete(projection_path)
			}
		}
		if smartcast_active {
			g.locals[subject_local] = smartcast_saved
			smartcast_value := if smartcast_is_reference {
				'(${smartcast_type} *)${temporary}${boxed_access}_object'
			} else if common_numeric_type != '' {
				fastc_match_multi_variant_value(temporary, boxed_access, branch_variants, common_numeric_type)
			} else {
				'*(${smartcast_type} *)${temporary}${boxed_access}_object'
			}
			declaration_type := if smartcast_is_reference {
				smartcast_type + '*'
			} else {
				smartcast_type
			}
			value = '({ ${declaration_type} ${fastc_c_identifier(subject_local)} = ${smartcast_value}; (${value}); })'
		}
		if g.selfhost && value_type != '' && outer_expected_type != '' && g.should_box_variant(outer_expected_type, value_type) {
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
		if g.selfhost && value_type.trim_right('*') == 'IError' && g.return_type == 'Option' {
			error_result_type := if result_type != '' {
				result_type
			} else {
				outer_expected_type
			}
			if error_result_type != '' && error_result_type != 'Option' {
				value = '({ return (Option){.err=${value}, .state=1}; (${fastc_normalize_inferred_type(error_result_type)}){0}; })'
				value_type = error_result_type
			}
		}
		if g.selfhost && result_type == 'Option' && value_type !in ['', 'Option'] {
			value = fastc_option_success_expression(value_type, value)
			value_type = 'Option'
		} else if g.selfhost && value_type == 'Option' && result_type !in ['', 'Option'] {
			for i, previous_value in values {
				values[i] = fastc_option_success_expression(result_type, previous_value)
			}
			if fallback != '' {
				fallback = fastc_option_success_expression(result_type, fallback)
			}
			result_type = 'Option'
		}
		if g.selfhost && value_type in ['', 'void'] && result_type != '' {
			// A diverging arm (a `@[noreturn]` call such as `eprintln_exit(...)`) yields no
			// usable value; run it, then fall through to a zeroed result so the branch still
			// matches the ternary's result type.
			zero_type := fastc_normalize_inferred_type(result_type)
			value = '({ (void)(${value}); (${zero_type}){0}; })'
			value_type = result_type
		}
		if result_type == '' && value_type !in ['', 'void'] {
			if g.selfhost {
				zero_type := fastc_normalize_inferred_type(value_type)
				for i, previous_value in values {
					values[i] = '({ (void)(${previous_value}); (${zero_type}){0}; })'
				}
				if fallback != '' {
					fallback = '({ (void)(${fallback}); (${zero_type}){0}; })'
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
	g.expected_expression_type = outer_expected_type
	if !has_else && !g.selfhost {
		return g.unsupported('non-exhaustive match expression without `else`')
	}
	if !has_else {
		fallback = '(${result_type}){0}'
	}
	mut expression := fallback
	for i := conditions.len - 1; i >= 0; i-- {
		expression = '((${conditions[i]}) ? (${values[i]}) : (${expression}))'
	}
	g.last_expression_type = if result_type == '' { outer_expected_type } else { result_type }
	g.last_expression = []FastcExpressionToken{}
	return '({ __typeof__((${subject})) ${temporary} = (${subject}); ${expression}; })'
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
	mut value_types := []string{}
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
	g.last_expression_type = 'MultiReturn'
	g.last_expression = []FastcExpressionToken{}
	g.last_multi_return_types = value_types.clone()
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
		g.last_expression_type = 'MultiReturn'
		g.last_expression = []FastcExpressionToken{}
		g.last_multi_return_types = value_types.clone()
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
		g.last_expression_type = ''
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
		g.last_expression_type = 'MultiReturn'
		g.last_expression = []FastcExpressionToken{}
		g.last_multi_return_types = value_types.clone()
		return '({ ${statements.join(' ')} ${fastc_multi_return_literal(packed_values)}; })'
	}
	if g.tok == .name {
		prefix := g.lit
		g.next()
		final_value := g.read_expression_with_prefix(prefix, [token.Token.semicolon,
			token.Token.rcbr])!
		value = if value.trim_space() in ['', ';'] {
			final_value
		} else {
			value + final_value
		}
	}
	return '({ ${statements.join(' ')} ${value}; })'
}
