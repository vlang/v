module fastc

import strings
import v3.token

fn (mut g Parser) read_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix_mode('', stops, false, false)
}

fn (mut g Parser) read_expression_with_prefix(prefix string, stops []token.Token) !string {
	return g.read_expression_with_prefix_mode(prefix, stops, false, false)
}

fn (mut g Parser) read_condition_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix_mode('', stops, false, true)
}

fn (mut g Parser) read_statement_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix_mode('', stops, true, false)
}

fn (mut g Parser) read_statement_expression_with_prefix(prefix string, stops []token.Token) !string {
	return g.read_expression_with_prefix_mode(prefix, stops, true, false)
}

fn (mut g Parser) read_expression_with_prefix_mode(prefix string, stops []token.Token, allow_mutation_statement bool, allow_declaration_guard bool) !string {
	g.expression_depth++
	if g.expression_depth == 1 && g.comparison_memo.len > 0 {
		// Memoized comparison renders are only valid while the expression's
		// token buffers and locals are live and unchanged; a new top-level
		// expression starts a fresh generation.
		g.comparison_memo.clear()
	}
	defer {
		g.expression_depth--
	}
	return g.read_expression_with_prefix_mode_impl(prefix, stops, allow_mutation_statement,
		allow_declaration_guard)
}

fn (mut g Parser) read_expression_with_prefix_mode_impl(prefix string, stops []token.Token, allow_mutation_statement bool, allow_declaration_guard bool) !string {
	if g.selfhost && prefix == '' && g.tok == .lcbr && token.Token.lcbr !in stops {
		return g.read_inferred_map_literal()!
	}
	if prefix == '' && g.tok == .key_if {
		return g.read_if_expression()!
	}
	if prefix == '' && g.tok == .key_match {
		return g.read_match_expression()!
	}
	if prefix == '' && g.tok == .dollar {
		return g.read_comptime_if_expression()!
	}
	mut result := strings.new_builder(64)
	mut expression_tokens := []FastcExpressionToken{}
	if prefix.len > 0 {
		result.write_string(g.resolved_expression_name(prefix, .unknown))
		expression_tokens << FastcExpressionToken{
			tok:          .name
			lit:          prefix
			unsafe_depth: g.unsafe_depth
		}
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	mut brace_depth := 0
	mut cast_depths := []int{}
	mut pointer_cast_depths := []int{}
	mut previous_was_pointer_cast := false
	mut has_sum_arithmetic_operator := false
	mut has_multiply_operator := false
	mut has_and_operator := false
	mut has_pipe_operator := false
	mut has_xor_operator := false
	mut previous_token := if prefix.len > 0 { token.Token.name } else { token.Token.unknown }
	mut previous_lit := prefix
	mut previous_token_end := g.s.pos
	mut previous_module_separator := false
	mut unsafe_expression_depth := 0
	mut struct_types := []string{}
	mut struct_depths := []int{}
	mut struct_paren_depths := []int{}
	mut expected_struct_field_type := ''
	mut enum_shorthand_type := ''
	mut next_token_is_mut_argument := false
	mut source_token_count := if prefix == '' { 0 } else { 1 }
	mut mutation_operator := token.Token.unknown
	mut tokens_before_mutation := 0
	for g.tok != .eof {
		if g.selfhost && g.tok == .semicolon && g.semicolon_continues_expression() {
			g.next()
			continue
		}
		if g.selfhost && expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0
			&& brace_depth == 0 && unsafe_expression_depth == 0 && g.tok == .mul
			&& g.s.src[previous_token_end..g.s.pos].contains('\n') {
			mut lookahead := g.s
			if lookahead.scan() == .name && lookahead.scan().is_assignment() {
				break
			}
		}
		if expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0
			&& unsafe_expression_depth == 0 && previous_token in [.inc, .dec]
			&& g.s.src[previous_token_end..g.s.pos].contains('\n') {
			break
		}
		if g.tok in [.key_if, .key_unsafe] && expression_tokens.len > 0 && paren_depth == 0
			&& bracket_depth == 0 && brace_depth == 0 && unsafe_expression_depth == 0
			&& g.s.src[previous_token_end..g.s.pos].contains('\n') {
			break
		}
		if g.tok == .key_unsafe {
			g.next()
			if g.tok != .lcbr {
				return g.unsupported('unsafe expression without a block')
			}
			unsafe_expression_depth++
			g.unsafe_depth += 1
			g.next()
			continue
		}
		if unsafe_expression_depth > 0 && g.tok == .rcbr {
			unsafe_expression_depth--
			g.unsafe_depth -= 1
			g.next()
			if unsafe_expression_depth == 0 {
				continue
			}
		}
		if unsafe_expression_depth > 0 && g.tok == .semicolon {
			g.next()
			continue
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && g.tok in stops {
			break
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && g.tok == .comma {
			// V's top-level commas form simultaneous multi-target assignments.
			// Copying them to C would instead emit comma operators.
			return g.unsupported('parallel assignments')
		}
		if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
			literal := g.lit
			previous_expected_type := g.expected_expression_type
			if expected_struct_field_type != '' {
				g.expected_expression_type = expected_struct_field_type
			}
			interpolation := g.read_interpolated_string()!
			g.expected_expression_type = previous_expected_type
			if result.len > 0 && fastc_needs_space(result.last(), interpolation)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(interpolation)
			expression_tokens << FastcExpressionToken{
				tok:    .string
				lit:    literal
				source: interpolation
			}
			previous_token = .string
			previous_lit = literal
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .lcbr && expression_tokens.len >= 2
			&& expression_tokens[0].tok == .name && expression_tokens[0].lit == 'chan' {
			g.skip_balanced(.lcbr, .rcbr)!
			result.go_back(result.len)
			result.write_string('(chan){0}')
			expression_tokens = [
				FastcExpressionToken{
					tok: .name
					lit: '(chan){0}'
					typ: 'chan'
				},
			]
			previous_token = .name
			previous_lit = '(chan){0}'
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.tok in [.key_mut, .key_shared] {
			g.next()
			if g.tok in [.amp, .and] {
				next_token_is_mut_argument = true
				continue
			}
			if g.tok == .name && g.local_is_pointer(g.lit) {
				mut next_offset := g.s.offset
				for next_offset < g.s.src.len && g.s.src[next_offset].is_space() {
					next_offset++
				}
				if next_offset >= g.s.src.len || g.s.src[next_offset] !in [`.`, `[`] {
					next_token_is_mut_argument = true
					continue
				}
			}
			result.write_u8(`&`)
			expression_tokens << FastcExpressionToken{
				tok:             .amp
				lit:             '&'
				is_mut_argument: true
			}
			previous_token = .amp
			previous_lit = '&'
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_if {
			previous_expected_type := g.expected_expression_type
			if expected_struct_field_type != '' {
				g.expected_expression_type = expected_struct_field_type
			}
			conditional := g.read_if_expression()!
			conditional_type := g.last_expression_type
			g.expected_expression_type = previous_expected_type
			if result.len > 0 && fastc_needs_space(result.last(), conditional)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(conditional)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: conditional
				typ: conditional_type
			}
			previous_token = .name
			previous_lit = conditional
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_match {
			matched := g.read_match_expression()!
			matched_type := g.last_expression_type
			if result.len > 0 && fastc_needs_space(result.last(), matched)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(matched)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: matched
				typ: matched_type
			}
			previous_token = .name
			previous_lit = matched
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_or {
			or_expression_is_statement := g.expression_tokens_are_statement(expression_tokens)
			or_return_types := g.multi_return_types_for_expression(expression_tokens)
			mut wrapper_parens := 0
			for wrapper_parens < expression_tokens.len
				&& expression_tokens[wrapper_parens].tok == .lpar {
				wrapper_parens++
			}
			mut option_expression := result.str().trim_space()
			mut value_type := g.expected_expression_type
			mut option_tokens := expression_tokens.clone()
			mut assignment_prefix := ''
			mut assignment_depth := 0
			for i, assignment_token in expression_tokens {
				if assignment_token.tok in [.lpar, .lsbr, .lcbr] {
					assignment_depth++
				} else if assignment_token.tok in [.rpar, .rsbr, .rcbr] {
					assignment_depth--
				} else if assignment_depth == 0 && assignment_token.tok.is_assignment() && i > 0
					&& i + 1 < expression_tokens.len {
					left_tokens := expression_tokens[..i].clone()
					left_type := g.infer_expression_type(left_tokens) or { '' }
					if left_type != '' {
						left_source := g.render_membership_candidate(left_tokens, left_type) or {
							''
						}
						if left_source != '' {
							assignment_prefix = '${left_source}${assignment_token.tok.str()}'
							value_type = left_type
							option_tokens = expression_tokens[i + 1..].clone()
							option_expression = g.render_call_argument_expression(option_tokens,
								left_type) or { '' }
							wrapper_parens = 0
						}
					}
					break
				}
			}
			if expression_tokens.len >= 3 && expression_tokens[0].tok == .name
				&& expression_tokens[1].tok == .lpar
				&& fastc_primitive_c_type(expression_tokens[0].lit) != none {
				option_tokens = expression_tokens[2..].clone()
			}
			mut option_value_type := g.option_value_type_for_expression(option_tokens)
			if map_lookup := g.render_map_lookup_option_expression(option_tokens) {
				option_expression = map_lookup.source
				option_value_type = map_lookup.typ
			} else if array_lookup := g.render_array_lookup_option_expression(option_tokens) {
				option_expression = array_lookup.source
				option_value_type = array_lookup.typ
			} else if method_call := g.render_method_call_expression(option_tokens,
				option_expression)
			{
				option_expression = method_call.source
				option_value_type = g.option_value_type_for_expression(option_tokens)
			} else if call := g.render_missing_call_arguments(option_tokens) {
				option_expression = call.source
				option_value_type = g.option_value_type_for_expression(option_tokens)
			}
			outer_cast := assignment_prefix == '' && option_tokens.len != expression_tokens.len
			if expression_tokens.len >= 2 && expression_tokens[0].tok == .name
				&& expression_tokens[1].tok == .lpar {
				value_type = fastc_primitive_c_type(expression_tokens[0].lit) or { value_type }
				cast_prefix := '((${value_type})('
				if option_expression.starts_with(cast_prefix) {
					option_expression = option_expression[cast_prefix.len..]
				}
			}
			g.next()
			g.expect(.lcbr)!
			temporary := g.temporary_name('option')
			if g.or_block_has_statements() {
				previous_capture := g.capturing_defer
				previous_lines := g.captured_defer_lines.clone()
				previous_err := g.locals['err'] or { FastcLocal{} }
				had_err := 'err' in g.locals
				g.locals['err'] = FastcLocal{
					typ: 'IError'
				}
				g.capturing_defer = true
				g.captured_defer_lines = []string{}
				_ = g.parse_block_body()!
				block_lines := g.captured_defer_lines.clone()
				g.capturing_defer = previous_capture
				g.captured_defer_lines = previous_lines.clone()
				if had_err {
					g.locals['err'] = previous_err
				} else {
					g.locals.delete('err')
				}
				complex_value_type := if option_value_type == '' {
					'void'
				} else {
					option_value_type
				}
				complex_success := if complex_value_type == 'void' {
					'0'
				} else {
					'*((${complex_value_type} *)${temporary}.data)'
				}
				result.go_back(result.len)
				result.write_string('${assignment_prefix}({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { IError err = ${temporary}.err; ${block_lines.join(' ')} } ${complex_success}; })')
				expression_tokens = [
					FastcExpressionToken{
						tok:          .name
						lit:          temporary
						typ:          complex_value_type
						is_statement: or_expression_is_statement
					},
				]
				if complex_value_type == 'void' {
					expression_tokens << FastcExpressionToken{
						tok: .assign
						lit: '='
					}
				}
				if assignment_prefix != '' {
					expression_tokens << FastcExpressionToken{
						tok: .assign
						lit: '='
					}
				}
				previous_token = .name
				previous_lit = temporary
				previous_module_separator = false
				previous_token_end = g.s.pos
				g.last_expression_type = complex_value_type
				g.last_expression = expression_tokens
				g.last_multi_return_types = or_return_types.clone()
				return result.str().trim_space()
			}
			previous_err := g.locals['err'] or { FastcLocal{} }
			had_err := 'err' in g.locals
			g.locals['err'] = FastcLocal{
				typ: 'IError'
			}
			mut fallback := g.read_expression([token.Token.rcbr])!
			fallback_type := fastc_normalize_inferred_type(g.last_expression_type)
			if fallback == '' {
				fallback = '0'
			} else if fallback_type.starts_with('Map_') && fallback.contains('){}') {
				key_type, map_value_type := fastc_map_key_value_types(fallback_type) or {
					return g.unsupported('map fallback type `${fallback_type}`')
				}
				hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
				fallback = '(builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(map_value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
			}
			if had_err {
				g.locals['err'] = previous_err
			} else {
				g.locals.delete('err')
			}
			g.expect(.rcbr)!
			if fallback.contains('err') {
				fallback = '({ IError err = ${temporary}.err; ${fallback}; })'
			}
			if value_type == '' {
				value_type = if option_value_type != '' {
					option_value_type
				} else if fallback_type == '' {
					'void'
				} else {
					fallback_type
				}
			}
			if outer_cast && paren_depth > 0 && g.tok == .rpar {
				paren_depth--
				g.next()
			}
			result.go_back(result.len)
			success_value := if value_type == 'void' {
				'0'
			} else {
				'*((${value_type} *)${temporary}.data)'
			}
			result.write_string(assignment_prefix)
			result.write_string('('.repeat(wrapper_parens))
			if value_type != 'void' && fallback_type in ['', 'void'] {
				result.write_string('({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { ${fallback}; } ${success_value}; })')
			} else {
				result.write_string('({ Option ${temporary} = (${option_expression}); ${temporary}.state ? (${fallback}) : ${success_value}; })')
			}
			expression_tokens = []FastcExpressionToken{}
			for _ in 0 .. wrapper_parens {
				expression_tokens << FastcExpressionToken{
					tok: .lpar
					lit: '('
				}
			}
			expression_tokens << FastcExpressionToken{
				tok:          .name
				lit:          temporary
				typ:          value_type
				is_statement: or_expression_is_statement
			}
			if value_type == 'void' {
				expression_tokens << FastcExpressionToken{
					tok: .assign
					lit: '='
				}
			}
			if assignment_prefix != '' {
				expression_tokens << FastcExpressionToken{
					tok: .assign
					lit: '='
				}
			}
			previous_token = .name
			previous_lit = temporary
			previous_module_separator = false
			previous_token_end = g.s.pos
			g.last_multi_return_types = or_return_types.clone()
			continue
		}
		if g.selfhost && g.tok == .name && g.lit.starts_with('@') {
			pseudo_name := g.lit
			pseudo := g.comptime_pseudo_expression(pseudo_name) or {
				return g.unsupported('compile-time pseudo value `${pseudo_name}`')
			}
			if result.len > 0 && fastc_needs_space(result.last(), pseudo)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(pseudo)
			expression_tokens << FastcExpressionToken{
				tok:    .string
				lit:    pseudo_name
				source: pseudo
				typ:    'string'
			}
			previous_token = .string
			previous_lit = pseudo_name
			previous_module_separator = false
			previous_token_end = g.s.offset
			g.next()
			continue
		}
		if !g.selfhost
			&& g.tok in [.left_shift, .right_shift, .right_shift_unsigned, .left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
			// V defines oversized shifts to produce zero. Raw C shifts are
			// undefined and may mask the count to the operand width instead.
			return g.unsupported('shift expressions')
		}
		if !g.selfhost && g.tok in [.div, .div_assign, .mod, .mod_assign] {
			// Integer division and modulo require V's runtime zero checks. This
			// scanner-only lane has no type information to add them selectively.
			return g.unsupported('division or modulo expressions')
		}
		if !g.selfhost && g.tok == .key_sizeof {
			// Direct C representations can differ from V layouts. Reject sizeof
			// until the parser tracks enough V type information to lower it.
			return g.unsupported('sizeof expressions')
		}
		if !g.selfhost && g.tok in [.lsbr, .rsbr] {
			// Indexing requires V element types and bounds checks. C pointer/array
			// indexing cannot preserve either in this scanner-only lane.
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if (!g.selfhost || g.tok !in [.lcbr, .rcbr])
			&& g.tok in [.lcbr, .rcbr, .str_dollar, .key_match, .key_or, .key_as, .arrow, .power] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if !g.selfhost && g.tok in [.key_in, .not_in] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if !g.selfhost && g.tok in [.key_is, .not_is] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		match g.tok {
			.plus, .minus {
				has_sum_arithmetic_operator = true
			}
			.mul {
				has_multiply_operator = true
			}
			.amp {
				has_and_operator = true
			}
			.pipe {
				has_pipe_operator = true
			}
			.xor {
				has_xor_operator = true
			}
			else {}
		}
		if !g.selfhost && ((has_sum_arithmetic_operator && (has_and_operator
			|| has_pipe_operator || has_xor_operator))
			|| (has_multiply_operator && has_and_operator)
			|| (has_pipe_operator && has_xor_operator)) {
			// V groups + and - with | and ^, and * with &, while C splits those
			// levels and also orders + and - above &. Reject ambiguous token streams.
			return g.unsupported('mixed operator precedence')
		}
		if g.tok.is_assignment() || g.tok in [.inc, .dec] {
			is_declaration_guard := allow_declaration_guard && g.tok == .decl_assign
				&& source_token_count == 1
			if (!allow_mutation_statement && !is_declaration_guard)
				|| paren_depth != 0 || bracket_depth != 0 || brace_depth != 0
				|| unsafe_expression_depth != 0 {
				return g.unsupported('mutation `${g.token_source()}` inside an expression')
			}
			if mutation_operator != .unknown {
				return g.unsupported('multiple mutations in one expression')
			}
			mutation_operator = g.tok
			tokens_before_mutation = source_token_count
			mut mutation_lookahead := g.s
			next_mutation_token := mutation_lookahead.scan()
			mutation_ends_line := mutation_lookahead.pos >= g.s.offset
				&& g.s.src[g.s.offset..mutation_lookahead.pos].contains('\n')
			if mutation_operator in [.inc, .dec] && next_mutation_token !in stops
				&& next_mutation_token != .eof && !mutation_ends_line {
				return g.unsupported('postfix mutation used inside an expression')
			}
			if mutation_operator.is_assignment()
				&& (next_mutation_token in stops || next_mutation_token == .eof) {
				return g.unsupported('assignment without a value')
			}
		}
		source_token_count++
		expression_tokens << FastcExpressionToken{
			tok:             g.tok
			lit:             g.lit
			unsafe_depth:    g.unsafe_depth
			is_mut_argument: next_token_is_mut_argument
		}
		next_token_is_mut_argument = false
		module_separator := g.expression_dot_is_module_separator(expression_tokens,
			expression_tokens.len - 1)
		qualified_name_owner := if g.tok == .name && previous_token == .dot
			&& expression_tokens.len >= 3 {
			expression_tokens[expression_tokens.len - 3].lit
		} else {
			''
		}
		mut piece := g.expression_token(previous_token, previous_lit, qualified_name_owner)!
		if g.tok == .name {
			if local := g.locals[g.lit] {
				mut lookahead := g.s
				next_token := lookahead.scan()
				is_single_value := expression_tokens.len == 1
					&& (next_token in stops || next_token == .eof)
				if local.is_reference && !expression_tokens.last().is_mut_argument
					&& next_token !in [.dot, .lsbr] && !is_single_value {
					piece = '(*(${piece}))'
				}
			}
		}
		if module_separator && piece == '.' {
			piece = '__'
		}
		if g.tok == .name && previous_token == .dot {
			mut method_lookahead := g.s
			if method_lookahead.scan() == .lpar {
				piece = g.lit
			}
		}
		if g.tok == .name && expression_tokens.len >= 3
			&& expression_tokens[expression_tokens.len - 2].tok == .dot
			&& expression_tokens[expression_tokens.len - 3].tok == .name
			&& expression_tokens[expression_tokens.len - 3].lit == 'C' {
			piece = g.lit
		}
		if g.selfhost && brace_depth > 0 && g.tok == .name {
			mut field_lookahead := g.s
			if field_lookahead.scan() == .colon {
				piece = g.lit
			}
		}
		if g.selfhost && g.tok == .dot && previous_was_pointer_cast {
			piece = '->'
		}
		if g.tok == .lpar && previous_token == .name {
			pointer_token := if expression_tokens.len >= 3
				&& fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 3) {
				expression_tokens[expression_tokens.len - 3].tok
			} else {
				token.Token.unknown
			}
			pointer_count := if pointer_token == .and {
				2
			} else if pointer_token == .amp {
				1
			} else {
				0
			}
			pointer_cast := pointer_count > 0
			pointer_suffix := '*'.repeat(pointer_count)
			pointer_prefix_len := pointer_count
			if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .lsbr
				&& expression_tokens[expression_tokens.len - 3].tok == .rsbr {
				element_type := fastc_primitive_c_type(previous_lit) or { previous_lit }
				array_type := fastc_array_c_type(element_type)
				result.go_back(previous_lit.len + 2)
				piece = '((${array_type})('
				cast_depths << paren_depth + 1
			} else if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .name
				&& expression_tokens[expression_tokens.len - 4].lit == 'C'
				&& expression_tokens[expression_tokens.len - 3].tok == .dot && previous_lit.len > 0
				&& previous_lit[0].is_capital() && 'C.${previous_lit}' !in g.functions {
				c_pointer_token := if expression_tokens.len >= 5
					&& fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 5) {
					expression_tokens[expression_tokens.len - 5].tok
				} else {
					token.Token.unknown
				}
				c_pointer_count := if c_pointer_token == .and {
					2
				} else if c_pointer_token == .amp {
					1
				} else {
					0
				}
				result.go_back(previous_lit.len + c_pointer_count)
				piece = '((${previous_lit}${'*'.repeat(c_pointer_count)})('
				cast_depths << paren_depth + 1
				if c_pointer_count > 0 {
					pointer_cast_depths << paren_depth + 1
				}
			} else if cast_type := fastc_primitive_c_type(previous_lit) {
				result.go_back(cast_type.len + pointer_prefix_len)
				piece = '((${cast_type}${pointer_suffix})('
				cast_depths << paren_depth + 1
				if pointer_cast {
					pointer_cast_depths << paren_depth + 1
				}
			} else if type_key := fastc_resolve_declared_type_key(g.module_name, previous_lit,
				g.imports, g.declared_types)
			{
				cast_type := fastc_c_declared_type_name(type_key)
				result.go_back(cast_type.len + pointer_prefix_len)
				piece = '((${cast_type}${pointer_suffix})('
				cast_depths << paren_depth + 1
				if pointer_cast {
					pointer_cast_depths << paren_depth + 1
				}
			}
		}
		if g.selfhost && g.tok == .lcbr {
			initializer_tokens := expression_tokens[..expression_tokens.len - 1].clone()
			initializer_start := fastc_initializer_type_start(initializer_tokens)
			initializer_type_tokens := initializer_tokens[initializer_start..]
			mut is_struct_literal := false
			if map_type := g.map_initializer_type(initializer_type_tokens) {
				fastc_register_composite_type(map_type, mut g.composite_types)
				initializer_source := g.render_raw_expression_tokens(initializer_type_tokens) or {
					return g.unsupported('map initializer type')
				}
				result.go_back(initializer_source.len)
				piece = '(${map_type}){'
				brace_depth++
				struct_types << map_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if array_type := g.array_initializer_type(initializer_type_tokens) {
				fastc_register_composite_type(array_type, mut g.composite_types)
				initializer_source := g.render_raw_expression_tokens(initializer_type_tokens) or {
					return g.unsupported('array initializer type')
				}
				result.go_back(initializer_source.len)
				c_array_type := fastc_array_initializer_c_type(array_type)
				if array_type.starts_with('FixedArray_') {
					g.fixed_array_types[c_array_type] = array_type
				}
				piece = '(${c_array_type}){'
				brace_depth++
				struct_types << c_array_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .name
				&& expression_tokens[expression_tokens.len - 4].lit == 'C'
				&& expression_tokens[expression_tokens.len - 3].tok == .dot
				&& expression_tokens[expression_tokens.len - 2].tok == .name {
				raw_c_type := expression_tokens[expression_tokens.len - 2].lit
				c_type := if '#Cstruct#${raw_c_type}' in g.declared_types {
					'struct ${raw_c_type}'
				} else {
					raw_c_type
				}
				result.go_back(raw_c_type.len)
				piece = '(${c_type}){'
				brace_depth++
				struct_types << c_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .name
				&& expression_tokens[expression_tokens.len - 3].tok == .dot
				&& expression_tokens[expression_tokens.len - 2].tok == .name {
				module_alias := expression_tokens[expression_tokens.len - 4].lit
				if imported_module := g.imports[module_alias] {
					raw_type := expression_tokens[expression_tokens.len - 2].lit
					type_key := fastc_type_key(imported_module, raw_type)
					if type_key in g.declared_types {
						c_type := fastc_c_declared_type_name(type_key)
						result.go_back(c_type.len)
						piece = '(${c_type}){'
						brace_depth++
						struct_types << c_type
						struct_depths << brace_depth
						struct_paren_depths << paren_depth
						is_struct_literal = true
					}
				}
			} else if previous_token == .name {
				if type_key := fastc_resolve_declared_type_key(g.module_name, previous_lit,
					g.imports, g.declared_types)
				{
					c_type := fastc_c_declared_type_name(type_key)
					result.go_back(c_type.len)
					piece = '(${c_type}){'
					brace_depth++
					struct_types << c_type
					struct_depths << brace_depth
					struct_paren_depths << paren_depth
					is_struct_literal = true
				}
			}
			if !is_struct_literal && brace_depth > 0 {
				brace_depth++
				piece = '{'
			}
		} else if g.selfhost && g.tok == .rcbr && brace_depth > 0 {
			if struct_depths.len > 0 && struct_depths.last() == brace_depth {
				struct_depths.delete_last()
				if struct_types.len > 0 {
					struct_types.delete_last()
				}
				if struct_paren_depths.len > 0 {
					struct_paren_depths.delete_last()
				}
				expected_struct_field_type = ''
			}
			brace_depth--
			piece = '}'
		} else if g.selfhost && g.tok == .colon && struct_depths.len > 0
			&& brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() {
			piece = '='
		} else if g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last()
			&& paren_depth == struct_paren_depths.last() && g.tok == .semicolon {
			piece = ','
		} else if g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last()
			&& paren_depth == struct_paren_depths.last() && g.tok == .name
			&& previous_token in [.lcbr, .comma, .semicolon] && struct_types.len > 0 {
			mut fields := map[string]string{}
			if struct_types.last() in g.struct_fields {
				fields = g.struct_fields[struct_types.last()].clone()
			}
			expected_struct_field_type = fields[g.lit] or { '' }
			piece = '.${piece}'
		} else if g.selfhost && g.tok == .dot
			&& fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 1) {
			mut contextual_type := if expected_struct_field_type != '' {
				expected_struct_field_type
			} else {
				g.expected_expression_type
			}
			if contextual_type == '' {
				mut assignment_depth := 0
				for i, assignment_token in expression_tokens[..expression_tokens.len - 1] {
					if assignment_token.tok in [.lpar, .lsbr, .lcbr] {
						assignment_depth++
					} else if assignment_token.tok in [.rpar, .rsbr, .rcbr] {
						assignment_depth--
					} else if assignment_depth == 0 && assignment_token.tok.is_assignment() && i > 0 {
						contextual_type = g.infer_expression_type(expression_tokens[..i]) or { '' }
						break
					}
				}
			}
			if g.declared_kinds[g.semantic_type_key(contextual_type)] != .enum_ {
				contextual_type = ''
			}
			if contextual_type == '' {
				contextual_type = g.expected_call_argument_type(expression_tokens)
			}
			if expression_tokens.len >= 2
				&& expression_tokens[expression_tokens.len - 2].tok in [.eq, .ne, .gt, .lt, .ge, .le, .pipe, .amp, .xor] {
				operator_index := expression_tokens.len - 2
				mut operand_start := 0
				mut operand_depth := 0
				for i := operator_index - 1; i >= 0; i-- {
					if expression_tokens[i].tok in [.rpar, .rsbr, .rcbr] {
						operand_depth++
					} else if expression_tokens[i].tok in [.lpar, .lsbr, .lcbr] {
						operand_depth--
					} else if operand_depth == 0 && expression_tokens[i].tok in [.and, .logical_or] {
						operand_start = i + 1
						break
					}
				}
				inferred_type := g.infer_expression_type(expression_tokens[operand_start..operator_index]) or {
					''
				}
				if inferred_type != '' {
					contextual_type = inferred_type
				}
			}
			if g.declared_kinds[g.semantic_type_key(contextual_type)] == .enum_ {
				piece = ''
				enum_shorthand_type = contextual_type
			}
		} else if g.selfhost && enum_shorthand_type != '' && g.tok == .name {
			piece = '${enum_shorthand_type.trim_right('*')}__${g.lit}'
			expression_tokens[expression_tokens.len - 1].typ = enum_shorthand_type
			enum_shorthand_type = ''
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator
			&& !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
		match g.tok {
			.lpar {
				paren_depth++
			}
			.rpar {
				if paren_depth == 0 {
					break
				}
				if paren_depth in pointer_cast_depths {
					previous_was_pointer_cast = true
					pointer_cast_depths.delete(pointer_cast_depths.index(paren_depth))
				}
				if paren_depth in cast_depths {
					result.go_back(piece.len)
					piece = '))'
					result.write_string(piece)
					cast_depths.delete(cast_depths.index(paren_depth))
				}
				paren_depth--
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				bracket_depth--
			}
			else {}
		}
		if g.tok != .rpar {
			previous_was_pointer_cast = false
		}
		previous_token = g.tok
		previous_lit = g.lit
		previous_module_separator = module_separator
		previous_token_end = g.s.offset
		g.next()
	}
	if paren_depth != 0 {
		return g.unsupported('unbalanced expression')
	}
	if bracket_depth != 0 {
		return g.unsupported('unbalanced array expression')
	}
	if brace_depth != 0 {
		return g.unsupported('unbalanced struct literal')
	}
	if unsafe_expression_depth != 0 {
		g.unsafe_depth -= unsafe_expression_depth
		return g.unsupported('unbalanced unsafe expression `${fastc_expression_tokens_debug(expression_tokens)}`')
	}
	if mutation_operator != .unknown {
		if tokens_before_mutation == 0 {
			return g.unsupported('mutation without a target')
		}
	}
	g.validate_expression_mutation_lvalue(expression_tokens)!
	g.validate_expression_field_visibility(expression_tokens)!
	g.validate_expression_calls(expression_tokens)!
	mut rendered_expression := result.str().trim_space()
	rendered_expression = g.render_enum_alias_member_references(expression_tokens,
		rendered_expression)
	rendered_expression = g.render_constant_references(expression_tokens, rendered_expression)
	if special := g.render_special_expression(expression_tokens, rendered_expression) {
		g.last_expression_type = special.typ
		g.last_expression = expression_tokens
		return g.render_constant_references(expression_tokens, special.source)
	}
	g.last_expression_type = g.infer_expression_type(expression_tokens)!
	g.last_expression = expression_tokens
	return_types := g.multi_return_types_for_expression(expression_tokens)
	if return_types.len > 0 {
		g.last_multi_return_types = return_types.clone()
	}
	if !g.selfhost && g.last_expression_type == 'bool'
		&& fastc_expression_tokens_contain_boolean_operator(expression_tokens) {
		// C comparison and logical operators produce int. Preserve V's bool
		// expression type for inferred declarations and generic dispatch.
		return '((bool)(${rendered_expression}))'
	}
	return rendered_expression
}

fn (g &Parser) render_constant_references(tokens []FastcExpressionToken, source string) string {
	mut rendered := source
	for i, item in tokens {
		if item.tok != .name || (i > 0 && tokens[i - 1].tok == .dot)
			|| (i + 1 < tokens.len && tokens[i + 1].tok == .colon)
			|| item.lit in g.locals {
			continue
		}
		if i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			function_key := g.unqualified_function_key(item.lit)
			if function_key in g.functions {
				rendered = fastc_replace_c_identifier(rendered, item.lit,
					fastc_c_function_name_for_key(function_key))
				continue
			}
		}
		constant_key := fastc_constant_key(g.module_name, item.lit)
		if c_name := g.constants[constant_key] {
			rendered = fastc_replace_c_identifier(rendered, item.lit, c_name)
		} else if c_name := g.constants[fastc_constant_key('builtin', item.lit)] {
			rendered = fastc_replace_c_identifier(rendered, item.lit, c_name)
		}
	}
	return rendered
}

fn fastc_replace_c_identifier(source string, identifier string, replacement string) string {
	if identifier == '' || identifier == replacement || !source.contains(identifier) {
		return source
	}
	mut out := strings.new_builder(source.len + replacement.len)
	mut start := 0
	for start < source.len {
		remaining := source[start..]
		relative := remaining.index(identifier) or {
			out.write_string(remaining)
			break
		}
		index := start + relative
		end := index + identifier.len
		before_is_name := index > 0 && (source[index - 1].is_alnum() || source[index - 1] == `_`)
		after_is_name := end < source.len && (source[end].is_alnum() || source[end] == `_`)
		out.write_string(source[start..index])
		if before_is_name || after_is_name {
			out.write_string(identifier)
		} else {
			out.write_string(replacement)
		}
		start = end
	}
	return out.str()
}

fn (g &Parser) semicolon_continues_expression() bool {
	mut offset := g.s.offset
	for offset < g.s.src.len && g.s.src[offset] in [` `, `\t`, `\r`, `\n`] {
		offset++
	}
	if offset >= g.s.src.len {
		return false
	}
	if offset + 1 < g.s.src.len && g.s.src[offset] == `/` && g.s.src[offset + 1] in [`/`, `*`] {
		return false
	}
	return g.s.src[offset] in [`.`, `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<`, `>`, `=`, `?`]
}

fn fastc_runtime_c_type(typ string) string {
	base := typ.trim_right('*')
	mut runtime_type := if base.starts_with('Map_') {
		'map'
	} else if base.starts_with('Array_') {
		'array'
	} else {
		base
	}
	return runtime_type + '*'.repeat(typ.len - base.len)
}

fn (mut g Parser) read_inferred_map_literal() !string {
	g.expect(.lcbr)!
	mut keys := []string{}
	mut values := []string{}
	mut key_type := ''
	mut value_type := ''
	for g.tok != .rcbr {
		g.skip_semicolons()
		if g.tok == .rcbr {
			break
		}
		key := g.read_expression([token.Token.colon])!
		actual_key_type := fastc_normalize_inferred_type(g.last_expression_type)
		g.expect(.colon)!
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		actual_value_type := fastc_normalize_inferred_type(g.last_expression_type)
		if key_type == '' {
			key_type = actual_key_type
			value_type = actual_value_type
		}
		keys << key
		values << value
		if g.tok in [.comma, .semicolon] {
			g.next()
		}
	}
	g.expect(.rcbr)!
	if keys.len == 0 || key_type == '' || value_type == '' {
		return g.unsupported('empty inferred map literal')
	}
	map_type := fastc_map_c_type(key_type, value_type)
	hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
	map_name := g.temporary_name('map_literal')
	mut statements := [
		'map ${map_name} = builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn});',
	]
	for i, key in keys {
		key_name := g.temporary_name('map_key')
		value_name := g.temporary_name('map_value')
		statements << '${fastc_runtime_c_type(key_type)} ${key_name} = (${key});'
		statements << '${fastc_runtime_c_type(value_type)} ${value_name} = (${values[i]});'
		statements << 'builtin__map_set(&${map_name}, &${key_name}, &${value_name});'
	}
	g.last_expression_type = map_type
	g.last_expression = [
		FastcExpressionToken{
			tok: .name
			lit: map_name
			typ: map_type
		},
	]
	return '({ ${statements.join(' ')} ${map_name}; })'
}

fn (g &Parser) expected_call_argument_type(tokens []FastcExpressionToken) string {
	if tokens.len < 3 {
		return ''
	}
	mut depth := 0
	mut open_index := -1
	for i := tokens.len - 2; i >= 0; i-- {
		if tokens[i].tok == .rpar {
			depth++
		} else if tokens[i].tok == .lpar {
			if depth == 0 {
				open_index = i
				break
			}
			depth--
		}
	}
	if open_index <= 0 || tokens[open_index - 1].tok != .name {
		return ''
	}
	name_index := open_index - 1
	mut function_key := g.function_key_for_call(tokens, name_index)
	mut argument_offset := 0
	if name_index >= 2 && tokens[name_index - 1].tok == .dot && !(name_index == 2
		&& tokens[0].tok == .name && (tokens[0].lit in g.imports || tokens[0].lit == 'C')) {
		receiver_start := fastc_method_receiver_start(tokens, name_index - 1)
		receiver_type := g.infer_expression_type(tokens[receiver_start..name_index - 1]) or {
			return ''
		}
		function_key = '${g.semantic_type_key(receiver_type)}.${tokens[name_index].lit}'
		argument_offset = 1
	}
	signature := g.functions[function_key] or { return '' }
	mut argument_index := 0
	mut nested := 0
	for i in open_index + 1 .. tokens.len - 1 {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			nested++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			nested--
		} else if nested == 0 && tokens[i].tok == .comma {
			argument_index++
		}
	}
	parameter_index := argument_index + argument_offset
	if parameter_index >= signature.parameter_types.len {
		return ''
	}
	return signature.parameter_types[parameter_index]
}

fn (g &Parser) render_special_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if type_name := g.render_typeof_name_expression(tokens) {
		return type_name
	}
	if disabled_call := g.render_disabled_call_expression(tokens) {
		return disabled_call
	}
	if enum_print := g.render_enum_print_expression(tokens) {
		return enum_print
	}
	if bool_print := g.render_bool_print_expression(tokens) {
		return bool_print
	}
	if string_print := g.render_ordinary_string_print_expression(tokens) {
		return string_print
	}
	if tokens.len == 1 && tokens[0].tok == .name {
		if local := g.locals[tokens[0].lit] {
			if local.is_reference {
				return FastcRenderedExpression{
					source: '*(${rendered_expression})'
					typ:    local.typ.trim_right('*')
				}
			}
		}
	}
	if struct_comparison := g.render_struct_comparison_expression(tokens) {
		return struct_comparison
	}
	if integer_comparison := g.render_mixed_integer_comparison_expression(tokens) {
		return integer_comparison
	}
	if !g.selfhost {
		if string_comparison := g.render_string_comparison_expression(tokens) {
			return string_comparison
		}
		if concatenation := g.render_composed_string_concatenation(tokens) {
			return concatenation
		}
	}
	if g.selfhost {
		if interface_cast := g.render_interface_cast_expression(tokens, rendered_expression) {
			return interface_cast
		}
	}
	if cast_expression := g.render_cast_expression(tokens) {
		if pointer_members := g.render_pointer_member_access_expression(tokens,
			cast_expression.source)
		{
			return pointer_members
		}
		return cast_expression
	}
	if g.selfhost {
		if defaulted_call := g.render_missing_call_arguments(tokens) {
			return defaulted_call
		}
		if map_expression := g.render_map_expression(tokens) {
			return map_expression
		}
		if struct_literal := g.render_struct_literal_expression(tokens) {
			return struct_literal
		}
		if struct_literal := g.render_struct_literal_field_names(tokens, rendered_expression) {
			return struct_literal
		}
		if initializer_assignment := g.render_initializer_assignment_expression(tokens) {
			return initializer_assignment
		}
		if assignment := g.render_assignment_expression(tokens) {
			return assignment
		}
		if array_assignment := g.render_array_assignment_expression(tokens) {
			return array_assignment
		}
		if static_call := g.render_static_call_expression(tokens, rendered_expression) {
			return static_call
		}
		if logical := g.render_logical_expression(tokens) {
			return logical
		}
		if tokens.len > 1 && tokens[0].tok == .not {
			inner := g.render_call_argument_expression(tokens[1..], 'bool') or { return none }
			return FastcRenderedExpression{
				source: '!(${inner})'
				typ:    'bool'
			}
		}
		if option_comparison := g.render_option_none_comparison(tokens) {
			return option_comparison
		}
		if enum_comparison := g.render_enum_comparison_expression(tokens) {
			return enum_comparison
		}
		if string_comparison := g.render_string_comparison_expression(tokens) {
			return string_comparison
		}
		if concatenation := g.render_composed_string_concatenation(tokens) {
			return concatenation
		}
		if tokens.len > 1 && tokens.last().tok == .not && rendered_expression.ends_with('!')
			&& !(tokens[0].tok == .lsbr && tokens[tokens.len - 2].tok == .rsbr) {
			inner_tokens := tokens[..tokens.len - 1]
			mut inner_source := rendered_expression[..rendered_expression.len - 1]
			if method_expression := g.render_method_call_expression(inner_tokens, inner_source) {
				inner_source = method_expression.source
			} else if array_expression := g.render_array_access_expression(inner_tokens) {
				inner_source = array_expression.source
			} else if defaulted_call := g.render_missing_call_arguments(inner_tokens) {
				inner_source = defaulted_call.source
			}
			value_type := g.option_value_type_for_expression(inner_tokens)
			temporary := '__v_fastc_option_propagate'
			failure := if g.in_main {
				deferred := g.deferred_scopes_source()
				'${deferred} builtin__panic_result_not_set(builtin__IError_msg(${temporary}.err));'
			} else if g.return_type == 'Option' {
				'return ${temporary};'
			} else {
				'return 1;'
			}
			value := if value_type in ['', 'void'] {
				'0'
			} else {
				'*((${value_type} *)${temporary}.data)'
			}
			return FastcRenderedExpression{
				source: '({ Option ${temporary} = (${inner_source}); if (${temporary}.state) { ${failure} } ${value}; })'
				typ:    value_type
			}
		}
		if append_expression := g.render_append_expression(tokens, rendered_expression) {
			return append_expression
		}
		mut depth := 0
		for i, item in tokens {
			if item.tok in [.lpar, .lsbr, .lcbr] {
				depth++
			} else if item.tok in [.rpar, .rsbr, .rcbr] {
				depth--
			} else if depth == 0 && item.tok in [.key_in, .not_in] && i > 0 && i + 1 < tokens.len {
				temporary_namespace := g.temporary_namespace('membership')
				right_tokens := tokens[i + 1..]
				right_type := g.infer_expression_type(right_tokens) or { return none }
				if g.underlying_alias_type(right_type) == 'string' {
					left_type := g.infer_expression_type(tokens[..i]) or { return none }
					if g.underlying_alias_type(left_type) != 'string' {
						return none
					}
					substring := g.render_membership_candidate(tokens[..i], 'string') or {
						return none
					}
					value := g.render_call_argument_expression(right_tokens, right_type) or {
						return none
					}
					substring_name := '${temporary_namespace}_substring'
					value_name := '${temporary_namespace}_value'
					found := 'v_fastc_string_contains(${value_name}, ${substring_name})'
					predicate := if item.tok == .not_in { '!(${found})' } else { found }
					return FastcRenderedExpression{
						source: '({ string ${substring_name} = (${substring}); string ${value_name} = (${value}); ${predicate}; })'
						typ:    'bool'
					}
				}
				if right_type.trim_right('*').starts_with('Map_') {
					key_type, _ := fastc_map_key_value_types(right_type) or { return none }
					key_source := g.render_membership_candidate(tokens[..i], key_type) or {
						return none
					}
					map_source := g.render_member_receiver(right_tokens) or { return none }
					map_expression := if right_type.ends_with('*') {
						map_source
					} else {
						'&(${map_source})'
					}
					key_name := '${temporary_namespace}_map_key'
					found := 'builtin__map_get_check((map *)${map_expression}, &${key_name}) != NULL'
					predicate := if item.tok == .not_in { '!(${found})' } else { found }
					return FastcRenderedExpression{
						source: '({ ${fastc_runtime_c_type(key_type)} ${key_name} = (${key_source}); ${predicate}; })'
						typ:    'bool'
					}
				}
				if right_type.trim_right('*').starts_with('Array_') {
					element_type := g.array_element_type(right_type) or { return none }
					candidate := g.render_membership_candidate(tokens[..i], element_type) or {
						return none
					}
					collection := g.render_call_argument_expression(right_tokens, right_type) or {
						return none
					}
					item_name := '${temporary_namespace}_item'
					collection_name := '${temporary_namespace}_collection'
					found_name := '${temporary_namespace}_found'
					index_name := '${temporary_namespace}_index'
					access := if right_type.ends_with('*') { '->' } else { '.' }
					comparison := if g.underlying_alias_type(element_type).trim_right('*') == 'string' {
						'builtin__string_eq(${item_name}, ((${element_type} *)${collection_name}${access}data)[${index_name}])'
					} else {
						'(${item_name} == ((${element_type} *)${collection_name}${access}data)[${index_name}])'
					}
					// A hoisted predicate keeps this interpolation flat: the FastC
					// selfhost parser renders nested `${if ... { '${...}' }}` blocks
					// literally, corrupting the emitted membership expression.
					predicate := if item.tok == .not_in { '!${found_name}' } else { found_name }
					return FastcRenderedExpression{
						source: '({ ${element_type} ${item_name} = (${candidate}); __typeof__((${collection})) ${collection_name} = (${collection}); bool ${found_name} = false; for (int ${index_name} = 0; ${index_name} < ${collection_name}${access}len; ${index_name}++) { if (${comparison}) { ${found_name} = true; break; } } ${predicate}; })'
						typ:    'bool'
					}
				}
				if i + 2 >= tokens.len || tokens[i + 1].tok != .lsbr || tokens.last().tok != .rsbr {
					continue
				}
				lhs_type := g.infer_expression_type(tokens[..i]) or { return none }
				items := fastc_expression_list_items(tokens, i + 2, tokens.len - 1) or {
					return none
				}
				if items.len == 0 {
					return FastcRenderedExpression{
						source: if item.tok == .key_in { 'false' } else { 'true' }
						typ:    'bool'
					}
				}
				lhs_source := g.render_membership_candidate(tokens[..i], lhs_type) or {
					return none
				}
				lhs_name := '${temporary_namespace}_subject'
				value_type := fastc_runtime_c_type(fastc_normalize_inferred_type(lhs_type))
				mut initializers := []string{cap: items.len}
				mut comparisons := []string{cap: items.len}
				for candidate_index, candidate in items {
					candidate_source := g.render_membership_candidate(candidate, lhs_type) or {
						return none
					}
					candidate_name := '${temporary_namespace}_candidate_${candidate_index}'
					initializers << '${value_type} ${candidate_name} = (${candidate_source});'
					comparison := if g.underlying_alias_type(lhs_type).trim_right('*') == 'string' {
						'builtin__string_eq(${lhs_name}, ${candidate_name})'
					} else {
						'((${lhs_name}) == (${candidate_name}))'
					}
					comparisons << if item.tok == .key_in { comparison } else { '!${comparison}' }
				}
				joiner := if item.tok == .key_in { ' || ' } else { ' && ' }
				return FastcRenderedExpression{
					source: '({ ${value_type} ${lhs_name} = (${lhs_source}); ${initializers.join(' ')} (${comparisons.join(joiner)}); })'
					typ:    'bool'
				}
			}
		}
		// Resolve a complete index expression before the generic pointer/member
		// rewriter gets a chance to treat its base as part of a longer chain.
		if array_access := g.render_array_access_expression(tokens) {
			return array_access
		}
		if pointer_members := g.render_pointer_member_access_expression(tokens, rendered_expression) {
			return pointer_members
		}
		if method_expression := g.render_method_call_expression(tokens, rendered_expression) {
			if array_expression := g.render_nested_array_access_expression(tokens,
				method_expression.source)
			{
				return FastcRenderedExpression{
					source: array_expression.source
					typ:    method_expression.typ
				}
			}
			return method_expression
		}
		if defaulted_call := g.render_missing_call_arguments(tokens) {
			return defaulted_call
		}
		if array_expression := g.render_nested_array_access_expression(tokens, rendered_expression) {
			return array_expression
		}
	}
	if g.selfhost && tokens.len == 3 && tokens[0].tok == .name
		&& tokens[1].tok in [.key_is, .not_is] && tokens[2].tok == .name {
		lhs_type := g.infer_expression_type(tokens[..1]) or { return none }
		type_key := fastc_resolve_declared_type_key(g.module_name, tokens[2].lit, g.imports,
			g.declared_types) or { return none }
		access := if lhs_type.ends_with('*') { '->' } else { '.' }
		operator := if tokens[1].tok == .key_is { '==' } else { '!=' }
		return FastcRenderedExpression{
			source: '((${tokens[0].lit}${access}_typ) ${operator} __v_typeid_${fastc_c_declared_type_name(type_key)})'
			typ:    'bool'
		}
	}
	if g.selfhost {
		mut init_open := -1
		for i, item in tokens {
			if item.tok == .lcbr {
				init_open = i
				break
			}
		}
		if init_open > 0 && tokens.last().tok == .rcbr {
			if array_type := g.array_initializer_type(tokens[..init_open]) {
				return FastcRenderedExpression{
					source: rendered_expression
					typ:    array_type
				}
			}
		}
	}
	array_end := if tokens.len > 0 && tokens.last().tok == .not {
		tokens.len - 1
	} else {
		tokens.len
	}
	if g.selfhost && array_end == 2 && tokens[0].tok == .lsbr && tokens[1].tok == .rsbr
		&& g.expected_expression_type.trim_right('*').starts_with('Array_') {
		return FastcRenderedExpression{
			source: '(${g.expected_expression_type}){0}'
			typ:    g.expected_expression_type
		}
	}
	if g.selfhost && array_end >= 2 && tokens[0].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
		items := fastc_expression_list_items(tokens, 1, array_end - 1) or { return none }
		if items.len == 0 {
			return none
		}
		element_type := g.infer_expression_type(items[0]) or { return none }
		if element_type == '' {
			return none
		}
		array_type := fastc_array_c_type(fastc_normalize_inferred_type(element_type))
		mut rendered_items := []string{cap: items.len}
		for item in items {
			rendered_items << g.render_call_argument_expression(item, element_type) or {
				return none
			}
		}
		return FastcRenderedExpression{
			source: '((${array_type})builtin__new_array_from_c_array(${items.len}, ${items.len}, sizeof(${fastc_normalize_inferred_type(element_type)}), (${fastc_normalize_inferred_type(element_type)}[]){${rendered_items.join(',')}}))'
			typ:    array_type
		}
	}
	if g.selfhost && tokens.len > 0 && tokens.len % 2 == 1 {
		mut is_literal_concat := true
		mut literals := strings.new_builder(rendered_expression.len)
		for i, item in tokens {
			if i % 2 == 0 {
				if item.tok != .string {
					is_literal_concat = false
					break
				}
				literal := fastc_c_string(item.lit) or {
					is_literal_concat = false
					break
				}
				literals.write_string(literal)
			} else if item.tok != .plus {
				is_literal_concat = false
				break
			}
		}
		if is_literal_concat {
			return FastcRenderedExpression{
				source: '_S(${literals.str()})'
				typ:    'string'
			}
		}
	}
	if g.selfhost {
		mut depth := 0
		mut operand_start := 0
		mut string_operands := []bool{}
		mut plus_count := 0
		for i, item in tokens {
			match item.tok {
				.lpar, .lsbr, .lcbr {
					depth++
				}
				.rpar, .rsbr, .rcbr {
					depth--
				}
				.plus {
					if depth == 0 {
						operand_type := g.infer_expression_type(tokens[operand_start..i]) or { '' }
						string_operands << operand_type == 'string'
						operand_start = i + 1
						plus_count++
					}
				}
				else {}
			}
		}
		if plus_count > 0 {
			last_operand_type := g.infer_expression_type(tokens[operand_start..]) or { '' }
			string_operands << last_operand_type == 'string'
			if fastc_all_true(string_operands) {
				parts := fastc_split_top_level_c_plus(rendered_expression)
				if parts.len == plus_count + 1 {
					mut combined := parts[0]
					for part in parts[1..] {
						combined = 'builtin__string_plus(${combined},${part})'
					}
					return FastcRenderedExpression{
						source: combined
						typ:    'string'
					}
				}
			}
		}
	}
	return g.render_flag_method_expression(tokens, rendered_expression)
}
