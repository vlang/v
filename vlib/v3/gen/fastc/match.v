module fastc

import v3.token

fn (mut g Parser) read_match_expression() !string {
	outer_expected_type := g.expected_expression_type
	g.expect(.key_match)!
	g.expected_expression_type = ''
	subject := g.read_expression([token.Token.lcbr])!
	subject_type := g.last_expression_type
	if subject == '' || subject_type == '' {
		return g.unsupported('unverifiable match expression subject')
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
		if g.tok == .key_else {
			is_else = true
			g.next()
		} else {
			for {
				g.expected_expression_type = subject_type
				start :=
					g.read_expression([token.Token.comma, token.Token.lcbr, token.Token.dotdot])!
				start_tokens := g.last_expression.clone()
				mut case_key := g.normalized_match_case_key(start_tokens, start)
				mut case_source := start
				if g.tok == .dotdot {
					g.next()
					finish := g.read_expression([token.Token.comma, token.Token.lcbr])!
					finish_tokens := g.last_expression.clone()
					case_key = 'range:${case_key}..${g.normalized_match_case_key(finish_tokens,
						finish)}'
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
		mut value := g.read_match_block_expression_value()!
		mut value_type := g.last_expression_type
		g.skip_semicolons()
		if g.tok != .rcbr {
			return g.unsupported('match branch `${value}` left `${g.token_source()}` (`${g.tok.str()}`)')
		}
		g.expect(.rcbr)!
		g.skip_semicolons()
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
		if g.selfhost && value_type == '' && result_type != '' {
			zero_type := fastc_normalize_inferred_type(result_type)
			value = '({ (void)(${value}); (${zero_type}){0}; })'
			value_type = result_type
		}
		if result_type == '' && value_type != '' {
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
		packed_values << 'V_FASTC_MULTI_VALUE(${value})'
	}
	g.last_expression_type = 'MultiReturn'
	g.last_expression = []FastcExpressionToken{}
	g.last_multi_return_types = value_types.clone()
	return '(MultiReturn){.values={${packed_values.join(', ')}}}'
}

fn (mut g Parser) read_block_expression_value() !string {
	g.skip_semicolons()
	if g.tok == .key_if && g.if_starts_final_block_expression() {
		return g.read_if_expression()!
	}
	if !g.or_block_has_statements() {
		return g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	}
	previous_capture := g.capturing_defer
	previous_lines := g.captured_defer_lines.clone()
	g.capturing_defer = true
	g.captured_defer_lines = []string{}
	for g.or_block_has_statements() {
		if g.tok == .key_if && g.if_starts_final_block_expression() {
			break
		}
		_ = g.parse_statement()!
		g.skip_semicolons()
	}
	statements := g.captured_defer_lines.clone()
	g.capturing_defer = previous_capture
	g.captured_defer_lines = previous_lines
	if g.tok == .rcbr {
		g.last_expression_type = ''
		g.last_expression = []FastcExpressionToken{}
		return '({ ${statements.join(' ')} 0; })'
	}
	g.skip_semicolons()
	mut value := if g.tok == .name {
		prefix := g.lit
		g.next()
		g.read_expression_with_prefix(prefix, [token.Token.semicolon, token.Token.rcbr])!
	} else {
		g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	}
	if g.tok == .name {
		prefix := g.lit
		g.next()
		final_value := g.read_expression_with_prefix(prefix,
			[token.Token.semicolon, token.Token.rcbr])!
		value = if value.trim_space() in ['', ';'] {
			final_value
		} else {
			value + final_value
		}
	}
	return '({ ${statements.join(' ')} ${value}; })'
}
