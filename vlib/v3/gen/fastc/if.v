module fastc

import v3.scanner
import v3.token

fn (g &Parser) or_block_has_statements() bool {
	if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
		return false
	}
	if g.tok in [.key_return, .key_if, .key_for, .key_match, .key_mut, .key_defer, .key_break,
		.key_continue] {
		return true
	}
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := g.tok
	mut depth := 0
	for tok != .eof {
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			if depth == 0 {
				return false
			}
			depth--
		} else if depth == 0 && tok == .key_return {
			return true
		} else if depth == 0 && tok == .semicolon {
			next_token := lookahead.scan()
			return next_token != .rcbr
		}
		tok = lookahead.scan()
	}
	return false
}

fn (mut g Parser) parse_if() !bool {
	g.next()
	mut condition := g.read_condition_expression([token.Token.semicolon, token.Token.lcbr])!
	if condition.len == 0 {
		return g.unsupported('empty if condition')
	}
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	if g.selfhost && g.last_expression.len >= 4 && g.last_expression[0].tok == .name
		&& g.last_expression[1].tok == .decl_assign {
		right_tokens := g.last_expression[2..]
		if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[0].lit
			guard_type = map_lookup.typ
			guard_option = g.temporary_name('if_guard')
			g.write_line('Option ${guard_option} = (${map_lookup.source});')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[0].lit
				guard_type = option_type
				guard_option = g.temporary_name('if_guard')
				right_source := condition.all_after(':=').trim_space()
				g.write_line('Option ${guard_option} = (${right_source});')
				condition = '${guard_option}.state == 0'
				g.last_expression_type = 'bool'
			}
		}
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	g.write_line('if (${condition}) {')
	g.indent++
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		g.write_line('${guard_type} ${fastc_c_identifier(guard_name)} = *((${guard_type} *)${guard_option}.data);')
		g.locals[guard_name] = FastcLocal{
			typ: guard_type
		}
	}
	then_terminates := g.parse_block_body()!
	if guard_name != '' {
		if had_guard {
			g.locals[guard_name] = previous_guard
		} else {
			g.locals.delete(guard_name)
		}
	}
	g.indent--
	if g.tok != .key_else {
		g.write_line('}')
		return false
	}
	g.next()
	if g.tok == .key_if {
		g.write_line('} else {')
		g.indent++
		else_terminates := g.parse_if()!
		g.indent--
		g.write_line('}')
		return then_terminates && else_terminates
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	else_terminates := g.parse_block_body()!
	g.indent--
	g.write_line('}')
	return then_terminates && else_terminates
}

fn (g &Parser) if_starts_final_block_expression() bool {
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := lookahead.scan()
	for {
		for tok !in [.lcbr, .eof] {
			tok = lookahead.scan()
		}
		if tok != .lcbr {
			return false
		}
		tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
		for tok == .semicolon {
			tok = lookahead.scan()
		}
		if tok != .key_else {
			return false
		}
		tok = lookahead.scan()
		for tok == .semicolon {
			tok = lookahead.scan()
		}
		if tok == .key_if {
			tok = lookahead.scan()
			continue
		}
		if tok != .lcbr {
			return false
		}
		tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
		break
	}
	for tok == .semicolon {
		tok = lookahead.scan()
	}
	return tok == .rcbr
}

fn fastc_option_success_expression(value_type string, expression string) string {
	base := fastc_normalize_inferred_type(value_type)
	return '(Option){.data=${fastc_box_expression(base, expression)}, .state=0}'
}

fn fastc_box_expression(value_type string, expression string) string {
	return '({ ${value_type} __v_fastc_box_value = (${expression}); v_fastc_interface_box(&__v_fastc_box_value, sizeof(${value_type})); })'
}

fn (mut g Parser) read_if_expression() !string {
	outer_expected_type := g.expected_expression_type
	branch_expected_type := if outer_expected_type != '' {
		outer_expected_type
	} else if g.declared_kinds[g.semantic_type_key(g.return_type)] == .enum_ {
		g.return_type
	} else {
		''
	}
	g.expect(.key_if)!
	g.expected_expression_type = ''
	mut condition := g.read_condition_expression([token.Token.semicolon, token.Token.lcbr])!
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	mut guard_source := ''
	if g.selfhost && g.last_expression.len >= 4 && g.last_expression[0].tok == .name
		&& g.last_expression[1].tok == .decl_assign {
		right_tokens := g.last_expression[2..]
		if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[0].lit
			guard_type = map_lookup.typ
			guard_source = map_lookup.source
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[0].lit
				guard_type = option_type
				guard_source = condition.all_after(':=').trim_space()
			}
		}
		if guard_name != '' {
			guard_option = g.temporary_name('if_guard')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		}
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		g.locals[guard_name] = FastcLocal{
			typ: guard_type
		}
	}
	g.expected_expression_type = branch_expected_type
	mut then_expression := if g.tok == .key_return {
		g.read_return_expression_branch()!
	} else {
		g.read_block_expression_value()!
	}
	if guard_name != '' {
		then_expression = '({ ${guard_type} ${fastc_c_identifier(guard_name)} = *((${guard_type} *)${guard_option}.data); ${then_expression}; })'
		if had_guard {
			g.locals[guard_name] = previous_guard
		} else {
			g.locals.delete(guard_name)
		}
	}
	mut then_type := g.last_expression_type
	if enum_expression := g.expected_enum_shorthand_expression() {
		then_expression = enum_expression
		then_type = g.return_type
	}
	g.skip_semicolons()
	g.expect(.rcbr)!
	if g.tok != .key_else {
		return g.unsupported('if expression without `else`')
	}
	g.next()
	mut else_expression := ''
	mut else_type := ''
	if g.tok == .key_if {
		g.expected_expression_type = branch_expected_type
		else_expression = g.read_if_expression()!
		else_type = g.last_expression_type
	} else {
		g.expect(.lcbr)!
		g.expected_expression_type = branch_expected_type
		else_expression = if g.tok == .key_return {
			g.read_return_expression_branch()!
		} else {
			g.read_block_expression_value()!
		}
		else_type = g.last_expression_type
		if enum_expression := g.expected_enum_shorthand_expression() {
			else_expression = enum_expression
			else_type = g.return_type
		}
		g.skip_semicolons()
		g.expect(.rcbr)!
	}
	if g.selfhost && outer_expected_type == 'Option' {
		if then_type != 'Option' {
			then_expression = g.option_branch_expression(then_type, then_expression)
			then_type = 'Option'
		}
		if else_type != 'Option' {
			else_expression = g.option_branch_expression(else_type, else_expression)
			else_type = 'Option'
		}
	} else if g.selfhost && then_type == 'Option' && else_type !in ['', 'Option'] {
		else_base := fastc_normalize_inferred_type(else_type)
		else_expression = '(Option){.data=${fastc_box_expression(else_base, else_expression)}, .state=0}'
		else_type = 'Option'
	} else if g.selfhost && else_type == 'Option' && then_type !in ['', 'Option'] {
		then_base := fastc_normalize_inferred_type(then_type)
		then_expression = '(Option){.data=${fastc_box_expression(then_base, then_expression)}, .state=0}'
		then_type = 'Option'
	}
	if then_type == else_type {
		g.last_expression_type = then_type
	} else if g.selfhost && then_type == '' {
		g.last_expression_type = else_type
	} else if g.selfhost && else_type == '' {
		g.last_expression_type = then_type
	} else if g.selfhost && fastc_is_integer_expression_type(then_type)
		&& fastc_is_integer_expression_type(else_type) {
		g.last_expression_type = if then_type == 'integer literal' { else_type } else { then_type }
	} else {
		g.last_expression_type = if outer_expected_type != '' {
			outer_expected_type
		} else if then_type != '' {
			then_type
		} else {
			else_type
		}
	}
	g.expected_expression_type = outer_expected_type
	g.last_expression = []FastcExpressionToken{}
	conditional := '((${condition}) ? (${then_expression}) : (${else_expression}))'
	return if guard_option == '' {
		conditional
	} else {
		'({ Option ${guard_option} = (${guard_source}); ${conditional}; })'
	}
}

fn (g &Parser) option_branch_expression(value_type string, expression string) string {
	if value_type.trim_right('*') == 'IError' {
		return '(Option){.err=${expression}, .state=1}'
	}
	if value_type == 'voidptr' && g.option_return_type != 'voidptr' {
		return '(Option){.err=(IError){._object=(voidptr)(${expression})}, .state=1}'
	}
	return fastc_option_success_expression(value_type, expression)
}

fn (mut g Parser) read_return_expression_branch() !string {
	g.expect(.key_return)!
	if g.return_type.trim_right('*') == 'MultiReturn' {
		mut values := []string{}
		for {
			value :=
				g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			values << 'V_FASTC_MULTI_VALUE(${value})'
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		g.last_expression_type = ''
		g.last_expression = []FastcExpressionToken{}
		return '({ return (MultiReturn){.values={${values.join(', ')}}}; 0; })'
	}
	value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.consume_statement_end()
	g.last_expression_type = ''
	g.last_expression = []FastcExpressionToken{}
	return '({ return ${value}; 0; })'
}

fn (g &Parser) expected_enum_shorthand_expression() ?string {
	if !g.selfhost || g.last_expression_type != '' || g.last_expression.len != 2
		|| g.last_expression[0].tok != .dot || g.last_expression[1].tok != .name
		|| g.declared_kinds[g.semantic_type_key(g.return_type)] != .enum_ {
		return none
	}
	return '${g.return_type.trim_right('*')}__${g.last_expression[1].lit}'
}
