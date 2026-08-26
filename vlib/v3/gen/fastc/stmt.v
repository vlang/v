module fastc

import v3.scanner
import v3.token

fn (mut g Parser) parse_block_body() !bool {
	outer_locals := g.locals.clone()
	outer_statement_reachable := g.statement_reachable
	deferred_line_start := g.deferred_lines.len
	deferred_block_start := g.deferred_block_starts.len
	mut terminates := false
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished block')
		}
		g.statement_reachable = outer_statement_reachable && !terminates
		statement_terminates := g.parse_statement()!
		if statement_terminates {
			terminates = true
			if g.selfhost {
				for g.tok !in [.rcbr, .eof] {
					if g.tok == .lcbr {
						g.skip_balanced(.lcbr, .rcbr)!
					} else {
						g.next()
					}
				}
			}
		}
		g.skip_semicolons()
	}
	g.next()
	g.skip_semicolons()
	g.write_deferred_blocks_from(deferred_block_start)
	g.deferred_lines.trim(deferred_line_start)
	g.deferred_block_starts.trim(deferred_block_start)
	g.locals = outer_locals.clone()
	g.statement_reachable = outer_statement_reachable
	return terminates
}

fn (mut g Parser) parse_statement() !bool {
	if g.defer_depth > 0 {
		match g.tok {
			.key_return {
				return g.unsupported('`return` not allowed inside a `defer` block')
			}
			.key_break, .key_continue {
				return g.unsupported('`${g.tok.str()}` is not allowed in defer statements')
			}
			.key_goto {
				return g.unsupported('goto is not allowed in defer statements')
			}
			.key_defer {
				return g.unsupported('`defer` blocks cannot be nested')
			}
			else {}
		}
	}
	return match g.tok {
		.dollar {
			g.parse_comptime_if_statement()!
		}
		.key_if {
			g.parse_if()!
		}
		.key_for {
			g.parse_for()!
		}
		.key_match {
			g.parse_match_statement()!
		}
		.key_select {
			g.parse_select_statement()!
		}
		.key_return {
			g.parse_return()!
		}
		.key_break {
			g.next()
			g.consume_statement_end()
			if g.loop_defer_block_starts.len == 0 {
				return g.unsupported('`break` outside a loop')
			}
			if g.statement_reachable && g.loop_has_breaks.len > 0 {
				g.loop_has_breaks[g.loop_has_breaks.len - 1] = true
			}
			g.write_deferred_blocks_from(g.loop_defer_block_starts.last())
			g.write_line('break;')
			true
		}
		.key_continue {
			g.next()
			g.consume_statement_end()
			if g.loop_defer_block_starts.len == 0 {
				return g.unsupported('`continue` outside a loop')
			}
			g.write_deferred_blocks_from(g.loop_defer_block_starts.last())
			g.write_line('continue;')
			true
		}
		.key_goto {
			g.next()
			if g.tok != .name {
				return g.unsupported('goto without a label')
			}
			label := fastc_c_identifier(g.lit)
			g.next()
			g.consume_statement_end()
			g.write_line('goto ${label};')
			false
		}
		.key_defer {
			g.parse_defer()!
			false
		}
		.key_mut {
			g.parse_mutable_declaration()!
			false
		}
		.key_unsafe {
			g.next()
			g.expect(.lcbr)!
			g.unsafe_depth += 1
			terminates := g.parse_block_body()!
			g.unsafe_depth -= 1
			terminates
		}
		else {
			g.parse_simple_statement()!
			false
		}
	}
}

fn (mut g Parser) parse_select_statement() !bool {
	return g.unsupported('select statements')
}

fn (g &Parser) open_block_contains_select_statement() bool {
	if g.tok != .lcbr {
		return false
	}
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut depth := 1
	mut previous := token.Token.lcbr
	mut tok := lookahead.scan()
	for depth > 0 && tok != .eof {
		if tok == .key_select && previous != .dot {
			return true
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
		}
		previous = tok
		tok = lookahead.scan()
	}
	return false
}

fn (mut g Parser) parse_defer() ! {
	g.next()
	g.expect(.lcbr)!
	previous_capture := g.capturing_defer
	previous_lines := g.captured_defer_lines.clone()
	g.capturing_defer = true
	g.defer_depth++
	g.captured_defer_lines = []string{}
	_ = g.parse_block_body()!
	block := g.captured_defer_lines.clone()
	g.defer_depth--
	g.capturing_defer = previous_capture
	g.captured_defer_lines = previous_lines.clone()
	g.deferred_block_starts << g.deferred_lines.len
	for line in block {
		g.deferred_lines << line
	}
}

fn (mut g Parser) write_deferred_blocks_from(first int) {
	for block_index := g.deferred_block_starts.len - 1; block_index >= first; block_index-- {
		line_start := g.deferred_block_starts[block_index]
		line_end := if block_index + 1 < g.deferred_block_starts.len {
			g.deferred_block_starts[block_index + 1]
		} else {
			g.deferred_lines.len
		}
		for line_index in line_start .. line_end {
			g.out.writeln(g.deferred_lines[line_index])
		}
	}
}

fn (mut g Parser) write_all_deferred_scopes() {
	if g.deferred_block_starts.len > 0 {
		g.write_deferred_blocks_from(0)
	}
}

fn (g &Parser) deferred_scopes_source() string {
	mut lines := []string{}
	for block_index := g.deferred_block_starts.len - 1; block_index >= 0; block_index-- {
		line_start := g.deferred_block_starts[block_index]
		line_end := if block_index + 1 < g.deferred_block_starts.len {
			g.deferred_block_starts[block_index + 1]
		} else {
			g.deferred_lines.len
		}
		for line_index in line_start .. line_end {
			lines << g.deferred_lines[line_index]
		}
	}
	return lines.join(' ')
}

fn (mut g Parser) parse_loop_block_body() !FastcLoopBlockResult {
	g.loop_defer_block_starts << g.deferred_block_starts.len
	g.loop_has_breaks << false
	terminates := g.parse_block_body()!
	has_reachable_break := g.loop_has_breaks.last()
	g.loop_has_breaks.delete_last()
	g.loop_defer_block_starts.delete_last()
	return FastcLoopBlockResult{
		terminates:          terminates
		has_reachable_break: has_reachable_break
	}
}

fn (mut g Parser) parse_match_statement() !bool {
	g.expect(.key_match)!
	subject := g.read_expression([token.Token.lcbr])!
	subject_type := fastc_normalize_inferred_type(g.last_expression_type)
	if subject == '' || subject_type == '' {
		return g.unsupported('unverifiable match subject')
	}
	g.expect(.lcbr)!
	subject_name := g.temporary_name('match')
	g.write_line('__typeof__((${subject})) ${subject_name} = (${subject});')
	is_string := g.underlying_alias_type(subject_type).trim_right('*') == 'string'
	mut branch_index := 0
	mut all_terminate := true
	mut has_else := false
	mut handled_cases := map[string]bool{}
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished match statement')
		}
		is_else := g.tok == .key_else
		has_else = has_else || is_else
		mut values := []string{}
		if is_else {
			g.next()
		} else {
			for {
				mut value := ''
				mut value_tokens := []FastcExpressionToken{}
				if g.tok == .dot {
					g.next()
					if g.tok != .name {
						return g.unsupported('match enum value')
					}
					value = '${subject_type.trim_right('*')}__${g.lit}'
					g.next()
				} else {
					value = g.read_expression([token.Token.comma, token.Token.lcbr])!
					if value == '' {
						return g.unsupported('empty match branch value')
					}
					value_tokens = g.last_expression.clone()
				}
				case_key := g.normalized_match_case_key(value_tokens, value)
				if case_key in handled_cases {
					return g.unsupported('duplicate match case `${value}`')
				}
				handled_cases[case_key] = true
				values << value
				if g.tok != .comma {
					break
				}
				g.next()
			}
		}
		g.expect(.lcbr)!
		if is_else {
			g.write_line('else {')
		} else {
			mut conditions := []string{}
			for value in values {
				if is_string {
					conditions << 'builtin__string_eq(${subject_name}, ${value})'
				} else {
					conditions << '((${subject_name}) == (${value}))'
				}
			}
			prefix := if branch_index == 0 { 'if' } else { 'else if' }
			g.write_line('${prefix} (${conditions.join(' || ')}) {')
		}
		g.indent++
		terminates := g.parse_block_body()!
		if !terminates {
			all_terminate = false
		}
		g.indent--
		g.write_line('}')
		branch_index++
	}
	g.next()
	g.skip_semicolons()
	return has_else && all_terminate
}

fn (mut g Parser) parse_return() !bool {
	g.next()
	if g.tok == .semicolon || g.tok == .rcbr {
		g.consume_statement_end()
		g.write_all_deferred_scopes()
		g.write_line(if g.in_main {
			'return 0;'
		} else if g.return_type == 'Option' {
			'return (Option){0};'
		} else {
			'return;'
		})
		return true
	}
	if g.selfhost && (g.return_type.trim_right('*') == 'MultiReturn'
		|| (g.return_type == 'Option' && g.option_return_type == 'MultiReturn')) {
		mut values := []string{}
		mut value_types := []string{}
		for {
			value :=
				g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			if value == '' {
				return g.unsupported('empty multi-return value')
			}
			values << value
			value_types << fastc_normalize_inferred_type(g.last_expression_type)
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		mut evaluated_values := []string{cap: values.len}
		for value in values {
			if g.deferred_block_starts.len == 0 {
				evaluated_values << value
				continue
			}
			temporary := g.temporary_name('return')
			g.write_line('__typeof__((${value})) ${temporary} = (${value});')
			evaluated_values << temporary
		}
		g.write_all_deferred_scopes()
		if g.return_type == 'Option' && values.len == 1 && value_types[0] == 'Option' {
			g.write_line('return ${evaluated_values[0]};')
			return true
		}
		if g.return_type == 'Option' && values.len == 1
			&& value_types[0].trim_right('*') == 'IError' {
			g.write_line('return (Option){.err=${evaluated_values[0]}, .state=1};')
			return true
		}
		multi_value := if values.len == 1 && value_types[0] == 'MultiReturn' {
			evaluated_values[0]
		} else {
			mut packed_values := []string{cap: values.len}
			for value in evaluated_values {
				packed_values << 'V_FASTC_MULTI_VALUE(${value})'
			}
			'(MultiReturn){.values={${packed_values.join(', ')}}}'
		}
		if g.return_type == 'Option' {
			g.write_line('return (Option){.data=v_fastc_interface_box(&${multi_value}, sizeof(MultiReturn)), .state=0};')
		} else {
			g.write_line('return ${multi_value};')
		}
		return true
	}
	previous_expected_type := g.expected_expression_type
	if g.selfhost {
		g.expected_expression_type = g.return_type
	}
	mut expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.expected_expression_type = previous_expected_type
	mut actual_type := g.last_expression_type
	if g.selfhost && g.return_type == '' {
		g.consume_statement_end()
		g.write_return_expression(expression)
		return true
	}
	if g.selfhost && actual_type == '' && g.last_expression.len == 2
		&& g.last_expression[0].tok == .dot && g.last_expression[1].tok == .name
		&& g.declared_kinds[g.semantic_type_key(g.return_type)] == .enum_ {
		expression = '${g.return_type.trim_right('*')}__${g.last_expression[1].lit}'
		actual_type = g.return_type
	}
	if g.selfhost && g.return_type !in ['Option', 'MultiReturn']
		&& g.declared_kinds[g.semantic_type_key(g.return_type)] != .interface_
		&& !fastc_types_share_lowering_representation(actual_type, g.return_type)
		&& !g.selfhost_types_share_lowering_representation(actual_type, g.return_type) {
		actual_type = g.return_type
	}
	if g.selfhost && g.declared_kinds[g.semantic_type_key(g.return_type)] == .interface_
		&& g.declared_kinds[g.semantic_type_key(actual_type)] != .interface_ {
		expression = g.interface_value_expression(g.return_type, actual_type, expression)
		actual_type = g.return_type
	}
	if g.selfhost && g.return_type == 'Option' && actual_type.trim_right('*') == 'IError' {
		expression = '(Option){.err=${expression}, .state=1}'
		actual_type = 'Option'
	} else if g.selfhost && g.return_type == 'Option' && actual_type != 'Option' {
		actual_base := fastc_normalize_inferred_type(actual_type)
		expression = '(Option){.data=${fastc_box_expression(actual_base, expression)}, .state=0}'
		actual_type = 'Option'
	}
	g.consume_statement_end()
	g.write_return_expression(expression)
	return true
}

fn (mut g Parser) write_return_expression(expression string) {
	if g.deferred_block_starts.len == 0 {
		g.write_line('return ${expression};')
		return
	}
	temporary := g.temporary_name('return')
	g.write_line('__typeof__((${expression})) ${temporary} = (${expression});')
	g.write_all_deferred_scopes()
	g.write_line('return ${temporary};')
}

fn (g &Parser) interface_value_expression(interface_type string, actual_type string, expression string) string {
	actual_base := actual_type.trim_right('*')
	actual_key := g.semantic_type_key(actual_type)
	object := if fastc_is_pointer_type(actual_type) {
		'(void*)(${expression})'
	} else {
		fastc_box_expression(actual_base, expression)
	}
	return '(${interface_type}){._object=${object}, ._typ=__v_typeid_${fastc_c_declared_type_name(actual_key)}, ._methods=NULL}'
}

fn (mut g Parser) parse_mutable_declaration() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('mutable declaration')
	}
	name := g.lit
	g.next()
	if g.selfhost && g.tok == .comma {
		g.parse_parallel_assignment([name], true, true)!
		return
	}
	if g.tok != .decl_assign {
		return g.unsupported('`mut` statement without `:=`')
	}
	g.parse_declaration_after_name(name, true)!
}

fn (mut g Parser) parse_simple_statement() ! {
	if g.tok == .key_assert {
		return g.unsupported('assert statements')
	}
	if g.tok == .name {
		name := g.lit
		global_key := fastc_global_key(g.module_name, name)
		is_global := global_key in g.globals
		statement_local := g.locals[name] or { FastcLocal{} }
		is_known_local := name in g.locals
		c_target := if is_global {
			g.globals[global_key]
		} else if local := g.locals[name] {
			if local.is_reference {
				'(*${fastc_c_identifier(name)})'
			} else {
				fastc_c_identifier(name)
			}
		} else {
			fastc_c_identifier(name)
		}
		g.next()
		if g.selfhost && g.tok == .colon {
			g.next()
			g.skip_semicolons()
			g.write_line('${fastc_c_identifier(name)}:')
			return
		}
		if g.selfhost && g.tok == .comma {
			g.parse_parallel_assignment([name], false, false)!
			return
		}
		if g.tok == .decl_assign {
			g.parse_declaration_after_name(name, false)!
			return
		}
		if g.selfhost && g.tok == .left_shift {
			local := g.locals[name] or { return g.unsupported('append to unknown name `${name}`') }
			if !local.is_mut {
				return g.unsupported('append to immutable name `${name}`')
			}
			_ := g.array_element_type(local.typ) or {
				return g.unsupported('append to non-array `${name}` of type `${local.typ}`')
			}
			g.next()
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			value_type := fastc_normalize_inferred_type(g.last_expression_type)
			is_array_append := value_type == local.typ
			g.consume_statement_end()
			c_name := fastc_c_identifier(name)
			array_target := if local.typ.ends_with('*') {
				'(array *)${c_name}'
			} else {
				'(array *)&${c_name}'
			}
			value_name := g.temporary_name('push_value')
			g.write_line('__typeof__((${value})) ${value_name} = (${value});')
			if is_array_append {
				g.write_line('builtin__array_push_many(${array_target}, ${value_name}.data, ${value_name}.len);')
			} else {
				g.write_line('builtin__array_push(${array_target}, &${value_name});')
			}
			return
		}
		if !g.selfhost && (g.tok.is_assignment() || g.tok in [.inc, .dec]) && !is_global
			&& (!is_known_local || !statement_local.is_mut) {
			return g.unsupported('mutation of immutable or unknown name `${name}`')
		}
		g.validate_expression_name(name, .unknown)!
		if g.tok.is_assignment() {
			if !g.selfhost
				&& g.tok in [.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
				return g.unsupported('shift expressions')
			}
			if !g.selfhost && g.tok in [.div_assign, .mod_assign] {
				return g.unsupported('division or modulo expressions')
			}
			operator := g.tok
			expected_type := if is_global {
				g.global_types[global_key]
			} else if local := g.locals[name] {
				if local.is_reference { local.typ.trim_right('*') } else { local.typ }
			} else {
				''
			}
			g.next()
			previous_expected_type := g.expected_expression_type
			if g.selfhost && expected_type != '' {
				g.expected_expression_type = expected_type
			}
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			g.expected_expression_type = previous_expected_type
			if value.len == 0 {
				return g.unsupported('empty assignment to `${name}`')
			}
			if g.selfhost && name == '_' && operator == .assign {
				g.consume_statement_end()
				g.write_line('(void)(${value});')
				return
			}
			mut actual_type := g.last_expression_type
			if g.selfhost && actual_type == '' {
				actual_type = expected_type
			}
			mut resolved_expected_type := if g.selfhost && expected_type == '' {
				actual_type
			} else {
				expected_type
			}
			if g.selfhost && resolved_expected_type == 'int'
				&& !fastc_is_numeric_expression_type(actual_type) && name in g.locals {
				resolved_expected_type = actual_type
				g.locals[name] = FastcLocal{
					is_mut:       statement_local.is_mut
					is_reference: statement_local.is_reference
					typ:          actual_type
				}
			}
			expected_layout_type := g.underlying_alias_type(resolved_expected_type)
			actual_layout_type := g.underlying_alias_type(actual_type)
			if operator == .plus_assign && expected_layout_type == 'string'
				&& actual_layout_type == 'string' {
				g.consume_statement_end()
				concatenation := if g.selfhost {
					'builtin__string_plus(${c_target},${value})'
				} else {
					'builtin__string_plus_many(2, (string[]){${c_target},${value}})'
				}
				g.write_line('${c_target}=${concatenation};')
				return
			}
			g.consume_statement_end()
			if operator == .right_shift_unsigned_assign {
				shift := g.render_unsigned_right_shift_assignment(c_target, value,
					resolved_expected_type) or {
					return g.unsupported('unsigned right shift assignment on type `${resolved_expected_type}`')
				}
				g.write_line('${shift};')
				return
			}
			g.write_line('${c_target}${operator.str()}${value};')
			return
		}
		expression := g.read_statement_expression_with_prefix(name, [token.Token.comma,
			token.Token.semicolon, token.Token.rcbr])!
		if g.selfhost && g.tok == .comma {
			g.parse_parallel_expression_assignment(expression, g.last_expression.clone(),
				g.last_expression_type)!
			return
		}
		if !g.last_expression_is_statement() {
			return g.unsupported('value-only expression statement')
		}
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	expression := g.read_statement_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('statement `${g.token_source()}`')
	}
	if g.last_expression.len == 0 && g.last_expression_type.starts_with(fastc_thread_type_prefix) {
		// A discarded handle can never be joined, so the packed arguments and
		// pthread join state of every completed spawn would leak.
		return g.unsupported('statement-form `spawn` that discards its thread handle; assign the handle and call `.wait()`')
	}
	if g.selfhost && g.last_expression_is_statement() {
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	return g.unsupported('value-only expression statement')
}

fn (mut g Parser) parse_parallel_assignment(initial_names []string, initial_mut bool, force_declaration bool) ! {
	mut names := initial_names.clone()
	mut mutability := []bool{len: initial_names.len, init: initial_mut}
	for g.tok == .comma {
		g.next()
		mut is_mut := false
		if g.tok == .key_mut {
			is_mut = true
			g.next()
		}
		if g.tok != .name {
			return g.unsupported('parallel assignment target')
		}
		names << g.lit
		mutability << is_mut
		g.next()
	}
	is_declaration := force_declaration || g.tok == .decl_assign
	if g.tok !in [.decl_assign, .assign] {
		return g.unsupported('parallel assignment operator `${g.token_source()}`')
	}
	g.next()
	g.last_multi_return_types = []string{}
	mut values := []string{}
	mut value_types := []string{}
	for {
		item := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		if item == '' {
			return g.unsupported('empty parallel assignment')
		}
		values << item
		value_types << g.last_expression_type
		if g.tok != .comma {
			break
		}
		g.next()
	}
	value := values[0]
	if value == '' {
		return g.unsupported('empty parallel assignment')
	}
	g.consume_statement_end()
	if values.len > 1 {
		if values.len != names.len {
			return g.unsupported('parallel assignment with ${names.len} targets and ${values.len} values')
		}
		assignment_targets := if is_declaration {
			[]FastcRenderedExpression{}
		} else {
			g.validate_parallel_assignment_targets(names)!
		}
		mut temporaries := []string{cap: values.len}
		for item in values {
			temporary := g.temporary_name('parallel')
			g.write_line('__typeof__((${item})) ${temporary} = (${item});')
			temporaries << temporary
		}
		if is_declaration {
			for i, name in names {
				if name == '_' {
					continue
				}
				value_type := if value_types[i] == '' {
					'int'
				} else {
					fastc_normalize_inferred_type(value_types[i])
				}
				g.write_line('${value_type} ${fastc_c_identifier(name)} = ${temporaries[i]};')
				g.locals[name] = FastcLocal{
					is_mut: mutability[i]
					typ:    value_type
				}
			}
		} else {
			for i, name in names {
				if name == '_' {
					continue
				}
				g.write_line('${assignment_targets[i].source} = ${temporaries[i]};')
			}
		}
		return
	}
	mut component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len == 0 {
		component_types = g.last_multi_return_types.clone()
	}
	assignment_targets := if is_declaration {
		[]FastcRenderedExpression{}
	} else {
		g.validate_parallel_assignment_targets(names)!
	}
	temporary := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${temporary} = (${value});')
	for i, name in names {
		if name == '_' {
			continue
		}
		if is_declaration {
			component_type := if i < component_types.len { component_types[i] } else { 'usize' }
			c_name := fastc_c_identifier(name)
			g.write_line('${component_type} ${c_name} = (${component_type}){0};')
			g.write_line('memcpy(&${c_name}, ${temporary}.values[${i}].data, sizeof(${c_name}));')
			g.locals[name] = FastcLocal{
				is_mut: mutability[i]
				typ:    component_type
			}
		} else {
			c_name := assignment_targets[i].source
			g.write_line('memcpy(&${c_name}, ${temporary}.values[${i}].data, sizeof(${c_name}));')
		}
	}
}

fn (g &Parser) validate_parallel_assignment_targets(names []string) ![]FastcRenderedExpression {
	mut targets := []FastcRenderedExpression{cap: names.len}
	for name in names {
		if name == '_' {
			targets << FastcRenderedExpression{}
			continue
		}
		mut target := FastcRenderedExpression{}
		if local := g.locals[name] {
			if !local.is_mut {
				return g.unsupported('parallel assignment to immutable name `${name}`')
			}
			target = FastcRenderedExpression{
				source: if local.is_reference {
					'(*${fastc_c_identifier(name)})'
				} else {
					fastc_c_identifier(name)
				}
				typ:    if local.is_reference { local.typ.trim_right('*') } else { local.typ }
			}
		} else {
			global_key := fastc_global_key(g.module_name, name)
			global_name := g.globals[global_key] or {
				return g.unsupported('parallel assignment to unknown name `${name}`')
			}
			target = FastcRenderedExpression{
				source: global_name
				typ:    g.global_types[global_key]
			}
		}
		targets << target
	}
	return targets
}

fn (mut g Parser) parse_parallel_expression_assignment(first_source string, first_tokens []FastcExpressionToken, first_type string) ! {
	mut targets := []FastcRenderedExpression{}
	targets << g.validate_parallel_expression_assignment_target(first_source, first_tokens,
		first_type)!
	for g.tok == .comma {
		g.next()
		target_source := g.read_expression([token.Token.comma, token.Token.assign])!
		if target_source == '' {
			return g.unsupported('empty parallel assignment target')
		}
		targets << g.validate_parallel_expression_assignment_target(target_source,
			g.last_expression.clone(), g.last_expression_type)!
	}
	if g.tok != .assign {
		return g.unsupported('parallel assignment operator `${g.token_source()}`')
	}
	g.next()
	g.last_multi_return_types = []string{}
	mut values := []string{}
	for {
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		if value == '' {
			return g.unsupported('empty parallel assignment')
		}
		values << value
		if g.tok != .comma {
			break
		}
		g.next()
	}
	g.consume_statement_end()
	if values.len > 1 {
		if values.len != targets.len {
			return g.unsupported('parallel assignment with ${targets.len} targets and ${values.len} values')
		}
		mut temporaries := []string{cap: values.len}
		for value in values {
			temporary := g.temporary_name('parallel')
			g.write_line('__typeof__((${value})) ${temporary} = (${value});')
			temporaries << temporary
		}
		for i, target in targets {
			if target.source != '' {
				g.write_line('${target.source} = ${temporaries[i]};')
			}
		}
		return
	}
	temporary := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${temporary} = (${values[0]});')
	for i, target in targets {
		if target.source == '' {
			continue
		}
		g.write_line('memcpy(&${target.source}, ${temporary}.values[${i}].data, sizeof(${target.source}));')
	}
}

fn (g &Parser) validate_parallel_expression_assignment_target(source string, tokens []FastcExpressionToken, typ string) !FastcRenderedExpression {
	if tokens.len == 1 && tokens[0].tok == .name {
		mut names := []string{}
		names << tokens[0].lit
		targets := g.validate_parallel_assignment_targets(names)!
		return targets[0]
	}
	mut mutation_tokens := tokens.clone()
	mutation_tokens << FastcExpressionToken{
		tok: .assign
		lit: '='
	}
	g.validate_expression_mutation_lvalue(mutation_tokens)!
	return FastcRenderedExpression{
		source: source
		typ:    typ
	}
}

fn (g &Parser) multi_return_types_for_expression(tokens []FastcExpressionToken) []string {
	expression_tokens := if tokens.len > 0 && tokens.last().tok == .not {
		tokens[..tokens.len - 1]
	} else {
		tokens
	}
	if expression_tokens.len < 3 {
		return []string{}
	}
	mut name_index := 0
	mut open_index := 1
	if expression_tokens.len >= 4 && expression_tokens[0].tok == .name
		&& expression_tokens[1].tok == .dot && expression_tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if expression_tokens[name_index].tok != .name || expression_tokens[open_index].tok != .lpar {
		return []string{}
	}
	close := fastc_matching_rpar(expression_tokens, open_index) or { return []string{} }
	if close != expression_tokens.len - 1 {
		return []string{}
	}
	function_key := if name_index == 2 && expression_tokens[0].lit !in g.imports
		&& expression_tokens[0].lit != 'C' {
		receiver_type := g.infer_expression_type(expression_tokens[..1]) or { return []string{} }
		g.method_function_key(receiver_type, expression_tokens[name_index].lit)
	} else {
		g.function_key_for_call(expression_tokens, name_index)
	}
	signature := g.functions[function_key] or { return []string{} }
	return signature.return_types.clone()
}

fn (g &Parser) option_value_type_for_expression(tokens []FastcExpressionToken) string {
	if tokens.len < 3 {
		return ''
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok != .name || tokens[open_index].tok != .lpar {
		return ''
	}
	close := fastc_matching_rpar(tokens, open_index) or { return '' }
	if close != tokens.len - 1 {
		return ''
	}
	function_key := if name_index == 2 && tokens[0].lit !in g.imports && tokens[0].lit != 'C' {
		receiver_type := g.infer_expression_type(tokens[..1]) or { return '' }
		g.method_function_key(receiver_type, tokens[name_index].lit)
	} else {
		g.function_key_for_call(tokens, name_index)
	}
	signature := g.functions[function_key] or { return '' }
	return signature.option_type
}

fn (g &Parser) last_expression_is_statement() bool {
	return g.expression_tokens_are_statement(g.last_expression)
}

fn (g &Parser) expression_tokens_are_statement(expression_tokens []FastcExpressionToken) bool {
	tokens := if g.selfhost && expression_tokens.len > 0 && expression_tokens.last().tok == .not {
		expression_tokens[..expression_tokens.len - 1]
	} else {
		expression_tokens
	}
	if g.selfhost {
		for item in tokens {
			if item.is_statement {
				return true
			}
		}
	}
	if g.selfhost && fastc_expression_tokens_contain_assignment_or_mutation(tokens) {
		return true
	}
	if g.selfhost && fastc_expression_tokens_contain(tokens, .left_shift) {
		return true
	}
	if g.selfhost && fastc_expression_tokens_contain_statement_method(tokens) {
		return true
	}
	if tokens.len >= 4 {
		for i in 2 .. tokens.len - 1 {
			if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
				continue
			}
			call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			if call_end != tokens.len - 1 {
				continue
			}
			receiver_start := fastc_method_receiver_start(tokens, i - 1)
			receiver_type := g.infer_expression_type(tokens[receiver_start..i - 1]) or { continue }
			if g.method_function_key(receiver_type, tokens[i].lit) in g.functions
				|| g.struct_member_type(receiver_type, tokens[i].lit) != '' {
				return true
			}
		}
	}
	if tokens.len == 2 && tokens[0].tok == .name && tokens[1].tok in [.inc, .dec] {
		return true
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name
		&& (tokens[0].lit in g.imports || (tokens[0].lit == 'C' && g.has_declared_c_function())) {
		name_index = 2
		open_index = 3
	}
	if tokens.len <= open_index + 1 || tokens[name_index].tok != .name
		|| tokens[open_index].tok != .lpar {
		return false
	}
	call_close := fastc_matching_rpar(tokens, open_index) or { return false }
	if call_close != tokens.len - 1 {
		return false
	}
	name := tokens[name_index].lit
	function_key := g.function_key_for_call(tokens, name_index)
	return function_key in g.functions || (name_index == 0 && name in ['print', 'println'])
}

fn (mut g Parser) parse_declaration_after_name(name string, is_mut bool) ! {
	if !g.selfhost && name in g.locals {
		return g.unsupported('redeclaration of `${name}`')
	}
	g.next()
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('empty declaration')
	}
	g.consume_statement_end()
	// GNU typeof is unevaluated and is supported by bundled TinyCC. It lets the
	// direct path preserve V's `:=` without running any inference or type checker.
	c_name := fastc_c_identifier(name)
	if expression.starts_with('"') {
		// C's typeof preserves a literal's array type instead of applying the usual
		// pointer decay. The spelling alone is enough to lower this case.
		g.write_line('string ${c_name} = (${expression});')
	} else {
		g.write_line('__typeof__((${expression})) ${c_name} = (${expression});')
	}
	g.locals[name] = FastcLocal{
		is_mut: is_mut
		typ:    if g.selfhost && g.last_expression_type == '' {
			'int'
		} else {
			fastc_normalize_inferred_type(g.last_expression_type)
		}
	}
}

fn fastc_normalize_inferred_type(typ string) string {
	return match typ {
		'integer literal', 'negative integer literal' { 'int' }
		'float literal' { 'f64' }
		'nil' { 'voidptr' }
		else { typ }
	}
}

fn (mut g Parser) consume_statement_end() {
	if g.tok == .semicolon {
		g.next()
	}
}

fn (g &Parser) token_source() string {
	if g.lit.len > 0 {
		return g.lit
	}
	return g.tok.str()
}

fn (mut g Parser) write_line(line string) {
	if g.capturing_defer {
		indented_line := '\t'.repeat(g.indent) + line
		g.captured_defer_lines << indented_line
		return
	}
	for _ in 0 .. g.indent {
		g.out.write_u8(`\t`)
	}
	g.out.writeln(line)
}
