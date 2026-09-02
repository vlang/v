module fastc

import v3.scanner
import v3.token

fn (g &Parser) optional_function_field_for_expression(tokens []FastcExpressionToken) ?FastcStructField {
	if tokens.len < 3 || tokens[tokens.len - 2].tok != .dot || tokens.last().tok != .name {
		return none
	}
	receiver_type := g.infer_expression_type(tokens[..tokens.len - 2]) or { return none }
	field := g.struct_field_metadata(receiver_type, tokens.last().lit) or { return none }
	if !field.is_optional_function {
		return none
	}
	return field
}

fn (g &Parser) or_block_has_statements() bool {
	if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
		return false
	}
	if g.tok == .name && g.lit == 'panic' {
		return true
	}
	if g.tok in [.dollar, .key_return, .key_if, .key_for, .key_match, .key_mut, .key_defer,
		.key_break, .key_continue] {
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
	// `if a, b := opt_fn() { ... }` unwraps an option whose value is a multi-return
	// tuple and binds each component. The comma would otherwise trip the condition
	// reader as a parallel assignment, so detect the `name (, name)+ :=` prefix here.
	if g.tok == .name || (g.tok == .key_shared && g.shared_token_is_identifier(.key_if)) {
		mut probe := g.s
		mut guard_names := [g.lit]
		mut is_multi_guard := false
		for {
			t := probe.scan()
			if t == .comma {
				if probe.scan() !in [.name, .key_shared] {
					break
				}
				guard_names << probe.lit
			} else if t == .decl_assign {
				is_multi_guard = guard_names.len >= 2
				break
			} else {
				break
			}
		}
		if is_multi_guard {
			return g.parse_if_multi_return_guard(guard_names)
		}
	}
	mut condition := g.read_condition_expression([token.Token.semicolon, token.Token.lcbr])!
	if condition.len == 0 {
		return g.unsupported('empty if condition')
	}
	// `if local is Variant { ... }` on a boxed sum-type/interface local smart-casts
	// `local` to the concrete variant inside the then-branch, so its fields and
	// methods resolve. Only the exact `local is Variant` form (no `&&`/`||`) binds.
	cond_tokens := g.last_expression
	mut smartcast_name := ''
	mut smartcast_type := ''
	mut smartcast_tmp := ''
	mut smartcast_boxed_type := ''
	if cond_tokens.len >= 3 && cond_tokens[0].tok == .name && cond_tokens[1].tok == .key_is {
		if local := g.locals[cond_tokens[0].lit] {
			boxed := fastc_normalize_inferred_type(local.typ)
			if g.is_boxed_type(boxed) {
				if cond_tokens.len == 3 && cond_tokens[2].tok == .name {
					if variant_key := g.resolve_declared_type_key(cond_tokens[2].lit)
					{
						smartcast_name = cond_tokens[0].lit
						smartcast_type = fastc_c_declared_type_name(variant_key)
						smartcast_boxed_type = local.typ
						smartcast_tmp = g.temporary_name('smartcast_subject')
					}
				} else if resolved_target := g.type_from_expression_tokens(cond_tokens[2..]) {
					// Composite variant target (`x is []string` / `x is map[K]V`): the
					// type spans several tokens and lowers to a generated composite type.
					target := fastc_normalize_inferred_type(resolved_target)
					if target.starts_with('Array_') || target.starts_with('Map_') {
						fastc_register_composite_type(target, mut g.composite_types)
						smartcast_name = cond_tokens[0].lit
						smartcast_type = target
						smartcast_boxed_type = local.typ
						smartcast_tmp = g.temporary_name('smartcast_subject')
					}
				}
			}
		}
	}
	// A boxed member (`if mut holder.writer is File`) needs the same concrete view as
	// a boxed local, but its qualified source spelling cannot be shadowed. Keep a
	// branch-scoped member-path rewrite instead, backed by a pointer to the boxed object.
	mut member_smartcast_path := ''
	mut member_smartcast_type := ''
	mut member_smartcast_boxed_type := ''
	mut member_smartcast_source := ''
	mut member_smartcast_boxed_tmp := ''
	mut member_smartcast_tmp := ''
	mut is_index := -1
	for i, item in cond_tokens {
		if item.tok == .key_is {
			is_index = i
			break
		}
	}
	mut member_start := 0
	if cond_tokens.len > 0 && cond_tokens[0].tok in [.key_mut, .amp] {
		member_start = 1
	}
	if is_index > member_start + 1 && is_index + 1 < cond_tokens.len {
		left_tokens := cond_tokens[member_start..is_index]
		mut is_member_chain := left_tokens.len >= 3 && left_tokens[0].tok == .name
		mut path := if is_member_chain { left_tokens[0].lit } else { '' }
		mut index := 1
		for is_member_chain && index < left_tokens.len {
			if index + 1 >= left_tokens.len || left_tokens[index].tok != .dot
				|| left_tokens[index + 1].tok != .name {
				is_member_chain = false
				break
			}
			path += '.' + left_tokens[index + 1].lit
			index += 2
		}
		if is_member_chain {
			left_type := fastc_normalize_inferred_type(g.infer_expression_type(left_tokens) or {
				''
			})
			target_type := g.type_from_expression_tokens(cond_tokens[is_index + 1..]) or { '' }
			if left_type != '' && target_type != '' && g.is_boxed_type(left_type) {
				left_source := g.render_member_receiver(left_tokens) or { '' }
				if left_source != '' {
					member_smartcast_path = path
					member_smartcast_type = fastc_normalize_inferred_type(target_type)
					member_smartcast_boxed_type = left_type
					member_smartcast_source = left_source
					member_smartcast_boxed_tmp = g.temporary_name('smartcast_subject')
					member_smartcast_tmp = g.temporary_name('smartcast_member')
					condition = '((${member_smartcast_boxed_tmp}._typ) == __v_typeid_${member_smartcast_type})'
				}
			}
		}
	}
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	mut guard_is_mut := false
	mut guard_function := FastcStructField{}
	mut guard_function_source := ''
	mut guard_erased_generic := false
	// `if mut x := opt {` / `if mut x := map[k] {`: the leading `mut` (which renders to
	// a `&` token here) shifts the name one token to the right. `& <name> :=` at the
	// start of an if condition can only come from such a `mut` guard.
	guard_name_index := if g.selfhost && g.last_expression.len >= 1
		&& g.last_expression[0].tok in [token.Token.key_mut, token.Token.amp] {
		1
	} else {
		0
	}
	if g.selfhost && g.last_expression.len >= guard_name_index + 3
		&& g.last_expression[guard_name_index].tok == .name
		&& g.last_expression[guard_name_index + 1].tok == .decl_assign {
		right_tokens := g.last_expression[guard_name_index + 2..]
		if function_field := g.optional_function_field_for_expression(right_tokens) {
			guard_name = g.last_expression[guard_name_index].lit
			guard_type = 'voidptr'
			guard_is_mut = guard_name_index == 1
			guard_function = function_field
			guard_function_source = g.render_member_receiver(right_tokens) or { '' }
			if guard_function_source != '' {
				condition = '${guard_function_source} != NULL'
				g.last_expression_type = 'bool'
			}
		} else if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[guard_name_index].lit
			guard_type = map_lookup.typ
			guard_is_mut = guard_name_index == 1
			guard_option = g.temporary_name('if_guard')
			g.write_line('Option ${guard_option} = (${map_lookup.source});')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[guard_name_index].lit
				guard_type = option_type
				guard_is_mut = guard_name_index == 1
				guard_option = g.temporary_name('if_guard')
				guard_erased_generic = g.erased_generic_option_value_type_for_expression(right_tokens) != none
				right_source := condition.all_after(':=').trim_space()
				g.write_line('Option ${guard_option} = (${right_source});')
				condition = '${guard_option}.state == 0'
				g.last_expression_type = 'bool'
			}
		}
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	if smartcast_name != '' {
		// Copy the boxed value out before the branch so the shadowing cast below
		// does not reference its own uninitialized declaration.
		g.write_line('${smartcast_boxed_type} ${smartcast_tmp} = ${fastc_c_identifier(smartcast_name)};')
	}
	if member_smartcast_path != '' {
		g.write_line('${member_smartcast_boxed_type} ${member_smartcast_boxed_tmp} = ${member_smartcast_source};')
	}
	g.write_line('if (${condition}) {')
	g.indent++
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		if guard_function_source != '' {
			parameters := if guard_function.fn_parameter_types.len == 0 {
				'void'
			} else {
				guard_function.fn_parameter_types.join(', ')
			}
			return_type := if guard_function.fn_return_type == '' {
				'void'
			} else {
				guard_function.fn_return_type
			}
			name := fastc_c_identifier(guard_name)
			g.write_line('${return_type} (*${name})(${parameters}) = (${return_type} (*)(${parameters}))(${guard_function_source});')
			g.locals[guard_name] = FastcLocal{
				is_mut:               guard_is_mut
				typ:                  'voidptr'
				fn_return_type:       return_type
				fn_option_value_type: guard_function.fn_option_value_type
			}
		} else if guard_erased_generic {
			name := fastc_c_identifier(guard_name)
			if guard_type.ends_with('*') {
				g.write_line('${guard_type} ${name} = *((${guard_type} *)${guard_option}.data);')
			} else {
				g.write_line('${guard_type} ${name} = **((${guard_type} **)${guard_option}.data);')
			}
			g.locals[guard_name] = FastcLocal{
				is_mut: guard_is_mut
				typ:    guard_type
			}
		} else {
			function_alias := g.functions[guard_type] or { FastcFunctionSignature{} }
			g.write_line('${guard_type} ${fastc_c_identifier(guard_name)} = *((${guard_type} *)${guard_option}.data);')
			g.locals[guard_name] = FastcLocal{
				is_mut:               guard_is_mut
				typ:                  guard_type
				fn_return_type:       function_alias.return_type
				fn_option_value_type: function_alias.option_type
			}
		}
	}
	previous_smartcast := g.locals[smartcast_name] or { FastcLocal{} }
	had_smartcast := smartcast_name in g.locals
	if smartcast_name != '' {
		g.write_line('${smartcast_type} ${fastc_c_identifier(smartcast_name)} = *((${smartcast_type} *)${smartcast_tmp}._object);')
		g.locals[smartcast_name] = FastcLocal{
			is_mut: previous_smartcast.is_mut
			typ:    smartcast_type
		}
	}
	previous_member_smartcast := g.member_smartcasts[member_smartcast_path] or {
		FastcMemberSmartcast{}
	}
	had_member_smartcast := member_smartcast_path in g.member_smartcasts
	if member_smartcast_path != '' {
		g.write_line('${member_smartcast_type} *${member_smartcast_tmp} = (${member_smartcast_type} *)${member_smartcast_boxed_tmp}._object;')
		g.member_smartcasts[member_smartcast_path] = FastcMemberSmartcast{
			typ:    member_smartcast_type + '*'
			source: member_smartcast_tmp
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
	if smartcast_name != '' {
		if had_smartcast {
			g.locals[smartcast_name] = previous_smartcast
		} else {
			g.locals.delete(smartcast_name)
		}
	}
	if member_smartcast_path != '' {
		if had_member_smartcast {
			g.member_smartcasts[member_smartcast_path] = previous_member_smartcast
		} else {
			g.member_smartcasts.delete(member_smartcast_path)
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
		previous_err := g.locals['err'] or { FastcLocal{} }
		had_err := 'err' in g.locals
		if guard_option != '' {
			g.write_line('IError err = ${guard_option}.err;')
			g.locals['err'] = FastcLocal{
				typ: 'IError'
			}
		}
		else_terminates := g.parse_if()!
		if guard_option != '' {
			if had_err {
				g.locals['err'] = previous_err
			} else {
				g.locals.delete('err')
			}
		}
		g.indent--
		g.write_line('}')
		return then_terminates && else_terminates
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	previous_err := g.locals['err'] or { FastcLocal{} }
	had_err := 'err' in g.locals
	if guard_option != '' {
		g.write_line('IError err = ${guard_option}.err;')
		g.locals['err'] = FastcLocal{
			typ: 'IError'
		}
	}
	else_terminates := g.parse_block_body()!
	if guard_option != '' {
		if had_err {
			g.locals['err'] = previous_err
		} else {
			g.locals.delete('err')
		}
	}
	g.indent--
	g.write_line('}')
	return then_terminates && else_terminates
}

// parse_if_multi_return_guard lowers `if a, b := opt_fn() { ... }`: the option's
// value is a multi-return tuple, so on success the boxed `MultiReturn` is copied out
// of the option and each component is bound as a local inside the then-branch.
fn (mut g Parser) parse_if_multi_return_guard(names []string) !bool {
	for g.tok != .decl_assign && g.tok != .eof {
		g.next()
	}
	g.expect(.decl_assign)!
	rhs := g.read_expression([token.Token.semicolon, token.Token.lcbr])!
	if rhs == '' {
		return g.unsupported('empty multi-return option guard')
	}
	component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len < names.len {
		return g.unsupported('multi-return option guard component types')
	}
	g.skip_semicolons()
	g.expect(.lcbr)!
	guard_option := g.temporary_name('if_guard')
	g.write_line('Option ${guard_option} = (${rhs});')
	g.write_line('if (${guard_option}.state == 0) {')
	g.indent++
	multi_return := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${multi_return} = *((MultiReturn *)${guard_option}.data);')
	mut previous_locals := []FastcLocal{cap: names.len}
	mut had_locals := []bool{cap: names.len}
	for i, name in names {
		existing := name in g.locals
		previous_locals << (g.locals[name] or { FastcLocal{} })
		had_locals << existing
		if name == '_' {
			continue
		}
		component_type := fastc_normalize_inferred_type(component_types[i])
		c_name := fastc_c_identifier(name)
		g.write_line('${component_type} ${c_name} = (${component_type}){0};')
		g.write_line('memcpy(&${c_name}, V_FASTC_MULTI_SOURCE(${multi_return}.values[${i}], sizeof(${c_name})), sizeof(${c_name}));')
		g.locals[name] = FastcLocal{
			typ: component_type
		}
	}
	then_terminates := g.parse_block_body()!
	for i, name in names {
		if had_locals[i] {
			g.locals[name] = previous_locals[i]
		} else {
			g.locals.delete(name)
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
	if g.selfhost && g.last_expression.len >= 3 && g.last_expression[0].tok == .name
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
	mut then_option_value_type := if then_type == 'Option' {
		if g.last_expression.len > 0 {
			g.option_value_type_for_expression(g.last_expression)
		} else {
			g.last_option_value_type
		}
	} else {
		''
	}
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
	mut else_option_value_type := ''
	if g.tok == .key_if {
		g.expected_expression_type = branch_expected_type
		else_expression = g.read_if_expression()!
		else_type = g.last_expression_type
		else_option_value_type = g.last_option_value_type
	} else {
		g.expect(.lcbr)!
		g.expected_expression_type = branch_expected_type
		else_expression = if g.tok == .key_return {
			g.read_return_expression_branch()!
		} else {
			g.read_block_expression_value()!
		}
		else_type = g.last_expression_type
		if else_type == 'Option' {
			else_option_value_type = if g.last_expression.len > 0 {
				g.option_value_type_for_expression(g.last_expression)
			} else {
				g.last_option_value_type
			}
		}
		if enum_expression := g.expected_enum_shorthand_expression() {
			else_expression = enum_expression
			else_type = g.return_type
		}
		g.skip_semicolons()
		g.expect(.rcbr)!
	}
	if g.selfhost && g.return_type == 'Option' && outer_expected_type != 'Option' {
		if then_type.trim_right('*') == 'IError' && else_type !in ['', 'IError'] {
			then_expression = '({ return (Option){.err=${then_expression}, .state=1}; (${fastc_normalize_inferred_type(else_type)}){0}; })'
			then_type = else_type
		} else if else_type.trim_right('*') == 'IError' && then_type !in ['', 'IError'] {
			else_expression = '({ return (Option){.err=${else_expression}, .state=1}; (${fastc_normalize_inferred_type(then_type)}){0}; })'
			else_type = then_type
		}
	}
	if g.selfhost && outer_expected_type == 'Option' {
		if then_type != 'Option' {
			then_option_value_type = fastc_normalize_inferred_type(then_type)
			then_expression = g.option_branch_expression(then_type, then_expression)
			then_type = 'Option'
		}
		if else_type != 'Option' {
			else_option_value_type = fastc_normalize_inferred_type(else_type)
			else_expression = g.option_branch_expression(else_type, else_expression)
			else_type = 'Option'
		}
	} else if g.selfhost && then_type == 'Option' && else_type !in ['', 'Option'] {
		else_option_value_type = fastc_normalize_inferred_type(else_type)
		else_base := fastc_normalize_inferred_type(else_type)
		else_expression = '(Option){.data=${fastc_box_expression(else_base, else_expression)}, .state=0}'
		else_type = 'Option'
	} else if g.selfhost && else_type == 'Option' && then_type !in ['', 'Option'] {
		then_option_value_type = fastc_normalize_inferred_type(then_type)
		then_base := fastc_normalize_inferred_type(then_type)
		then_expression = '(Option){.data=${fastc_box_expression(then_base, then_expression)}, .state=0}'
		then_type = 'Option'
	}
	if g.selfhost && then_type == '' && else_type != '' {
		resolved_type := fastc_normalize_inferred_type(else_type)
		then_expression = '({ (void)(${then_expression}); (${resolved_type}){0}; })'
		then_type = resolved_type
		else_type = resolved_type
	} else if g.selfhost && else_type == '' && then_type != '' {
		resolved_type := fastc_normalize_inferred_type(then_type)
		else_expression = '({ (void)(${else_expression}); (${resolved_type}){0}; })'
		then_type = resolved_type
		else_type = resolved_type
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
	g.last_option_value_type = if g.last_expression_type == 'Option' && then_option_value_type != ''
		&& then_option_value_type == else_option_value_type {
		then_option_value_type
	} else {
		''
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
			values << 'V_FASTC_MULTI_VALUE((${value}))'
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		g.last_expression_type = ''
		g.last_expression = []FastcExpressionToken{}
		return '({ return ${fastc_multi_return_literal(values)}; 0; })'
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
