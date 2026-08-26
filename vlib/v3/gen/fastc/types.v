module fastc

import v3.token

fn fastc_matching_rpar(tokens []FastcExpressionToken, open int) ?int {
	mut depth := 0
	for i in open .. tokens.len {
		match tokens[i].tok {
			.lpar {
				depth++
			}
			.rpar {
				depth--
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return none
}

fn fastc_method_receiver_start(tokens []FastcExpressionToken, dot int) int {
	if dot <= 0 || dot > tokens.len {
		return 0
	}
	mut parens := 0
	mut brackets := 0
	mut start := dot - 1
	for start >= 0 {
		tok := tokens[start].tok
		if tok == .rpar {
			parens++
		} else if tok == .rsbr {
			brackets++
		} else if tok == .lpar {
			if parens == 0 && brackets == 0 {
				return start + 1
			}
			parens--
		} else if tok == .lsbr {
			if brackets == 0 && parens == 0 {
				return start + 1
			}
			brackets--
		} else if parens == 0 && brackets == 0 && tok in [.amp, .and, .mul] && start + 2 < dot
			&& tokens[start + 1].tok == .name && tokens[start + 2].tok == .lpar
			&& fastc_token_is_prefix_operator(tokens, start) {
			return start
		} else if parens == 0 && brackets == 0 && (tok.is_assignment()
			|| tok in [.comma, .semicolon, .colon, .ellipsis, .plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift, .right_shift, .right_shift_unsigned, .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .not, .bit_not, .lcbr]) {
			return start + 1
		}
		start--
	}
	return 0
}

fn fastc_call_arguments(tokens []FastcExpressionToken, open int, close int) ![][]FastcExpressionToken {
	if open + 1 == close {
		return [][]FastcExpressionToken{}
	}
	mut call_args := [][]FastcExpressionToken{}
	mut start := open + 1
	mut paren_depth := 0
	mut bracket_depth := 0
	mut brace_depth := 0
	for i in open + 1 .. close {
		match tokens[i].tok {
			.lpar {
				paren_depth++
			}
			.rpar {
				paren_depth--
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				bracket_depth--
			}
			.lcbr {
				brace_depth++
			}
			.rcbr {
				brace_depth--
			}
			.comma {
				if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
					if start == i {
						return error('empty fastc function argument')
					}
					call_args << tokens[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if start == close {
		return error('empty fastc function argument')
	}
	call_args << tokens[start..close]
	return call_args
}

fn fastc_boolean_operator_precedence(tok token.Token) int {
	if tok == .logical_or {
		return 0
	}
	if tok == .and {
		return 1
	}
	if tok in [.eq, .ne, .gt, .lt, .ge, .le, .key_is, .not_is, .key_in, .not_in] {
		return 2
	}
	return -1
}

// fastc_lowest_precedence_operator_index returns the leftmost top-level
// boolean operator of the lowest available precedence class (`||`, then `&&`,
// then comparisons) in one pass. Equivalent to probing each class with a full
// scan, which previously tripled the per-inference-step token traffic.
@[direct_array_access]
fn fastc_lowest_precedence_operator_index(tokens []FastcExpressionToken, start int, end int) ?int {
	mut depth := 0
	mut first_and := -1
	mut first_comparison := -1
	for i in start .. end {
		tok := tokens[i].tok
		match tok {
			.lpar, .lsbr, .lcbr {
				depth++
				continue
			}
			.rpar, .rsbr, .rcbr {
				depth--
				continue
			}
			else {}
		}
		if depth != 0 || i == start || i + 1 >= end {
			continue
		}
		precedence := fastc_boolean_operator_precedence(tok)
		if precedence == 0 {
			// Nothing binds looser than the leftmost `||`.
			return i
		}
		if precedence == 1 {
			if first_and == -1 {
				first_and = i
			}
		} else if precedence == 2 {
			if first_comparison == -1 {
				first_comparison = i
			}
		}
	}
	if first_and != -1 {
		return first_and
	}
	if first_comparison != -1 {
		return first_comparison
	}
	return none
}

fn (g &Parser) infer_boolean_binary_expression_type(tokens []FastcExpressionToken, start int, end int, operator_index int) !string {
	operator := tokens[operator_index].tok
	left_tokens := tokens[start..operator_index]
	right_tokens := tokens[operator_index + 1..end]
	mut left_type := fastc_normalize_inferred_type(g.infer_expression_type(left_tokens)!)
	if operator in [.key_is, .not_is] {
		right_type := g.type_from_expression_tokens(right_tokens) or {
			return g.unsupported('type test `${operator.str()}` with an undeclared target type')
		}
		if g.semantic_type_key(right_type) !in g.declared_types {
			return g.unsupported('type test `${operator.str()}` with undeclared type `${right_type}`')
		}
		return 'bool'
	}
	mut right_type := fastc_normalize_inferred_type(g.infer_expression_type(right_tokens)!)
	if operator in [.and, .logical_or] {
		return 'bool'
	}
	if operator in [.key_in, .not_in] {
		array_end := if right_tokens.len > 0 && right_tokens.last().tok == .not {
			right_tokens.len - 1
		} else {
			right_tokens.len
		}
		if array_end >= 2 && right_tokens[0].tok == .lsbr
			&& right_tokens[array_end - 1].tok == .rsbr {
			return 'bool'
		}
		if right_type.trim_right('*').starts_with('Map_') {
			_, _ := fastc_map_key_value_types(right_type) or {
				return g.unsupported('membership `${operator.str()}` with unverifiable map type `${right_type}`')
			}
		} else if right_type.trim_right('*') == 'string' {
		} else {
			_ := g.array_element_type(right_type) or {
				return g.unsupported('membership `${operator.str()}` in non-collection type `${right_type}`')
			}
		}
		return 'bool'
	}
	if operator in [.eq, .ne]
		&& ((fastc_is_pointer_type(left_type) && fastc_expression_is_zero(right_tokens))
		|| (fastc_is_pointer_type(right_type) && fastc_expression_is_zero(left_tokens))) {
		return 'bool'
	}
	if operator in [.eq, .ne] {
		if g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_type == ''
			&& fastc_expression_is_enum_shorthand(right_tokens) {
			right_type = left_type
		} else if g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_type == ''
			&& fastc_expression_is_enum_shorthand(left_tokens) {
			left_type = right_type
		}
		if !fastc_is_pointer_type(left_type) && !fastc_is_pointer_type(right_type) {
			left_layout := g.underlying_alias_type(left_type).trim_right('*')
			right_layout := g.underlying_alias_type(right_type).trim_right('*')
			left_key := g.semantic_type_key(left_layout)
			if left_layout == right_layout && left_layout !in ['Option', '_option', '_result']
				&& left_key in g.declared_kinds && g.declared_kinds[left_key] == .struct_
				&& !g.struct_equality_is_supported(left_type, []string{}) {
				return g.unsupported('struct equality for `${left_type}` with unsupported fields')
			}
		}
	}
	if g.selfhost && operator in [.eq, .ne]
		&& ((fastc_is_pointer_type(left_type) && right_type == '')
		|| (fastc_is_pointer_type(right_type) && left_type == '')) {
		return 'bool'
	}
	if g.selfhost && ((left_type == '' && fastc_expression_is_c_qualified_name(left_tokens))
		|| (right_type == '' && fastc_expression_is_c_qualified_name(right_tokens))) {
		return 'bool'
	}
	return 'bool'
}

fn (g &Parser) infer_expression_type(tokens []FastcExpressionToken) !string {
	if tokens.len == 0 {
		return ''
	}
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar {
		wrapper_end := fastc_matching_rpar(tokens[start..end], 0) or { break }
		if wrapper_end != end - start - 1 {
			break
		}
		start++
		end--
	}
	if start >= end {
		return ''
	}
	if end - start >= 4 && tokens[start].tok == .key_sizeof && tokens[start + 1].tok == .lpar
		&& tokens[end - 1].tok == .rpar {
		return 'int'
	}
	if tokens[start].tok == .not {
		_ = g.infer_expression_type(tokens[start + 1..end])!
		return 'bool'
	}
	if operator_index := fastc_lowest_precedence_operator_index(tokens, start, end) {
		return g.infer_boolean_binary_expression_type(tokens, start, end, operator_index)!
	}
	if end - start == 1 {
		item := tokens[start]
		if item.typ != '' {
			return item.typ
		}
		return match item.tok {
			.name {
				if local := g.locals[item.lit] {
					local.typ
				} else if constant_type := g.constant_types[fastc_constant_key(g.module_name,
					item.lit)]
				{
					constant_type
				} else if constant_type := g.constant_types[fastc_constant_key('builtin', item.lit)] {
					constant_type
				} else if fastc_constant_key(g.module_name, item.lit) in g.constants {
					'integer literal'
				} else if fastc_constant_key('builtin', item.lit) in g.constants {
					'integer literal'
				} else if global_type := g.global_types[fastc_global_key(g.module_name, item.lit)] {
					global_type
				} else if global_type := g.global_types[fastc_global_key('builtin', item.lit)] {
					global_type
				} else if g.selfhost {
					'int'
				} else {
					''
				}
			}
			.number {
				fastc_number_expression_type(item.lit)
			}
			.string {
				'string'
			}
			.char {
				if item.lit.starts_with('c:') {
					'charptr'
				} else {
					'rune'
				}
			}
			.key_true, .key_false {
				'bool'
			}
			.key_nil {
				'nil'
			}
			.key_none {
				'Option'
			}
			else {
				''
			}
		}
	}
	if end - start == 2 && tokens[start].tok == .dot && tokens[start + 1].typ != '' {
		return tokens[start + 1].typ
	}
	if end - start == 5 && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name && tokens[start + 3].tok == .dot
		&& tokens[start + 4].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if enum_type_key := g.underlying_enum_type_key(type_key) {
				return fastc_c_declared_type_name(enum_type_key)
			}
		}
	}
	if end - start >= 5 && tokens[start].tok == .lsbr && tokens[start + 1].tok == .rsbr
		&& tokens[start + 2].tok == .name && tokens[start + 3].tok == .lpar
		&& tokens[end - 1].tok == .rpar {
		mut element_type := tokens[start + 2].lit
		if primitive := fastc_primitive_c_type(element_type) {
			element_type = primitive
		}
		return fastc_array_c_type(element_type)
	}
	if end - start == 3 && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if type_key in g.declared_types {
				return fastc_c_declared_type_name(type_key)
			}
		}
		if type_key := fastc_resolve_declared_type_key(g.module_name, tokens[start].lit, g.imports,
			g.declared_types)
		{
			if enum_type_key := g.underlying_enum_type_key(type_key) {
				return fastc_c_declared_type_name(enum_type_key)
			}
		}
		if imported_module := g.imports[tokens[start].lit] {
			if constant_type := g.constant_types[fastc_constant_key(imported_module, tokens[start +
				2].lit)]
			{
				return constant_type
			}
			if fastc_constant_key(imported_module, tokens[start + 2].lit) in g.constants {
				return 'integer literal'
			}
			global_name := tokens[start + 2].lit
			if global_type := g.global_types[fastc_global_key(imported_module, global_name)] {
				g.validate_imported_global_visibility(imported_module, global_name)!
				return global_type
			}
		}
	}
	for init_open in start + 1 .. end {
		if tokens[init_open].tok == .lcbr {
			if array_type := g.array_initializer_type(tokens[start..init_open]) {
				return array_type
			}
			break
		}
	}
	array_end := if tokens[end - 1].tok == .not { end - 1 } else { end }
	if tokens[start].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
		items := fastc_expression_list_items(tokens, start + 1, array_end - 1)!
		if items.len == 0 {
			return ''
		}
		element_type := fastc_normalize_inferred_type(g.infer_expression_type(items[0])!)
		if element_type == '' {
			return ''
		}
		return fastc_array_c_type(element_type)
	}
	if start + 1 < end && tokens[start].tok == .name && tokens[start + 1].tok == .lcbr {
		if type_key := fastc_resolve_declared_type_key(g.module_name, tokens[start].lit, g.imports,
			g.declared_types)
		{
			return fastc_c_declared_type_name(type_key)
		}
	}
	mut call_name_index := start
	mut call_open_index := start + 1
	if start + 3 < end && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name
		&& (tokens[start].lit in g.imports || tokens[start].lit == 'C') {
		call_name_index = start + 2
		call_open_index = start + 3
	}
	if call_open_index < end && tokens[call_name_index].tok == .name
		&& tokens[call_open_index].tok == .lpar {
		if close := fastc_matching_rpar(tokens[start..end], call_open_index - start) {
			if close == end - start - 1 {
				name := tokens[call_name_index].lit
				function_key := g.function_key_for_call(tokens, call_name_index)
				if signature := g.functions[function_key] {
					return signature.return_type
				}
				if call_name_index == start {
					if primitive := fastc_primitive_c_type(name) {
						return primitive
					}
					if type_key := fastc_resolve_declared_type_key(g.module_name, name, g.imports,
						g.declared_types)
					{
						return fastc_c_declared_type_name(type_key)
					}
				}
				if call_name_index == start + 2 && tokens[start].lit == 'C' && name.len > 0
					&& name[0].is_capital() {
					return name
				}
				return ''
			}
		}
	}
	for i in start + 2 .. end - 1 {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		close := fastc_matching_rpar(tokens[start..end], i + 1 - start) or { continue }
		if close != end - start - 1 {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_type := g.infer_expression_type(tokens[receiver_start..i - 1])!
		if receiver_type == '' {
			continue
		}
		function_key := g.method_function_key(receiver_type, tokens[i].lit)
		if static_key := g.static_function_key_for_call(tokens, i) {
			if signature := g.functions[static_key] {
				return signature.return_type
			}
		}
		if signature := g.functions[function_key] {
			return g.specialized_method_return_type(receiver_type, function_key, signature)
		}
	}
	if end - start >= 3 && tokens[end - 2].tok == .dot && tokens[end - 1].tok == .name {
		receiver_start := fastc_method_receiver_start(tokens, end - 2)
		if receiver_start == start {
			receiver_type := g.infer_expression_type(tokens[start..end - 2])!
			if field := g.struct_field_metadata(receiver_type, tokens[end - 1].lit) {
				return field.typ
			}
		}
	}
	if tokens[start].tok in [.plus, .minus] {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		if tokens[start].tok == .minus && operand_type == 'integer literal' {
			return 'negative integer literal'
		}
		return operand_type
	}
	if tokens[start].tok in [.amp, .and] {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		mut pointer_count := 1
		if tokens[start].tok == .and {
			pointer_count = 2
		}
		return if operand_type == '' {
			'voidptr'
		} else {
			operand_type + '*'.repeat(pointer_count)
		}
	}
	if tokens[start].tok == .mul {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		return operand_type.trim_right('*')
	}
	if tokens[start].tok == .bit_not {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		return operand_type
	}
	if g.selfhost && tokens[end - 1].tok == .not {
		value_type := g.option_value_type_for_expression(tokens[start..end - 1])
		return if value_type == 'void' { '' } else { value_type }
	}
	if tokens[end - 1].tok in [.inc, .dec] {
		operand_type := g.infer_expression_type(tokens[start..end - 1])!
		if g.selfhost && operand_type == '' {
			return 'int'
		}
		return operand_type
	}
	if tokens[end - 1].tok == .rsbr {
		mut bracket_depth := 0
		mut open_index := -1
		for i := end - 1; i >= start; i-- {
			if tokens[i].tok == .rsbr {
				bracket_depth++
			} else if tokens[i].tok == .lsbr {
				bracket_depth--
				if bracket_depth == 0 {
					open_index = i
					break
				}
			}
		}
		if open_index > start {
			base_type := g.infer_expression_type(tokens[start..open_index])!
			if fastc_expression_tokens_contain(tokens[open_index + 1..end - 1], .dotdot) {
				return base_type
			}
			if base_type.trim_right('*').starts_with('Map_') {
				_, value_type := fastc_map_key_value_types(base_type) or { return '' }
				return value_type
			}
			if base_type.ends_with('*') {
				return base_type.trim_right('*')
			}
			if base_type.trim_right('*') == 'string' {
				return 'u8'
			}
			if element_type := g.array_element_type(base_type) {
				return element_type
			}
		}
	}
	if member_type := g.infer_member_access_type(tokens[start..end]) {
		return member_type
	}
	mut depth := 0
	for i in start .. end {
		match tokens[i].tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			else {}
		}
		if depth != 0 {
			continue
		}
		if tokens[i].tok.is_assignment() {
			return g.infer_expression_type(tokens[start..i])!
		}
		if tokens[i].tok in [.left_shift, .right_shift, .right_shift_unsigned] && i > start {
			mut left_type := g.infer_expression_type(tokens[start..i])!
			mut right_type := g.infer_expression_type(tokens[i + 1..end])!
			if left_element := g.indexed_array_operand_type(tokens[start..i], left_type) {
				left_type = left_element
			}
			if right_element := g.indexed_array_operand_type(tokens[i + 1..end], right_type) {
				right_type = right_element
			}
			if g.selfhost && tokens[i].tok == .left_shift && g.array_element_type(left_type) != none {
				return left_type
			}
			if g.selfhost && left_type == '' && fastc_is_integer_expression_type(right_type) {
				return 'int'
			}
			return if left_type != '' {
				left_type
			} else if right_type != '' {
				right_type
			} else {
				'int'
			}
		}
		if tokens[i].tok in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor] && i > start {
			left_tokens := tokens[start..i]
			right_tokens := tokens[i + 1..end]
			mut left_type := g.infer_expression_type(left_tokens)!
			mut right_type := g.infer_expression_type(right_tokens)!
			if left_element := g.indexed_array_operand_type(tokens[start..i], left_type) {
				left_type = left_element
			}
			if right_element := g.indexed_array_operand_type(tokens[i + 1..end], right_type) {
				right_type = right_element
			}
			if tokens[i].tok == .plus
				&& g.underlying_alias_type(left_type).trim_right('*') == 'string'
				&& g.underlying_alias_type(right_type).trim_right('*') == 'string' {
				return 'string'
			}
			if g.selfhost && tokens[i].tok == .plus && ((left_type == 'string' && right_type == '')
				|| (right_type == 'string' && left_type == '')) {
				return 'string'
			}
			if g.selfhost && tokens[i].tok in [.plus, .minus] && fastc_is_pointer_type(left_type)
				&& fastc_is_integer_expression_type(right_type) {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && left_type == right_type
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor]
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_type == '' {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor]
				&& g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_type == '' {
				return right_type
			}
			if g.selfhost && fastc_is_integer_expression_type(left_type)
				&& fastc_is_integer_expression_type(right_type) {
				return if left_type == 'integer literal' { right_type } else { left_type }
			}
			if g.selfhost && left_type == '' && fastc_is_numeric_expression_type(right_type) {
				return right_type
			}
			if g.selfhost && right_type == '' && fastc_is_numeric_expression_type(left_type) {
				return left_type
			}
			if g.selfhost && right_type == 'voidptr' && fastc_is_numeric_expression_type(left_type) {
				return left_type
			}
			if g.selfhost && left_type == 'voidptr' && fastc_is_numeric_expression_type(right_type) {
				return right_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(left_type)
				&& g.declared_kinds[g.semantic_type_key(right_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(right_type)
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return right_type
			}
			if g.selfhost && left_type == right_type
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && left_type == '' && right_type == '' {
				return 'int'
			}
			common_type := fastc_common_arithmetic_type(left_type, right_type)
			if common_type.len == 0 {
				return if left_type != '' {
					left_type
				} else if right_type != '' {
					right_type
				} else {
					'int'
				}
			}
			return common_type
		}
	}
	return ''
}

fn (g &Parser) indexed_array_operand_type(tokens []FastcExpressionToken, inferred_type string) ?string {
	if tokens.len < 3 || !fastc_expression_tokens_contain(tokens, .lsbr)
		|| tokens.last().tok != .rsbr {
		return none
	}
	return g.array_element_type(inferred_type)
}

fn (g &Parser) infer_member_access_type(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 || tokens[0].tok != .name {
		return none
	}
	mut current_type := ''
	if local := g.locals[tokens[0].lit] {
		current_type = local.typ
	} else if global_type := g.global_types[fastc_global_key(g.module_name, tokens[0].lit)] {
		current_type = global_type
	} else if constant_type := g.constant_types[fastc_constant_key(g.module_name, tokens[0].lit)] {
		current_type = constant_type
	} else if constant_type := g.constant_types[fastc_constant_key('builtin', tokens[0].lit)] {
		current_type = constant_type
	} else {
		return none
	}
	mut index := 1
	for index < tokens.len {
		if tokens[index].tok == .lsbr {
			close := fastc_matching_delimiter(tokens, index, .lsbr, .rsbr) or { return none }
			if fastc_expression_tokens_contain(tokens[index + 1..close], .dotdot) {
				if current_type.trim_right('*') != 'string'
					&& g.array_element_type(current_type) == none {
					return none
				}
			} else if current_type.trim_right('*') == 'string' {
				current_type = 'u8'
			} else if current_type.trim_right('*').starts_with('Map_') {
				_, value_type := fastc_map_key_value_types(current_type) or { return none }
				current_type = value_type
			} else if current_type.ends_with('*') {
				current_type = current_type.trim_right('*')
			} else {
				current_type = g.array_element_type(current_type) or { return none }
			}
			index = close + 1
			continue
		}
		if index + 1 >= tokens.len || tokens[index].tok != .dot || tokens[index + 1].tok != .name {
			return none
		}
		current_type = g.struct_member_type(current_type, tokens[index + 1].lit)
		if current_type == '' {
			return none
		}
		index += 2
	}
	if index != tokens.len {
		return none
	}
	return current_type
}

fn fastc_matching_delimiter(tokens []FastcExpressionToken, open_index int, open token.Token, close token.Token) ?int {
	mut depth := 0
	for i in open_index .. tokens.len {
		if tokens[i].tok == open {
			depth++
		} else if tokens[i].tok == close {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn (g &Parser) semantic_type_key(c_type string) string {
	base := c_type.trim_right('*')
	if key := g.declared_type_c_names[base] {
		return key
	}
	return base
}

fn (g &Parser) underlying_alias_type(c_type string) string {
	mut resolved := c_type
	mut seen := map[string]bool{}
	for {
		base := resolved.trim_right('*')
		if base in seen {
			return resolved
		}
		alias_base := g.alias_base_types[base] or { return resolved }
		seen[base] = true
		resolved = alias_base + resolved[base.len..]
	}
	return resolved
}

fn fastc_number_expression_type(literal string) string {
	clean := literal.replace('_', '')
	if clean.contains('.') || (!(clean.starts_with('0x') || clean.starts_with('0X'))
		&& clean.contains_any('eE')) {
		return 'float literal'
	}
	if clean.starts_with('-') {
		return 'negative integer literal'
	}
	return 'integer literal'
}

fn fastc_integer_literal_value(tokens []FastcExpressionToken) ?i64 {
	mut sign := i64(1)
	mut number_index := 0
	if tokens.len == 2 && tokens[0].tok in [.plus, .minus] {
		sign = if tokens[0].tok == .minus { -1 } else { 1 }
		number_index = 1
	} else if tokens.len != 1 {
		return none
	}
	if tokens[number_index].tok != .number
		|| fastc_number_expression_type(tokens[number_index].lit) != 'integer literal' {
		return none
	}
	mut value := tokens[number_index].lit.replace('_', '').i64()
	if sign < 0 {
		value = -value
	}
	return value
}

fn fastc_common_arithmetic_type(left string, right string) string {
	if left == right && fastc_is_numeric_expression_type(left) {
		return left
	}
	if left == 'negative integer literal' && fastc_is_unsigned_integer_type(right) {
		return ''
	}
	if right == 'negative integer literal' && fastc_is_unsigned_integer_type(left) {
		return ''
	}
	if fastc_is_integer_literal_expression_type(left) && fastc_is_integer_type(right) {
		return right
	}
	if fastc_is_integer_literal_expression_type(right) && fastc_is_integer_type(left) {
		return left
	}
	if fastc_is_integer_literal_expression_type(left)
		&& fastc_is_integer_literal_expression_type(right) {
		return if left == 'negative integer literal' || right == 'negative integer literal' {
			'negative integer literal'
		} else {
			'integer literal'
		}
	}
	if left == 'float literal' && right in ['f32', 'f64'] {
		return right
	}
	if right == 'float literal' && left in ['f32', 'f64'] {
		return left
	}
	return ''
}

fn fastc_is_numeric_expression_type(typ string) bool {
	return fastc_is_integer_literal_expression_type(typ) || typ in ['float literal', 'f32', 'f64']
		|| fastc_is_integer_type(typ)
}

fn fastc_is_integer_expression_type(typ string) bool {
	return fastc_is_integer_literal_expression_type(typ) || fastc_is_integer_type(typ)
}

fn fastc_is_integer_literal_expression_type(typ string) bool {
	return typ in ['integer literal', 'negative integer literal']
}

fn fastc_types_share_lowering_representation(actual string, expected string) bool {
	if actual == expected {
		return true
	}
	if actual == 'integer literal' {
		return fastc_is_integer_type(expected)
	}
	if actual == 'negative integer literal' {
		return fastc_is_integer_type(expected) && !fastc_is_unsigned_integer_type(expected)
	}
	if actual == 'float literal' {
		return expected in ['f32', 'f64']
	}
	if actual == 'nil' {
		return expected.ends_with('*') || expected in ['voidptr', 'byteptr', 'charptr']
	}
	return false
}

fn fastc_selfhost_types_share_lowering_representation(actual string, expected string) bool {
	if (actual == 'byteptr' && expected == 'u8*')
		|| (expected == 'byteptr' && actual == 'u8*')
		|| (actual == 'charptr' && expected == 'char*')
		|| (expected == 'charptr' && actual == 'char*') {
		return true
	}
	if actual == expected + '*' || expected == actual + '*' {
		return true
	}
	actual_base := actual.trim_right('*')
	expected_base := expected.trim_right('*')
	if (actual_base == 'array' && expected_base.starts_with('Array_'))
		|| (expected_base == 'array' && actual_base.starts_with('Array_'))
		|| (actual_base == 'map' && expected_base.starts_with('Map_'))
		|| (expected_base == 'map' && actual_base.starts_with('Map_')) {
		return true
	}
	if actual == 'negative integer literal' && fastc_is_unsigned_integer_type(expected) {
		return false
	}
	if fastc_is_integer_expression_type(actual) && fastc_is_integer_type(expected) {
		return true
	}
	if expected == 'voidptr' && fastc_is_pointer_type(actual) {
		return true
	}
	if actual == 'voidptr' && fastc_is_pointer_type(expected) {
		return true
	}
	return false
}

fn (g &Parser) selfhost_types_share_lowering_representation(actual string, expected string) bool {
	if fastc_selfhost_types_share_lowering_representation(actual, expected) {
		return true
	}
	if fastc_is_numeric_expression_type(actual)
		&& g.declared_kinds[g.semantic_type_key(expected)] == .alias_ {
		return true
	}
	if fastc_is_numeric_expression_type(expected)
		&& g.declared_kinds[g.semantic_type_key(actual)] == .alias_ {
		return true
	}
	return false
}

fn fastc_is_pointer_type(typ string) bool {
	return typ.ends_with('*') || typ in ['voidptr', 'byteptr', 'charptr']
}

fn fastc_array_element_type(typ string) ?string {
	base := typ.trim_right('*')
	if base.starts_with('Array_') && base.len > 'Array_'.len {
		element := base['Array_'.len..]
		return if element == 'char_ptr' { 'char*' } else { element }
	}
	if base.starts_with('FixedArray_') && base.len > 'FixedArray_'.len {
		if element_type := fastc_fixed_array_element_type(base) {
			return element_type
		}
		return base['FixedArray_'.len..]
	}
	return none
}

fn (g &Parser) array_element_type(typ string) ?string {
	if element_type := fastc_array_element_type(typ) {
		return element_type
	}
	layout_type := typ.trim_right('*')
	if layout_type !in g.struct_fields {
		return none
	}
	fields := g.struct_fields[layout_type].clone()
	element_type := fields['__fastc_element_type'] or { return none }
	return element_type
}

fn fastc_is_integer_type(typ string) bool {
	return typ in ['byte', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'rune', 'u8', 'u16',
		'u32', 'u64', 'unsigned int', 'usize']
}

fn fastc_is_unsigned_integer_type(typ string) bool {
	return typ in ['byte', 'u8', 'u16', 'u32', 'u64', 'unsigned int', 'usize']
}

fn fastc_is_wide_unsigned_integer_type(typ string) bool {
	return typ in ['u32', 'u64', 'unsigned int', 'usize']
}

fn fastc_is_signed_integer_type(typ string) bool {
	return typ in ['char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'rune']
}

fn fastc_nondecimal_literal_is_type_sensitive(literal string) bool {
	clean := literal.replace('_', '')
	if clean.len <= 2 || clean[0] != `0` {
		return false
	}
	digits := clean[2..].trim_left('0')
	if clean[1] in [`x`, `X`] {
		if digits.len > 8 {
			return true
		}
		return digits.len == 8 && ((digits[0] >= `8` && digits[0] <= `9`)
			|| (digits[0] >= `a` && digits[0] <= `f`)
			|| (digits[0] >= `A` && digits[0] <= `F`))
	}
	if clean[1] in [`b`, `B`] {
		return digits.len >= 32
	}
	if clean[1] in [`o`, `O`] {
		return digits.len > 11 || (digits.len == 11 && digits[0] >= `2`)
	}
	return false
}

fn fastc_decimal_literal_is_type_sensitive(literal string) bool {
	clean := literal.replace('_', '')
	if clean.len == 0 || clean.contains_any('.eE') {
		return false
	}
	for digit in clean {
		if !digit.is_digit() {
			return false
		}
	}
	digits := clean.trim_left('0')
	int_max_literal := '2147483647'
	if digits.len != int_max_literal.len {
		return digits.len > int_max_literal.len
	}
	for i in 0 .. digits.len {
		if digits[i] != int_max_literal[i] {
			return digits[i] > int_max_literal[i]
		}
	}
	return false
}

fn fastc_c_number(literal string) !string {
	clean := literal.replace('_', '')
	if fastc_decimal_literal_is_type_sensitive(literal) {
		// C assigns oversized decimal tokens a wider type before any surrounding
		// operation. Reject them until the direct parser can preserve V inference.
		return error('fastc parser does not support oversized decimal literal expressions')
	}
	if fastc_nondecimal_literal_is_type_sensitive(literal) {
		return error('fastc parser does not support high-bit nondecimal literals')
	}
	if clean.len > 2 && clean[0] == `0` && clean[1] in [`o`, `O`] {
		// V spells octal integers with an explicit 0o prefix. GNU C uses a
		// leading zero, so translate the prefix before emitting the token.
		return '0' + clean[2..]
	}
	if clean.len < 2 || clean[0] != `0` || !clean[1].is_digit() || clean.contains_any('.eE') {
		return clean
	}
	mut first_digit := 0
	for first_digit < clean.len - 1 && clean[first_digit] == `0` {
		first_digit++
	}
	return clean[first_digit..]
}

fn fastc_c_selfhost_number(literal string) string {
	clean := literal.replace('_', '')
	if clean.len > 2 && clean[0] == `0` && clean[1] in [`o`, `O`] {
		return '0${clean[2..]}${if fastc_nondecimal_literal_is_type_sensitive(literal) {
			'ULL'
		} else {
			''
		}}'
	}
	if fastc_decimal_literal_is_type_sensitive(literal)
		|| fastc_nondecimal_literal_is_type_sensitive(literal) {
		return clean + 'ULL'
	}
	if clean.len < 2 || clean[0] != `0` || !clean[1].is_digit() || clean.contains_any('.eE') {
		return clean
	}
	mut first_digit := 0
	for first_digit < clean.len - 1 && clean[first_digit] == `0` {
		first_digit++
	}
	return clean[first_digit..]
}
