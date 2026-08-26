module fastc

import strings

fn (g &Parser) render_typeof_name_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 6 || tokens[0].lit != 'typeof' || tokens[1].tok != .lpar
		|| tokens[tokens.len - 2].tok != .dot || tokens.last().lit != 'name' {
		return none
	}
	close := fastc_matching_rpar(tokens, 1) or { return none }
	if close != tokens.len - 3 || close <= 2 {
		return none
	}
	inner_tokens := tokens[2..close]
	mut type_name := g.infer_expression_type(inner_tokens) or { return none }
	if inner_tokens.len == 1 && inner_tokens[0].tok == .name {
		if local := g.locals[inner_tokens[0].lit] {
			if local.is_reference {
				type_name = type_name.trim_right('*')
			}
		}
	}
	if type_name == '' {
		return none
	}
	return FastcRenderedExpression{
		source: '_S("${type_name}")'
		typ:    'string'
	}
}

fn (g &Parser) render_disabled_call_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 3 || tokens.last().tok != .rpar {
		return none
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok != .name || tokens[open_index].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar(tokens, open_index) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	function_key := if name_index == 2 && tokens[0].lit !in g.imports && tokens[0].lit != 'C' {
		if static_key := g.static_function_key_for_call(tokens, name_index) {
			static_key
		} else {
			receiver_type := g.infer_expression_type(tokens[..1]) or { return none }
			g.method_function_key(receiver_type, tokens[name_index].lit)
		}
	} else {
		g.function_key_for_call(tokens, name_index)
	}
	signature := g.functions[function_key] or { return none }
	if !signature.is_disabled {
		return none
	}
	return FastcRenderedExpression{
		source: fastc_disabled_call_expression(signature.return_type)
		typ:    signature.return_type
	}
}

fn (g &Parser) render_cast_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut open := -1
	mut c_type := ''
	for i, item in tokens {
		if item.tok != .lpar {
			continue
		}
		// A leading `*` is a dereference around the cast, not part of its type.
		// The raw renderer preserves that unary operation while lowering the
		// nested `&Type(value)` cast.
		if tokens[0].tok == .mul {
			return none
		}
		c_type = g.type_from_expression_tokens(tokens[..i]) or { '' }
		if c_type == '' && i == 3 && tokens[0].tok == .name && tokens[0].lit == 'C'
			&& tokens[1].tok == .dot && tokens[2].tok == .name && tokens[2].lit.len > 0
			&& tokens[2].lit[0].is_capital() && 'C.${tokens[2].lit}' !in g.functions {
			c_type = tokens[2].lit
		}
		if c_type != '' {
			open = i
		}
		break
	}
	if open <= 0 || c_type == '' {
		return none
	}
	close := fastc_matching_rpar(tokens, open) or { return none }
	if close != tokens.len - 1 || open + 1 == close {
		return none
	}
	inner := g.render_call_argument_expression(tokens[open + 1..close], c_type) or { return none }
	return FastcRenderedExpression{
		source: '((${c_type})(${inner}))'
		typ:    c_type
	}
}

fn (g &Parser) render_flag_method_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 7 {
		return none
	}
	mut rendered := rendered_expression
	mut changed := false
	mut result_type := ''
	for i in 2 .. tokens.len {
		if tokens[i].tok != .name || tokens[i].lit !in ['has', 'set', 'clear']
			|| tokens[i - 1].tok != .dot || i + 2 >= tokens.len || tokens[i + 1].tok != .lpar {
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if call_end <= i + 2 {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
		receiver_key := g.flag_enum_type_key(receiver_type) or { continue }
		mut receiver := strings.new_builder(32)
		for token_item in receiver_tokens {
			receiver.write_string(match token_item.tok {
				.name { token_item.lit }
				.dot { '.' }
				else { token_item.tok.str() }
			})
		}
		mut raw_argument := strings.new_builder(24)
		mut c_argument := strings.new_builder(48)
		mut argument_index := i + 2
		for argument_index < call_end {
			if tokens[argument_index].tok == .dot && argument_index + 1 < call_end
				&& tokens[argument_index + 1].tok == .name {
				raw_argument.write_string('.${tokens[argument_index + 1].lit}')
				c_argument.write_string('${fastc_c_declared_type_name(receiver_key)}__${tokens[
					argument_index + 1].lit}')
				argument_index += 2
				continue
			}
			piece := tokens[argument_index].tok.str()
			raw_argument.write_string(piece)
			c_argument.write_string(piece)
			argument_index++
		}
		method := tokens[i].lit
		raw_receiver_source := receiver.str()
		receiver_source := g.render_member_receiver(receiver_tokens) or { raw_receiver_source }
		raw_argument_source := raw_argument.str()
		c_argument_source := c_argument.str()
		mut needle := '${raw_receiver_source}.${method}(${c_argument_source})'
		if !rendered.contains(needle) {
			needle = '${receiver_source}.${method}(${c_argument_source})'
		}
		if !rendered.contains(needle) {
			needle = '${raw_receiver_source}.${method}(${raw_argument_source})'
		}
		if !rendered.contains(needle) {
			needle = '${receiver_source}.${method}(${raw_argument_source})'
		}
		replacement := match method {
			'has' { '((${receiver_source} & ${c_argument_source}) != 0)' }
			'set' { '((${receiver_source}) |= (${c_argument_source}))' }
			else { '((${receiver_source}) &= ~(${c_argument_source}))' }
		}
		if !rendered.contains(needle) {
			continue
		}
		rendered = rendered.replace(needle, replacement)
		if method == 'has' {
			result_type = 'bool'
		}
		changed = true
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    result_type
		}
	} else {
		none
	}
}

fn (g &Parser) render_static_call_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut result_type := ''
	mut changed := false
	for i := 2; i + 1 < tokens.len; i++ {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lpar {
			continue
		}
		function_key := g.static_function_key_for_call(tokens, i) or { continue }
		signature := g.functions[function_key] or { FastcFunctionSignature{} }
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if signature.is_disabled {
			disabled_call := fastc_disabled_call_expression(signature.return_type)
			call_start := i - 2
			if call_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: disabled_call
					typ:    signature.return_type
				}
			}
			raw_call := g.render_raw_expression_tokens(tokens[call_start..call_end + 1]) or {
				continue
			}
			if rendered.contains(raw_call) {
				rendered = rendered.replace(raw_call, disabled_call)
				result_type = signature.return_type
				changed = true
			}
			continue
		}
		type_key := function_key.all_before_last('.')
		owner := fastc_c_declared_type_name(type_key)
		mut needle := '${owner}.${tokens[i].lit}('
		if !rendered.contains(needle) {
			needle = '${owner}__${tokens[i].lit}('
		}
		if !rendered.contains(needle) {
			continue
		}
		rendered = rendered.replace(needle,
			'${fastc_method_c_name_for_key(type_key, tokens[i].lit)}(')
		result_type = signature.return_type
		changed = true
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    result_type
		}
	} else {
		none
	}
}

fn (g &Parser) method_function_key(receiver_type string, name string) string {
	direct_key := '${g.semantic_type_key(receiver_type)}.${name}'
	if direct_key in g.functions {
		return direct_key
	}
	if g.selfhost && name in ['keys', 'values'] && 'map.${name}' in g.functions {
		return 'map.${name}'
	}
	mut layout_type := receiver_type.trim_right('*')
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	} else if layout_type.starts_with('Map_') {
		if 'map.${name}' in g.functions {
			return 'map.${name}'
		}
		layout_type = 'map'
	}
	mut fields := map[string]string{}
	if layout_type in g.struct_fields {
		fields = g.struct_fields[layout_type].clone()
	}
	if 'data' in fields && 'len' in fields && 'cap' in fields && 'array.${name}' in g.functions {
		return 'array.${name}'
	}
	return direct_key
}

fn (g &Parser) specialized_method_return_type(receiver_type string, method_key string, signature FastcFunctionSignature) string {
	if method_key in ['map.keys', 'map.values'] {
		key_type, value_type := fastc_map_key_value_types(receiver_type) or {
			return signature.return_type
		}
		element_type := if method_key == 'map.keys' { key_type } else { value_type }
		return fastc_array_c_type(element_type)
	}
	if method_key.starts_with('array.') {
		if element_type := g.array_element_type(receiver_type) {
			method_name := method_key.all_after_last('.')
			if method_name in ['first', 'last', 'pop', 'pop_left', 'get', 'get_unsafe', 'get_i64',
				'get_u64', 'get_ni', 'get_with_check', 'get_with_check_i64', 'get_with_check_u64',
				'get_with_check_ni'] {
				return element_type
			}
			if signature.return_type == 'array' {
				return fastc_array_c_type(element_type)
			}
		}
	}
	if method_key.starts_with('map.') && signature.return_type == 'map'
		&& receiver_type.trim_right('*').starts_with('Map_') {
		return receiver_type.trim_right('*')
	}
	return signature.return_type
}

fn (g &Parser) render_interface_cast_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .name || tokens[1].tok != .lpar
		|| tokens.last().tok != .rpar {
		return none
	}
	type_key := fastc_resolve_declared_type_key(g.module_name, tokens[0].lit, g.imports,
		g.declared_types) or { return none }
	if g.declared_kinds[type_key] != .interface_ {
		return none
	}
	close := fastc_matching_rpar(tokens, 1) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	interface_type := fastc_c_declared_type_name(type_key)
	prefix := '((${interface_type})('
	if !rendered_expression.starts_with(prefix) || !rendered_expression.ends_with('))') {
		return none
	}
	inner_source := rendered_expression[prefix.len..rendered_expression.len - 2]
	actual_type := g.infer_expression_type(tokens[2..close]) or { return none }
	if actual_type == '' {
		return none
	}
	return FastcRenderedExpression{
		source: g.interface_value_expression(interface_type, actual_type, inner_source)
		typ:    interface_type
	}
}

fn (g &Parser) render_map_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if lookup := g.render_map_lookup_option_expression(tokens) {
		return FastcRenderedExpression{
			source: '({ Option lookup = (${lookup.source}); lookup.state ? (${lookup.typ}){0} : *((${lookup.typ} *)lookup.data); })'
			typ:    lookup.typ
		}
	}
	mut literal_open := -1
	for i, item in tokens {
		if item.tok == .lcbr {
			literal_open = i
			break
		}
	}
	if literal_open > 0 && literal_open + 1 == tokens.len - 1 && tokens.last().tok == .rcbr {
		if map_type := g.map_initializer_type(tokens[..literal_open]) {
			key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
			hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
			return FastcRenderedExpression{
				source: '(builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
				typ:    map_type
			}
		}
	}
	mut depth := 0
	mut assignment_index := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.assign {
				if depth == 0 {
					assignment_index = i
					break
				}
			}
			else {}
		}
	}
	if assignment_index > 3 && tokens[assignment_index - 1].tok == .rsbr {
		close := assignment_index - 1
		mut open := -1
		mut bracket_depth := 0
		for i := close; i >= 0; i-- {
			if tokens[i].tok == .rsbr {
				bracket_depth++
			} else if tokens[i].tok == .lsbr {
				bracket_depth--
				if bracket_depth == 0 {
					open = i
					break
				}
			}
		}
		if open <= 0 {
			return none
		}
		base_tokens := tokens[..open]
		map_type := g.infer_expression_type(base_tokens) or { return none }
		key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
		key_source := g.render_call_argument_expression(tokens[open + 1..close], key_type) or {
			return none
		}
		value_source := g.render_call_argument_expression(tokens[assignment_index + 1..],
			value_type) or { return none }
		map_source := g.render_member_receiver(base_tokens) or {
			g.render_raw_expression_tokens(base_tokens) or { return none }
		}
		map_address := if map_type.ends_with('*') { map_source } else { '&${map_source}' }
		return FastcRenderedExpression{
			source: '({ ${key_type} __v_fastc_map_key = (${key_source}); ${value_type} __v_fastc_map_value = (${value_source}); builtin__map_set((map *)${map_address}, &__v_fastc_map_key, &__v_fastc_map_value); __v_fastc_map_value; })'
			typ:    value_type
		}
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lsbr
		&& tokens.last().tok == .rsbr {
		close := fastc_matching_delimiter(tokens, 1, .lsbr, .rsbr) or { return none }
		if close != tokens.len - 1 {
			return none
		}
		map_type := g.infer_expression_type(tokens[..1]) or { return none }
		key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
		key_source := g.render_call_argument_expression(tokens[2..close], key_type) or {
			return none
		}
		global_key := fastc_global_key(g.module_name, tokens[0].lit)
		map_source := g.globals[global_key] or { tokens[0].lit }
		map_address := if map_type.ends_with('*') { map_source } else { '&${map_source}' }
		return FastcRenderedExpression{
			source: '({ ${key_type} __v_fastc_map_key = (${key_source}); ${value_type} __v_fastc_map_zero = (${value_type}){0}; *((${value_type} *)builtin__map_get((map *)${map_address}, &__v_fastc_map_key, &__v_fastc_map_zero)); })'
			typ:    value_type
		}
	}
	return none
}

fn (g &Parser) render_bool_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if g.selfhost || tokens.len < 4 || tokens[0].tok != .name
		|| tokens[0].lit !in ['print', 'println'] || tokens[1].tok != .lpar
		|| tokens.last().tok != .rpar {
		return none
	}
	call_end := fastc_matching_rpar(tokens, 1) or { return none }
	if call_end != tokens.len - 1 {
		return none
	}
	call_arguments := fastc_call_arguments(tokens, 1, call_end) or { return none }
	if call_arguments.len != 1 {
		return none
	}
	if !fastc_expression_tokens_contain_boolean_operator(call_arguments[0]) {
		return none
	}
	argument_type := g.infer_expression_type(call_arguments[0]) or { return none }
	if fastc_normalize_inferred_type(argument_type) != 'bool' {
		return none
	}
	argument := g.render_call_argument_expression(call_arguments[0], 'bool') or { return none }
	function_name := if tokens[0].lit == 'println' {
		'v_fastc_println_bool'
	} else {
		'v_fastc_print_bool'
	}
	return FastcRenderedExpression{
		source: '${function_name}((bool)(${argument}))'
		typ:    'void'
	}
}

fn (g &Parser) render_ordinary_string_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if g.selfhost || tokens.len < 4 || tokens[0].tok != .name
		|| tokens[0].lit !in ['print', 'println'] || tokens[1].tok != .lpar
		|| tokens.last().tok != .rpar {
		return none
	}
	call_end := fastc_matching_rpar(tokens, 1) or { return none }
	if call_end != tokens.len - 1 {
		return none
	}
	call_arguments := fastc_call_arguments(tokens, 1, call_end) or { return none }
	if call_arguments.len != 1 || !fastc_expression_tokens_contain(call_arguments[0], .plus) {
		return none
	}
	argument_type := g.infer_expression_type(call_arguments[0]) or { return none }
	if g.underlying_alias_type(argument_type).trim_right('*') != 'string' {
		return none
	}
	argument := g.render_call_argument_expression(call_arguments[0], 'string') or { return none }
	return FastcRenderedExpression{
		source: '${tokens[0].lit}(${argument})'
		typ:    'void'
	}
}

fn (g &Parser) render_enum_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .name || tokens[0].lit !in ['print', 'println']
		|| tokens[1].tok != .lpar || tokens.last().tok != .rpar {
		return none
	}
	call_end := fastc_matching_rpar(tokens, 1) or { return none }
	if call_end != tokens.len - 1 {
		return none
	}
	call_arguments := fastc_call_arguments(tokens, 1, call_end) or { return none }
	if call_arguments.len != 1 {
		return none
	}
	argument_type := g.infer_expression_type(call_arguments[0]) or { return none }
	type_key := g.semantic_type_key(argument_type)
	enum_key := g.underlying_enum_type_key(type_key) or { return none }
	c_type := fastc_c_declared_type_name(enum_key)
	argument := g.render_call_argument_expression(call_arguments[0], c_type) or { return none }
	return FastcRenderedExpression{
		source: 'v_fastc_print_enum_${c_type}(${argument}, ${if tokens[0].lit == 'println' {
			'true'
		} else {
			'false'
		}})'
		typ:    'void'
	}
}

fn (g &Parser) render_struct_literal_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut open := -1
	mut delimiter_depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr] {
			delimiter_depth++
		} else if item.tok in [.rpar, .rsbr] {
			delimiter_depth--
		} else if item.tok == .lcbr && delimiter_depth == 0 {
			open = i
			break
		}
	}
	if open <= 0 || tokens.last().tok != .rcbr {
		return none
	}
	close := fastc_matching_delimiter(tokens, open, .lcbr, .rcbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	is_c_struct_literal := open == 3 && tokens[0].tok == .name && tokens[0].lit == 'C'
		&& tokens[1].tok == .dot && tokens[2].tok == .name
	mut c_type := g.type_from_expression_tokens(tokens[..open]) or { '' }
	if c_type == '' && is_c_struct_literal {
		c_type = if '#Cstruct#${tokens[2].lit}' in g.declared_types {
			'struct ${tokens[2].lit}'
		} else {
			tokens[2].lit
		}
	}
	mut layout_type := c_type.trim_right('*')
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	}
	if c_type == '' || (!is_c_struct_literal && layout_type !in g.struct_fields
		&& g.declared_kinds[g.semantic_type_key(c_type)] !in [.struct_, .union_]) {
		return none
	}
	mut fields := map[string]string{}
	if layout_type in g.struct_fields {
		fields = g.struct_fields[layout_type].clone()
	}
	if is_c_struct_literal && open + 1 < close {
		items := fastc_expression_list_items(tokens, open + 1, close) or { return none }
		mut is_positional := false
		for item in items {
			if item.len == 0 {
				continue
			}
			if !(item.len >= 2 && item[0].tok == .name && item[1].tok == .colon) && !(item.len == 1
				&& item[0].tok == .name && item[0].lit in fields) {
				is_positional = true
				break
			}
		}
		if is_positional {
			mut values := []string{cap: items.len}
			for item in items {
				values << g.render_call_argument_expression(item, '') or { return none }
			}
			source := if c_type.ends_with('*') {
				'&(${c_type.trim_right('*')}){${values.join(',')}}'
			} else {
				'(${c_type}){${values.join(',')}}'
			}
			return FastcRenderedExpression{
				source: source
				typ:    c_type
			}
		}
	}
	mut rendered_fields := []string{}
	mut rendered_fields_by_name := map[string]string{}
	mut field_values := map[string]string{}
	mut explicit_initializers := []string{}
	mut has_applied_defaults := false
	mut update_source := ''
	mut index := open + 1
	for index < close {
		for index < close && tokens[index].tok in [.semicolon, .comma] {
			index++
		}
		if index >= close {
			break
		}
		if tokens[index].tok == .ellipsis {
			index++
			value_start := index
			for index < close && tokens[index].tok !in [.semicolon, .comma] {
				index++
			}
			if value_start == index {
				return none
			}
			update_source = g.render_call_argument_expression(tokens[value_start..index], c_type) or {
				return none
			}
			continue
		}
		if tokens[index].tok != .name {
			return none
		}
		field_name := tokens[index].lit
		index++
		mut value_tokens := []FastcExpressionToken{}
		if index < close && tokens[index].tok == .colon {
			index++
			value_start := index
			mut parens := 0
			mut brackets := 0
			mut braces := 0
			for index < close {
				match tokens[index].tok {
					.lpar {
						parens++
					}
					.rpar {
						parens--
					}
					.lsbr {
						brackets++
					}
					.rsbr {
						brackets--
					}
					.lcbr {
						braces++
					}
					.rcbr {
						braces--
					}
					.semicolon, .comma {
						if parens == 0 && brackets == 0 && braces == 0 {
							break
						}
					}
					else {}
				}
				index++
			}
			if value_start == index {
				return none
			}
			value_tokens = tokens[value_start..index].clone()
		} else {
			value_tokens = [
				FastcExpressionToken{
					tok: .name
					lit: field_name
				},
			]
		}
		mut c_field_name := if is_c_struct_literal {
			field_name
		} else {
			fastc_c_identifier(field_name)
		}
		mut expected_type := if layout_type == 'array' && field_name == 'init' {
			g.array_element_type(c_type) or { '' }
		} else {
			fields[field_name] or { '' }
		}
		if expected_type == '' && !is_c_struct_literal {
			if field := g.struct_field_metadata(c_type, field_name) {
				expected_type = field.typ
				mut storage_path := field.storage_path.clone()
				storage_path << field.name
				mut c_storage_path := []string{}
				for storage_name in storage_path {
					c_storage_path << fastc_c_identifier(storage_name)
				}
				c_field_name = c_storage_path.join('.')
			}
		}
		if fixed_element_type := fastc_fixed_array_element_type(expected_type) {
			array_end := if value_tokens.len > 0 && value_tokens.last().tok == .not {
				value_tokens.len - 1
			} else {
				value_tokens.len
			}
			if array_end >= 2 && value_tokens[0].tok == .lsbr
				&& value_tokens[array_end - 1].tok == .rsbr {
				items := fastc_expression_list_items(value_tokens, 1, array_end - 1) or {
					return none
				}
				mut values := []string{}
				for item in items {
					rendered_item := g.render_call_argument_expression(item, fixed_element_type) or {
						return none
					}
					temporary := '__v_fastc_struct_field_${explicit_initializers.len}'
					explicit_initializers << '__typeof__((${rendered_item})) ${temporary} = (${rendered_item});'
					values << temporary
				}
				rendered_field := '.${c_field_name}={${values.join(',')}}'
				rendered_fields << rendered_field
				rendered_fields_by_name[field_name] = rendered_field
				field_values[field_name] = '{${values.join(',')}}'
				continue
			}
		}
		value := g.render_call_argument_expression(value_tokens, expected_type) or { return none }
		temporary := '__v_fastc_struct_field_${explicit_initializers.len}'
		explicit_initializers << '__typeof__((${value})) ${temporary} = (${value});'
		rendered_field := '.${c_field_name}=(${temporary})'
		rendered_fields << rendered_field
		rendered_fields_by_name[field_name] = rendered_field
		field_values[field_name] = temporary
	}
	if update_source == '' {
		for field in g.struct_field_info[layout_type] {
			if field.default_value == '' || field.name in field_values {
				continue
			}
			c_field_name := fastc_c_identifier(field.name)
			rendered_field := '.${c_field_name}=(${field.default_value})'
			rendered_fields << rendered_field
			rendered_fields_by_name[field.name] = rendered_field
			field_values[field.name] = field.default_value
			has_applied_defaults = true
		}
	}
	if layout_type in g.struct_field_info {
		mut ordered_fields := []string{cap: rendered_fields.len}
		mut ordered_values := map[string]bool{}
		for field in g.struct_field_info[layout_type] {
			if rendered_field := rendered_fields_by_name[field.name] {
				ordered_fields << rendered_field
				ordered_values[rendered_field] = true
			}
		}
		for rendered_field in rendered_fields {
			if rendered_field !in ordered_values {
				ordered_fields << rendered_field
			}
		}
		rendered_fields = ordered_fields.clone()
	}
	if layout_type == 'array' {
		element_type := g.array_element_type(c_type) or { return none }
		length := field_values['len'] or { '0' }
		capacity := field_values['cap'] or { '0' }
		base := '((${c_type})builtin____new_array(${length},${capacity},sizeof(${element_type})))'
		if initial := field_values['init'] {
			return FastcRenderedExpression{
				source: '({ ${explicit_initializers.join(' ')} ${c_type} __v_fastc_array_init = ${base}; ${element_type} __v_fastc_array_default = (${initial}); for (int __v_fastc_array_index = 0; __v_fastc_array_index < __v_fastc_array_init.len; __v_fastc_array_index++) { ((${element_type} *)__v_fastc_array_init.data)[__v_fastc_array_index] = __v_fastc_array_default; } __v_fastc_array_init; })'
				typ:    c_type
			}
		}
		if explicit_initializers.len > 0 {
			return FastcRenderedExpression{
				source: '({ ${explicit_initializers.join(' ')} ${base}; })'
				typ:    c_type
			}
		}
		return FastcRenderedExpression{
			source: base
			typ:    c_type
		}
	}
	if update_source != '' {
		mut assignments := []string{cap: rendered_fields.len}
		for field in rendered_fields {
			assignments << '__v_fastc_struct_update${field};'
		}
		if c_type.ends_with('*') {
			base_type := c_type.trim_right('*')
			return FastcRenderedExpression{
				source: '({ ${base_type} __v_fastc_struct_update = *(${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} (${c_type})v_fastc_interface_box(&__v_fastc_struct_update, sizeof(${base_type})); })'
				typ:    c_type
			}
		}
		return FastcRenderedExpression{
			source: '({ ${c_type} __v_fastc_struct_update = (${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} __v_fastc_struct_update; })'
			typ:    c_type
		}
	}
	if has_applied_defaults {
		base_type := c_type.trim_right('*')
		mut assignments := []string{cap: rendered_fields.len}
		mut initializers := []string{}
		mut initialized_fields := map[string]bool{}
		for field in g.struct_field_info[layout_type] {
			if fastc_fixed_array_element_type(field.typ) == none {
				continue
			}
			if rendered_field := rendered_fields_by_name[field.name] {
				initializers << rendered_field
				initialized_fields[rendered_field] = true
			}
		}
		for field in rendered_fields {
			if field in initialized_fields {
				continue
			}
			assignments << '__v_fastc_struct_default${field};'
		}
		initializer := if initializers.len > 0 {
			'(${base_type}){${initializers.join(',')}}'
		} else {
			'(${base_type}){0}'
		}
		result := if c_type.ends_with('*') {
			'(${c_type})v_fastc_interface_box(&__v_fastc_struct_default, sizeof(${base_type}))'
		} else {
			'__v_fastc_struct_default'
		}
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${base_type} __v_fastc_struct_default = ${initializer}; ${assignments.join(' ')} ${result}; })'
			typ:    c_type
		}
	}
	literal_source := if c_type.ends_with('*') {
		'(${c_type})v_fastc_interface_box(&(${c_type.trim_right('*')}){${rendered_fields.join(',')}}, sizeof(${c_type.trim_right('*')}))'
	} else {
		'(${c_type}){${rendered_fields.join(',')}}'
	}
	if explicit_initializers.len > 0 {
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${literal_source}; })'
			typ:    c_type
		}
	}
	return FastcRenderedExpression{
		source: literal_source
		typ:    c_type
	}
}

fn (g &Parser) render_struct_literal_field_names(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut open := -1
	for i, item in tokens {
		if item.tok == .lcbr {
			open = i
			break
		}
	}
	if open <= 0 || tokens.last().tok != .rcbr {
		return none
	}
	c_type := g.type_from_expression_tokens(tokens[..open]) or { return none }
	fields := g.struct_fields[c_type.trim_right('*')].clone()
	mut rendered := rendered_expression
	mut changed := false
	for field_name in fields.keys() {
		for module_name in [g.module_name, 'builtin'] {
			constant_name := g.constants[fastc_constant_key(module_name, field_name)] or {
				continue
			}
			needle := '.${constant_name}='
			if rendered.contains(needle) {
				rendered = rendered.replace(needle, '.${fastc_c_identifier(field_name)}=')
				changed = true
			}
		}
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    c_type
		}
	} else {
		none
	}
}

fn (g &Parser) render_array_assignment_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut assignment_index := -1
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok.is_assignment() {
			assignment_index = i
			break
		}
	}
	if assignment_index <= 0 || assignment_index + 1 >= tokens.len
		|| tokens[assignment_index - 1].tok != .rsbr {
		return none
	}
	left := g.render_array_access_expression(tokens[..assignment_index]) or { return none }
	right := g.render_call_argument_expression(tokens[assignment_index + 1..], left.typ) or {
		return none
	}
	operator := tokens[assignment_index].tok
	source := if operator == .right_shift_unsigned_assign {
		g.render_unsigned_right_shift_assignment(left.source, right, left.typ) or { return none }
	} else {
		'${left.source}${operator.str()}${right}'
	}
	return FastcRenderedExpression{
		source: source
		typ:    left.typ
	}
}

fn (g &Parser) render_initializer_assignment_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	initializer_start := fastc_initializer_type_start(tokens)
	if initializer_start <= 0 || initializer_start >= tokens.len {
		return none
	}
	mut initializer_open := -1
	for i in initializer_start .. tokens.len {
		if tokens[i].tok == .lcbr {
			initializer_open = i
			break
		}
	}
	if initializer_open <= initializer_start || tokens.last().tok != .rcbr {
		return none
	}
	initializer_type_tokens := tokens[initializer_start..initializer_open]
	if g.array_initializer_type(initializer_type_tokens) == none
		&& g.map_initializer_type(initializer_type_tokens) == none {
		return none
	}
	return g.render_assignment_expression(tokens)
}

fn (g &Parser) render_assignment_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut assignment_index := -1
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok.is_assignment() {
			assignment_index = i
			break
		}
	}
	if assignment_index <= 0 || assignment_index + 1 >= tokens.len {
		return none
	}
	left_tokens := tokens[..assignment_index]
	left_type := g.infer_expression_type(left_tokens) or { return none }
	if left_type == '' {
		return none
	}
	mut left := ''
	if array_access := g.render_array_access_expression(left_tokens) {
		left = array_access.source
	} else if member := g.render_member_receiver(left_tokens) {
		left = member
	} else {
		raw := g.render_raw_expression_tokens(left_tokens) or { return none }
		left = if pointer_members := g.render_pointer_member_access_expression(left_tokens, raw) {
			pointer_members.source
		} else {
			raw
		}
	}
	right := g.render_call_argument_expression(tokens[assignment_index + 1..], left_type) or {
		return none
	}
	operator := tokens[assignment_index].tok
	source := if operator == .plus_assign && g.underlying_alias_type(left_type) == 'string' {
		'${left}=builtin__string_plus(${left},${right})'
	} else if operator == .right_shift_unsigned_assign {
		g.render_unsigned_right_shift_assignment(left, right, left_type) or { return none }
	} else {
		'${left}${operator.str()}${right}'
	}
	return FastcRenderedExpression{
		source: source
		typ:    left_type
	}
}

fn (g &Parser) render_unsigned_right_shift_assignment(target string, value string, target_type string) ?string {
	resolved_type := g.underlying_alias_type(target_type).trim_right('*')
	unsigned_type, bits := match resolved_type {
		'byte', 'char', 'i8', 'u8' { 'u8', '8' }
		'i16', 'u16' { 'u16', '16' }
		'i32', 'int', 'rune', 'u32', 'unsigned int' { 'u32', '32' }
		'i64', 'u64' { 'u64', '64' }
		'isize', 'usize' { 'usize', '${g.prefs.target.pointer_bits}' }
		else { return none }
	}
	return '({ ${target_type} *__v_fastc_unsigned_shift_target = &(${target}); ${unsigned_type} __v_fastc_unsigned_shift_value = (${unsigned_type})(*__v_fastc_unsigned_shift_target); u64 __v_fastc_unsigned_shift_count = (u64)(${value}); *__v_fastc_unsigned_shift_target = (${target_type})(__v_fastc_unsigned_shift_count >= ${bits} ? (${unsigned_type})0 : (__v_fastc_unsigned_shift_value >> __v_fastc_unsigned_shift_count)); })'
}

fn (g &Parser) render_pointer_member_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok == .dot && tokens[i + 1].tok == .name && i + 2 < tokens.len
			&& tokens[i + 2].tok == .lpar {
			return none
		}
	}
	mut rendered := rendered_expression
	mut changed := false
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i)
		receiver_tokens := tokens[receiver_start..i]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		if !receiver_type.ends_with('*') {
			continue
		}
		receiver_source := g.render_member_receiver(receiver_tokens) or {
			g.render_membership_candidate(receiver_tokens, '') or { continue }
		}
		needle := '${receiver_source}.${tokens[i + 1].lit}'
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, '${receiver_source}->${tokens[i + 1].lit}')
			changed = true
			continue
		}
		raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
		raw_needle := '${raw_receiver}.${tokens[i + 1].lit}'
		if raw_receiver != '' && rendered.contains(raw_needle) {
			rendered = rendered.replace(raw_needle, '${raw_receiver}->${tokens[i + 1].lit}')
			changed = true
			continue
		}
		parenthesized_needle := ').${tokens[i + 1].lit}'
		if receiver_tokens.last().tok == .rpar && rendered.contains(parenthesized_needle) {
			rendered = rendered.replace(parenthesized_needle, ')->${tokens[i + 1].lit}')
			changed = true
		}
	}
	if chained_array := g.render_chained_array_access_expression(tokens, rendered) {
		rendered = chained_array.source
		changed = true
	}
	if !changed {
		return none
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ:    inferred_type
	}
}

fn (g &Parser) render_chained_array_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut changed := false
	for open, item in tokens {
		if item.tok != .lsbr {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close <= open + 1 || fastc_expression_tokens_contain(tokens[open + 1..close], .dotdot) {
			continue
		}
		start := fastc_method_receiver_start(tokens, open)
		if start >= open {
			continue
		}
		base_tokens := tokens[start..open]
		base_type := g.infer_expression_type(base_tokens) or { continue }
		is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
		element_type := if base_type == 'string' {
			'u8'
		} else if is_array_pointer {
			g.array_element_type(base_type) or { continue }
		} else if base_type.ends_with('*') {
			base_type.trim_right('*')
		} else {
			g.array_element_type(base_type) or { continue }
		}
		raw_base := g.render_raw_expression_tokens(base_tokens) or { continue }
		base_is_global_or_constant := base_tokens.len == 1
			&& (fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals
			|| fastc_constant_key(g.module_name, base_tokens[0].lit) in g.constants
			|| base_tokens[0].lit in g.constants)
		base_source := if base_is_global_or_constant {
			raw_base
		} else {
			g.render_member_receiver(base_tokens) or { raw_base }
		}
		index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or {
			continue
		}
		is_raw_fixed_array := base_type.trim_right('*').starts_with('FixedArray_')
			&& (base_tokens.len > 1 || (base_tokens.len == 1
			&& fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals))
		replacement := if checked_access := g.render_array_access_expression(tokens[start..close + 1]) {
			checked_access.source
		} else if is_raw_fixed_array {
			'((${base_source})[${index_source}])'
		} else if base_type.ends_with('*') && !is_array_pointer {
			'((${base_source})[${index_source}])'
		} else {
			array_value := if base_type.ends_with('*') { '*(${base_source})' } else { base_source }
			'(*(${element_type} *)builtin__array_get(${array_value}, ${index_source}))'
		}
		mut needle := '${base_source}[${index_source}]'
		if !rendered.contains(needle) {
			needle = '${raw_base}[${index_source}]'
		}
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, replacement)
			changed = true
		}
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    inferred_type
		}
	} else {
		none
	}
}

fn (g &Parser) render_logical_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.and, .logical_or] && i > 0 && i + 1 < tokens.len {
			left := g.render_call_argument_expression(tokens[..i], 'bool') or { return none }
			right := g.render_call_argument_expression(tokens[i + 1..], 'bool') or { return none }
			return FastcRenderedExpression{
				source: '((${left})${if item.tok == .and { '&&' } else { '||' }}(${right}))'
				typ:    'bool'
			}
		}
	}
	return none
}

fn (g &Parser) struct_equality_is_supported(typ string, seen []string) bool {
	if fastc_is_pointer_type(typ) {
		return true
	}
	layout_type := g.underlying_alias_type(typ).trim_right('*')
	if layout_type == 'string' || layout_type == 'bool'
		|| fastc_is_numeric_expression_type(layout_type) {
		return true
	}
	if element_type := fastc_fixed_array_element_type(layout_type) {
		length_source := fastc_fixed_array_length(layout_type) or { return false }
		_ = g.fixed_array_length_value(length_source) or { return false }
		return g.struct_equality_is_supported(element_type, seen)
	}
	if layout_type in ['Option', 'array', 'map'] || layout_type.starts_with('Array_')
		|| layout_type.starts_with('Map_') {
		return false
	}
	type_key := g.semantic_type_key(layout_type)
	if type_key in g.declared_kinds && g.declared_kinds[type_key] == .enum_ {
		return true
	}
	if type_key !in g.declared_kinds || g.declared_kinds[type_key] != .struct_
		|| layout_type !in g.struct_field_info {
		return false
	}
	if layout_type in seen {
		return true
	}
	mut nested_seen := seen.clone()
	nested_seen << layout_type
	for field in g.struct_field_info[layout_type] {
		if !g.struct_equality_is_supported(field.typ, nested_seen) {
			return false
		}
	}
	return true
}

fn (g &Parser) struct_equality_source(left string, right string, typ string, seen []string) string {
	if fastc_is_pointer_type(typ) {
		return '((${left}) == (${right}))'
	}
	layout_type := g.underlying_alias_type(typ).trim_right('*')
	if layout_type == 'string' {
		return 'builtin__string_eq(${left}, ${right})'
	}
	if layout_type == 'bool' || fastc_is_numeric_expression_type(layout_type) {
		return '((${left}) == (${right}))'
	}
	if element_type := fastc_fixed_array_element_type(layout_type) {
		length_source := fastc_fixed_array_length(layout_type) or { return 'false' }
		length := g.fixed_array_length_value(length_source) or { return 'false' }
		mut comparisons := []string{cap: length}
		for index in 0 .. length {
			comparisons << g.struct_equality_source('(${left})[${index}]', '(${right})[${index}]',
				element_type, seen)
		}
		return if comparisons.len == 0 { 'true' } else { '(${comparisons.join(' && ')})' }
	}
	type_key := g.semantic_type_key(layout_type)
	if type_key in g.declared_kinds && g.declared_kinds[type_key] == .enum_ {
		return '((${left}) == (${right}))'
	}
	if type_key !in g.declared_kinds || g.declared_kinds[type_key] != .struct_
		|| layout_type in seen {
		return '((${left}) == (${right}))'
	}
	mut nested_seen := seen.clone()
	nested_seen << layout_type
	mut comparisons := []string{}
	for field in g.struct_field_info[layout_type] {
		field_name := fastc_c_identifier(field.name)
		comparisons << g.struct_equality_source('(${left}).${field_name}',
			'(${right}).${field_name}', field.typ, nested_seen)
	}
	return if comparisons.len == 0 { 'true' } else { '(${comparisons.join(' && ')})' }
}

fn (g &Parser) render_struct_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	memo_key := fastc_comparison_memo_key(tokens, 0)
	if memo_key != 0 {
		if cached := g.comparison_memo[memo_key] {
			if cached.source == '' {
				return none
			}
			return cached
		}
	}
	mut result := FastcRenderedExpression{}
	if rendered := g.render_struct_comparison_expression_impl(tokens) {
		result = rendered
	}
	if memo_key != 0 {
		mut w := unsafe { &Parser(g) }
		w.comparison_memo[memo_key] = result
	}
	if result.source == '' {
		return none
	}
	return result
}

fn (g &Parser) render_struct_comparison_expression_impl(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len >= 3 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		close := fastc_matching_rpar(tokens, 0) or { -1 }
		if close == tokens.len - 1 {
			inner := g.render_struct_comparison_expression(tokens[1..tokens.len - 1]) or {
				return none
			}
			return FastcRenderedExpression{
				source: '((${inner.source}))'
				typ:    'bool'
			}
		}
	}
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.and, .logical_or] && i > 0 && i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			mut left_comparison := FastcRenderedExpression{}
			if comparison := g.render_struct_comparison_expression(left_tokens) {
				left_comparison = comparison
			}
			mut right_comparison := FastcRenderedExpression{}
			if comparison := g.render_struct_comparison_expression(right_tokens) {
				right_comparison = comparison
			}
			if left_comparison.source == '' && right_comparison.source == '' {
				continue
			}
			left := if left_comparison.source != '' {
				left_comparison.source
			} else {
				g.render_call_argument_expression(left_tokens, 'bool') or { return none }
			}
			right := if right_comparison.source != '' {
				right_comparison.source
			} else {
				g.render_call_argument_expression(right_tokens, 'bool') or { return none }
			}
			return FastcRenderedExpression{
				source: '((${left}) ${if item.tok == .and { '&&' } else { '||' }} (${right}))'
				typ:    'bool'
			}
		}
	}
	if tokens.len > 1 && tokens[0].tok == .not {
		inner := g.render_struct_comparison_expression(tokens[1..]) or { return none }
		return FastcRenderedExpression{
			source: '!(${inner.source})'
			typ:    'bool'
		}
	}
	depth = 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.eq, .ne] && i > 0 && i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			left_inferred_type := g.infer_expression_type(left_tokens) or { return none }
			right_inferred_type := g.infer_expression_type(right_tokens) or { return none }
			left_type := fastc_normalize_inferred_type(left_inferred_type)
			right_type := fastc_normalize_inferred_type(right_inferred_type)
			if fastc_is_pointer_type(left_type) || fastc_is_pointer_type(right_type) {
				return none
			}
			left_layout := g.underlying_alias_type(left_type).trim_right('*')
			right_layout := g.underlying_alias_type(right_type).trim_right('*')
			left_key := g.semantic_type_key(left_layout)
			if left_layout != right_layout || left_key !in g.declared_kinds
				|| g.declared_kinds[left_key] != .struct_
				|| !g.struct_equality_is_supported(left_type, []string{}) {
				return none
			}
			left := g.render_comparison_operand(left_tokens, left_type) or { return none }
			right := g.render_comparison_operand(right_tokens, right_type) or { return none }
			equality := g.struct_equality_source('__v_fastc_eq_left', '__v_fastc_eq_right',
				left_type, []string{})
			result := if item.tok == .ne { '!(${equality})' } else { equality }
			return FastcRenderedExpression{
				source: '({ ${left_type} __v_fastc_eq_left = (${left}); ${right_type} __v_fastc_eq_right = (${right}); ${result}; })'
				typ:    'bool'
			}
		}
	}
	return none
}

fn (g &Parser) render_enum_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.eq, .ne, .lt, .gt, .le, .ge] && i > 0
			&& i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			mut left_type := g.infer_expression_type(left_tokens) or { '' }
			mut right_type := g.infer_expression_type(right_tokens) or { '' }
			if left_type == '' && left_tokens.len > 2
				&& left_tokens[left_tokens.len - 2].tok == .dot && left_tokens.last().tok == .name {
				receiver_type := g.infer_expression_type(left_tokens[..left_tokens.len - 2]) or {
					''
				}
				left_type = g.struct_member_type(receiver_type, left_tokens.last().lit)
			}
			if right_type == '' && right_tokens.len > 2
				&& right_tokens[right_tokens.len - 2].tok == .dot
				&& right_tokens.last().tok == .name {
				receiver_type := g.infer_expression_type(right_tokens[..right_tokens.len - 2]) or {
					''
				}
				right_type = g.struct_member_type(receiver_type, right_tokens.last().lit)
			}
			if g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_tokens.len == 2
				&& right_tokens[0].tok == .dot && right_tokens[1].tok == .name {
				left := g.render_call_argument_expression(left_tokens, left_type) or { return none }
				enum_type := left_type.trim_right('*')
				return FastcRenderedExpression{
					source: '((${left}) ${item.tok.str()} (${enum_type}__${right_tokens[1].lit}))'
					typ:    'bool'
				}
			}
			if g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_tokens.len == 2
				&& left_tokens[0].tok == .dot && left_tokens[1].tok == .name {
				right := g.render_call_argument_expression(right_tokens, right_type) or {
					return none
				}
				enum_type := right_type.trim_right('*')
				return FastcRenderedExpression{
					source: '((${enum_type}__${left_tokens[1].lit}) ${item.tok.str()} (${right}))'
					typ:    'bool'
				}
			}
		}
	}
	return none
}

fn (g &Parser) render_option_none_comparison(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.eq, .ne] && i > 0 && i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			left_is_none := left_tokens.len == 1 && left_tokens[0].tok == .key_none
			right_is_none := right_tokens.len == 1 && right_tokens[0].tok == .key_none
			if left_is_none == right_is_none {
				return none
			}
			value_tokens := if left_is_none { right_tokens } else { left_tokens }
			value := g.render_call_argument_expression(value_tokens, 'Option') or { return none }
			operator := if item.tok == .eq { '==' } else { '!=' }
			return FastcRenderedExpression{
				source: '((${value}).state ${operator} 2)'
				typ:    'bool'
			}
		}
	}
	return none
}

fn (g &Parser) render_string_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 || item.tok !in [.and, .logical_or] || i == 0 || i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		mut left_special := FastcRenderedExpression{}
		if special := g.render_string_comparison_expression(left_tokens) {
			left_special = special
		}
		mut right_special := FastcRenderedExpression{}
		if special := g.render_string_comparison_expression(right_tokens) {
			right_special = special
		}
		if left_special.source == '' && right_special.source == '' {
			continue
		}
		left_source := if left_special.source != '' {
			left_special.source
		} else {
			g.render_comparison_operand(left_tokens, '') or { return none }
		}
		right_source := if right_special.source != '' {
			right_special.source
		} else {
			g.render_comparison_operand(right_tokens, '') or { return none }
		}
		return FastcRenderedExpression{
			source: '(${left_source}${if item.tok == .and { '&&' } else { '||' }}${right_source})'
			typ:    'bool'
		}
	}
	depth = 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0
			|| i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		left_type := g.infer_expression_type(left_tokens) or { return none }
		right_type := g.infer_expression_type(right_tokens) or { return none }
		if g.underlying_alias_type(left_type).trim_right('*') != 'string'
			|| g.underlying_alias_type(right_type).trim_right('*') != 'string' {
			return none
		}
		left_source := g.render_comparison_operand(left_tokens, 'string') or { return none }
		right_source := g.render_comparison_operand(right_tokens, 'string') or { return none }
		source := match item.tok {
			.eq { 'builtin__string_eq(${left_source},${right_source})' }
			.ne { '!builtin__string_eq(${left_source},${right_source})' }
			.lt { 'builtin__string_lt(${left_source},${right_source})' }
			.gt { 'builtin__string_lt(${right_source},${left_source})' }
			.le { '!builtin__string_lt(${right_source},${left_source})' }
			.ge { '!builtin__string_lt(${left_source},${right_source})' }
			else { return none }
		}
		return FastcRenderedExpression{
			source: source
			typ:    'bool'
		}
	}
	return none
}

fn (g &Parser) render_mixed_integer_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	memo_key := fastc_comparison_memo_key(tokens, 1)
	if memo_key != 0 {
		if cached := g.comparison_memo[memo_key] {
			if cached.source == '' {
				return none
			}
			return cached
		}
	}
	mut result := FastcRenderedExpression{}
	if rendered := g.render_mixed_integer_comparison_expression_impl(tokens) {
		result = rendered
	}
	if memo_key != 0 {
		mut w := unsafe { &Parser(g) }
		w.comparison_memo[memo_key] = result
	}
	if result.source == '' {
		return none
	}
	return result
}

fn (g &Parser) render_mixed_integer_comparison_expression_impl(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len >= 3 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		close := fastc_matching_rpar(tokens, 0) or { -1 }
		if close == tokens.len - 1 {
			inner := g.render_mixed_integer_comparison_expression(tokens[1..tokens.len - 1]) or {
				return none
			}
			return FastcRenderedExpression{
				source: '((${inner.source}))'
				typ:    'bool'
			}
		}
	}
	if tokens.len > 1 && tokens[0].tok == .not {
		inner := g.render_mixed_integer_comparison_expression(tokens[1..]) or { return none }
		return FastcRenderedExpression{
			source: '!(${inner.source})'
			typ:    'bool'
		}
	}
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 || item.tok !in [.and, .logical_or] || i == 0 || i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		mut left_special := FastcRenderedExpression{}
		if special := g.render_mixed_integer_comparison_expression(left_tokens) {
			left_special = special
		}
		mut right_special := FastcRenderedExpression{}
		if special := g.render_mixed_integer_comparison_expression(right_tokens) {
			right_special = special
		}
		if left_special.source == '' && right_special.source == '' {
			continue
		}
		left_source := if left_special.source != '' {
			left_special.source
		} else {
			g.render_comparison_operand(left_tokens, 'bool') or { return none }
		}
		right_source := if right_special.source != '' {
			right_special.source
		} else {
			g.render_comparison_operand(right_tokens, 'bool') or { return none }
		}
		return FastcRenderedExpression{
			source: '((${left_source})${if item.tok == .and { '&&' } else { '||' }}(${right_source}))'
			typ:    'bool'
		}
	}
	depth = 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0
			|| i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		left_inferred_type := g.infer_expression_type(left_tokens) or { return none }
		right_inferred_type := g.infer_expression_type(right_tokens) or { return none }
		left_type := g.underlying_alias_type(left_inferred_type)
		right_type := g.underlying_alias_type(right_inferred_type)
		left_is_unsigned := fastc_is_wide_unsigned_integer_type(left_type)
		right_is_unsigned := fastc_is_wide_unsigned_integer_type(right_type)
		left_is_signed := fastc_is_signed_integer_type(left_type)
		right_is_signed := fastc_is_signed_integer_type(right_type)
		if !(left_is_unsigned && right_is_signed) && !(right_is_unsigned && left_is_signed) {
			return none
		}
		left_source := g.render_comparison_operand(left_tokens, left_type) or { return none }
		right_source := g.render_comparison_operand(right_tokens, right_type) or { return none }
		mut operation := match item.tok {
			.eq { 'eq' }
			.ne { 'ne' }
			.gt { 'gt' }
			.lt { 'lt' }
			.ge { 'ge' }
			.le { 'le' }
			else { return none }
		}
		unsigned_source := if left_is_unsigned { left_source } else { right_source }
		signed_source := if left_is_signed { left_source } else { right_source }
		if right_is_unsigned {
			operation = match operation {
				'gt' { 'lt' }
				'lt' { 'gt' }
				'ge' { 'le' }
				'le' { 'ge' }
				else { operation }
			}
		}
		return FastcRenderedExpression{
			source: 'v_fastc_us_${operation}((u64)(${unsigned_source}), (i64)(${signed_source}))'
			typ:    'bool'
		}
	}
	return none
}

fn (g &Parser) render_comparison_operand(tokens []FastcExpressionToken, expected_type string) ?string {
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if integer_comparison := g.render_mixed_integer_comparison_expression(tokens) {
		return integer_comparison.source
	}
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation.source
	}
	if method_call := g.render_method_call_expression(tokens, raw) {
		return method_call.source
	}
	if call := g.render_missing_call_arguments(tokens) {
		return call.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		return pointer_members.source
	}
	return g.render_membership_candidate(tokens, expected_type)
}

fn (g &Parser) render_call_argument_expression(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len > 1 && tokens[0].tok == .amp && tokens[0].is_mut_argument {
		inner_expected_type := expected_type.trim_right('*')
		inner := g.render_call_argument_expression(tokens[1..], inner_expected_type) or {
			return none
		}
		if fastc_expression_tokens_contain(tokens[1..], .dotdot) {
			return '({ __typeof__((${inner})) __v_fastc_mut_argument = (${inner}); &__v_fastc_mut_argument; })'
		}
		return '&(${inner})'
	}
	if tokens.len >= 2 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		close := fastc_matching_rpar(tokens, 0) or { -1 }
		if close == tokens.len - 1 {
			inner := g.render_call_argument_expression(tokens[1..tokens.len - 1], expected_type) or {
				return none
			}
			return '(${inner})'
		}
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if tokens.len == 1 && tokens[0].tok == .name {
		if local := g.locals[tokens[0].lit] {
			if local.is_reference {
				value_type := local.typ.trim_right('*')
				if fastc_is_pointer_type(expected_type) {
					return raw
				}
				if expected_type != '' && (expected_type == value_type
					|| fastc_selfhost_types_share_lowering_representation(value_type, expected_type)) {
					return '*(${raw})'
				}
			}
		}
	}
	mut rendered := ''
	if special := g.render_special_expression(tokens, raw) {
		rendered = special.source
	} else {
		rendered = g.render_membership_candidate(tokens, expected_type) or { return none }
	}
	rendered = g.render_constant_references(tokens, rendered)
	actual_type := g.infer_expression_type(tokens) or { '' }
	if expected_type == 'string' && actual_type.trim_right('*') == 'IError' {
		return 'builtin__IError_msg(${rendered})'
	}
	if actual_type.ends_with('*') && expected_type == actual_type.trim_right('*')
		&& expected_type.starts_with('Map_') {
		return '*(${rendered})'
	}
	if expected_type.ends_with('*') && actual_type == expected_type.trim_right('*')
		&& actual_type.starts_with('Map_') {
		return '&(${rendered})'
	}
	return rendered
}

fn (g &Parser) render_map_lookup_option_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut start := 0
	for start < tokens.len && tokens[start].tok == .lpar {
		start++
	}
	lookup_tokens := tokens[start..]
	if lookup_tokens.len < 4 || lookup_tokens.last().tok != .rsbr {
		return none
	}
	mut open := -1
	mut depth := 0
	for i := lookup_tokens.len - 1; i >= 0; i-- {
		if lookup_tokens[i].tok == .rsbr {
			depth++
		} else if lookup_tokens[i].tok == .lsbr {
			depth--
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open <= 0 {
		return none
	}
	base_tokens := lookup_tokens[..open]
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
	mut map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
		g.globals[fastc_global_key(g.module_name, base_tokens[0].lit)] or { base_tokens[0].lit }
	} else {
		g.render_member_receiver(base_tokens) or { return none }
	}
	if map_type.ends_with('*') {
		map_source = '*(${map_source})'
	}
	key_source := g.render_membership_candidate(lookup_tokens[open + 1..lookup_tokens.len - 1],
		key_type) or { return none }
	return FastcRenderedExpression{
		source: '({ ${key_type} __v_fastc_map_key = (${key_source}); ${value_type} *__v_fastc_map_value = (${value_type} *)builtin__map_get_check((map *)&(${map_source}), &__v_fastc_map_key); (Option){.data=__v_fastc_map_value, .state=__v_fastc_map_value == NULL ? 2 : 0}; })'
		typ:    value_type
	}
}

fn (g &Parser) render_array_lookup_option_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut start := 0
	for start < tokens.len && tokens[start].tok == .lpar {
		start++
	}
	lookup_tokens := tokens[start..]
	if lookup_tokens.len < 4 || lookup_tokens.last().tok != .rsbr {
		return none
	}
	mut open := -1
	mut depth := 0
	for i := lookup_tokens.len - 1; i >= 0; i-- {
		if lookup_tokens[i].tok == .rsbr {
			depth++
		} else if lookup_tokens[i].tok == .lsbr {
			depth--
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open <= 0 || open + 1 == lookup_tokens.len - 1 {
		return none
	}
	base_tokens := lookup_tokens[..open]
	base_type := g.infer_expression_type(base_tokens) or { return none }
	if !base_type.trim_right('*').starts_with('Array_') {
		return none
	}
	element_type := g.array_element_type(base_type) or { return none }
	base_source := g.render_call_argument_expression(base_tokens, base_type) or { return none }
	array_source := if base_type.ends_with('*') { '*(${base_source})' } else { base_source }
	index_source := g.render_membership_candidate(lookup_tokens[open + 1..lookup_tokens.len - 1],
		'int') or { return none }
	return FastcRenderedExpression{
		source: '({ int __v_fastc_array_index = (${index_source}); ${base_type.trim_right('*')} __v_fastc_array = (${array_source}); bool __v_fastc_array_missing = __v_fastc_array_index < 0 || __v_fastc_array_index >= __v_fastc_array.len; ${element_type} *__v_fastc_array_value = __v_fastc_array_missing ? NULL : (${element_type} *)((byteptr)__v_fastc_array.data + (usize)__v_fastc_array_index * (usize)__v_fastc_array.element_size); (Option){.data=__v_fastc_array_value, .state=__v_fastc_array_missing ? 2 : 0}; })'
		typ:    element_type
	}
}

fn fastc_map_runtime_functions(key_type string, pointer_bits int) (string, string, string, string) {
	if key_type == 'string' {
		return 'builtin__map_hash_string', 'builtin__map_eq_string', 'builtin__map_clone_string', 'builtin__map_free_string'
	}
	suffix := if key_type in ['i8', 'u8', 'byte', 'char', 'bool'] {
		'1'
	} else if key_type in ['i16', 'u16'] {
		'2'
	} else if key_type in ['i64', 'u64'] {
		'8'
	} else if key_type in ['isize', 'usize'] || fastc_is_pointer_type(key_type) {
		if pointer_bits == 32 { '4' } else { '8' }
	} else {
		'4'
	}
	return 'builtin__map_hash_int_${suffix}', 'builtin__map_eq_int_${suffix}', 'builtin__map_clone_int_${suffix}', 'builtin__map_free_nop'
}

fn (g &Parser) map_runtime_functions(key_type string) (string, string, string, string) {
	mut resolved_type := g.underlying_alias_type(key_type)
	if enum_key := g.underlying_enum_type_key(g.semantic_type_key(resolved_type)) {
		resolved_type = if g.enum_flags[enum_key] { 'u64' } else { 'int' }
	}
	return fastc_map_runtime_functions(resolved_type, g.prefs.target.pointer_bits)
}

fn (g &Parser) render_missing_call_arguments(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 3 || tokens.last().tok != .rpar {
		return none
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name
		&& (tokens[0].lit in g.imports || tokens[0].lit == 'C') {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok != .name || tokens[open_index].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar(tokens, open_index) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	function_key := g.function_key_for_call(tokens, name_index)
	signature := g.functions[function_key] or { return none }
	if signature.is_disabled {
		return FastcRenderedExpression{
			source: fastc_disabled_call_expression(signature.return_type)
			typ:    signature.return_type
		}
	}
	call_args := fastc_call_arguments(tokens, open_index, close) or { return none }
	mut named_start := -1
	for i, argument in call_args {
		if argument.len >= 3 && argument[0].tok == .name && argument[1].tok == .colon {
			named_start = i
			break
		}
	}
	if named_start >= 0 && named_start < signature.parameter_types.len {
		mut rendered_arguments := []string{}
		for argument_index, argument in call_args[..named_start] {
			expected_type := if argument_index < signature.parameter_types.len {
				signature.parameter_types[argument_index]
			} else {
				''
			}
			rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
				return none
			}
			rendered_arguments << rendered_argument
		}
		parameter_type := signature.parameter_types[named_start]
		mut fields := []string{}
		for argument in call_args[named_start..] {
			if argument.len < 3 || argument[0].tok != .name || argument[1].tok != .colon {
				return none
			}
			value := g.render_call_argument_expression(argument[2..], '') or { return none }
			fields << '.${fastc_c_identifier(argument[0].lit)}=${value}'
		}
		rendered_arguments << '(${parameter_type}){${fields.join(',')}}'
		return FastcRenderedExpression{
			source: '${fastc_c_function_name_for_key(function_key)}(${rendered_arguments.join(',')})'
			typ:    signature.return_type
		}
	}
	if signature.is_variadic && !function_key.starts_with('C.') {
		fixed_arguments := signature.parameter_types.len - 1
		if call_args.len < fixed_arguments {
			return none
		}
		variadic_type := signature.parameter_types.last()
		element_type := g.array_element_type(variadic_type) or { return none }
		mut rendered_arguments := []string{}
		for argument_index, argument in call_args {
			expected_type := if argument_index < fixed_arguments {
				signature.parameter_types[argument_index]
			} else {
				element_type
			}
			rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
				return none
			}
			rendered_arguments << rendered_argument
		}
		variadic_arguments := rendered_arguments[fixed_arguments..].clone()
		packed := if variadic_arguments.len == 0 {
			'(${variadic_type}){0}'
		} else {
			'((${variadic_type})builtin__new_array_from_c_array(${variadic_arguments.len}, ${variadic_arguments.len}, sizeof(${element_type}), (${element_type}[]){${variadic_arguments.join(',')}}))'
		}
		mut c_arguments := rendered_arguments[..fixed_arguments].clone()
		c_arguments << packed
		return FastcRenderedExpression{
			source: '${fastc_c_function_name_for_key(function_key)}(${c_arguments.join(',')})'
			typ:    signature.return_type
		}
	}
	if call_args.len < signature.parameter_types.len && (!signature.last_parameter_is_params
		|| call_args.len + 1 != signature.parameter_types.len) {
		return none
	}
	mut rendered_arguments := []string{cap: signature.parameter_types.len}
	for argument_index, argument in call_args {
		expected_type := if argument_index < signature.parameter_types.len {
			signature.parameter_types[argument_index]
		} else {
			''
		}
		rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
			return none
		}
		rendered_arguments << rendered_argument
	}
	for parameter_type in signature.parameter_types[call_args.len..] {
		rendered_arguments << '(${parameter_type}){0}'
	}
	call_name := if function_key.starts_with('C.') {
		function_key.all_after_last('.')
	} else {
		fastc_c_function_name_for_key(function_key)
	}
	return FastcRenderedExpression{
		source: '${call_name}(${rendered_arguments.join(',')})'
		typ:    signature.return_type
	}
}

fn (g &Parser) render_append_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut depth := 0
	mut operator_index := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.left_shift {
				if depth == 0 {
					operator_index = i
					break
				}
			}
			else {}
		}
	}
	if operator_index <= 0 || operator_index + 1 >= tokens.len {
		return none
	}
	left_type := g.infer_expression_type(tokens[..operator_index]) or { return none }
	if g.array_element_type(left_type) == none {
		return none
	}
	separator := rendered_expression.index('<<') or { return none }
	left_source := rendered_expression[..separator]
	right_source := rendered_expression[separator + 2..]
	temporary := '__v_fastc_append_value'
	return FastcRenderedExpression{
		source: '({ __typeof__((${right_source})) ${temporary} = (${right_source}); builtin__array_push((array *)&(${left_source}), &${temporary}); 0; })'
		typ:    'void'
	}
}

fn (g &Parser) render_method_call_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut changed := false
	if flags := g.render_flag_method_expression(tokens, rendered) {
		rendered = flags.source
		changed = true
	}
	for i := tokens.len - 2; i >= 2; i-- {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		if tokens[i - 2].tok == .name
			&& (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C') {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		if tokens[i].lit == 'wait' && receiver_type.starts_with(fastc_thread_type_prefix) {
			// `.wait()` joins a spawned thread (see spawn.v); it has no entry in
			// the collected function signatures.
			wait_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
			value_type := g.thread_value_types[receiver_type] or { '' }
			wait_call := '${fastc_thread_wait_name(receiver_type)}(${receiver.source})'
			if receiver_start == 0 && wait_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: wait_call
					typ:    if value_type == '' { 'void' } else { value_type }
				}
			}
			// A wait nested in a larger expression replaces its raw call form,
			// exactly like ordinary method calls.
			mut wait_needle := '${receiver.source}.wait()'
			if !rendered.contains(wait_needle) {
				raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
				raw_needle := '${raw_receiver}.wait()'
				if raw_receiver != '' && rendered.contains(raw_needle) {
					wait_needle = raw_needle
				}
			}
			if rendered.contains(wait_needle) {
				rendered = rendered.replace(wait_needle, wait_call)
				changed = true
			}
			continue
		}
		method_key := g.method_function_key(receiver_type, tokens[i].lit)
		if method_key !in g.functions {
			if g.struct_member_type(receiver_type, tokens[i].lit) != '' {
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				for separator in ['->', '.'] {
					marker := '${receiver.source}${separator}${tokens[i].lit}('
					if rendered.contains(marker) {
						rendered = rendered.replace(marker,
							'(${receiver.source}${separator}${tokens[i].lit})(')
						changed = true
						break
					}
				}
			}
			continue
		}
		signature := g.functions[method_key]
		if signature.parameter_types.len == 0 {
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if signature.is_disabled {
			disabled_call := fastc_disabled_call_expression(signature.return_type)
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: disabled_call
					typ:    signature.return_type
				}
			}
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
				continue
			}
			if rendered.contains(raw_call) {
				rendered = rendered.replace(raw_call, disabled_call)
				changed = true
			}
			continue
		}
		receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
		mut receiver_source := receiver.source
		mut separator := if receiver_tokens.len == 1 && receiver_type.ends_with('*') {
			'->'
		} else {
			'.'
		}
		mut method_marker := '${separator}${tokens[i].lit}('
		if receiver_start == 0 {
			raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { receiver_source }
			if receiver_source == raw_receiver && rendered.contains(method_marker) {
				receiver_source = rendered.all_before_last(method_marker)
			} else {
				alternate_separator := if separator == '.' { '->' } else { '.' }
				alternate_marker := '${alternate_separator}${tokens[i].lit}('
				if rendered.contains(alternate_marker) {
					separator = alternate_separator
					method_marker = alternate_marker
					receiver_source = rendered.all_before_last(method_marker)
				}
			}
		}
		mut needle := '${receiver_source}${separator}${tokens[i].lit}('
		if !rendered.contains(needle) {
			raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
			raw_needle := '${raw_receiver}${separator}${tokens[i].lit}('
			if raw_receiver != '' && rendered.contains(raw_needle) {
				needle = raw_needle
			}
		}
		expected_receiver := signature.parameter_types[0]
		receiver_argument := if expected_receiver.ends_with('*') && !receiver_type.ends_with('*') {
			'&(${receiver_source})'
		} else if !expected_receiver.ends_with('*') && receiver_type.ends_with('*') {
			'*(${receiver_source})'
		} else {
			receiver_source
		}
		has_arguments := call_end > i + 2
		method_c_name := fastc_method_c_name(signature.module_name, expected_receiver,
			tokens[i].lit)
		mut direct_arguments := []string{}
		if has_arguments {
			call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
			for argument_index, argument in call_args {
				expected_index := argument_index + 1
				expected_type := if expected_index < signature.parameter_types.len {
					signature.parameter_types[expected_index]
				} else {
					''
				}
				argument_source := g.render_call_argument_expression(argument, expected_type) or {
					continue
				}
				argument_type := g.infer_expression_type(argument) or { '' }
				if expected_type == 'voidptr' && !fastc_is_pointer_type(argument_type) {
					direct_arguments << '&(${argument_source})'
				} else {
					direct_arguments << argument_source
				}
			}
		}
		replacement := '${method_c_name}(${receiver_argument}${if has_arguments {
			','
		} else {
			''
		}}'
		mut call_needle := needle
		mut call_replacement := replacement
		if receiver_start == 0 && call_end == tokens.len - 1 {
			result_type := g.specialized_method_return_type(receiver_type, method_key, signature)
			is_pointer_result_method := method_key.starts_with('array.')
				&& tokens[i].lit in ['first', 'last', 'pop', 'pop_left']
			if !is_pointer_result_method && !has_arguments && rendered.contains(needle) {
				return FastcRenderedExpression{
					source: rendered.replace(needle, replacement)
					typ:    result_type
				}
			}
			argument_suffix := if direct_arguments.len > 0 {
				',' + direct_arguments.join(',')
			} else {
				''
			}
			mut direct_call := '${method_c_name}(${receiver_argument}${argument_suffix})'
			if is_pointer_result_method {
				element_type := g.array_element_type(receiver_type) or { continue }
				direct_call = '(*(((${element_type} *)${direct_call})))'
			}
			return FastcRenderedExpression{
				source: direct_call
				typ:    result_type
			}
		}
		if method_key.starts_with('array.') && !has_arguments
			&& tokens[i].lit in ['first', 'last', 'pop', 'pop_left'] {
			element_type := g.array_element_type(receiver_type) or { continue }
			call_needle = '${needle})'
			call_replacement = '(*(((${element_type} *)${replacement}))))'
		}
		if rendered.contains(call_needle) {
			rendered = rendered.replace(call_needle, call_replacement)
			changed = true
		}
	}
	if !changed {
		return none
	}
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ:    inferred_type
	}
}

fn (g &Parser) render_composed_string_concatenation(tokens []FastcExpressionToken) ?FastcRenderedExpression {
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
					string_operands << g.underlying_alias_type(operand_type).trim_right('*') == 'string'
					operand_start = i + 1
					plus_count++
				}
			}
			else {}
		}
	}
	if plus_count == 0 {
		return none
	}
	last_operand_type := g.infer_expression_type(tokens[operand_start..]) or { '' }
	string_operands << g.underlying_alias_type(last_operand_type).trim_right('*') == 'string'
	mut has_string_operand := false
	for is_string in string_operands {
		if is_string {
			has_string_operand = true
			break
		}
	}
	if !fastc_all_true(string_operands) && !has_string_operand {
		return none
	}
	mut parts := []string{}
	depth = 0
	operand_start = 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if item.tok == .plus && depth == 0 {
			part := g.render_comparison_operand(tokens[operand_start..i], 'string') or {
				return none
			}
			parts << part
			operand_start = i + 1
		}
	}
	last_part := g.render_comparison_operand(tokens[operand_start..], 'string') or { return none }
	parts << last_part
	mut combined := ''
	if g.selfhost {
		combined = parts[0]
		for part in parts[1..] {
			combined = 'builtin__string_plus(${combined},${part})'
		}
	} else {
		combined = 'builtin__string_plus_many(${parts.len}, (string[]){${parts.join(',')}})'
	}
	return FastcRenderedExpression{
		source: combined
		typ:    'string'
	}
}

fn (g &Parser) render_method_receiver_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	receiver_type := g.infer_expression_type(tokens) or { return none }
	if source := g.render_map_expression(tokens) {
		return source
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access
	}
	if source := g.render_member_receiver(tokens) {
		return FastcRenderedExpression{
			source: source
			typ:    receiver_type
		}
	}
	if raw := g.render_raw_expression_tokens(tokens) {
		if source := g.render_method_call_expression(tokens, raw) {
			return source
		}
	}
	if source := g.render_membership_candidate(tokens, '') {
		return FastcRenderedExpression{
			source: source
			typ:    receiver_type
		}
	}
	return none
}

fn (g &Parser) render_member_receiver(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens[0].tok != .name {
		return none
	}
	mut source := g.resolved_expression_name(tokens[0].lit, .unknown)
	mut current_type := g.infer_expression_type(tokens[..1]) or { return none }
	mut i := 1
	for i < tokens.len {
		if i + 1 >= tokens.len || tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		field := g.struct_field_metadata(current_type, tokens[i + 1].lit) or { return none }
		for storage_name in field.storage_path {
			separator := if current_type.ends_with('*') { '->' } else { '.' }
			source += separator + fastc_c_identifier(storage_name)
			current_type = g.struct_direct_member_type(current_type, storage_name)
			if current_type == '' {
				return none
			}
		}
		separator := if current_type.ends_with('*') { '->' } else { '.' }
		source += separator + fastc_c_identifier(field.name)
		current_type = field.typ
		i += 2
	}
	return source
}

fn fastc_split_top_level_c_plus(source string) []string {
	mut parts := []string{}
	mut start := 0
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	mut quote := u8(0)
	mut escaped := false
	for i, character in source {
		if quote != 0 {
			if escaped {
				escaped = false
			} else if character == `\\` {
				escaped = true
			} else if character == quote {
				quote = 0
			}
			continue
		}
		if character in [`'`, `"`] {
			quote = character
			continue
		}
		match character {
			`(` {
				parens++
			}
			`)` {
				parens--
			}
			`[` {
				brackets++
			}
			`]` {
				brackets--
			}
			`{` {
				braces++
			}
			`}` {
				braces--
			}
			`+` {
				if parens == 0 && brackets == 0 && braces == 0 {
					parts << source[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if parts.len == 0 {
		return [source]
	}
	parts << source[start..]
	return parts
}
