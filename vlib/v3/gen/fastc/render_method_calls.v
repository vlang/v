module fastc

fn (g &Parser) render_missing_call_arguments(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 3 || tokens.last().tok != .rpar {
		return none
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name && (tokens[0].lit in g.imports || tokens[0].lit == 'C') {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok !in [.name, .key_select] || tokens[open_index].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar(tokens, open_index) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	function_key := g.function_key_for_call(tokens, name_index)
	signature := if function_key in g.functions {
		g.functions[function_key]
	} else {
		g.mono_functions[function_key] or { return none }
	}
	if signature.is_disabled {
		return FastcRenderedExpression{
			source: fastc_disabled_call_expression(signature.return_type)
			typ: signature.return_type
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
	if named_start >= 0 && named_start == signature.parameter_types.len - 1 && named_start <= call_args.len && (signature.last_parameter_is_params || g.fastc_type_is_declared_struct(signature.parameter_types[named_start])) {
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
		named_initializer := g.render_named_struct_initializer(parameter_type, call_args[named_start..]) or { return none }
		rendered_arguments << named_initializer
		return FastcRenderedExpression{
			source: '${fastc_c_function_name_for_key(function_key)}(${rendered_arguments.join(',')})'
			typ: signature.return_type
		}
	}
	if signature.is_variadic && !function_key.starts_with('C.') {
		fixed_arguments := signature.parameter_types.len - 1
		if call_args.len < fixed_arguments {
			return none
		}
		variadic_type := signature.parameter_types.last()
		element_type := g.array_element_type(variadic_type) or { return none }
		if named_start == fixed_arguments {
			mut c_arguments := []string{cap: fixed_arguments + 1}
			for argument_index, argument in call_args[..fixed_arguments] {
				c_arguments << g.render_call_argument_expression(argument, signature.parameter_types[argument_index]) or { return none }
			}
			named_initializer := g.render_named_struct_initializer(element_type, call_args[named_start..]) or { return none }
			c_arguments << '((${variadic_type})builtin__new_array_from_c_array(1, 1, sizeof(${element_type}), (${element_type}[]){${named_initializer}}))'
			return FastcRenderedExpression{
				source: '${fastc_c_function_name_for_key(function_key)}(${c_arguments.join(',')})'
				typ: signature.return_type
			}
		}
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
		if fixed_arguments < 0 || fixed_arguments > rendered_arguments.len {
			return none
		}
		variadic_count := rendered_arguments.len - fixed_arguments
		variadic_values := rendered_arguments[fixed_arguments..].join(',')
		packed := if variadic_count == 0 {
			'(${variadic_type}){0}'
		} else {
			'((${variadic_type})builtin__new_array_from_c_array(${variadic_count}, ${variadic_count}, sizeof(${element_type}), (${element_type}[]){${variadic_values}}))'
		}
		rendered_arguments.trim(fixed_arguments)
		rendered_arguments << packed
		return FastcRenderedExpression{
			source: '${fastc_c_function_name_for_key(function_key)}(${rendered_arguments.join(',')})'
			typ: signature.return_type
		}
	}
	if call_args.len < signature.parameter_types.len && (!signature.last_parameter_is_params || call_args.len + 1 != signature.parameter_types.len) {
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
	if call_args.len > signature.parameter_types.len {
		return none
	}
	for parameter_type in signature.parameter_types[call_args.len..] {
		rendered_arguments << g.render_empty_struct_initializer(parameter_type)
	}
	call_name := if function_key.starts_with('C.') {
		function_key.all_after_last('.')
	} else {
		fastc_c_function_name_for_key(function_key)
	}
	return FastcRenderedExpression{
		source: '${call_name}(${rendered_arguments.join(',')})'
		typ: signature.return_type
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
		// `mod.func()` is a module-qualified call; skip method rendering. But a FIELD named
		// like an imported module (`recv.mod.method()`, e.g. `h.time.elapsed()`) is a real
		// method call — only skip when `mod` is a bare module ref, not preceded by a `.`.
		if tokens[i - 2].tok == .name && (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C') && (i < 3 || tokens[i - 3].tok != .dot) {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		if tokens[i].lit == 'contains' && fastc_trim_pointer_suffix(fastc_normalize_inferred_type(receiver_type)).starts_with('Array_') {
			call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
			if call_args.len != 1 {
				continue
			}
			element_type := g.array_element_type(receiver_type) or { continue }
			receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
			argument := g.render_call_argument_expression(call_args[0], element_type) or {
				continue
			}
			access := if receiver_type.ends_with('*') { '->' } else { '.' }
			comparison := if fastc_trim_pointer_suffix(g.underlying_alias_type(element_type)) == 'string' {
				'builtin__string_eq(__v_fastc_contains_item, ((${element_type} *)__v_fastc_contains_collection${access}data)[__v_fastc_contains_index])'
			} else {
				'(__v_fastc_contains_item == ((${element_type} *)__v_fastc_contains_collection${access}data)[__v_fastc_contains_index])'
			}
			call_source := '({ ${element_type} __v_fastc_contains_item = (${argument}); __typeof__((${receiver.source})) __v_fastc_contains_collection = (${receiver.source}); bool __v_fastc_contains_found = false; for (int __v_fastc_contains_index = 0; __v_fastc_contains_index < __v_fastc_contains_collection${access}len; __v_fastc_contains_index++) { if (${comparison}) { __v_fastc_contains_found = true; break; } } __v_fastc_contains_found; })'
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: call_source
					typ: 'bool'
				}
			}
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
				continue
			}
			if rendered.contains(raw_call) {
				rendered = rendered.replace(raw_call, call_source)
				changed = true
			}
			continue
		}
		if tokens[i].lit == 'wait' && receiver_type.starts_with(fastc_thread_type_prefix) {
			// `.wait()` joins a spawned thread (see spawn.v); it has no entry in
			// the collected function signatures.
			wait_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
			value_type := g.thread_value_types[receiver_type] or { '' }
			wait_call := '${g.fastc_unclaimed_generated_name(fastc_thread_wait_name(receiver_type))}(${receiver.source})'
			if receiver_start == 0 && wait_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: wait_call
					typ: if value_type == '' { 'void' } else { value_type }
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
		method_key, embedded_path := g.resolve_method(receiver_type, tokens[i].lit)
		if method_key !in g.functions && method_key !in g.mono_functions {
			if tokens[i].lit == 'str' && g.can_generate_default_struct_str(receiver_type) {
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				mut w := unsafe { &Parser(g) }
				helper := w.fastc_default_struct_str_name(receiver_type.trim_right('*')) or {
					continue
				}
				receiver_argument := if receiver_type.ends_with('*') {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				call_source := '${helper}(${receiver_argument})'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ: 'string'
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				if rendered.contains(raw_call) {
					rendered = rendered.replace(raw_call, call_source)
					changed = true
				}
				continue
			}
			if field := g.struct_field_metadata(receiver_type, tokens[i].lit) {
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if field.is_function {
					field_tokens := tokens[receiver_start..i + 1]
					field_source := g.render_member_receiver(field_tokens) or { continue }
					call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
					if call_args.len != field.fn_parameter_types.len {
						continue
					}
					mut arguments := []string{cap: call_args.len}
					for argument_index, argument in call_args {
						arguments << g.render_call_argument_expression(argument, field.fn_parameter_types[argument_index]) or { continue }
					}
					parameter_types := if field.fn_parameter_types.len == 0 {
						'void'
					} else {
						field.fn_parameter_types.join(', ')
					}
					return_type := if field.fn_return_type == '' {
						'void'
					} else {
						field.fn_return_type
					}
					call_source := '((${return_type} (*)(${parameter_types}))(${field_source}))(${arguments.join(', ')})'
					if receiver_start == 0 && call_end == tokens.len - 1 {
						return FastcRenderedExpression{
							source: call_source
							typ: return_type
						}
					}
					raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
						continue
					}
					if rendered.contains(raw_call) {
						rendered = rendered.replace(raw_call, call_source)
						changed = true
					}
					continue
				}
				for separator in ['->', '.'] {
					marker := '${receiver.source}${separator}${tokens[i].lit}('
					if rendered.contains(marker) {
						rendered = rendered.replace(marker, '(${receiver.source}${separator}${tokens[i].lit})(')
						changed = true
						break
					}
				}
			}
			continue
		}
		signature := if method_key in g.functions {
			g.functions[method_key]
		} else {
			g.mono_functions[method_key]
		}
		if signature.parameter_types.len == 0 {
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if signature.is_disabled {
			disabled_call := fastc_disabled_call_expression(signature.return_type)
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: disabled_call
					typ: signature.return_type
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
		mut effective_receiver_source := receiver_source
		mut effective_is_pointer := receiver.typ.ends_with('*')
		if embedded_path.len > 0 {
			// Promote through embedded fields: `d.method()`, where `method` lives on
			// an embedded type, becomes `Type_method(&(d.__embedded_N), ...)`. The
			// embedded field is stored by value, so the promoted receiver is a value.
			access := if effective_is_pointer { '->' } else { '.' }
			effective_receiver_source = '(${receiver_source})'
			for idx, part in embedded_path {
				separator_c := if idx == 0 { access } else { '.' }
				effective_receiver_source += '${separator_c}${part}'
			}
			effective_is_pointer = false
		}
		receiver_argument := if expected_receiver.ends_with('*') && !effective_is_pointer {
			'&(${effective_receiver_source})'
		} else if !expected_receiver.ends_with('*') && effective_is_pointer {
			'*(${effective_receiver_source})'
		} else {
			effective_receiver_source
		}
		has_arguments := call_end > i + 2
		method_c_name := fastc_method_c_name(signature.module_name, expected_receiver, tokens[i].lit)
		mut direct_arguments := []string{}
		if has_arguments {
			call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
			mut named_start := -1
			for argument_index, argument in call_args {
				if argument.len >= 3 && argument[0].tok == .name && argument[1].tok == .colon {
					named_start = argument_index
					break
				}
			}
			if named_start >= 0 && named_start + 1 == signature.parameter_types.len - 1 && (signature.last_parameter_is_params || g.fastc_type_is_declared_struct(signature.parameter_types.last())) {
				for argument_index, argument in call_args[..named_start] {
					expected_type := signature.parameter_types[argument_index + 1]
					direct_arguments << g.render_call_argument_expression(argument, expected_type) or {
						continue
					}
				}
				named_parameter_type := if signature.is_variadic {
					g.array_element_type(signature.parameter_types.last()) or { continue }
				} else {
					signature.parameter_types.last()
				}
				named_initializer := g.render_named_struct_initializer(named_parameter_type, call_args[named_start..]) or { continue }
				direct_arguments << named_initializer
			} else {
				for argument_index, argument in call_args {
					expected_index := argument_index + 1
					expected_type := if signature.is_variadic && expected_index >= signature.parameter_types.len - 1 {
						g.array_element_type(signature.parameter_types.last()) or { continue }
					} else if expected_index < signature.parameter_types.len {
						signature.parameter_types[expected_index]
					} else {
						''
					}
					argument_source := g.render_call_argument_expression(argument, expected_type) or {
						continue
					}
					direct_arguments << argument_source
				}
			}
		}
		if signature.is_variadic && !method_key.starts_with('C.') {
			fixed_arguments := signature.parameter_types.len - 2
			if direct_arguments.len < fixed_arguments {
				continue
			}
			variadic_type := signature.parameter_types.last()
			element_type := g.array_element_type(variadic_type) or { continue }
			variadic_count := direct_arguments.len - fixed_arguments
			variadic_values := direct_arguments[fixed_arguments..].join(',')
			packed := if variadic_count == 0 {
				'(${variadic_type}){0}'
			} else {
				'((${variadic_type})builtin__new_array_from_c_array(${variadic_count}, ${variadic_count}, sizeof(${element_type}), (${element_type}[]){${variadic_values}}))'
			}
			direct_arguments.trim(fixed_arguments)
			direct_arguments << packed
		}
		if signature.last_parameter_is_params && direct_arguments.len + 1 == signature.parameter_types.len - 1 {
			direct_arguments << g.render_empty_struct_initializer(signature.parameter_types.last())
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
			is_pointer_result_method := method_key.starts_with('array.') && tokens[i].lit in [
				'first',
				'last',
				'pop',
				'pop_left',
			]
			if !is_pointer_result_method && !has_arguments && direct_arguments.len == 0 && rendered.contains(needle) {
				return FastcRenderedExpression{
					source: rendered.replace(needle, replacement)
					typ: result_type
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
				typ: result_type
			}
		}
		if direct_arguments.len > 0 {
			argument_suffix := ',' + direct_arguments.join(',')
			direct_call := '${method_c_name}(${receiver_argument}${argument_suffix})'
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
				continue
			}
			if rendered.contains(raw_call) {
				rendered = rendered.replace(raw_call, direct_call)
				changed = true
				continue
			}
		}
		if method_key.starts_with('array.') && !has_arguments && tokens[i].lit in [
			'first',
			'last',
			'pop',
			'pop_left',
		] {
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
		typ: inferred_type
	}
}
