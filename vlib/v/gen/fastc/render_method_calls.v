module fastc

import v.flat

fn (g &Parser) render_missing_call_arguments(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if callback_call := g.render_function_alias_call_expression(tokens) {
		return callback_call
	}
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
	} else if name_index == 0 && tokens[0].lit in g.locals
		&& g.locals[tokens[0].lit].fn_signature_key in g.mono_functions {
		g.mono_functions[g.locals[tokens[0].lit].fn_signature_key]
	} else {
		g.mono_functions[function_key] or { return none }
	}
	call_name := if function_key.starts_with('C.') {
		function_key.all_after_last('.')
	} else if name_index == 0 && tokens[0].lit in g.locals {
		local := g.locals[tokens[0].lit]
		local_name := g.local_c_name(tokens[0].lit)
		if local.typ_text() == 'voidptr' && local.fn_return_type != 0 {
			parameters := fastc_c_prototype_parameters(signature.parameter_types.ids)
			'((${fastc_output_c_type(signature.return_type_text())} (*)(${parameters}))(${local_name}))'
		} else {
			local_name
		}
	} else {
		g.c_function_name_for_key(function_key)
	}
	if signature.is_disabled {
		return FastcRenderedExpression{
			source: fastc_disabled_call_expression(signature.return_type_text())
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
	if named_start >= 0 && named_start == signature.parameter_types.ids.len - 1 && named_start <= call_args.len && (signature.last_parameter_is_params || g.fastc_type_is_declared_struct(signature.parameter_types.ids[named_start])) {
		mut rendered_arguments := []string{}
		for argument_index, argument in call_args[..named_start] {
			expected_type := if argument_index < signature.parameter_types.ids.len {
				signature.parameter_types.ids[argument_index]
			} else {
				u32(0)
			}
			rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
				return none
			}
			rendered_arguments << rendered_argument
		}
		parameter_type := signature.parameter_types.ids[named_start]
		named_initializer := g.render_named_struct_initializer(parameter_type, call_args[named_start..]) or { return none }
		rendered_arguments << named_initializer
		return FastcRenderedExpression{
			source: '${call_name}(${rendered_arguments.join(',')})'
			typ:    signature.return_type
		}
	}
	if signature.is_variadic && !function_key.starts_with('C.') {
		fixed_arguments := signature.parameter_types.ids.len - 1
		if call_args.len < fixed_arguments {
			return none
		}
		variadic_type := signature.parameter_types.ids.last()
		element_type := g.array_element_type(variadic_type) or { return none }
		if named_start == fixed_arguments {
			mut c_arguments := []string{cap: fixed_arguments + 1}
			for argument_index, argument in call_args[..fixed_arguments] {
				c_arguments << g.render_call_argument_expression(argument, signature.parameter_types.ids[argument_index]) or { return none }
			}
			named_initializer := g.render_named_struct_initializer(element_type, call_args[named_start..]) or { return none }
			c_arguments << g.dynamic_array_literal_source(flat.source_type_name(variadic_type), flat.source_type_name(element_type),
				[named_initializer])
			return FastcRenderedExpression{
				source: '${call_name}(${c_arguments.join(',')})'
				typ:    signature.return_type
			}
		}
		// `f(x, ...arr)` spreads an existing array into the variadic parameter: the array itself
		// is the variadic argument, so pass it directly rather than packing its `...` token into a
		// C array literal.
		if call_args.len == fixed_arguments + 1 && call_args[fixed_arguments].len >= 2 && call_args[fixed_arguments][0].tok == .ellipsis {
			mut c_arguments := []string{cap: fixed_arguments + 1}
			for argument_index in 0 .. fixed_arguments {
				c_arguments << g.render_call_argument_expression(call_args[argument_index], signature.parameter_types.ids[argument_index]) or { return none }
			}
			spread := g.render_call_argument_expression(call_args[fixed_arguments][1..], variadic_type) or { return none }
			c_arguments << spread
			return FastcRenderedExpression{
				source: '${call_name}(${c_arguments.join(',')})'
				typ:    signature.return_type
			}
		}
		mut rendered_arguments := []string{}
		for argument_index, argument in call_args {
			expected_type := if argument_index < fixed_arguments {
				signature.parameter_types.ids[argument_index]
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
		variadic_values := rendered_arguments[fixed_arguments..].clone()
		packed := if variadic_count == 0 {
			'(${flat.source_type_name(variadic_type)}){0}'
		} else {
			g.dynamic_array_literal_source(flat.source_type_name(variadic_type), flat.source_type_name(element_type), variadic_values)
		}
		rendered_arguments.trim(fixed_arguments)
		rendered_arguments << packed
		return FastcRenderedExpression{
			source: '${call_name}(${rendered_arguments.join(',')})'
			typ:    signature.return_type
		}
	}
	if call_args.len < signature.parameter_types.ids.len && (!signature.last_parameter_is_params || call_args.len + 1 != signature.parameter_types.ids.len) {
		return none
	}
	mut rendered_arguments := []string{cap: signature.parameter_types.ids.len}
	for argument_index, argument in call_args {
		expected_type := if argument_index < signature.parameter_types.ids.len {
			signature.parameter_types.ids[argument_index]
		} else {
			u32(0)
		}
		rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
			return none
		}
		rendered_arguments << rendered_argument
	}
	if call_args.len > signature.parameter_types.ids.len
		&& !(signature.is_variadic && function_key.starts_with('C.')) {
		return none
	}
	if call_args.len < signature.parameter_types.ids.len {
		for parameter_type in signature.parameter_types.ids[call_args.len..] {
			rendered_arguments << g.render_empty_struct_initializer(parameter_type)
		}
	}
	return FastcRenderedExpression{
		source: '${call_name}(${rendered_arguments.join(',')})'
		typ:    signature.return_type
	}
}

// function_alias_call_open recognizes a cast to a function alias followed by an
// invocation, including a parenthesized cast and an imported alias.
fn (g &Parser) function_alias_call_open(tokens []FastcExpressionToken) int {
	if tokens.len < 5 || tokens.last().tok != .rpar {
		return -1
	}
	for open in 1 .. tokens.len - 1 {
		if tokens[open].tok != .lpar || tokens[open - 1].tok != .rpar
			|| fastc_matching_rpar(tokens, open) or { -1 } != tokens.len - 1 {
			continue
		}
		_ := g.function_alias_cast_type(tokens[..open]) or { continue }
		return open
	}
	return -1
}

fn (g &Parser) function_alias_cast_type(tokens []FastcExpressionToken) ?u32 {
	callee := fastc_strip_paren_tokens(tokens)
	cast_open := if callee.len >= 4 && callee[0].tok == .name
		&& callee[1].tok == .dot && callee[2].tok == .name {
		3
	} else {
		1
	}
	if callee.len <= cast_open + 2 || callee[cast_open].tok != .lpar {
		return none
	}
	alias_type := g.type_from_expression_tokens(callee[..cast_open]) or { return none }
	if !g.fn_alias_return_types.contains(flat.source_type_name(alias_type)) || flat.source_type_name(alias_type) !in g.functions {
		return none
	}
	if fastc_matching_rpar(callee, cast_open) or { -1 } != callee.len - 1
		|| !fastc_call_has_one_argument(callee, cast_open) {
		return none
	}
	return alias_type
}

fn (g &Parser) function_alias_call_signature(tokens []FastcExpressionToken) ?FastcFunctionSignature {
	open := g.function_alias_call_open(tokens)
	if open < 0 {
		return none
	}
	alias_type := g.function_alias_cast_type(tokens[..open]) or { return none }
	return g.functions[flat.source_type_name(alias_type)] or { return none }
}

fn (g &Parser) render_function_alias_call_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	open := g.function_alias_call_open(tokens)
	if open < 0 {
		return none
	}
	callee := g.render_cast_expression(fastc_strip_paren_tokens(tokens[..open])) or {
		return none
	}
	signature := g.functions[callee.typ_text()] or { return none }
	call_args := fastc_call_arguments(tokens, open, tokens.len - 1) or { return none }
	if call_args.len != signature.parameter_types.ids.len {
		return none
	}
	mut arguments := []string{cap: call_args.len}
	for i, argument in call_args {
		arguments << g.render_call_argument_expression(argument, signature.parameter_types.ids[i]) or {
			return none
		}
	}
	return FastcRenderedExpression{
		source: '(${callee.source})(${arguments.join(', ')})'
		typ:    signature.return_type
	}
}

fn (g &Parser) render_method_call_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut changed := false
	mut flags_processed := false
	for i := tokens.len - 2; i >= 2; i-- {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		// `mod.func()` is a module-qualified call; skip method rendering. But a FIELD named
		// like an imported module (`recv.mod.method()`, e.g. `h.time.elapsed()`) is a real
		// method call — only skip when `mod` is a bare module ref, not preceded by a `.`. A LOCAL
		// of the module's name (`for token in … { token.starts_with(…) }`) shadows the module.
		if tokens[i - 2].tok == .name && (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C') && tokens[i - 2].lit !in g.locals && (i < 3 || tokens[i - 3].tok != .dot) {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		if !flags_processed && tokens[i].lit in ['has', 'set', 'clear'] {
			flags_processed = true
			if flags := g.render_flag_method_expression(tokens, rendered) {
				rendered = flags.source
				changed = true
			}
		}
		receiver_tokens := tokens[receiver_start..i - 1]
		mut receiver_type := (g.infer_expression_type(receiver_tokens) or { u32(0) })
		if receiver_type == u32(0) {
			// An imported struct reached through an active sum-type smartcast can
			// lose its member type in generic expression inference. The dedicated
			// member walk still has the narrowed receiver and field metadata.
			receiver_type = (g.infer_member_access_type(receiver_tokens, 0, receiver_tokens.len) or {
				continue
			})
		}
		if flat.source_type_name(receiver_type) == 'chan' && tokens[i].lit in ['close', 'try_push',
			'try_pop'] {
			call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
			receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
			mut arguments := []string{}
			for argument in call_args {
				arguments << g.render_call_argument_expression(argument, flat.source_type_id(if tokens[i].lit == 'close' {
					'IError'
				} else {
					'voidptr'
				})) or { continue }
			}
			packed := if tokens[i].lit == 'close' && arguments.len == 0 {
				'(Array_IError){0}'
			} else if tokens[i].lit == 'close' {
				g.dynamic_array_literal_source('Array_IError', 'IError', arguments)
			} else {
				arguments.join(',')
			}
			call_source := '${g.channel_runtime_name(tokens[i].lit)}((chan)(${receiver.source}), ${packed})'
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: call_source
					typ:    (if tokens[i].lit == 'close' {
						flat.source_type_void
					} else {
						flat.source_type_id('ChanState')
					})
				}
			}
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or { continue }
			rendered = fastc_replace(rendered, raw_call, call_source)
			changed = true
			continue
		}
		// A method defined on the whole sum type (`fn (e Expr) is_blank_ident()`) stays
		// callable after narrowing. A grouped arm must retain whole-sum dispatch even when
		// its first variant defines the same method. Bare locals also retain whole-sum
		// methods absent from a single narrowed variant.
		mut unnarrowed_receiver := ''
		if receiver_tokens.len == 1 && receiver_tokens[0].tok == .name && receiver_tokens[0].lit in g.member_smartcasts {
			narrowed_key, _ := g.resolve_method(receiver_type, tokens[i].lit)
			grouped := g.member_smartcasts[receiver_tokens[0].lit].variants.ids.len > 1
			if grouped || (narrowed_key !in g.functions && narrowed_key !in g.mono_functions) {
				if local := g.locals[receiver_tokens[0].lit] {
					original_key, _ := g.resolve_method(local.typ, tokens[i].lit)
					if original_key in g.functions || original_key in g.mono_functions {
						receiver_type = local.typ
						unnarrowed_receiver = g.resolved_expression_name(receiver_tokens[0].lit, .unknown)
					}
				}
			}
		}
		// A bare-local `is` smart-cast shadows the local with the narrowed variant value, whose
		// declared C name still holds the boxed original. A method named like a variant field but
		// defined on the whole sum type (`ast.Ident` has a `pos` field; `ast.Expr` has a `pos()`
		// method) is not on the variant, so dispatch it on the boxed original C variable.
		if unnarrowed_receiver == '' && receiver_tokens.len == 1 && receiver_tokens[0].tok == .name {
			if local := g.locals[receiver_tokens[0].lit] {
				if local.smartcast_origin_type != u32(0) && local.smartcast_origin_type_text() != flat.source_type_name(receiver_type) {
					narrowed_key, _ := g.resolve_method(receiver_type, tokens[i].lit)
					if narrowed_key !in g.functions && narrowed_key !in g.mono_functions {
						origin_key, _ := g.resolve_method(local.smartcast_origin_type, tokens[i].lit)
						if origin_key in g.functions || origin_key in g.mono_functions {
							receiver_type = local.smartcast_origin_type
							unnarrowed_receiver = fastc_c_identifier(receiver_tokens[0].lit)
						}
					}
				}
			}
		}
		is_array_receiver := tokens[i].lit in ['contains', 'index', 'last_index']
			&& fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(fastc_normalize_inferred_type(receiver_type)))).starts_with('Array_')
		mut array_has_user_contains := false
		if tokens[i].lit == 'contains' && is_array_receiver {
			contains_method_key, _ := g.resolve_method(receiver_type, 'contains')
			// The generic `array.contains` is a compiler-magic builtin with no emitted body, so it must
			// still be lowered inline; only a genuine USER-defined `contains` (a non-builtin module)
			// suppresses the inline. Without this the call links against an undefined builtin__array_contains.
			array_has_user_contains = contains_method_key in g.functions
				|| contains_method_key in g.mono_functions
			if sig := g.functions[contains_method_key] {
				if sig.module_name == 'builtin' {
					array_has_user_contains = false
				}
			}
		}
		if tokens[i].lit == 'contains' && is_array_receiver && !array_has_user_contains {
			call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
			if call_args.len != 1 {
				continue
			}
			element_type := g.array_element_type(g.underlying_alias_type(receiver_type)) or { continue }
			receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
			argument := g.render_call_argument_expression(call_args[0], element_type) or {
				continue
			}
			element_storage_type := g.collection_storage_type(element_type)
			access := if flat.source_type_name(receiver_type).ends_with('*') { '->' } else { '.' }
			comparison := if fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(element_type))) == 'string' {
				'builtin__string_eq(__vf_contains_item, ((${flat.source_type_name(element_type)} *)__vf_contains_collection${access}data)[__vf_contains_index])'
			} else {
				'(__vf_contains_item == ((${element_storage_type} *)__vf_contains_collection${access}data)[__vf_contains_index])'
			}
			receiver_decl_type := g.declaration_c_type(receiver_type, receiver.source)
			call_source := '({ ${element_storage_type} __vf_contains_item = (${argument}); ${receiver_decl_type} __vf_contains_collection = (${receiver.source}); bool __vf_contains_found = false; for (${fastc_output_c_type('int')} __vf_contains_index = 0; __vf_contains_index < __vf_contains_collection${access}len; __vf_contains_index++) { if (${comparison}) { __vf_contains_found = true; break; } } __vf_contains_found; })'
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: call_source
					typ:    flat.source_type_bool
				}
			}
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
				continue
			}
			replaced := fastc_replace(rendered, raw_call, call_source)
			if replaced.str != rendered.str {
				rendered = replaced
				changed = true
			}
			continue
		}
		if tokens[i].lit in ['index', 'last_index'] && is_array_receiver {
			element_type := g.array_element_type(g.underlying_alias_type(receiver_type)) or { u32(0) }
			element_layout := g.underlying_alias_type(element_type)
			if element_layout == flat.source_type_string || fastc_is_pointer_type(element_layout) {
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
				if call_args.len != 1 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				argument := g.render_call_argument_expression(call_args[0], element_type) or {
					continue
				}
				access := if flat.source_type_name(receiver_type).ends_with('*') {
					'->'
				} else {
					'.'
				}
				loop_init := if tokens[i].lit == 'last_index' {
					'__vf_index_collection${access}len - 1'
				} else {
					'0'
				}
				loop_condition := if tokens[i].lit == 'last_index' {
					'__vf_index_cursor >= 0'
				} else {
					'__vf_index_cursor < __vf_index_collection${access}len'
				}
				loop_step := if tokens[i].lit == 'last_index' {
					'__vf_index_cursor--'
				} else {
					'__vf_index_cursor++'
				}
				receiver_decl_type := g.declaration_c_type(receiver_type, receiver.source)
				element_storage_type := g.collection_storage_type(element_type)
				comparison := if element_layout == flat.source_type_string {
					'builtin__string_eq(__vf_index_item, ((${element_storage_type} *)__vf_index_collection${access}data)[__vf_index_cursor])'
				} else {
					'(__vf_index_item == ((${element_storage_type} *)__vf_index_collection${access}data)[__vf_index_cursor])'
				}
				call_source := '({ ${element_storage_type} __vf_index_item = (${argument}); ${receiver_decl_type} __vf_index_collection = (${receiver.source}); int __vf_index_result = -1; for (int __vf_index_cursor = ${loop_init}; ${loop_condition}; ${loop_step}) { if (${comparison}) { __vf_index_result = __vf_index_cursor; break; } } __vf_index_result; })'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_int
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
		}
		if tokens[i].lit == 'wait' && flat.source_type_name(receiver_type).trim_right('*').starts_with('Array_') {
			element := (g.array_element_type(receiver_type) or { u32(0) })
			if flat.source_type_name(element).starts_with(fastc_thread_type_prefix) {
				// `[]thread T`.wait() joins every handle and gathers their `[]T` results.
				wait_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				value_type := g.fastc_thread_value_type(element) or { continue }
				value_storage_type := g.collection_storage_type(value_type)
				result_type := if value_type == 0 {
					'void'
				} else {
					fastc_array_c_type(flat.source_type_name(value_type))
				}
				mut w := unsafe { &Parser(g) }
				fastc_register_composite_type(result_type, mut w.composite_types)
				wait_helper := g.fastc_thread_wait_helper(flat.source_type_name(element), value_type)
				recv_src := if flat.source_type_name(receiver_type).ends_with('*') {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				receiver_decl_type := g.declaration_c_type(flat.source_type_id(flat.source_type_name(receiver_type).trim_right('*')), recv_src)
				wait_all := if value_type == u32(0) {
					'({ ${receiver_decl_type} __vf_threads = (${recv_src}); for (int __vf_ti = 0; __vf_ti < __vf_threads.len; __vf_ti++) { ${wait_helper}(((${flat.source_type_name(element)} *)__vf_threads.data)[__vf_ti]); } })'
				} else {
					'({ ${receiver_decl_type} __vf_threads = (${recv_src}); ${result_type} __vf_results = (${result_type})builtin____new_array(0, __vf_threads.len, sizeof(${value_storage_type})); for (int __vf_ti = 0; __vf_ti < __vf_threads.len; __vf_ti++) { ${value_storage_type} __vf_tv = ${wait_helper}(((${flat.source_type_name(element)} *)__vf_threads.data)[__vf_ti]); builtin__array_push((array *)&__vf_results, &__vf_tv); } __vf_results; })'
				}
				if receiver_start == 0 && wait_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: wait_all
						typ:    flat.source_type_id(result_type)
					}
				}
				mut wait_needle := '${receiver.source}.wait()'
				if !fastc_contains(rendered, wait_needle) {
					raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
					raw_needle := '${raw_receiver}.wait()'
					if raw_receiver != '' && fastc_contains(rendered, raw_needle) {
						wait_needle = raw_needle
					}
				}
				replaced := fastc_replace(rendered, wait_needle, wait_all)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
		}
		if tokens[i].lit == 'wait' && flat.source_type_name(receiver_type).starts_with(fastc_thread_type_prefix) {
			// `.wait()` joins a spawned thread (see spawn.v); it has no entry in
			// the collected function signatures.
			wait_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
			value_type := (g.fastc_thread_value_type(receiver_type) or { u32(0) })
			wait_call := '${g.fastc_thread_wait_helper(flat.source_type_name(receiver_type).trim_right('*'), value_type)}(${receiver.source})'
			if receiver_start == 0 && wait_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: wait_call
					typ:    (if value_type == u32(0) {
						flat.source_type_void
					} else {
						value_type
					})
				}
			}
			// A wait nested in a larger expression replaces its raw call form,
			// exactly like ordinary method calls.
			mut wait_needle := '${receiver.source}.wait()'
			if !fastc_contains(rendered, wait_needle) {
				raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
				raw_needle := '${raw_receiver}.wait()'
				if raw_receiver != '' && fastc_contains(rendered, raw_needle) {
					wait_needle = raw_needle
				}
			}
			replaced := fastc_replace(rendered, wait_needle, wait_call)
			if replaced.str != rendered.str {
				rendered = replaced
				changed = true
			}
			continue
		}
		method_key, embedded_path := g.resolve_method(receiver_type, tokens[i].lit)
		if method_key !in g.functions && method_key !in g.mono_functions {
			if tokens[i].lit == 'str' && g.can_generate_default_struct_str(flat.source_type_name(receiver_type)) {
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				mut w := unsafe { &Parser(g) }
				helper := w.fastc_default_struct_str_name(flat.source_type_name(receiver_type).trim_right('*')) or {
					continue
				}
				// A member smart-cast reads the narrowed struct through a variant pointer, so its
				// rendered source is a `T*` even though the inferred receiver type is the value
				// `T`; the by-value default `str(T)` helper then needs an explicit deref.
				receiver_is_pointer := flat.source_type_name(receiver_type).ends_with('*') || (g.selfhost && g.expression_uses_member_smartcast(receiver_tokens))
				receiver_argument := if receiver_is_pointer {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				call_source := '${helper}(${receiver_argument})'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_string
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
			if tokens[i].lit == 'str' && g.declared_kinds[flat.source_type_name(g.semantic_type_key(receiver_type))] == .enum_ {
				// `enum_val.str()`: reuse the generated `v_fastc_enum_str_<T>` helper.
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				enum_c := flat.source_type_name(fastc_normalize_inferred_type(receiver_type)).trim_right('*')
				receiver_argument := if flat.source_type_name(receiver_type).ends_with('*') {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				call_source := 'v_fastc_enum_str_${enum_c}(${receiver_argument})'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_string
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
			if tokens[i].lit == 'str' && (flat.source_type_name(receiver_type).trim_right('*').starts_with('Array_')
				|| flat.source_type_name(receiver_type).trim_right('*') in g.sum_types) {
				// `arr.str()`: reuse the same generated array formatter as `${arr}`.
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				mut w := unsafe { &Parser(g) }
				helper := if flat.source_type_name(receiver_type).trim_right('*') in g.sum_types {
					w.fastc_sumtype_str_helper(flat.source_type_name(receiver_type)) or { continue }
				} else {
					w.fastc_array_str_name(flat.source_type_name(receiver_type)) or { continue }
				}
				receiver_argument := if flat.source_type_name(receiver_type).ends_with('*') {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				call_source := '${helper}(${receiver_argument})'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_string
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
			if tokens[i].lit == 'free' {
				// A struct's auto-generated `free()` has no FastC method; under `-gc none`
				// it is a no-op. (Real `free()` methods — arrays, strings, user types —
				// resolve above and never reach here.)
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				call_source := '((void)0)'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_void
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
			if tokens[i].lit == 'str' && g.selfhost
				&& fastc_trim_pointer_suffix(flat.source_type_name(fastc_normalize_inferred_type(receiver_type))).starts_with('Map_') {
				// `m.str()`: the map's generated `{'k': v}` formatter.
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				mut w := unsafe { &Parser(g) }
				helper := w.fastc_map_str_name(flat.source_type_name(receiver_type)) or { continue }
				map_value := if flat.source_type_name(receiver_type).ends_with('*') {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				call_source := '${helper}(${map_value})'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_string
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
				}
				continue
			}
			if tokens[i].lit == 'type_name' && g.is_boxed_type(fastc_normalize_inferred_type(receiver_type)) {
				// `x.type_name()` on a sum type / interface: switch on the runtime tag.
				call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
				if call_end != i + 2 {
					continue
				}
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				mut w := unsafe { &Parser(g) }
				helper := w.fastc_sumtype_type_name_helper(flat.source_type_name(receiver_type)) or { continue }
				receiver_argument := if flat.source_type_name(receiver_type).ends_with('*') {
					'*(${receiver.source})'
				} else {
					receiver.source
				}
				call_source := '${helper}(${receiver_argument})'
				if receiver_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    flat.source_type_string
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
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
					if call_args.len != field.fn_parameter_types.ids.len {
						continue
					}
					mut arguments := []string{cap: call_args.len}
					for argument_index, argument in call_args {
						arguments << g.render_call_argument_expression(argument, field.fn_parameter_types.ids[argument_index]) or { continue }
					}
					parameter_types := fastc_c_prototype_parameters(field.fn_parameter_types.ids.clone())
					return_type := if field.fn_return_type == u32(0) {
						flat.source_type_void
					} else {
						field.fn_return_type
					}
					call_source := '((${fastc_output_c_type(flat.source_type_name(return_type))} (*)(${parameter_types}))(${field_source}))(${arguments.join(', ')})'
					if receiver_start == 0 && call_end == tokens.len - 1 {
						return FastcRenderedExpression{
							source: call_source
							typ:    return_type
						}
					}
					raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
						continue
					}
					mut replaced := fastc_replace_c_name(rendered, raw_call, call_source, true)
					if replaced.str == rendered.str {
						// Its arguments may already be rewritten (`m.id(i)` to `M_id(m,i)`), so
						// the raw spelling no longer occurs; replace the call by its head instead.
						replaced = fastc_replace_c_call_at_head(rendered, '${field_source}(', call_source)
					}
					if replaced.str != rendered.str {
						rendered = replaced
						changed = true
					}
					continue
				}
				for separator in ['->', '.'] {
					marker := '${receiver.source}${separator}${tokens[i].lit}('
					replaced := fastc_replace(rendered, marker, '(${receiver.source}${separator}${tokens[i].lit})(')
					if replaced.str != rendered.str {
						rendered = replaced
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
		if signature.parameter_types.ids.len == 0 {
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if signature.is_disabled {
			disabled_call := fastc_disabled_call_expression(signature.return_type_text())
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: disabled_call
					typ:    signature.return_type
				}
			}
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
				continue
			}
			replaced := fastc_replace(rendered, raw_call, disabled_call)
			if replaced.str != rendered.str {
				rendered = replaced
				changed = true
			}
			continue
		}
		receiver := if unnarrowed_receiver != '' {
			FastcRenderedExpression{
				source: unnarrowed_receiver
				typ:    receiver_type
			}
		} else {
			g.render_method_receiver_expression(receiver_tokens) or { continue }
		}
		mut receiver_source := receiver.source
		mut separator := if receiver_tokens.len == 1 && flat.source_type_name(receiver_type).ends_with('*') {
			'->'
		} else {
			'.'
		}
		mut method_marker := '${separator}${tokens[i].lit}('
		if receiver_start == 0 {
			raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { receiver_source }
			// Only align `receiver_source` to the text already in `rendered` when the receiver
			// renders the same way there (no member smart-cast). A smart-cast receiver renders
			// differently (a variant pointer) and must be kept for the call argument; the raw
			// receiver text is matched separately by the raw-needle fallback below.
			if receiver_source == raw_receiver && fastc_contains(rendered, method_marker) {
				receiver_source = rendered.all_before_last(method_marker)
			} else if receiver_source == raw_receiver {
				alternate_separator := if separator == '.' { '->' } else { '.' }
				alternate_marker := '${alternate_separator}${tokens[i].lit}('
				if fastc_contains(rendered, alternate_marker) {
					separator = alternate_separator
					method_marker = alternate_marker
					receiver_source = rendered.all_before_last(method_marker)
				}
			}
		}
		mut needle := '${receiver_source}${separator}${tokens[i].lit}('
		if !fastc_contains(rendered, needle) {
			raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
			if raw_receiver != '' {
				// The smart-cast receiver may render with a pointer `->` while the raw receiver
				// text uses a value `.` (a narrowed local read as a variant pointer); try both.
				for candidate_separator in ['.', '->'] {
					raw_needle := '${raw_receiver}${candidate_separator}${tokens[i].lit}('
					if fastc_contains(rendered, raw_needle) {
						needle = raw_needle
						break
					}
				}
			}
		}
		if !fastc_contains(rendered, needle) && receiver_start == 0
			&& fastc_contains(rendered, method_marker) {
			// A receiver that renders differently in the boxed method form than in the raw
			// expression (a sum-type conversion `Node(x).m()`, whose grouping parens differ):
			// splice at the raw receiver text preceding the call, while the boxed receiver is
			// still used for the actual call argument built below.
			needle = rendered.all_before_last(method_marker) + method_marker
		}
		expected_receiver := signature.parameter_types.at(0)
		mut effective_receiver_source := receiver_source
		mut effective_is_pointer := receiver.typ_text().ends_with('*')
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
			g.value_address_expression(flat.source_type_id(expected_receiver.trim_right('*')), effective_receiver_source,
				receiver_tokens)
		} else if !expected_receiver.ends_with('*') && effective_is_pointer {
			'*(${effective_receiver_source})'
		} else {
			effective_receiver_source
		}
		has_arguments := call_end > i + 2
		method_c_name := g.c_function_name_or(method_key, fastc_method_c_name(signature.module_name, expected_receiver, tokens[i].lit))
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
			if named_start >= 0 && named_start + 1 == signature.parameter_types.ids.len - 1 && (signature.last_parameter_is_params || g.fastc_type_is_declared_struct(signature.parameter_types.ids.last())) {
				for argument_index, argument in call_args[..named_start] {
					expected_type := signature.parameter_types.ids[argument_index + 1]
					direct_arguments << g.render_call_argument_expression(argument, expected_type) or {
						continue
					}
				}
				named_parameter_type := if signature.is_variadic {
					g.array_element_type(signature.parameter_types.ids.last()) or { continue }
				} else {
					signature.parameter_types.ids.last()
				}
				named_initializer := g.render_named_struct_initializer(named_parameter_type, call_args[named_start..]) or { continue }
				direct_arguments << named_initializer
			} else {
				for argument_index, argument in call_args {
					if argument_index == 0 && tokens[i].lit == 'delete'
						&& g.map_type_of(receiver_type) != none {
						key_type_source_id, _ := g.map_key_value_types(flat.source_type_id(flat.source_type_name(receiver_type).trim_right('*'))) or {
							continue
						}
						key_type := key_type_source_id

						key_source := g.render_call_argument_expression(argument, key_type) or {
							continue
						}
						// The map ABI takes the address of the key's bytes, even when
						// the key is itself a pointer (as in the closure live map).
						direct_arguments << '((${g.collection_storage_type(key_type)}[]){${key_source}})'
						continue
					}
					expected_index := argument_index + 1
					expected_type := if signature.is_variadic && expected_index >= signature.parameter_types.ids.len - 1 {
						g.array_element_type(signature.parameter_types.ids.last()) or { continue }
					} else if expected_index < signature.parameter_types.ids.len {
						signature.parameter_types.ids[expected_index]
					} else {
						u32(0)
					}
					argument_source := g.render_call_argument_expression(argument, expected_type) or {
						continue
					}
					direct_arguments << argument_source
				}
			}
		}
		if signature.is_variadic && !method_key.starts_with('C.') {
			fixed_arguments := signature.parameter_types.ids.len - 2
			if direct_arguments.len < fixed_arguments {
				continue
			}
			variadic_type := signature.parameter_types.ids.last()
			element_type := g.array_element_type(variadic_type) or { continue }
			variadic_count := direct_arguments.len - fixed_arguments
			variadic_values := direct_arguments[fixed_arguments..].clone()
			packed := if variadic_count == 0 {
				'(${flat.source_type_name(variadic_type)}){0}'
			} else {
				g.dynamic_array_literal_source(flat.source_type_name(variadic_type), flat.source_type_name(element_type), variadic_values)
			}
			direct_arguments.trim(fixed_arguments)
			direct_arguments << packed
		}
		if signature.last_parameter_is_params && direct_arguments.len + 1 == signature.parameter_types.ids.len - 1 {
			direct_arguments << g.render_empty_struct_initializer(signature.parameter_types.ids.last())
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
			if !is_pointer_result_method && !has_arguments && direct_arguments.len == 0 && fastc_contains(rendered, needle) {
				return FastcRenderedExpression{
					source: fastc_replace(rendered, needle, replacement)
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
				direct_call = '(*(((${g.collection_storage_type(element_type)} *)${direct_call})))'
			}
			return FastcRenderedExpression{
				source: direct_call
				typ:    result_type
			}
		}
		if direct_arguments.len > 0 {
			argument_suffix := ',' + direct_arguments.join(',')
			direct_call := '${method_c_name}(${receiver_argument}${argument_suffix})'
			raw_call := g.render_raw_expression_tokens(tokens[receiver_start..call_end + 1]) or {
				continue
			}
			replaced := fastc_replace(rendered, raw_call, direct_call)
			if replaced.str != rendered.str {
				rendered = replaced
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
			call_replacement = '(*(((${g.collection_storage_type(element_type)} *)${replacement}))))'
		}
		if fastc_contains(rendered, call_needle) {
			// Boundary-aware replace: a receiver needle like `return_type.clear(` must NOT match
			// the suffix of a longer chain `g.fn_decl.return_type_text().clear(` (whose receiver is the
			// whole `g.fn_decl.return_type_text()`), or the method would splice onto the wrong receiver.
			replaced := fastc_replace_call_needle(rendered, call_needle, call_replacement)
			if replaced != rendered {
				rendered = replaced
				changed = true
			}
		}
	}
	if !changed {
		return none
	}
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation
	}
	return FastcRenderedExpression{
		source: rendered
		typ:    (g.infer_expression_type(tokens) or { u32(0) })
	}
}

// fastc_replace_c_call_at_head replaces the first call in `source` that starts with `head`
// (`m->store.get(`, not preceded by a name character) through its closing `)`.
// String and character literals are skipped while locating the call.
fn fastc_replace_c_call_at_head(source string, head string, replacement string) string {
	mut from := 0
	for from < source.len {
		if source[from] in [`'`, `"`] {
			from = fastc_c_literal_end(source, from)
			continue
		}
		if !source[from..].starts_with(head) {
			from++
			continue
		}
		index := from
		from = index + head.len
		if index > 0 && (source[index - 1].is_alnum() || source[index - 1] in [`_`, `.`, `>`]) {
			continue
		}
		mut depth := 1
		mut i := index + head.len
		for i < source.len && depth > 0 {
			c := source[i]
			if c in [`'`, `"`] {
				i = fastc_c_literal_end(source, i)
				continue
			}
			if c == `(` {
				depth++
			} else if c == `)` {
				depth--
			}
			i++
		}
		if depth != 0 {
			return source
		}
		return source[..index] + replacement + source[i..]
	}
	return source
}
