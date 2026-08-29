module fastc

import strings
import v3.token

// fastc_builtin_type_idx maps a primitive type name to V's canonical builtin
// type index (vlib/v/ast/types.v). Used to evaluate `typeof[T]().idx`.
fn fastc_builtin_type_idx(type_name string) ?int {
	return match type_name {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8', 'byte' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'bool' { 19 }
		'string' { 21 }
		'rune' { 22 }
		'array' { 23 }
		'map' { 24 }
		'chan' { 25 }
		'any' { 26 }
		'float_literal' { 27 }
		'int_literal' { 28 }
		'thread' { 29 }
		else { none }
	}
}

// render_typeof_generic_expression lowers `typeof[T]().idx` and `typeof[T]().name`
// (compile-time type reflection, used by vlib/orm) to a constant int / string.
fn (g &Parser) render_typeof_generic_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 8 || tokens[0].lit != 'typeof' || tokens[1].tok != .lsbr {
		return none
	}
	close_bracket := fastc_matching_delimiter(tokens, 1, .lsbr, .rsbr) or { return none }
	// Require exactly `typeof[T] ( ) . field` with `field` ending the expression.
	if close_bracket + 4 != tokens.len - 1 || tokens[close_bracket + 1].tok != .lpar
		|| tokens[close_bracket + 2].tok != .rpar || tokens[close_bracket + 3].tok != .dot
		|| tokens[tokens.len - 1].tok != .name {
		return none
	}
	type_tokens := tokens[2..close_bracket]
	if type_tokens.len != 1 || type_tokens[0].tok != .name {
		return none
	}
	type_name := type_tokens[0].lit
	match tokens[tokens.len - 1].lit {
		'idx' {
			idx := fastc_builtin_type_idx(type_name) or { return none }
			return FastcRenderedExpression{
				source: idx.str()
				typ:    'int'
			}
		}
		'name' {
			return FastcRenderedExpression{
				source: '_S("${type_name}")'
				typ:    'string'
			}
		}
		else {
			return none
		}
	}
}

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

fn (g &Parser) render_typeof_generic_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
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
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0
			|| i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		left_typeof := g.render_typeof_generic_expression(left_tokens)
		right_typeof := g.render_typeof_generic_expression(right_tokens)
		if left_typeof == none && right_typeof == none {
			return none
		}
		mut left := ''
		if left_value := left_typeof {
			left = left_value.source
		} else {
			left = g.render_comparison_operand(left_tokens, 'int') or { return none }
		}
		mut right := ''
		if right_value := right_typeof {
			right = right_value.source
		} else {
			right = g.render_comparison_operand(right_tokens, 'int') or { return none }
		}
		return FastcRenderedExpression{
			source: '((${left})${item.tok.str()}(${right}))'
			typ:    'bool'
		}
	}
	return none
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
	if tokens[name_index].tok !in [.name, .key_select] || tokens[open_index].tok != .lpar {
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
	is_option_cast := tokens.len > 0 && tokens[0].tok == .question
	type_start := if is_option_cast { 1 } else { 0 }
	mut open := -1
	mut c_type := ''
	type_qualifier_start := if tokens.len > 0 && tokens[0].tok in [.amp, .and] { 1 } else { 0 }
	for i, item in tokens {
		if item.tok != .lpar || i <= type_start {
			continue
		}
		if i >= 2 && tokens[i - 2].tok == .dot && !(i == type_qualifier_start + 3
			&& tokens[type_qualifier_start].tok == .name
			&& (tokens[type_qualifier_start].lit in g.imports
			|| tokens[type_qualifier_start].lit == 'C')) {
			// `receiver.u32()` is a method even though its name is also a primitive
			// type. Only a bare/qualified type at the start can introduce a cast.
			continue
		}
		// A leading `*` is a dereference around the cast, not part of its type.
		// The raw renderer preserves that unary operation while lowering the
		// nested `&Type(value)` cast.
		if tokens[0].tok == .mul {
			return none
		}
		if i == type_qualifier_start + 3 && tokens[type_qualifier_start].tok == .name
			&& tokens[type_qualifier_start].lit == 'C'
			&& ('C.${tokens[type_qualifier_start + 2].lit}' in g.functions
			|| !fastc_call_has_one_argument(tokens, i)) {
			return none
		}
		c_type = g.type_from_expression_tokens(tokens[type_start..i]) or { '' }
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
	inner_tokens := tokens[open + 1..close]
	if is_option_cast {
		if inner_tokens.len == 1 && inner_tokens[0].tok == .key_none {
			return FastcRenderedExpression{
				source: '(Option){.state=2}'
				typ:    'Option'
			}
		}
		inner_type := g.infer_expression_type(inner_tokens) or { '' }
		inner := g.render_call_argument_expression(inner_tokens, if inner_type == 'Option' {
			'Option'
		} else {
			c_type
		}) or { return none }
		return FastcRenderedExpression{
			source: if inner_type == 'Option' {
				inner
			} else {
				fastc_option_success_expression(c_type, inner)
			}
			typ:    'Option'
		}
	}
	inner := g.render_call_argument_expression(inner_tokens, c_type) or { return none }
	return FastcRenderedExpression{
		source: '((${c_type})(${inner}))'
		typ:    c_type
	}
}

// render_c_interface_object_address lowers the C-interop escape hatch used to free
// an interface object: `&C.mod__Interface(value)._object`. The raw `C.Type(value)`
// spelling denotes a pointer reinterpretation here; leaving it untouched makes C
// parse `Type` as a function call.
fn (g &Parser) render_c_interface_object_address(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len != 9 || tokens[0].tok != .amp || tokens[1].tok != .name || tokens[1].lit != 'C'
		|| tokens[2].tok != .dot || tokens[3].tok != .name || tokens[4].tok != .lpar
		|| tokens[6].tok != .rpar || tokens[7].tok != .dot || tokens[8].tok != .name
		|| tokens[8].lit != '_object' {
		return none
	}
	close := fastc_matching_rpar(tokens, 4) or { return none }
	if close != 6 {
		return none
	}
	c_type := tokens[3].lit
	if g.declared_kinds[g.semantic_type_key(c_type)] != .interface_ {
		return none
	}
	inner := g.render_call_argument_expression(tokens[5..6], '${c_type}*') or { return none }
	return FastcRenderedExpression{
		source: '&(((${c_type} *)(${inner}))->_object)'
		typ:    'voidptr*'
	}
}

fn (g &Parser) render_c_struct_sizeof(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len != 6 || tokens[0].tok != .key_sizeof || tokens[1].tok != .lpar
		|| tokens[2].tok != .name || tokens[2].lit != 'C' || tokens[3].tok != .dot
		|| tokens[4].tok != .name || tokens[5].tok != .rpar {
		return none
	}
	c_name := tokens[4].lit
	if '#Cstruct#${c_name}' !in g.declared_types {
		return none
	}
	return FastcRenderedExpression{
		source: 'sizeof(struct ${c_name})'
		typ:    'int'
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
		call_start := if i >= 4 && tokens[i - 4].tok == .name && tokens[i - 3].tok == .dot {
			i - 4
		} else {
			i - 2
		}
		call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
		mut named_start := -1
		for argument_index, argument in call_args {
			if argument.len >= 3 && argument[0].tok == .name && argument[1].tok == .colon {
				named_start = argument_index
				break
			}
		}
		if named_start >= 0 && named_start == signature.parameter_types.len - 1
			&& named_start <= call_args.len && (signature.last_parameter_is_params
			|| g.fastc_type_is_declared_struct(signature.parameter_types[named_start])) {
			mut rendered_arguments := []string{cap: signature.parameter_types.len}
			mut arguments_ok := true
			for argument_index, argument in call_args[..named_start] {
				rendered_argument := g.render_call_argument_expression(argument,
					signature.parameter_types[argument_index]) or {
					arguments_ok = false
					break
				}
				rendered_arguments << rendered_argument
			}
			if arguments_ok {
				named_initializer := g.render_named_struct_initializer(signature.parameter_types[named_start],
					call_args[named_start..]) or { '' }
				if named_initializer != '' {
					rendered_arguments << named_initializer
					call_source := '${fastc_method_c_name_for_key(type_key, tokens[i].lit)}(${rendered_arguments.join(',')})'
					if call_start == 0 && call_end == tokens.len - 1 {
						return FastcRenderedExpression{
							source: call_source
							typ:    signature.return_type
						}
					}
					raw_call := g.render_raw_expression_tokens(tokens[call_start..call_end + 1]) or {
						continue
					}
					if rendered.contains(raw_call) {
						rendered = rendered.replace(raw_call, call_source)
						result_type = signature.return_type
						changed = true
						continue
					}
				}
			}
		}
		if call_args.len == signature.parameter_types.len {
			mut rendered_arguments := []string{cap: call_args.len}
			mut arguments_ok := true
			for argument_index, argument in call_args {
				rendered_argument := g.render_call_argument_expression(argument,
					signature.parameter_types[argument_index]) or {
					arguments_ok = false
					break
				}
				rendered_arguments << rendered_argument
			}
			if arguments_ok {
				call_source := '${fastc_method_c_name_for_key(type_key, tokens[i].lit)}(${rendered_arguments.join(',')})'
				if call_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    signature.return_type
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[call_start..call_end + 1]) or {
					continue
				}
				if rendered.contains(raw_call) {
					rendered = rendered.replace(raw_call, call_source)
					result_type = signature.return_type
					changed = true
					continue
				}
			}
		}
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
	// A type alias (`type GitlyDb = sqlite.DB`) inherits its base type's methods, so a
	// method missing on the alias resolves against the underlying type.
	base := g.underlying_alias_type(receiver_type)
	if base != receiver_type {
		base_key := '${g.semantic_type_key(base)}.${name}'
		if base_key in g.functions {
			return base_key
		}
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

// resolve_method finds a method for `receiver_type`, promoting through embedded
// fields (`__embedded_N`) when the receiver has no direct method of that name (as
// V does with struct embedding). It returns the resolved function key and the
// member-access path to the embedded field whose type owns the method (empty for
// a direct method).
fn (g &Parser) resolve_method(receiver_type string, name string) (string, []string) {
	direct := g.method_function_key(receiver_type, name)
	if direct in g.functions {
		return direct, []string{}
	}
	layout_type := receiver_type.trim_right('*')
	for field in g.struct_field_info[layout_type] {
		if !field.name.starts_with('__embedded_') {
			continue
		}
		key, path := g.resolve_method(field.typ, name)
		if key in g.functions {
			mut full := [field.name]
			full << path
			return key, full
		}
	}
	return direct, []string{}
}

fn (g &Parser) specialized_method_return_type(receiver_type string, method_key string, signature FastcFunctionSignature) string {
	if method_key in ['map.keys', 'map.values'] {
		key_type, value_type := g.map_key_value_types(receiver_type) or {
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
	if tokens.len < 4 || tokens.last().tok != .rpar {
		return none
	}
	// The cast type may be unqualified (`Sum(value)`, type at token 0, `(` at 1) or
	// qualified (`mod.Sum(value)`, type at token 2, `(` at 3).
	mut type_key := ''
	mut open_index := 0
	if tokens[0].tok == .name && tokens[1].tok == .lpar {
		type_key = fastc_resolve_declared_type_key(g.module_name, tokens[0].lit, g.imports,
			g.declared_types) or { return none }
		open_index = 1
	} else if tokens.len >= 6 && tokens[0].tok == .name && tokens[1].tok == .dot
		&& tokens[2].tok == .name && tokens[3].tok == .lpar && tokens[0].lit in g.imports {
		qualified_key := fastc_type_key(g.imports[tokens[0].lit], tokens[2].lit)
		if qualified_key !in g.declared_types {
			return none
		}
		type_key = qualified_key
		open_index = 3
	} else {
		return none
	}
	interface_type := fastc_c_declared_type_name(type_key)
	// An explicit conversion to an interface or a sum type (`Animal(Dog{})`) boxes
	// the concrete operand; both share the boxed representation.
	if g.declared_kinds[type_key] != .interface_ && interface_type !in g.sum_types {
		return none
	}
	close := fastc_matching_rpar(tokens, open_index) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	prefix := '((${interface_type})('
	if !rendered_expression.starts_with(prefix) || !rendered_expression.ends_with('))') {
		return none
	}
	actual_type := g.infer_expression_type(tokens[open_index + 1..close]) or { return none }
	if actual_type == '' {
		return none
	}
	inner_tokens := tokens[open_index + 1..close]
	// A composite operand (array / map literal) does not survive as raw streamed
	// text (`[1,2,3]` is not valid C), so render it through the argument path.
	// A struct literal likewise needs its designated-initializer lowering before boxing.
	// Scalars and pointers keep the already-streamed spelling.
	actual_base := actual_type.trim_right('*')
	inner_source := if struct_literal := g.render_struct_literal_expression(inner_tokens) {
		struct_literal.source
	} else if actual_base.starts_with('Array_') || actual_base.starts_with('Map_') {
		g.render_call_argument_expression(inner_tokens, actual_type) or { return none }
	} else {
		rendered_expression[prefix.len..rendered_expression.len - 2]
	}
	return FastcRenderedExpression{
		source: g.interface_value_expression(interface_type, actual_type, inner_source)
		typ:    interface_type
	}
}

// render_mutable_map_value_pointer returns a pointer to a map entry, inserting a
// zero value first when the key is absent. Nested map assignments use it to turn
// `outer[key]` into an addressable inner map instead of emitting invalid C indexing.
fn (g &Parser) render_mutable_map_value_pointer(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens.last().tok != .rsbr {
		return none
	}
	mut open := -1
	mut depth := 0
	for i := tokens.len - 1; i >= 0; i-- {
		if tokens[i].tok == .rsbr {
			depth++
		} else if tokens[i].tok == .lsbr {
			depth--
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open <= 0 || open + 1 == tokens.len - 1 {
		return none
	}
	base_tokens := tokens[..open]
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type, value_type := g.map_key_value_types(map_type) or { return none }
	key_source := g.render_call_argument_expression(tokens[open + 1..tokens.len - 1], key_type) or {
		return none
	}
	mut map_address := ''
	if nested := g.render_mutable_map_value_pointer(base_tokens) {
		if nested.typ != map_type.trim_right('*') {
			return none
		}
		map_address = nested.source
	} else {
		map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
			g.globals[fastc_global_key(g.module_name, base_tokens[0].lit)] or {
				g.resolved_expression_name(base_tokens[0].lit, .unknown)
			}
		} else {
			g.render_member_receiver(base_tokens) or {
				g.render_raw_expression_tokens(base_tokens) or { return none }
			}
		}
		map_address = if map_type.ends_with('*') { map_source } else { '&(${map_source})' }
	}
	mut empty_value := '(${value_type}){0}'
	if nested_key_type, nested_value_type := g.map_key_value_types(value_type) {
		hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(nested_key_type)
		empty_value = '(${value_type})builtin__new_map(sizeof(${fastc_runtime_c_type(nested_key_type)}), sizeof(${fastc_runtime_c_type(nested_value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn})'
	}
	return FastcRenderedExpression{
		source: '({ ${key_type} __v_fastc_nested_map_key = (${key_source}); ${value_type} *__v_fastc_nested_map_value = (${value_type} *)builtin__map_get_check((map *)(${map_address}), &__v_fastc_nested_map_key); if (__v_fastc_nested_map_value == NULL) { ${value_type} __v_fastc_nested_map_empty = ${empty_value}; builtin__map_set((map *)(${map_address}), &__v_fastc_nested_map_key, &__v_fastc_nested_map_empty); __v_fastc_nested_map_value = (${value_type} *)builtin__map_get_check((map *)(${map_address}), &__v_fastc_nested_map_key); } __v_fastc_nested_map_value; })'
		typ:    value_type
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
			key_type, value_type := g.map_key_value_types(map_type) or { return none }
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
		key_type, value_type := g.map_key_value_types(map_type) or { return none }
		key_source := g.render_call_argument_expression(tokens[open + 1..close], key_type) or {
			return none
		}
		value_source := g.render_call_argument_expression(tokens[assignment_index + 1..],
			value_type) or { return none }
		mut map_address := ''
		if nested := g.render_mutable_map_value_pointer(base_tokens) {
			if nested.typ != map_type.trim_right('*') {
				return none
			}
			map_address = nested.source
		} else {
			map_source := g.render_member_receiver(base_tokens) or {
				g.render_raw_expression_tokens(base_tokens) or { return none }
			}
			map_address = if map_type.ends_with('*') { map_source } else { '&${map_source}' }
		}
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
		key_type, value_type := g.map_key_value_types(map_type) or { return none }
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

fn (g &Parser) render_selfhost_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if !g.selfhost || tokens.len < 4 || tokens[0].tok != .name
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
	argument_type := fastc_normalize_inferred_type(g.infer_expression_type(call_arguments[0]) or {
		return none
	})
	if g.underlying_alias_type(argument_type).trim_right('*') == 'string' {
		return none
	}
	argument := g.render_call_argument_expression(call_arguments[0], argument_type) or {
		return none
	}
	method_key, embedded_path := g.resolve_method(argument_type, 'str')
	if embedded_path.len > 0 {
		return none
	}
	signature := g.functions[method_key] or { return none }
	if signature.parameter_types.len == 0 || signature.return_type != 'string' {
		return none
	}
	expected_receiver := signature.parameter_types[0]
	receiver_argument := if expected_receiver.ends_with('*') && !argument_type.ends_with('*') {
		'&(${argument})'
	} else if !expected_receiver.ends_with('*') && argument_type.ends_with('*') {
		'*(${argument})'
	} else {
		argument
	}
	string_value := '${fastc_method_c_name(signature.module_name, expected_receiver, 'str')}(${receiver_argument})'
	return FastcRenderedExpression{
		source: 'builtin__${tokens[0].lit}(${string_value})'
		typ:    'void'
	}
}

fn (g &Parser) render_struct_literal_with_defaults(c_type string, layout_type string, explicit_initializers []string, rendered_fields []string, rendered_fields_by_name map[string]string) FastcRenderedExpression {
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

fn (g &Parser) render_empty_struct_initializer(c_type string) string {
	layout_type := c_type.trim_right('*')
	mut rendered_fields := []string{}
	mut rendered_fields_by_name := map[string]string{}
	for field in g.struct_field_info[layout_type] {
		if field.default_value == '' {
			continue
		}
		rendered_field := '.${fastc_c_identifier(field.name)}=(${field.default_value})'
		rendered_fields << rendered_field
		rendered_fields_by_name[field.name] = rendered_field
	}
	if rendered_fields.len == 0 {
		if c_type.ends_with('*') {
			return '(${c_type})v_fastc_interface_box(&(${layout_type}){0}, sizeof(${layout_type}))'
		}
		return '(${c_type}){0}'
	}
	empty_initializers := []string{}
	return g.render_struct_literal_with_defaults(c_type, layout_type, empty_initializers,
		rendered_fields, rendered_fields_by_name).source
}

// fastc_type_is_declared_struct reports whether `c_type` names a declared struct/union.
// (A missing key defaults to `.struct_`, the zero enum value, so the key must be present.)
fn (g &Parser) fastc_type_is_declared_struct(c_type string) bool {
	key := g.semantic_type_key(c_type)
	return key in g.declared_kinds && g.declared_kinds[key] in [.struct_, .union_]
}

fn (g &Parser) render_named_struct_initializer(c_type string, fields [][]FastcExpressionToken) ?string {
	mut tokens := [
		FastcExpressionToken{
			tok: .name
			lit: c_type
			typ: c_type
		},
		FastcExpressionToken{
			tok: .lcbr
			lit: '{'
		},
	]
	for index, field in fields {
		if index > 0 {
			tokens << FastcExpressionToken{
				tok: .comma
				lit: ','
			}
		}
		tokens << field
	}
	tokens << FastcExpressionToken{
		tok: .rcbr
		lit: '}'
	}
	rendered := g.render_struct_literal_expression(tokens) or { return none }
	return rendered.source
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
	mut c_type := if open == 1 && tokens[0].typ != '' {
		tokens[0].typ
	} else {
		g.type_from_expression_tokens(tokens[..open]) or { '' }
	}
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
	if open + 1 < close {
		items := fastc_expression_list_items(tokens, open + 1, close) or { return none }
		mut is_positional := false
		if !fastc_expression_tokens_contain(tokens[open + 1..close], .ellipsis) {
			for item in items {
				if item.len == 0 {
					continue
				}
				if !(item.len >= 2 && item[0].tok == .name && item[1].tok == .colon)
					&& !(item.len == 1 && item[0].tok == .name && item[0].lit in fields) {
					is_positional = true
					break
				}
			}
		}
		if is_positional {
			mut values := []string{cap: items.len}
			for item_index, item in items {
				expected_type := if !is_c_struct_literal
					&& item_index < g.struct_field_info[layout_type].len {
					g.struct_field_info[layout_type][item_index].typ
				} else {
					''
				}
				values << g.render_call_argument_expression(item, expected_type) or { return none }
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
	mut fixed_array_copies := []string{}
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
			} else {
				// Initializing an embedded field by its type name:
				// `Derived{ Base: Base{...} }` sets the `__embedded_N` field.
				for embed_field in g.struct_field_info[layout_type] {
					if embed_field.name.starts_with('__embedded_') && (embed_field.typ == field_name
						|| embed_field.typ.all_after_last('__') == field_name) {
						c_field_name = embed_field.name
						expected_type = embed_field.typ
						break
					}
				}
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
			value := g.render_call_argument_expression(value_tokens, expected_type) or {
				return none
			}
			is_raw_fixed_array := value_tokens.len > 1 || (value_tokens.len == 1
				&& value_tokens[0].tok == .name
				&& fastc_global_key(g.module_name, value_tokens[0].lit) in g.globals)
			copy_source := if is_raw_fixed_array {
				value
			} else if expected_type.ends_with('*') {
				'(${value})->data'
			} else {
				'(${value}).data'
			}
			fixed_array_copies << 'memcpy(__v_fastc_struct_fixed.${c_field_name}, ${copy_source}, sizeof(__v_fastc_struct_fixed.${c_field_name}));'
			field_values[field_name] = value
			continue
		}
		value := if value_tokens.len == 1 && value_tokens[0].source != '' {
			// A field value carried as a pre-rendered `({ ... })` (e.g. an `or`-unwrap) is used
			// directly so its internal temporaries stay self-contained.
			value_tokens[0].source
		} else {
			g.render_call_argument_expression(value_tokens, expected_type) or { return none }
		}
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
		array_type := c_type.trim_right('*')
		element_type := g.array_element_type(array_type) or { return none }
		length := field_values['len'] or { '0' }
		capacity := field_values['cap'] or { '0' }
		base := '((${array_type})builtin____new_array(${length},${capacity},sizeof(${element_type})))'
		mut value_source := base
		if initial := field_values['init'] {
			value_source = '({ ${explicit_initializers.join(' ')} ${array_type} __v_fastc_array_init = ${base}; ${element_type} __v_fastc_array_default = (${initial}); for (int __v_fastc_array_index = 0; __v_fastc_array_index < __v_fastc_array_init.len; __v_fastc_array_index++) { ((${element_type} *)__v_fastc_array_init.data)[__v_fastc_array_index] = __v_fastc_array_default; } __v_fastc_array_init; })'
		} else if explicit_initializers.len > 0 {
			value_source = '({ ${explicit_initializers.join(' ')} ${base}; })'
		}
		if c_type.ends_with('*') {
			value_source = '({ ${array_type} __v_fastc_array_pointer_value = (${value_source}); (${c_type})v_fastc_interface_box(&__v_fastc_array_pointer_value, sizeof(${array_type})); })'
		}
		return FastcRenderedExpression{
			source: value_source
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
			copy_statements := fixed_array_copies.join(' ')
			return FastcRenderedExpression{
				source: '({ ${base_type} __v_fastc_struct_update = *(${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} ${copy_statements.replace('__v_fastc_struct_fixed',
					'__v_fastc_struct_update')} (${c_type})v_fastc_interface_box(&__v_fastc_struct_update, sizeof(${base_type})); })'
				typ:    c_type
			}
		}
		copy_statements := fixed_array_copies.join(' ')
		return FastcRenderedExpression{
			source: '({ ${c_type} __v_fastc_struct_update = (${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} ${copy_statements.replace('__v_fastc_struct_fixed',
				'__v_fastc_struct_update')} __v_fastc_struct_update; })'
			typ:    c_type
		}
	}
	if has_applied_defaults {
		rendered := g.render_struct_literal_with_defaults(c_type, layout_type,
			explicit_initializers, rendered_fields, rendered_fields_by_name)
		if fixed_array_copies.len == 0 {
			return rendered
		}
		access := if c_type.ends_with('*') { '->' } else { '.' }
		copies := fixed_array_copies.join(' ').replace('__v_fastc_struct_fixed.',
			'__v_fastc_struct_with_fixed${access}')
		return FastcRenderedExpression{
			source: '({ ${c_type} __v_fastc_struct_with_fixed = (${rendered.source}); ${copies} __v_fastc_struct_with_fixed; })'
			typ:    c_type
		}
	}
	literal_source := if c_type.ends_with('*') {
		'(${c_type})v_fastc_interface_box(&(${c_type.trim_right('*')}){${rendered_fields.join(',')}}, sizeof(${c_type.trim_right('*')}))'
	} else {
		'(${c_type}){${rendered_fields.join(',')}}'
	}
	if fixed_array_copies.len > 0 {
		base_type := c_type.trim_right('*')
		copies := fixed_array_copies.join(' ')
		result := if c_type.ends_with('*') {
			'(${c_type})v_fastc_interface_box(&__v_fastc_struct_fixed, sizeof(${base_type}))'
		} else {
			'__v_fastc_struct_fixed'
		}
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${base_type} __v_fastc_struct_fixed = (${base_type}){${rendered_fields.join(',')}}; ${copies} ${result}; })'
			typ:    c_type
		}
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
			constant_name := g.constants[fastc_constant_key(module_name, field_name)] or { '' }
			mut resolved_names := []string{}
			if constant_name != '' {
				resolved_names << constant_name
			}
			function_key := fastc_function_key(module_name, field_name)
			if function_key in g.functions || function_key in g.mono_functions {
				resolved_names << fastc_c_function_name_for_key(function_key)
			}
			for resolved_name in resolved_names {
				needle := '.${resolved_name}='
				if rendered.contains(needle) {
					rendered = rendered.replace(needle, '.${fastc_c_identifier(field_name)}=')
					changed = true
				}
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
	source := if overloaded := g.render_overloaded_assignment(left.source, right, left.typ,
		operator)
	{
		overloaded
	} else if operator == .right_shift_unsigned_assign {
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
	rhs_tokens := tokens[assignment_index + 1..]
	mut right := ''
	if rhs_tokens.len == 2 && rhs_tokens[0].tok == .lsbr && rhs_tokens[1].tok == .rsbr
		&& left_type.trim_right('*').starts_with('Array_') {
		// An empty array literal `[]` has no element type of its own; assigned to an
		// array target it lowers to a typed empty array from the target's type (the
		// standalone `x = []` case is handled where `expected_expression_type` is set).
		right = '(${left_type}){0}'
	} else {
		right = g.render_call_argument_expression(rhs_tokens, left_type) or { return none }
	}
	operator := tokens[assignment_index].tok
	option_payload_type := g.option_value_type_for_expression(left_tokens)
	if g.selfhost && operator == .assign && left_type == 'Option' && option_payload_type != '' {
		rhs_type := fastc_normalize_inferred_type(g.infer_expression_type(rhs_tokens) or { '' })
		if rhs_type != 'Option' {
			if rhs_type.trim_right('*') == 'IError' {
				right = '(Option){.err=${right}, .state=1}'
			} else {
				payload := g.render_call_argument_expression(rhs_tokens, option_payload_type) or {
					right
				}
				right = fastc_option_success_expression(option_payload_type, payload)
			}
		}
	}
	source := if operator == .plus_assign && g.underlying_alias_type(left_type) == 'string' {
		'${left}=builtin__string_plus(${left},${right})'
	} else if overloaded := g.render_overloaded_assignment(left, right, left_type, operator) {
		overloaded
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

fn (g &Parser) render_overloaded_assignment(target string, value string, target_type string, assignment token.Token) ?string {
	operator := match assignment {
		.plus_assign { '+' }
		.minus_assign { '-' }
		.mul_assign { '*' }
		.div_assign { '/' }
		.mod_assign { '%' }
		.power_assign { '**' }
		.or_assign { '|' }
		.xor_assign { '^' }
		else { return none }
	}
	method_key := g.method_function_key(target_type, operator)
	signature := g.functions[method_key] or { return none }
	if signature.parameter_types.len < 2 || signature.is_disabled {
		return none
	}
	receiver_type := signature.parameter_types[0]
	method_name := fastc_method_c_name(signature.module_name, receiver_type, operator)
	return '({ ${target_type} *__v_fastc_overloaded_assignment_target = &(${target}); *__v_fastc_overloaded_assignment_target = ${method_name}(*__v_fastc_overloaded_assignment_target,${value}); })'
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

fn fastc_overloaded_binary_precedence(tok token.Token) int {
	return match tok {
		.pipe { 1 }
		.xor { 2 }
		.amp { 3 }
		.left_shift, .right_shift { 4 }
		.plus, .minus { 5 }
		.mul, .div, .mod { 6 }
		else { 0 }
	}
}

fn (g &Parser) render_overloaded_comparison_expression(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, operator token.Token) ?FastcRenderedExpression {
	if array_comparison := g.render_array_equality_comparison(left_tokens, right_tokens, operator) {
		return array_comparison
	}
	mut method_operator := ''
	mut negate := false
	match operator {
		.eq {
			method_operator = '=='
		}
		.ne {
			method_operator = '=='
			negate = true
		}
		.lt {
			method_operator = '<'
		}
		.gt {
			method_operator = '<'
		}
		.le {
			method_operator = '<'
			negate = true
		}
		.ge {
			method_operator = '<'
			negate = true
		}
		else {
			return none
		}
	}
	reverse_arguments := operator in [.gt, .le]
	receiver_tokens := if reverse_arguments { right_tokens } else { left_tokens }
	argument_tokens := if reverse_arguments { left_tokens } else { right_tokens }
	receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
	method_key := g.method_function_key(receiver_type, method_operator)
	if method_key !in g.functions {
		return none
	}
	signature := g.functions[method_key]
	if signature.parameter_types.len < 2 || signature.is_disabled {
		return none
	}
	receiver := g.render_call_argument_expression(receiver_tokens, signature.parameter_types[0]) or {
		return none
	}
	argument := g.render_call_argument_expression(argument_tokens, signature.parameter_types[1]) or {
		return none
	}
	call := '${fastc_method_c_name(signature.module_name, signature.parameter_types[0],
		method_operator)}(${receiver},${argument})'
	return FastcRenderedExpression{
		source: if negate { '!(${call})' } else { call }
		typ:    'bool'
	}
}

fn (g &Parser) render_array_equality_comparison(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, operator token.Token) ?FastcRenderedExpression {
	if operator !in [.eq, .ne] {
		return none
	}
	left_type := fastc_normalize_inferred_type(g.infer_expression_type(left_tokens) or {
		return none
	})
	right_type := fastc_normalize_inferred_type(g.infer_expression_type(right_tokens) or {
		return none
	})
	left_layout := left_type.trim_right('*')
	if left_type != right_type
		|| (!left_layout.starts_with('Array_') && !left_layout.starts_with('FixedArray_')) {
		return none
	}
	element_type := g.array_element_type(left_type) or { return none }
	resolved_element := g.underlying_alias_type(element_type).trim_right('*')
	is_scalar := fastc_is_numeric_expression_type(resolved_element) || resolved_element == 'bool'
		|| fastc_is_pointer_type(element_type)
		|| g.underlying_enum_type_key(g.semantic_type_key(element_type)) != none
	if resolved_element != 'string' && !is_scalar {
		return none
	}
	if left_layout.starts_with('FixedArray_') {
		length := fastc_fixed_array_length(left_layout) or { return none }
		left_data := g.render_fixed_array_equality_data(left_tokens, left_type) or { return none }
		right_data := g.render_fixed_array_equality_data(right_tokens, right_type) or {
			return none
		}
		element_comparison := if resolved_element == 'string' {
			'builtin__string_eq(__v_fastc_array_eq_left[__v_fastc_array_eq_index], __v_fastc_array_eq_right[__v_fastc_array_eq_index])'
		} else {
			'(__v_fastc_array_eq_left[__v_fastc_array_eq_index] == __v_fastc_array_eq_right[__v_fastc_array_eq_index])'
		}
		result := if operator == .ne { '!__v_fastc_array_equal' } else { '__v_fastc_array_equal' }
		return FastcRenderedExpression{
			source: '({ ${element_type} *__v_fastc_array_eq_left = (${element_type} *)(${left_data}); ${element_type} *__v_fastc_array_eq_right = (${element_type} *)(${right_data}); bool __v_fastc_array_equal = true; for (int __v_fastc_array_eq_index = 0; __v_fastc_array_eq_index < ${length}; __v_fastc_array_eq_index++) { if (!(${element_comparison})) { __v_fastc_array_equal = false; break; } } ${result}; })'
			typ:    'bool'
		}
	}
	left := g.render_call_argument_expression(left_tokens, left_type) or { return none }
	right := g.render_call_argument_expression(right_tokens, right_type) or { return none }
	element_comparison := if resolved_element == 'string' {
		'builtin__string_eq(((${element_type} *)__v_fastc_array_eq_left.data)[__v_fastc_array_eq_index], ((${element_type} *)__v_fastc_array_eq_right.data)[__v_fastc_array_eq_index])'
	} else {
		'(((${element_type} *)__v_fastc_array_eq_left.data)[__v_fastc_array_eq_index] == ((${element_type} *)__v_fastc_array_eq_right.data)[__v_fastc_array_eq_index])'
	}
	result := if operator == .ne { '!__v_fastc_array_equal' } else { '__v_fastc_array_equal' }
	return FastcRenderedExpression{
		source: '({ ${left_type} __v_fastc_array_eq_left = (${left}); ${right_type} __v_fastc_array_eq_right = (${right}); bool __v_fastc_array_equal = __v_fastc_array_eq_left.len == __v_fastc_array_eq_right.len; if (__v_fastc_array_equal) { for (int __v_fastc_array_eq_index = 0; __v_fastc_array_eq_index < __v_fastc_array_eq_left.len; __v_fastc_array_eq_index++) { if (!(${element_comparison})) { __v_fastc_array_equal = false; break; } } } ${result}; })'
		typ:    'bool'
	}
}

fn (g &Parser) render_fixed_array_equality_data(tokens []FastcExpressionToken, c_type string) ?string {
	layout_type := c_type.trim_right('*')
	if literal := g.render_array_literal_argument(tokens, layout_type) {
		return '(${literal.source}).data'
	}
	mut initializer_open := -1
	for i, item in tokens {
		if item.tok == .lcbr {
			initializer_open = i
			break
		}
	}
	if initializer_open > 0 && tokens.last().tok == .rcbr && initializer_open + 1 == tokens.len - 1 {
		initializer_type := g.array_initializer_type(tokens[..initializer_open]) or { '' }
		if initializer_type == layout_type {
			return '((${layout_type}){0}).data'
		}
	}
	source := if member := g.render_member_receiver(tokens) {
		member
	} else if access := g.render_array_access_expression(tokens) {
		access.source
	} else {
		g.render_raw_expression_tokens(tokens) or { return none }
	}
	if g.fixed_array_uses_raw_storage(tokens) {
		return source
	}
	return if c_type.ends_with('*') { '(${source})->data' } else { '(${source}).data' }
}

fn (g &Parser) render_overloaded_binary_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len >= 3 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		close := fastc_matching_rpar(tokens, 0) or { -1 }
		if close == tokens.len - 1 {
			inner := g.render_overloaded_binary_expression(tokens[1..tokens.len - 1]) or {
				return none
			}
			return FastcRenderedExpression{
				source: '((${inner.source}))'
				typ:    inner.typ
			}
		}
	}
	if boolean_index := fastc_lowest_precedence_operator_index(tokens, 0, tokens.len) {
		left_tokens := tokens[..boolean_index]
		right_tokens := tokens[boolean_index + 1..]
		operator := tokens[boolean_index].tok
		if comparison := g.render_overloaded_comparison_expression(left_tokens, right_tokens,
			operator)
		{
			return comparison
		}
		mut left_special := FastcRenderedExpression{}
		if rendered := g.render_overloaded_binary_expression(left_tokens) {
			left_special = rendered
		}
		mut right_special := FastcRenderedExpression{}
		if rendered := g.render_overloaded_binary_expression(right_tokens) {
			right_special = rendered
		}
		if left_special.source != '' || right_special.source != '' {
			left_expected := if operator in [.and, .logical_or] {
				'bool'
			} else if left_special.typ != '' {
				left_special.typ
			} else {
				g.infer_expression_type(left_tokens) or { '' }
			}
			right_expected := if operator in [.and, .logical_or] {
				'bool'
			} else if right_special.typ != '' {
				right_special.typ
			} else {
				g.infer_expression_type(right_tokens) or { '' }
			}
			left := if left_special.source != '' {
				left_special.source
			} else {
				g.render_call_argument_expression(left_tokens, left_expected) or { return none }
			}
			right := if right_special.source != '' {
				right_special.source
			} else {
				g.render_call_argument_expression(right_tokens, right_expected) or { return none }
			}
			return FastcRenderedExpression{
				source: '((${left})${operator.str()}(${right}))'
				typ:    'bool'
			}
		}
		return none
	}
	mut depth := 0
	mut operator_index := -1
	mut precedence := 100
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && i > 0 && i + 1 < tokens.len {
			candidate_precedence := fastc_overloaded_binary_precedence(item.tok)
			if candidate_precedence > 0 && candidate_precedence <= precedence {
				operator_index = i
				precedence = candidate_precedence
			}
		}
	}
	if operator_index < 1 {
		return none
	}
	left_tokens := tokens[..operator_index]
	right_tokens := tokens[operator_index + 1..]
	left_type := g.infer_expression_type(left_tokens) or { return none }
	operator := tokens[operator_index].tok.str()
	method_key := g.method_function_key(left_type, operator)
	if method_key !in g.functions {
		mut left_special := FastcRenderedExpression{}
		if rendered := g.render_overloaded_binary_expression(left_tokens) {
			left_special = rendered
		}
		mut right_special := FastcRenderedExpression{}
		if rendered := g.render_overloaded_binary_expression(right_tokens) {
			right_special = rendered
		}
		if left_special.source == '' && right_special.source == '' {
			return none
		}
		right_type := g.infer_expression_type(right_tokens) or { return none }
		left := if left_special.source != '' {
			left_special.source
		} else {
			g.render_call_argument_expression(left_tokens, left_type) or { return none }
		}
		right := if right_special.source != '' {
			right_special.source
		} else {
			g.render_call_argument_expression(right_tokens, right_type) or { return none }
		}
		return FastcRenderedExpression{
			source: '((${left})${operator}(${right}))'
			typ:    g.infer_expression_type(tokens) or { left_type }
		}
	}
	signature := g.functions[method_key]
	if signature.parameter_types.len < 2 || signature.is_disabled {
		return none
	}
	left := g.render_call_argument_expression(left_tokens, signature.parameter_types[0]) or {
		return none
	}
	right := g.render_call_argument_expression(right_tokens, signature.parameter_types[1]) or {
		return none
	}
	return FastcRenderedExpression{
		source: '${fastc_method_c_name(signature.module_name, signature.parameter_types[0],
			operator)}(${left},${right})'
		typ:    signature.return_type
	}
}

fn (g &Parser) render_pointer_member_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok != .dot || tokens[i + 1].tok != .name || i + 2 >= tokens.len
			|| tokens[i + 2].tok != .lpar {
			continue
		}
		// A qualified function call can contain an embedded-field argument that
		// still needs promotion here. A real method call is rendered later with
		// its receiver and arguments, so preserve the old early exit for it.
		is_qualified_call := tokens[i - 1].tok == .name
			&& (tokens[i - 1].lit in g.imports || tokens[i - 1].lit == 'C')
			&& (i < 2 || tokens[i - 2].tok != .dot)
		method_marker := '.${tokens[i + 1].lit}('
		pointer_method_marker := '->${tokens[i + 1].lit}('
		if !is_qualified_call && (rendered_expression.contains(method_marker)
			|| rendered_expression.contains(pointer_method_marker)) {
			return none
		}
	}
	mut rendered := rendered_expression
	mut changed := false
	// Promote pointer-rooted embedded fields even when the member chain is nested
	// inside a call. Value-rooted and indexed chains keep using their dedicated
	// member/slice renderers, which also preserve fixed-array storage semantics.
	for start, item in tokens {
		if item.tok != .name || (start > 0 && tokens[start - 1].tok == .dot) {
			continue
		}
		root_type := g.infer_expression_type(tokens[start..start + 1]) or { continue }
		root_is_reference := if local := g.locals[item.lit] { local.is_reference } else { false }
		if !root_type.ends_with('*') && !root_is_reference {
			continue
		}
		mut end := start + 1
		for end + 1 < tokens.len && tokens[end].tok == .dot && tokens[end + 1].tok == .name {
			if end + 2 < tokens.len && tokens[end + 2].tok == .lpar {
				break
			}
			end += 2
		}
		if end <= start + 1 || (end < tokens.len && tokens[end].tok == .lsbr) {
			continue
		}
		chain_tokens := tokens[start..end]
		raw_chain := g.render_raw_expression_tokens(chain_tokens) or { continue }
		promoted_chain := g.render_member_receiver(chain_tokens) or { continue }
		mut needle := raw_chain
		if !rendered.contains(needle) {
			root_source := g.resolved_expression_name(item.lit, .unknown)
			pointer_chain := raw_chain.replace_once('${root_source}.', '${root_source}->')
			if rendered.contains(pointer_chain) {
				needle = pointer_chain
			}
		}
		if promoted_chain != needle && rendered.contains(needle) {
			rendered = rendered.replace(needle, promoted_chain)
			changed = true
		}
	}
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			continue
		}
		if i + 2 < tokens.len && tokens[i + 2].tok == .lpar {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i)
		receiver_tokens := tokens[receiver_start..i]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		if tokens[i + 1].lit in ['len', 'cap'] {
			if fixed_length := fastc_fixed_array_length(receiver_type.trim_right('*')) {
				raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { continue }
				receiver_source := g.render_member_receiver(receiver_tokens) or { raw_receiver }
				mut needle := '${receiver_source}.${tokens[i + 1].lit}'
				if !rendered.contains(needle) {
					needle = '${raw_receiver}.${tokens[i + 1].lit}'
				}
				if rendered.contains(needle) {
					rendered = rendered.replace(needle, fixed_length)
					changed = true
				}
				continue
			}
		}
		if !receiver_type.ends_with('*') {
			continue
		}
		receiver_source := g.render_member_receiver(receiver_tokens) or {
			g.render_membership_candidate(receiver_tokens, '') or { continue }
		}
		needle := '${receiver_source}.${tokens[i + 1].lit}'
		replaced := fastc_replace_c_identifier(rendered, needle,
			'${receiver_source}->${tokens[i + 1].lit}')
		if replaced != rendered {
			rendered = replaced
			changed = true
			continue
		}
		raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
		raw_needle := '${raw_receiver}.${tokens[i + 1].lit}'
		raw_replaced := fastc_replace_c_identifier(rendered, raw_needle, '${raw_receiver}->${tokens[
			i + 1].lit}')
		if raw_receiver != '' && raw_replaced != rendered {
			rendered = raw_replaced
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
	for open := tokens.len - 1; open >= 0; open-- {
		item := tokens[open]
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
		if base_type.trim_right('*').starts_with('Map_') {
			lookup_tokens := tokens[start..close + 1]
			lookup := g.render_map_expression(lookup_tokens) or { continue }
			raw_lookup := g.render_raw_expression_tokens(lookup_tokens) or { continue }
			if rendered.contains(raw_lookup) {
				rendered = rendered.replace(raw_lookup, lookup.source)
				changed = true
			}
			continue
		}
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

fn fastc_trim_expression_parentheses(tokens []FastcExpressionToken) []FastcExpressionToken {
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar && tokens[end - 1].tok == .rpar {
		trimmed := tokens[start..end]
		close := fastc_matching_delimiter(trimmed, 0, .lpar, .rpar) or { break }
		if close != trimmed.len - 1 {
			break
		}
		start++
		end--
	}
	return tokens[start..end].clone()
}

// render_refined_enum_logical_expression lowers `x is Enum && x == .value`.
// The right side is safe to unbox because C's `&&` preserves V's short-circuit
// refinement semantics.
fn (g &Parser) render_refined_enum_logical_expression(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken) ?FastcRenderedExpression {
	left := fastc_trim_expression_parentheses(left_tokens)
	if left.len < 3 || left[0].tok != .name || left[1].tok != .key_is {
		return none
	}
	local := g.locals[left[0].lit] or { return none }
	boxed_type := fastc_normalize_inferred_type(local.typ)
	if !g.is_boxed_type(boxed_type) {
		return none
	}
	target_type := g.type_from_expression_tokens(left[2..]) or { return none }
	enum_type := fastc_normalize_inferred_type(target_type).trim_right('*')
	if g.declared_kinds[g.semantic_type_key(enum_type)] != .enum_ {
		return none
	}
	right := fastc_trim_expression_parentheses(right_tokens)
	mut comparison_index := -1
	for i, item in right {
		if item.tok in [.eq, .ne, .lt, .gt, .le, .ge] {
			if comparison_index != -1 {
				return none
			}
			comparison_index = i
		}
	}
	if comparison_index == -1 {
		return none
	}
	comparison := right[comparison_index].tok.str()
	comparison_left := right[..comparison_index]
	comparison_right := right[comparison_index + 1..]
	mut enum_value := ''
	mut enum_first := false
	if comparison_left.len == 1 && comparison_left[0].tok == .name
		&& comparison_left[0].lit == left[0].lit && comparison_right.len == 2
		&& comparison_right[0].tok == .dot && comparison_right[1].tok == .name {
		enum_value = comparison_right[1].lit
	} else if comparison_right.len == 1 && comparison_right[0].tok == .name
		&& comparison_right[0].lit == left[0].lit && comparison_left.len == 2
		&& comparison_left[0].tok == .dot && comparison_left[1].tok == .name {
		enum_value = comparison_left[1].lit
		enum_first = true
	} else {
		return none
	}
	subject := fastc_c_identifier(left[0].lit)
	access := if boxed_type.ends_with('*') { '->' } else { '.' }
	type_test := '((${subject}${access}_typ) == __v_typeid_${enum_type})'
	concrete := '*((${enum_type} *)${subject}${access}_object)'
	value := '${enum_type}__${enum_value}'
	right_source := if enum_first {
		'((${value}) ${comparison} (${concrete}))'
	} else {
		'((${concrete}) ${comparison} (${value}))'
	}
	return FastcRenderedExpression{
		source: '((${type_test})&&(${right_source}))'
		typ:    'bool'
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
			if item.tok == .and {
				if refined := g.render_refined_enum_logical_expression(tokens[..i], tokens[i + 1..]) {
					return refined
				}
			}
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
	if element_type := g.array_element_type(layout_type) {
		return g.struct_equality_is_supported(element_type, seen)
	}
	if layout_type in ['Option', 'array', 'map'] || layout_type.starts_with('Map_') {
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
	if element_type := g.array_element_type(layout_type) {
		left_array := '__v_fastc_array_eq_left'
		right_array := '__v_fastc_array_eq_right'
		equal := '__v_fastc_array_eq_equal'
		index := '__v_fastc_array_eq_index'
		left_element := '((${element_type} *)${left_array}.data)[${index}]'
		right_element := '((${element_type} *)${right_array}.data)[${index}]'
		element_equality :=
			g.struct_equality_source(left_element, right_element, element_type, seen)
		return '({ ${layout_type} ${left_array} = (${left}); ${layout_type} ${right_array} = (${right}); bool ${equal} = ${left_array}.len == ${right_array}.len; for (int ${index} = 0; ${equal} && ${index} < ${left_array}.len; ${index}++) { if (!(${element_equality})) { ${equal} = false; } } ${equal}; })'
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
			if left_layout == right_layout && left_layout in g.sum_types {
				// Sum-type equality (`value == Primitive(Null{})`): the boxed struct
				// cannot be compared with C `==`, so compare the variant tag (and, for a
				// scalar variant cast on the right, the unboxed value).
				if sum_eq := g.render_sum_type_equality(left_tokens, right_tokens, left_layout) {
					source := if item.tok == .ne { '!(${sum_eq})' } else { sum_eq }
					return FastcRenderedExpression{
						source: source
						typ:    'bool'
					}
				}
				return none
			}
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

// render_sum_type_equality lowers `left == SumType(operand)` (both of sum type
// `sum_type`) to a variant-tag comparison on the left's boxed `_typ`, plus the
// unboxed value for a scalar/string variant. A fieldless or struct variant matches
// on the tag alone. Returns none when the right side is not a variant cast.
fn (g &Parser) render_sum_type_equality(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, sum_type string) ?string {
	mut operand_start := -1
	mut open_index := -1
	if right_tokens.len >= 3 && right_tokens[0].tok == .name && right_tokens[1].tok == .lpar {
		operand_start = 2
		open_index = 1
	} else if right_tokens.len >= 5 && right_tokens[0].tok == .name && right_tokens[1].tok == .dot
		&& right_tokens[2].tok == .name && right_tokens[3].tok == .lpar {
		operand_start = 4
		open_index = 3
	}
	if operand_start < 0 {
		return none
	}
	close := fastc_matching_rpar(right_tokens, open_index) or { return none }
	if close != right_tokens.len - 1 {
		return none
	}
	operand_tokens := right_tokens[operand_start..close]
	inferred_variant := g.infer_expression_type(operand_tokens) or { return none }
	variant_type := fastc_normalize_inferred_type(inferred_variant)
	if variant_type == '' {
		return none
	}
	left := g.render_comparison_operand(left_tokens, sum_type) or { return none }
	left_temp := '__v_fastc_sum_left'
	tag := '${left_temp}._typ == __v_typeid_${variant_type}'
	mut comparison := tag
	if variant_type == 'string' {
		operand := g.render_call_argument_expression(operand_tokens, variant_type) or {
			return none
		}
		comparison = '(${tag}) && builtin__string_eq(*(string *)${left_temp}._object, ${operand})'
	} else if fastc_primitive_c_type(variant_type) != none {
		operand := g.render_call_argument_expression(operand_tokens, variant_type) or {
			return none
		}
		comparison = '(${tag}) && (*(${variant_type} *)${left_temp}._object == (${operand}))'
	}
	return '({ ${sum_type} ${left_temp} = (${left}); ${comparison}; })'
}

// render_as_cast_expression lowers `<boxed> as Type`. A boxed sum-type / interface
// value shares the `{_object, _typ, _methods}` layout and dispatches by `_typ`, so a
// downcast to ANOTHER interface / sum type just re-boxes the same object under the
// target type, and a cast to a CONCRETE type unboxes the stored object. Returns none
// unless `as` is the top-level operator, the right side is a declared type, and the
// left operand is a boxed value.
fn (g &Parser) render_as_cast_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut as_index := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.key_as {
				if depth == 0 {
					as_index = i
				}
			}
			else {}
		}
	}
	if as_index <= 0 {
		return none
	}
	left_tokens := tokens[..as_index]
	mut type_key := ''
	if as_index == tokens.len - 2 && tokens[as_index + 1].tok == .name {
		type_key = fastc_resolve_declared_type_key(g.module_name, tokens[as_index + 1].lit,
			g.imports, g.declared_types) or { return none }
	} else if as_index == tokens.len - 4 && tokens[as_index + 1].tok == .name
		&& tokens[as_index + 2].tok == .dot && tokens[as_index + 3].tok == .name
		&& tokens[as_index + 1].lit in g.imports {
		type_key = fastc_type_key(g.imports[tokens[as_index + 1].lit], tokens[as_index + 3].lit)
		if type_key !in g.declared_types {
			return none
		}
	} else {
		return none
	}
	target_c := fastc_c_declared_type_name(type_key)
	inferred_left := g.infer_expression_type(left_tokens) or { return none }
	left_type := fastc_normalize_inferred_type(inferred_left)
	if !g.is_boxed_type(left_type) {
		return none
	}
	left_source := g.render_call_argument_expression(left_tokens, left_type) or { return none }
	access := if left_type.ends_with('*') { '->' } else { '.' }
	src := '__v_fastc_as_src'
	if g.is_boxed_type(target_c) {
		return FastcRenderedExpression{
			source: '({ ${left_type} ${src} = (${left_source}); (${target_c}){._object = ${src}${access}_object, ._typ = ${src}${access}_typ, ._methods = ${src}${access}_methods}; })'
			typ:    target_c
		}
	}
	return FastcRenderedExpression{
		source: '({ ${left_type} ${src} = (${left_source}); *((${target_c} *)${src}${access}_object); })'
		typ:    target_c
	}
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
	if g.selfhost && tokens.len > 1 && tokens.last().tok == .not
		&& !fastc_trailing_not_marks_fixed_array_literal(tokens) {
		if propagation := g.render_option_propagation(tokens[..tokens.len - 1]) {
			return propagation.source
		}
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if integer_comparison := g.render_mixed_integer_comparison_expression(tokens) {
		return integer_comparison.source
	}
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation.source
	}
	if struct_literal := g.render_struct_literal_expression(tokens) {
		return struct_literal.source
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access.source
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
	if tokens.len == 1 && tokens[0].source != '' {
		// A single token carrying a pre-rendered `({ ... })` (e.g. an `or`-unwrap synthesized
		// as an array element or call argument): use it directly.
		return tokens[0].source
	}
	if tokens.len > 1 && tokens[0].tok == .amp && tokens[0].is_mut_argument {
		if g.selfhost && expected_type == 'voidptr*' {
			actual_type := g.infer_expression_type(tokens[1..]) or { '' }
			if actual_type !in ['', 'voidptr'] {
				inner := g.render_call_argument_expression(tokens[1..], actual_type) or {
					return none
				}
				return '((voidptr *)(&(${inner})))'
			}
		}
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
	if g.selfhost && expected_type == 'Option' {
		if tokens.len == 1 && tokens[0].tok == .key_none {
			return '(Option){.state=2}'
		}
		actual_type := fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { '' })
		if actual_type !in ['', 'Option'] {
			value := g.render_call_argument_expression(tokens, actual_type) or { return none }
			return fastc_option_success_expression(actual_type, value)
		}
	}
	if g.selfhost && expected_type == 'voidptr' && tokens.len > 1 && tokens[0].tok == .mul {
		operand_type := g.infer_expression_type(tokens[1..]) or { '' }
		if operand_type == 'voidptr' {
			// A generic pointer type is erased to `voidptr`; C cannot dereference it
			// without the missing concrete type. At another erased boundary, forward
			// that pointer representation directly.
			return g.render_call_argument_expression(tokens[1..], expected_type)
		}
	}
	if array_literal := g.render_array_literal_argument(tokens, expected_type) {
		return array_literal.source
	}
	if map_literal := g.render_map_literal_argument(tokens, expected_type) {
		return map_literal.source
	}
	if tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name
		&& g.declared_kinds[g.semantic_type_key(expected_type)] == .enum_ {
		return '${expected_type.trim_right('*')}__${tokens[1].lit}'
	}
	if function_value := g.render_function_value_expression(tokens) {
		return function_value
	}
	if method_value := g.render_method_value_expression(tokens) {
		return method_value
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if tokens.len == 1 && tokens[0].tok == .name {
		if local := g.locals[tokens[0].lit] {
			if local.is_reference {
				if g.should_box_variant(expected_type, local.typ) {
					// A `mut value T` parameter is a C `T*`. Interface conversion must
					// preserve that pointer as the boxed object instead of auto-dereferencing
					// it to the value used by ordinary by-value arguments.
					return g.interface_value_expression(expected_type, local.typ, raw)
				}
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
	mut rendered_type := ''
	if special := g.render_special_expression(tokens, raw) {
		rendered = special.source
		rendered_type = special.typ
	} else {
		rendered = g.render_membership_candidate(tokens, expected_type) or { return none }
	}
	rendered = g.render_constant_references(tokens, rendered)
	actual_type := if rendered_type != '' {
		fastc_normalize_inferred_type(rendered_type)
	} else {
		fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { '' })
	}
	if expected_type == 'voidptr' && actual_type !in ['', 'voidptr', 'nil']
		&& !fastc_expression_is_zero(tokens) && !fastc_is_pointer_type(actual_type) {
		box_value := '__v_fastc_generic_argument'
		return '({ ${actual_type} ${box_value} = (${rendered}); v_fastc_interface_box(&${box_value}, sizeof(${actual_type})); })'
	}
	if actual_type == 'voidptr' && !expected_type.ends_with('*')
		&& (expected_type.trim_right('*') in g.struct_fields
		|| expected_type.trim_right('*').starts_with('Array_')
		|| expected_type.trim_right('*').starts_with('Map_')
		|| expected_type.trim_right('*').starts_with('FixedArray_')) {
		return '*((${expected_type} *)(${rendered}))'
	}
	if expected_type == 'string' && actual_type.trim_right('*') == 'IError' {
		return 'builtin__IError_msg(${rendered})'
	}
	if actual_type.ends_with('*') && expected_type == actual_type.trim_right('*')
		&& !fastc_expression_tokens_contain(tokens, .lsbr) {
		// V automatically dereferences an explicit reference when a by-value
		// parameter is expected (`s &string` passed to `log_line(s string)`). A
		// slice of a mutable array is already a by-value array even though the
		// receiver local retains its pointer type during inference.
		return '*(${rendered})'
	}
	if expected_type.ends_with('*') && actual_type == 'voidptr' && rendered.trim_space() == 'NULL' {
		return 'NULL'
	}
	if expected_type.ends_with('*') && fastc_expression_is_zero(tokens) {
		return 'NULL'
	}
	if expected_type.ends_with('*') && actual_type == expected_type.trim_right('*') {
		return '&(${rendered})'
	}
	if g.should_box_variant(expected_type, actual_type) {
		return g.interface_value_expression(expected_type, actual_type, rendered)
	}
	return rendered
}

fn (g &Parser) render_array_literal_argument(tokens []FastcExpressionToken, expected_type string) ?FastcRenderedExpression {
	array_type := expected_type.trim_right('*')
	is_fixed := array_type.starts_with('FixedArray_')
	if (!array_type.starts_with('Array_') && !is_fixed) || tokens.len < 2 || tokens[0].tok != .lsbr {
		return none
	}
	close := fastc_matching_delimiter(tokens, 0, .lsbr, .rsbr) or { return none }
	expected_end := if is_fixed { tokens.len - 2 } else { tokens.len - 1 }
	if close != expected_end || (is_fixed && tokens.last().tok != .not) {
		return none
	}
	mut w := unsafe { &Parser(g) }
	fastc_register_composite_type(array_type, mut w.composite_types)
	element_type := g.array_element_type(array_type) or { return none }
	items := fastc_expression_list_items(tokens, 1, close) or { return none }
	if items.len == 0 {
		return FastcRenderedExpression{
			source: '(${array_type}){0}'
			typ:    array_type
		}
	}
	mut rendered_items := []string{cap: items.len}
	for item in items {
		rendered_items << g.render_call_argument_expression(item, element_type) or { return none }
	}
	normalized_element := fastc_normalize_inferred_type(element_type)
	if is_fixed {
		c_array_type := fastc_array_initializer_c_type(array_type)
		w.fixed_array_types[c_array_type] = array_type
		return FastcRenderedExpression{
			source: '((${c_array_type}){.data={${rendered_items.join(',')}}})'
			typ:    array_type
		}
	}
	return FastcRenderedExpression{
		source: '((${array_type})builtin__new_array_from_c_array(${items.len}, ${items.len}, sizeof(${normalized_element}), (${normalized_element}[]){${rendered_items.join(',')}}))'
		typ:    array_type
	}
}

fn (g &Parser) render_function_value_expression(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens.last().tok != .name {
		return none
	}
	if tokens.len == 1 && tokens[0].lit in g.locals {
		return none
	}
	if tokens.len != 1 && !(tokens.len == 3 && tokens[0].tok == .name && tokens[1].tok == .dot
		&& (tokens[0].lit in g.imports || tokens[0].lit == 'C')) {
		return none
	}
	function_key := g.function_key_for_call(tokens, tokens.len - 1)
	if function_key !in g.functions && function_key !in g.mono_functions {
		return none
	}
	return '&${fastc_c_function_name_for_key(function_key)}'
}

fn (g &Parser) render_method_value_expression(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 || tokens.last().tok != .name || tokens[tokens.len - 2].tok != .dot {
		return none
	}
	receiver_tokens := tokens[..tokens.len - 2]
	receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
	if g.struct_member_type(receiver_type, tokens.last().lit) != '' {
		return none
	}
	method_key, _ := g.resolve_method(receiver_type, tokens.last().lit)
	signature := if method_key in g.functions {
		g.functions[method_key]
	} else if method_key in g.mono_functions {
		g.mono_functions[method_key]
	} else {
		return none
	}
	if signature.parameter_types.len == 0 {
		return none
	}
	return '&${fastc_method_c_name(signature.module_name, signature.parameter_types[0],
		tokens.last().lit)}'
}

fn (g &Parser) render_map_literal_argument(tokens []FastcExpressionToken, expected_type string) ?FastcRenderedExpression {
	map_type := expected_type.trim_right('*')
	key_type, value_type := g.map_key_value_types(map_type) or { return none }
	if tokens.len < 2 || tokens[0].tok != .lcbr || tokens.last().tok != .rcbr {
		return none
	}
	close := fastc_matching_delimiter(tokens, 0, .lcbr, .rcbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	entries := fastc_map_literal_entries(tokens, 1, close) or { return none }
	hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
	mut statements := [
		'map __v_fastc_argument_map = builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn});',
	]
	for entry_index, entry in entries {
		mut colon := -1
		mut parens := 0
		mut brackets := 0
		mut braces := 0
		for i, item in entry {
			match item.tok {
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
				.colon {
					if parens == 0 && brackets == 0 && braces == 0 {
						colon = i
						break
					}
				}
				else {}
			}
		}
		if colon <= 0 || colon + 1 >= entry.len {
			return none
		}
		key := g.render_call_argument_expression(entry[..colon], key_type) or { return none }
		value := g.render_call_argument_expression(entry[colon + 1..], value_type) or {
			return none
		}
		key_name := '__v_fastc_argument_map_key_${entry_index}'
		value_name := '__v_fastc_argument_map_value_${entry_index}'
		statements << '${fastc_runtime_c_type(key_type)} ${key_name} = (${key});'
		statements << '${fastc_runtime_c_type(value_type)} ${value_name} = (${value});'
		statements << 'builtin__map_set(&__v_fastc_argument_map, &${key_name}, &${value_name});'
	}
	return FastcRenderedExpression{
		source: '({ ${statements.join(' ')} __v_fastc_argument_map; })'
		typ:    map_type
	}
}

fn fastc_map_literal_entries(tokens []FastcExpressionToken, start int, end int) ?[][]FastcExpressionToken {
	mut entries := [][]FastcExpressionToken{}
	mut entry_start := start
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for i in start .. end {
		match tokens[i].tok {
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
			.comma, .semicolon {
				if parens == 0 && brackets == 0 && braces == 0 {
					if entry_start < i {
						entries << tokens[entry_start..i]
					}
					entry_start = i + 1
				}
			}
			else {}
		}
	}
	if entry_start < end {
		entries << tokens[entry_start..end]
	}
	return entries
}

// is_boxed_type reports whether a type uses the boxed `{_object, _typ, _methods}`
// representation: interfaces and sum types. A concrete struct used where such a
// type is expected is boxed with its type id (see interface_value_expression).
fn (g &Parser) is_boxed_type(c_type string) bool {
	if c_type.trim_right('*') in g.sum_types {
		return true
	}
	return g.declared_kinds[g.semantic_type_key(c_type)] == .interface_
}

// should_box_variant reports whether a value of `actual_type` used where the
// boxed `expected_type` is expected must be boxed: a concrete struct into an
// interface or sum type, or a primitive scalar into a sum type (interfaces have
// no primitive implementers, so primitives are only boxed for sum types).
fn (g &Parser) should_box_variant(expected_type string, actual_type string) bool {
	if actual_type == '' || fastc_is_pointer_type(expected_type) || !g.is_boxed_type(expected_type) {
		return false
	}
	resolved_actual := g.underlying_alias_type(actual_type)
	if g.declared_kinds[g.semantic_type_key(resolved_actual)] == .struct_ {
		return true
	}
	if expected_type.trim_right('*') in g.sum_types {
		// Primitive or composite (`Array_`/`Map_`) value into a sum type. Interfaces
		// have no primitive/composite implementers, so this is sum-type only.
		normalized := fastc_normalize_inferred_type(actual_type).trim_right('*')
		return fastc_primitive_c_type(normalized) != none || normalized.starts_with('Array_')
			|| normalized.starts_with('Map_')
	}
	return false
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
	key_type, value_type := g.map_key_value_types(map_type) or { return none }
	mut map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
		g.resolved_root_expression_name(base_tokens[0].lit)
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
	if named_start >= 0 && named_start == signature.parameter_types.len - 1
		&& named_start <= call_args.len && (signature.last_parameter_is_params
		|| g.fastc_type_is_declared_struct(signature.parameter_types[named_start])) {
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
		named_initializer := g.render_named_struct_initializer(parameter_type,
			call_args[named_start..]) or { return none }
		rendered_arguments << named_initializer
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
		if named_start == fixed_arguments {
			mut c_arguments := []string{cap: fixed_arguments + 1}
			for argument_index, argument in call_args[..fixed_arguments] {
				c_arguments << g.render_call_argument_expression(argument,
					signature.parameter_types[argument_index]) or { return none }
			}
			named_initializer := g.render_named_struct_initializer(element_type,
				call_args[named_start..]) or { return none }
			c_arguments << '((${variadic_type})builtin__new_array_from_c_array(1, 1, sizeof(${element_type}), (${element_type}[]){${named_initializer}}))'
			return FastcRenderedExpression{
				source: '${fastc_c_function_name_for_key(function_key)}(${c_arguments.join(',')})'
				typ:    signature.return_type
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
		typ:    signature.return_type
	}
}

fn (g &Parser) render_explicit_generic_call_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	for open in 1 .. tokens.len - 1 {
		if tokens[open].tok != .lsbr || tokens[open - 1].tok != .name {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close + 1 >= tokens.len || tokens[close + 1].tok != .lpar {
			continue
		}
		mut normalized := tokens[..open].clone()
		normalized << tokens[close + 1..]
		raw := g.render_raw_expression_tokens(normalized) or { return none }
		if method := g.render_method_call_expression(normalized, raw) {
			return method
		}
		return g.render_missing_call_arguments(normalized)
	}
	return none
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
	element_type := g.array_element_type(left_type) or { return none }
	separator := rendered_expression.index('<<') or { return none }
	left_tokens := tokens[..operator_index]
	mut left_source := rendered_expression[..separator]
	mut right_source := rendered_expression[separator + 2..]
	// The raw streamed value is only valid C for a plain operand. A boxed sum/interface
	// cast (`arr << Primitive(x)`), a top-level index/array-literal (`arr << type_idx['int']`,
	// `arr << [a, b]`), and any call (`arr << s.clone()`, `arr << f(x)`) need the argument
	// renderer, which boxes variants, lowers indexing, and routes method/function calls.
	// Render through it whenever it succeeds, keeping the raw form only as a fallback.
	if rerendered := g.render_call_argument_expression(tokens[operator_index + 1..], element_type) {
		right_source = rerendered
	}
	temporary := '__v_fastc_append_value'
	if left_tokens.len >= 4 && left_tokens.last().tok == .rsbr {
		mut open := -1
		mut bracket_depth := 0
		for i := left_tokens.len - 1; i >= 0; i-- {
			if left_tokens[i].tok == .rsbr {
				bracket_depth++
			} else if left_tokens[i].tok == .lsbr {
				bracket_depth--
				if bracket_depth == 0 {
					open = i
					break
				}
			}
		}
		if open > 0 {
			base_tokens := left_tokens[..open]
			map_type := g.infer_expression_type(base_tokens) or { '' }
			if key_type, value_type := g.map_key_value_types(map_type) {
				if value_type == left_type {
					key_source := g.render_call_argument_expression(left_tokens[open + 1..left_tokens.len - 1],
						key_type) or { return none }
					mut map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
						g.globals[fastc_global_key(g.module_name, base_tokens[0].lit)] or {
							g.resolved_expression_name(base_tokens[0].lit, .unknown)
						}
					} else {
						g.render_member_receiver(base_tokens) or { return none }
					}
					if map_type.ends_with('*') {
						map_source = '*(${map_source})'
					}
					left_source = '({ ${key_type} __v_fastc_append_map_key = (${key_source}); ${value_type} *__v_fastc_append_map_value = (${value_type} *)builtin__map_get_check((map *)&(${map_source}), &__v_fastc_append_map_key); if (__v_fastc_append_map_value == NULL) { ${value_type} __v_fastc_append_map_empty = (${value_type}){0}; builtin__map_set((map *)&(${map_source}), &__v_fastc_append_map_key, &__v_fastc_append_map_empty); __v_fastc_append_map_value = (${value_type} *)builtin__map_get_check((map *)&(${map_source}), &__v_fastc_append_map_key); } __v_fastc_append_map_value; })'
					return FastcRenderedExpression{
						source: '({ ${element_type} ${temporary} = (${right_source}); ${value_type} *__v_fastc_append_map_target = ${left_source}; builtin__array_push((array *)__v_fastc_append_map_target, &${temporary}); 0; })'
						typ:    'void'
					}
				}
			}
		}
	}
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
		// `mod.func()` is a module-qualified call; skip method rendering. But a FIELD named
		// like an imported module (`recv.mod.method()`, e.g. `h.time.elapsed()`) is a real
		// method call — only skip when `mod` is a bare module ref, not preceded by a `.`.
		if tokens[i - 2].tok == .name
			&& (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C')
			&& (i < 3 || tokens[i - 3].tok != .dot) {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		if tokens[i].lit == 'contains'
			&& fastc_normalize_inferred_type(receiver_type).trim_right('*').starts_with('Array_') {
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
			comparison := if g.underlying_alias_type(element_type).trim_right('*') == 'string' {
				'builtin__string_eq(__v_fastc_contains_item, ((${element_type} *)__v_fastc_contains_collection${access}data)[__v_fastc_contains_index])'
			} else {
				'(__v_fastc_contains_item == ((${element_type} *)__v_fastc_contains_collection${access}data)[__v_fastc_contains_index])'
			}
			call_source := '({ ${element_type} __v_fastc_contains_item = (${argument}); __typeof__((${receiver.source})) __v_fastc_contains_collection = (${receiver.source}); bool __v_fastc_contains_found = false; for (int __v_fastc_contains_index = 0; __v_fastc_contains_index < __v_fastc_contains_collection${access}len; __v_fastc_contains_index++) { if (${comparison}) { __v_fastc_contains_found = true; break; } } __v_fastc_contains_found; })'
			if receiver_start == 0 && call_end == tokens.len - 1 {
				return FastcRenderedExpression{
					source: call_source
					typ:    'bool'
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
						typ:    'string'
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
						arguments << g.render_call_argument_expression(argument,
							field.fn_parameter_types[argument_index]) or { continue }
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
							typ:    return_type
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
						rendered = rendered.replace(marker,
							'(${receiver.source}${separator}${tokens[i].lit})(')
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
		method_c_name := fastc_method_c_name(signature.module_name, expected_receiver,
			tokens[i].lit)
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
			if named_start >= 0 && named_start + 1 == signature.parameter_types.len - 1
				&& (signature.last_parameter_is_params
				|| g.fastc_type_is_declared_struct(signature.parameter_types.last())) {
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
				named_initializer := g.render_named_struct_initializer(named_parameter_type,
					call_args[named_start..]) or { continue }
				direct_arguments << named_initializer
			} else {
				for argument_index, argument in call_args {
					expected_index := argument_index + 1
					expected_type := if signature.is_variadic
						&& expected_index >= signature.parameter_types.len - 1 {
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
			variadic_arguments := direct_arguments[fixed_arguments..].clone()
			packed := if variadic_arguments.len == 0 {
				'(${variadic_type}){0}'
			} else {
				'((${variadic_type})builtin__new_array_from_c_array(${variadic_arguments.len}, ${variadic_arguments.len}, sizeof(${element_type}), (${element_type}[]){${variadic_arguments.join(',')}}))'
			}
			direct_arguments = direct_arguments[..fixed_arguments].clone()
			direct_arguments << packed
		}
		if signature.last_parameter_is_params
			&& direct_arguments.len + 1 == signature.parameter_types.len - 1 {
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
			is_pointer_result_method := method_key.starts_with('array.')
				&& tokens[i].lit in ['first', 'last', 'pop', 'pop_left']
			if !is_pointer_result_method && !has_arguments && direct_arguments.len == 0
				&& rendered.contains(needle) {
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

// render_option_propagation lowers a `!`/`?`-propagated expression (`inner_tokens` is
// the operand WITHOUT the trailing `!`): evaluate the option, propagate its error state
// (return in a result fn, panic in `main`), else yield the unwrapped value. Used both
// as a standalone value and as the receiver of `f()!.m()`.
fn (g &Parser) render_option_propagation(inner_tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if inner_tokens.len == 0 {
		return none
	}
	inner_raw := g.render_raw_expression_tokens(inner_tokens) or { return none }
	mut inner_source := inner_raw
	if explicit_generic := g.render_explicit_generic_call_expression(inner_tokens) {
		inner_source = explicit_generic.source
	} else if static_expression := g.render_static_call_expression(inner_tokens, inner_raw) {
		inner_source = static_expression.source
	} else if method_expression := g.render_method_call_expression(inner_tokens, inner_raw) {
		inner_source = method_expression.source
	} else if map_lookup := g.render_map_lookup_option_expression(inner_tokens) {
		inner_source = map_lookup.source
	} else if array_lookup := g.render_array_lookup_option_expression(inner_tokens) {
		inner_source = array_lookup.source
	} else if array_expression := g.render_array_access_expression(inner_tokens) {
		inner_source = array_expression.source
	} else if defaulted_call := g.render_missing_call_arguments(inner_tokens) {
		inner_source = defaulted_call.source
	}
	if pointer_members := g.render_pointer_member_access_expression(inner_tokens, inner_source) {
		inner_source = pointer_members.source
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

fn (g &Parser) render_nested_option_propagation(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut changed := false
	for i := tokens.len - 1; i >= 0; i-- {
		item := tokens[i]
		if item.tok != .not || i == 0 || tokens[i - 1].tok !in [.name, .rpar] {
			continue
		}
		start := fastc_method_receiver_start(tokens, i)
		if start >= i {
			continue
		}
		inner_tokens := tokens[start..i]
		propagation := g.render_option_propagation(inner_tokens) or { continue }
		raw_inner := g.render_raw_expression_tokens(inner_tokens) or { continue }
		needle := '${raw_inner}!'
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, propagation.source)
			changed = true
		}
	}
	if !changed {
		return none
	}
	if logical := g.render_logical_expression(tokens) {
		return logical
	}
	if struct_literal := g.render_struct_literal_expression(tokens) {
		rendered = struct_literal.source
	}
	if methods := g.render_method_call_expression(tokens, rendered) {
		rendered = methods.source
	}
	if array_access := g.render_array_access_expression(tokens) {
		rendered = array_access.source
	} else if nested_array := g.render_nested_array_access_expression(tokens, rendered) {
		rendered = nested_array.source
	}
	return FastcRenderedExpression{
		source: rendered
		typ:    g.infer_expression_type(tokens) or { '' }
	}
}

fn (g &Parser) render_method_receiver_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len > 1 && tokens.last().tok == .not && !(tokens[0].tok == .lsbr
		&& tokens[tokens.len - 2].tok == .rsbr) {
		// A `!`-propagated receiver (`f()!.m()`): unwrap the result before the call.
		if propagation := g.render_option_propagation(tokens[..tokens.len - 1]) {
			return propagation
		}
	}
	receiver_type := g.infer_expression_type(tokens) or { return none }
	if tokens.len == 1 && tokens[0].source != '' {
		// A pre-rendered synthetic receiver (e.g. an `or {}`-unwrap `({ ... })` used as
		// `expr or { ... }.method()`) carries its full rendered form as its `source`;
		// use it directly so the method call binds to it.
		return FastcRenderedExpression{
			source: tokens[0].source
			typ:    receiver_type
		}
	}
	if source := g.render_map_expression(tokens) {
		return source
	}
	if array_literal := g.render_array_literal_argument(tokens, receiver_type) {
		return array_literal
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access
	}
	if cast_expression := g.render_cast_expression(tokens) {
		return cast_expression
	}
	if struct_literal := g.render_struct_literal_expression(tokens) {
		return struct_literal
	}
	if overloaded := g.render_overloaded_binary_expression(tokens) {
		return overloaded
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
	mut member_path := tokens[0].lit
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
		member_path += '.' + tokens[i + 1].lit
		if smartcast := g.member_smartcasts[member_path] {
			source = smartcast.source
			current_type = smartcast.typ
		}
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
