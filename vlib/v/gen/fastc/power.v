module fastc

import v.flat

// C has no `**` operator. finish_read_expression replaces every `a ** b` of an
// expression with one typed atom before the expression is rendered: a call of the
// receiver's `**` overload, or of a FastC helper with the C backend's semantics
// (`__v_pow_i64`, `__v_pow_u64` and `pow` in cgen's power_expr_string).

fn fastc_expression_tokens_contain_power(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok == .power {
			return true
		}
	}
	return false
}

// lower_power_expressions returns `tokens` with each `**` and its operands replaced
// by an atom, innermost first. `**` binds tighter than any other binary operator
// and groups to the right (`a ** b ** c` is `a ** (b ** c)`), while a prefix minus
// applies to the result (`-a ** b` is `-(a ** b)`), as in the C backend.
fn (g &Parser) lower_power_expressions(tokens []FastcExpressionToken) ?[]FastcExpressionToken {
	mut result := []FastcExpressionToken{cap: tokens.len + 2}
	for i, item in tokens {
		if item.tok == .power && fastc_token_is_prefix_operator(tokens, i) {
			// `**p` dereferences twice.
			result << FastcExpressionToken{
				tok: .mul
				lit: '*'
			}
			result << FastcExpressionToken{
				tok: .mul
				lit: '*'
			}
			continue
		}
		result << item
	}
	for {
		mut lowered := false
		for i := result.len - 1; i > 0; i-- {
			if result[i].tok != .power {
				continue
			}
			start := fastc_power_left_operand_start(result, i)
			end := fastc_power_right_operand_end(result, i)
			if start < 0 || start >= i || end <= i + 1 {
				return none
			}
			if fastc_expression_tokens_contain_power(result[start..i])
				|| fastc_expression_tokens_contain_power(result[i + 1..end]) {
				// An operand still holds a `**`; lower that one first.
				continue
			}
			mut next := result[..start].clone()
			if value := fastc_folded_integer_power(result[start..i], result[i + 1..end]) {
				// An integer literal stays a constant, so it can size a fixed array.
				next << FastcExpressionToken{
					tok: .number
					lit: value.str()
				}
			} else {
				rendered := g.render_power_expression(result[start..i], result[i + 1..end])?
				next << FastcExpressionToken{
					tok:    .name
					source: rendered.source
					typ:    rendered.typ
				}
			}
			next << result[end..]
			result = next.clone()
			lowered = true
			break
		}
		if !lowered {
			break
		}
	}
	if fastc_expression_tokens_contain_power(result) {
		return none
	}
	return result
}

// fastc_power_left_operand_start returns the first token of the left operand of
// the `**` at `operator`: a primary expression with its member, call, index and
// propagation suffixes, and its prefix operators other than `-` and `+`.
fn fastc_power_left_operand_start(tokens []FastcExpressionToken, operator int) int {
	mut i := operator - 1
	mut start := -1
	for i >= 0 {
		tok := tokens[i].tok
		if tok in [.rpar, .rsbr, .rcbr] {
			open := fastc_power_matching_open(tokens, i)
			if open < 0 {
				return -1
			}
			start = open
			// A call, index or initializer continues the operand to the left.
			if open > 0 && tokens[open - 1].tok in [.name, .rpar, .rsbr, .not, .question] {
				i = open - 1
				continue
			}
			break
		}
		if tok in [.not, .question] && i > 0 && fastc_token_ends_operand(tokens[i - 1].tok) {
			i--
			continue
		}
		if tok == .name {
			start = i
			if i >= 2 && tokens[i - 1].tok == .dot {
				i -= 2
				continue
			}
			break
		}
		if tok in [.number, .string, .char, .key_true, .key_false, .key_nil, .key_none] {
			start = i
			break
		}
		return -1
	}
	if start < 0 {
		return -1
	}
	for start > 0 && tokens[start - 1].tok in [.mul, .amp, .bit_not, .not]
		&& fastc_token_is_prefix_operator(tokens, start - 1) {
		start--
	}
	return start
}

// fastc_power_right_operand_end returns the index after the right operand of the
// `**` at `operator`, which takes prefix operators and continues through a
// following `**`.
fn fastc_power_right_operand_end(tokens []FastcExpressionToken, operator int) int {
	mut i := operator + 1
	for i < tokens.len && tokens[i].tok in [.minus, .plus, .bit_not, .not, .mul, .amp] {
		i++
	}
	if i >= tokens.len {
		return -1
	}
	match tokens[i].tok {
		.lpar, .lsbr {
			close := fastc_power_matching_close(tokens, i)
			if close < 0 {
				return -1
			}
			i = close + 1
		}
		.name, .number, .string, .char, .key_true, .key_false, .key_nil, .key_none {
			i++
		}
		else {
			return -1
		}
	}
	for i < tokens.len {
		tok := tokens[i].tok
		if tok == .dot && i + 1 < tokens.len && tokens[i + 1].tok == .name {
			i += 2
			continue
		}
		if tok in [.lpar, .lsbr] || (tok == .lcbr && tokens[i - 1].tok == .name) {
			close := fastc_power_matching_close(tokens, i)
			if close < 0 {
				return -1
			}
			i = close + 1
			continue
		}
		if tok in [.not, .question] {
			i++
			continue
		}
		break
	}
	if i < tokens.len && tokens[i].tok == .power {
		return fastc_power_right_operand_end(tokens, i)
	}
	return i
}

fn fastc_power_matching_open(tokens []FastcExpressionToken, close int) int {
	mut depth := 0
	for i := close; i >= 0; i-- {
		if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			depth++
		} else if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn fastc_power_matching_close(tokens []FastcExpressionToken, open int) int {
	mut depth := 0
	for i in open .. tokens.len {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

// fastc_folded_integer_power returns the value of `left ** right` when both are
// integer literals (the exponent possibly negative) and the result is a
// non-negative i64, computed as v_fastc_pow_i64 computes it.
fn fastc_folded_integer_power(left []FastcExpressionToken, right []FastcExpressionToken) ?i64 {
	if left.len != 1 || !fastc_is_integer_literal_token(left[0]) {
		return none
	}
	negative_exponent := right.len == 2 && right[0].tok == .minus
	if !(right.len == 1 || negative_exponent) || !fastc_is_integer_literal_token(right.last()) {
		return none
	}
	base := left[0].lit.i64()
	exponent := right.last().lit.i64()
	if base < 0 || exponent < 0 {
		return none
	}
	if negative_exponent && exponent > 0 {
		if base == 0 {
			// -1, which is not a single literal token.
			return none
		}
		return if base == 1 { i64(1) } else { i64(0) }
	}
	mut value := i64(1)
	for _ in 0 .. exponent {
		if base != 0 && value > max_i64 / base {
			return none
		}
		value *= base
		if value == 0 || value == 1 {
			break
		}
	}
	return value
}

fn fastc_is_integer_literal_token(item FastcExpressionToken) bool {
	if item.tok != .number || item.lit.len == 0 {
		return false
	}
	lit := item.lit.to_lower()
	if lit.starts_with('0x') || lit.starts_with('0o') || lit.starts_with('0b') {
		return true
	}
	return !lit.contains('.') && !lit.contains('e')
}

// render_power_expression renders `left ** right`.
fn (g &Parser) render_power_expression(left []FastcExpressionToken, right []FastcExpressionToken) ?FastcRenderedExpression {
	left_type := g.infer_expression_type(left) or { return none }
	method_key := g.method_function_key(left_type, '**')
	if signature := g.functions[method_key] {
		if signature.parameter_types.ids.len < 2 || signature.is_disabled {
			return none
		}
		left_source := g.render_call_argument_expression(left, signature.parameter_types.ids[0])?
		right_source := g.render_call_argument_expression(right, signature.parameter_types.ids[1])?
		method_c_name := g.c_function_name_or(method_key, fastc_method_c_name(signature.module_name,
			signature.parameter_types.at(0), '**'))
		return FastcRenderedExpression{
			source: '${method_c_name}(${left_source},${right_source})'
			typ:    signature.return_type
		}
	}
	right_type := g.infer_expression_type(right) or { return none }
	left_value_type := g.narrowed_numeric_operand_type(left, left_type)
	right_value_type := g.narrowed_numeric_operand_type(right, right_type)
	result_type := fastc_power_result_type(g.underlying_alias_type(left_value_type),
		g.underlying_alias_type(right_value_type))
	if result_type == 0 {
		return none
	}
	left_source := g.render_call_argument_expression(left, left_value_type)?
	right_source := g.render_call_argument_expression(right, right_value_type)?
	return FastcRenderedExpression{
		source: g.render_power_call(left_source, right_source, result_type)
		typ:    result_type
	}
}

// render_power_assignment renders `target **= value` for a numeric `target`, which
// is evaluated once.
fn (g &Parser) render_power_assignment(target string, value string, target_type u32) ?string {
	value_type := fastc_normalize_inferred_type(g.underlying_alias_type(target_type))
	if !fastc_is_numeric_expression_type(value_type) {
		return none
	}
	c_type := fastc_output_c_type(flat.source_type_name(target_type))
	power := g.render_power_call('*__vf_power_target', value, value_type)
	return '({ ${c_type} *__vf_power_target = &(${target}); *__vf_power_target = ${power}; })'
}

// render_power_call spells `left ** right` with a result of `result_type`, as the C
// backend's power_expr_string does.
fn (g &Parser) render_power_call(left string, right string, result_type u32) string {
	c_type := fastc_output_c_type(flat.source_type_name(result_type))
	if result_type in [flat.source_type_f32, flat.source_type_f64] {
		return '((${c_type})${g.power_helper('f64')}((f64)(${left}), (f64)(${right})))'
	}
	if fastc_is_unsigned_integer_type(result_type) {
		return '((${c_type})${g.power_helper('u64')}((u64)(${left}), (i64)(${right})))'
	}
	return '((${c_type})${g.power_helper('i64')}((i64)(${left}), (i64)(${right})))'
}

// fastc_power_result_type returns the type of `left ** right`, which V types like
// the other arithmetic operators (the checker's resolve_type for an infix): a
// float operand makes a float, an integer literal takes the other operand's type,
// and two integer types meet as in infix_integer_promotion_type.
fn fastc_power_result_type(left u32, right u32) u32 {
	if !fastc_is_numeric_expression_type(left) || !fastc_is_numeric_expression_type(right) {
		return 0
	}
	left_float := left in [flat.source_type_f32, flat.source_type_f64, fastc_type_float_literal]
	right_float := right in [flat.source_type_f32, flat.source_type_f64, fastc_type_float_literal]
	if left_float || right_float {
		if (left == flat.source_type_f32 && right != flat.source_type_f64)
			|| (right == flat.source_type_f32 && left != flat.source_type_f64) {
			return flat.source_type_f32
		}
		return flat.source_type_f64
	}
	if fastc_is_integer_literal_expression_type(left) {
		return fastc_normalize_inferred_type(right)
	}
	if fastc_is_integer_literal_expression_type(right) || left == right {
		return left
	}
	if fastc_integer_widens_to(left, right) {
		return right
	}
	return left
}

// fastc_integer_widens_to mirrors the checker's call_arg_implicit_numeric_widening
// for two integer types.
fn fastc_integer_widens_to(actual u32, expected u32) bool {
	actual_index := fastc_integer_promotion_index(actual)
	expected_index := fastc_integer_promotion_index(expected)
	if actual_index < 0 || expected_index < 0 {
		return false
	}
	high := if actual_index > expected_index { actual_index } else { expected_index }
	low := if actual_index > expected_index { expected_index } else { actual_index }
	promoted := if low >= 11 {
		high
	} else if low >= 5 && (high <= 10 || high == 22) {
		if low == 9 { low } else { high }
	} else if (high == 11 && low > 5) || (high == 12 && low > 6) || (high == 13 && low > 8)
		|| (high == 14 && low >= 9) || (high == 15 && low >= 10) {
		low
	} else {
		-1
	}
	return promoted == expected_index
}

fn fastc_integer_promotion_index(typ u32) int {
	return match flat.source_type_name(typ) {
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'uint', 'usize' { 15 }
		'rune' { 22 }
		else { -1 }
	}
}

// power_helper registers the C helper of a power with a `kind` result (`i64`,
// `u64` or `f64`) and returns its name. A negative integer exponent gives what
// the C backend's helpers give; the integer product wraps like V arithmetic.
fn (g &Parser) power_helper(kind string) string {
	name := 'v_fastc_pow_${kind}'
	if name in g.spawn_helpers {
		return name
	}
	body := match kind {
		'f64' {
			'f64 ${name}(f64 base, f64 exponent) {\n\textern double pow(double, double);\n\treturn pow(base, exponent);\n}'
		}
		'u64' {
			'u64 ${name}(u64 base, i64 exponent) {\n\tif (exponent < 0) {\n\t\tif (base == 0) return (u64)-1;\n\t\treturn base == 1 ? 1 : 0;\n\t}\n\tu64 value = 1;\n\tfor (; exponent > 0; exponent >>= 1) {\n\t\tif ((exponent & 1) != 0) value *= base;\n\t\tbase *= base;\n\t}\n\treturn value;\n}'
		}
		else {
			'i64 ${name}(i64 base, i64 exponent) {\n\tif (exponent < 0) {\n\t\tif (base == 0) return -1;\n\t\tif (base != 1 && base != -1) return 0;\n\t\treturn (exponent & 1) != 0 ? base : 1;\n\t}\n\tu64 value = 1;\n\tu64 power = (u64)base;\n\tfor (; exponent > 0; exponent >>= 1) {\n\t\tif ((exponent & 1) != 0) value *= power;\n\t\tpower *= power;\n\t}\n\treturn (i64)value;\n}'
		}
	}
	mut w := unsafe { &Parser(g) }
	w.spawn_helpers[name] = body
	return name
}
