module fastc

import v.flat
import v.token

const fastc_type_legacy_option = flat.source_type_id('_option')
const fastc_type_legacy_result = flat.source_type_id('_result')
const fastc_type_charptr = flat.source_type_id('charptr')
const fastc_type_char_pointer = flat.source_type_id('char*')
const fastc_type_u8_pointer = flat.source_type_id('u8*')
const fastc_type_voidptr_pointer = flat.source_type_id('voidptr*')
const fastc_type_i128 = flat.source_type_id('i128')
const fastc_type_u128 = flat.source_type_id('u128')
const fastc_type_uint = flat.source_type_id('uint')
const fastc_type_option = flat.source_type_id('Option')
const fastc_type_result = flat.source_type_id('__v_result')
const fastc_type_ierror = flat.source_type_id('IError')
const fastc_type_multi_return = flat.source_type_id('MultiReturn')
const fastc_type_chan = flat.source_type_id('chan')
const fastc_type_integer_literal = flat.source_type_id('integer literal')
const fastc_type_negative_integer_literal = flat.source_type_id('negative integer literal')
const fastc_type_float_literal = flat.source_type_id('float literal')
const fastc_type_unsigned_int = flat.source_type_id('unsigned int')
const fastc_type_byteptr = flat.source_type_id('byteptr')
const fastc_type_array = flat.source_type_id('array')
const fastc_type_map = flat.source_type_id('map')
const fastc_type_time_time = flat.source_type_id('time__Time')

fn fastc_matching_rpar(tokens []FastcExpressionToken, open int) ?int {
	return fastc_matching_rpar_before(tokens, open, tokens.len)
}

// fastc_bare_as_cast_index returns the index of a top-level `as` in tokens[start..end]
// when the tokens are a bare `X as T` cast (no surrounding binary operator, so
// `a == b as T` — a comparison — is excluded). Returns none otherwise.
fn fastc_bare_as_cast_index(tokens []FastcExpressionToken, start int, end int) ?int {
	mut contains_as := false
	for i := start; i < end; i++ {
		if tokens[i].tok == .key_as {
			contains_as = true
			break
		}
	}
	if !contains_as {
		return none
	}
	mut depth := 0
	mut as_index := -1
	for i := start; i < end; i++ {
		match tokens[i].tok {
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
			.eq, .ne, .lt, .gt, .le, .ge, .and, .logical_or, .plus, .minus, .mul, .div, .mod,
			.pipe, .amp, .xor, .left_shift, .right_shift, .right_shift_unsigned {
				if depth == 0 && as_index < 0 {
					// A binary operator before any `as` means the whole expression is not a
					// cast (e.g. `a == b as T`).
					return none
				}
			}
			else {}
		}
	}
	if as_index < 0 {
		return none
	}
	return as_index
}

fn fastc_matching_rpar_before(tokens []FastcExpressionToken, open int, end int) ?int {
	mut depth := 0
	for i in open .. end {
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
	return fastc_method_receiver_start_after(tokens, dot, 0)
}

fn fastc_method_receiver_start_after(tokens []FastcExpressionToken, dot int, lower_bound int) int {
	if dot <= lower_bound || dot > tokens.len {
		return lower_bound
	}
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	mut start := dot - 1
	for start >= lower_bound {
		tok := tokens[start].tok
		if tok == .rpar {
			parens++
		} else if tok == .rsbr {
			brackets++
		} else if tok == .rcbr {
			braces++
		} else if tok == .lpar {
			if parens == 0 && brackets == 0 && braces == 0 {
				return start + 1
			}
			parens--
		} else if tok == .lsbr {
			if brackets == 0 && parens == 0 && braces == 0 {
				return start + 1
			}
			brackets--
		} else if tok == .lcbr {
			// A balanced struct-literal `{ ... }` is part of the receiver (`T{...}.m()`);
			// only an UNMATCHED `{` (e.g. an enclosing block) stops the scan.
			if braces == 0 && parens == 0 && brackets == 0 {
				return start + 1
			}
			braces--
		} else if parens == 0 && brackets == 0 && braces == 0 && tok in [.amp, .and, .mul] && start + 2 < dot && tokens[start + 1].tok == .name && tokens[start + 2].tok == .lpar && fastc_token_is_prefix_operator(tokens, start) {
			return start
		} else if parens == 0 && brackets == 0 && braces == 0 && tok == .not && start > 0 && tokens[start - 1].tok in [
			.rpar,
			.rsbr,
			.name,
			.number,
			.string,
		] {
			// A postfix `!` (result propagation) follows a value and is part of the
			// receiver (`f()!.m()`); keep scanning into the underlying call. A prefix `!`
			// (negation) is not preceded by a value and falls to the stop below.
		} else if parens == 0 && brackets == 0 && braces == 0 && (tok.is_assignment() || tok in [
			.comma,
			.semicolon,
			.colon,
			.ellipsis,
			.plus,
			.minus,
			.mul,
			.div,
			.mod,
			.amp,
			.pipe,
			.xor,
			.left_shift,
			.right_shift,
			.right_shift_unsigned,
			.eq,
			.ne,
			.gt,
			.lt,
			.ge,
			.le,
			.and,
			.logical_or,
			.not,
			.bit_not,
		]) {
			return start + 1
		}
		start--
	}
	return lower_bound
}

fn (g &Parser) map_receiver_type_before_dot(tokens []FastcExpressionToken, dot int) ?u32 {
	if dot <= 0 || dot >= tokens.len || tokens[dot].tok != .dot {
		return none
	}
	receiver_start := fastc_method_receiver_start(tokens, dot)
	receiver_type := g.infer_expression_type(tokens[receiver_start..dot]) or { return none }
	if !g.is_map_type(receiver_type) {
		return none
	}
	return receiver_type
}

fn (g &Parser) map_length_receiver_type(tokens []FastcExpressionToken, dot int) ?u32 {
	if dot + 1 >= tokens.len || tokens[dot + 1].tok != .name || tokens[dot + 1].lit != 'len' {
		return none
	}
	return g.map_receiver_type_before_dot(tokens, dot)
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
	// A final comma terminates the last argument; interior empty arguments
	// were rejected while splitting above.
	if start < close {
		call_args << tokens[start..close]
	}
	return call_args
}

fn fastc_call_has_one_argument(tokens []FastcExpressionToken, open int) bool {
	close := fastc_matching_rpar(tokens, open) or { return false }
	arguments := fastc_call_arguments(tokens, open, close) or { return false }
	return arguments.len == 1
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

fn (g &Parser) infer_boolean_binary_expression_type(tokens []FastcExpressionToken, start int, end int, operator_index int) !u32 {
	operator := tokens[operator_index].tok
	mut left_type := fastc_normalize_inferred_type(g.infer_expression_type_range(tokens, start, operator_index)!)
	if operator in [.key_is, .not_is] {
		right_type := (g.type_from_expression_tokens(tokens[operator_index + 1..end]) or {
			return g.unsupported('type test `${operator.str()}` with an undeclared target type')
		})
		if g.semantic_type_key(right_type) !in g.declared_types && !flat.source_type_query(right_type).has(.c_array_prefix) && !flat.source_type_query(right_type).has(.c_map_prefix) && fastc_primitive_c_type_id(right_type) == none {
			// Composite variants (`x is []string` / `x is map[…]`) and primitive variants
			// (`x is u64`) are not declared types but do get generated `__v_typeid_` tags,
			// so allow them.
			return g.unsupported('type test `${operator.str()}` with undeclared type `${flat.source_type_name(right_type)}`')
		}
		return flat.source_type_bool
	}
	mut right_type := fastc_normalize_inferred_type(g.infer_expression_type_range(tokens, operator_index + 1, end)!)
	if operator in [.and, .logical_or] {
		return flat.source_type_bool
	}
	if operator in [.key_in, .not_in] {
		right_type = g.map_type_through_alias(right_type)
		array_end := if operator_index + 1 < end && tokens[end - 1].tok == .not {
			end - 1
		} else {
			end
		}
		if array_end - operator_index - 1 >= 2 && tokens[operator_index + 1].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
			return flat.source_type_bool
		}
		if flat.source_type_query(right_type).has(.c_map_prefix) {
			_, _ := g.map_key_value_types(right_type) or {
				return g.unsupported('membership `${operator.str()}` with unverifiable map type `${flat.source_type_name(right_type)}`')
			}
		} else if fastc_trim_pointer_type(right_type) == flat.source_type_string {
		} else {
			_ := (g.array_element_type(right_type) or {
				return g.unsupported('membership `${operator.str()}` in non-collection type `${flat.source_type_name(right_type)}`')
			})
		}
		return flat.source_type_bool
	}
	if operator in [.eq, .ne] && ((fastc_is_pointer_type(left_type) && fastc_expression_range_is_zero(tokens, operator_index + 1, end)) || (fastc_is_pointer_type(right_type) && fastc_expression_range_is_zero(tokens, start, operator_index))) {
		return flat.source_type_bool
	}
	if operator in [.eq, .ne] {
		if g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_type == u32(0) && fastc_expression_range_is_enum_shorthand(tokens, operator_index + 1, end) {
			right_type = left_type
		} else if g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_type == u32(0) && fastc_expression_range_is_enum_shorthand(tokens, start, operator_index) {
			left_type = right_type
		}
		if !fastc_is_pointer_type(left_type) && !fastc_is_pointer_type(right_type) {
			left_layout := fastc_trim_pointer_type(g.underlying_alias_type(left_type))
			right_layout := fastc_trim_pointer_type(g.underlying_alias_type(right_type))
			left_key := g.semantic_type_key(left_layout)
			if left_layout == right_layout && left_layout !in [fastc_type_option,
				fastc_type_legacy_option, fastc_type_legacy_result] && left_key in g.declared_kinds && g.declared_kinds[left_key] == .struct_ && !g.struct_equality_is_supported(left_type, []u32{}) {
				return g.unsupported('struct equality for `${flat.source_type_name(left_type)}` with unsupported fields')
			}
		}
	}
	if g.selfhost && operator in [.eq, .ne] && ((fastc_is_pointer_type(left_type) && right_type == u32(0)) || (fastc_is_pointer_type(right_type) && left_type == u32(0))) {
		return flat.source_type_bool
	}
	if g.selfhost && ((left_type == u32(0) && fastc_expression_range_is_c_qualified_name(tokens, start, operator_index)) || (right_type == u32(0) && fastc_expression_range_is_c_qualified_name(tokens, operator_index + 1, end))) {
		return flat.source_type_bool
	}
	return flat.source_type_bool
}

fn fastc_expression_range_is_zero(tokens []FastcExpressionToken, start int, end int) bool {
	return end - start == 1 && tokens[start].tok == .number && tokens[start].lit.replace('_', '').trim_left('0') == ''
}

fn fastc_expression_range_is_c_qualified_name(tokens []FastcExpressionToken, start int, end int) bool {
	return end - start == 3 && tokens[start].tok == .name && tokens[start].lit == 'C' && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name
}

fn fastc_expression_range_is_enum_shorthand(tokens []FastcExpressionToken, start int, end int) bool {
	return end - start == 2 && tokens[start].tok == .dot && tokens[start + 1].tok == .name
}

// fastc_typeof_generic_field_type returns the type of `typeof[T]().idx` (int) or
// `typeof[T]().name` (string), or none if the tokens are not that shape.
fn fastc_typeof_generic_field_type(tokens []FastcExpressionToken, start int, end int) ?u32 {
	if end - start < 8 || tokens[start].lit != 'typeof' || tokens[start + 1].tok != .lsbr {
		return none
	}
	close_bracket := fastc_matching_delimiter_before(tokens, start + 1, end, .lsbr, .rsbr) or { return none }
	if close_bracket + 4 != end - 1 || tokens[close_bracket + 1].tok != .lpar || tokens[close_bracket + 2].tok != .rpar || tokens[close_bracket + 3].tok != .dot || tokens[end - 1].tok != .name {
		return none
	}
	return match tokens[end - 1].lit {
		'idx' { flat.source_type_int }
		'name' { flat.source_type_string }
		else { none }
	}
}

// nonlocal_name_type infers the type of a bare name that is not a local from
// the constant and global tables. File generation parsers see fixed tables, so
// the answer is memoized per name there; declaration initializer parsers
// extend those tables while parsing, so they always recompute.
fn (g &Parser) nonlocal_name_type(name string) u32 {
	if !g.declaration_initializer_mode {
		if cached := g.nonlocal_name_type_memo.type_id(name) {
			return cached
		}
	}
	typ := if constant_type := g.constant_types.type_id(fastc_constant_key(g.module_name, name)) {
		constant_type
	} else if constant_type := g.constant_types.type_id(fastc_constant_key('builtin', name)) {
		constant_type
	} else if fastc_constant_key(g.module_name, name) in g.constants {
		fastc_type_integer_literal
	} else if fastc_constant_key('builtin', name) in g.constants {
		fastc_type_integer_literal
	} else if global_type := g.global_types.type_id(fastc_global_key(g.module_name, name)) {
		global_type
	} else if global_type := g.global_types.type_id(fastc_global_key('builtin', name)) {
		global_type
	} else if global_type := g.resolve_cross_module_global_type(name) {
		global_type
	} else if g.unqualified_function_key(name) in g.functions {
		// A function name used as a value is already an address. Calls infer their
		// return type separately; boxing this value as an integer breaks callbacks.
		flat.source_type_voidptr
	} else if g.selfhost {
		flat.source_type_int
	} else {
		u32(0)
	}
	if !g.declaration_initializer_mode {
		mut w := unsafe { &Parser(g) }
		w.nonlocal_name_type_memo.set_id(name, typ)
	}
	return typ
}

fn (g &Parser) infer_expression_type(tokens []FastcExpressionToken) !u32 {
	return g.infer_expression_type_range(tokens, 0, tokens.len)
}

// infer_expression_type_range memoizes the inferred type of a multi-token
// subrange for the duration of the current top-level expression: the
// lowerings ask about the same operands and receivers repeatedly.
fn (g &Parser) infer_expression_type_range(tokens []FastcExpressionToken, expression_start int, expression_end int) !u32 {
	if expression_end - expression_start < 2 {
		return g.infer_expression_type_range_impl(tokens, expression_start, expression_end)!
	}
	// Flow-sensitive member smartcasts are installed while an `&&` expression is rendered.
	// A type cached before the left operand narrows its subject is stale for the operands to
	// its right, so infer those ranges directly while an active smartcast affects them.
	if g.member_smartcasts.len > 0
		&& g.expression_uses_member_smartcast(tokens[expression_start..expression_end]) {
		return g.infer_expression_type_range_impl(tokens, expression_start, expression_end)!
	}
	memo_key := fastc_comparison_memo_range_key(tokens, expression_start, expression_end, 2)
	if memo_key != 0 {
		if cached := g.type_memo[memo_key] {
			return cached
		}
	}
	inferred := g.infer_expression_type_range_impl(tokens, expression_start, expression_end)!
	if memo_key != 0 {
		mut w := unsafe { &Parser(g) }
		w.type_memo[memo_key] = inferred
	}
	return inferred
}

// infer_local_direct_member_type only shortcuts a source-local direct lookup.
// The full inference path retains all ambiguous or flow-sensitive expressions.
fn (g &Parser) infer_local_direct_member_type(tokens []FastcExpressionToken, start int, end int) u32 {
	if end - start != 3 || tokens[start].tok != .name || tokens[start + 1].tok != .dot
		|| tokens[start + 2].tok != .name || !g.selfhost || g.translated
		|| g.member_smartcasts.len > 0 || g.in_mono_drain || g.in_generic_placeholder {
		return u32(0)
	}
	for i in start .. end {
		item := tokens[i]
		if item.has_source() || item.is_mut_argument || item.is_statement
			|| item.is_capturing_closure || item.is_storage || item.chan_element_type != 0
			|| item.option_value_type != 0 {
			return u32(0)
		}
	}
	root := tokens[start]
	member := tokens[start + 2]
	if root.lit == 'C' || root.lit.starts_with('@') || member.lit.starts_with('@')
		|| root.lit in g.imports || tokens[start + 1].typ != 0 || member.typ != 0 {
		return u32(0)
	}
	local := g.locals[root.lit] or { return u32(0) }
	if local.typ == 0 || local.option_value_type != 0 || local.smartcast_origin_type != 0
		|| local.smartcast_origin_source != '' || (root.typ != 0 && root.typ != local.typ) {
		return u32(0)
	}
	// A declared enum (including an alias) wins before the local-field branch.
	if g.resolve_declared_type_key(root.lit) != none { return u32(0) }
	return g.struct_direct_member_type(local.typ, member.lit)
}

fn (g &Parser) infer_expression_type_range_impl(tokens []FastcExpressionToken, expression_start int, expression_end int) !u32 {
	if expression_start >= expression_end {
		return u32(0)
	}
	mut start := expression_start
	mut end := expression_end
	for end - start >= 2 && tokens[start].tok == .lpar {
		wrapper_end := fastc_matching_rpar_before(tokens, start, end) or { break }
		if wrapper_end != end - 1 {
			break
		}
		start++
		end--
	}
	if start >= end {
		return u32(0)
	}
	if end - start == 1 {
		item := tokens[start]
		if item.typ != 0 {
			return item.typ
		}
		return match item.tok {
			.name {
				if smartcast := g.member_smartcasts[item.lit] {
					// A bare local narrowed by `x is T` reads as the concrete variant.
					smartcast.typ
				} else if local := g.locals[item.lit] {
					if local.is_reference && local.smartcast_origin_type != 0 {
						g.narrowed_numeric_operand_type(tokens[start..end], local.typ)
					} else {
						local.typ
					}
				} else {
					g.nonlocal_name_type(item.lit)
				}
			}
			.number {
				fastc_number_expression_type(item.lit)
			}
			.string {
				flat.source_type_string
			}
			.char {
				if item.lit.starts_with('c:') { fastc_type_charptr } else { flat.source_type_rune }
			}
			.key_true, .key_false {
				flat.source_type_bool
			}
			.key_nil {
				flat.source_type_nil
			}
			.key_none {
				fastc_type_option
			}
			else {
				u32(0)
			}
		}
	}
	if end - start == 3 && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name {
		direct := g.infer_local_direct_member_type(tokens, start, end)
		if direct != 0 { return direct }
	}
	if signature := g.function_alias_call_signature(tokens[start..end]) {
		return signature.return_type
	}
	// `X as T` yields the target type `T` (so `(x as T).field` resolves).
	if as_index := fastc_bare_as_cast_index(tokens, start, end) {
		if as_index + 1 < end {
			if target_source_id := g.type_from_expression_tokens(tokens[as_index + 1..end]) {
				target := target_source_id

				return fastc_normalize_inferred_type(target)
			}
		}
	}
	if end - start >= 4 && tokens[start].tok == .key_sizeof && tokens[start + 1].tok == .lpar && tokens[end - 1].tok == .rpar {
		return flat.source_type_int
	}
	if end - start >= 8 && tokens[start].lit == 'typeof' {
		if reflection_type := fastc_typeof_generic_field_type(tokens, start, end) {
			return reflection_type
		}
	}
	if end - start >= 9 && tokens[start].tok in [.amp, .and] {
		if c_field_type_source_id := g.infer_c_pointer_cast_member_type(tokens, start, end) {
			c_field_type := c_field_type_source_id

			return c_field_type
		}
	}
	if tokens[start].tok in [.amp, .and] {
		if close := fastc_member_root_call_close(tokens, start, end) {
			if tokens[close + 1].tok == .dot {
				mut root_name := start
				for root_name < close && tokens[root_name].tok in [.amp, .and] {
					root_name++
				}
				mut open := root_name
				for open < close && tokens[open].tok != .lpar {
					open++
				}
				if g.type_from_expression_tokens(tokens[start..open]) != none {
					// The pointer prefix belongs to the root's cast, while a following
					// field (and any arithmetic on it) has the field's value type.
					return g.infer_expression_type_range(tokens, root_name, end)!
				}
			}
		}
		if member_type := g.infer_member_access_type(tokens, start, end) {
			return member_type
		}
	}
	if tokens[start].tok == .not {
		_ = g.infer_expression_type_range(tokens, start + 1, end)!
		return flat.source_type_bool
	}
	if operator_index := fastc_lowest_precedence_operator_index(tokens, start, end) {
		return g.infer_boolean_binary_expression_type(tokens, start, end, operator_index)!
	}
	if tokens[end - 1].tok == .rpar {
		open := g.fastc_element_call_open(tokens[start..end])
		if open > 0 {
			element_type := g.infer_expression_type_range(tokens, start, start + open)!
			if signature := g.mono_functions[flat.source_type_name(element_type)] {
				return signature.return_type
			}
		}
	}
	if end - start == 2 && tokens[start].tok == .dot && tokens[start + 1].typ != 0 {
		return tokens[start + 1].typ
	}
	if end - start == 5 && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name && tokens[start + 3].tok == .dot && tokens[start + 4].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if type_key_id := flat.source_type_existing_id(type_key) {
				if enum_type_key := g.underlying_enum_type_key(type_key_id) {
					return flat.source_type_id(fastc_c_declared_type_name(flat.source_type_name(enum_type_key)))
				}
			}
		}
	}
	if tokens[start].tok in [.lsbr, .amp, .and] && tokens[end - 1].tok == .rpar {
		for open in start + 1 .. end {
			if tokens[open].tok != .lpar {
				continue
			}
			if cast_type_source_id := g.type_from_expression_tokens(tokens[start..open]) {
				cast_type := cast_type_source_id

				close := fastc_matching_rpar_before(tokens, open, end) or { break }
				if flat.source_type_query(cast_type).has(.c_array_prefix) && close == end - 1 {
					return cast_type
				}
			}
			break
		}
	}
	if end - start == 3 && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if fastc_source_type_key_exists(g.declared_types, type_key) {
				return flat.source_type_id(fastc_c_declared_type_name(type_key))
			}
		}
		if type_key := g.resolve_declared_type_key(tokens[start].lit) {
			if enum_type_key := g.underlying_enum_type_key(type_key) {
				return flat.source_type_id(fastc_c_declared_type_name(flat.source_type_name(enum_type_key)))
			}
		}
		if imported_module := g.imports[tokens[start].lit] {
			if constant_type := g.constant_types.type_id(fastc_constant_key(imported_module, tokens[start + 2].lit)) {
				return constant_type
			}
			if fastc_constant_key(imported_module, tokens[start + 2].lit) in g.constants {
				return fastc_type_integer_literal
			}
			global_name := tokens[start + 2].lit
			if global_type := g.global_types.type_id(fastc_global_key(imported_module, global_name)) {
				g.validate_imported_global_visibility(imported_module, global_name)!
				return global_type
			}
		}
	}
	for init_open in start + 1 .. end {
		if tokens[init_open].tok == .lcbr {
			if array_type_source_id := g.array_initializer_type(tokens[start..init_open]) {
				array_type := array_type_source_id

				return array_type
			}
			break
		}
	}
	array_end := if tokens[end - 1].tok == .not { end - 1 } else { end }
	array_literal_close := if tokens[start].tok == .lsbr {
		fastc_matching_delimiter_before(tokens, start, array_end, .lsbr, .rsbr) or { -1 }
	} else {
		-1
	}
	if array_literal_close == array_end - 1 && tokens[start].tok == .lsbr {
		items := fastc_expression_list_items(tokens, start + 1, array_end - 1)!
		if items.len == 0 {
			return u32(0)
		}
		if items[0].len > 1 && items[0][0].tok == .ellipsis {
			return g.infer_expression_type(items[0][1..])!
		}
		element_type := fastc_normalize_inferred_type(g.infer_expression_type(items[0])!)
		if element_type == u32(0) {
			return u32(0)
		}
		if array_end < end {
			return fastc_fixed_array_type_id('${items.len}', element_type)
		}
		return fastc_array_c_type_id(element_type)
	}
	if tokens[start].tok == .lcbr && tokens[end - 1].tok == .rcbr {
		// An inline map literal `{key: value, ...}` takes its type from its first entry,
		// as read_inferred_map_literal does.
		map_close := fastc_matching_delimiter_before(tokens, start, end, .lcbr, .rcbr) or { -1 }
		if map_close != end - 1 {
			return u32(0)
		}
		entries := fastc_map_literal_entries(tokens, start + 1, end - 1) or { return u32(0) }
		if entries.len == 0 {
			return u32(0)
		}
		colon := fastc_map_literal_entry_colon(entries[0])
		if colon <= 0 || colon + 1 >= entries[0].len || entries[0][0].tok == .dot {
			return u32(0)
		}
		key_type := fastc_normalize_inferred_type(g.infer_expression_type(entries[0][..colon])!)
		value_type := fastc_normalize_inferred_type(g.infer_expression_type(entries[0][colon + 1..])!)
		if key_type == u32(0) || value_type == u32(0) {
			return u32(0)
		}
		return fastc_map_c_type_id(key_type, value_type)
	}
	if start + 1 < end && tokens[start].tok == .name && tokens[start + 1].tok == .lcbr {
		if close := fastc_matching_delimiter_before(tokens, start + 1, end, .lcbr, .rcbr) {
			if close == end - 1 {
				if type_key := g.resolve_declared_type_key(tokens[start].lit) {
					return flat.source_type_id(fastc_c_declared_type_name(flat.source_type_name(type_key)))
				}
			}
		}
	}
	if start + 3 < end && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name && tokens[start + 3].tok == .lcbr {
		// A module-qualified struct literal `mod.Type{...}`.
		if tokens[start].lit == 'C' {
			if close := fastc_matching_delimiter_before(tokens, start + 3, end, .lcbr, .rcbr) {
				if close == end - 1 {
					return g.type_from_expression_tokens(tokens[start..start + 3]) or { u32(0) }
				}
			}
		}
		if imported_module := g.imports[tokens[start].lit] {
			if close := fastc_matching_delimiter_before(tokens, start + 3, end, .lcbr, .rcbr) {
				if close == end - 1 {
					type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
					if fastc_source_type_key_exists(g.declared_types, type_key) {
						return flat.source_type_id(fastc_c_declared_type_name(type_key))
					}
				}
			}
		}
	}
	mut call_name_index := start
	element_call_open := g.fastc_element_call_open(tokens[start..end])
	if element_call_open > 0 {
		if signature := g.callable_signature_for_expression(tokens[start..start + element_call_open]) {
			if signature.callable_index_depth == 0 {
				return signature.return_type
			}
		}
	}
	mut call_open_index := start + 1
	if start + 3 < end && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name && (tokens[start].lit in g.imports || tokens[start].lit == 'C') {
		call_name_index = start + 2
		call_open_index = start + 3
	}
	if call_open_index < end && tokens[call_name_index].tok in [.name, .key_select] && tokens[call_open_index].tok == .lpar {
		if close := fastc_matching_rpar_before(tokens, call_open_index, end) {
			if close == end - 1 {
				name := tokens[call_name_index].lit
				function_key := g.function_key_for_call(tokens, call_name_index)
				signature := if function_key in g.functions {
					g.functions[function_key]
				} else {
					g.mono_functions[function_key] or { FastcFunctionSignature{} }
				}
				if signature.return_type != 0 {
					return signature.return_type
				}
				if call_name_index == start {
					if local := g.locals[name] {
						if local.fn_return_type != 0 {
							// Calling a function-pointer parameter (`f(x)`).
							return local.fn_return_type
						}
						// Calling a value whose type is a `type X = fn (...) Ret` alias.
						if ret := g.fn_alias_return_types.type_id(flat.source_type_name(fastc_trim_pointer_type(local.typ))) {
							return ret
						}
					}
					if primitive := fastc_primitive_c_type(name) {
						return flat.source_type_id(primitive)
					}
					if type_key := g.resolve_declared_type_key(name) {
						return flat.source_type_id(fastc_c_declared_type_name(flat.source_type_name(type_key)))
					}
				}
				if call_name_index == start + 2 && tokens[start].lit in g.imports {
					module_name := g.imports[tokens[start].lit]
					type_key := fastc_type_key(module_name, name)
					if fastc_source_type_key_exists(g.declared_types, type_key) {
						return flat.source_type_id(fastc_c_declared_type_name(type_key))
					}
				}
				if call_name_index == start + 2 && tokens[start].lit == 'C' {
					if fastc_source_type_key_exists(g.declared_types, '#Cstruct#${name}') {
						return flat.source_type_id('struct ${name}')
					}
					if name.len > 0 && (name[0].is_capital() || fastc_source_type_key_exists(g.declared_types, '#Ctypedef#${name}')) {
						return flat.source_type_id(name)
					}
				}
				return u32(0)
			}
		}
	}
	for i in start + 2 .. end - 1 {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		close := fastc_matching_rpar_before(tokens, i + 1, end) or { continue }
		if close != end - 1 {
			continue
		}
		receiver_start := fastc_method_receiver_start_after(tokens, i - 1, start)
		if receiver_start != start {
			// A prefix before the receiver (`*a.node(id)`, `-x.len()`) modifies the
			// method result, so its type is not the method return type; let the
			// prefix-operator handling below (e.g. `*` deref) infer it instead.
			continue
		}
		receiver_type := g.infer_expression_type_range(tokens, receiver_start, i - 1)!
		if receiver_type == u32(0) {
			continue
		}
		if tokens[i].lit in ['map', 'filter', 'any', 'all', 'count'] && flat.source_type_query(fastc_normalize_inferred_type(g.underlying_alias_type(receiver_type))).has(.c_array_prefix) {
			// The magic closure methods change the result type: `filter` keeps the array,
			// `count` is an int, `any`/`all` are bools, and `map` yields an array of the
			// closure's element type (inferred with `it` bound to the element).
			method := tokens[i].lit
			if method == 'filter' {
				return fastc_trim_pointer_type(fastc_normalize_inferred_type(g.underlying_alias_type(receiver_type)))
			}
			if method == 'count' {
				return flat.source_type_int
			}
			if method in ['any', 'all'] {
				return flat.source_type_bool
			}
			array_type := fastc_normalize_inferred_type(g.underlying_alias_type(receiver_type))
			if element_type_source_id := g.array_element_type(array_type) {
				element_type := element_type_source_id

				mut closure_start := i + 2
				mut it_name := 'it'
				if tokens[closure_start].tok == .pipe && closure_start + 2 < close {
					it_name = tokens[closure_start + 1].lit
					closure_start += 3
				}
				mut w := unsafe { &Parser(g) }
				had_it := it_name in g.locals
				saved_it := g.locals[it_name] or { FastcLocal{} }
				w.locals[it_name] = FastcLocal{
					typ: element_type
				}
				closure_type := (g.infer_expression_type_range(tokens, closure_start, close) or { u32(0) })
				if had_it {
					w.locals[it_name] = saved_it
				} else {
					w.locals.delete(it_name)
				}
				if closure_type != u32(0) {
					return fastc_array_c_type_id(fastc_normalize_inferred_type(closure_type))
				}
			}
		}
		if tokens[i].lit == 'wait' && flat.source_type_query(receiver_type).has(.c_thread_prefix) {
			value_type := (g.fastc_thread_value_type(receiver_type) or { u32(0) })
			if value_type == u32(0) {
				return flat.source_type_void
			}
			return value_type
		}
		if tokens[i].lit == 'wait' && flat.source_type_query(receiver_type).has(.c_array_prefix) {
			// `[]thread T`.wait() joins every thread and returns their `[]T` results.
			element := (g.array_element_type(receiver_type) or { u32(0) })
			if flat.source_type_query(element).has(.c_thread_prefix) {
				value_type := (g.fastc_thread_value_type(element) or { u32(0) })
				if value_type != u32(0) {
					return fastc_array_c_type_id(value_type)
				}
			}
		}
		mut function_key, _ := g.resolve_method(receiver_type, tokens[i].lit)
		mut method_receiver_type := receiver_type
		if function_key !in g.functions && i - receiver_start == 2 && tokens[receiver_start].tok == .name && tokens[i - 1].tok == .dot {
			// A method of the whole sum type (`fn (t Type) tname()`) invoked on a NARROWED variant
			// receiver (`base is Prim && base.tname()`) is not found on the variant; resolve it on
			// the local's boxed origin, as the method renderer does — so a `base.tname() == 'u8'`
			// string comparison is typed as string and not left a raw pointer `==`.
			if local := g.locals[tokens[receiver_start].lit] {
				origin := if local.smartcast_origin_type != 0 {
					local.smartcast_origin_type
				} else {
					local.typ
				}
				if origin != 0 && origin != receiver_type {
					origin_key, _ := g.resolve_method(origin, tokens[i].lit)
					if origin_key in g.functions {
						function_key = origin_key
						method_receiver_type = origin
					}
				}
			}
		}
		if static_key := g.static_function_key_for_call(tokens, i) {
			if signature := g.functions[static_key] {
				return signature.return_type
			}
		}
		if signature := g.functions[function_key] {
			return g.specialized_method_return_type(method_receiver_type, function_key, signature)
		}
		if field := g.struct_field_metadata(receiver_type, tokens[i].lit) {
			if field.is_function && field.fn_return_type != 0 {
				return field.fn_return_type
			}
			if function_alias := g.functions[field.typ_text()] {
				return function_alias.return_type
			}
		}
		if tokens[i].lit == 'str' && g.can_generate_default_struct_str(receiver_type) {
			return flat.source_type_string
		}
		if tokens[i].lit == 'str' && g.declared_kinds[g.semantic_type_key(receiver_type)] == .enum_ {
			return flat.source_type_string
		}
		if tokens[i].lit == 'str' && (flat.source_type_query(receiver_type).has(.c_array_prefix)
			|| flat.source_type_name(fastc_trim_pointer_type(receiver_type)) in g.sum_types) {
			return flat.source_type_string
		}
		if tokens[i].lit == 'type_name' && g.is_boxed_type(fastc_normalize_inferred_type(receiver_type)) {
			return flat.source_type_string
		}
	}
	if end - start >= 3 && tokens[end - 2].tok == .dot && tokens[end - 1].tok == .name {
		receiver_start := fastc_method_receiver_start_after(tokens, end - 2, start)
		if receiver_start == start {
			// A member smartcast on the full path overrides the declared field type
			// (`if x.f is T { … x.f.g … }`, incl. an indexed chain `x.args[0].expr` keyed
			// as `x.args[].expr`, reads the member as the concrete variant).
			if member_path := fastc_indexed_member_chain_path(tokens[start..end]) {
				if smartcast := g.member_smartcasts[member_path] {
					return smartcast.typ
				}
			}
			receiver_type := (g.infer_expression_type_range(tokens, start, end - 2)!)
			direct_type := g.struct_direct_member_type(receiver_type, tokens[end - 1].lit)
			if direct_type != 0 {
				return direct_type
			}
			if field := g.struct_field_metadata(receiver_type, tokens[end - 1].lit) {
				return field.typ
			}
			// A field shared by every variant of a boxed sum type (`node.name` where
			// `node` is `ast.TypeDecl`) resolves to that common field's type.
			if common_source_id := g.sumtype_common_field_type(receiver_type, tokens[end - 1].lit) {
				common := common_source_id

				return common
			}
		}
	}
	if tokens[start].tok in [.plus, .minus] {
		operand_type := g.infer_expression_type_range(tokens, start + 1, end)!
		if tokens[start].tok == .minus && operand_type == fastc_type_integer_literal {
			return fastc_type_negative_integer_literal
		}
		return operand_type
	}
	if tokens[start].tok in [.amp, .and] {
		// `&` binds tighter than `+`/`-`: `&u8(raw) + n` is pointer arithmetic on
		// `&u8(raw)`, so only the operand before the operator is addressed.
		operand_end := fastc_prefix_operand_end(tokens, start + 1, end)
		operand_type := g.infer_expression_type_range(tokens, start + 1, operand_end)!
		// A mutable/reference local has pointer storage but value type `T` in V.
		// Taking `&local` therefore yields its existing `T*` storage type, not
		// `T**`. The rendered `&(*local)` expression follows the same rule.
		if tokens[start].tok == .amp && end - start == 2 && tokens[start + 1].tok == .name {
			if local := g.locals[tokens[start + 1].lit] {
				if local.is_reference {
					return local.typ
				}
			}
		}
		mut pointer_count := 1
		if tokens[start].tok == .and {
			pointer_count = 2
		}
		return if operand_type == 0 {
			flat.source_type_voidptr
		} else {
			fastc_with_pointer_depth(operand_type, pointer_count)
		}
	}
	if tokens[start].tok == .mul {
		operand_type := g.infer_expression_type_range(tokens, start + 1, end)!
		// Dereferencing removes exactly one pointer level. trim_right('*') collapses
		// `T**` all the way to `T`, which makes values such as
		// `*(&&ast.File(ptr))` look by-value and adds a bogus address-of later.
		return if flat.source_type_query(operand_type).has(.c_pointer_suffix) {
			flat.source_type_syntax(operand_type).c_pointer_base()
		} else {
			operand_type
		}
	}
	if tokens[start].tok == .bit_not {
		operand_type := g.infer_expression_type_range(tokens, start + 1, end)!
		return operand_type
	}
	if g.selfhost && tokens[end - 1].tok == .not {
		value_type := g.option_value_type_for_expression(tokens[start..end - 1])
		return flat.source_type_id(if value_type == flat.source_type_void {
			''
		} else {
			flat.source_type_name(value_type)
		})
	}
	if tokens[end - 1].tok in [.inc, .dec] {
		operand_type := g.infer_expression_type_range(tokens, start, end - 1)!
		if g.selfhost && operand_type == u32(0) {
			return flat.source_type_int
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
			base_type := g.infer_expression_type_range(tokens, start, open_index)!
			base_layout_id := fastc_trim_pointer_type(g.map_type_through_alias(base_type))
			if fastc_top_level_token_index(tokens, open_index + 1, end - 1, .dotdot) != none {
				// Slicing a fixed array yields a dynamic array of its element type.
				if flat.source_type_query(base_layout_id).has(.c_fixed_array_prefix) {
					if element_source_id := g.array_element_type(base_layout_id) {
						element := element_source_id

						return fastc_array_c_type_id(fastc_normalize_inferred_type(element))
					}
				}
				if g.is_string_layout(base_type)
					&& g.is_reference_local_base(tokens[start..open_index], base_type) {
					// Slicing a `mut` string local copies bytes of the string it refers to.
					return flat.source_type_syntax(base_type).c_pointer_base()
				}
				return base_type
			}
			if flat.source_type_query(base_layout_id).has(.c_map_prefix) {
				_, value_type_source_id := g.map_key_value_types(base_type) or { return u32(0) }
				value_type := value_type_source_id

				return value_type
			}
			if base_layout_id == flat.source_type_string || g.is_string_layout(base_type) {
				return g.indexed_string_type(base_type, tokens[start..open_index])
			}
			if element_type_source_id := g.fixed_array_pointee_element_type(base_type, tokens[start..open_index]) {
				element_type := element_type_source_id

				return element_type
			}
			if element_type_source_id := g.array_element_type(base_type) {
				element_type := element_type_source_id

				return element_type
			}
			if flat.source_type_query(base_type).has(.c_pointer_suffix) {
				return flat.source_type_syntax(base_type).c_pointer_base()
			}
		}
	}
	if member_type_source_id := g.infer_member_access_type(tokens, start, end) {
		member_type := member_type_source_id

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
			return g.infer_expression_type_range(tokens, start, i)!
		}
		if tokens[i].tok in [.left_shift, .right_shift, .right_shift_unsigned] && i > start {
			mut left_type := g.infer_expression_type_range(tokens, start, i)!
			mut right_type := g.infer_expression_type_range(tokens, i + 1, end)!
			if left_element_source_id := g.indexed_array_operand_type(tokens, start, i, left_type) {
				left_element := left_element_source_id

				left_type = left_element
			}
			if right_element_source_id := g.indexed_array_operand_type(tokens, i + 1, end, right_type) {
				right_element := right_element_source_id

				right_type = right_element
			}
			if g.selfhost && tokens[i].tok == .left_shift && g.array_element_type(left_type) != none {
				return left_type
			}
			if g.selfhost && left_type == u32(0) && fastc_is_integer_expression_type(right_type) {
				return flat.source_type_int
			}
			return if left_type != 0 {
				left_type
			} else if right_type != 0 {
				right_type
			} else {
				flat.source_type_int
			}
		}
		if tokens[i].tok in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor] && i > start {
			mut left_type := g.infer_expression_type_range(tokens, start, i)!
			mut right_type := g.infer_expression_type_range(tokens, i + 1, end)!
			if left_element_source_id := g.indexed_array_operand_type(tokens, start, i, left_type) {
				left_element := left_element_source_id

				left_type = left_element
			}
			if right_element_source_id := g.indexed_array_operand_type(tokens, i + 1, end, right_type) {
				right_element := right_element_source_id

				right_type = right_element
			}
			if tokens[i].tok == .plus && fastc_trim_pointer_type(g.underlying_alias_type(left_type)) == flat.source_type_string && fastc_trim_pointer_type(g.underlying_alias_type(right_type)) == flat.source_type_string {
				return flat.source_type_string
			}
			if g.selfhost && tokens[i].tok == .plus && ((left_type == flat.source_type_string && right_type == u32(0)) || (right_type == flat.source_type_string && left_type == u32(0))) {
				return flat.source_type_string
			}
			if g.selfhost && tokens[i].tok in [.plus, .minus] && fastc_is_pointer_type(left_type) && fastc_is_integer_expression_type(right_type) {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && left_type == right_type && g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_type == u32(0) {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_type == u32(0) {
				return right_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(left_type)
				&& fastc_is_numeric_expression_type(right_type) {
				return fastc_numeric_infix_result_type(left_type, right_type, fastc_integer_literal_operand(tokens[start..i]),
					fastc_integer_literal_operand(tokens[i + 1..end]))
			}
			if g.selfhost && left_type == u32(0) && fastc_is_numeric_expression_type(right_type) {
				return right_type
			}
			if g.selfhost && right_type == u32(0) && fastc_is_numeric_expression_type(left_type) {
				return left_type
			}
			if g.selfhost && right_type == flat.source_type_voidptr && fastc_is_numeric_expression_type(left_type) {
				return left_type
			}
			if g.selfhost && left_type == flat.source_type_voidptr && fastc_is_numeric_expression_type(right_type) {
				return right_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(left_type) && g.declared_kinds[g.semantic_type_key(right_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(right_type) && g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return right_type
			}
			if g.selfhost && left_type == right_type && g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && left_type == u32(0) && right_type == u32(0) {
				return flat.source_type_int
			}
			common_type := fastc_common_arithmetic_type(left_type, right_type)
			if common_type == 0 {
				return if left_type != 0 {
					left_type
				} else if right_type != 0 {
					right_type
				} else {
					flat.source_type_int
				}
			}
			return common_type
		}
	}
	return u32(0)
}

fn (g &Parser) infer_c_pointer_cast_member_type(tokens []FastcExpressionToken, start int, end int) ?u32 {
	if end - start < 9 || tokens[start].tok !in [.amp, .and] || tokens[start + 1].tok != .name || tokens[start + 1].lit != 'C' || tokens[start + 2].tok != .dot || tokens[start + 3].tok != .name || tokens[start + 4].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar_before(tokens, start + 4, end) or { return none }
	if close + 2 >= end || tokens[close + 1].tok != .dot || tokens[close + 2].tok != .name || close + 3 != end {
		return none
	}
	c_name := tokens[start + 3].lit
	c_type := if fastc_source_type_key_exists(g.declared_types, '#Cstruct#${c_name}') {
		'struct ${c_name}'
	} else {
		c_name
	}
	field_type := g.struct_member_type(flat.source_type_id(c_type + '*'), tokens[close + 2].lit)
	if field_type == u32(0) {
		return none
	}
	return field_type
}

// is_string_layout reports whether `typ`, or the type it points to, is laid out as a
// `string`, through aliases (`type Name = string`).
fn (g &Parser) is_string_layout(typ u32) bool {
	return fastc_trim_pointer_type(g.underlying_alias_type(typ)) == flat.source_type_string
}

// indexed_string_type is the type of `base[i]` for a base laid out as a `string`. A string
// value, or an auto-deref `mut` local referring to one (C type `string*`), yields a byte.
// Any other `string` pointer (a `&string` parameter) is indexed by pointer arithmetic, as
// in `unsafe { input_base[i] }`, and yields the pointee `string`.
fn (g &Parser) indexed_string_type(base_type u32, base_tokens []FastcExpressionToken) u32 {
	if !flat.source_type_query(base_type).has(.c_pointer_suffix) {
		return flat.source_type_u8
	}
	if base_tokens.len == 1 && base_tokens[0].tok == .name {
		if local := g.locals[base_tokens[0].lit] {
			if local.is_reference {
				return flat.source_type_u8
			}
		}
	}
	return flat.source_type_syntax(base_type).c_pointer_base()
}

fn (g &Parser) indexed_array_operand_type(tokens []FastcExpressionToken, start int, end int, inferred_type u32) ?u32 {
	if end - start < 3 || !fastc_expression_tokens_contain_range(tokens, start, end, .lsbr) || tokens[end - 1].tok != .rsbr {
		return none
	}
	return g.array_element_type(inferred_type)
}

fn fastc_expression_tokens_contain_range(tokens []FastcExpressionToken, start int, end int, wanted token.Token) bool {
	for i in start .. end {
		if tokens[i].tok == wanted {
			return true
		}
	}
	return false
}

// fastc_prefix_operand_end returns where the operand of a prefix operator that
// starts at `start` ends: before the first top-level binary `+` or `-`.
fn fastc_prefix_operand_end(tokens []FastcExpressionToken, start int, end int) int {
	mut depth := 0
	for i in start .. end {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && i > start && tokens[i].tok in [.plus, .minus]
			&& !tokens[i - 1].tok.is_infix() && !tokens[i - 1].tok.is_prefix() {
			return i
		}
	}
	return end
}

fn fastc_top_level_token_index(tokens []FastcExpressionToken, start int, end int, wanted token.Token) ?int {
	mut depth := 0
	for i in start .. end {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && tokens[i].tok == wanted {
			return i
		}
	}
	return none
}

// fastc_member_chain_path returns the dotted path (`a.b.c`) for a pure member
// chain `name (. name)*` spanning tokens[start..end], or none for anything else.
fn fastc_member_chain_path(tokens []FastcExpressionToken, start int, end int) ?string {
	if start >= end || tokens[start].tok != .name {
		return none
	}
	mut path := tokens[start].lit
	mut i := start + 1
	for i < end {
		if i + 1 >= end || tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		path += '.' + tokens[i + 1].lit
		i += 2
	}
	return path
}

// fastc_indexed_member_chain_path is like fastc_member_chain_path but also accepts array
// index segments (`right.args[0].expr`), keying each `[…]` as the `[]` marker that
// render_member_receiver uses so a member smart-cast registered here is found when the
// chain is rendered.
fn fastc_indexed_member_chain_path(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens[0].tok != .name {
		return none
	}
	mut path := tokens[0].lit
	mut i := 1
	for i < tokens.len {
		if tokens[i].tok == .lsbr {
			close := fastc_matching_delimiter(tokens, i, .lsbr, .rsbr) or { return none }
			path += '[]'
			i = close + 1
			continue
		}
		if i + 1 < tokens.len && tokens[i].tok == .dot && tokens[i + 1].tok == .name {
			path += '.' + tokens[i + 1].lit
			i += 2
			continue
		}
		return none
	}
	return path
}

// sum_type_variant_list returns the C variant type names of a sum type (`base` is a
// normalized C type name; any pointer suffix is trimmed).
fn (g &Parser) sum_type_variant_list(sum_type_id u32) []u32 {
	base := fastc_trim_pointer_type(sum_type_id)
	mut variants := []u32{}
	for key, present in g.sum_type_variants {
		if present && u32(key >> 32) == base {
			variants << u32(key)
		}
	}
	return variants
}

@[inline]
fn fastc_sum_type_variant_key(sum_type_id u32, variant_id u32) u64 {
	return (u64(sum_type_id) << 32) | u64(variant_id)
}

// sum_type_variant_keys keeps output helper cases in the spelling order used by the C renderer.
fn (g &Parser) sum_type_variant_keys(sum_type_id u32) []u32 {
	mut variants := []u32{}
	for key, _ in g.sum_type_variants {
		if u32(key >> 32) == sum_type_id {
			variants << u32(key)
		}
	}
	variants.sort_with_compare(fn (a &u32, b &u32) int {
		return flat.source_type_name(*a).compare(flat.source_type_name(*b))
	})
	return variants
}

// sum_type_leaf_variants returns the concrete (non-sum-type) variants of `sum_type`,
// recursively expanding any variant that is itself a sum type. A value whose static type is
// a sum type always carries the runtime tag of one of these leaf struct variants, so field
// dispatch and common-field detection must reason over the flattened set.
fn (g &Parser) sum_type_leaf_variants(sum_type u32) []u32 {
	mut leaves := []u32{}
	for variant in g.sum_type_variant_list(sum_type) {
		if flat.source_type_name(fastc_trim_pointer_type(variant)) in g.sum_types {
			leaves << g.sum_type_leaf_variants(variant)
		} else {
			leaves << variant
		}
	}
	return leaves
}

// sumtype_common_field_type returns the shared C type of a field that every variant of
// `sum_type` declares (a V "common sum-type field"), or none when the field is absent
// from some variant, has different types across variants, or is reached through an
// embedded field. Only direct fields are treated as common. Variants that are themselves
// sum types are flattened to their leaf struct variants first.
fn (g &Parser) sumtype_common_field_type(sum_type_id u32, field_name string) ?u32 {
	base := fastc_trim_pointer_type(sum_type_id)
	if flat.source_type_name(base) !in g.sum_types {
		return none
	}
	variants := g.sum_type_leaf_variants(base)
	if variants.len == 0 {
		return none
	}
	mut common_type := u32(0)
	for variant in variants {
		field := g.struct_field_metadata(variant, field_name) or { return none }
		if field.storage_path.len > 0 {
			return none
		}
		if common_type == u32(0) {
			common_type = field.typ
		} else if field.typ != common_type {
			return none
		}
	}
	if common_type == u32(0) {
		return none
	}
	return common_type
}

// resolve_cross_module_global_type returns the type of a `__global` declared in ANY module,
// looked up by its bare name — `__global`s are truly global, so a reference from a different
// module than the declaring one (`global_table` in v.ast, used from v.checker) still resolves.
fn (g &Parser) resolve_cross_module_global_type(name string) ?u32 {
	if key := g.unqualified_global_key(name) {
		return g.global_types.type_id(key)
	}
	if g.global_suffix_index.ready && !g.declaration_initializer_mode {
		key := g.global_suffix_index.type_keys[name] or { return none }
		return g.global_types.type_id(key)
	}
	suffix := '.${name}'
	for key, global_type_id in g.global_types.ids {
		if key == name || key.ends_with(suffix) {
			return global_type_id
		}
	}
	return none
}

fn (g &Parser) infer_member_access_type(tokens []FastcExpressionToken, start int, end int) ?u32 {
	if end - start < 3 {
		return none
	}
	mut current_type := u32(0)
	mut index := start + 1
	if call_close := fastc_member_root_call_close(tokens, start, end) {
		if tokens[start].tok in [.amp, .and] {
			mut cast_open := start + 1
			for cast_open < call_close && tokens[cast_open].tok != .lpar {
				cast_open++
			}
			if g.type_from_expression_tokens(tokens[start..cast_open]) == none {
				// `&factory().field` takes the field's address; only a typed
				// pointer cast makes the leading `&` part of the member's root.
				return none
			}
		}
		current_type = g.infer_expression_type_range(tokens, start, call_close + 1) or { return none }
		index = call_close + 1
	} else if tokens[start].tok != .name {
		return none
	} else if tokens[start].typ != 0 {
		current_type = tokens[start].typ
	} else if local := g.locals[tokens[start].lit] {
		current_type = local.typ
	} else if global_type := g.global_types.type_id(fastc_global_key(g.module_name, tokens[start].lit)) {
		current_type = global_type
	} else if constant_type := g.constant_types.type_id(fastc_constant_key(g.module_name, tokens[start].lit)) {
		current_type = constant_type
	} else if constant_type := g.constant_types.type_id(fastc_constant_key('builtin', tokens[start].lit)) {
		current_type = constant_type
	} else if global_type := g.resolve_cross_module_global_type(tokens[start].lit) {
		current_type = global_type
	} else {
		return none
	}
	mut member_path := if index == start + 1 { tokens[start].lit } else { '()' }
	// A smart-cast on the bare subject itself (`x is T`, or an option local narrowed by
	// `x != none`) reshapes it before any field access, exactly as render_member_receiver
	// does when producing the matching source.
	if smartcast := g.member_smartcasts[member_path] {
		current_type = smartcast.typ
	}
	for index < end {
		if tokens[index].tok == .lsbr {
			close := fastc_matching_delimiter_before(tokens, index, end, .lsbr, .rsbr) or {
				return none
			}
			current_layout_id := fastc_trim_pointer_type(g.map_type_through_alias(current_type))
			if fastc_top_level_token_index(tokens, index + 1, close, .dotdot) != none {
				if current_layout_id != flat.source_type_string && g.array_element_type(current_type) == none {
					return none
				}
				// A fixed-array slice becomes a dynamic array of its element type.
				if flat.source_type_query(current_layout_id).has(.c_fixed_array_prefix) {
					if element_source_id := g.array_element_type(current_layout_id) {
						element := element_source_id

						current_type = fastc_array_c_type_id(fastc_normalize_inferred_type(element))
					}
				}
			} else if current_layout_id == flat.source_type_string || g.is_string_layout(current_type) {
				current_type = g.indexed_string_type(current_type, tokens[start..index])
			} else if flat.source_type_query(current_layout_id).has(.c_map_prefix) {
				_, value_type_source_id := g.map_key_value_types(current_type) or { return none }
				value_type := value_type_source_id

				current_type = value_type
			} else if element_type_source_id := g.fixed_array_pointee_element_type(current_type, tokens[start..index]) {
				element_type := element_type_source_id

				current_type = element_type
			} else if element_type_source_id := g.array_element_type(current_type) {
				element_type := element_type_source_id

				current_type = element_type
			} else if flat.source_type_query(current_type).has(.c_pointer_suffix) {
				current_type = flat.source_type_syntax(current_type).c_pointer_base()
			} else {
				return none
			}
			// Key the index segment as the `[]` marker so a member smart-cast registered on an
			// indexed chain (`right.args[0].expr is T` → `right.args[].expr`) is found below.
			member_path += '[]'
			if smartcast := g.member_smartcasts[member_path] {
				current_type = smartcast.typ
			}
			index = close + 1
			continue
		}
		if index + 1 >= end || tokens[index].tok != .dot || tokens[index + 1].tok != .name {
			return none
		}
		field_name := tokens[index + 1].lit
		current_type = g.struct_member_type(current_type, field_name)
		if current_type == u32(0) {
			return none
		}
		member_path += '.' + field_name
		if smartcast := g.member_smartcasts[member_path] {
			current_type = smartcast.typ
		}
		index += 2
	}
	if index != end {
		return none
	}
	return current_type
}

fn fastc_matching_delimiter(tokens []FastcExpressionToken, open_index int, open token.Token, close token.Token) ?int {
	return fastc_matching_delimiter_before(tokens, open_index, tokens.len, open, close)
}

fn fastc_matching_delimiter_before(tokens []FastcExpressionToken, open_index int, end int, open token.Token, close token.Token) ?int {
	mut depth := 0
	for i in open_index .. end {
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

// fastc_trim_pointer_type retains the spelling ID when no pointer suffix is present.
@[inline]
fn fastc_trim_pointer_type(typ_id u32) u32 {
	return flat.source_type_syntax(typ_id).c_pointer_raw_base()
}

fn fastc_with_pointer_depth(typ_id u32, depth int) u32 {
	if depth == 0 { return typ_id }
	mut parts := [typ_id]
	for _ in 0 .. depth { parts << flat.source_type_id('*') }
	return flat.source_type_composition(.concat, parts)
}

// semantic_type_key memoizes only lookups against finalized file-generation
// descriptors. Collisions recompute the original lookup and replace the slot.
@[direct_array_access]
fn (g &Parser) semantic_type_key(c_type_id u32) u32 {
	if isnil(g.type_descriptor_memo) || c_type_id == 0 {
		return g.semantic_type_key_uncached(c_type_id)
	}
	slot := int(c_type_id & 63)
	entry := g.type_descriptor_memo.semantic_keys[slot]
	if entry.key == c_type_id { return entry.value }
	resolved := g.semantic_type_key_uncached(c_type_id)
	mut memo := g.type_descriptor_memo
	memo.semantic_keys[slot] = FastcTypeDescriptorMemoEntry{ key: c_type_id, value: resolved }
	return resolved
}

fn (g &Parser) semantic_type_key_uncached(c_type_id u32) u32 {
	base_id := fastc_trim_pointer_type(c_type_id)
	// The C ABI keeps the old typedef, while methods use the canonical V name.
	if base_id == flat.source_type_usize { return flat.source_type_uint }
	if key := g.declared_type_c_names.type_id(flat.source_type_name(base_id)) {
		return key
	}
	return base_id
}

// underlying_alias_type uses the same per-file immutability contract as the
// semantic-key memo; alias collection and arbitrary Parser values stay uncached.
@[direct_array_access]
fn (g &Parser) underlying_alias_type(c_type_id u32) u32 {
	if isnil(g.type_descriptor_memo) || c_type_id == 0 {
		return fastc_underlying_alias_type(c_type_id, g.alias_base_types)
	}
	slot := int(c_type_id & 63)
	entry := g.type_descriptor_memo.aliases[slot]
	if entry.key == c_type_id { return entry.value }
	resolved := fastc_underlying_alias_type(c_type_id, g.alias_base_types)
	mut memo := g.type_descriptor_memo
	memo.aliases[slot] = FastcTypeDescriptorMemoEntry{ key: c_type_id, value: resolved }
	return resolved
}

fn fastc_underlying_alias_type(c_type_id u32, alias_base_types flat.TypeTextBindings) u32 {
	// Most descriptors are not aliases. Avoid allocating a cycle guard on that path.
	first_syntax := flat.source_type_syntax(c_type_id)
	base0 := first_syntax.c_pointer_raw_base()
	first := alias_base_types.type_id(flat.source_type_name(base0)) or { return c_type_id }
	mut resolved := fastc_with_pointer_depth(first, first_syntax.c_pointer_depth())
	// A single alias hop needs no cycle-guard map. Preserve the first
	// repeated-base result before checking whether another hop exists.
	second_syntax := flat.source_type_syntax(resolved)
	base1 := second_syntax.c_pointer_raw_base()
	if base1 == base0 { return resolved }
	second := alias_base_types.type_id(flat.source_type_name(base1)) or { return resolved }
	mut seen := map[u32]bool{}
	seen[base0] = true
	seen[base1] = true
	resolved = fastc_with_pointer_depth(second, second_syntax.c_pointer_depth())
	for {
		syntax := flat.source_type_syntax(resolved)
		base_id := syntax.c_pointer_raw_base()
		if base_id in seen { return resolved }
		alias_base := alias_base_types.type_id(flat.source_type_name(base_id)) or { return resolved }
		seen[base_id] = true
		resolved = fastc_with_pointer_depth(alias_base, syntax.c_pointer_depth())
	}
}

fn fastc_number_expression_type(literal string) u32 {
	clean := literal.replace('_', '')
	if clean.contains('.') || (!(clean.starts_with('0x') || clean.starts_with('0X')) && clean.contains_any('eE')) {
		return fastc_type_float_literal
	}
	if clean.starts_with('-') {
		return fastc_type_negative_integer_literal
	}
	return fastc_type_integer_literal
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
	if tokens[number_index].tok != .number || fastc_number_expression_type(tokens[number_index].lit) != fastc_type_integer_literal {
		return none
	}
	mut value := tokens[number_index].lit.replace('_', '').i64()
	if sign < 0 {
		value = -value
	}
	return value
}

fn fastc_common_arithmetic_type(left_id u32, right_id u32) u32 {
	if left_id == right_id && fastc_is_numeric_expression_type(left_id) {
		return left_id
	}
	if left_id == fastc_type_negative_integer_literal && fastc_is_unsigned_integer_type(right_id) {
		return u32(0)
	}
	if right_id == fastc_type_negative_integer_literal && fastc_is_unsigned_integer_type(left_id) {
		return u32(0)
	}
	if fastc_is_integer_literal_expression_type(left_id) && fastc_is_integer_type(right_id) {
		return right_id
	}
	if fastc_is_integer_literal_expression_type(right_id) && fastc_is_integer_type(left_id) {
		return left_id
	}
	if fastc_is_integer_literal_expression_type(left_id) && fastc_is_integer_literal_expression_type(right_id) {
		return if left_id == fastc_type_negative_integer_literal || right_id == fastc_type_negative_integer_literal {
			fastc_type_negative_integer_literal
		} else {
			fastc_type_integer_literal
		}
	}
	if left_id == fastc_type_float_literal && right_id in [flat.source_type_f32, flat.source_type_f64] {
		return right_id
	}
	if right_id == fastc_type_float_literal && left_id in [flat.source_type_f32, flat.source_type_f64] {
		return left_id
	}
	return u32(0)
}

// fastc_numeric_infix_result_type returns the type of an arithmetic infix on two
// numeric operands, as the checker's resolve_type gives it: a float operand makes
// a float (f32 only with f32, integer or untyped float operands), an untyped integer
// literal takes the other operand's type (`int` when its value does not fit an 8
// or 16-bit type), and two integer types meet as in infix_integer_promotion_type.
// `left_value` and `right_value` are the values of integer literal operands.
fn fastc_numeric_infix_result_type(left u32, right u32, left_value ?i64, right_value ?i64) u32 {
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
		if left == fastc_type_float_literal && right == fastc_type_float_literal {
			return fastc_type_float_literal
		}
		return flat.source_type_f64
	}
	left_literal := fastc_is_integer_literal_expression_type(left)
	right_literal := fastc_is_integer_literal_expression_type(right)
	if left_literal && right_literal {
		return if left == fastc_type_integer_literal { right } else { left }
	}
	if left_literal {
		return fastc_integer_literal_adopted_type(right, left_value)
	}
	if right_literal {
		return fastc_integer_literal_adopted_type(left, right_value)
	}
	if left == right {
		return left
	}
	if fastc_integer_widens_to(left, right) {
		return right
	}
	return left
}

// fastc_integer_literal_adopted_type is the type an untyped integer literal takes
// next to an operand of type `other` (the checker's int_literal_promoted_infix_type).
fn fastc_integer_literal_adopted_type(other u32, value ?i64) u32 {
	if other in [flat.source_type_i8, flat.source_type_u8, flat.source_type_i16, flat.source_type_u16] {
		if v := value {
			fits := match other {
				flat.source_type_i8 { v >= -128 && v <= 127 }
				flat.source_type_u8 { v >= 0 && v <= 255 }
				flat.source_type_i16 { v >= -32768 && v <= 32767 }
				else { v >= 0 && v <= 65535 }
			}
			if !fits {
				return flat.source_type_int
			}
		}
	}
	return other
}

// fastc_integer_literal_operand returns the value of an operand that is an integer
// literal, possibly negated.
fn fastc_integer_literal_operand(tokens []FastcExpressionToken) ?i64 {
	if tokens.len == 1 && fastc_is_integer_literal_token(tokens[0]) {
		return tokens[0].lit.i64()
	}
	if tokens.len == 2 && tokens[0].tok == .minus && fastc_is_integer_literal_token(tokens[1]) {
		value := tokens[1].lit.i64()
		return -value
	}
	return none
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
	return match typ {
		flat.source_type_i8 { 5 }
		flat.source_type_i16 { 6 }
		flat.source_type_i32 { 7 }
		flat.source_type_int { 8 }
		flat.source_type_i64 { 9 }
		flat.source_type_isize { 10 }
		flat.source_type_u8 { 11 }
		flat.source_type_u16 { 12 }
		flat.source_type_u32 { 13 }
		flat.source_type_u64 { 14 }
		flat.source_type_uint, flat.source_type_usize { 15 }
		flat.source_type_rune { 22 }
		else { -1 }
	}
}

fn fastc_is_numeric_expression_type(typ u32) bool {
	return fastc_is_integer_literal_expression_type(typ) || typ == fastc_type_float_literal
		|| typ == flat.source_type_f32 || typ == flat.source_type_f64 || fastc_is_integer_type(typ)
}

fn fastc_is_integer_expression_type(typ u32) bool {
	return fastc_is_integer_literal_expression_type(typ) || fastc_is_integer_type(typ)
}

fn fastc_is_integer_literal_expression_type(typ u32) bool {
	return typ == fastc_type_integer_literal || typ == fastc_type_negative_integer_literal
}

fn fastc_types_share_lowering_representation(actual u32, expected u32) bool {
	if actual == expected { return true }
	if actual == fastc_type_integer_literal { return fastc_is_integer_type(expected) }
	if actual == fastc_type_negative_integer_literal {
		return fastc_is_integer_type(expected) && !fastc_is_unsigned_integer_type(expected)
	}
	if actual == fastc_type_float_literal {
		return expected in [flat.source_type_f32, flat.source_type_f64]
	}
	if actual == flat.source_type_nil { return fastc_is_pointer_type(expected) }
	return false
}

fn fastc_selfhost_types_share_lowering_representation(actual_id u32, expected_id u32) bool {
	if (actual_id == flat.source_type_byteptr && expected_id == fastc_type_u8_pointer) || (expected_id == flat.source_type_byteptr && actual_id == fastc_type_u8_pointer) || (actual_id == flat.source_type_charptr && expected_id == fastc_type_char_pointer) || (expected_id == flat.source_type_charptr && actual_id == fastc_type_char_pointer) {
		return true
	}
	if actual_id == flat.source_type_composition_two(.concat, expected_id, flat.source_type_id('*')) || expected_id == flat.source_type_composition_two(.concat, actual_id, flat.source_type_id('*')) {
		return true
	}
	actual_base_id := fastc_trim_pointer_type(actual_id)
	expected_base_id := fastc_trim_pointer_type(expected_id)
	if (actual_base_id == fastc_type_array && flat.source_type_query(expected_base_id).has(.c_array_prefix)) || (expected_base_id == fastc_type_array && flat.source_type_query(actual_base_id).has(.c_array_prefix)) || (actual_base_id == fastc_type_map && flat.source_type_query(expected_base_id).has(.c_map_prefix)) || (expected_base_id == fastc_type_map && flat.source_type_query(actual_base_id).has(.c_map_prefix)) {
		return true
	}
	if actual_id == fastc_type_negative_integer_literal && fastc_is_unsigned_integer_type(expected_id) {
		return false
	}
	if fastc_is_integer_expression_type(actual_id) && fastc_is_integer_type(expected_id) {
		return true
	}
	if expected_id == flat.source_type_voidptr && fastc_is_pointer_type(actual_id) {
		return true
	}
	if actual_id == flat.source_type_voidptr && fastc_is_pointer_type(expected_id) {
		return true
	}
	return false
}

fn (g &Parser) selfhost_types_share_lowering_representation(actual u32, expected u32) bool {
	if fastc_selfhost_types_share_lowering_representation(actual, expected) {
		return true
	}
	if fastc_is_numeric_expression_type(actual) && g.declared_kinds[g.semantic_type_key(expected)] == .alias_ {
		return true
	}
	if fastc_is_numeric_expression_type(expected) && g.declared_kinds[g.semantic_type_key(actual)] == .alias_ {
		return true
	}
	return false
}

fn fastc_is_pointer_type(typ u32) bool {
	return flat.source_type_query(typ).has(.c_pointer_suffix) || typ == flat.source_type_voidptr
		|| typ == fastc_type_byteptr || typ == fastc_type_charptr
}

fn fastc_array_element_type(typ_id u32) ?u32 {
	base := fastc_trim_pointer_type(typ_id)
	syntax := flat.source_type_syntax(base)
	query := flat.source_type_query(base)
	if query.has(.c_fixed_array_prefix) && syntax.fastc_array_marker() {
		if element := fastc_fixed_array_element_type(base) { return element }
	}
	if query.has(.c_array_prefix) && syntax.c_array_payload() != 0 {
		return fastc_decode_ptr_element_type_id(syntax.c_array_payload())
	}
	if query.has(.c_fixed_array_prefix) && syntax.c_fixed_array_payload() != 0 {
		if element := fastc_fixed_array_element_type(base) { return element }
		return fastc_decode_ptr_element_type_id(syntax.c_fixed_array_payload())
	}
	return none
}

// fastc_decode_ptr_element_type reverses the `*`→`_ptr` composite-name encoding for an
// array/channel element, including the spaces in tagged C types:
// `Stream_ptr` → `Stream*`, `struct_iovec` → `struct iovec`.
fn fastc_decode_ptr_element_type(element string) string {
	return flat.source_type_name(fastc_decode_ptr_element_type_id(flat.source_type_id(element)))
}

fn fastc_decode_ptr_element_type_id(element u32) u32 {
	mut name := element
	mut pointers := 0
	for flat.source_type_syntax(name).c_ptr_name_suffix() {
		name = flat.source_type_syntax(name).c_ptr_name_base()
		pointers++
	}
	name = flat.source_type_syntax(name).c_tagged_type()
	for _ in 0 .. pointers {
		name = flat.source_type_composition_two(.concat, name, flat.source_type_id('*'))
	}
	return name
}

// fixed_array_pointee_element_type is the element of a fixed array reached through a
// pointer (a `mut x [N]T` parameter, `FixedArray_N_FASTC_ARRAY_OF_T*`), which
// render_array_access_expression indexes with `->data`: the array's element, without
// that `*`, so inference and rendering agree. Raw storage (a struct field or `__global`,
// `T* values[N]`) is not reached through a pointer: there the `*` is its element's (`[N]&T`).
fn (g &Parser) fixed_array_pointee_element_type(typ_id u32, base_tokens []FastcExpressionToken) ?u32 {
	if !flat.source_type_query(typ_id).has(.c_pointer_suffix) {
		return none
	}
	pointee := flat.source_type_syntax(typ_id).c_pointer_base()
	_ := fastc_fixed_array_length_id(g.underlying_alias_type(pointee)) or { return none }
	if flat.source_type_query(typ_id).has(.c_fixed_array_prefix) && g.fixed_array_uses_raw_storage(base_tokens) {
		return none
	}
	return g.array_element_type(pointee)
}

fn (g &Parser) array_element_type(typ_id u32) ?u32 {
	if element_type := fastc_array_element_type(typ_id) {
		return element_type
	}
	layout_id := fastc_trim_pointer_type(typ_id)
	layout_type := flat.source_type_name(layout_id)
	if fields := g.struct_fields[layout_type] {
		return fields.type_id('__fastc_element_type') or { none }
	}
	// An alias of an array type (`type Gf = [16]i64`) has the aliased array's elements.
	aliased := g.underlying_alias_type(layout_id)
	if aliased != layout_id {
		return g.array_element_type(aliased)
	}
	return none
}

@[inline]
fn fastc_is_integer_type(typ u32) bool {
	if fastc_integer_type_mask.enabled { return fastc_integer_type_mask.contains(typ) }
	return typ == flat.source_type_char || typ == flat.source_type_i8 || typ == flat.source_type_i16 || typ == flat.source_type_i32 || typ == flat.source_type_i64 || typ == flat.source_type_int || typ == flat.source_type_isize || typ == flat.source_type_rune || typ == flat.source_type_u8 || typ == flat.source_type_u16 || typ == flat.source_type_u32 || typ == flat.source_type_u64 || typ == fastc_type_unsigned_int || typ == flat.source_type_uint || typ == flat.source_type_usize
}

@[inline]
fn fastc_is_unsigned_integer_type(typ u32) bool {
	if fastc_unsigned_integer_type_mask.enabled {
		return fastc_unsigned_integer_type_mask.contains(typ)
	}
	return typ == flat.source_type_u8 || typ == flat.source_type_u16 || typ == flat.source_type_u32 || typ == flat.source_type_u64 || typ == fastc_type_unsigned_int || typ == flat.source_type_uint || typ == flat.source_type_usize
}

@[inline]
fn fastc_is_wide_unsigned_integer_type(typ u32) bool {
	if fastc_wide_unsigned_integer_type_mask.enabled {
		return fastc_wide_unsigned_integer_type_mask.contains(typ)
	}
	return typ == flat.source_type_u32 || typ == flat.source_type_u64 || typ == fastc_type_unsigned_int || typ == flat.source_type_uint || typ == flat.source_type_usize
}

@[inline]
fn fastc_is_signed_integer_type(typ u32) bool {
	if fastc_signed_integer_type_mask.enabled {
		return fastc_signed_integer_type_mask.contains(typ)
	}
	return typ == flat.source_type_char || typ == flat.source_type_i8 || typ == flat.source_type_i16 || typ == flat.source_type_i32 || typ == flat.source_type_i64 || typ == flat.source_type_int || typ == flat.source_type_isize || typ == flat.source_type_rune
}

fn fastc_nondecimal_literal_is_type_sensitive(literal string) bool {
	clean := fastc_number_without_separators(literal)
	return fastc_clean_nondecimal_literal_is_type_sensitive(clean)
}

fn fastc_clean_nondecimal_literal_is_type_sensitive(clean string) bool {
	if clean.len <= 2 || clean[0] != `0` {
		return false
	}
	mut first_digit := 2
	for first_digit < clean.len && clean[first_digit] == `0` {
		first_digit++
	}
	digits_len := clean.len - first_digit
	if clean[1] in [`x`, `X`] {
		if digits_len > 8 {
			return true
		}
		return digits_len == 8 && ((clean[first_digit] >= `8` && clean[first_digit] <= `9`) || (clean[first_digit] >= `a` && clean[first_digit] <= `f`) || (clean[first_digit] >= `A` && clean[first_digit] <= `F`))
	}
	if clean[1] in [`b`, `B`] {
		return digits_len >= 32
	}
	if clean[1] in [`o`, `O`] {
		return digits_len > 11 || (digits_len == 11 && clean[first_digit] >= `2`)
	}
	return false
}

fn fastc_decimal_literal_is_type_sensitive(literal string) bool {
	clean := fastc_number_without_separators(literal)
	return fastc_clean_decimal_literal_is_type_sensitive(clean)
}

fn fastc_clean_decimal_literal_is_type_sensitive(clean string) bool {
	if clean.len == 0 || clean.contains_any('.eE') {
		return false
	}
	for digit in clean {
		if !digit.is_digit() {
			return false
		}
	}
	mut first_digit := 0
	for first_digit < clean.len && clean[first_digit] == `0` {
		first_digit++
	}
	digits_len := clean.len - first_digit
	int_max_literal := '2147483647'
	if digits_len != int_max_literal.len {
		return digits_len > int_max_literal.len
	}
	for i in 0 .. digits_len {
		if clean[first_digit + i] != int_max_literal[i] {
			return clean[first_digit + i] > int_max_literal[i]
		}
	}
	return false
}

@[inline]
fn fastc_number_without_separators(literal string) string {
	if literal.index_u8(`_`) < 0 {
		return literal
	}
	return literal.replace('_', '')
}

fn fastc_c_number(literal string) !string {
	clean := fastc_number_without_separators(literal)
	if fastc_clean_decimal_literal_is_type_sensitive(clean) {
		// C assigns oversized decimal tokens a wider type before any surrounding
		// operation. Reject them until the direct parser can preserve V inference.
		return error('fastc parser does not support oversized decimal literal expressions')
	}
	if fastc_clean_nondecimal_literal_is_type_sensitive(clean) {
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

// fastc_number_is_float reports whether a rendered number literal is a floating point
// one; the `e` of a hexadecimal literal is a digit, not an exponent.
fn fastc_number_is_float(number string) bool {
	if number.len > 1 && number[0] == `0` && number[1] in [`x`, `X`] {
		return false
	}
	return number.contains_any('.eE')
}

// fastc_shift_operand_number types an integer literal that is the left operand of `<<`.
// A V integer literal is an `int`, the platform's width; a bare C literal is a 32-bit
// `int`, so `1 << 51` would overflow.
fn fastc_shift_operand_number(number string) string {
	if number.ends_with('ULL') || fastc_number_is_float(number) {
		return number
	}
	return '((${fastc_output_c_type('int')})${number})'
}

fn fastc_c_selfhost_number(literal string) string {
	clean := fastc_number_without_separators(literal)
	if clean.len > 2 && clean[0] == `0` && clean[1] in [`o`, `O`] {
		return '0${clean[2..]}${if fastc_clean_nondecimal_literal_is_type_sensitive(clean) {
			'ULL'
		} else {
			''
		}}'
	}
	if fastc_clean_decimal_literal_is_type_sensitive(clean) || fastc_clean_nondecimal_literal_is_type_sensitive(clean) {
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
