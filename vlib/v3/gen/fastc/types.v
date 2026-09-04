module fastc

import v3.token

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
			.eq, .ne, .lt, .gt, .le, .ge, .and, .logical_or, .plus, .minus, .mul, .div, .mod, .pipe, .amp, .xor, .left_shift, .right_shift, .right_shift_unsigned {
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

fn (g &Parser) infer_boolean_binary_expression_type(tokens []FastcExpressionToken, start int, end int, operator_index int) !string {
	operator := tokens[operator_index].tok
	mut left_type := fastc_normalize_inferred_type(g.infer_expression_type_range(tokens, start, operator_index)!)
	if operator in [.key_is, .not_is] {
		right_type := g.type_from_expression_tokens(tokens[operator_index + 1..end]) or {
			return g.unsupported('type test `${operator.str()}` with an undeclared target type')
		}
		if g.semantic_type_key(right_type) !in g.declared_types && !right_type.starts_with('Array_') && !right_type.starts_with('Map_') && fastc_primitive_c_type(right_type) == none {
			// Composite variants (`x is []string` / `x is map[…]`) and primitive variants
			// (`x is u64`) are not declared types but do get generated `__v_typeid_` tags,
			// so allow them.
			return g.unsupported('type test `${operator.str()}` with undeclared type `${right_type}`')
		}
		return 'bool'
	}
	mut right_type := fastc_normalize_inferred_type(g.infer_expression_type_range(tokens, operator_index + 1, end)!)
	if operator in [.and, .logical_or] {
		return 'bool'
	}
	if operator in [.key_in, .not_in] {
		array_end := if operator_index + 1 < end && tokens[end - 1].tok == .not {
			end - 1
		} else {
			end
		}
		if array_end - operator_index - 1 >= 2 && tokens[operator_index + 1].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
			return 'bool'
		}
		if fastc_trim_pointer_suffix(right_type).starts_with('Map_') {
			_, _ := g.map_key_value_types(right_type) or {
				return g.unsupported('membership `${operator.str()}` with unverifiable map type `${right_type}`')
			}
		} else if fastc_trim_pointer_suffix(right_type) == 'string' {
		} else {
			_ := g.array_element_type(right_type) or {
				return g.unsupported('membership `${operator.str()}` in non-collection type `${right_type}`')
			}
		}
		return 'bool'
	}
	if operator in [.eq, .ne] && ((fastc_is_pointer_type(left_type) && fastc_expression_range_is_zero(tokens, operator_index + 1, end)) || (fastc_is_pointer_type(right_type) && fastc_expression_range_is_zero(tokens, start, operator_index))) {
		return 'bool'
	}
	if operator in [.eq, .ne] {
		if g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_type == '' && fastc_expression_range_is_enum_shorthand(tokens, operator_index + 1, end) {
			right_type = left_type
		} else if g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_type == '' && fastc_expression_range_is_enum_shorthand(tokens, start, operator_index) {
			left_type = right_type
		}
		if !fastc_is_pointer_type(left_type) && !fastc_is_pointer_type(right_type) {
			left_layout := fastc_trim_pointer_suffix(g.underlying_alias_type(left_type))
			right_layout := fastc_trim_pointer_suffix(g.underlying_alias_type(right_type))
			left_key := g.semantic_type_key(left_layout)
			if left_layout == right_layout && left_layout !in ['Option', '_option', '_result'] && left_key in g.declared_kinds && g.declared_kinds[left_key] == .struct_ && !g.struct_equality_is_supported(left_type, []string{}) {
				return g.unsupported('struct equality for `${left_type}` with unsupported fields')
			}
		}
	}
	if g.selfhost && operator in [.eq, .ne] && ((fastc_is_pointer_type(left_type) && right_type == '') || (fastc_is_pointer_type(right_type) && left_type == '')) {
		return 'bool'
	}
	if g.selfhost && ((left_type == '' && fastc_expression_range_is_c_qualified_name(tokens, start, operator_index)) || (right_type == '' && fastc_expression_range_is_c_qualified_name(tokens, operator_index + 1, end))) {
		return 'bool'
	}
	return 'bool'
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
fn fastc_typeof_generic_field_type(tokens []FastcExpressionToken, start int, end int) ?string {
	if end - start < 8 || tokens[start].lit != 'typeof' || tokens[start + 1].tok != .lsbr {
		return none
	}
	close_bracket := fastc_matching_delimiter_before(tokens, start + 1, end, .lsbr, .rsbr) or { return none }
	if close_bracket + 4 != end - 1 || tokens[close_bracket + 1].tok != .lpar || tokens[close_bracket + 2].tok != .rpar || tokens[close_bracket + 3].tok != .dot || tokens[end - 1].tok != .name {
		return none
	}
	return match tokens[end - 1].lit {
		'idx' { 'int' }
		'name' { 'string' }
		else { none }
	}
}

// nonlocal_name_type infers the type of a bare name that is not a local from
// the constant and global tables. File generation parsers see fixed tables, so
// the answer is memoized per name there; declaration initializer parsers
// extend those tables while parsing, so they always recompute.
fn (g &Parser) nonlocal_name_type(name string) string {
	if !g.declaration_initializer_mode {
		if cached := g.nonlocal_name_type_memo[name] {
			return cached
		}
	}
	typ := if constant_type := g.constant_types[fastc_constant_key(g.module_name, name)] {
		constant_type
	} else if constant_type := g.constant_types[fastc_constant_key('builtin', name)] {
		constant_type
	} else if fastc_constant_key(g.module_name, name) in g.constants {
		'integer literal'
	} else if fastc_constant_key('builtin', name) in g.constants {
		'integer literal'
	} else if global_type := g.global_types[fastc_global_key(g.module_name, name)] {
		global_type
	} else if global_type := g.global_types[fastc_global_key('builtin', name)] {
		global_type
	} else if global_type := g.resolve_cross_module_global_type(name) {
		global_type
	} else if g.selfhost {
		'int'
	} else {
		''
	}
	if !g.declaration_initializer_mode {
		mut w := unsafe { &Parser(g) }
		w.nonlocal_name_type_memo[name] = typ
	}
	return typ
}

fn (g &Parser) infer_expression_type(tokens []FastcExpressionToken) !string {
	return g.infer_expression_type_range(tokens, 0, tokens.len)
}

// infer_expression_type_range memoizes the inferred type of a multi-token
// subrange for the duration of the current top-level expression: the
// lowerings ask about the same operands and receivers repeatedly.
fn (g &Parser) infer_expression_type_range(tokens []FastcExpressionToken, expression_start int, expression_end int) !string {
	if expression_end - expression_start < 2 {
		return g.infer_expression_type_range_impl(tokens, expression_start, expression_end)!
	}
	// Flow-sensitive member smartcasts are installed while an `&&` expression is rendered.
	// A type cached before the left operand narrows its subject is stale for the operands to
	// its right, so infer those ranges directly while an active smartcast affects them.
	if g.expression_uses_member_smartcast(tokens[expression_start..expression_end]) {
		return g.infer_expression_type_range_impl(tokens, expression_start, expression_end)!
	}
	memo_key := fastc_comparison_memo_key(tokens[expression_start..expression_end], 2)
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

fn (g &Parser) infer_expression_type_range_impl(tokens []FastcExpressionToken, expression_start int, expression_end int) !string {
	if expression_start >= expression_end {
		return ''
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
		return ''
	}
	if end - start == 1 {
		item := tokens[start]
		if item.typ != '' {
			return item.typ
		}
		return match item.tok {
			.name {
				if smartcast := g.member_smartcasts[item.lit] {
					// A bare local narrowed by `x is T` reads as the concrete variant.
					smartcast.typ
				} else if local := g.locals[item.lit] {
					local.typ
				} else {
					g.nonlocal_name_type(item.lit)
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
	// `X as T` yields the target type `T` (so `(x as T).field` resolves).
	if as_index := fastc_bare_as_cast_index(tokens, start, end) {
		if as_index + 1 < end {
			if target := g.type_from_expression_tokens(tokens[as_index + 1..end]) {
				return fastc_normalize_inferred_type(target)
			}
		}
	}
	if end - start >= 4 && tokens[start].tok == .key_sizeof && tokens[start + 1].tok == .lpar && tokens[end - 1].tok == .rpar {
		return 'int'
	}
	if end - start >= 8 && tokens[start].lit == 'typeof' {
		if reflection_type := fastc_typeof_generic_field_type(tokens, start, end) {
			return reflection_type
		}
	}
	if end - start >= 9 && tokens[start].tok in [.amp, .and] {
		if c_field_type := g.infer_c_pointer_cast_member_type(tokens, start, end) {
			return c_field_type
		}
	}
	if tokens[start].tok == .not {
		_ = g.infer_expression_type_range(tokens, start + 1, end)!
		return 'bool'
	}
	if operator_index := fastc_lowest_precedence_operator_index(tokens, start, end) {
		return g.infer_boolean_binary_expression_type(tokens, start, end, operator_index)!
	}
	if end - start == 2 && tokens[start].tok == .dot && tokens[start + 1].typ != '' {
		return tokens[start + 1].typ
	}
	if end - start == 5 && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name && tokens[start + 3].tok == .dot && tokens[start + 4].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if enum_type_key := g.underlying_enum_type_key(type_key) {
				return fastc_c_declared_type_name(enum_type_key)
			}
		}
	}
	if end - start >= 5 && tokens[start].tok == .lsbr && tokens[start + 1].tok == .rsbr && tokens[start + 2].tok == .name && tokens[start + 3].tok == .lpar && tokens[end - 1].tok == .rpar {
		mut element_type := tokens[start + 2].lit
		if primitive := fastc_primitive_c_type(element_type) {
			element_type = primitive
		}
		return fastc_array_c_type(element_type)
	}
	if end - start == 3 && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if type_key in g.declared_types {
				return fastc_c_declared_type_name(type_key)
			}
		}
		if type_key := g.resolve_declared_type_key(tokens[start].lit) {
			if enum_type_key := g.underlying_enum_type_key(type_key) {
				return fastc_c_declared_type_name(enum_type_key)
			}
		}
		if imported_module := g.imports[tokens[start].lit] {
			if constant_type := g.constant_types[fastc_constant_key(imported_module, tokens[start + 2].lit)] {
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
		if array_end < end {
			return fastc_fixed_array_type('${items.len}', element_type)
		}
		return fastc_array_c_type(element_type)
	}
	if start + 1 < end && tokens[start].tok == .name && tokens[start + 1].tok == .lcbr {
		if close := fastc_matching_delimiter_before(tokens, start + 1, end, .lcbr, .rcbr) {
			if close == end - 1 {
				if type_key := g.resolve_declared_type_key(tokens[start].lit) {
					return fastc_c_declared_type_name(type_key)
				}
			}
		}
	}
	if start + 3 < end && tokens[start].tok == .name && tokens[start + 1].tok == .dot && tokens[start + 2].tok == .name && tokens[start + 3].tok == .lcbr {
		// A module-qualified struct literal `mod.Type{...}`.
		if imported_module := g.imports[tokens[start].lit] {
			if close := fastc_matching_delimiter_before(tokens, start + 3, end, .lcbr, .rcbr) {
				if close == end - 1 {
					type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
					if type_key in g.declared_types {
						return fastc_c_declared_type_name(type_key)
					}
				}
			}
		}
	}
	mut call_name_index := start
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
				if signature.return_type != '' {
					return signature.return_type
				}
				if call_name_index == start {
					if local := g.locals[name] {
						if local.fn_return_type != '' {
							// Calling a function-pointer parameter (`f(x)`).
							return local.fn_return_type
						}
						// Calling a value whose type is a `type X = fn (...) Ret` alias.
						if ret := g.fn_alias_return_types[fastc_trim_pointer_suffix(local.typ)] {
							return ret
						}
					}
					if primitive := fastc_primitive_c_type(name) {
						return primitive
					}
					if type_key := g.resolve_declared_type_key(name) {
						return fastc_c_declared_type_name(type_key)
					}
				}
				if call_name_index == start + 2 && tokens[start].lit in g.imports {
					module_name := g.imports[tokens[start].lit]
					type_key := fastc_type_key(module_name, name)
					if type_key in g.declared_types {
						return fastc_c_declared_type_name(type_key)
					}
				}
				if call_name_index == start + 2 && tokens[start].lit == 'C' {
					if '#Cstruct#${name}' in g.declared_types {
						return 'struct ${name}'
					}
					if name.len > 0 && name[0].is_capital() {
						return name
					}
				}
				return ''
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
		if receiver_type == '' {
			continue
		}
		if tokens[i].lit in ['map', 'filter', 'any', 'all', 'count'] && fastc_trim_pointer_suffix(fastc_normalize_inferred_type(g.underlying_alias_type(receiver_type))).starts_with('Array_') {
			// The magic closure methods change the result type: `filter` keeps the array,
			// `count` is an int, `any`/`all` are bools, and `map` yields an array of the
			// closure's element type (inferred with `it` bound to the element).
			method := tokens[i].lit
			if method == 'filter' {
				return fastc_trim_pointer_suffix(fastc_normalize_inferred_type(g.underlying_alias_type(receiver_type)))
			}
			if method == 'count' {
				return 'int'
			}
			if method in ['any', 'all'] {
				return 'bool'
			}
			array_type := fastc_normalize_inferred_type(g.underlying_alias_type(receiver_type))
			if element_type := g.array_element_type(array_type) {
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
				closure_type := g.infer_expression_type_range(tokens, closure_start, close) or {
					''
				}
				if had_it {
					w.locals[it_name] = saved_it
				} else {
					w.locals.delete(it_name)
				}
				if closure_type != '' {
					return fastc_array_c_type(fastc_normalize_inferred_type(closure_type))
				}
			}
		}
		if tokens[i].lit == 'wait' && receiver_type.starts_with(fastc_thread_type_prefix) {
			value_type := g.thread_value_types[receiver_type] or { '' }
			if value_type == '' {
				return 'void'
			}
			return value_type
		}
		if tokens[i].lit == 'wait' && receiver_type.trim_right('*').starts_with('Array_') {
			// `[]thread T`.wait() joins every thread and returns their `[]T` results.
			element := g.array_element_type(receiver_type) or { '' }
			if element.starts_with(fastc_thread_type_prefix) {
				value_type := g.thread_value_types[element] or { '' }
				if value_type != '' {
					return fastc_array_c_type(value_type)
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
				origin := if local.smartcast_origin_type != '' {
					local.smartcast_origin_type
				} else {
					local.typ
				}
				if origin != '' && origin != receiver_type {
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
			if function_alias := g.functions[field.typ] {
				return function_alias.return_type
			}
		}
		if tokens[i].lit == 'str' && g.can_generate_default_struct_str(receiver_type) {
			return 'string'
		}
		if tokens[i].lit == 'str' && g.declared_kinds[g.semantic_type_key(receiver_type)] == .enum_ {
			return 'string'
		}
		if tokens[i].lit == 'str' && receiver_type.trim_right('*').starts_with('Array_') {
			return 'string'
		}
		if tokens[i].lit == 'type_name' && g.is_boxed_type(fastc_normalize_inferred_type(receiver_type)) {
			return 'string'
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
			receiver_type := g.infer_expression_type_range(tokens, start, end - 2)!
			if field := g.struct_field_metadata(receiver_type, tokens[end - 1].lit) {
				return field.typ
			}
			// A field shared by every variant of a boxed sum type (`node.name` where
			// `node` is `ast.TypeDecl`) resolves to that common field's type.
			if common := g.sumtype_common_field_type(receiver_type, tokens[end - 1].lit) {
				return common
			}
		}
	}
	if tokens[start].tok in [.plus, .minus] {
		operand_type := g.infer_expression_type_range(tokens, start + 1, end)!
		if tokens[start].tok == .minus && operand_type == 'integer literal' {
			return 'negative integer literal'
		}
		return operand_type
	}
	if tokens[start].tok in [.amp, .and] {
		operand_type := g.infer_expression_type_range(tokens, start + 1, end)!
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
		return if operand_type == '' {
			'voidptr'
		} else {
			operand_type + '*'.repeat(pointer_count)
		}
	}
	if tokens[start].tok == .mul {
		operand_type := g.infer_expression_type_range(tokens, start + 1, end)!
		// Dereferencing removes exactly one pointer level. trim_right('*') collapses
		// `T**` all the way to `T`, which makes values such as
		// `*(&&ast.File(ptr))` look by-value and adds a bogus address-of later.
		return if operand_type.ends_with('*') {
			operand_type[..operand_type.len - 1]
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
		return if value_type == 'void' { '' } else { value_type }
	}
	if tokens[end - 1].tok in [.inc, .dec] {
		operand_type := g.infer_expression_type_range(tokens, start, end - 1)!
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
			base_type := g.infer_expression_type_range(tokens, start, open_index)!
			base_layout := fastc_trim_pointer_suffix(base_type)
			if fastc_expression_tokens_contain(tokens[open_index + 1..end - 1], .dotdot) {
				// Slicing a fixed array yields a dynamic array of its element type.
				if base_layout.starts_with('FixedArray_') {
					if element := g.array_element_type(base_layout) {
						return fastc_array_c_type(fastc_normalize_inferred_type(element))
					}
				}
				return base_type
			}
			if base_layout.starts_with('Map_') {
				_, value_type := g.map_key_value_types(base_type) or { return '' }
				return value_type
			}
			if base_layout == 'string' {
				return 'u8'
			}
			if element_type := g.array_element_type(base_type) {
				return element_type
			}
			if base_type.ends_with('*') {
				return base_type[..base_type.len - 1]
			}
		}
	}
	if member_type := g.infer_member_access_type(tokens, start, end) {
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
			if left_element := g.indexed_array_operand_type(tokens, start, i, left_type) {
				left_type = left_element
			}
			if right_element := g.indexed_array_operand_type(tokens, i + 1, end, right_type) {
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
			mut left_type := g.infer_expression_type_range(tokens, start, i)!
			mut right_type := g.infer_expression_type_range(tokens, i + 1, end)!
			if left_element := g.indexed_array_operand_type(tokens, start, i, left_type) {
				left_type = left_element
			}
			if right_element := g.indexed_array_operand_type(tokens, i + 1, end, right_type) {
				right_type = right_element
			}
			if tokens[i].tok == .plus && fastc_trim_pointer_suffix(g.underlying_alias_type(left_type)) == 'string' && fastc_trim_pointer_suffix(g.underlying_alias_type(right_type)) == 'string' {
				return 'string'
			}
			if g.selfhost && tokens[i].tok == .plus && ((left_type == 'string' && right_type == '') || (right_type == 'string' && left_type == '')) {
				return 'string'
			}
			if g.selfhost && tokens[i].tok in [.plus, .minus] && fastc_is_pointer_type(left_type) && fastc_is_integer_expression_type(right_type) {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && left_type == right_type && g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_type == '' {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_type == '' {
				return right_type
			}
			if g.selfhost && fastc_is_integer_expression_type(left_type) && fastc_is_integer_expression_type(right_type) {
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
			if g.selfhost && fastc_is_numeric_expression_type(left_type) && g.declared_kinds[g.semantic_type_key(right_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(right_type) && g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return right_type
			}
			if g.selfhost && left_type == right_type && g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
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

fn (g &Parser) infer_c_pointer_cast_member_type(tokens []FastcExpressionToken, start int, end int) ?string {
	if end - start < 9 || tokens[start].tok !in [.amp, .and] || tokens[start + 1].tok != .name || tokens[start + 1].lit != 'C' || tokens[start + 2].tok != .dot || tokens[start + 3].tok != .name || tokens[start + 4].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar_before(tokens, start + 4, end) or { return none }
	if close + 2 >= end || tokens[close + 1].tok != .dot || tokens[close + 2].tok != .name || close + 3 != end {
		return none
	}
	c_name := tokens[start + 3].lit
	c_type := if '#Cstruct#${c_name}' in g.declared_types { 'struct ${c_name}' } else { c_name }
	field_type := g.struct_member_type(c_type + '*', tokens[close + 2].lit)
	if field_type == '' {
		return none
	}
	return field_type
}

fn (g &Parser) indexed_array_operand_type(tokens []FastcExpressionToken, start int, end int, inferred_type string) ?string {
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
fn (g &Parser) sum_type_variant_list(sum_type string) []string {
	base := fastc_trim_pointer_suffix(sum_type)
	prefix := '${base}|'
	mut variants := []string{}
	for key, present in g.sum_type_variants {
		if present && key.starts_with(prefix) {
			variants << key[prefix.len..]
		}
	}
	return variants
}

// sum_type_leaf_variants returns the concrete (non-sum-type) variants of `sum_type`,
// recursively expanding any variant that is itself a sum type. A value whose static type is
// a sum type always carries the runtime tag of one of these leaf struct variants, so field
// dispatch and common-field detection must reason over the flattened set.
fn (g &Parser) sum_type_leaf_variants(sum_type string) []string {
	mut leaves := []string{}
	for variant in g.sum_type_variant_list(sum_type) {
		if fastc_trim_pointer_suffix(variant) in g.sum_types {
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
fn (g &Parser) sumtype_common_field_type(sum_type string, field_name string) ?string {
	base := fastc_trim_pointer_suffix(sum_type)
	if base !in g.sum_types {
		return none
	}
	variants := g.sum_type_leaf_variants(base)
	if variants.len == 0 {
		return none
	}
	mut common_type := ''
	for variant in variants {
		field := g.struct_field_metadata(variant, field_name) or { return none }
		if field.storage_path.len > 0 {
			return none
		}
		if common_type == '' {
			common_type = field.typ
		} else if field.typ != common_type {
			return none
		}
	}
	if common_type == '' {
		return none
	}
	return common_type
}

// resolve_cross_module_global_type returns the type of a `__global` declared in ANY module,
// looked up by its bare name — `__global`s are truly global, so a reference from a different
// module than the declaring one (`global_table` in v.ast, used from v.checker) still resolves.
fn (g &Parser) resolve_cross_module_global_type(name string) ?string {
	suffix := '.${name}'
	for key, global_type in g.global_types {
		if key == name || key.ends_with(suffix) {
			return global_type
		}
	}
	return none
}

fn (g &Parser) infer_member_access_type(tokens []FastcExpressionToken, start int, end int) ?string {
	if end - start < 3 || tokens[start].tok != .name {
		return none
	}
	mut current_type := ''
	if local := g.locals[tokens[start].lit] {
		current_type = local.typ
	} else if global_type := g.global_types[fastc_global_key(g.module_name, tokens[start].lit)] {
		current_type = global_type
	} else if constant_type := g.constant_types[fastc_constant_key(g.module_name, tokens[start].lit)] {
		current_type = constant_type
	} else if constant_type := g.constant_types[fastc_constant_key('builtin', tokens[start].lit)] {
		current_type = constant_type
	} else if global_type := g.resolve_cross_module_global_type(tokens[start].lit) {
		current_type = global_type
	} else {
		return none
	}
	mut member_path := tokens[start].lit
	// A smart-cast on the bare subject itself (`x is T`, or an option local narrowed by
	// `x != none`) reshapes it before any field access, exactly as render_member_receiver
	// does when producing the matching source.
	if smartcast := g.member_smartcasts[member_path] {
		current_type = smartcast.typ
	}
	mut index := start + 1
	for index < end {
		if tokens[index].tok == .lsbr {
			close := fastc_matching_delimiter_before(tokens, index, end, .lsbr, .rsbr) or {
				return none
			}
			current_layout := fastc_trim_pointer_suffix(current_type)
			if fastc_expression_tokens_contain_range(tokens, index + 1, close, .dotdot) {
				if current_layout != 'string' && g.array_element_type(current_type) == none {
					return none
				}
				// A fixed-array slice becomes a dynamic array of its element type.
				if current_layout.starts_with('FixedArray_') {
					if element := g.array_element_type(current_layout) {
						current_type = fastc_array_c_type(fastc_normalize_inferred_type(element))
					}
				}
			} else if current_layout == 'string' {
				current_type = 'u8'
			} else if current_layout.starts_with('Map_') {
				_, value_type := g.map_key_value_types(current_type) or { return none }
				current_type = value_type
			} else if element_type := g.array_element_type(current_type) {
				current_type = element_type
			} else if current_type.ends_with('*') {
				current_type = current_type[..current_type.len - 1]
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
		if current_type == '' {
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

@[inline]
fn fastc_trim_pointer_suffix(typ string) string {
	if typ.len == 0 || typ[typ.len - 1] != `*` {
		return typ
	}
	return typ.trim_right('*')
}

fn (g &Parser) semantic_type_key(c_type string) string {
	base := fastc_trim_pointer_suffix(c_type)
	if key := g.declared_type_c_names[base] {
		return key
	}
	return base
}

fn (g &Parser) underlying_alias_type(c_type string) string {
	return fastc_underlying_alias_type(c_type, g.alias_base_types)
}

fn fastc_underlying_alias_type(c_type string, alias_base_types map[string]string) string {
	// Fast path: most types are not aliases, so resolve the first hop before
	// allocating the cycle-guard map. underlying_alias_type is called for
	// nearly every inferred expression type; the unconditional map allocation
	// showed up as a hot allocation site under -prealloc.
	base0 := fastc_trim_pointer_suffix(c_type)
	first := alias_base_types[base0] or { return c_type }
	mut resolved := first + c_type[base0.len..]
	mut seen := map[string]bool{}
	seen[base0] = true
	for {
		base := fastc_trim_pointer_suffix(resolved)
		if base in seen {
			return resolved
		}
		alias_base := alias_base_types[base] or { return resolved }
		seen[base] = true
		resolved = alias_base + resolved[base.len..]
	}
	return resolved
}

fn fastc_number_expression_type(literal string) string {
	clean := literal.replace('_', '')
	if clean.contains('.') || (!(clean.starts_with('0x') || clean.starts_with('0X')) && clean.contains_any('eE')) {
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
	if tokens[number_index].tok != .number || fastc_number_expression_type(tokens[number_index].lit) != 'integer literal' {
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
	if fastc_is_integer_literal_expression_type(left) && fastc_is_integer_literal_expression_type(right) {
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
	return fastc_is_integer_literal_expression_type(typ) || typ in ['float literal', 'f32', 'f64'] || fastc_is_integer_type(typ)
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
	if (actual == 'byteptr' && expected == 'u8*') || (expected == 'byteptr' && actual == 'u8*') || (actual == 'charptr' && expected == 'char*') || (expected == 'charptr' && actual == 'char*') {
		return true
	}
	if actual == expected + '*' || expected == actual + '*' {
		return true
	}
	actual_base := fastc_trim_pointer_suffix(actual)
	expected_base := fastc_trim_pointer_suffix(expected)
	if (actual_base == 'array' && expected_base.starts_with('Array_')) || (expected_base == 'array' && actual_base.starts_with('Array_')) || (actual_base == 'map' && expected_base.starts_with('Map_')) || (expected_base == 'map' && actual_base.starts_with('Map_')) {
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
	if fastc_is_numeric_expression_type(actual) && g.declared_kinds[g.semantic_type_key(expected)] == .alias_ {
		return true
	}
	if fastc_is_numeric_expression_type(expected) && g.declared_kinds[g.semantic_type_key(actual)] == .alias_ {
		return true
	}
	return false
}

fn fastc_is_pointer_type(typ string) bool {
	return typ.ends_with('*') || typ in ['voidptr', 'byteptr', 'charptr']
}

fn fastc_array_element_type(typ string) ?string {
	// A `FixedArray_<len>_FASTC_ARRAY_OF_<elem>` name carries the raw element type after the
	// marker, and that element may itself be a pointer (`[N]&T` -> `..._FASTC_ARRAY_OF_T*`).
	// Extract it before any trailing-`*` trim mistakes the element's pointer for a suffix on
	// the whole array type.
	if typ.starts_with('FixedArray_') && typ.contains('_FASTC_ARRAY_OF_') {
		if element_type := fastc_fixed_array_element_type(typ) {
			return element_type
		}
	}
	base := fastc_trim_pointer_suffix(typ)
	if base.starts_with('Array_') && base.len > 'Array_'.len {
		return fastc_decode_ptr_element_type(base['Array_'.len..])
	}
	if base.starts_with('FixedArray_') && base.len > 'FixedArray_'.len {
		if element_type := fastc_fixed_array_element_type(base) {
			return element_type
		}
		return fastc_decode_ptr_element_type(base['FixedArray_'.len..])
	}
	return none
}

// fastc_decode_ptr_element_type reverses the `*`→`_ptr` composite-name encoding for an
// array/channel element: `Stream_ptr` (not a real C type) → `Stream*`, `char_ptr` →
// `char*`. Non-pointer element names are returned unchanged.
fn fastc_decode_ptr_element_type(element string) string {
	mut name := element
	mut pointers := 0
	for name.ends_with('_ptr') {
		name = name[..name.len - '_ptr'.len]
		pointers++
	}
	if pointers == 0 {
		return element
	}
	return name + '*'.repeat(pointers)
}

fn (g &Parser) array_element_type(typ string) ?string {
	if element_type := fastc_array_element_type(typ) {
		return element_type
	}
	layout_type := fastc_trim_pointer_suffix(typ)
	if fields := g.struct_fields[layout_type] {
		return fields['__fastc_element_type'] or { none }
	}
	return none
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
