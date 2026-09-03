module fastc

import strings
import v3.token

// fastc_sort_compare_key hashes a `.sort()` comparison expression (FNV-1a) into a stable,
// C-identifier-safe suffix so equal comparisons on the same element type share one
// generated comparator function.
fn fastc_sort_compare_key(condition string) string {
	mut h := u64(14695981039346656037)
	for c in condition {
		h = (h ^ u64(c)) * u64(1099511628211)
	}
	return h.hex()
}

// current_call_has_one_argument peeks from the scanner position immediately after the
// current `(`. It disambiguates a lowercase tagged C type conversion from a same-named
// C function: conversions have one operand, while calls like `C.stat(path, &out)` do not.
fn (g &Parser) current_call_has_one_argument() bool {
	mut look := g.s
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	mut saw_argument := false
	for {
		tok := look.scan()
		if tok == .eof {
			return false
		}
		if tok == .rpar && parens == 0 && brackets == 0 && braces == 0 {
			return saw_argument
		}
		if tok == .comma && parens == 0 && brackets == 0 && braces == 0 {
			return false
		}
		match tok {
			.lpar { parens++ }
			.rpar { parens-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			.lcbr { braces++ }
			.rcbr { braces-- }
			.semicolon {}
			else {
				saw_argument = true
			}
		}
	}
	return false
}

// fastc_struct_field_value_token_start returns the index of the first token of the
// current struct-literal field VALUE: the token just after the last field `:` at bracket
// depth 0. Returns 0 when there is no such colon (e.g. a positional literal).
fn fastc_struct_field_value_token_start(tokens []FastcExpressionToken) int {
	mut depth := 0
	for i := tokens.len - 1; i >= 0; i-- {
		match tokens[i].tok {
			.rpar, .rsbr, .rcbr {
				depth++
			}
			.lpar, .lsbr, .lcbr {
				depth--
			}
			.colon {
				// depth < 0 means an unmatched opener sits between here and the end — a
				// grouping `(` around the field value (`f: (x or {…}).m()`); the field value
				// still starts right after this `:`.
				if depth <= 0 {
					return i + 1
				}
			}
			else {}
		}
	}
	return 0
}

// fastc_or_operand_token_start returns the first token of the trailing operand that an `or`
// binds to. Array elements start after their nearest comma/opener. In an arithmetic or
// comparison expression, the option-producing operand starts after the nearest binary
// operator. Other parenthesized/braced cases are handled by the existing grouped/call paths.
fn fastc_or_operand_token_start(tokens []FastcExpressionToken) int {
	mut depth := 0
	mut nearest_comma := -1
	for i := tokens.len - 1; i >= 0; i-- {
		match tokens[i].tok {
			.rpar, .rsbr, .rcbr {
				depth++
			}
			.lsbr {
				if depth == 0 {
					return if nearest_comma >= 0 { nearest_comma + 1 } else { i + 1 }
				}
				depth--
			}
			.lpar {
				if depth == 0 {
					// An unbalanced `(` at depth 0 opens the operand's enclosing group. Even a
					// primitive cast (`u8(parse(s) or {…})`) scopes the `or` to the value INSIDE the
					// cast, so return the position after `(` and let the cast prefix be re-emitted.
					return if nearest_comma >= 0 { nearest_comma + 1 } else { i + 1 }
				}
				depth--
			}
			.lcbr {
				if depth == 0 {
					return 0
				}
				depth--
			}
			.comma {
				if depth == 0 && nearest_comma < 0 {
					nearest_comma = i
				}
			}
			.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift, .right_shift, .right_shift_unsigned, .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or {
				if depth == 0 {
					// A leading `&`, `*`, `+` or `-` belongs to the option-producing operand
					// (`&m[k] or { nil }`); it is not the binary separator before it.
					if tokens[i].tok in [.amp, .and, .mul, .plus, .minus] && fastc_token_is_prefix_operator(tokens, i) {
						continue
					}
					return i + 1
				}
			}
			else {}
		}
	}
	return 0
}

fn fastc_trailing_not_marks_fixed_array_literal(tokens []FastcExpressionToken) bool {
	if tokens.len < 3 || tokens.last().tok != .not || tokens[tokens.len - 2].tok != .rsbr {
		return false
	}
	mut depth := 0
	mut open := -1
	for i := tokens.len - 2; i >= 0; i-- {
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
	if open == 0 {
		return true
	}
	return open > 0 && tokens[open - 1].tok in [.lpar, .comma, .assign, .decl_assign, .key_in,
		.not_in, .plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .and, .logical_or]
}

fn (mut g Parser) read_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix_mode('', stops, false, false)
}

fn (mut g Parser) read_expression_with_prefix(prefix string, stops []token.Token) !string {
	return g.read_expression_with_prefix_mode(prefix, stops, false, false)
}

fn (mut g Parser) read_condition_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix_mode('', stops, false, true)
}

// condition_brace_opens_struct_literal reports whether a `{` reached while reading an
// if/for condition opens a struct literal (`if pos == Pos{}`) rather than the block that
// terminates the condition. The distinction is the token preceding the type reference: a
// value operator (`==`, `(`, …) precedes a struct literal, whereas `is`/`as` precede the
// block that follows a smart-cast target (`if x is Ident {`).
fn (g &Parser) condition_brace_opens_struct_literal(tokens []FastcExpressionToken, previous_token token.Token, previous_lit string) bool {
	if previous_token != .name {
		return false
	}
	mut type_resolves := false
	mut before_index := tokens.len - 2
	if tokens.len >= 3 && tokens[tokens.len - 2].tok == .dot && tokens[tokens.len - 3].tok == .name {
		module_alias := tokens[tokens.len - 3].lit
		if imported_module := g.imports[module_alias] {
			type_key := fastc_type_key(imported_module, previous_lit)
			type_resolves = type_key in g.declared_types
		}
		before_index = tokens.len - 4
	} else if previous_lit in g.locals {
		// A local variable whose name shadows a type (e.g. the loop variable `array`,
		// which also names the builtin `array` type) is a value, so `array {` opens the
		// loop/if block, not a struct literal.
		return false
	} else if tokens.len >= 2 && tokens[tokens.len - 2].tok == .dot {
		// A dotted name that is not a module-qualified type is an enum shorthand
		// (`sym.kind == .array {`) or member access, not a bare struct-literal type.
		return false
	} else {
		type_resolves = g.resolve_declared_type_key(previous_lit) != none
	}
	if !type_resolves {
		return false
	}
	if before_index >= 0 && tokens[before_index].tok in [.key_is, .not_is, .key_as] {
		return false
	}
	return true
}

fn (mut g Parser) read_statement_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix_mode('', stops, true, false)
}

fn (mut g Parser) read_statement_expression_with_prefix(prefix string, stops []token.Token) !string {
	return g.read_expression_with_prefix_mode(prefix, stops, true, false)
}

fn (mut g Parser) read_expression_with_prefix_mode(prefix string, stops []token.Token, allow_mutation_statement bool, allow_declaration_guard bool) !string {
	if g.expression_depth == 0 {
		g.last_option_value_type = ''
	}
	g.expression_depth++
	if g.expression_depth == 1 {
		// Memoized comparison renders and inferred types are only valid while
		// the expression's token buffers and locals are live and unchanged; a
		// new top-level expression starts a fresh generation.
		if g.comparison_memo.len > 0 {
			g.comparison_memo.clear()
		}
		if g.type_memo.len > 0 {
			g.type_memo.clear()
		}
	}
	// `ok && x.field` where `ok := x is Variant && …`: pre-register the bool's implied member
	// smart-casts so the streamed `x.field` reads the narrowed variant, then drop them after.
	mut bool_implied := []string{}
	if g.selfhost && g.expression_depth == 1 && prefix == '' {
		bool_implied = g.apply_bool_implication_smartcasts(stops)
	}
	defer {
		for implied_name in bool_implied {
			g.member_smartcasts.delete(implied_name)
		}
		g.expression_depth--
	}
	return g.read_expression_with_prefix_mode_impl(prefix, stops, allow_mutation_statement, allow_declaration_guard)
}

struct FastcHigherOrderExpression {
	found bool
	lit   string
}

fn (mut g Parser) lower_higher_order_expression(mut result strings.Builder, mut expression_tokens []FastcExpressionToken) !FastcHigherOrderExpression {
	mut lookahead := g.s
	if lookahead.scan() != .name {
		return FastcHigherOrderExpression{}
	}
	higher_order_method := lookahead.lit
	if higher_order_method in ['map', 'filter', 'any', 'all', 'count']
		&& lookahead.scan() == .lpar {
		receiver_start := fastc_method_receiver_start(expression_tokens, expression_tokens.len)
		receiver_tokens := expression_tokens[receiver_start..].clone()
		// Array aliases (`strings.Builder = []u8`) share the array layout and support
		// the same compiler-magic methods as plain arrays.
		receiver_type := fastc_normalize_inferred_type(g.underlying_alias_type(g.infer_expression_type(receiver_tokens) or {
			''
		}))
		// Array-literal receivers (`[.a, .b].map(...)`) do not round-trip
		// through the receiver renderer yet; leave them to the normal path.
		if receiver_type.starts_with('Array_') {
			fastc_register_composite_type(receiver_type, mut g.composite_types)
			element_type := g.array_element_type(receiver_type) or { '' }
			// Method calls in the receiver are only resolved by a post-pass,
			// so the live buffer holds an unresolved form; re-render through the
			// resolving path and drop the unresolved form from the buffer (its
			// whole content when the receiver is the entire expression, else its
			// raw suffix length). Array literals need the element type as expected
			// context to resolve `.enum` shorthand elements.
			mut receiver_source := ''
			if receiver_tokens[0].tok == .lsbr && receiver_tokens.last().tok == .rsbr {
				items := fastc_expression_list_items(receiver_tokens, 1, receiver_tokens.len - 1) or {
					return g.unsupported('`.${higher_order_method}` array-literal receiver')
				}
				norm_element := fastc_normalize_inferred_type(element_type)
				previous_expected := g.expected_expression_type
				g.expected_expression_type = element_type
				mut rendered_items := []string{cap: items.len}
				for item in items {
					rendered_items << g.render_call_argument_expression(item, element_type) or {
						g.expected_expression_type = previous_expected
						return g.unsupported('`.${higher_order_method}` array-literal element')
					}
				}
				g.expected_expression_type = previous_expected
				receiver_source = '((${receiver_type})builtin__new_array_from_c_array(${items.len}, ${items.len}, sizeof(${norm_element}), (${norm_element}[]){${rendered_items.join(',')}}))'
				if receiver_start == 0 {
					result.str()
				} else {
					prefix := g.render_raw_expression_tokens(expression_tokens[..receiver_start]) or {
						return g.unsupported('`.${higher_order_method}` expression prefix')
					}
					result.go_back(result.len)
					result.write_string(prefix)
				}
			} else {
				resolved := g.render_method_receiver_expression(receiver_tokens) or {
					return g.unsupported('`.${higher_order_method}` receiver')
				}
				receiver_source = resolved.source
				if receiver_start == 0 {
					result.str()
				} else {
					prefix := g.render_raw_expression_tokens(expression_tokens[..receiver_start]) or {
						return g.unsupported('`.${higher_order_method}` expression prefix')
					}
					result.go_back(result.len)
					result.write_string(prefix)
				}
			}
			g.next() // `.`
			g.next() // method name
			g.next() // `(`
			mut it_name := 'it'
			if g.tok == .pipe {
				// An explicit closure header (`arr.filter(|item| item.ok)`) names the
				// element in place of the implicit `it` local.
				g.next()
				if g.tok != .name {
					return g.unsupported('`.${higher_order_method}` closure parameter')
				}
				it_name = g.lit
				g.next()
				if g.tok != .pipe {
					return g.unsupported('`.${higher_order_method}` closure `|param|` header')
				}
				g.next()
			}
			had_it := it_name in g.locals
			saved_it := g.locals[it_name] or { FastcLocal{} }
			it_c_name := fastc_c_identifier(it_name)
			g.type_memo.clear()
			g.locals[it_name] = FastcLocal{
				typ: element_type
				c_name: it_c_name
			}
			// The closure type is independent of the surrounding assignment type.
			saved_closure_expected := g.expected_expression_type
			g.expected_expression_type = ''
			mut closure := g.read_expression([token.Token.rpar])!
			g.expected_expression_type = saved_closure_expected
			mut closure_type := g.last_expression_type
			// A bare function is applied to each element (`items.map(convert)`).
			if g.last_expression.len == 1 && g.last_expression[0].tok == .name
				&& g.last_expression[0].lit != it_name {
				function_key := g.unqualified_function_key(g.last_expression[0].lit)
				if signature := g.functions[function_key] {
					closure = '${closure}(${it_c_name})'
					closure_type = signature.return_type
				}
			}
			if had_it {
				g.type_memo.clear()
				g.locals[it_name] = saved_it
			} else {
				g.locals.delete(it_name)
			}
			g.next() // `)`
			if closure_type == '' {
				return g.unsupported('`.${higher_order_method}` closure type')
			}
			src := g.temporary_name('collection')
			dst := g.temporary_name('mapped')
			idx := g.temporary_name('index')
			elem := g.temporary_name('element')
			mut lowered := ''
			// A mutable array local has pointer type; iterate over a value copy of its
			// header while retaining the shared data buffer.
			collection_type := receiver_type.trim_right('*')
			collection_source := if receiver_type.ends_with('*') {
				'*(${receiver_source})'
			} else {
				receiver_source
			}
			mut result_type := collection_type
			if higher_order_method == 'map' {
				result_type = fastc_array_c_type(closure_type)
				fastc_register_composite_type(result_type, mut g.composite_types)
				lowered = '({ ${collection_type} ${src} = (${collection_source}); ${result_type} ${dst} = (${result_type})builtin____new_array(0, ${src}.len, sizeof(${closure_type})); for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; ${closure_type} ${elem} = (${closure}); builtin__array_push((array *)&${dst}, &${elem}); } ${dst}; })'
			} else if higher_order_method == 'filter' {
				lowered = '({ ${collection_type} ${src} = (${collection_source}); ${collection_type} ${dst} = (${collection_type})builtin____new_array(0, ${src}.len, sizeof(${element_type})); for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; if (${closure}) { builtin__array_push((array *)&${dst}, &${it_c_name}); } } ${dst}; })'
			} else if higher_order_method == 'count' {
				result_type = 'int'
				lowered = '({ ${collection_type} ${src} = (${collection_source}); int ${dst} = 0; for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; if (${closure}) { ${dst}++; } } ${dst}; })'
			} else {
				result_type = 'bool'
				initial := if higher_order_method == 'all' { 'true' } else { 'false' }
				condition := if higher_order_method == 'all' {
					'!(${closure})'
				} else {
					closure
				}
				matched := if higher_order_method == 'all' { 'false' } else { 'true' }
				lowered = '({ ${collection_type} ${src} = (${collection_source}); bool ${dst} = ${initial}; for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; if (${condition}) { ${dst} = ${matched}; break; } } ${dst}; })'
			}
			result.write_string(lowered)
			expression_tokens = expression_tokens[..receiver_start].clone()
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: lowered
				typ: result_type
			}
			return FastcHigherOrderExpression{
				found: true
				lit: lowered
			}
		}
	}
	// `arr.sort(a.x < b.x)`: generate a comparator function (`a`/`b` are the two
	// elements) keyed by element type + comparison, and lower to sort_with_compare.
	// `.sort()` (no argument) keeps its existing builtin lowering (the `!= .rpar`).
	if higher_order_method == 'sort' && lookahead.scan() == .lpar && lookahead.scan() != .rpar {
		receiver_start := fastc_method_receiver_start(expression_tokens, expression_tokens.len)
		receiver_tokens := expression_tokens[receiver_start..].clone()
		receiver_type := g.infer_expression_type(receiver_tokens) or { '' }
		if receiver_type.starts_with('Array_') {
			element_type := g.array_element_type(receiver_type) or {
				return g.unsupported('`.sort` element type')
			}
			norm_element := fastc_normalize_inferred_type(element_type)
			resolved := g.render_method_receiver_expression(receiver_tokens) or {
				return g.unsupported('`.sort` receiver')
			}
			receiver_source := resolved.source
			if receiver_start == 0 {
				result.str()
			} else {
				prefix := g.render_raw_expression_tokens(expression_tokens[..receiver_start]) or {
					return g.unsupported('`.sort` expression prefix')
				}
				result.go_back(result.len)
				result.write_string(prefix)
			}
			g.next() // `.`
			g.next() // `sort`
			g.next() // `(`
			had_a := 'a' in g.locals
			saved_a := g.locals['a'] or { FastcLocal{} }
			had_b := 'b' in g.locals
			saved_b := g.locals['b'] or { FastcLocal{} }
			g.type_memo.clear()
			g.locals['a'] = FastcLocal{
				typ: element_type
			}
			g.type_memo.clear()
			g.locals['b'] = FastcLocal{
				typ: element_type
			}
			condition := g.read_expression([token.Token.rpar])!
			if had_a {
				g.type_memo.clear()
				g.locals['a'] = saved_a
			} else {
				g.locals.delete('a')
			}
			if had_b {
				g.type_memo.clear()
				g.locals['b'] = saved_b
			} else {
				g.locals.delete('b')
			}
			g.next() // `)`
			if condition.trim_space() == '' {
				return g.unsupported('`.sort` comparison')
			}
			cmp_name := 'v_fastc_sort_${fastc_c_identifier(norm_element)}_${fastc_sort_compare_key(condition)}'
			if cmp_name !in g.spawn_helpers {
				// The same rendered comparison in two blocks with swapped element
				// bindings yields the -1 / +1 / 0 ordering without re-rendering it.
				g.spawn_helpers[cmp_name] = 'static int ${cmp_name}(void *__v_fastc_a, void *__v_fastc_b) { { ${norm_element} a = *(${norm_element} *)__v_fastc_a; ${norm_element} b = *(${norm_element} *)__v_fastc_b; if (${condition}) { return -1; } } { ${norm_element} a = *(${norm_element} *)__v_fastc_b; ${norm_element} b = *(${norm_element} *)__v_fastc_a; if (${condition}) { return 1; } } return 0; }'
			}
			lowered := '({ builtin__array_sort_with_compare((array *)&(${receiver_source}), ${cmp_name}); })'
			result.write_string(lowered)
			expression_tokens = expression_tokens[..receiver_start].clone()
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: lowered
				typ: 'void'
				is_statement: true
			}
			return FastcHigherOrderExpression{
				found: true
				lit: lowered
			}
		}
	}
	return FastcHigherOrderExpression{}
}

struct FastcLoweredOrExpression {
	complete       bool
	source         string
	previous_token token.Token
	previous_lit   string
	paren_depth    int
}

fn (mut g Parser) lower_or_expression(mut result strings.Builder, mut expression_tokens []FastcExpressionToken, input_paren_depth int, brace_depth int, struct_depths []int, struct_paren_depths []int, struct_field_value_start int, expected_struct_field_type string, saved_expected_expression_type string) !FastcLoweredOrExpression {
	mut previous_token := token.Token.name
	mut previous_lit := ''
	mut paren_depth := input_paren_depth
	or_expression_is_statement := g.expression_tokens_are_statement(expression_tokens)
	or_return_types := g.multi_return_types_for_expression(expression_tokens)
	mut wrapper_parens := 0
	for wrapper_parens < expression_tokens.len && expression_tokens[wrapper_parens].tok == .lpar {
		wrapper_parens++
	}
	raw_option_buffer := fastc_take_string(mut result)
	mut option_expression := raw_option_buffer.trim_space()
	mut value_type := g.expected_expression_type
	mut option_tokens := expression_tokens.clone()
	mut assignment_prefix := ''
	mut assignment_suffix := ''
	mut scoped_operand_prefix := ''
	mut scoped_or_operand := false
	mut assignment_depth := 0
	// An `or` inside a struct-literal field VALUE (`Type{ f: expr or {...} }`) applies
	// only to that value, not the whole literal-so-far. Scope the option to the field
	// value and keep the `(Type){.f = ` prefix (rebuilt via assignment_prefix).
	mut in_struct_field := false
	mut struct_field_prefix_tokens := []FastcExpressionToken{}
	field_extra_parens := if struct_paren_depths.len > 0 {
		paren_depth - struct_paren_depths.last()
	} else {
		-1
	}
	if struct_depths.len > 0 && brace_depth == struct_depths.last() && field_extra_parens >= 0 && struct_field_value_start > 0 && struct_field_value_start <= raw_option_buffer.len {
		value_token_start := fastc_struct_field_value_token_start(expression_tokens)
		// When the field value is wrapped in grouping parens (`f: (x or {…}).m()`),
		// paren_depth is above the struct's baseline; require the extra depth to be
		// exactly that many leading `(` of the value, so a call/index arg
		// (`f: g(x or {…})`) is left to the operand-scoping path instead.
		mut field_leading_open := 0
		if value_token_start > 0 && value_token_start < expression_tokens.len {
			for value_token_start + field_leading_open < expression_tokens.len && expression_tokens[value_token_start + field_leading_open].tok == .lpar {
				field_leading_open++
			}
		}
		if value_token_start > 0 && value_token_start < expression_tokens.len && (field_extra_parens == 0 || field_leading_open >= field_extra_parens) {
			in_struct_field = true
			assignment_prefix = raw_option_buffer[..struct_field_value_start]
			option_expression = raw_option_buffer[struct_field_value_start..].trim_space()
			option_tokens = expression_tokens[value_token_start..].clone()
			struct_field_prefix_tokens = expression_tokens[..value_token_start].clone()
			if expected_struct_field_type != '' {
				value_type = expected_struct_field_type
			}
		}
	}
	for i, assignment_token in expression_tokens {
		if in_struct_field {
			// The enclosing assignment must not re-scope an `or` that already belongs
			// to a struct field value.
			break
		}
		if assignment_token.tok in [.lpar, .lsbr, .lcbr] {
			assignment_depth++
		} else if assignment_token.tok in [.rpar, .rsbr, .rcbr] {
			assignment_depth--
		} else if assignment_depth == 0 && assignment_token.tok.is_assignment() && i > 0 && i + 1 < expression_tokens.len {
			left_tokens := expression_tokens[..i].clone()
			mut rhs_paren_depth := 0
			for rhs_token in expression_tokens[i + 1..] {
				match rhs_token.tok {
					.lpar { rhs_paren_depth++ }
					.rpar { rhs_paren_depth-- }
					else {}
				}
			}
			if rhs_paren_depth != 0 {
				// A parenthesized RHS operand is handled by operand scoping below.
				continue
			}
			if assignment_token.tok == .assign {
				if map_wrap := g.render_map_index_assignment_wrapping(left_tokens) {
					assignment_prefix = map_wrap.prefix
					assignment_suffix = map_wrap.suffix
					value_type = map_wrap.value_type
					option_tokens = expression_tokens[i + 1..].clone()
					option_expression = g.render_call_argument_expression(option_tokens, value_type) or { '' }
					wrapper_parens = 0
					break
				}
			}
			left_type := g.infer_expression_type(left_tokens) or { '' }
			if left_type != '' {
				left_source := g.render_membership_candidate(left_tokens, left_type) or {
					''
				}
				if left_source != '' {
					assignment_prefix = '${left_source}${assignment_token.tok.str()}'
					value_type = left_type
					option_tokens = expression_tokens[i + 1..].clone()
					option_expression = g.render_call_argument_expression(option_tokens, left_type) or { '' }
					wrapper_parens = 0
				}
			}
			break
		}
	}
	// An `or` inside an array literal / index / call-argument list
	// (`[a, expr or {..}]`, `f(expr or {..})`): scope the option to the current operand
	// (after the last `,` / `[` / `(`), keeping the enclosing prefix tokens so the
	// collection/call re-renders correctly from tokens. Reuses the struct-field rebuild
	// path via `in_struct_field` + `struct_field_prefix_tokens`.
	if !in_struct_field && assignment_prefix == '' {
		operand_start := fastc_or_operand_token_start(expression_tokens)
		if operand_start > 0 && operand_start < expression_tokens.len {
			scoped_or_operand = true
			in_struct_field = true
			struct_field_prefix_tokens = expression_tokens[..operand_start].clone()
			option_tokens = expression_tokens[operand_start..].clone()
			option_expression = g.render_call_argument_expression(option_tokens, value_type) or {
				option_expression
			}
			separator := expression_tokens[operand_start - 1].tok
			// A grouping `(` after an operator is not rebuilt as a call/index prefix.
			mut grouping_prefix := false
			if separator == .lpar {
				is_call_paren := operand_start >= 2 && expression_tokens[operand_start - 2].tok in [
					.name,
					.rpar,
					.rsbr,
				]
				mut before_content := false
				for prefix_token in expression_tokens[..operand_start - 1] {
					if prefix_token.tok != .lpar {
						before_content = true
						break
					}
				}
				grouping_prefix = !is_call_paren && before_content
			}
			if separator !in [.lpar, .lsbr, .comma] || grouping_prefix {
				scoped_operand_prefix = g.render_raw_expression_tokens(struct_field_prefix_tokens) or {
					''
				}
				wrapper_parens = 0
			}
		}
	}
	// A grouping-parenthesized operand (`(x or {…}).method()`) renders with an
	// unmatched leading `(` in the option buffer; that `(` is re-emitted as a
	// `wrapper_parens`, so strip it from the option expression to keep the generated
	// `Option t = (x)` balanced. option_tokens is left intact so the method-call /
	// missing-call detection below still sees the whole operand.
	mut grouped_operand := false
	mut grouped_paren_count := 0
	mut grouped_value_tokens := []FastcExpressionToken{}
	// Handle a grouping-parenthesized operand for the plain case (`(x or {…}).m()`)
	// and the struct-field case (`Type{ f: (x or {…}).m() }`), but not a plain
	// assignment RHS (which has its own membership rebuild).
	if in_struct_field || assignment_prefix == '' {
		mut operand_open := 0
		for operand_open < option_tokens.len && option_tokens[operand_open].tok == .lpar {
			operand_open++
		}
		mut operand_balance := 0
		for balance_item in option_tokens {
			match balance_item.tok {
				.lpar, .lsbr, .lcbr {
					operand_balance++
				}
				.rpar, .rsbr, .rcbr {
					operand_balance--
				}
				else {}
			}
		}
		if operand_balance < operand_open {
			operand_open = if operand_balance > 0 { operand_balance } else { 0 }
		}
		if operand_open > 0 && operand_open < option_tokens.len {
			grouped_operand = true
			grouped_paren_count = operand_open
			stripped_tokens := option_tokens[operand_open..].clone()
			grouped_value_tokens = stripped_tokens.clone()
			option_expression = g.render_call_argument_expression(stripped_tokens, value_type) or { option_expression }
		}
	}
	if expression_tokens.len >= 3 && expression_tokens[0].tok == .name && expression_tokens[1].tok == .lpar && fastc_primitive_c_type(expression_tokens[0].lit) != none {
		option_tokens = expression_tokens[2..].clone()
		option_expression = g.render_raw_expression_tokens(option_tokens) or {
			option_expression
		}
	}
	// For a grouped operand the option value type must come from the balanced tokens
	// (`make_box(x)` → Box); option_tokens still carries the unmatched leading `(`,
	// which would defeat the lookup.
	mut option_value_type := if grouped_operand {
		g.option_value_type_for_expression(grouped_value_tokens)
	} else {
		g.option_value_type_for_expression(option_tokens)
	}
	if member_source := g.render_member_receiver(option_tokens) {
		// Re-render pure member chains so pointer fields at any depth use `->`
		// inside the Option temporary (`outer.inner.value or { ... }`).
		option_expression = member_source
	}
	if map_lookup := g.render_map_lookup_option_expression(option_tokens) {
		option_expression = map_lookup.source
		option_value_type = map_lookup.typ
	} else if slice_option := g.render_slice_option_expression(option_tokens) {
		option_expression = slice_option.source
		option_value_type = slice_option.typ
	} else if array_lookup := g.render_array_lookup_option_expression(option_tokens) {
		option_expression = array_lookup.source
		option_value_type = array_lookup.typ
	} else if explicit_generic := g.render_explicit_generic_call_expression(option_tokens) {
		option_expression = explicit_generic.source
		option_value_type = g.option_value_type_for_expression(option_tokens)
	} else if static_call := g.render_static_call_expression(option_tokens, option_expression) {
		option_expression = static_call.source
		option_value_type = g.option_value_type_for_expression(option_tokens)
	} else if call := g.render_missing_call_arguments(option_tokens) {
		// Rebuild a complete free-function call before resolving methods nested in
		// its arguments. This supplies contextual types for array/map literals.
		option_expression = call.source
		option_value_type = g.option_value_type_for_expression(option_tokens)
	} else if method_call := g.render_method_call_expression(option_tokens, option_expression) {
		option_expression = method_call.source
		option_value_type = g.option_value_type_for_expression(option_tokens)
	}
	// The `or` result is the option payload, not the enclosing expression's type.
	if g.selfhost && option_value_type != '' && option_value_type != value_type {
		value_type = option_value_type
	}
	if pointer_members := g.render_pointer_member_access_expression(option_tokens, option_expression) {
		option_expression = pointer_members.source
	}
	outer_cast := assignment_prefix == '' && scoped_operand_prefix == '' && !scoped_or_operand && option_tokens.len != expression_tokens.len
	if expression_tokens.len >= 2 && expression_tokens[0].tok == .name && expression_tokens[1].tok == .lpar {
		value_type = fastc_primitive_c_type(expression_tokens[0].lit) or { value_type }
		cast_prefix := '((${fastc_output_c_type(value_type)})('
		if option_expression.starts_with(cast_prefix) {
			option_expression = option_expression[cast_prefix.len..]
		}
	}
	g.next()
	g.expect(.lcbr)!
	temporary := g.temporary_name('option')
	if g.or_block_has_statements() {
		previous_capture := g.capturing_defer
		previous_lines := g.captured_defer_lines.clone()
		previous_err := g.locals['err'] or { FastcLocal{} }
		had_err := 'err' in g.locals
		g.type_memo.clear()
		g.locals['err'] = FastcLocal{
			typ: 'IError'
		}
		g.capturing_defer = true
		g.captured_defer_lines = []string{}
		// A trailing bare VALUE (`or { ...; User{} }`) is the block's fallback: let
		// parse_block_body capture it into `or_value_captured` instead of rejecting
		// it as a value-only statement.
		previous_or_capture := g.or_value_capture
		previous_or_captured := g.or_value_captured
		previous_or_expected := g.or_value_expected_type
		g.or_value_capture = true
		g.or_value_captured = ''
		g.or_value_expected_type = option_value_type
		_ = g.parse_block_body()!
		fallback_value := g.or_value_captured
		g.or_value_capture = previous_or_capture
		g.or_value_captured = previous_or_captured
		g.or_value_expected_type = previous_or_expected
		block_lines := g.captured_defer_lines.clone()
		g.capturing_defer = previous_capture
		g.captured_defer_lines = previous_lines.clone()
		if had_err {
			g.type_memo.clear()
			g.locals['err'] = previous_err
		} else {
			g.locals.delete('err')
		}
		// A primitive cast around the or (`int(f() or {...})`): the result type is the
		// cast type, and its closing `)` must be consumed (mirrors the single-value
		// path's `outer_cast` handling below).
		cast_type := if outer_cast && expression_tokens.len >= 2 && expression_tokens[0].tok == .name && expression_tokens[1].tok == .lpar {
			fastc_primitive_c_type(expression_tokens[0].lit) or { '' }
		} else {
			''
		}
		complex_value_type := if cast_type != '' {
			cast_type
		} else if option_value_type == '' {
			'void'
		} else {
			option_value_type
		}
		complex_payload_type := if option_value_type != '' {
			option_value_type
		} else {
			complex_value_type
		}
		complex_unwrapped := if complex_payload_type == 'void' {
			'0'
		} else {
			'*((${complex_payload_type} *)${temporary}.data)'
		}
		complex_success := if cast_type != '' && complex_payload_type != cast_type {
			'((${fastc_output_c_type(cast_type)})(${complex_unwrapped}))'
		} else {
			complex_unwrapped
		}
		if outer_cast && paren_depth > 0 && g.tok == .rpar {
			paren_depth--
			g.next()
		}
		result.go_back(result.len)
		or_expr_body := if fallback_value != '' && complex_value_type != 'void' {
			// The or-block ends in a fallback VALUE: run the leading statements and
			// use the value on failure, the unwrapped option value on success.
			or_result := g.temporary_name('or_result')
			'({ Option ${temporary} = (${option_expression}); ${complex_value_type} ${or_result}; if (${temporary}.state) { IError err = ${temporary}.err; ${block_lines.join(' ')} ${or_result} = (${fallback_value}); } else { ${or_result} = ${complex_success}; } ${or_result}; })'
		} else {
			// The or-block only runs statements (it diverges): run them on failure,
			// then use the unwrapped value.
			'({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { IError err = ${temporary}.err; ${block_lines.join(' ')} } ${complex_success}; })'
		}
		result.write_string(assignment_prefix)
		result.write_string(scoped_operand_prefix)
		result.write_string(or_expr_body)
		result.write_string(assignment_suffix)
		if in_struct_field {
			expression_tokens = struct_field_prefix_tokens.clone()
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: temporary
				source: or_expr_body
				typ: complex_value_type
				is_statement: or_expression_is_statement
			}
		} else {
			expression_tokens = [
				FastcExpressionToken{
					tok: .name
					lit: temporary
					// Carry the rendered `({ ... })` so a trailing `.method()` binds to it.
					source: or_expr_body
					typ: complex_value_type
					is_statement: or_expression_is_statement
				},
			]
			if complex_value_type == 'void' {
				expression_tokens << FastcExpressionToken{
					tok: .assign
					lit: '='
				}
			}
			if assignment_prefix != '' && assignment_suffix == '' {
				expression_tokens << FastcExpressionToken{
					tok: .assign
					lit: '='
				}
			}
		}
		previous_token = .name
		previous_lit = temporary
		g.last_expression_type = complex_value_type
		g.last_expression = expression_tokens
		g.last_multi_return_types = or_return_types.clone()
		if in_struct_field {
			// parse_block_body consumed the field separator; re-insert a `,` as both a
			// token (render_struct_literal_expression works off tokens) and buffer text
			// before the next field so the struct literal keeps rendering.
			if g.tok == .name {
				expression_tokens << FastcExpressionToken{
					tok: .comma
					lit: ','
				}
				result.write_string(', ')
				previous_token = .comma
			}
			return FastcLoweredOrExpression{ previous_token: previous_token, previous_lit: previous_lit, paren_depth: paren_depth }
		}
		// parse_block_body already skipped the statement separator after `}`, so an
		// immediate `.` here is a method on the or-result (`x or { ... }.method()`):
		// continue so it binds to the temporary (via its `source`). Otherwise the
		// expression is complete.
		if g.tok == .dot {
			return FastcLoweredOrExpression{ previous_token: previous_token, previous_lit: previous_lit, paren_depth: paren_depth }
		}
		// A trailing binary operator (`data.index(c) or { return … } + 1`) also binds
		// to the or-result: keep reading so it renders `or_expr_body + 1` rather than
		// orphaning `+ 1` as its own value-only statement. (The single-value path
		// already falls through to the shared read loop for this.)
		if g.tok in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift, .right_shift,
			.right_shift_unsigned, .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or] {
			return FastcLoweredOrExpression{ previous_token: previous_token, previous_lit: previous_lit, paren_depth: paren_depth }
		}
		g.expected_expression_type = saved_expected_expression_type
		return FastcLoweredOrExpression{ complete: true, source: fastc_take_trimmed(mut result) }
	}
	previous_err := g.locals['err'] or { FastcLocal{} }
	had_err := 'err' in g.locals
	g.type_memo.clear()
	g.locals['err'] = FastcLocal{
		typ: 'IError'
	}
	// A multiline final expression gets a scanner-inserted semicolon before
	// the block's `}`. Keep it out of the expression tokens so composite
	// literals retain their inferred type.
	// Supply the option payload type so bare `[]`/`{}` fallbacks are typed.
	previous_fallback_expected := g.expected_expression_type
	if g.selfhost && option_value_type != '' {
		g.expected_expression_type = option_value_type
	}
	mut fallback := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.expected_expression_type = previous_fallback_expected
	mut fallback_type := fastc_normalize_inferred_type(g.last_expression_type)
	if fallback == '' {
		fallback = '0'
	} else if fallback_type.starts_with('Map_') && fallback.contains('){}') {
		key_type, map_value_type := g.map_key_value_types(fallback_type) or {
			return g.unsupported('map fallback type `${fallback_type}`')
		}
		hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
		fallback = '(builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(map_value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
	}
	// A concrete variant fallback must be boxed to match the option payload.
	if g.selfhost && option_value_type != '' && fallback_type != ''
		&& fallback_type != option_value_type
		&& g.should_box_variant(option_value_type, fallback_type) {
		fallback = g.interface_value_expression(option_value_type, fallback_type, fallback)
		fallback_type = option_value_type
	}
	if had_err {
		g.type_memo.clear()
		g.locals['err'] = previous_err
	} else {
		g.locals.delete('err')
	}
	g.skip_semicolons()
	g.expect(.rcbr)!
	if fallback.contains('err') {
		fallback = '({ IError err = ${temporary}.err; ${fallback}; })'
	}
	if value_type == '' {
		value_type = if option_value_type != '' {
			option_value_type
		} else if fallback_type == '' {
			'void'
		} else {
			fallback_type
		}
	}
	if (g.tok == .dot || grouped_operand || scoped_or_operand) && option_value_type != '' {
		// A method immediately follows the or-block (`expr or { v }.method()`):
		// the unwrapped receiver's type is the option's value type, not the
		// surrounding expression's expected type (which describes the whole
		// `.method()` call's result). For a grouped operand (`(x or { v }).method()`)
		// the closing `)` sits between the block and the `.`, so `g.tok` is `.rpar`
		// here; the grouped expression's value is still the option value type (in the
		// non-method uses the expected type already equals it, so this is a no-op).
		value_type = option_value_type
	}
	if outer_cast && paren_depth > 0 && g.tok == .rpar {
		paren_depth--
		g.next()
	}
	// A grouped struct-field operand (`Type{ f: (x or {…}).m() }`) absorbed its
	// opening `(` into the field prefix, so consume the matching closing `)` here;
	// otherwise it would trail the or-result token as an unmatched `)` and break the
	// struct field's comma-splitting. wrapper_parens==0 in this path, so the plain
	// grouped case (which balances via wrapper_parens) is unaffected.
	if grouped_operand && in_struct_field {
		mut skipped := 0
		for skipped < grouped_paren_count && g.tok == .rpar {
			if paren_depth > 0 {
				paren_depth--
			}
			g.next()
			skipped++
		}
	}
	result.go_back(result.len)
	payload_type := if option_value_type != '' { option_value_type } else { value_type }
	unwrapped_value := if payload_type == 'void' {
		'0'
	} else {
		'*((${payload_type} *)${temporary}.data)'
	}
	mut success_value := if outer_cast && payload_type != value_type {
		'((${fastc_output_c_type(value_type)})(${unwrapped_value}))'
	} else {
		unwrapped_value
	}
	if fallback_type == 'Option' && option_value_type != '' {
		success_value = fastc_option_success_expression(option_value_type, unwrapped_value)
		value_type = 'Option'
	}
	or_expr_body := if fallback_type == 'IError' && g.return_type == 'Option' {
		// `return result_call() or { error(...) }`: the IError is a replacement
		// result failure, not a value of the result payload type.
		'({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { return (Option){.err = (${fallback}), .state = 1}; } ${success_value}; })'
	} else if value_type == 'void' || fallback_type in ['', 'void'] {
		'({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { ${fallback}; } ${success_value}; })'
	} else {
		'({ Option ${temporary} = (${option_expression}); ${temporary}.state ? (${fallback}) : ${success_value}; })'
	}
	if fallback_type == 'IError' && g.return_type == 'Option' && option_value_type != '' {
		value_type = option_value_type
	}
	result.write_string(assignment_prefix)
	result.write_string(scoped_operand_prefix)
	result.write_string('('.repeat(wrapper_parens))
	result.write_string(or_expr_body)
	result.write_string(assignment_suffix)
	if in_struct_field {
		expression_tokens = struct_field_prefix_tokens.clone()
		expression_tokens << FastcExpressionToken{
			tok: .name
			lit: temporary
			source: or_expr_body
			typ: value_type
			is_statement: or_expression_is_statement
		}
	} else {
		expression_tokens = []FastcExpressionToken{}
		for _ in 0 .. wrapper_parens {
			expression_tokens << FastcExpressionToken{
				tok: .lpar
				lit: '('
			}
		}
		expression_tokens << FastcExpressionToken{
			tok: .name
			lit: temporary
			// Carry the rendered `({ ... })` so a trailing `.method()`
			// (`expr or { v }.str()`) binds to it via render_method_receiver_expression.
			source: or_expr_body
			typ: value_type
			is_statement: or_expression_is_statement
		}
		if value_type == 'void' {
			expression_tokens << FastcExpressionToken{
				tok: .assign
				lit: '='
			}
		}
		if assignment_prefix != '' && assignment_suffix == '' {
			expression_tokens << FastcExpressionToken{
				tok: .assign
				lit: '='
			}
		}
	}
	previous_token = .name
	previous_lit = temporary
	g.last_multi_return_types = or_return_types.clone()
	return FastcLoweredOrExpression{ previous_token: previous_token, previous_lit: previous_lit, paren_depth: paren_depth }
}

fn (mut g Parser) read_expression_with_prefix_mode_impl(prefix string, stops []token.Token, allow_mutation_statement bool, allow_declaration_guard bool) !string {
	if g.selfhost && prefix == '' && g.tok == .lcbr && token.Token.lcbr !in stops {
		return g.read_inferred_map_literal()!
	}
	if g.selfhost && prefix == '' && g.tok == .arrow {
		return g.read_channel_receive(stops)!
	}
	if prefix == '' && g.tok == .key_if && !g.selfhost {
		return g.read_if_expression()!
	}
	if prefix == '' && g.tok == .key_match && !g.selfhost {
		return g.read_match_expression()!
	}
	if prefix == '' && g.tok == .dollar {
		return g.read_comptime_if_expression()!
	}
	if prefix == '' && g.tok == .key_spawn {
		return g.read_spawn_expression()!
	}
	if prefix == '' && g.tok == .name && g.lit == 'sql' && 'sql' !in g.locals {
		mut lookahead := g.s
		if lookahead.scan() == .name {
			// `x := sql db { select ... }` is lowered at the declaration level
			// (parse_declaration_after_name); a `sql` expression in any other position
			// has nowhere to bind the row-parsing loop.
			return g.unsupported('ORM `sql` select is only supported as `x := sql db { select ... }`')
		}
	}
	mut result := strings.new_builder(32)
	// Most expressions are a handful of tokens; start with room for them so
	// the token buffer is not regrown several times per expression.
	mut expression_tokens := []FastcExpressionToken{cap: 8}
	if prefix.len > 0 {
		result.write_string(g.resolved_expression_name(prefix, .unknown))
		expression_tokens << FastcExpressionToken{
			tok: .name
			lit: prefix
			unsafe_depth: g.unsafe_depth
		}
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	mut brace_depth := 0
	mut cast_depths := []int{}
	mut pointer_cast_depths := []int{}
	mut previous_was_pointer_cast := false
	mut has_sum_arithmetic_operator := false
	mut has_multiply_operator := false
	mut has_and_operator := false
	mut has_pipe_operator := false
	mut has_xor_operator := false
	mut previous_token := if prefix.len > 0 { token.Token.name } else { token.Token.unknown }
	mut previous_lit := prefix
	mut previous_token_end := g.s.pos
	mut previous_module_separator := false
	mut unsafe_expression_depth := 0
	// The brace_depth at which each still-open unsafe/lock block was opened, so a `}` is matched
	// to the innermost open construct (a nested struct literal vs. the unsafe block itself).
	mut unsafe_open_brace_depths := []int{}
	mut struct_types := []string{}
	mut struct_depths := []int{}
	mut struct_paren_depths := []int{}
	mut expected_struct_field_type := ''
	// Result-buffer offset where the current struct-literal field VALUE begins (just past
	// its `=`), letting an `or` inside a field value be scoped to that value.
	mut struct_field_value_start := 0
	mut pending_field_value_mark := false
	mut enum_shorthand_type := ''
	mut next_token_is_mut_argument := false
	mut source_token_count := if prefix == '' { 0 } else { 1 }
	mut mutation_operator := token.Token.unknown
	mut tokens_before_mutation := 0
	// A plain assignment gives its right-hand side the target's type (below), so an
	// if/match-expression or `[]` on the right infers against the field type rather
	// than a stale `expected_expression_type` (e.g. wrapping a string result as an
	// Option). This is restored after the expression is read.
	saved_expected_expression_type := g.expected_expression_type
	for g.tok != .eof {
		if g.selfhost && g.tok == .semicolon && g.semicolon_continues_expression() {
			g.next()
			continue
		}
		if g.selfhost && g.tok == .semicolon && struct_depths.len > 0 && brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() && previous_token in [
			.lcbr,
			.comma,
		] {
			// A newline right after a struct-literal `{` (or a field separator) auto-inserts a `;`
			// (the preceding `Struct{`/`,` leaves the scanner's insert-semi flag set). It precedes
			// the first/next field, so it is spurious — skip it. A `;` following a rendered field
			// VALUE is instead converted to `,` by the struct-field handling further below.
			g.next()
			continue
		}
		// `$res(N)` (the Nth function result, referenced in a defer to record the return value
		// for debugging) is a comptime construct the V3 backend does not support. The defer body
		// is gated by a runtime flag, so render a zero value of the Nth return type: valid C that
		// keeps the surrounding function compiling and linking.
		if g.tok == .dollar {
			mut res_lookahead := g.s
			if res_lookahead.scan() == .name && res_lookahead.lit == 'res' {
				g.next() // `$`
				g.next() // `res`
				g.expect(.lpar)!
				mut res_index := 0
				if g.tok == .number {
					res_index = g.lit.int()
					g.next()
				}
				g.expect(.rpar)!
				res_type := if g.return_types.len > res_index {
					g.return_types[res_index]
				} else if g.return_type !in ['', 'void', 'MultiReturn'] {
					g.return_type
				} else {
					'int'
				}
				res_value := '(${fastc_normalize_inferred_type(res_type)}){0}'
				if result.len > 0 && fastc_needs_space(result.last(), res_value) {
					result.write_u8(` `)
				}
				result.write_string(res_value)
				expression_tokens << FastcExpressionToken{
					tok: .name
					lit: res_value
					typ: fastc_normalize_inferred_type(res_type)
				}
				previous_token = .name
				previous_lit = res_value
				previous_module_separator = false
				previous_token_end = g.s.pos
				continue
			}
		}
		// `$d('key', default)` may appear mid-expression (e.g. `int($d(...))`).
		// Lower it inline to its default value.
		if g.tok == .dollar {
			mut lookahead := g.s
			if lookahead.scan() == .name && lookahead.lit == 'd' {
				g.next() // consume `$`
				value := g.read_comptime_d_expression()!
				value_type := g.last_expression_type
				if result.len > 0 && fastc_needs_space(result.last(), value) {
					result.write_u8(` `)
				}
				result.write_string(value)
				expression_tokens << FastcExpressionToken{
					tok: .name
					lit: value
					typ: value_type
				}
				previous_token = .name
				previous_lit = value
				previous_module_separator = false
				previous_token_end = g.s.pos
				continue
			}
		}
		// `arr.map/filter/any/all/count(expr)`: expand the `it`-closure into a C statement
		// expression. These are compiler-magic methods (not builtin functions), so
		// they must be lowered here, where locals are mutable and the closure body
		// can be rendered with `it` in scope.
		if g.tok == .dot && expression_tokens.len > 0 {
			lowered := g.lower_higher_order_expression(mut result, mut expression_tokens)!
			if lowered.found {
				previous_token = .name
				previous_lit = lowered.lit
				previous_module_separator = false
				previous_token_end = g.s.pos
				continue
			}
		}
		if g.selfhost && expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && unsafe_expression_depth == 0 && g.tok == .mul && g.s.src[previous_token_end..g.s.pos].contains('\n') {
			mut lookahead := g.s
			if lookahead.scan() == .name && lookahead.scan().is_assignment() {
				break
			}
		}
		if expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && unsafe_expression_depth == 0 && previous_token in [
			.inc,
			.dec,
		] && g.s.src[previous_token_end..g.s.pos].contains('\n') {
			break
		}
		if g.tok in [.key_if, .key_unsafe] && expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && unsafe_expression_depth == 0 && previous_token !in [
			.plus,
			.minus,
			.mul,
			.div,
			.mod,
			.amp,
			.pipe,
			.xor,
			.and,
			.logical_or,
			.eq,
			.ne,
			.lt,
			.gt,
			.le,
			.ge,
			.comma,
			.lpar,
			.lsbr,
		] && g.s.src[previous_token_end..g.s.pos].contains('\n') {
			break
		}
		if g.tok == .key_unsafe {
			g.next()
			if g.tok != .lcbr {
				return g.unsupported('unsafe expression without a block')
			}
			unsafe_expression_depth++
			g.unsafe_depth += 1
			unsafe_open_brace_depths << brace_depth
			g.next()
			continue
		}
		if (g.tok == .key_lock || g.tok == .key_rlock) && previous_token != .dot {
			// `rlock x { value }` yields the block's value. FastC does no real locking,
			// so skip the lock targets and treat the block transparently, exactly like an
			// `unsafe { value }` expression. A `.lock`/`.rlock` after `.` is a method name
			// (`mutex.lock()`), not the keyword, so it is excluded.
			g.next()
			for g.tok != .lcbr && g.tok != .eof {
				g.next()
			}
			if g.tok != .lcbr {
				return g.unsupported('lock expression without a block')
			}
			unsafe_expression_depth++
			g.unsafe_depth += 1
			unsafe_open_brace_depths << brace_depth
			g.next()
			continue
		}
		if unsafe_expression_depth > 0 && g.tok == .rcbr && unsafe_open_brace_depths.len > 0 && unsafe_open_brace_depths.last() == brace_depth {
			// This `}` matches the innermost open unsafe/lock block. A `}` at a deeper brace_depth
			// belongs to a nested struct/array literal opened inside the block
			// (`unsafe { U64F64{ f: value }.u }`); leaving it to the brace-depth handler preserves
			// the member access after the literal. Conversely, an unsafe block opened as a struct
			// field value (`string{ str: unsafe { m() } }`) sits at the literal's brace_depth and
			// was opened later, so it still closes here.
			unsafe_expression_depth--
			g.unsafe_depth -= 1
			unsafe_open_brace_depths.delete_last()
			g.next()
			if unsafe_expression_depth == 0 {
				continue
			}
		}
		if unsafe_expression_depth > 0 && g.tok == .semicolon {
			g.next()
			continue
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && g.tok in stops {
			if !(g.selfhost && g.tok == .lcbr && g.condition_brace_opens_struct_literal(expression_tokens, previous_token, previous_lit)) {
				break
			}
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && g.tok == .comma {
			// V's top-level commas form simultaneous multi-target assignments.
			// Copying them to C would instead emit comma operators.
			return g.unsupported('parallel assignments')
		}
		if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
			literal := g.lit
			previous_expected_type := g.expected_expression_type
			if expected_struct_field_type != '' {
				g.expected_expression_type = expected_struct_field_type
			}
			interpolation := g.read_interpolated_string()!
			g.expected_expression_type = previous_expected_type
			if result.len > 0 && fastc_needs_space(result.last(), interpolation) && !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(interpolation)
			expression_tokens << FastcExpressionToken{
				tok: .string
				lit: literal
				source: interpolation
				typ: 'string'
			}
			previous_token = .string
			previous_lit = literal
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .lcbr && expression_tokens.len >= 2 && expression_tokens[0].tok == .name && expression_tokens[0].lit == 'chan' {
			g.skip_balanced(.lcbr, .rcbr)!
			result.go_back(result.len)
			result.write_string('(chan){0}')
			expression_tokens = [
				FastcExpressionToken{
					tok: .name
					lit: '(chan){0}'
					typ: 'chan'
				},
			]
			previous_token = .name
			previous_lit = '(chan){0}'
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		shared_classification := g.classify_shared_token(previous_token)
		shared_is_struct_field := shared_classification.is_identifier && g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() && previous_token in [
			.lcbr,
			.comma,
			.semicolon,
		]
		if g.tok == .key_shared && shared_classification.is_identifier && !shared_is_struct_field && enum_shorthand_type == '' {
			// A `.shared` enum shorthand (member named with the `shared` keyword) is resolved
			// against the pending `enum_shorthand_type` below, not treated as a `shared` local.
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: g.lit
				unsafe_depth: g.unsafe_depth
				is_mut_argument: next_token_is_mut_argument
			}
			next_token_is_mut_argument = false
			source_token_count++
			mut piece := if previous_token == .dot {
				g.lit
			} else {
				g.reference_local_value_piece(g.lit, g.lit, previous_token, expression_tokens, stops)
			}
			if g.selfhost && g.lit in g.generic_method_names {
				if mono := g.queue_expression_monomorphization(expression_tokens) {
					piece = mono
					expression_tokens[expression_tokens.len - 1].lit = mono
				}
			}
			result.write_string(piece)
			previous_token = .name
			previous_lit = g.lit
			previous_module_separator = false
			previous_token_end = g.s.pos
			g.next()
			if shared_classification.ends_expression && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && unsafe_expression_depth == 0 {
				break
			}
			continue
		}
		if g.tok in [.key_mut, .key_shared] && !shared_is_struct_field && enum_shorthand_type == '' {
			consumed_mut := g.tok == .key_mut
			g.next()
			if g.selfhost && consumed_mut && allow_declaration_guard && g.tok == .name {
				mut probe := g.s
				if probe.scan() in [token.Token.key_is, token.Token.not_is] {
					// `if mut x is T`: the `mut` only marks the smart-cast subject mutable;
					// it must not emit an address-of. Preserve that marker on the name token
					// so parse_if can expose the boxed payload by reference in the branch.
					next_token_is_mut_argument = true
					continue
				}
			}
			if g.tok in [.amp, .and] {
				next_token_is_mut_argument = true
				continue
			}
			if (g.tok == .name || g.shared_token_is_identifier(previous_token)) && g.local_is_pointer(g.lit) {
				mut next_offset := g.s.offset
				for next_offset < g.s.src.len && g.s.src[next_offset].is_space() {
					next_offset++
				}
				if next_offset >= g.s.src.len || g.s.src[next_offset] !in [`.`, `[`] {
					next_token_is_mut_argument = true
					continue
				}
			}
			result.write_u8(`&`)
			expression_tokens << FastcExpressionToken{
				tok: .amp
				lit: '&'
				is_mut_argument: true
			}
			previous_token = .amp
			previous_lit = '&'
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_if {
			previous_expected_type := g.expected_expression_type
			if expected_struct_field_type != '' {
				g.expected_expression_type = expected_struct_field_type
			} else if paren_depth > 0 {
				// A parenthesized if-expression — a call argument (`f(if c {…} else {…})`) or a
				// A parenthesized if-expression — a call argument (`f(if c {…} else {…})`) or a
				// grouped operand — takes its type from its own branches, not the surrounding
				// expected type (which describes the whole call), so clear it to avoid boxing an
				// `int` argument into the outer sum type. But propagate an ENUM parameter type so
				// branch `.member` shorthands (`f(if c { .arrow } else { .dot })`) resolve.
				arg_type := g.streaming_call_argument_type(expression_tokens)
				if arg_type != '' && g.declared_kinds[g.semantic_type_key(arg_type)] == .enum_ {
					g.expected_expression_type = arg_type
				} else {
					g.expected_expression_type = ''
				}
			}
			conditional := g.read_if_expression()!
			conditional_type := g.last_expression_type
			g.expected_expression_type = previous_expected_type
			if result.len > 0 && fastc_needs_space(result.last(), conditional) && !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(conditional)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: conditional
				typ: conditional_type
			}
			previous_token = .name
			previous_lit = conditional
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_match {
			matched := g.read_match_expression()!
			matched_type := g.last_expression_type
			if result.len > 0 && fastc_needs_space(result.last(), matched) && !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(matched)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: matched
				typ: matched_type
			}
			previous_token = .name
			previous_lit = matched
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_fn {
			anon := g.parse_anonymous_function()!
			if result.len > 0 && fastc_needs_space(result.last(), anon) && !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(anon)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: anon
				source: anon
			}
			previous_token = .name
			previous_lit = anon
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		mut spawn_is_field_name := false
		if g.selfhost && g.tok == .key_spawn && previous_token != .dot {
			mut spawn_probe := g.s
			// `spawn:` (a struct-field/map key named `spawn`) is a field name, not the
			// `spawn` keyword.
			spawn_is_field_name = spawn_probe.scan() == .colon
		}
		// A struct-literal field may be named with a word that is also a keyword
		// (`MonomorphCacheSpec{ module: … }`). At a field-name position (right after `{`,
		// `,` or `;` inside the struct's braces) a keyword followed by `:` is that field's
		// name, not the keyword.
		mut keyword_is_field_name := false
		if g.selfhost && g.tok.is_keyword() && previous_token in [.lcbr, .comma, .semicolon] && struct_depths.len > 0 && brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() {
			mut keyword_field_probe := g.s
			keyword_is_field_name = keyword_field_probe.scan() == .colon
		}
		if g.selfhost && g.tok == .key_spawn && previous_token != .dot && !spawn_is_field_name {
			spawned := g.read_spawn_expression()!
			spawned_type := g.last_expression_type
			if result.len > 0 && fastc_needs_space(result.last(), spawned) && !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(spawned)
			// The eager spawn parser consumes the complete call. Keep it as one
			// typed atom so enclosing calls, appends, and operators can infer it.
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: spawned
				typ: spawned_type
			}
			previous_token = .name
			previous_lit = spawned
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_or {
			lowered := g.lower_or_expression(mut result, mut expression_tokens, paren_depth, brace_depth, struct_depths, struct_paren_depths, struct_field_value_start, expected_struct_field_type, saved_expected_expression_type)!
			if lowered.complete {
				return lowered.source
			}
			previous_token = lowered.previous_token
			previous_lit = lowered.previous_lit
			paren_depth = lowered.paren_depth
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .name && g.lit.starts_with('@') {
			pseudo_name := g.lit
			pseudo := g.comptime_pseudo_expression(pseudo_name) or {
				return g.unsupported('compile-time pseudo value `${pseudo_name}`')
			}
			if result.len > 0 && fastc_needs_space(result.last(), pseudo) && !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(pseudo)
			expression_tokens << FastcExpressionToken{
				tok: .string
				lit: pseudo_name
				source: pseudo
				typ: 'string'
			}
			previous_token = .string
			previous_lit = pseudo_name
			previous_module_separator = false
			previous_token_end = g.s.offset
			g.next()
			continue
		}
		if !g.selfhost && g.tok in [.left_shift, .right_shift, .right_shift_unsigned,
			.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
			// V defines oversized shifts to produce zero. Raw C shifts are
			// undefined and may mask the count to the operand width instead.
			return g.unsupported('shift expressions')
		}
		if !g.selfhost && g.tok in [.div, .div_assign, .mod, .mod_assign] {
			// Integer division and modulo require V's runtime zero checks. This
			// scanner-only lane has no type information to add them selectively.
			return g.unsupported('division or modulo expressions')
		}
		if !g.selfhost && g.tok == .key_sizeof {
			// Direct C representations can differ from V layouts. Reject sizeof
			// until the parser tracks enough V type information to lower it.
			return g.unsupported('sizeof expressions')
		}
		if !g.selfhost && g.tok in [.lsbr, .rsbr] {
			// Indexing requires V element types and bounds checks. C pointer/array
			// indexing cannot preserve either in this scanner-only lane.
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if (!g.selfhost || g.tok !in [.lcbr, .rcbr]) && g.tok in [.lcbr, .rcbr, .str_dollar,
			.key_match, .key_or, .arrow, .power] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if !g.selfhost && g.tok in [.key_in, .not_in] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if !g.selfhost && g.tok in [.key_is, .not_is, .key_as] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		match g.tok {
			.plus, .minus {
				has_sum_arithmetic_operator = true
			}
			.mul {
				has_multiply_operator = true
			}
			.amp {
				has_and_operator = true
			}
			.pipe {
				has_pipe_operator = true
			}
			.xor {
				has_xor_operator = true
			}
			else {}
		}
		if !g.selfhost && ((has_sum_arithmetic_operator && (has_and_operator || has_pipe_operator || has_xor_operator)) || (has_multiply_operator && has_and_operator) || (has_pipe_operator && has_xor_operator)) {
			// V groups + and - with | and ^, and * with &, while C splits those
			// levels and also orders + and - above &. Reject ambiguous token streams.
			return g.unsupported('mixed operator precedence')
		}
		value_context := paren_depth != 0 || bracket_depth != 0 || brace_depth != 0 || unsafe_expression_depth != 0
		if g.selfhost && g.tok in [.inc, .dec] && value_context && source_token_count > 0 {
			// A post-increment/decrement used as a value (`Node{ id: c.x++ }`) is a valid C
			// post-fix expression with matching semantics, so append it as an operand
			// rather than treating it as a statement mutation.
		} else if g.tok.is_assignment() || g.tok in [.inc, .dec] {
			is_declaration_guard := allow_declaration_guard && g.tok == .decl_assign && source_token_count == 1
			if (!allow_mutation_statement && !is_declaration_guard) || paren_depth != 0 || bracket_depth != 0 || brace_depth != 0 || unsafe_expression_depth != 0 {
				return g.unsupported('mutation `${g.token_source()}` inside an expression')
			}
			if mutation_operator != .unknown {
				return g.unsupported('multiple mutations in one expression')
			}
			mutation_operator = g.tok
			tokens_before_mutation = source_token_count
			if g.selfhost && g.tok == .assign {
				target_type := g.infer_expression_type(expression_tokens[..source_token_count]) or {
					''
				}
				if target_type != '' {
					g.expected_expression_type = target_type
				}
			}
			mut mutation_lookahead := g.s
			next_mutation_token := mutation_lookahead.scan()
			mutation_ends_line := mutation_lookahead.pos >= g.s.offset && g.s.src[g.s.offset..mutation_lookahead.pos].contains('\n')
			if mutation_operator in [.inc, .dec] && next_mutation_token !in stops && next_mutation_token != .eof && !mutation_ends_line {
				return g.unsupported('postfix mutation used inside an expression')
			}
			if mutation_operator.is_assignment() && (next_mutation_token in stops || next_mutation_token == .eof) {
				return g.unsupported('assignment without a value')
			}
		}
		source_token_count++
		// A word that is also a keyword (`conn.select(...)`, `x.lock`) is a member
		// name, not a keyword, once it follows `.`; store it as a plain name so the
		// method-call and inference paths recognize it like any other member.
		stored_tok := if (previous_token == .dot && g.tok.is_keyword()) || shared_is_struct_field || spawn_is_field_name || keyword_is_field_name {
			token.Token.name
		} else {
			g.tok
		}
		expression_tokens << FastcExpressionToken{
			tok: stored_tok
			lit: g.lit
			unsafe_depth: g.unsafe_depth
			is_mut_argument: next_token_is_mut_argument
		}
		next_token_is_mut_argument = false
		module_separator := g.tok == .dot
			&& g.expression_dot_is_module_separator(expression_tokens, expression_tokens.len - 1)
		qualified_name_owner := if g.tok == .name && previous_token == .dot
			&& expression_tokens.len >= 3 && previous_module_separator {
			expression_tokens[expression_tokens.len - 3].lit
		} else {
			''
		}
		selfhost_bare_name := g.selfhost && g.tok == .name && previous_token != .dot
		mut piece := if selfhost_bare_name {
			fastc_c_identifier(g.lit)
		} else {
			g.expression_token(previous_token, previous_lit, qualified_name_owner, module_separator)!
		}
		mut monomorphized_name := false
		if g.selfhost && g.tok == .name && g.lit in g.generic_method_names {
			if mono := g.queue_expression_monomorphization(expression_tokens) {
				piece = mono
				expression_tokens[expression_tokens.len - 1].lit = mono
				monomorphized_name = true
			}
		}
		if g.tok == .name && previous_token != .dot {
			// A name after `.` is a field or method: never substitute a local
			// (a mut parameter's deref form) that happens to share its name.
			if selfhost_bare_name {
				if local := g.locals[g.lit] {
					piece = g.reference_local_value_piece_for_local(local, piece, previous_token, expression_tokens, stops)
				} else if !monomorphized_name {
					piece = if g.lit == 'C' {
						''
					} else {
						g.resolved_nonlocal_expression_name_cached(g.lit)
					}
				}
			} else {
				piece = g.reference_local_value_piece(g.lit, piece, previous_token, expression_tokens, stops)
			}
		}
		if module_separator && piece == '.' {
			piece = '__'
		}
		if previous_token == .dot && (g.tok == .name || g.tok.is_keyword())
			&& expression_tokens.len >= 3 && previous_module_separator {
			// A module or enum prefix makes a keyword-named symbol C-safe
			// (`TokenKind.float` -> `TokenKind__float`).
			piece = g.lit
		}
		if g.tok == .name && previous_token == .dot {
			mut method_lookahead := g.s
			if method_lookahead.scan() == .lpar {
				piece = expression_tokens.last().lit
			}
		}
		if g.tok == .name && expression_tokens.len >= 3 && expression_tokens[expression_tokens.len - 2].tok == .dot && expression_tokens[expression_tokens.len - 3].tok == .name && expression_tokens[expression_tokens.len - 3].lit == 'C' {
			piece = g.lit
		}
		if g.selfhost && brace_depth > 0 && (g.tok == .name || keyword_is_field_name) {
			mut field_lookahead := g.s
			if field_lookahead.scan() == .colon {
				piece = g.lit
			}
		}
		if g.selfhost && g.tok == .dot && previous_was_pointer_cast {
			piece = '->'
		}
		if g.tok == .lpar && previous_token == .name {
			name_is_member := expression_tokens.len >= 3 && expression_tokens[expression_tokens.len - 3].tok == .dot
			pointer_token := if expression_tokens.len >= 3 && fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 3) {
				expression_tokens[expression_tokens.len - 3].tok
			} else {
				token.Token.unknown
			}
			pointer_count := if pointer_token == .and {
				2
			} else if pointer_token == .amp {
				1
			} else {
				0
			}
			pointer_cast := pointer_count > 0
			pointer_suffix := '*'.repeat(pointer_count)
			pointer_prefix_len := pointer_count
			if expression_tokens.len >= 4 && expression_tokens[expression_tokens.len - 4].tok == .lsbr && expression_tokens[expression_tokens.len - 3].tok == .rsbr {
				element_type := fastc_primitive_c_type(previous_lit) or { previous_lit }
				array_type := fastc_array_c_type(element_type)
				rendered_previous := g.resolved_expression_name(previous_lit, .unknown)
				result.go_back(rendered_previous.len + 2)
				piece = '((${array_type})('
				cast_depths << paren_depth + 1
			} else if expression_tokens.len >= 4 && expression_tokens[expression_tokens.len - 4].tok == .name && expression_tokens[expression_tokens.len - 4].lit == 'C' && expression_tokens[expression_tokens.len - 3].tok == .dot && previous_lit.len > 0 && 'C.${previous_lit}' !in g.functions && g.current_call_has_one_argument() && (previous_lit[0].is_capital() || '#Cstruct#${previous_lit}' in g.declared_types) {
				c_pointer_token := if expression_tokens.len >= 5 && fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 5) {
					expression_tokens[expression_tokens.len - 5].tok
				} else {
					token.Token.unknown
				}
				c_pointer_count := if c_pointer_token == .and {
					2
				} else if c_pointer_token == .amp {
					1
				} else {
					0
				}
				c_cast_type := if '#Cstruct#${previous_lit}' in g.declared_types {
					'struct ${previous_lit}'
				} else {
					previous_lit
				}
				result.go_back(previous_lit.len + c_pointer_count)
				piece = '((${fastc_output_c_type(c_cast_type)}${'*'.repeat(c_pointer_count)})('
				cast_depths << paren_depth + 1
				if c_pointer_count > 0 {
					pointer_cast_depths << paren_depth + 1
				}
			} else if expression_tokens.len >= 4 && expression_tokens[expression_tokens.len - 3].tok == .dot && expression_tokens[expression_tokens.len - 4].tok == .name && expression_tokens[expression_tokens.len - 4].lit in g.imports {
				// Qualified conversion `mod.Type(value)` (e.g. `orm.Primitive(x)`): render
				// it as a C cast so a sum-type/interface operand is boxed downstream,
				// instead of the default `mod__Type(value)` call-style spelling.
				module_lit := expression_tokens[expression_tokens.len - 4].lit
				qualified_key := fastc_type_key(g.imports[module_lit], previous_lit)
				if qualified_key in g.declared_types {
					cast_type := fastc_c_declared_type_name(qualified_key)
					// The pointer prefix (`&mod.Type(x)` / `&&mod.Type(x)`) sits BEFORE the module
					// name (`… & ast . File (`), i.e. at len-5, not the len-3 the bare-type case
					// checks — there len-3 is the `.` of the qualification.
					qual_pointer_token := if expression_tokens.len >= 5 && fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 5) {
						expression_tokens[expression_tokens.len - 5].tok
					} else {
						token.Token.unknown
					}
					qual_pointer_count := if qual_pointer_token == .and {
						2
					} else if qual_pointer_token == .amp {
						1
					} else {
						0
					}
					result.go_back(cast_type.len + qual_pointer_count)
					piece = '((${cast_type}${'*'.repeat(qual_pointer_count)})('
					cast_depths << paren_depth + 1
					if qual_pointer_count > 0 {
						pointer_cast_depths << paren_depth + 1
					}
				}
			} else if cast_type := fastc_primitive_c_type(previous_lit) {
				if !name_is_member {
					rendered_previous := g.resolved_expression_name(previous_lit, .unknown)
					result.go_back(rendered_previous.len + pointer_prefix_len)
					piece = '((${fastc_output_c_type(cast_type)}${pointer_suffix})('
					cast_depths << paren_depth + 1
					if pointer_cast {
						pointer_cast_depths << paren_depth + 1
					}
				}
			} else if type_key := g.resolve_declared_type_key(previous_lit) {
				if !name_is_member {
					cast_type := fastc_c_declared_type_name(type_key)
					rendered_previous := g.resolved_expression_name(previous_lit, .unknown)
					result.go_back(rendered_previous.len + pointer_prefix_len)
					piece = '((${cast_type}${pointer_suffix})('
					cast_depths << paren_depth + 1
					if pointer_cast {
						pointer_cast_depths << paren_depth + 1
					}
				}
			}
		}
		if g.selfhost && g.tok == .lcbr {
			initializer_tokens := expression_tokens[..expression_tokens.len - 1].clone()
			initializer_start := fastc_initializer_type_start(initializer_tokens)
			initializer_type_tokens := initializer_tokens[initializer_start..]
			mut is_struct_literal := false
			if map_type := g.map_initializer_type(initializer_type_tokens) {
				fastc_register_composite_type(map_type, mut g.composite_types)
				initializer_source := g.render_raw_expression_tokens(initializer_type_tokens) or {
					return g.unsupported('map initializer type')
				}
				result.go_back(initializer_source.len)
				piece = '(${map_type}){'
				brace_depth++
				struct_types << map_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if array_type := g.array_initializer_type(initializer_type_tokens) {
				fastc_register_composite_type(array_type, mut g.composite_types)
				initializer_source := g.render_raw_expression_tokens(initializer_type_tokens) or {
					return g.unsupported('array initializer type')
				}
				result.go_back(initializer_source.len)
				c_array_type := fastc_array_initializer_c_type(array_type)
				if array_type.starts_with('FixedArray_') {
					g.fixed_array_types[c_array_type] = array_type
				}
				piece = '(${c_array_type}){'
				brace_depth++
				struct_types << c_array_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if expression_tokens.len >= 4 && expression_tokens[expression_tokens.len - 4].tok == .name && expression_tokens[expression_tokens.len - 4].lit == 'C' && expression_tokens[expression_tokens.len - 3].tok == .dot && expression_tokens[expression_tokens.len - 2].tok == .name {
				raw_c_type := expression_tokens[expression_tokens.len - 2].lit
				c_type := if '#Cstruct#${raw_c_type}' in g.declared_types {
					'struct ${raw_c_type}'
				} else {
					raw_c_type
				}
				result.go_back(raw_c_type.len)
				piece = '(${c_type}){'
				brace_depth++
				struct_types << c_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if expression_tokens.len >= 4 && expression_tokens[expression_tokens.len - 4].tok == .name && expression_tokens[expression_tokens.len - 3].tok == .dot && expression_tokens[expression_tokens.len - 2].tok == .name {
				module_alias := expression_tokens[expression_tokens.len - 4].lit
				if imported_module := g.imports[module_alias] {
					raw_type := expression_tokens[expression_tokens.len - 2].lit
					type_key := fastc_type_key(imported_module, raw_type)
					if type_key in g.declared_types {
						c_type := fastc_c_declared_type_name(type_key)
						result.go_back(c_type.len)
						piece = '(${c_type}){'
						brace_depth++
						struct_types << c_type
						struct_depths << brace_depth
						struct_paren_depths << paren_depth
						is_struct_literal = true
					}
				}
			} else if previous_token == .name {
				if type_key := g.resolve_declared_type_key(previous_lit) {
					c_type := fastc_c_declared_type_name(type_key)
					result.go_back(c_type.len)
					piece = '(${c_type}){'
					brace_depth++
					struct_types << c_type
					struct_depths << brace_depth
					struct_paren_depths << paren_depth
					is_struct_literal = true
				}
			}
			if !is_struct_literal && brace_depth > 0 {
				brace_depth++
				piece = '{'
			}
		} else if g.selfhost && g.tok == .rcbr && brace_depth > 0 {
			if struct_depths.len > 0 && struct_depths.last() == brace_depth {
				struct_depths.delete_last()
				if struct_types.len > 0 {
					struct_types.delete_last()
				}
				if struct_paren_depths.len > 0 {
					struct_paren_depths.delete_last()
				}
				expected_struct_field_type = ''
			}
			brace_depth--
			piece = '}'
		} else if g.selfhost && g.tok == .colon && struct_depths.len > 0 && brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() {
			piece = '='
			pending_field_value_mark = true
		} else if g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() && g.tok == .semicolon {
			piece = ','
		} else if g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() && (g.tok == .name || shared_is_struct_field || spawn_is_field_name || keyword_is_field_name) && previous_token in [
			.lcbr,
			.comma,
			.semicolon,
		] && struct_types.len > 0 {
			if fields := g.struct_fields[struct_types.last()] {
				expected_struct_field_type = fields[g.lit] or { '' }
			} else {
				expected_struct_field_type = ''
			}
			piece = '.${piece}'
		} else if g.selfhost && g.tok == .dot && (fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 1) || (expression_tokens.len > 0 && expression_tokens.last().tok in [
			.xor,
			.pipe,
			.amp,
		])) {
			// `.member` right after a value would be a field access, but after a binary flag
			// operator (`~Show.zero() ^ .name`, `a | .b`) it is an enum-shorthand operand, which
			// fastc_token_is_prefix_operator misses (it inspects the token BEFORE the operator).
			mut contextual_type := if expected_struct_field_type != '' {
				expected_struct_field_type
			} else {
				g.expected_expression_type
			}
			if contextual_type == '' {
				mut assignment_depth := 0
				for i, assignment_token in expression_tokens[..expression_tokens.len - 1] {
					if assignment_token.tok in [.lpar, .lsbr, .lcbr] {
						assignment_depth++
					} else if assignment_token.tok in [.rpar, .rsbr, .rcbr] {
						assignment_depth--
					} else if assignment_depth == 0 && assignment_token.tok.is_assignment() && i > 0 {
						contextual_type = g.infer_expression_type(expression_tokens[..i]) or { '' }
						break
					}
				}
			}
			if contextual_type != '' {
				// An unqualified same-module field type (`Show`) must resolve to its declared C
				// type so the shorthand becomes `flag__Show__name`, not a raw unqualified name.
				if resolved_enum_key := g.resolve_declared_type_key(contextual_type.trim_right('*')) {
					contextual_type = fastc_c_declared_type_name(resolved_enum_key)
				}
			}
			if g.declared_kinds[g.semantic_type_key(contextual_type)] != .enum_ {
				contextual_type = ''
			}
			if contextual_type == '' {
				contextual_type = g.expected_call_argument_type(expression_tokens)
			}
			if expression_tokens.len >= 2 && expression_tokens[expression_tokens.len - 2].tok in [
				.eq,
				.ne,
				.gt,
				.lt,
				.ge,
				.le,
				.pipe,
				.amp,
				.xor,
			] {
				operator_index := expression_tokens.len - 2
				mut operand_start := 0
				mut operand_depth := 0
				for i := operator_index - 1; i >= 0; i-- {
					if expression_tokens[i].tok in [.rpar, .rsbr, .rcbr] {
						operand_depth++
					} else if expression_tokens[i].tok in [.lpar, .lsbr, .lcbr] {
						operand_depth--
						if operand_depth < 0 {
							// Reached the enclosing `(`/`[`/`{` (`int(l.mode == .x)`); the left
							// operand begins just after it, not at the whole expression.
							operand_start = i + 1
							break
						}
					} else if operand_depth == 0 && expression_tokens[i].tok in [.and, .logical_or] {
						operand_start = i + 1
						break
					}
				}
				inferred_type := g.infer_expression_type(expression_tokens[operand_start..operator_index]) or {
					''
				}
				if inferred_type != '' {
					contextual_type = inferred_type
				}
			}
			if g.declared_kinds[g.semantic_type_key(contextual_type)] == .enum_ {
				piece = ''
				enum_shorthand_type = contextual_type
			}
		} else if g.selfhost && enum_shorthand_type != '' && (g.tok == .name || g.tok.is_keyword()) {
			piece = '${enum_shorthand_type.trim_right('*')}__${g.lit}'
			expression_tokens[expression_tokens.len - 1].typ = enum_shorthand_type
			enum_shorthand_type = ''
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator && !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
		if pending_field_value_mark {
			struct_field_value_start = result.len
			pending_field_value_mark = false
		}
		match g.tok {
			.lpar {
				paren_depth++
			}
			.rpar {
				if paren_depth == 0 {
					break
				}
				if paren_depth in pointer_cast_depths {
					previous_was_pointer_cast = true
					pointer_cast_depths.delete(pointer_cast_depths.index(paren_depth))
				}
				if paren_depth in cast_depths {
					result.go_back(piece.len)
					piece = '))'
					result.write_string(piece)
					cast_depths.delete(cast_depths.index(paren_depth))
				}
				paren_depth--
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				bracket_depth--
			}
			else {}
		}
		if g.tok != .rpar {
			previous_was_pointer_cast = false
		}
		previous_token = g.tok
		previous_lit = g.lit
		previous_module_separator = module_separator
		previous_token_end = g.s.offset
		g.next()
	}
	g.expected_expression_type = saved_expected_expression_type
	if paren_depth != 0 {
		return g.unsupported('unbalanced expression')
	}
	if bracket_depth != 0 {
		return g.unsupported('unbalanced array expression')
	}
	if brace_depth != 0 {
		return g.unsupported('unbalanced struct literal')
	}
	if unsafe_expression_depth != 0 {
		g.unsafe_depth -= unsafe_expression_depth
		return g.unsupported('unbalanced unsafe expression `${fastc_expression_tokens_debug(expression_tokens)}`')
	}
	if mutation_operator != .unknown {
		if tokens_before_mutation == 0 {
			return g.unsupported('mutation without a target')
		}
	}
	if !g.selfhost {
		g.validate_expression_mutation_lvalue(expression_tokens)!
		g.validate_expression_field_visibility(expression_tokens)!
		g.validate_expression_calls(expression_tokens)!
	}
	mut rendered_expression := fastc_take_trimmed(mut result)
	if g.selfhost && expression_tokens.len > 1 && expression_tokens.last().tok == .question {
		// A trailing `?` propagates an option exactly like `!` propagates a result.
		// FastC represents both with one `Option` type, so normalize `?` to the `!`
		// handling used everywhere below.
		last_token := expression_tokens.last()
		expression_tokens[expression_tokens.len - 1] = FastcExpressionToken{
			tok: .not
			source: last_token.source
			unsafe_depth: last_token.unsafe_depth
			is_mut_argument: last_token.is_mut_argument
			is_statement: last_token.is_statement
			lit: last_token.lit
			typ: last_token.typ
		}
	}
	// A boolean `is` expression with a flow-sensitive `&&` narrowing is handled first, on the
	// original tokens: its operands (including any `(x as T).field`) must render with the
	// narrowing active, which the general as-cast rewrite below cannot do. A top-level
	// assignment (`x = a is T && …`) is left to render_assignment_expression, which routes its
	// RHS through the same narrowing renderer; handling the whole `x = …` here would mis-split
	// at the `=` and register a stale smart-cast that the RHS then re-applies (double unwrap).
	if g.selfhost && !fastc_tokens_have_top_level_assignment(expression_tokens) {
		if narrowed := g.render_narrowing_boolean_expression(expression_tokens) {
			g.last_expression_type = 'bool'
			g.last_expression = expression_tokens
			return g.render_constant_references(expression_tokens, narrowed)
		}
	}
	if g.selfhost {
		if rewritten := g.rewrite_embedded_as_casts(expression_tokens) {
			expression_tokens = rewritten.clone()
			rendered_expression = g.render_raw_expression_tokens(expression_tokens) or {
				rendered_expression
			}
		}
	}
	if g.selfhost && expression_tokens.len > 1 && expression_tokens.last().tok == .not && rendered_expression.ends_with('?') {
		rendered_expression = rendered_expression[..rendered_expression.len - 1] + '!'
	}
	rendered_expression = g.render_enum_alias_member_references(expression_tokens, rendered_expression)
	rendered_expression = g.render_constant_references(expression_tokens, rendered_expression)
	if g.selfhost {
		// `m[k].field = value`: a map value is not a C lvalue, so assign the field through a
		// mutable pointer to the entry rather than the map-read spelling render_special yields.
		if map_field := g.render_map_value_field_assignment(expression_tokens) {
			g.last_expression_type = map_field.typ
			g.last_expression = expression_tokens
			return g.render_constant_references(expression_tokens, map_field.source)
		}
		// `m[k].field++` / `m[k].field--`: same lvalue problem, incremented through the entry.
		if map_field := g.render_map_value_field_inc_dec(expression_tokens) {
			g.last_expression_type = map_field.typ
			g.last_expression = expression_tokens
			return g.render_constant_references(expression_tokens, map_field.source)
		}
		// `x.field = value` where `field` is common to every variant of the boxed sum type `x`:
		// dispatch on the runtime tag and write through the matched variant, not the box.
		if common_assign := g.render_sumtype_common_field_assignment(expression_tokens) {
			g.last_expression_type = common_assign.typ
			g.last_expression = expression_tokens
			return g.render_constant_references(expression_tokens, common_assign.source)
		}
	}
	if special := g.render_special_expression(expression_tokens, rendered_expression) {
		g.last_expression_type = special.typ
		g.last_expression = expression_tokens
		return g.render_constant_references(expression_tokens, special.source)
	}
	g.last_expression_type = g.infer_expression_type(expression_tokens)!
	g.last_expression = expression_tokens
	return_types := g.multi_return_types_for_expression(expression_tokens)
	if return_types.len > 0 {
		g.last_multi_return_types = return_types.clone()
	}
	if !g.selfhost && g.last_expression_type == 'bool' && fastc_expression_tokens_contain_boolean_operator(expression_tokens) {
		// C comparison and logical operators produce int. Preserve V's bool
		// expression type for inferred declarations and generic dispatch.
		return '((bool)(${rendered_expression}))'
	}
	return rendered_expression
}

fn (g &Parser) reference_local_value_piece(name string, piece string, previous_token token.Token, expression_tokens []FastcExpressionToken, stops []token.Token) string {
	local := g.locals[name] or { return piece }
	return g.reference_local_value_piece_for_local(local, piece, previous_token, expression_tokens, stops)
}

fn (g &Parser) reference_local_value_piece_for_local(local FastcLocal, piece string, previous_token token.Token, expression_tokens []FastcExpressionToken, stops []token.Token) string {
	local_piece := if local.c_name != '' { local.c_name } else { piece }
	mut lookahead := g.s
	next_token := lookahead.scan()
	is_single_value := expression_tokens.len == 1 && (next_token in stops || next_token == .eof)
	// An explicit deref of a reference local (`*b` where `b` is a `mut` receiver,
	// C type `T*`): the leading `*` already dereferences, so auto-deref would
	// double it (`*(*(b))`). Skip the auto-deref so `*b` renders as `*(b)`. Only a
	// PREFIX `*` counts — a binary `a * b` still needs `b`'s value.
	is_deref_prefix := previous_token == .mul && expression_tokens.len > 0 && fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 1)
	cast_type := if previous_token == .lpar && expression_tokens.len >= 3 && expression_tokens[expression_tokens.len - 3].tok == .name {
		fastc_primitive_c_type(expression_tokens[expression_tokens.len - 3].lit) or { '' }
	} else {
		''
	}
	is_pointer_cast_operand := cast_type != '' && fastc_is_pointer_type(cast_type)
	if local.is_reference && !expression_tokens.last().is_mut_argument && next_token !in [
		.dot,
		.lsbr,
	] && !is_single_value && !is_deref_prefix && !is_pointer_cast_operand {
		return '(*(${local_piece}))'
	}
	return local_piece
}

fn fastc_shared_modifier_operand_start(tok token.Token) bool {
	if tok in [.key_or, .key_in, .key_as, .key_is] {
		return false
	}
	return tok == .name || (tok.is_keyword() && tok != .key_volatile) || tok in [.amp, .and,
		.question, .not, .lpar, .lsbr]
}

fn fastc_shared_modifier_may_cross_line(previous token.Token, next token.Token) bool {
	return previous in [.lpar, .lsbr, .comma, .colon] && fastc_shared_modifier_operand_start(next)
}

fn fastc_token_continues_expression_after_operand(tok token.Token) bool {
	return tok.is_infix() || tok.is_postfix() || tok.is_assignment() || tok in [
		.key_or,
		.key_as,
		.question,
		.not,
		.dot,
		.lpar,
		.lsbr,
	]
}

struct FastcSharedTokenClassification {
	is_identifier   bool
	ends_expression bool
}

fn (g &Parser) shared_token_is_identifier(previous token.Token) bool {
	return g.classify_shared_token(previous).is_identifier
}

fn (g &Parser) classify_shared_token(previous token.Token) FastcSharedTokenClassification {
	if g.tok != .key_shared {
		return FastcSharedTokenClassification{}
	}
	if previous == .dot {
		return FastcSharedTokenClassification{
			is_identifier: true
		}
	}
	mut lookahead := g.s
	next := lookahead.scan()
	start := g.s.offset
	mut offset := start
	mut crossed_line := false
	// Whitespace and comments separate a modifier from its operand. A line
	// comment can still continue inside an open argument or collection context.
	for {
		for offset < g.s.src.len && g.s.src[offset].is_space() {
			if g.s.src[offset] in [`\r`, `\n`] {
				crossed_line = true
			}
			offset++
		}
		if offset + 1 >= g.s.src.len || g.s.src[offset] != `/` {
			break
		}
		if g.s.src[offset + 1] == `/` {
			offset += 2
			for offset < g.s.src.len && g.s.src[offset] !in [`\r`, `\n`] {
				offset++
			}
			crossed_line = true
			continue
		}
		if g.s.src[offset + 1] != `*` {
			break
		}
		offset += 2
		mut depth := 1
		for offset + 1 < g.s.src.len && depth > 0 {
			if g.s.src[offset] == `/` && g.s.src[offset + 1] == `*` && (offset + 2 >= g.s.src.len || g.s.src[offset + 2] != `/`) {
				depth++
				offset += 2
				continue
			}
			if g.s.src[offset] == `*` && g.s.src[offset + 1] == `/` {
				depth--
				offset += 2
				continue
			}
			offset++
		}
		if depth > 0 {
			return FastcSharedTokenClassification{
				is_identifier: true
			}
		}
	}
	if offset >= g.s.src.len {
		return FastcSharedTokenClassification{
			is_identifier: true
		}
	}
	if crossed_line && !fastc_shared_modifier_may_cross_line(previous, next) {
		return FastcSharedTokenClassification{
			is_identifier: true
			ends_expression: !fastc_token_continues_expression_after_operand(next)
		}
	}
	if next in [.key_or, .key_in, .key_as, .key_is] {
		return FastcSharedTokenClassification{
			is_identifier: true
		}
	}
	if g.s.src[offset] == `(` {
		return FastcSharedTokenClassification{
			is_identifier: offset == start
		}
	}
	return FastcSharedTokenClassification{
		is_identifier: g.s.src[offset] in [`.`, `[`, `{`, `)`, `]`, `}`, `,`, `;`, `:`, `?`, `+`,
			`-`, `*`, `/`, `%`, `=`, `!`, `<`, `>`, `&`, `|`, `^`]
	}
}

fn (g &Parser) render_constant_references(tokens []FastcExpressionToken, source string) string {
	mut rendered := source
	for i, item in tokens {
		if item.tok != .name || (i > 0 && tokens[i - 1].tok == .dot) || (i + 1 < tokens.len && tokens[i + 1].tok == .colon) || item.lit in g.locals {
			continue
		}
		if i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			if fastc_primitive_c_type(item.lit) != none {
				continue
			}
			if g.resolve_declared_type_key(item.lit) != none {
				continue
			}
			function_key := g.unqualified_function_key(item.lit)
			if function_key in g.functions || function_key in g.mono_functions {
				rendered = fastc_replace_c_call_identifier(rendered, item.lit, g.c_function_name_for_key(function_key))
				continue
			}
		}
		constant_key := fastc_constant_key(g.module_name, item.lit)
		if c_name := g.constants[constant_key] {
			rendered = fastc_replace_c_root_identifier(rendered, item.lit, c_name)
		} else if c_name := g.constants[fastc_constant_key('builtin', item.lit)] {
			rendered = fastc_replace_c_root_identifier(rendered, item.lit, c_name)
		}
	}
	return rendered
}

// fastc_index_of returns the first index of `needle` in `text` at or after
// `start`, or -1. The builtin search builds a KMP table per call for needles
// longer than two bytes; the needles here are short identifiers and call
// prefixes, for which a plain scan is cheaper.
@[direct_array_access]
fn fastc_index_of(text string, needle string, start int) int {
	if needle.len == 0 || needle.len > text.len {
		return -1
	}
	first := needle[0]
	last := text.len - needle.len
	mut i := if start < 0 { 0 } else { start }
	for i <= last {
		if text[i] == first && unsafe { vmemcmp(text.str + i, needle.str, needle.len) } == 0 {
			return i
		}
		i++
	}
	return -1
}

fn fastc_contains(text string, needle string) bool {
	return fastc_index_of(text, needle, 0) >= 0
}

// fastc_replace replaces every occurrence of `needle` in `text` with
// `replacement`, scanning left to right; `text` itself is returned when it
// holds no occurrence.
fn fastc_replace(text string, needle string, replacement string) string {
	mut index := fastc_index_of(text, needle, 0)
	if index < 0 {
		return text
	}
	mut out := strings.new_builder(text.len + replacement.len)
	mut start := 0
	for index >= 0 {
		unsafe { out.write_ptr(text.str + start, index - start) }
		out.write_string(replacement)
		start = index + needle.len
		index = fastc_index_of(text, needle, start)
	}
	unsafe { out.write_ptr(text.str + start, text.len - start) }
	return out.str()
}

fn fastc_replace_c_call_identifier(source string, identifier string, replacement string) string {
	if identifier == '' || identifier == replacement || !fastc_contains(source, identifier) {
		return source
	}
	mut out := strings.new_builder(source.len + replacement.len)
	mut start := 0
	for start < source.len {
		index := fastc_index_of(source, identifier, start)
		if index < 0 {
			unsafe { out.write_ptr(source.str + start, source.len - start) }
			break
		}
		end := index + identifier.len
		before_is_name_or_member := index > 0 && (source[index - 1].is_alnum() || source[index - 1] in [
			`_`,
			`.`,
		])
		mut after := end
		for after < source.len && source[after] in [` `, `\t`, `\r`, `\n`] {
			after++
		}
		unsafe { out.write_ptr(source.str + start, index - start) }
		if before_is_name_or_member || after >= source.len || source[after] != `(` {
			out.write_string(identifier)
		} else {
			out.write_string(replacement)
		}
		start = end
	}
	return out.str()
}

fn fastc_replace_c_root_identifier(source string, identifier string, replacement string) string {
	if identifier == '' || identifier == replacement || !fastc_contains(source, identifier) {
		return source
	}
	mut out := strings.new_builder(source.len + replacement.len)
	mut start := 0
	for start < source.len {
		index := fastc_index_of(source, identifier, start)
		if index < 0 {
			unsafe { out.write_ptr(source.str + start, source.len - start) }
			break
		}
		end := index + identifier.len
		before_is_name := index > 0 && (source[index - 1].is_alnum() || source[index - 1] == `_`)
		before_is_member := index > 0 && source[index - 1] in [`.`, `>`]
		after_is_name := end < source.len && (source[end].is_alnum() || source[end] == `_`)
		unsafe { out.write_ptr(source.str + start, index - start) }
		if before_is_name || before_is_member || after_is_name {
			out.write_string(identifier)
		} else {
			out.write_string(replacement)
		}
		start = end
	}
	return out.str()
}

// fastc_replace_call_needle replaces a method-call needle (`receiver.method(`, ending in `(`)
// only where its START is not preceded by an identifier char or `.` — so a needle like
// `return_type.clear(` does not match the suffix of a longer chain `fn_decl.return_type.clear(`
// (whose real receiver is the whole `fn_decl.return_type`). The trailing `(` is its own boundary,
// so (unlike fastc_replace_c_identifier) no end-boundary check is applied.
fn fastc_replace_call_needle(source string, needle string, replacement string) string {
	if needle == '' || needle == replacement || !source.contains(needle) {
		return source
	}
	mut out := strings.new_builder(source.len + replacement.len)
	mut start := 0
	for start < source.len {
		remaining := source[start..]
		relative := remaining.index(needle) or {
			out.write_string(remaining)
			break
		}
		index := start + relative
		before_blocks := index > 0 && (source[index - 1].is_alnum() || source[index - 1] in [
			`_`,
			`.`,
		])
		out.write_string(source[start..index])
		if before_blocks {
			out.write_string(needle)
		} else {
			out.write_string(replacement)
		}
		start = index + needle.len
	}
	return out.str()
}

fn fastc_replace_c_identifier(source string, identifier string, replacement string) string {
	if identifier == '' || identifier == replacement || !fastc_contains(source, identifier) {
		return source
	}
	mut out := strings.new_builder(source.len + replacement.len)
	mut start := 0
	for start < source.len {
		index := fastc_index_of(source, identifier, start)
		if index < 0 {
			unsafe { out.write_ptr(source.str + start, source.len - start) }
			break
		}
		end := index + identifier.len
		before_is_name := index > 0 && (source[index - 1].is_alnum() || source[index - 1] == `_`)
		after_is_name := end < source.len && (source[end].is_alnum() || source[end] == `_`)
		unsafe { out.write_ptr(source.str + start, index - start) }
		if before_is_name || after_is_name {
			out.write_string(identifier)
		} else {
			out.write_string(replacement)
		}
		start = end
	}
	return out.str()
}

// fastc_token_continues_expression reports whether a token appearing right after an
// auto-inserted `;` continues the previous expression (a binary/postfix operator),
// so the `;` is a line continuation rather than a statement boundary. The token
// equivalent of semicolon_continues_expression, used where only a lookahead scanner
// (not the source cursor) is available.
// streaming_call_argument_type returns the declared parameter type of the call argument the
// streaming reader is currently inside (the innermost unmatched `(` whose preceding token is a
// function/method name), or '' when the enclosing `(` is a grouping paren or the callee is
// unknown. Used to give an if/match-expression argument its parameter type mid-stream.
fn (g &Parser) streaming_call_argument_type(tokens []FastcExpressionToken) string {
	mut depth := 0
	mut open := -1
	for i := tokens.len - 1; i >= 0; i-- {
		match tokens[i].tok {
			.rpar, .rsbr, .rcbr {
				depth++
			}
			.lpar, .lsbr, .lcbr {
				if depth == 0 {
					open = i
					break
				}
				depth--
			}
			else {}
		}
	}
	if open <= 0 || tokens[open].tok != .lpar || tokens[open - 1].tok != .name {
		return ''
	}
	mut arg_index := 0
	mut inner := 0
	for i := open + 1; i < tokens.len; i++ {
		match tokens[i].tok {
			.lpar, .lsbr, .lcbr { inner++ }
			.rpar, .rsbr, .rcbr { inner-- }
			.comma {
				if inner == 0 { arg_index++ }
			}
			else {}
		}
	}
	name := tokens[open - 1].lit
	if open >= 2 && tokens[open - 2].tok == .dot {
		// `recv.method(…)`: resolve on the receiver's type; parameter_types[0] is the receiver.
		receiver_start := fastc_method_receiver_start(tokens, open - 2)
		receiver_tokens := tokens[receiver_start..open - 2]
		receiver_type := g.infer_expression_type(receiver_tokens) or { return '' }
		method_key, _ := g.resolve_method(receiver_type, name)
		signature := g.functions[method_key] or { return '' }
		idx := arg_index + 1
		return if idx < signature.parameter_types.len { signature.parameter_types[idx] } else { '' }
	}
	function_key := g.unqualified_function_key(name)
	signature := g.functions[function_key] or { return '' }
	return if arg_index < signature.parameter_types.len {
		signature.parameter_types[arg_index]
	} else {
		''
	}
}

fn fastc_token_continues_expression(tok token.Token) bool {
	// `.mul`/`.plus`/`.minus`/`.amp` are omitted: a leading `*`/`+`/`-`/`&` after an auto-`;`
	// is a unary prefix starting a new statement (deref/sign/address-of), not a binary
	// continuation. `.and` (`&&`) stays — it can only be binary.
	return tok in [.dot, .div, .mod, .and, .pipe, .logical_or, .xor, .lt, .le, .gt, .ge, .eq, .ne,
		.left_shift, .right_shift, .right_shift_unsigned, .question, .rpar, .key_or, .key_in, .key_is,
		.key_as]
}

fn (g &Parser) semicolon_continues_expression() bool {
	mut offset := g.s.offset
	for offset < g.s.src.len && g.s.src[offset] in [` `, `\t`, `\r`, `\n`] {
		offset++
	}
	if offset >= g.s.src.len {
		return false
	}
	if offset + 1 < g.s.src.len && g.s.src[offset] == `/` && g.s.src[offset + 1] in [
		`/`,
		`*`,
	] {
		return false
	}
	if g.in_enum_keyed_map_value && g.s.src[offset] == `.` {
		// The next `.field` starts the following map entry, not a member chain.
		return false
	}
	next := g.s.src[offset]
	if next in [`*`, `-`, `+`] {
		// A leading `*`/`-`/`+` after an auto-inserted `;` is a unary prefix (deref, sign)
		// beginning a NEW statement (`record(err)\n*default`), not a binary continuation — V
		// requires a binary operator at the previous line's END to continue.
		return false
	}
	if next == `&` {
		// `&&` continues a logical expression; a lone `&` is unary address-of (new statement).
		return offset + 1 < g.s.src.len && g.s.src[offset + 1] == `&`
	}
	return next in [`.`, `/`, `%`, `|`, `^`, `<`, `>`, `=`, `?`, `)`]
}

fn fastc_runtime_c_type(typ string) string {
	base := fastc_trim_pointer_suffix(typ)
	mut runtime_type := if base.starts_with('Map_') {
		'map'
	} else if base.starts_with('Array_') {
		'array'
	} else {
		base
	}
	return runtime_type + '*'.repeat(typ.len - base.len)
}

fn (mut g Parser) read_inferred_map_literal() !string {
	g.expect(.lcbr)!
	mut keys := []string{}
	mut values := []string{}
	mut key_type := ''
	mut value_type := ''
	// Enum-shorthand keys (`{ .field: v }`) carry no type of their own; recover the
	// key enum type from the declared enum that owns the first `.field`, and read
	// every key with it as the expected type so `.field` resolves.
	g.skip_semicolons()
	mut key_enum_type := ''
	if g.tok == .dot {
		// Shorthand key `.field`: recover the enum from the field name.
		mut lookahead := g.s
		if lookahead.scan() == .name {
			if enum_type := g.enum_field_types[lookahead.lit] {
				key_enum_type = enum_type
			}
		}
	} else if g.tok == .name {
		// Explicit key `EnumType.field` (the first entry often spells it out).
		mut lookahead := g.s
		if lookahead.scan() == .dot {
			if type_key := g.resolve_declared_type_key(g.lit) {
				if g.declared_kinds[type_key] == .enum_ {
					key_enum_type = fastc_c_declared_type_name(type_key)
				}
			}
		}
	}
	for g.tok != .rcbr {
		g.skip_semicolons()
		if g.tok == .rcbr {
			break
		}
		previous_expected := g.expected_expression_type
		if key_enum_type != '' {
			g.expected_expression_type = key_enum_type
		}
		key := g.read_expression([token.Token.colon])!
		g.expected_expression_type = previous_expected
		actual_key_type := if key_enum_type != '' {
			key_enum_type
		} else {
			fastc_normalize_inferred_type(g.last_expression_type)
		}
		g.expect(.colon)!
		// In an enum-keyed map the next entry begins with `.field` on a new line; a
		// value must not treat that `.` as a member-chain continuation.
		previous_enum_map := g.in_enum_keyed_map_value
		g.in_enum_keyed_map_value = key_enum_type != ''
		previous_value_expected := g.expected_expression_type
		if value_type != '' {
			g.expected_expression_type = value_type
		}
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		g.expected_expression_type = previous_value_expected
		g.in_enum_keyed_map_value = previous_enum_map
		actual_value_type := fastc_normalize_inferred_type(g.last_expression_type)
		if key_type == '' {
			key_type = actual_key_type
			value_type = actual_value_type
		}
		keys << key
		values << value
		if g.tok in [.comma, .semicolon] {
			g.next()
		}
	}
	g.expect(.rcbr)!
	mut map_type := ''
	if keys.len == 0 {
		map_type = fastc_normalize_inferred_type(g.expected_expression_type)
		expected_key_type, expected_value_type := g.map_key_value_types(map_type) or {
			return g.unsupported('empty inferred map literal')
		}
		key_type = expected_key_type
		value_type = expected_value_type
	} else if key_type == '' || value_type == '' {
		return g.unsupported('empty inferred map literal')
	}
	if map_type == '' {
		map_type = fastc_map_c_type(key_type, value_type)
	}
	hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
	map_name := g.temporary_name('map_literal')
	mut statements := [
		'map ${map_name} = builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn});',
	]
	for i, key in keys {
		key_name := g.temporary_name('map_key')
		value_name := g.temporary_name('map_value')
		statements << '${fastc_runtime_c_type(key_type)} ${key_name} = (${key});'
		statements << '${fastc_runtime_c_type(value_type)} ${value_name} = (${values[i]});'
		statements << 'builtin__map_set(&${map_name}, &${key_name}, &${value_name});'
	}
	g.last_expression_type = map_type
	g.last_expression = [
		FastcExpressionToken{
			tok: .name
			lit: map_name
			typ: map_type
		},
	]
	return '({ ${statements.join(' ')} ${map_name}; })'
}

// read_channel_receive lowers `<-ch` / `<-ch or { … }`. Channels are the type-erased
// `void*` stub, so this pops via the non-blocking `builtin__chan_try_pop` into a temp
// of the element type (recovered from the expected type or the channel's tracked
// element). The stub pop always "succeeds", so an `or` block is dead and is discarded.
fn (mut g Parser) read_channel_receive(stops []token.Token) !string {
	g.expect(.arrow)!
	mut operand_stops := stops.clone()
	if token.Token.key_or !in operand_stops {
		operand_stops << token.Token.key_or
	}
	expected := g.expected_expression_type
	g.expected_expression_type = ''
	channel := g.read_expression(operand_stops)!
	channel_tokens := g.last_expression.clone()
	g.expected_expression_type = expected
	mut element_type := fastc_normalize_inferred_type(expected)
	if element_type in ['', 'chan', 'Option', 'void'] {
		element_type = g.channel_element_type(channel_tokens)
	}
	if element_type == '' {
		element_type = 'int'
	}
	if g.tok == .key_or {
		g.next()
		if g.tok == .lcbr {
			g.skip_balanced(.lcbr, .rcbr)!
		}
	}
	recv_tmp := g.temporary_name('chan_recv')
	g.last_expression_type = element_type
	g.last_expression = [
		FastcExpressionToken{
			tok: .name
			lit: recv_tmp
			typ: element_type
		},
	]
	return '({ ${element_type} ${recv_tmp} = (${element_type}){0}; builtin__chan_try_pop((chan)(${channel}), &${recv_tmp}); ${recv_tmp}; })'
}

// channel_element_type recovers the element C type of the channel that `tokens`
// evaluates to: a bare local, or an `x.field` member access on a struct with a
// `chan` field. Returns '' when it cannot be determined.
fn (g &Parser) channel_element_type(tokens []FastcExpressionToken) string {
	if tokens.len == 1 && tokens[0].tok == .name {
		if local := g.locals[tokens[0].lit] {
			return local.chan_element_type
		}
	}
	if tokens.len >= 3 && tokens.last().tok == .name && tokens[tokens.len - 2].tok == .dot {
		receiver_type := g.infer_expression_type(tokens[..tokens.len - 2]) or { return '' }
		receiver_layout := fastc_trim_pointer_suffix(fastc_normalize_inferred_type(receiver_type))
		field_name := tokens.last().lit
		if fields := g.struct_field_info[receiver_layout] {
			for field in fields {
				if field.name == field_name {
					return field.chan_element_type
				}
			}
		}
	}
	return ''
}

fn (g &Parser) expected_call_argument_type(tokens []FastcExpressionToken) string {
	if tokens.len < 3 {
		return ''
	}
	mut depth := 0
	mut open_index := -1
	for i := tokens.len - 2; i >= 0; i-- {
		if tokens[i].tok == .rpar {
			depth++
		} else if tokens[i].tok == .lpar {
			if depth == 0 {
				open_index = i
				break
			}
			depth--
		}
	}
	if open_index <= 0 || tokens[open_index - 1].tok != .name {
		return ''
	}
	name_index := open_index - 1
	mut function_key := g.function_key_for_call(tokens, name_index)
	mut argument_offset := 0
	if name_index >= 2 && tokens[name_index - 1].tok == .dot && !(name_index == 2 && tokens[0].tok == .name && (tokens[0].lit in g.imports || tokens[0].lit == 'C')) {
		receiver_start := fastc_method_receiver_start(tokens, name_index - 1)
		receiver_type := g.infer_expression_type(tokens[receiver_start..name_index - 1]) or {
			return ''
		}
		function_key = '${g.semantic_type_key(receiver_type)}.${tokens[name_index].lit}'
		argument_offset = 1
	}
	signature := g.functions[function_key] or { return '' }
	mut argument_index := 0
	mut nested := 0
	for i in open_index + 1 .. tokens.len - 1 {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			nested++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			nested--
		} else if nested == 0 && tokens[i].tok == .comma {
			argument_index++
		}
	}
	parameter_index := argument_index + argument_offset
	if parameter_index >= signature.parameter_types.len {
		return ''
	}
	return signature.parameter_types[parameter_index]
}

// FastcTokenFlags records which token kinds an expression contains, so the
// renderer dispatch can skip lowerings whose trigger token is absent instead
// of letting each of them rescan the expression.
struct FastcTokenFlags {
	has_dot        bool
	has_lpar       bool
	has_lsbr       bool
	has_lcbr       bool
	has_logical    bool
	has_comparison bool
	has_binary     bool
	has_assignment bool
	has_not        bool
	has_plus       bool
}

// The guarded_* wrappers below skip a lowering when the expression lacks a
// token it necessarily acts on; each mirrors the first check of the renderer
// it wraps.
fn (g &Parser) guarded_overloaded_binary_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_binary {
		return none
	}
	return g.render_overloaded_binary_expression(tokens)
}

fn (g &Parser) guarded_struct_comparison_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_comparison {
		return none
	}
	return g.render_struct_comparison_expression(tokens)
}

fn (g &Parser) guarded_mixed_integer_comparison_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_comparison {
		return none
	}
	return g.render_mixed_integer_comparison_expression(tokens)
}

fn (g &Parser) guarded_enum_comparison_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_comparison {
		return none
	}
	return g.render_enum_comparison_expression(tokens)
}

fn (g &Parser) guarded_composed_string_concatenation(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_plus {
		return none
	}
	return g.render_composed_string_concatenation(tokens)
}

fn (g &Parser) guarded_map_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_lsbr && !flags.has_lcbr {
		return none
	}
	return g.render_map_expression(tokens)
}

fn (g &Parser) guarded_assignment_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_assignment {
		return none
	}
	return g.render_assignment_expression(tokens)
}

fn (g &Parser) guarded_nested_option_propagation(tokens []FastcExpressionToken, rendered_expression string, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_not {
		return none
	}
	return g.render_nested_option_propagation(tokens, rendered_expression)
}

fn (g &Parser) guarded_cast_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_lpar {
		return none
	}
	return g.render_cast_expression(tokens)
}

fn (g &Parser) guarded_pointer_member_access_expression(tokens []FastcExpressionToken, rendered_expression string, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_dot || !flags.has_lpar {
		return none
	}
	return g.render_pointer_member_access_expression(tokens, rendered_expression)
}

fn (g &Parser) guarded_struct_literal_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_lcbr {
		return none
	}
	return g.render_struct_literal_expression(tokens)
}

fn (g &Parser) guarded_logical_expression(tokens []FastcExpressionToken, flags FastcTokenFlags) ?FastcRenderedExpression {
	if !flags.has_logical {
		return none
	}
	return g.render_logical_expression(tokens)
}

// fastc_strip_paren_tokens removes fully-matching outer parentheses from a token slice.
fn fastc_strip_paren_tokens(tokens []FastcExpressionToken) []FastcExpressionToken {
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar && tokens[end - 1].tok == .rpar {
		mut depth := 0
		mut matches := true
		for i := start; i < end; i++ {
			if tokens[i].tok == .lpar {
				depth++
			} else if tokens[i].tok == .rpar {
				depth--
				if depth == 0 && i != end - 1 {
					matches = false
					break
				}
			}
		}
		if !matches {
			break
		}
		start++
		end--
	}
	return tokens[start..end]
}

// render_boolean_is_expression renders a boolean expression built from `is`/`!is` type
// tests whose left operand is not a bare name (`unalias(x) is Alias`, and `&&`/`||`
// combinations of such), which the simple `name is T` renderer cannot handle. Returns
// none for anything without such a test, leaving other renderers unaffected.
fn (g &Parser) render_boolean_is_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut has_is := false
	for item in tokens {
		if item.tok in [.key_is, .not_is] {
			has_is = true
			break
		}
	}
	if !has_is {
		return none
	}
	inner := fastc_strip_paren_tokens(tokens)
	if inner.len < 3 {
		return none
	}
	mut depth := 0
	for i, item in inner {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.and, .logical_or, .eq, .ne {
				if depth == 0 && i > 0 && i + 1 < inner.len {
					// `==`/`!=` only combine boolean `is` tests when an operand actually holds one
					// (`(a is T) != (b is T)`); a plain value comparison is left to the numeric/
					// enum/struct comparison paths.
					if item.tok in [.eq, .ne] && !fastc_expression_tokens_contain(inner[..i], .key_is) && !fastc_expression_tokens_contain(inner[..i], .not_is) && !fastc_expression_tokens_contain(inner[i + 1..], .key_is) && !fastc_expression_tokens_contain(inner[i + 1..], .not_is) {
						continue
					}
					left := g.render_boolean_is_operand(inner[..i]) or { return none }
					mut right := g.render_boolean_is_operand(inner[i + 1..]) or { return none }
					op := match item.tok {
						.and { '&&' }
						.logical_or { '||' }
						.eq { '==' }
						else { '!=' }
					}
					// `<call>() is T && … <call>().field …`: a smart-cast on a method-call
					// result cannot be tracked as a member path (detect_member_smartcasts owns
					// only names/chains), so narrow the SAME rendered call in the right conjunct
					// by reading its fields through the concrete variant's `_object`.
					if item.tok == .and {
						right = g.apply_call_is_narrowing(inner[..i], right)
					}
					return FastcRenderedExpression{
						source: '((${left}) ${op} (${right}))'
						typ: 'bool'
					}
				}
			}
			else {}
		}
	}
	depth = 0
	mut is_idx := -1
	for i, item in inner {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.key_is, .not_is {
				if depth == 0 && is_idx < 0 {
					is_idx = i
				}
			}
			else {}
		}
	}
	if is_idx <= 1 {
		// A bare-name left operand (`x is T`) is handled by the simpler renderer.
		return none
	}
	lhs_tokens := inner[..is_idx]
	// A call (`unalias(x) is T`), a member chain (`e.expr is T`, e.g. inside a match-arm
	// boolean value) or an indexed element (`ptypes[0] is T`, e.g. `arr.len > 0 && arr[0]
	// is T`) left operand is a plain boolean tag test here. A bare-name left operand
	// (`x is T`) is a smart-cast subject owned by detect_member_smartcasts, so leave it.
	mut lhs_is_boolean_test := false
	for item in lhs_tokens {
		if item.tok in [.lpar, .dot, .lsbr] {
			lhs_is_boolean_test = true
			break
		}
	}
	if !lhs_is_boolean_test {
		return none
	}
	lhs_type := fastc_normalize_inferred_type(g.infer_expression_type(lhs_tokens) or { return none })
	if !g.is_boxed_type(lhs_type) {
		return none
	}
	mut variant_c := ''
	if is_idx + 2 == inner.len && inner[is_idx + 1].tok == .name {
		if type_key := g.resolve_declared_type_key(inner[is_idx + 1].lit) {
			variant_c = fastc_c_declared_type_name(type_key)
		} else if fastc_primitive_c_type(inner[is_idx + 1].lit) != none {
			variant_c = inner[is_idx + 1].lit
		}
	} else if resolved := g.type_from_expression_tokens(inner[is_idx + 1..]) {
		variant_c = fastc_normalize_inferred_type(resolved).trim_right('*')
	}
	if variant_c == '' {
		return none
	}
	lhs_source := g.render_call_argument_expression(lhs_tokens, lhs_type) or { return none }
	access := if lhs_type.ends_with('*') { '->' } else { '.' }
	operator := if inner[is_idx].tok == .key_is { '==' } else { '!=' }
	return FastcRenderedExpression{
		source: '(((${lhs_source})${access}_typ) ${operator} __v_typeid_${variant_c})'
		typ: 'bool'
	}
}

fn (g &Parser) render_boolean_is_operand(tokens []FastcExpressionToken) ?string {
	if boolean_is := g.render_boolean_is_expression(tokens) {
		return boolean_is.source
	}
	return g.render_call_argument_expression(tokens, 'bool')
}

struct FastcCallIsNarrowing {
	box     string
	variant string
}

// render_call_is_narrowing returns the boxed subject render and concrete variant for a positive
// `<call>() is T` operand — a smart-cast whose subject is a method-call result, which the
// name/member-chain paths (detect_member_smartcasts) do not own. Returns none for `!is`, a
// pointer subject, or a bare-name/pure-member-chain subject.
fn (g &Parser) render_call_is_narrowing(tokens []FastcExpressionToken) ?FastcCallIsNarrowing {
	inner := fastc_strip_paren_tokens(tokens)
	mut depth := 0
	mut is_idx := -1
	for i, item in inner {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.key_is {
				if depth == 0 && is_idx < 0 {
					is_idx = i
				}
			}
			.not_is {
				if depth == 0 {
					return none
				}
			}
			else {}
		}
	}
	if is_idx <= 0 || is_idx + 1 >= inner.len {
		return none
	}
	lhs_tokens := inner[..is_idx]
	if fastc_indexed_member_chain_path(lhs_tokens) != none {
		return none
	}
	mut has_call := false
	for it in lhs_tokens {
		if it.tok == .lpar {
			has_call = true
			break
		}
	}
	if !has_call {
		return none
	}
	lhs_type := fastc_normalize_inferred_type(g.infer_expression_type(lhs_tokens) or { return none })
	if !g.is_boxed_type(lhs_type) || lhs_type.ends_with('*') {
		return none
	}
	mut variant_c := ''
	if is_idx + 2 == inner.len && inner[is_idx + 1].tok == .name {
		if type_key := g.resolve_declared_type_key(inner[is_idx + 1].lit) {
			variant_c = fastc_c_declared_type_name(type_key)
		} else if fastc_primitive_c_type(inner[is_idx + 1].lit) != none {
			variant_c = inner[is_idx + 1].lit
		}
	} else if resolved := g.type_from_expression_tokens(inner[is_idx + 1..]) {
		variant_c = fastc_normalize_inferred_type(resolved).trim_right('*')
	}
	if variant_c == '' {
		return none
	}
	box := g.render_call_argument_expression(lhs_tokens, lhs_type) or { return none }
	return FastcCallIsNarrowing{
		box: box
		variant: variant_c
	}
}

// apply_call_is_narrowing rewrites field reads of the boxed call subject of a `<call>() is T`
// left operand in the already-rendered `right` conjunct so they read the concrete variant `T`
// through `_object`. Returns `right` unchanged when the left is not such a narrowing.
fn (g &Parser) apply_call_is_narrowing(left_tokens []FastcExpressionToken, right string) string {
	narrowing := g.render_call_is_narrowing(left_tokens) or { return right }
	needle := '${narrowing.box}.'
	if !right.contains(needle) {
		return right
	}
	return right.replace(needle, '((${narrowing.variant} *)(${narrowing.box}._object))->')
}

// has_call_is_narrowing reports (shape-only, no rendering) whether `tokens` is a positive
// `<call>() is T` on a boxed value subject — the flavour of narrowing apply_call_is_narrowing
// handles. Used to gate the narrowing-boolean renderer on.
fn (g &Parser) has_call_is_narrowing(tokens []FastcExpressionToken) bool {
	inner := fastc_strip_paren_tokens(tokens)
	mut depth := 0
	mut is_idx := -1
	for i, item in inner {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.key_is {
				if depth == 0 && is_idx < 0 {
					is_idx = i
				}
			}
			.not_is {
				if depth == 0 {
					return false
				}
			}
			else {}
		}
	}
	if is_idx <= 0 {
		return false
	}
	lhs := inner[..is_idx]
	if fastc_indexed_member_chain_path(lhs) != none {
		return false
	}
	mut has_call := false
	for it in lhs {
		if it.tok == .lpar {
			has_call = true
			break
		}
	}
	if !has_call {
		return false
	}
	lhs_type := fastc_normalize_inferred_type(g.infer_expression_type(lhs) or { return false })
	return g.is_boxed_type(lhs_type) && !lhs_type.ends_with('*')
}

// fastc_top_level_boolean_split returns the first index of `op` (`&&` / `||`) that sits at
// bracket depth zero, or none when the operator does not appear at the top level.
fn fastc_top_level_boolean_split(tokens []FastcExpressionToken, op token.Token) ?int {
	mut depth := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			else {
				if depth == 0 && item.tok == op && i > 0 && i + 1 < tokens.len {
					return i
				}
			}
		}
	}
	return none
}

// conjunction_narrowing describes the smart-cast a positive `subject is T` conjunct imposes
// on the conjuncts to its right within an `&&` chain.
struct FastcConjunctionNarrowing {
	path      string
	smartcast FastcMemberSmartcast
}

// conjunction_narrowing returns the subject narrowing implied by a lone `subject is T`
// operand (`subject` a bare local or member chain), or none when the operand is not such a
// narrowing. `!is`, calls, and compound operands never narrow.
fn (g &Parser) conjunction_narrowing(tokens []FastcExpressionToken) ?FastcConjunctionNarrowing {
	inner := fastc_strip_paren_tokens(tokens)
	mut depth := 0
	mut is_idx := -1
	for i, item in inner {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.key_is {
				if depth == 0 && is_idx < 0 {
					is_idx = i
				}
			}
			.not_is, .and, .logical_or {
				if depth == 0 {
					return none
				}
			}
			else {}
		}
	}
	if is_idx < 1 {
		return none
	}
	subject_tokens := inner[..is_idx]
	mut has_index := false
	for item in subject_tokens {
		if item.tok == .lsbr {
			has_index = true
			break
		}
	}
	// The narrowed subject may be an indexed member chain (`node.args[0].expr is CallExpr`);
	// key it with the same `[]` markers render_member_receiver uses so the smart-cast is found
	// when `.is_method`/`.name` on that chain are rendered later.
	path := if has_index {
		fastc_indexed_member_chain_path(subject_tokens) or { return none }
	} else {
		for item in subject_tokens {
			if item.tok !in [.name, .dot] {
				return none
			}
		}
		fastc_member_chain_path(subject_tokens, 0, subject_tokens.len) or { return none }
	}
	mut subject_type := g.infer_expression_type(subject_tokens) or { '' }
	if member_type := g.infer_member_access_type(subject_tokens, 0, subject_tokens.len) {
		subject_type = member_type
	}
	if subject_type == '' {
		return none
	}
	if !g.is_boxed_type(fastc_normalize_inferred_type(subject_type)) {
		return none
	}
	mut variant_c := ''
	if is_idx + 2 == inner.len && inner[is_idx + 1].tok == .name {
		if type_key := g.resolve_declared_type_key(inner[is_idx + 1].lit) {
			variant_c = fastc_c_declared_type_name(type_key)
		} else if fastc_primitive_c_type(inner[is_idx + 1].lit) != none {
			variant_c = inner[is_idx + 1].lit
		}
	} else if resolved := g.type_from_expression_tokens(inner[is_idx + 1..]) {
		variant_c = fastc_normalize_inferred_type(resolved).trim_right('*')
	}
	if variant_c == '' {
		return none
	}
	// Render the subject WITHOUT any smart-cast already registered for this exact path, so a
	// re-evaluation (`x.left is Ident && … && x.left.method()` narrowed more than once) does not
	// wrap the subject in its own variant unwrap and produce a double `((T*)((T*)…_object)._object)`.
	mut w := unsafe { &Parser(g) }
	had_self := path in g.member_smartcasts
	saved_self := g.member_smartcasts[path] or { FastcMemberSmartcast{} }
	if had_self {
		w.member_smartcasts.delete(path)
	}
	subject_source := g.render_member_receiver(subject_tokens) or {
		if had_self {
			w.member_smartcasts[path] = saved_self
		}
		return none
	}
	if had_self {
		w.member_smartcasts[path] = saved_self
	}
	access := if subject_type.ends_with('*') { '->' } else { '.' }
	return FastcConjunctionNarrowing{
		path: path
		smartcast: FastcMemberSmartcast{
			typ: variant_c + '*'
			source: '((${variant_c} *)(${subject_source})${access}_object)'
		}
	}
}

// boolean_expression_has_narrowing reports whether a boolean `is` expression contains a
// top-level `&&` whose left conjunct narrows a subject (`x is T && x.f is U`), which the
// per-operand rendering cannot express because the right conjunct must see the narrowing.
fn (g &Parser) boolean_expression_has_narrowing(tokens []FastcExpressionToken) bool {
	inner := fastc_strip_paren_tokens(tokens)
	if inner.len < 3 {
		return false
	}
	if inner[0].tok == .not {
		return g.boolean_expression_has_narrowing(inner[1..])
	}
	if idx := fastc_top_level_boolean_split(inner, .logical_or) {
		return g.boolean_expression_has_narrowing(inner[..idx]) || g.boolean_expression_has_narrowing(inner[idx + 1..])
	}
	if idx := fastc_top_level_boolean_split(inner, .and) {
		// The left conjunct itself may be a parenthesized boolean that narrows internally
		// (`(k != .b || !(x.info is Struct && x.info.is_anon)) && …`), so recurse into it too —
		// not only test whether it is a bare `subject is T` narrowing.
		if g.conjunction_narrowing(inner[..idx]) != none || g.has_call_is_narrowing(inner[..idx]) || g.boolean_expression_has_narrowing(inner[..idx]) {
			return true
		}
		return g.boolean_expression_has_narrowing(inner[idx + 1..])
	}
	return false
}

// render_narrowing_boolean_expression lowers a boolean `is` expression whose `&&` chains
// carry flow-sensitive smart-casts (`e is UnsafeExpr && e.expr is Nil`). Each narrowing left
// conjunct registers a temporary member smart-cast so the operands to its right — rendered
// through the ordinary non-mutating renderers — resolve the narrowed subject. Returns none
// when no such narrowing is present, leaving the plain boolean path untouched.
fn (mut g Parser) render_narrowing_boolean_expression(tokens []FastcExpressionToken) ?string {
	mut has_is := false
	for item in tokens {
		if item.tok in [.key_is, .not_is] {
			has_is = true
			break
		}
	}
	if !has_is || !g.boolean_expression_has_narrowing(tokens) {
		return none
	}
	return g.render_narrowing_boolean_impl(tokens)
}

fn (mut g Parser) render_narrowing_boolean_operand(tokens []FastcExpressionToken) ?string {
	if compound := g.render_narrowing_boolean_impl(tokens) {
		return compound
	}
	// A `(subject as T).field` operand reads the narrowed subject through the concrete
	// variant; render it directly so the member `as` cast is not left for the raw renderer.
	if tokens.len > 0 && tokens[0].tok == .lpar {
		if as_member := g.render_as_cast_member_access(tokens) {
			return as_member.source
		}
	}
	// Any other embedded `(x as T)` cast in this operand (e.g. a method-call receiver
	// `(x as T).m()`) is lowered with the current narrowing active before the fallback.
	if rewritten := g.rewrite_embedded_as_casts(tokens) {
		return g.render_boolean_is_operand(rewritten)
	}
	return g.render_boolean_is_operand(tokens)
}

fn (mut g Parser) render_narrowing_boolean_impl(tokens []FastcExpressionToken) ?string {
	inner := fastc_strip_paren_tokens(tokens)
	if inner.len > 1 && inner[0].tok == .not && fastc_top_level_boolean_split(inner, .and) == none && fastc_top_level_boolean_split(inner, .logical_or) == none {
		// `!(a is T && a.f == …)`: the negation wraps the WHOLE expression (no top-level
		// `&&`/`||` sits outside it), and the `&&` inside still narrows its own right operand,
		// so recurse through the `!`. A `!x && y` (where `!` binds tighter than `&&`) is left to
		// the boolean split below, so the negation is not spread across the whole conjunction.
		operand := g.render_narrowing_boolean_impl(inner[1..]) or { return none }
		return '(!(${operand}))'
	}
	if idx := fastc_top_level_boolean_split(inner, .logical_or) {
		left := g.render_narrowing_boolean_operand(inner[..idx]) or { return none }
		right := g.render_narrowing_boolean_operand(inner[idx + 1..]) or { return none }
		return '((${left}) || (${right}))'
	}
	if idx := fastc_top_level_boolean_split(inner, .and) {
		left_tokens := inner[..idx]
		left := g.render_narrowing_boolean_operand(left_tokens) or { return none }
		if narrowing := g.conjunction_narrowing(left_tokens) {
			had := narrowing.path in g.member_smartcasts
			saved := g.member_smartcasts[narrowing.path] or { FastcMemberSmartcast{} }
			g.member_smartcasts[narrowing.path] = narrowing.smartcast
			right := g.render_narrowing_boolean_operand(inner[idx + 1..]) or {
				if had {
					g.member_smartcasts[narrowing.path] = saved
				} else {
					g.member_smartcasts.delete(narrowing.path)
				}
				return none
			}
			if had {
				g.member_smartcasts[narrowing.path] = saved
			} else {
				g.member_smartcasts.delete(narrowing.path)
			}
			return '((${left}) && (${right}))'
		}
		mut right := g.render_narrowing_boolean_operand(inner[idx + 1..]) or { return none }
		// A `<call>() is T` left operand narrows the SAME call in the right operand (a smart-cast
		// on a method-call result, which conjunction_narrowing does not own).
		right = g.apply_call_is_narrowing(left_tokens, right)
		return '((${left}) && (${right}))'
	}
	return none
}

fn (g &Parser) render_special_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len == 1 {
		if tokens[0].tok == .name {
			if local := g.locals[tokens[0].lit] {
				if local.is_reference {
					return FastcRenderedExpression{
						source: '*(${rendered_expression})'
						typ: local.typ.trim_right('*')
					}
				}
			}
		}
		return none
	}
	if tokens.len == 6 && tokens[0].tok == .key_sizeof {
		if c_sizeof := g.render_c_struct_sizeof(tokens) {
			return c_sizeof
		}
	}
	if tokens.len == 9 && tokens[0].tok == .amp {
		if interface_object := g.render_c_interface_object_address(tokens) {
			return interface_object
		}
	}
	mut has_typeof := false
	mut has_lpar := false
	mut has_binary := false
	mut has_comparison := false
	mut has_plus := false
	mut has_dot := false
	mut has_lsbr := false
	mut has_lcbr := false
	mut has_left_shift := false
	mut has_logical := false
	mut has_as := false
	mut has_assignment := false
	mut has_membership := false
	mut has_not := false
	for item in tokens {
		if item.tok.is_assignment() {
			has_assignment = true
		}
		if item.lit == 'typeof' {
			has_typeof = true
		}
		match item.tok {
			.dot {
				has_dot = true
			}
			.lpar {
				has_lpar = true
			}
			.lsbr {
				has_lsbr = true
			}
			.lcbr {
				has_lcbr = true
			}
			.plus {
				has_plus = true
				has_binary = true
			}
			.left_shift {
				has_left_shift = true
				has_binary = true
			}
			.minus, .mul, .div, .mod, .amp, .pipe, .xor, .right_shift, .right_shift_unsigned {
				has_binary = true
			}
			.and, .logical_or {
				has_binary = true
				has_comparison = true
				has_logical = true
			}
			.eq, .ne, .gt, .lt, .ge, .le, .key_is, .not_is {
				has_binary = true
				has_comparison = true
			}
			.key_in, .not_in {
				has_binary = true
				has_comparison = true
				has_membership = true
			}
			.key_as {
				has_as = true
			}
			.not {
				has_not = true
			}
			else {}
		}
	}
	flags := FastcTokenFlags{
		has_dot: has_dot
		has_lpar: has_lpar
		has_lsbr: has_lsbr
		has_lcbr: has_lcbr
		has_logical: has_logical
		has_comparison: has_comparison
		has_binary: has_binary
		has_assignment: has_assignment
		has_not: has_not
		has_plus: has_plus
	}
	if has_typeof {
		if type_name := g.render_typeof_name_expression(tokens) {
			return type_name
		}
		if type_reflection := g.render_typeof_generic_expression(tokens) {
			return type_reflection
		}
		if type_comparison := g.render_typeof_generic_comparison_expression(tokens) {
			return type_comparison
		}
	}
	if has_lpar && tokens.len > 0 && tokens.last().tok == .rpar {
		if disabled_call := g.render_disabled_call_expression(tokens) {
			return disabled_call
		}
	}
	is_print := tokens.len >= 4 && tokens[0].tok == .name && tokens[0].lit in ['print', 'println']
	if is_print {
		if enum_print := g.render_enum_print_expression(tokens) {
			return enum_print
		}
		if selfhost_print := g.render_selfhost_print_expression(tokens) {
			return selfhost_print
		}
		if bool_print := g.render_bool_print_expression(tokens) {
			return bool_print
		}
		if string_print := g.render_ordinary_string_print_expression(tokens) {
			return string_print
		}
	}
	// A common sum-type field read (`node.pos`, present in every variant) must be tried
	// before the member-smart-cast short-circuit below: when the receiver itself is a
	// smart-cast subject, render_member_receiver would otherwise splice a raw `.pos`
	// access onto the boxed sum-type struct, which has no such field.
	if g.selfhost && has_dot && tokens.len >= 3 && tokens.last().tok == .name && tokens[tokens.len - 2].tok == .dot {
		if common_field := g.render_sumtype_common_field_access(tokens) {
			return common_field
		}
	}
	// `&x.field` where `x.field` reads through a member smart-cast: the address must point at
	// the concrete-variant object (`(Var *)subject._object`), not the raw boxed field. Without
	// this the reference decays to `&subject.field`, a `ScopeObject *`, and later `obj.typ`
	// reads a field the box lacks. A member smart-cast already represents the narrowed field as
	// a pointer into the box (`typ` ends with `*`), so its read source IS the field's address —
	// return it directly; only a value-typed narrowing needs an explicit `&`.
	if g.selfhost && tokens.len > 2 && tokens[0].tok == .amp && has_dot && g.expression_uses_member_smartcast(tokens[1..]) {
		if member_source := g.render_member_receiver(tokens[1..]) {
			member_type := g.infer_member_access_type(tokens[1..], 0, tokens.len - 1) or { '' }
			if member_type.ends_with('*') {
				return FastcRenderedExpression{
					source: member_source
					typ: member_type
				}
			}
			return FastcRenderedExpression{
				source: '&(${member_source})'
				typ: if member_type != '' { member_type + '*' } else { 'voidptr' }
			}
		}
	}
	if g.selfhost && has_dot && g.expression_uses_member_smartcast(tokens) {
		if source := g.render_member_receiver(tokens) {
			if typ := g.infer_member_access_type(tokens, 0, tokens.len) {
				return FastcRenderedExpression{
					source: source
					typ: typ
				}
			}
		}
	}
	// A member access cast (`x.f as T`) must be lowered here, before the pointer /
	// method-access branches below claim the member chain — otherwise a cast whose
	// target shares a name with a FastC builtin (`MultiReturn`, `Chan`, ...) never
	// reaches the general `as` handling.
	if g.selfhost && has_as && has_dot {
		if as_expression := g.render_as_cast_expression(tokens) {
			return as_expression
		}
		// `(x as T).field`: the cast is parenthesized, then a field chain follows.
		if as_member := g.render_as_cast_member_access(tokens) {
			return as_member
		}
	}
	// An Option is represented by the generic C `Option` struct, so equality with
	// `none` must compare its state before generic binary/struct comparisons can
	// claim the expression and emit an invalid C struct comparison.
	if has_comparison && fastc_top_level_boolean_split(tokens, .and) == none
		&& fastc_top_level_boolean_split(tokens, .logical_or) == none {
		if option_comparison := g.render_option_none_comparison(tokens) {
			return option_comparison
		}
	}
	if has_binary {
		if overloaded_binary := g.guarded_overloaded_binary_expression(tokens, flags) {
			return overloaded_binary
		}
	}
	if g.selfhost && has_assignment && has_comparison && !has_lsbr {
		// `x.f = a == b` is an assignment whose RHS is a comparison. Assignment binds looser
		// than comparison, so split at the top-level `=` before the comparison handlers below
		// claim the `==` and emit `(x.f = a) == b`. Restricted to the assignment+comparison
		// case (and excluding index targets, which route through the map/array assignment
		// lowering) so ordinary assignments keep their existing handling.
		if assignment := g.render_assignment_expression(tokens) {
			return assignment
		}
	}
	if has_comparison {
		// A comparison whose operand reads a NESTED member smart-cast (`x.expr.obj.generic_typ
		// == 0`, where both `x.expr` and `x.expr.obj` are narrowed) must render its operands
		// through render_guard_comparison, whose render_comparison_operand walks the full chain
		// via render_member_receiver — the integer/struct comparison paths below only apply the
		// first-level narrowing spliced by the streaming reader. A call-bearing operand is left
		// to those paths (render_comparison_operand's pointer-member handling mis-lowers it).
		if g.selfhost && g.expression_uses_member_smartcast(tokens) && !fastc_expression_tokens_contain(tokens, .lpar) && fastc_top_level_boolean_split(tokens, .and) == none && fastc_top_level_boolean_split(tokens, .logical_or) == none {
			if guard := g.render_guard_comparison(tokens) {
				return FastcRenderedExpression{
					source: guard
					typ: 'bool'
				}
			}
		}
		// Render indexed comparison operands independently. Besides chained accesses
		// (`fields[0][0] == \`0\``), this ensures both sides of `s[i] > a[i]` use the
		// direct-array-access lowering instead of leaving the second string index raw.
		if g.selfhost && has_lsbr && !has_assignment && fastc_top_level_boolean_split(tokens, .and) == none && fastc_top_level_boolean_split(tokens, .logical_or) == none {
			if guard := g.render_guard_comparison(tokens) {
				return FastcRenderedExpression{
					source: guard
					typ: 'bool'
				}
			}
		}
		// A parenthesized membership used as a comparison operand (`(key in m) != known`)
		// must be lowered before the raw comparison renderer leaves V's `in` in C.
		if g.selfhost && has_membership && fastc_top_level_boolean_split(tokens, .and) == none
			&& fastc_top_level_boolean_split(tokens, .logical_or) == none {
			if guard := g.render_guard_comparison(tokens) {
				return FastcRenderedExpression{
					source: guard
					typ: 'bool'
				}
			}
		}
		// A comparison whose call operand propagates a result/option in one of its arguments
		// (`f(g()!) == 0`) must render that operand through render_guard_comparison so the
		// nested `!` is unwrapped rather than emitted as a raw C `!` on the returned Option.
		if g.selfhost && fastc_tokens_contain_nested_propagation(tokens) && fastc_top_level_boolean_split(tokens, .and) == none && fastc_top_level_boolean_split(tokens, .logical_or) == none {
			if guard := g.render_guard_comparison(tokens) {
				return FastcRenderedExpression{
					source: guard
					typ: 'bool'
				}
			}
		}
		if struct_comparison := g.guarded_struct_comparison_expression(tokens, flags) {
			return struct_comparison
		}
		if integer_comparison := g.guarded_mixed_integer_comparison_expression(tokens, flags) {
			return integer_comparison
		}
		if g.selfhost {
			if common_field_comparison := g.render_common_field_comparison_expression(tokens) {
				return common_field_comparison
			}
		}
	}
	if !g.selfhost {
		if has_comparison {
			if string_comparison := g.render_string_comparison_expression(tokens) {
				return string_comparison
			}
		}
		if has_plus {
			if concatenation := g.guarded_composed_string_concatenation(tokens, flags) {
				return concatenation
			}
		}
	}
	if g.selfhost {
		if tokens.len > 1 && tokens.last().tok == .not {
			if has_lsbr {
				if map_expression := g.guarded_map_expression(tokens, flags) {
					// A propagated RHS in `values[key] = load()!` must remain part of
					// the map assignment lowering, not become a raw C subexpression.
					return map_expression
				}
			}
			if assignment := g.guarded_assignment_expression(tokens, flags) {
				// Assignment supplies the target type to its RHS. In particular, an
				// option-returning call propagated with `!` is unwrapped recursively and
				// then boxed when the target itself is an Option field/local.
				return assignment
			}
		}
		if has_lpar {
			if interface_cast := g.render_interface_cast_expression(tokens, rendered_expression) {
				return interface_cast
			}
		}
		// An append RHS may itself use result propagation (`items << load()!`). Lower
		// the append first so nested propagation is applied to the RHS call rather than
		// returning a raw C expression that still contains V's `<<` operator.
		if has_left_shift {
			if append_expression := g.render_append_expression(tokens, rendered_expression) {
				return append_expression
			}
		}
		if tokens.len > 1 && tokens.last().tok == .not {
			// A string concatenation whose operands are `?`/`!`-propagated (`a()? + b()?`) must
			// lower through string_plus first; the generic nested-propagation replace below only
			// unwraps the trailing operand and would leave a raw C `+` between two strings.
			if has_plus {
				if concatenation := g.render_composed_string_concatenation(tokens) {
					return concatenation
				}
			}
			if nested_propagation := g.guarded_nested_option_propagation(tokens, rendered_expression, flags) {
				return nested_propagation
			}
		}
	}
	if has_lpar {
		if cast_expression := g.guarded_cast_expression(tokens, flags) {
			if pointer_members := g.guarded_pointer_member_access_expression(tokens, cast_expression.source, flags) {
				return pointer_members
			}
			return cast_expression
		}
	}
	if g.selfhost {
		if has_lsbr && has_lpar {
			if explicit_generic := g.render_explicit_generic_call_expression(tokens) {
				return explicit_generic
			}
		}
		if has_lpar {
			if defaulted_call := g.render_missing_call_arguments(tokens) {
				return defaulted_call
			}
		}
		if has_lsbr {
			if g.selfhost && has_as {
				// `arr[i] as T`: the `as` cast on an array-element (or map-value) receiver must
				// lower here, before the array/map-read branches below claim the `[…]` and leave
				// the trailing `as T` as raw C.
				if as_expression := g.render_as_cast_expression(tokens) {
					return as_expression
				}
			}
			if g.selfhost && tokens.len >= 2 && tokens[0].tok == .lsbr {
				// An array LITERAL whose element embeds a map read (`[m[k]]`) must lower as a whole
				// `new_array`; render_embedded_map_reads below would rewrite only the inner read and
				// leave the outer `[…]` as invalid raw C. render_array_literal_argument returns none
				// unless the tokens truly are `[ … ]`, so it is safe to try before the map paths.
				mut array_type := g.infer_expression_type(tokens) or { '' }
				if !array_type.starts_with('Array_') && !array_type.starts_with('FixedArray_') && (g.expected_expression_type.starts_with('Array_') || g.expected_expression_type.starts_with('FixedArray_')) {
					array_type = fastc_trim_pointer_suffix(g.expected_expression_type)
				}
				if array_literal := g.render_array_literal_argument(tokens, array_type) {
					return array_literal
				}
			}
			if tokens.last().tok in [.inc, .dec] {
				if map_inc := g.render_map_index_inc_dec_expression(tokens) {
					return map_inc
				}
			}
			if map_expression := g.guarded_map_expression(tokens, flags) {
				return map_expression
			}
			// An outer array/string index whose INDEX is itself a map read (`arr[m[k]]`) must be
			// lowered as a whole array access — its index renderer already lowers the inner map
			// read to a value. render_embedded_map_reads below would otherwise rewrite only the
			// inner read and leave the outer `arr[…]` as a raw C index on the array struct.
			// render_array_access returns none unless the whole expression is such an access.
			if array_access := g.render_array_access_expression(tokens) {
				return array_access
			}
			if has_comparison {
				// `m[k] == .enum_`: resolve the enum-shorthand comparison against the map value type
				// here — render_embedded_map_reads below would rewrite only the `m[k]` read and leave
				// the raw `.enum_` shorthand as invalid C.
				if enum_comparison := g.render_enum_comparison_expression(tokens) {
					return enum_comparison
				}
			}
			if !has_assignment && !has_membership && !has_logical {
				if embedded_map := g.render_embedded_map_reads(tokens) {
					return embedded_map
				}
			}
		}
		if has_lcbr {
			if struct_literal := g.guarded_struct_literal_expression(tokens, flags) {
				return struct_literal
			}
			if struct_literal := g.render_struct_literal_field_names(tokens, rendered_expression) {
				return struct_literal
			}
		}
		if has_assignment && has_lcbr {
			if initializer_assignment := g.render_initializer_assignment_expression(tokens) {
				return initializer_assignment
			}
		}
		if has_assignment {
			if has_lsbr {
				if array_assignment := g.render_array_assignment_expression(tokens) {
					return array_assignment
				}
			}
			if assignment := g.guarded_assignment_expression(tokens, flags) {
				return assignment
			}
		}
		if has_dot && has_lpar && tokens.last().tok == .rpar {
			// `arr.map/filter/any/all/count(it…)` used as a sub-expression (call argument,
			// membership collection, struct-field value): the streaming reader only lowers
			// these magic closure methods when they front the whole statement, so lower a
			// trailing one here too instead of leaving a raw `array_map(arr, it.f)`.
			if higher_order := g.render_higher_order_method_expression(tokens) {
				return higher_order
			}
		}
		if has_dot && has_lpar && (tokens.len == 0 || tokens.last().tok != .not) {
			if static_call := g.render_static_call_expression(tokens, rendered_expression) {
				return static_call
			}
		}
		if has_logical {
			if logical := g.guarded_logical_expression(tokens, flags) {
				return logical
			}
		}
		if tokens.len > 1 && tokens[0].tok == .not {
			inner := g.render_call_argument_expression(tokens[1..], 'bool') or { return none }
			return FastcRenderedExpression{
				source: '!(${inner})'
				typ: 'bool'
			}
		}
		if has_comparison {
			if g.selfhost {
				if nil_comparison := g.render_nil_comparison(tokens) {
					return nil_comparison
				}
			}
			if enum_comparison := g.guarded_enum_comparison_expression(tokens, flags) {
				return enum_comparison
			}
			// `f(x) is Variant` (a call left operand, possibly combined with `&&`/`||`) is a
			// boolean tag test, not something the smart-cast reader can shadow.
			if boolean_is := g.render_boolean_is_expression(tokens) {
				return boolean_is
			}
			if string_comparison := g.render_string_comparison_expression(tokens) {
				return string_comparison
			}
			// A comparison whose operand is a plain call (`node_kind_id(child) != 75`) must be
			// lowered through render_guard_comparison so the call's arguments receive their
			// value/pointer coercions (auto-deref of a `&T` argument for a by-value parameter);
			// the raw fallback below would stream the call verbatim and leave the mismatch.
			if g.selfhost && fastc_comparison_operand_is_plain_call(tokens) && !g.expression_uses_member_smartcast(tokens) && fastc_top_level_boolean_split(tokens, .and) == none && fastc_top_level_boolean_split(tokens, .logical_or) == none {
				if guard := g.render_guard_comparison(tokens) {
					return FastcRenderedExpression{
						source: guard
						typ: 'bool'
					}
				}
			}
		}
		if has_plus && !has_membership {
			// `+` binds tighter than `in`, so a top-level membership (`'@' + name in tokens`) owns the
			// whole expression; without this gate the concat renderer wrongly splits at `+` and treats
			// `name in tokens` (a bool) as a string operand. The membership handler below renders the
			// concatenated subject itself.
			if concatenation := g.guarded_composed_string_concatenation(tokens, flags) {
				return concatenation
			}
		}
		if g.selfhost {
			// `x()?.field` / `x()?.method(a)`: the `?`/`!` propagates the option and the trailing
			// member chain applies to the unwrapped value, so lower it before the raw fallback
			// leaves a stray C `?`/`!` between the call and the member access.
			if propagation_member := g.render_propagation_before_member(tokens) {
				return propagation_member
			}
		}
		if tokens.len > 1 && tokens.last().tok == .not && rendered_expression.ends_with('!') && !fastc_trailing_not_marks_fixed_array_literal(tokens) {
			inner_tokens := tokens[..tokens.len - 1]
			mut inner_source := rendered_expression[..rendered_expression.len - 1]
			if explicit_generic := g.render_explicit_generic_call_expression(inner_tokens) {
				inner_source = explicit_generic.source
			} else if static_expression := g.render_static_call_expression(inner_tokens, inner_source) {
				inner_source = static_expression.source
			} else if method_expression := g.render_method_call_expression(inner_tokens, inner_source) {
				inner_source = method_expression.source
			} else if array_expression := g.render_array_access_expression(inner_tokens) {
				inner_source = array_expression.source
			} else if defaulted_call := g.render_missing_call_arguments(inner_tokens) {
				inner_source = defaulted_call.source
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
				typ: value_type
			}
		}
		if has_membership {
			// A fully-parenthesized membership (`(x in m)`, as in `for (candidate in used) {`)
			// keeps its `in` at paren depth 1 where the depth-0 scan below misses it; render the
			// stripped inner expression so the membership lowers, then restore the grouping.
			stripped := fastc_strip_paren_tokens(tokens)
			if stripped.len < tokens.len && (fastc_expression_tokens_contain(stripped, .key_in) || fastc_expression_tokens_contain(stripped, .not_in)) {
				raw_stripped := g.render_raw_expression_tokens(stripped) or { rendered_expression }
				if inner := g.render_special_expression(stripped, raw_stripped) {
					return FastcRenderedExpression{
						source: '(${inner.source})'
						typ: inner.typ
					}
				}
			}
			mut depth := 0
			for i, item in tokens {
				if item.tok in [.lpar, .lsbr, .lcbr] {
					depth++
				} else if item.tok in [.rpar, .rsbr, .rcbr] {
					depth--
				} else if depth == 0 && item.tok in [.key_in, .not_in] && i > 0 && i + 1 < tokens.len {
					temporary_namespace := g.temporary_namespace('membership')
					right_tokens := tokens[i + 1..]
					// `x in [Type1, Type2, …]` where `x` is a boxed sum type is a membership
					// over the runtime type tag (V's `x is Type1 || x is Type2 || …`), not a
					// value search. The list elements are declared type names, not values.
					left_boxed_type := fastc_normalize_inferred_type(g.infer_expression_type(tokens[..i]) or {
						''
					})
					if g.is_boxed_type(left_boxed_type) && right_tokens.len >= 2 && right_tokens[0].tok == .lsbr && right_tokens.last().tok == .rsbr {
						if elements := fastc_expression_list_items(right_tokens, 1, right_tokens.len - 1) {
							mut type_ids := []string{}
							for element in elements {
								type_key := g.type_from_expression_tokens(element) or { break }
								type_ids << fastc_normalize_inferred_type(type_key).trim_right('*')
							}
							if type_ids.len == elements.len && type_ids.len > 0 {
								subject := g.render_call_argument_expression(tokens[..i], left_boxed_type) or {
									return none
								}
								access := if left_boxed_type.ends_with('*') { '->' } else { '.' }
								subject_name := '${temporary_namespace}_subject'
								mut checks := []string{}
								for type_id in type_ids {
									checks << '(${subject_name}${access}_typ == __v_typeid_${type_id})'
								}
								joined := checks.join(' || ')
								predicate := if item.tok == .not_in {
									'!(${joined})'
								} else {
									'(${joined})'
								}
								return FastcRenderedExpression{
									source: '({ ${left_boxed_type} ${subject_name} = (${subject}); ${predicate}; })'
									typ: 'bool'
								}
							}
						}
					}
					right_type := g.infer_expression_type(right_tokens) or {
						// A bare array literal of enum shorthands (`x !in [.a, .b]`) can be
						// impossible to infer on its own: with the full builtin loaded several
						// enums share member names like `struct`/`interface`, so the list has
						// no unambiguous element type. Fall through to the array-literal path
						// below, which derives the element type from the left operand instead.
						if right_tokens.len >= 2 && right_tokens[0].tok == .lsbr && right_tokens.last().tok == .rsbr {
							''
						} else {
							return none
						}
					}
					if right_type != '' && g.underlying_alias_type(right_type) == 'string' {
						left_type := g.infer_expression_type(tokens[..i]) or { return none }
						if g.underlying_alias_type(left_type) != 'string' {
							return none
						}
						substring := g.render_membership_candidate(tokens[..i], 'string') or {
							return none
						}
						value := g.render_call_argument_expression(right_tokens, right_type) or {
							return none
						}
						substring_name := '${temporary_namespace}_substring'
						value_name := '${temporary_namespace}_value'
						found := 'v_fastc_string_contains(${value_name}, ${substring_name})'
						predicate := if item.tok == .not_in { '!(${found})' } else { found }
						return FastcRenderedExpression{
							source: '({ string ${substring_name} = (${substring}); string ${value_name} = (${value}); ${predicate}; })'
							typ: 'bool'
						}
					}
					if fastc_trim_pointer_suffix(right_type).starts_with('Map_') {
						key_type, _ := g.map_key_value_types(right_type) or { return none }
						key_source := g.render_membership_candidate(tokens[..i], key_type) or {
							return none
						}
						map_source := g.render_member_receiver(right_tokens) or { return none }
						map_expression := if right_type.ends_with('*') {
							map_source
						} else {
							'&(${map_source})'
						}
						key_name := '${temporary_namespace}_map_key'
						found := 'builtin__map_get_check((map *)${map_expression}, &${key_name}) != NULL'
						predicate := if item.tok == .not_in { '!(${found})' } else { found }
						return FastcRenderedExpression{
							source: '({ ${fastc_runtime_c_type(key_type)} ${key_name} = (${key_source}); ${predicate}; })'
							typ: 'bool'
						}
					}
					right_layout := fastc_trim_pointer_suffix(right_type)
					if right_layout.starts_with('Array_') || right_layout.starts_with('FixedArray_') {
						element_type := g.array_element_type(right_type) or { return none }
						candidate := g.render_membership_candidate(tokens[..i], element_type) or {
							return none
						}
						collection := g.render_call_argument_expression(right_tokens, right_type) or {
							return none
						}
						item_name := '${temporary_namespace}_item'
						collection_name := '${temporary_namespace}_collection'
						found_name := '${temporary_namespace}_found'
						index_name := '${temporary_namespace}_index'
						access := if right_type.ends_with('*') { '->' } else { '.' }
						collection_length := if fixed_length := fastc_fixed_array_length(right_layout) {
							fixed_length
						} else {
							'${collection_name}${access}len'
						}
						right_element := '((${element_type} *)${collection_name}${access}data)[${index_name}]'
						mut comparison := '(${item_name} == ${right_element})'
						if g.underlying_alias_type(element_type).trim_right('*') == 'string' {
							comparison = 'builtin__string_eq(${item_name}, ${right_element})'
						} else if fastc_trim_pointer_suffix(g.underlying_alias_type(element_type)).starts_with('Array_') {
							// Array elements (`types in [][]Type`) cannot be compared with C `==`;
							// compare the two arrays element-wise inline.
							comparison = g.fastc_inline_array_element_equality(element_type, item_name, right_element) or { return none }
						} else if g.struct_equality_is_supported(element_type, []) {
							// Struct elements (`typ in []StrType`) cannot be compared with C `==`; compare them
							// field-wise, like `struct == struct`.
							comparison = g.struct_equality_source(item_name, right_element, element_type, [])
						}
						// A hoisted predicate keeps this interpolation flat: the FastC
						// selfhost parser renders nested `${if ... { '${...}' }}` blocks
						// literally, corrupting the emitted membership expression.
						predicate := if item.tok == .not_in { '!${found_name}' } else { found_name }
						// A collection lowered to a `({ … for … })` statement-expression (e.g.
						// `arr.map(it.f)`) cannot be `__typeof__`'d by TinyCC; name its known array
						// type directly instead.
						collection_c_type := if collection.starts_with('({') {
							right_type
						} else {
							'__typeof__((${collection}))'
						}
						return FastcRenderedExpression{
							source: '({ ${element_type} ${item_name} = (${candidate}); ${collection_c_type} ${collection_name} = (${collection}); bool ${found_name} = false; for (int ${index_name} = 0; ${index_name} < ${collection_length}; ${index_name}++) { if (${comparison}) { ${found_name} = true; break; } } ${predicate}; })'
							typ: 'bool'
						}
					}
					array_end := if tokens.last().tok == .not { tokens.len - 1 } else { tokens.len }
					if i + 2 >= array_end || tokens[i + 1].tok != .lsbr || tokens[array_end - 1].tok != .rsbr {
						continue
					}
					lhs_type := g.infer_expression_type(tokens[..i]) or { return none }
					items := fastc_expression_list_items(tokens, i + 2, array_end - 1) or {
						return none
					}
					if items.len == 0 {
						return FastcRenderedExpression{
							source: if item.tok == .key_in { 'false' } else { 'true' }
							typ: 'bool'
						}
					}
					lhs_source := g.render_membership_candidate(tokens[..i], lhs_type) or {
						return none
					}
					lhs_name := '${temporary_namespace}_subject'
					value_type := fastc_runtime_c_type(fastc_normalize_inferred_type(lhs_type))
					mut initializers := []string{cap: items.len}
					mut comparisons := []string{cap: items.len}
					for candidate_index, candidate in items {
						candidate_source := g.render_membership_candidate(candidate, lhs_type) or {
							return none
						}
						candidate_name := '${temporary_namespace}_candidate_${candidate_index}'
						initializers << '${value_type} ${candidate_name} = (${candidate_source});'
						comparison := if g.underlying_alias_type(lhs_type).trim_right('*') == 'string' {
							'builtin__string_eq(${lhs_name}, ${candidate_name})'
						} else {
							'((${lhs_name}) == (${candidate_name}))'
						}
						comparisons << if item.tok == .key_in {
							comparison
						} else {
							'!${comparison}'
						}
					}
					joiner := if item.tok == .key_in { ' || ' } else { ' && ' }
					return FastcRenderedExpression{
						source: '({ ${value_type} ${lhs_name} = (${lhs_source}); ${initializers.join(' ')} (${comparisons.join(joiner)}); })'
						typ: 'bool'
					}
				}
			}
		}
		if has_lsbr {
			if array_type := g.infer_expression_type(tokens) {
				if array_literal := g.render_array_literal_argument(tokens, array_type) {
					return array_literal
				}
			}
		}
		// Resolve a complete index expression before the generic pointer/member
		// rewriter gets a chance to treat its base as part of a longer chain.
		if has_lsbr {
			if array_access := g.render_array_access_expression(tokens) {
				return array_access
			}
			// An index embedded in a larger expression (`node.items()[0].field > 0`)
			// must be lowered before pointer-member rewriting can return the partially
			// rewritten method call with raw C indexing still attached.
			if nested_array := g.render_nested_array_access_expression(tokens, rendered_expression) {
				mut nested_source := nested_array.source
				mut nested_type := nested_array.typ
				if has_dot && has_lpar {
					if method_expression := g.render_method_call_expression(tokens, nested_array.source) {
						nested_source = method_expression.source
						nested_type = method_expression.typ
					}
				}
				if has_dot {
					if pointer_members := g.render_pointer_member_access_expression(tokens, nested_source) {
						return pointer_members
					}
				}
				return FastcRenderedExpression{
					source: nested_source
					typ: nested_type
				}
			}
		}
		if has_dot {
			if pointer_members := g.render_pointer_member_access_expression(tokens, rendered_expression) {
				if has_lsbr {
					if nested_array := g.render_nested_array_access_expression(tokens, pointer_members.source) {
						return nested_array
					}
				}
				return pointer_members
			}
		}
		if has_dot && has_lpar {
			if method_expression := g.render_method_call_expression(tokens, rendered_expression) {
				if array_expression := g.render_nested_array_access_expression(tokens, method_expression.source) {
					return FastcRenderedExpression{
						source: array_expression.source
						typ: method_expression.typ
					}
				}
				if array_type := g.infer_expression_type(tokens) {
					if array_literal := g.render_array_literal_argument(tokens, array_type) {
						return array_literal
					}
				}
				if pointer_members := g.render_pointer_member_access_expression(tokens, method_expression.source) {
					return pointer_members
				}
				return method_expression
			}
		}
		if has_lpar {
			if defaulted_call := g.render_missing_call_arguments(tokens) {
				return defaulted_call
			}
		}
		if has_lsbr {
			if array_expression := g.render_nested_array_access_expression(tokens, rendered_expression) {
				return array_expression
			}
		}
	}
	if g.selfhost && tokens.len >= 3 && tokens[0].tok == .name && tokens[1].tok in [
		.key_is,
		.not_is,
	] {
		// `x is T` tests the boxed subject's tag, so the `._typ`/`->_typ` access is
		// decided by the local's own (boxed) type — never a member smart-cast that a
		// sibling guard in the same condition registered on the same name. A folded
		// `(x as T).field` group arrives as a synthetic token carrying its C text in `.source`
		// and its type in `.typ` (an empty `.lit`), so read those directly for it.
		is_synth_subject := tokens[0].source != ''
		lhs_type := if is_synth_subject && tokens[0].typ != '' {
			fastc_normalize_inferred_type(tokens[0].typ)
		} else if local := g.locals[tokens[0].lit] {
			local.typ
		} else {
			g.infer_expression_type(tokens[..1]) or { return none }
		}
		mut variant_c := ''
		if tokens.len == 3 && tokens[2].tok == .name {
			if type_key := g.resolve_declared_type_key(tokens[2].lit) {
				variant_c = fastc_c_declared_type_name(type_key)
			} else if fastc_primitive_c_type(tokens[2].lit) != none {
				// A primitive sum-type variant (`v is u64`) is not a declared type; its
				// own spelling is the `__v_typeid_` suffix (see decl.v's primitive ids).
				variant_c = tokens[2].lit
			}
		} else if resolved_target := g.type_from_expression_tokens(tokens[2..]) {
			// Qualified (`err is io.Eof`) and composite (`x is []string`) targets both
			// lower to their concrete C spelling and its generated `__v_typeid_` tag.
			target := fastc_normalize_inferred_type(resolved_target)
			variant_c = target.trim_right('*')
		}
		if variant_c == '' {
			return none
		}
		access := if lhs_type.ends_with('*') { '->' } else { '.' }
		operator := if tokens[1].tok == .key_is { '==' } else { '!=' }
		subject := if is_synth_subject { tokens[0].source } else { tokens[0].lit }
		return FastcRenderedExpression{
			source: '((${subject}${access}_typ) ${operator} __v_typeid_${variant_c})'
			typ: 'bool'
		}
	}
	if g.selfhost && has_as {
		if as_expression := g.render_as_cast_expression(tokens) {
			return as_expression
		}
	}
	if g.selfhost && tokens.len >= 3 && tokens.last().tok == .name && tokens[tokens.len - 2].tok == .dot && tokens.last().lit in [
		'len',
		'cap',
		'closed',
	] {
		// The erased `void*` chan stub has no fields. Channels are non-functional in
		// this scanner lane, so report the empty/open stub state.
		receiver_type := g.infer_expression_type(tokens[..tokens.len - 2]) or { '' }
		if fastc_trim_pointer_suffix(fastc_normalize_inferred_type(receiver_type)) == 'chan' {
			return FastcRenderedExpression{
				source: if tokens.last().lit == 'closed' { 'false' } else { '0' }
				typ: if tokens.last().lit == 'closed' { 'bool' } else { 'int' }
			}
		}
	}
	if g.selfhost && has_lcbr {
		mut init_open := -1
		for i, item in tokens {
			if item.tok == .lcbr {
				init_open = i
				break
			}
		}
		if init_open > 0 && tokens.last().tok == .rcbr {
			if array_type := g.array_initializer_type(tokens[..init_open]) {
				if array_type.starts_with('FixedArray_') && rendered_expression.contains('{.init=') {
					length := fastc_fixed_array_length(array_type) or { return none }
					marker := '{.init='
					marker_index := rendered_expression.index(marker) or { return none }
					value := rendered_expression[marker_index + marker.len..rendered_expression.len - 1]
					return FastcRenderedExpression{
						source: rendered_expression[..marker_index] + '{.data={ [0 ... ${length} -\n\t\t\t\t\t\t\t1] = ' + value + '}}'
						typ: array_type
					}
				}
				return FastcRenderedExpression{
					source: rendered_expression
					typ: array_type
				}
			}
		}
	}
	array_end := if tokens.len > 0 && tokens.last().tok == .not {
		tokens.len - 1
	} else {
		tokens.len
	}
	if g.selfhost && array_end == 2 && tokens[0].tok == .lsbr && tokens[1].tok == .rsbr && g.expected_expression_type.trim_right('*').starts_with('Array_') {
		// A dynamic empty array needs a real header carrying `element_size`, otherwise a later
		// `<<` push copies `len * 0` bytes into a NULL buffer and silently drops the elements.
		array_type := fastc_trim_pointer_suffix(g.expected_expression_type)
		if element_type := g.array_element_type(array_type) {
			mut w := unsafe { &Parser(g) }
			fastc_register_composite_type(array_type, mut w.composite_types)
			return FastcRenderedExpression{
				source: '((${array_type})builtin____new_array(0, 0, sizeof(${fastc_normalize_inferred_type(element_type)})))'
				typ: g.expected_expression_type
			}
		}
		return FastcRenderedExpression{
			source: '(${g.expected_expression_type}){0}'
			typ: g.expected_expression_type
		}
	}
	if g.selfhost && array_end >= 2 && tokens[0].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
		items := fastc_expression_list_items(tokens, 1, array_end - 1) or { return none }
		if items.len == 0 {
			return none
		}
		element_type := g.infer_expression_type(items[0]) or { return none }
		if element_type == '' {
			return none
		}
		array_type := fastc_array_c_type(fastc_normalize_inferred_type(element_type))
		mut w := unsafe { &Parser(g) }
		fastc_register_composite_type(array_type, mut w.composite_types)
		mut rendered_items := []string{cap: items.len}
		for item in items {
			rendered_items << g.render_call_argument_expression(item, element_type) or {
				return none
			}
		}
		return FastcRenderedExpression{
			source: '((${array_type})builtin__new_array_from_c_array(${items.len}, ${items.len}, sizeof(${fastc_normalize_inferred_type(element_type)}), (${fastc_normalize_inferred_type(element_type)}[]){${rendered_items.join(',')}}))'
			typ: array_type
		}
	}
	if g.selfhost && tokens.len > 0 && tokens.len % 2 == 1 && (has_plus || (tokens.len == 1 && tokens[0].tok == .string)) {
		mut is_literal_concat := true
		mut literals := strings.new_builder(rendered_expression.len)
		for i, item in tokens {
			if i % 2 == 0 {
				if item.tok != .string {
					is_literal_concat = false
					break
				}
				literal := fastc_c_string(item.lit) or {
					is_literal_concat = false
					break
				}
				literals.write_string(literal)
			} else if item.tok != .plus {
				is_literal_concat = false
				break
			}
		}
		if is_literal_concat {
			return FastcRenderedExpression{
				source: '_S(${literals.str()})'
				typ: 'string'
			}
		}
	}
	if g.selfhost && has_plus {
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
						string_operands << operand_type == 'string'
						operand_start = i + 1
						plus_count++
					}
				}
				else {}
			}
		}
		if plus_count > 0 {
			last_operand_type := g.infer_expression_type(tokens[operand_start..]) or { '' }
			string_operands << last_operand_type == 'string'
			if fastc_all_true(string_operands) {
				parts := fastc_split_top_level_c_plus(rendered_expression)
				if parts.len == plus_count + 1 {
					mut combined := parts[0]
					for part in parts[1..] {
						combined = 'builtin__string_plus(${combined},${part})'
					}
					return FastcRenderedExpression{
						source: combined
						typ: 'string'
					}
				}
			}
		}
	}
	if has_dot && has_lpar {
		return g.render_flag_method_expression(tokens, rendered_expression)
	}
	return none
}

fn (g &Parser) expression_uses_member_smartcast(tokens []FastcExpressionToken) bool {
	if g.member_smartcasts.len == 0 {
		return false
	}
	// A smart-cast subject can appear anywhere in the expression, not only at its start
	// (`!left.is_blank()`, `a && x.f`). Scan every chain-start name (one not preceded by a
	// `.`) and its member chain.
	for i, item in tokens {
		if item.tok != .name || (i > 0 && tokens[i - 1].tok == .dot) {
			continue
		}
		mut path := item.lit
		if path in g.member_smartcasts {
			return true
		}
		mut index := i + 1
		for index < tokens.len {
			if tokens[index].tok == .lsbr {
				// An array index segment keys as the `[]` marker, matching render_member_receiver
				// (`right.args[0].expr` → `right.args[].expr`).
				close := fastc_matching_delimiter(tokens, index, .lsbr, .rsbr) or { break }
				path += '[]'
				if path in g.member_smartcasts {
					return true
				}
				index = close + 1
				continue
			}
			if index + 1 < tokens.len && tokens[index].tok == .dot && tokens[index + 1].tok == .name {
				path += '.' + tokens[index + 1].lit
				if path in g.member_smartcasts {
					return true
				}
				index += 2
				continue
			}
			break
		}
	}
	return false
}
