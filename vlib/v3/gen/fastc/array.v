module fastc

import strings
import v3.token

fn (g &Parser) fixed_array_uses_raw_storage(tokens []FastcExpressionToken) bool {
	if tokens.len == 1 {
		return fastc_global_key(g.module_name, tokens[0].lit) in g.globals
	}
	if tokens.len >= 3 && tokens[tokens.len - 2].tok == .dot && tokens.last().tok == .name {
		return true
	}
	if tokens.len >= 4 && tokens.last().tok == .rsbr {
		mut depth := 0
		for i := tokens.len - 1; i >= 0; i-- {
			if tokens[i].tok == .rsbr {
				depth++
			} else if tokens[i].tok == .lsbr {
				depth--
				if depth == 0 {
					// A fixed array nested inside a raw struct field remains raw C array
					// storage after indexing (`s[0]` in `u32 s[4][256]`).
					return i > 0 && g.fixed_array_uses_raw_storage(tokens[..i])
				}
			}
		}
	}
	return false
}

fn (g &Parser) render_array_access_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .name || tokens.last().tok != .rsbr {
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
	if open <= 0 {
		return none
	}
	mut prefix_depth := 0
	for item in tokens[..open] {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			prefix_depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			prefix_depth--
		} else if prefix_depth == 0 && item.tok.is_assignment() {
			// `target = [value]` ends in `]`, but its bracket pair is an array literal,
			// not an index applied to the assignment prefix.
			return none
		}
	}
	close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	base_tokens := tokens[..open]
	base_type := g.infer_expression_type(base_tokens) or { return none }
	base_layout_type := g.underlying_alias_type(base_type)
	base_source := if base_tokens.len == 1 {
		g.resolved_root_expression_name(tokens[0].lit)
	} else if nested_base := g.render_array_access_expression(base_tokens) {
		nested_base.source
	} else if raw_base := g.render_raw_expression_tokens(base_tokens) {
		if method_base := g.render_method_call_expression(base_tokens, raw_base) {
			method_base.source
		} else if member_base := g.render_member_receiver(base_tokens) {
			member_base
		} else {
			raw_base
		}
	} else if member_base := g.render_member_receiver(base_tokens) {
		member_base
	} else {
		return none
	}
	mut range_index := -1
	for i in open + 1 .. close {
		if tokens[i].tok == .dotdot {
			range_index = i
			break
		}
	}
	if range_index >= 0 {
		start := if range_index == open + 1 {
			'0'
		} else {
			g.render_membership_candidate(tokens[open + 1..range_index], 'int') or { return none }
		}
		omitted_end := range_index + 1 == close
		is_fixed_array := base_layout_type.starts_with('FixedArray_')
		is_raw_fixed_array := is_fixed_array && g.fixed_array_uses_raw_storage(base_tokens)
		needs_receiver_temporary := omitted_end && base_tokens.len > 1 && !is_raw_fixed_array
		receiver_name := '__v_fastc_slice_receiver'
		receiver_source := if needs_receiver_temporary { receiver_name } else { base_source }
		receiver_is_pointer := base_type.ends_with('*') && !needs_receiver_temporary
		access := if receiver_is_pointer { '->' } else { '.' }
		end := if omitted_end {
			if is_fixed_array {
				fastc_fixed_array_length(base_layout_type.trim_right('*')) or { return none }
			} else {
				'${receiver_source}${access}len'
			}
		} else {
			g.render_membership_candidate(tokens[range_index + 1..close], 'int') or { return none }
		}
		mut slice_type := if base_layout_type == 'string' {
			'string'
		} else {
			base_type.trim_right('*')
		}
		mut slice_source := if base_layout_type == 'string' {
			'builtin__string_substr(${if receiver_is_pointer { '*' } else { '' }}(${receiver_source}), ${start}, ${end})'
		} else if is_fixed_array {
			// Slicing a fixed array yields a NEW dynamic array copied from the element
			// range; the raw `u8 x[N]` / wrapped `.data` storage differs by receiver kind.
			element := g.array_element_type(base_layout_type.trim_right('*')) or { return none }
			norm_element := fastc_normalize_inferred_type(element)
			data_expr := if is_raw_fixed_array {
				'(${receiver_source})'
			} else {
				'(${receiver_source})${access}data'
			}
			slice_len := '((${end}) - (${start}))'
			slice_type = fastc_array_c_type(norm_element)
			mut w := unsafe { &Parser(g) }
			fastc_register_composite_type(slice_type, mut w.composite_types)
			'((${slice_type})builtin__new_array_from_c_array(${slice_len}, ${slice_len}, sizeof(${norm_element}), &((${data_expr})[${start}])))'
		} else {
			array_value := if receiver_is_pointer {
				'*(${receiver_source})'
			} else {
				receiver_source
			}
			'builtin__array_slice(${array_value}, ${start}, ${end})'
		}
		if needs_receiver_temporary {
			slice_source = '({ __typeof__((${base_source})) ${receiver_name} = (${base_source}); ${slice_source}; })'
		}
		return FastcRenderedExpression{
			source: slice_source
			typ: slice_type
		}
	}
	is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
	element_type := if is_array_pointer {
		g.array_element_type(base_type) or { return none }
	} else if base_type.ends_with('*') {
		base_type[..base_type.len - 1]
	} else if base_layout_type == 'string' {
		'u8'
	} else {
		g.array_element_type(base_type) or { return none }
	}
	index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or { return none }
	if base_layout_type == 'string' {
		return FastcRenderedExpression{
			source: 'builtin__string_at(${base_source}, ${index_source})'
			typ: element_type
		}
	}
	is_raw_fixed_array := base_type.starts_with('FixedArray_') && g.fixed_array_uses_raw_storage(base_tokens)
	if fixed_length := fastc_fixed_array_length(base_type.trim_right('*')) {
		checked_index := 'builtin__v_fixed_index(${index_source}, ${fixed_length})'
		if is_raw_fixed_array {
			return FastcRenderedExpression{
				source: '((${base_source})[${checked_index}])'
				typ: element_type
			}
		}
		access := if base_type.ends_with('*') { '->' } else { '.' }
		return FastcRenderedExpression{
			source: '((${base_source})${access}data[${checked_index}])'
			typ: element_type
		}
	}
	if is_raw_fixed_array {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ: element_type
		}
	}
	if base_type.ends_with('*') && !is_array_pointer {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ: element_type
		}
	}
	array_value := if base_type.ends_with('*') { '*(${base_source})' } else { base_source }
	return FastcRenderedExpression{
		source: '(*(${element_type} *)builtin__array_get(${array_value}, ${index_source}))'
		typ: element_type
	}
}

fn (g &Parser) render_nested_array_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	mut rendered := rendered_expression
	mut changed := false
	for i := tokens.len - 2; i >= 0; i-- {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lsbr {
			continue
		}
		// A name after `.` is a field, not a new root expression. Treating it as
		// a local can replace the field suffix inside its owning expression. Render
		// the complete member-rooted access instead, preserving embedded promotion.
		if i > 0 && tokens[i - 1].tok == .dot {
			close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { continue }
			start := fastc_method_receiver_start(tokens, i + 1)
			if start >= i {
				continue
			}
			access_tokens := tokens[start..close + 1]
			raw_access := g.render_raw_expression_tokens(access_tokens) or { continue }
			replacement := g.render_array_access_expression(access_tokens) or { continue }
			if rendered.contains(raw_access) {
				rendered = rendered.replace(raw_access, replacement.source)
				changed = true
			}
			continue
		}
		close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { continue }
		if close <= i + 1 {
			continue
		}
		if fastc_expression_tokens_contain(tokens[i + 2..close], .dotdot) {
			access_tokens := tokens[i..close + 1]
			raw_access := g.render_raw_expression_tokens(access_tokens) or { continue }
			replacement := g.render_array_access_expression(access_tokens) or { continue }
			if rendered.contains(raw_access) {
				rendered = rendered.replace(raw_access, replacement.source)
				changed = true
			}
			continue
		}
		index_source := g.render_membership_candidate(tokens[i + 2..close], 'int') or { continue }
		base_source := g.resolved_root_expression_name(tokens[i].lit)
		needle := '${base_source}[${index_source}]'
		replacement := g.render_array_access_expression(tokens[i..close + 1]) or { continue }
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, replacement.source)
			changed = true
		}
	}
	for open, item in tokens {
		if item.tok != .lsbr || open == 0 || tokens[open - 1].tok != .rpar {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close <= open + 1 || fastc_expression_tokens_contain(tokens[open + 1..close], .dotdot) {
			continue
		}
		start := fastc_method_receiver_start(tokens, open)
		if start >= open || tokens[start].tok != .name {
			continue
		}
		access_tokens := tokens[start..close + 1]
		replacement := g.render_array_access_expression(access_tokens) or { continue }
		raw_access := g.render_raw_expression_tokens(access_tokens) or { '' }
		if raw_access != '' && rendered.contains(raw_access) {
			rendered = rendered.replace(raw_access, replacement.source)
			changed = true
			continue
		}
		base_tokens := tokens[start..open]
		raw_base := g.render_raw_expression_tokens(base_tokens) or { continue }
		method_base := g.render_method_call_expression(base_tokens, raw_base) or { continue }
		index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or {
			continue
		}
		needle := '${method_base.source}[${index_source}]'
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, replacement.source)
			changed = true
		}
	}
	if !changed {
		return none
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ: inferred_type
	}
}

fn (g &Parser) resolved_root_expression_name(name string) string {
	if global_name := g.globals[fastc_global_key(g.module_name, name)] {
		return global_name
	}
	if constant_name := g.constants[fastc_constant_key(g.module_name, name)] {
		return constant_name
	}
	if constant_name := g.constants[name] {
		return constant_name
	}
	return name
}

fn (g &Parser) render_membership_candidate(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name && g.declared_kinds[g.semantic_type_key(expected_type)] == .enum_ {
		return '${expected_type.trim_right('*')}__${tokens[1].lit}'
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access.source
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if special := g.render_special_expression(tokens, raw) {
		return special.source
	}
	if map_expression := g.render_map_expression(tokens) {
		return map_expression.source
	}
	if method_expression := g.render_method_call_expression(tokens, raw) {
		return method_expression.source
	}
	if call_expression := g.render_missing_call_arguments(tokens) {
		return call_expression.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		return pointer_members.source
	}
	if member_source := g.render_member_receiver(tokens) {
		return member_source
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lpar && tokens.last().tok == .rpar {
		if cast_type := fastc_primitive_c_type(tokens[0].lit) {
			close := fastc_matching_rpar(tokens, 1) or { return none }
			if close == tokens.len - 1 {
				inner := g.render_membership_candidate(tokens[2..close], '') or { return none }
				return '((${fastc_output_c_type(cast_type)})(${inner}))'
			}
		}
	}
	if promoted := g.render_leading_member_chain_promotion(tokens, expected_type) {
		return promoted
	}
	return raw
}

// render_leading_member_chain_promotion handles a compound expression that begins
// with a member chain reaching an EMBEDDED field (`ss.pos + 2`), which the pure
// member-chain renderer rejects because of the trailing operator. It promotes the
// leading chain and renders the rest recursively. Returns none unless the leading
// chain is actually an embedded access, so every other expression keeps its
// byte-for-byte raw form.
fn (g &Parser) render_leading_member_chain_promotion(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len < 4 || tokens[0].tok != .name {
		return none
	}
	mut chain_end := 1
	for chain_end + 1 < tokens.len && tokens[chain_end].tok == .dot && tokens[chain_end + 1].tok == .name {
		chain_end += 2
	}
	// Need `root.field` (chain_end >= 3) followed by an arithmetic operator; a pure
	// member chain (chain_end == tokens.len) has no trailing token and is handled
	// upstream, so bail before indexing past the end.
	if chain_end < 3 || chain_end >= tokens.len {
		return none
	}
	operator := tokens[chain_end]
	if operator.tok !in [.plus, .minus, .mul, .div, .mod] {
		return none
	}
	chain_source := g.render_member_receiver(tokens[..chain_end]) or { return none }
	if !chain_source.contains('__embedded_') {
		return none
	}
	rest_source := g.render_membership_candidate(tokens[chain_end + 1..], expected_type) or {
		return none
	}
	return '${chain_source} ${operator.tok.str()} ${rest_source}'
}

fn (g &Parser) render_raw_expression_tokens(tokens []FastcExpressionToken) ?string {
	if tokens.len == 1 {
		item := tokens[0]
		if item.source != '' {
			return item.source
		}
		return match item.tok {
			.name { g.resolved_expression_name(item.lit, .unknown) }
			.number {
				if g.selfhost {
					fastc_c_selfhost_number(item.lit)
				} else {
					fastc_c_number(item.lit) or { return none }
				}
			}
			.string {
				literal := fastc_c_string(item.lit) or { return none }
				if g.selfhost { '_S(${literal})' } else { literal }
			}
			.char {
				if item.lit.starts_with('c:') {
					fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
				} else {
					fastc_c_rune(item.lit) or { return none }
				}
			}
			.key_true { '((bool)true)' }
			.key_false { '((bool)false)' }
			.key_nil { 'NULL' }
			.key_none { '(Option){.state=2}' }
			else {
				if item.lit == '' { item.tok.str() } else { item.lit }
			}
		}
	}
	mut result := strings.new_builder(32)
	mut cast_closes := []int{}
	mut cast_open := -1
	mut previous_module_separator := false
	for i, item in tokens {
		mut piece := item.lit
		module_separator := g.expression_dot_is_module_separator(tokens, i)
		is_direct_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) && i + 2 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 2].tok == .lpar && (fastc_primitive_c_type(tokens[i + 1].lit) != none || g.resolve_declared_type_key(tokens[i + 1].lit) != none)
		is_c_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) && i + 4 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 1].lit == 'C' && tokens[i + 2].tok == .dot && tokens[i + 3].tok == .name && tokens[i + 4].tok == .lpar
		if item.source != '' {
			// Synthetic expression atoms (an `or` unwrap, interpolation, anonymous
			// function, etc.) carry their complete C spelling in `source`. Preserve it
			// when the atom is nested inside a larger binary or call expression.
			piece = item.source
		} else if is_direct_pointer_cast || is_c_pointer_cast {
			piece = ''
		} else if item.tok == .name && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			is_member_call := i > 0 && tokens[i - 1].tok == .dot
			mut cast_type := if is_member_call {
				''
			} else {
				fastc_primitive_c_type(item.lit) or { '' }
			}
			is_c_cast := i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' && tokens[i - 1].tok == .dot && item.lit.len > 0 && 'C.${item.lit}' !in g.functions && fastc_call_has_one_argument(tokens, i + 1) && (item.lit[0].is_capital() || '#Cstruct#${item.lit}' in g.declared_types)
			if is_c_cast {
				cast_type = if '#Cstruct#${item.lit}' in g.declared_types {
					'struct ${item.lit}'
				} else {
					item.lit
				}
			}
			if cast_type == '' && !is_member_call {
				if type_key := g.resolve_declared_type_key(item.lit) {
					cast_type = fastc_c_declared_type_name(type_key)
				}
			}
			if cast_type != '' {
				pointer_token := if i > 0 && tokens[i - 1].tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i - 1) {
					tokens[i - 1].tok
				} else if is_c_cast && i >= 3 && tokens[i - 3].tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i - 3) {
					tokens[i - 3].tok
				} else {
					token.Token.unknown
				}
				pointer_suffix := '*'.repeat(if pointer_token == .and {
					2
				} else if pointer_token == .amp {
					1
				} else {
					0
				})
				piece = '((${cast_type}${pointer_suffix})('
				close := fastc_matching_rpar(tokens, i + 1) or { return none }
				cast_open = i + 1
				cast_closes << close
			} else {
				previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
				piece = if previous == .dot {
					item.lit
				} else {
					g.resolved_expression_name(item.lit, previous)
				}
			}
		} else if item.tok == .lpar && i == cast_open {
			piece = ''
		} else if item.tok == .rpar && i in cast_closes {
			piece = '))'
		} else if item.tok == .number {
			piece = if g.selfhost {
				fastc_c_selfhost_number(item.lit)
			} else {
				fastc_c_number(item.lit) or { return none }
			}
		} else if item.tok == .string {
			if item.source != '' {
				piece = item.source
			} else {
				literal := fastc_c_string(item.lit) or { return none }
				piece = if g.selfhost { '_S(${literal})' } else { literal }
			}
		} else if item.tok == .char {
			piece = if item.lit.starts_with('c:') {
				fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
			} else {
				fastc_c_rune(item.lit) or { return none }
			}
		} else if item.tok == .key_true {
			piece = '((bool)true)'
		} else if item.tok == .key_false {
			piece = '((bool)false)'
		} else if item.tok == .key_nil {
			piece = 'NULL'
		} else if item.tok == .key_none {
			piece = '(Option){.state=2}'
		} else if item.tok == .name {
			previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
			piece = if previous == .dot && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
				item.lit
			} else if previous == .dot && g.expression_dot_is_module_separator(tokens, i - 1) {
				// The module prefix already makes a qualified keyword-named constant safe
				// (`orm.float` -> `orm__float`), so do not sanitize the member by itself.
				item.lit
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name && g.is_enum_type_name(tokens[i - 2].lit) {
				// An enum type prefix likewise makes keyword-named fields safe
				// (`TokenKind.float` -> `TokenKind__float`).
				item.lit
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' {
				item.lit
			} else if previous == .dot {
				fastc_c_identifier(item.lit)
			} else {
				g.resolved_expression_name(item.lit, previous)
			}
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit in g.imports {
			piece = '__'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit == 'C' {
			piece = ''
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && g.local_is_pointer(tokens[i - 1].lit) {
			piece = '->'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit !in g.locals && g.is_enum_type_name(tokens[i - 1].lit) {
			piece = '__'
		} else if item.tok == .dot && module_separator {
			piece = '__'
		} else if piece == '' {
			piece = item.tok.str()
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator && !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
		previous_module_separator = module_separator
	}
	return g.render_enum_alias_member_references(tokens, result.str())
}

fn (g &Parser) expression_dot_is_module_separator(tokens []FastcExpressionToken, index int) bool {
	if index <= 0 || index >= tokens.len || tokens[index].tok != .dot || tokens[index - 1].tok != .name {
		return false
	}
	previous_name := tokens[index - 1].lit
	// An imported module name is only a qualifier at the start of a member chain.
	// In `app.config.value`, `config` is a field even when the file imports `config`.
	if (index < 2 || tokens[index - 2].tok != .dot) && (previous_name in g.imports || previous_name == 'C' || (previous_name !in g.locals && g.is_enum_type_name(previous_name))) {
		return true
	}
	if index < 3 || tokens[index - 2].tok != .dot || tokens[index - 3].tok != .name {
		return false
	}
	imported_module := g.imports[tokens[index - 3].lit] or { return false }
	type_key := fastc_type_key(imported_module, previous_name)
	return g.underlying_enum_type_key(type_key) != none
}

fn fastc_token_is_prefix_operator(tokens []FastcExpressionToken, index int) bool {
	if index == 0 {
		return true
	}
	return tokens[index - 1].tok !in [.name, .number, .string, .char, .key_true, .key_false, .key_nil,
		.key_none, .rpar, .rsbr, .rcbr, .inc, .dec]
}

fn (g &Parser) array_initializer_type(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 {
		return none
	}
	mut index := 0
	mut dimensions := 0
	mut fixed_length := ''
	if tokens.len >= 4 && tokens[0].tok == .lsbr && tokens[1].tok in [.name, .number] && tokens[2].tok == .rsbr {
		fixed_length = if tokens[1].tok == .name {
			constant_key := fastc_constant_key(g.module_name, tokens[1].lit)
			g.constants[constant_key] or { fastc_c_constant_name(g.module_name, tokens[1].lit) }
		} else {
			fastc_c_selfhost_number(tokens[1].lit)
		}
		dimensions = 1
		index = 3
	}
	for index + 1 < tokens.len && tokens[index].tok == .lsbr && tokens[index + 1].tok == .rsbr {
		dimensions++
		index += 2
	}
	if dimensions == 0 || index >= tokens.len {
		return none
	}
	mut element_type := g.type_from_expression_tokens(tokens[index..]) or { '' }
	if element_type == '' && index + 1 == tokens.len && tokens[index].tok == .name && tokens[index].lit == 'thread' {
		// `[]thread` is an array of spawned-thread handles (all handles share one
		// C layout, keyed as the void-thread type).
		element_type = fastc_thread_type_name('')
	}
	if element_type == '' {
		return none
	}
	if fixed_length != '' {
		return fastc_fixed_array_type(fixed_length, element_type)
	}
	mut result := element_type
	for _ in 0 .. dimensions {
		result = fastc_array_c_type(result)
	}
	return result
}

fn fastc_initializer_type_start(tokens []FastcExpressionToken) int {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok.is_assignment() {
			return i + 1
		}
	}
	return 0
}

fn (g &Parser) map_initializer_type(tokens []FastcExpressionToken) ?string {
	map_type := g.type_from_expression_tokens(tokens) or { return none }
	return if map_type.starts_with('Map_') { map_type } else { none }
}

fn (g &Parser) type_from_expression_tokens(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 {
		return none
	}
	mut pointers := 0
	mut start := 0
	for start < tokens.len && tokens[start].tok in [.amp, .and, .mul] {
		pointers += if tokens[start].tok == .and { 2 } else { 1 }
		start++
	}
	if start >= tokens.len {
		return none
	}
	remaining := tokens[start..]
	if remaining.len >= 3 && remaining[0].tok == .lsbr && remaining[1].tok == .rsbr {
		element_type := g.type_from_expression_tokens(remaining[2..]) or { return none }
		return fastc_array_c_type(element_type) + '*'.repeat(pointers)
	}
	if remaining.len >= 5 && remaining[0].tok == .name && remaining[0].lit == 'map' && remaining[1].tok == .lsbr {
		close := fastc_matching_delimiter(remaining, 1, .lsbr, .rsbr) or { return none }
		if close <= 2 || close + 1 >= remaining.len {
			return none
		}
		key_type := g.type_from_expression_tokens(remaining[2..close]) or { return none }
		value_type := g.type_from_expression_tokens(remaining[close + 1..]) or { return none }
		return fastc_map_c_type(key_type, value_type) + '*'.repeat(pointers)
	}
	if remaining.len >= 2 && remaining[0].tok == .name && remaining[0].lit == 'chan' {
		// Channels use one erased runtime representation. Still validate the element
		// spelling so an arbitrary trailing expression is not accepted as a type.
		_ := g.type_from_expression_tokens(remaining[1..]) or { return none }
		return 'chan' + '*'.repeat(pointers)
	}
	if remaining.len == 1 && remaining[0].tok == .name {
		mut base := fastc_primitive_c_type(remaining[0].lit) or { '' }
		if base == '' {
			type_key := g.resolve_declared_type_key(remaining[0].lit) or { return none }
			base = fastc_c_declared_type_name(type_key)
		}
		return base + '*'.repeat(pointers)
	}
	if remaining.len == 3 && remaining[0].tok == .name && remaining[1].tok == .dot && remaining[2].tok == .name {
		if remaining[0].lit == 'C' {
			raw_type := remaining[2].lit
			if '#Cstruct#${raw_type}' in g.declared_types {
				return 'struct ${raw_type}' + '*'.repeat(pointers)
			}
			if 'C.${raw_type}' in g.functions {
				return none
			}
			if raw_type.len == 0 || !raw_type[0].is_capital() {
				return none
			}
			return raw_type + '*'.repeat(pointers)
		}
		module_name := g.imports[remaining[0].lit] or { return none }
		type_key := fastc_type_key(module_name, remaining[2].lit)
		if type_key !in g.declared_types {
			return none
		}
		return fastc_c_declared_type_name(type_key) + '*'.repeat(pointers)
	}
	return none
}

fn fastc_array_initializer_c_type(array_type string) string {
	length := fastc_fixed_array_length(array_type) or { return array_type }
	element_type := fastc_fixed_array_element_type(array_type) or { return array_type }
	return 'FixedArray_${fastc_composite_type_part(length)}_${fastc_composite_type_part(element_type)}'
}

fn fastc_generate_fixed_array_declarations(fixed_array_types map[string]string) string {
	mut names := fixed_array_types.keys()
	names.sort()
	mut out := strings.new_builder(256)
	for name in names {
		array_type := fixed_array_types[name]
		length := fastc_fixed_array_length(array_type) or { continue }
		element_type := fastc_fixed_array_element_type(array_type) or { continue }
		declaration_name := if fastc_composite_type_part(array_type) == array_type {
			array_type
		} else {
			name
		}
		out.writeln('typedef struct { ${element_type} data[${length}]; } ${declaration_name};')
		if name != declaration_name {
			out.writeln('typedef ${declaration_name} ${name};')
		}
	}
	if out.len > 0 {
		out.writeln('')
	}
	return out.str()
}

fn fastc_expression_list_items(tokens []FastcExpressionToken, start int, end int) ![][]FastcExpressionToken {
	if start == end {
		return [][]FastcExpressionToken{}
	}
	mut result := [][]FastcExpressionToken{}
	mut item_start := start
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
			.comma {
				if parens == 0 && brackets == 0 && braces == 0 {
					if item_start == i {
						return error('empty expression-list item')
					}
					// Items are consumed while `tokens` is alive and never mutated, so views are safe.
					item := unsafe { tokens[item_start..i] }
					result << item
					item_start = i + 1
				}
			}
			else {}
		}
	}
	if item_start == end {
		return result
	}
	item := unsafe { tokens[item_start..end] }
	result << item
	return result
}
