module fastc

import strings
import v3.token

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
	} else if member_base := g.render_member_receiver(base_tokens) {
		member_base
	} else {
		g.render_raw_expression_tokens(base_tokens) or { return none }
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
		needs_receiver_temporary := omitted_end && base_tokens.len > 1
		receiver_name := '__v_fastc_slice_receiver'
		receiver_source := if needs_receiver_temporary { receiver_name } else { base_source }
		access := if base_type.ends_with('*') { '->' } else { '.' }
		end := if omitted_end {
			'${receiver_source}${access}len'
		} else {
			g.render_membership_candidate(tokens[range_index + 1..close], 'int') or { return none }
		}
		mut slice_source := if base_layout_type == 'string' {
			'builtin__string_substr(${if base_type.ends_with('*') { '*' } else { '' }}(${receiver_source}), ${start}, ${end})'
		} else {
			array_value := if base_type.ends_with('*') {
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
			typ:    if base_layout_type == 'string' { 'string' } else { base_type.trim_right('*') }
		}
	}
	is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
	element_type := if is_array_pointer {
		g.array_element_type(base_type) or { return none }
	} else if base_type.ends_with('*') {
		base_type.trim_right('*')
	} else if base_layout_type == 'string' {
		'u8'
	} else {
		g.array_element_type(base_type) or { return none }
	}
	index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or { return none }
	if base_layout_type == 'string' {
		return FastcRenderedExpression{
			source: 'builtin__string_at(${base_source}, ${index_source})'
			typ:    element_type
		}
	}
	is_raw_fixed_array := base_type.starts_with('FixedArray_') && (base_tokens.len > 1
		|| (base_tokens.len == 1
		&& fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals))
	if fixed_length := fastc_fixed_array_length(base_type.trim_right('*')) {
		checked_index := 'builtin__v_fixed_index(${index_source}, ${fixed_length})'
		if is_raw_fixed_array {
			return FastcRenderedExpression{
				source: '((${base_source})[${checked_index}])'
				typ:    element_type
			}
		}
		access := if base_type.ends_with('*') { '->' } else { '.' }
		return FastcRenderedExpression{
			source: '((${base_source})${access}data[${checked_index}])'
			typ:    element_type
		}
	}
	if is_raw_fixed_array {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ:    element_type
		}
	}
	if base_type.ends_with('*') && !is_array_pointer {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ:    element_type
		}
	}
	array_value := if base_type.ends_with('*') { '*(${base_source})' } else { base_source }
	return FastcRenderedExpression{
		source: '(*(${element_type} *)builtin__array_get(${array_value}, ${index_source}))'
		typ:    element_type
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
		// a local can replace the field suffix inside its owning expression (for
		// example `str.str[0]`) and produce invalid C.
		if i > 0 && tokens[i - 1].tok == .dot {
			continue
		}
		close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { continue }
		if close <= i + 1 || fastc_expression_tokens_contain(tokens[i + 2..close], .dotdot) {
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
	if !changed {
		return none
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ:    inferred_type
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
	if tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name
		&& g.declared_kinds[g.semantic_type_key(expected_type)] == .enum_ {
		return '${expected_type.trim_right('*')}__${tokens[1].lit}'
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access.source
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if map_expression := g.render_map_expression(tokens, raw) {
		return map_expression.source
	}
	if method_expression := g.render_method_call_expression(tokens, raw) {
		return method_expression.source
	}
	if call_expression := g.render_missing_call_arguments(tokens, raw) {
		return call_expression.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		return pointer_members.source
	}
	if member_source := g.render_member_receiver(tokens) {
		return member_source
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lpar
		&& tokens.last().tok == .rpar {
		if cast_type := fastc_primitive_c_type(tokens[0].lit) {
			close := fastc_matching_rpar(tokens, 1) or { return none }
			if close == tokens.len - 1 {
				inner := g.render_membership_candidate(tokens[2..close], '') or { return none }
				return '((${cast_type})(${inner}))'
			}
		}
	}
	return raw
}

fn (g &Parser) render_raw_expression_tokens(tokens []FastcExpressionToken) ?string {
	mut result := strings.new_builder(32)
	mut cast_closes := map[int]bool{}
	mut cast_opens := map[int]bool{}
	for i, item in tokens {
		mut piece := item.lit
		module_separator := g.expression_dot_is_module_separator(tokens, i)
		previous_module_separator := g.expression_dot_is_module_separator(tokens, i - 1)
		is_direct_pointer_cast := item.tok in [.amp, .and]
			&& fastc_token_is_prefix_operator(tokens, i) && i + 2 < tokens.len
			&& tokens[i + 1].tok == .name && tokens[i + 2].tok == .lpar
			&& (fastc_primitive_c_type(tokens[i + 1].lit) != none
			|| fastc_resolve_declared_type_key(g.module_name, tokens[i + 1].lit, g.imports, g.declared_types) != none)
		is_c_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i)
			&& i + 4 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 1].lit == 'C'
			&& tokens[i + 2].tok == .dot && tokens[i + 3].tok == .name && tokens[i + 4].tok == .lpar
		if is_direct_pointer_cast || is_c_pointer_cast {
			piece = ''
		} else if item.tok == .name && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			mut cast_type := fastc_primitive_c_type(item.lit) or { '' }
			is_c_cast := i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C'
				&& tokens[i - 1].tok == .dot && item.lit.len > 0 && item.lit[0].is_capital()
				&& 'C.${item.lit}' !in g.functions
			if is_c_cast {
				cast_type = item.lit
			}
			if cast_type == '' {
				if type_key := fastc_resolve_declared_type_key(g.module_name, item.lit, g.imports,
					g.declared_types)
				{
					cast_type = fastc_c_declared_type_name(type_key)
				}
			}
			if cast_type != '' {
				pointer_token := if i > 0 && tokens[i - 1].tok in [.amp, .and]
					&& fastc_token_is_prefix_operator(tokens, i - 1) {
					tokens[i - 1].tok
				} else if is_c_cast && i >= 3 && tokens[i - 3].tok in [.amp, .and]
					&& fastc_token_is_prefix_operator(tokens, i - 3) {
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
				cast_opens[i + 1] = true
				cast_closes[close] = true
			}
		} else if item.tok == .lpar && i in cast_opens {
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
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name
				&& tokens[i - 2].lit == 'C' {
				item.lit
			} else {
				g.resolved_expression_name(item.lit, previous)
			}
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& tokens[i - 1].lit in g.imports {
			piece = '__'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& tokens[i - 1].lit == 'C' {
			piece = ''
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& g.local_is_pointer(tokens[i - 1].lit) {
			piece = '->'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& tokens[i - 1].lit !in g.locals && g.is_enum_type_name(tokens[i - 1].lit) {
			piece = '__'
		} else if item.tok == .dot && module_separator {
			piece = '__'
		} else if piece == '' {
			piece = item.tok.str()
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator
			&& !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
	}
	return g.render_enum_alias_member_references(tokens, result.str())
}

fn (g &Parser) expression_dot_is_module_separator(tokens []FastcExpressionToken, index int) bool {
	if index <= 0 || index >= tokens.len || tokens[index].tok != .dot
		|| tokens[index - 1].tok != .name {
		return false
	}
	previous_name := tokens[index - 1].lit
	if previous_name in g.imports || previous_name == 'C'
		|| (previous_name !in g.locals && g.is_enum_type_name(previous_name)) {
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
	if tokens.len >= 4 && tokens[0].tok == .lsbr && tokens[1].tok in [.name, .number]
		&& tokens[2].tok == .rsbr {
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
	mut pointers := 0
	for index < tokens.len && tokens[index].tok in [.amp, .mul] {
		pointers++
		index++
	}
	if dimensions == 0 || index >= tokens.len || tokens[index].tok != .name {
		return none
	}
	mut element_type := fastc_primitive_c_type(tokens[index].lit) or { '' }
	if element_type == '' {
		if type_key := fastc_resolve_declared_type_key(g.module_name, tokens[index].lit, g.imports,
			g.declared_types)
		{
			element_type = fastc_c_declared_type_name(type_key)
		}
	}
	index++
	if element_type == '' || index != tokens.len {
		return none
	}
	element_type += '*'.repeat(pointers)
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
	for start < tokens.len && tokens[start].tok in [.amp, .mul] {
		pointers++
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
	if remaining.len >= 5 && remaining[0].tok == .name && remaining[0].lit == 'map'
		&& remaining[1].tok == .lsbr {
		close := fastc_matching_delimiter(remaining, 1, .lsbr, .rsbr) or { return none }
		if close <= 2 || close + 1 >= remaining.len {
			return none
		}
		key_type := g.type_from_expression_tokens(remaining[2..close]) or { return none }
		value_type := g.type_from_expression_tokens(remaining[close + 1..]) or { return none }
		return fastc_map_c_type(key_type, value_type) + '*'.repeat(pointers)
	}
	if remaining.len == 1 && remaining[0].tok == .name {
		mut base := fastc_primitive_c_type(remaining[0].lit) or { '' }
		if base == '' {
			type_key := fastc_resolve_declared_type_key(g.module_name, remaining[0].lit, g.imports,
				g.declared_types) or { return none }
			base = fastc_c_declared_type_name(type_key)
		}
		return base + '*'.repeat(pointers)
	}
	if remaining.len == 3 && remaining[0].tok == .name && remaining[1].tok == .dot
		&& remaining[2].tok == .name {
		if remaining[0].lit == 'C' {
			raw_type := remaining[2].lit
			if 'C.${raw_type}' in g.functions {
				return none
			}
			if '#Cstruct#${raw_type}' in g.declared_types {
				return 'struct ${raw_type}' + '*'.repeat(pointers)
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
		out.writeln('typedef struct { ${element_type} data[${length}]; } ${name};')
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
					result << tokens[item_start..i]
					item_start = i + 1
				}
			}
			else {}
		}
	}
	if item_start == end {
		return result
	}
	result << tokens[item_start..end]
	return result
}
