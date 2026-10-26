module fastc

import v.flat
import v.token

// fastc_builtin_type_idx maps a primitive type name to V's canonical builtin
// type index (vlib/v/ast/types.v). Used to evaluate `typeof[T]().idx`.
fn fastc_builtin_type_idx(type_name_source_id u32) ?int {
	type_name := flat.source_type_name(type_name_source_id)
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
		'u8' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'uint', 'usize' { 15 }
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

// fastc_resolve_flag_enum_statics rewrites a flag enum's compiler-magic static methods that FastC
// leaves as calls to non-existent functions: `Enum.zero()` (the empty flag, value 0) and
// `Enum.all()` (every member OR-ed together). Only applied for a flag enum.
fn (g &Parser) fastc_resolve_flag_enum_statics(source string, enum_c string) string {
	if g.flag_enum_type_key(flat.source_type_id(enum_c)) == none {
		return source
	}
	mut out := source.replace('${enum_c}__zero()', '((${enum_c})0)')
	if fastc_contains(out, '${enum_c}__all()') {
		if members := g.enum_field_names[enum_c] {
			mut parts := []string{cap: members.len}
			for member in members {
				parts << '${enum_c}__${member}'
			}
			if parts.len > 0 {
				out = out.replace('${enum_c}__all()', '((${enum_c})(${parts.join(' | ')}))')
			}
		}
	}
	return out
}

// fastc_resolve_enum_shorthands_in_source rewrites `.member` enum shorthands that appear in a
// value position (the `.` is not preceded by an identifier byte, so a real `.field` access is
// left alone) into their `Enum__member` C constant. String literals are skipped so a `.member`
// inside `_S("…")` is never rewritten.
fn fastc_resolve_enum_shorthands_in_source(source string, enum_c string, fields []string) string {
	mut out := ''
	mut i := 0
	for i < source.len {
		c := source[i]
		if c == `"` {
			out += c.ascii_str()
			i++
			for i < source.len {
				out += source[i].ascii_str()
				if source[i] == `\\` && i + 1 < source.len {
					out += source[i + 1].ascii_str()
					i += 2
					continue
				}
				if source[i] == `"` {
					i++
					break
				}
				i++
			}
			continue
		}
		if c == `.` && (i == 0 || !fastc_identifier_byte(source[i - 1])) && i + 1 < source.len && fastc_identifier_byte(source[i + 1]) {
			mut j := i + 1
			for j < source.len && fastc_identifier_byte(source[j]) {
				j++
			}
			member := source[i + 1..j]
			if member in fields {
				out += '${enum_c}__${member}'
				i = j
				continue
			}
		}
		out += c.ascii_str()
		i++
	}
	return out
}

// render_contextual_empty_map_initializer uses the destination's map layout
// for a complete empty typed literal. No existing entries need conversion.
fn (g &Parser) render_contextual_empty_map_initializer(tokens []FastcExpressionToken, expected_type u32) ?FastcRenderedExpression {
	if !g.selfhost || expected_type == 0 || tokens.len < 3 || tokens[0].tok !in [
		.name,
		.lpar,
	] {
		return none
	}
	literal_tokens := fastc_strip_paren_tokens(tokens)
	if literal_tokens.len < 3 || literal_tokens[literal_tokens.len - 2].tok != .lcbr
		|| literal_tokens.last().tok != .rcbr {
		return none
	}
	expected_layout := g.underlying_alias_type(expected_type)
	if fastc_is_pointer_type(expected_layout) {
		return none
	}
	key_type, value_type := g.map_key_value_types(expected_layout) or { return none }
	actual_type := g.map_initializer_type(literal_tokens[..literal_tokens.len - 2]) or { return none }
	if actual_type == expected_layout || fastc_is_pointer_type(actual_type) {
		return none
	}
	actual_key, actual_value := g.map_key_value_types(actual_type) or { return none }
	if actual_key == key_type && actual_value == value_type {
		return none
	}
	hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
	return FastcRenderedExpression{
		source: '(builtin__new_map(sizeof(${g.collection_runtime_type(key_type)}), sizeof(${g.collection_runtime_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
		typ:    expected_type
	}
}

fn (g &Parser) render_map_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if lookup := g.render_map_lookup_option_expression(tokens) {
		// The Option temp uses the reserved `__vf_` prefix, NOT a plain name like `lookup`: a
		// source variable of that name used in the key (`m[lookup]`) would otherwise be shadowed by
		// this declaration and read uninitialized inside its own initializer.
		missing_value := g.map_lookup_missing_value_expression(lookup.typ)
		return FastcRenderedExpression{
			source: '({ Option __vf_ml = (${lookup.source}); __vf_ml.state ? ${missing_value} : ${g.option_value_expression(lookup.typ, '__vf_ml')}; })'
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
		if map_type_source_id := g.map_initializer_type(tokens[..literal_open]) {
			map_type := map_type_source_id

			key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
			key_type := key_type_source_id
			value_type := value_type_source_id

			hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
			initializer := '(builtin__new_map(sizeof(${g.collection_runtime_type(key_type)}), sizeof(${g.collection_runtime_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
			return FastcRenderedExpression{
				source: if flat.source_type_name(map_type).ends_with('*') {
					'((${flat.source_type_name(map_type)})${fastc_box_expression(flat.source_type_name(map_type).trim_right('*'), initializer)})'
				} else {
					initializer
				}
				typ:    map_type
			}
		}
	}
	if update := g.render_map_index_compound_assignment(tokens) {
		return update
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
		key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
		key_type := key_type_source_id
		value_type := value_type_source_id

		key_source := g.render_call_argument_expression(tokens[open + 1..close], key_type) or {
			return none
		}
		value_source := g.render_call_argument_expression(tokens[assignment_index + 1..], value_type) or { return none }
		mut map_address := ''
		if nested := g.render_mutable_map_value_pointer(base_tokens) {
			if nested.typ_text() != flat.source_type_name(map_type).trim_right('*') {
				return none
			}
			map_address = nested.source
		} else {
			map_source := g.render_member_receiver(base_tokens) or {
				g.render_raw_expression_tokens(base_tokens) or { return none }
			}
			map_address = if flat.source_type_name(map_type).ends_with('*') {
				map_source
			} else {
				'&${map_source}'
			}
		}
		return FastcRenderedExpression{
			source: '({ ${g.collection_storage_type(key_type)} __vf_k = (${key_source}); ${g.collection_storage_type(value_type)} __vf_mv = (${value_source}); builtin__map_set((map *)${map_address}, &__vf_k, &__vf_mv); __vf_mv; })'
			typ:    value_type
		}
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lsbr && tokens.last().tok == .rsbr {
		close := fastc_matching_delimiter(tokens, 1, .lsbr, .rsbr) or { return none }
		if close != tokens.len - 1 {
			return none
		}
		map_type := g.infer_expression_type(tokens[..1]) or { return none }
		key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
		key_type := key_type_source_id
		value_type := value_type_source_id

		key_source := g.render_call_argument_expression(tokens[2..close], key_type) or {
			return none
		}
		global_key := fastc_global_key(g.module_name, tokens[0].lit)
		map_source := g.globals[global_key] or { tokens[0].lit }
		map_address := if flat.source_type_name(map_type).ends_with('*') {
			map_source
		} else {
			'&${map_source}'
		}
		missing_value := g.map_lookup_missing_value_expression(value_type)
		return FastcRenderedExpression{
			source: '({ ${g.collection_storage_type(key_type)} __vf_k = (${key_source}); ${g.collection_storage_type(value_type)} *__vf_map_value = (${g.collection_storage_type(value_type)} *)builtin__map_get_check((map *)${map_address}, &__vf_k); __vf_map_value == NULL ? ${missing_value} : *__vf_map_value; })'
			typ:    value_type
		}
	}
	return none
}

// FastcMapValueField is a struct field of a map value (`m[k].a.b`), reached through a
// mutable pointer to the map entry.
struct FastcMapValueField {
	entry      FastcRenderedExpression // pointer to the entry; inserts a zero default when absent
	access     string                  // `->a.b`, applied to the entry pointer
	field_type u32
}

// map_value_field_target splits `m[k].a.b` into a mutable pointer to the map entry and the
// field access through it.
fn (g &Parser) map_value_field_target(lhs []FastcExpressionToken) ?FastcMapValueField {
	if lhs.len < 5 || lhs.last().tok != .name {
		return none
	}
	// The last top-level `]` must be followed by a pure `.name` chain (the fields).
	mut close := -1
	mut bracket_depth := 0
	for i, item in lhs {
		match item.tok {
			.lsbr { bracket_depth++ }
			.rsbr {
				bracket_depth--
				if bracket_depth == 0 {
					close = i
				}
			}
			else {}
		}
	}
	if close <= 0 || close + 1 >= lhs.len || lhs[close + 1].tok != .dot {
		return none
	}
	map_index_tokens := lhs[..close + 1]
	field_tokens := lhs[close + 1..]
	if field_tokens.len < 2 || field_tokens.len % 2 != 0 {
		return none
	}
	for i := 0; i < field_tokens.len; i += 2 {
		if field_tokens[i].tok != .dot || field_tokens[i + 1].tok != .name {
			return none
		}
	}
	ptr := g.render_mutable_map_value_pointer(map_index_tokens) or { return none }
	mut current_type := ptr.typ_text()
	mut field_access := ''
	for i := 0; i < field_tokens.len; i += 2 {
		field := g.struct_field_metadata(flat.source_type_id(current_type), field_tokens[i + 1].lit) or { return none }
		separator := if i == 0 { '->' } else { '.' }
		field_access += separator + fastc_c_identifier(field.name)
		current_type = field.typ_text()
	}
	return FastcMapValueField{
		entry:      ptr
		access:     field_access
		field_type: flat.source_type_id(current_type)
	}
}

// render_map_value_field_wrapping lowers the target of `m[k].field = <value>` or
// `m[k].field op= <value>` (a struct field of a map value) into a wrapper split around the
// value, like render_map_index_assignment_wrapping. A map value is not a C lvalue, so a
// mutable pointer to the entry is taken (inserting a zero default when absent) and the field
// assigned through it.
fn (g &Parser) render_map_value_field_wrapping(left_tokens []FastcExpressionToken, operator token.Token) ?FastcMapAssignmentWrap {
	if !operator.is_assignment() || operator == .decl_assign {
		return none
	}
	target := g.map_value_field_target(left_tokens) or { return none }
	entry := '({ ${target.entry.typ_text()} *__vf_map_field_ptr = (${target.entry.source}); '
	field := '__vf_map_field_ptr${target.access}'
	if operator == .assign {
		return FastcMapAssignmentWrap{
			prefix:     '${entry}${field} = ('
			suffix:     '); (void)0; })'
			value_type: target.field_type
		}
	}
	if fastc_is_pointer_type(g.underlying_alias_type(target.field_type)) {
		// Pointer arithmetic takes an integer operand, not a value of the field's type.
		return none
	}
	update := g.render_assignment_operator_source(field, '__vf_compound_rhs', target.field_type,
		operator) or { return none }
	return FastcMapAssignmentWrap{
		prefix:     '${entry}${g.collection_storage_type(target.field_type)} __vf_compound_rhs = ('
		suffix:     '); ${update}; (void)0; })'
		value_type: target.field_type
	}
}

// render_map_value_field_assignment lowers a whole `m[k].field = value` or
// `m[k].field op= value` expression through render_map_value_field_wrapping.
fn (g &Parser) render_map_value_field_assignment(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut assign_idx := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			else {
				if depth == 0 && item.tok.is_assignment() {
					assign_idx = i
					break
				}
			}
		}
	}
	if assign_idx <= 0 || assign_idx + 1 >= tokens.len {
		return none
	}
	wrap := g.render_map_value_field_wrapping(tokens[..assign_idx], tokens[assign_idx].tok) or {
		return none
	}
	value_source := g.render_call_argument_expression(tokens[assign_idx + 1..], wrap.value_type) or {
		return none
	}
	return FastcRenderedExpression{
		source: '${wrap.prefix}${value_source}${wrap.suffix}'
		typ:    flat.source_type_void
	}
}

// render_map_value_field_inc_dec lowers `m[k].field++` / `m[k].field--` (a struct field of a map
// value). Like the assignment form, the map value is not a C lvalue, so a mutable pointer to the
// entry (inserting a zero default when absent) is taken and the field incremented through it.
fn (g &Parser) render_map_value_field_inc_dec(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 6 || tokens.last().tok !in [.inc, .dec] {
		return none
	}
	op := if tokens.last().tok == .inc { '++' } else { '--' }
	target := g.map_value_field_target(tokens[..tokens.len - 1]) or { return none }
	return FastcRenderedExpression{
		source: '({ ${target.entry.typ_text()} *__vf_map_field_ptr = (${target.entry.source}); __vf_map_field_ptr${target.access}${op}; (void)0; })'
		typ:    flat.source_type_void
	}
}

struct FastcMapAssignmentWrap {
	prefix     string
	suffix     string
	value_type u32
}

// render_embedded_map_reads lowers each `name[key]` map-index READ that appears as a
// sub-expression of a larger expression (`m[k] + 1`, `if … { m[k] } …`) into the
// `builtin__map_get` form, which a bare-read only receives when it is the whole
// expression. Only bare-name map bases are rewritten; the caller must exclude assignment
// targets so `m[k] = …` still routes through the map-set lowering.
fn (g &Parser) render_embedded_map_reads(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut rewritten := []FastcExpressionToken{}
	mut i := 0
	mut found := false
	// A map read nested in a call's argument list or a struct-literal field (`f(m[k])`,
	// `sb.write(m[k])`, `T{ x: m[k] }`) is lowered by the call/literal renderer, so leave it
	// alone. But a cast's parens (`int(m[k])`) and plain grouping (`(m[k]) + 1`) do not lower
	// their contents, so those map reads must still be rewritten here. Track, per open
	// bracket, whether it introduces a call/braces scope to skip.
	mut skip_stack := []bool{}
	for i < tokens.len {
		if tokens[i].tok == .lpar {
			// A call's `(` follows a function/method name; a cast's follows a type name and
			// grouping follows an operator/`(`.
			is_call := i > 0 && tokens[i - 1].tok == .name && fastc_primitive_c_type(tokens[i - 1].lit) == none && g.resolve_declared_type_key(tokens[i - 1].lit) == none
			skip_stack << is_call
			rewritten << tokens[i]
			i++
			continue
		}
		if tokens[i].tok == .lcbr {
			skip_stack << true
			rewritten << tokens[i]
			i++
			continue
		}
		if tokens[i].tok in [.rpar, .rcbr] {
			if skip_stack.len > 0 {
				skip_stack.pop()
			}
			rewritten << tokens[i]
			i++
			continue
		}
		inside_skip := skip_stack.any(it)
		if !inside_skip && tokens[i].tok == .name && i + 1 < tokens.len && tokens[i + 1].tok == .lsbr && (i == 0 || tokens[i - 1].tok != .dot) {
			close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { -1 }
			if close > i + 1 {
				map_type := (g.infer_expression_type(tokens[i..i + 1]) or { u32(0) })
				is_assign_target := close + 1 < tokens.len && tokens[close + 1].tok.is_assignment()
				// A map value that is itself indexed or has a field taken (`m[k][i]`,
				// `m[k].f`) is the base of a larger access; leave the whole chain to the
				// array/member renderers rather than rewriting just the map read.
				followed_by_access := close + 1 < tokens.len && tokens[close + 1].tok in [
					.lsbr,
					.dot,
				]
				if map_type != u32(0) && !is_assign_target && !followed_by_access {
					if g.map_key_value_types(map_type) != none {
						if read := g.render_map_expression(tokens[i..close + 1]) {
							rewritten << FastcExpressionToken{
								tok:    .name
								lit:    read.source
								source: fastc_token_source(read.source)
								typ:    read.typ
							}
							i = close + 1
							found = true
							continue
						}
					} else if array_read := g.render_array_access_expression(tokens[i..close + 1]) {
						// An array read (`a[i]`) sharing an expression with a rewritten map read
						// (`a[i] < m[k]`) must also be lowered, or the raw renderer would index the
						// array header struct directly. This does not itself set `found`, so a
						// map-free array expression still returns none and keeps its usual path.
						rewritten << FastcExpressionToken{
							tok:    .name
							lit:    array_read.source
							source: fastc_token_source(array_read.source)
							typ:    array_read.typ
						}
						i = close + 1
						continue
					}
				}
			}
		}
		rewritten << tokens[i]
		i++
	}
	if !found {
		return none
	}
	source := g.render_raw_expression_tokens(rewritten) or { return none }
	return FastcRenderedExpression{
		source: source
		typ:    (g.infer_expression_type(tokens) or { u32(0) })
	}
}

// render_map_index_assignment_wrapping lowers the target of `m[k] = <value>` into a
// `builtin__map_set` wrapper split around the value, so an assignment whose RHS cannot be a
// plain C lvalue operand (notably `m[k] = rhs or { … }`, where the RHS is a statement
// expression) still stores through the map runtime. The caller splices the rendered value
// between `.prefix` and `.suffix`. A compound operator (`m[k] += <value>`) is lowered by
// render_map_index_compound_wrapping, and a field of a map value (`m[k].f = <value>`) by
// render_map_value_field_wrapping.
fn (g &Parser) render_map_index_assignment_wrapping(left_tokens []FastcExpressionToken, operator token.Token) ?FastcMapAssignmentWrap {
	if left_tokens.len > 0 && left_tokens.last().tok == .name {
		return g.render_map_value_field_wrapping(left_tokens, operator)
	}
	if operator != .assign {
		return g.render_map_index_compound_wrapping(left_tokens, operator)
	}
	if left_tokens.len < 4 || left_tokens.last().tok != .rsbr {
		return none
	}
	close := left_tokens.len - 1
	mut open := -1
	mut bracket_depth := 0
	for i := close; i >= 0; i-- {
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
	if open <= 0 {
		return none
	}
	base_tokens := left_tokens[..open]
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
	key_type := key_type_source_id
	value_type := value_type_source_id

	key_source := g.render_call_argument_expression(left_tokens[open + 1..close], key_type) or {
		return none
	}
	mut map_address := ''
	if nested := g.render_mutable_map_value_pointer(base_tokens) {
		if nested.typ_text() != flat.source_type_name(map_type).trim_right('*') {
			return none
		}
		map_address = nested.source
	} else {
		map_source := g.render_member_receiver(base_tokens) or {
			g.render_raw_expression_tokens(base_tokens) or { return none }
		}
		map_address = if flat.source_type_name(map_type).ends_with('*') {
			map_source
		} else {
			'&${map_source}'
		}
	}
	return FastcMapAssignmentWrap{
		prefix:     '({ ${flat.source_type_name(key_type)} __vf_k = (${key_source}); ${flat.source_type_name(value_type)} __vf_mv = ('
		suffix:     '); builtin__map_set((map *)${map_address}, &__vf_k, &__vf_mv); __vf_mv; })'
		value_type: value_type
	}
}

// render_map_index_compound_wrapping lowers the target of `m[k] op= <value>` on a map entry
// into a wrapper split around the value, like render_map_index_assignment_wrapping. A map
// read is not a C lvalue, so, as in cgen, the key is evaluated once, the entry is read (the
// value type's zero when the key is absent), the value is evaluated, the operator is applied
// to the copy, and the result is stored back with `builtin__map_set`. When the map is itself
// an entry of another map (`m[a][b] += v`), the value is evaluated before the entry that
// holds the inner map is looked up, because the value may insert into the outer map and move
// that entry.
fn (g &Parser) render_map_index_compound_wrapping(left_tokens []FastcExpressionToken, operator token.Token) ?FastcMapAssignmentWrap {
	if !operator.is_assignment() || operator in [.assign, .decl_assign] || left_tokens.len < 4
		|| left_tokens.last().tok != .rsbr {
		return none
	}
	close := left_tokens.len - 1
	mut open := -1
	mut bracket_depth := 0
	for i := close; i >= 0; i-- {
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
	if open <= 0 || open + 1 == close {
		return none
	}
	base_tokens := left_tokens[..open]
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type, value_type := g.map_key_value_types(map_type) or { return none }
	if fastc_is_pointer_type(g.underlying_alias_type(value_type)) {
		// Pointer arithmetic takes an integer operand, not a value of the entry's type.
		return none
	}
	key_source := g.render_call_argument_expression(left_tokens[open + 1..close], key_type) or {
		return none
	}
	mut map_address := ''
	mut nested := false
	if nested_map := g.render_mutable_map_value_pointer(base_tokens) {
		if nested_map.typ_text() != flat.source_type_name(map_type).trim_right('*') {
			return none
		}
		map_address = nested_map.source
		nested = true
	} else {
		map_source := g.render_member_receiver(base_tokens) or {
			g.render_raw_expression_tokens(base_tokens) or { return none }
		}
		map_address = if flat.source_type_name(map_type).ends_with('*') {
			map_source
		} else {
			'&(${map_source})'
		}
	}
	update := g.render_assignment_operator_source('__vf_compound_value', '__vf_compound_rhs',
		value_type, operator) or { return none }
	value_storage_type := g.collection_storage_type(value_type)
	missing_value := g.map_lookup_missing_value_expression(value_type)
	key_declaration := '${g.collection_runtime_type(key_type)} __vf_compound_key = (${key_source}); '
	entry_read := 'map *__vf_compound_map = (map *)(${map_address}); ${value_storage_type} *__vf_compound_entry = (${value_storage_type} *)builtin__map_get_check(__vf_compound_map, &__vf_compound_key); ${value_storage_type} __vf_compound_value = __vf_compound_entry == NULL ? ${missing_value} : *__vf_compound_entry; '
	value_open := '${value_storage_type} __vf_compound_rhs = ('
	store := '${update}; builtin__map_set(__vf_compound_map, &__vf_compound_key, &__vf_compound_value); (void)0; })'
	if nested {
		return FastcMapAssignmentWrap{
			prefix:     '({ ${key_declaration}${value_open}'
			suffix:     '); ${entry_read}${store}'
			value_type: value_type
		}
	}
	return FastcMapAssignmentWrap{
		prefix:     '({ ${key_declaration}${entry_read}${value_open}'
		suffix:     '); ${store}'
		value_type: value_type
	}
}

// render_map_index_compound_assignment lowers a whole `m[k] op= value` expression through
// render_map_index_compound_wrapping.
fn (g &Parser) render_map_index_compound_assignment(tokens []FastcExpressionToken) ?FastcRenderedExpression {
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
			else {
				if depth == 0 && item.tok.is_assignment() {
					assignment_index = i
					break
				}
			}
		}
	}
	if assignment_index <= 0 || assignment_index + 1 >= tokens.len {
		return none
	}
	wrap := g.render_map_index_compound_wrapping(tokens[..assignment_index], tokens[assignment_index].tok) or {
		return none
	}
	value_source := g.render_call_argument_expression(tokens[assignment_index + 1..],
		wrap.value_type) or { return none }
	return FastcRenderedExpression{
		source: '${wrap.prefix}${value_source}${wrap.suffix}'
		typ:    flat.source_type_void
	}
}

fn (g &Parser) render_ordinary_collection_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if enum_print := g.render_enum_print_expression(tokens) {
		return enum_print
	}
	close := fastc_matching_rpar(tokens, 1) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	call_arguments := fastc_call_arguments(tokens, 1, close) or { return none }
	if call_arguments.len != 1 {
		return none
	}
	typ := fastc_normalize_inferred_type(g.infer_expression_type(call_arguments[0]) or { return none })
	value := g.render_call_argument_expression(call_arguments[0], typ) or { return none }
	function_name := if typ == flat.source_type_bool {
		'v_fastc_${tokens[0].lit}_bool'
	} else {
		tokens[0].lit
	}
	return FastcRenderedExpression{
		source: '${function_name}(${value})'
		typ:    flat.source_type_void
	}
}

fn (g &Parser) render_bool_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if g.selfhost || tokens.len < 4 || tokens[0].tok != .name || tokens[0].lit !in [
		'print',
		'println',
	] || tokens[1].tok != .lpar || tokens.last().tok != .rpar {
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
	if fastc_normalize_inferred_type(argument_type) != flat.source_type_bool {
		return none
	}
	argument := g.render_call_argument_expression(call_arguments[0], flat.source_type_id('bool')) or { return none }
	function_name := if tokens[0].lit == 'println' {
		'v_fastc_println_bool'
	} else {
		'v_fastc_print_bool'
	}
	return FastcRenderedExpression{
		source: '${function_name}((bool)(${argument}))'
		typ:    flat.source_type_void
	}
}

fn (g &Parser) render_ordinary_string_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if g.selfhost || tokens.len < 4 || tokens[0].tok != .name || tokens[0].lit !in [
		'print',
		'println',
	] || tokens[1].tok != .lpar || tokens.last().tok != .rpar {
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
	if fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(argument_type))) != 'string' {
		return none
	}
	argument := g.render_call_argument_expression(call_arguments[0], flat.source_type_id('string')) or { return none }
	return FastcRenderedExpression{
		source: '${tokens[0].lit}(${argument})'
		typ:    flat.source_type_void
	}
}

fn (g &Parser) render_enum_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .name || tokens[1].tok != .lpar || tokens.last().tok != .rpar {
		return none
	}
	// `eprint`/`eprintln` exist only in the real builtin of the selfhost path.
	to_stderr := tokens[0].lit in ['eprint', 'eprintln']
	if tokens[0].lit !in ['print', 'println'] && !(to_stderr && g.selfhost) {
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
	enum_key := g.underlying_enum_type_key(flat.source_type_name(type_key)) or { return none }
	c_type := fastc_c_declared_type_name(enum_key)
	argument := g.render_call_argument_expression(call_arguments[0], flat.source_type_id(c_type)) or { return none }
	if to_stderr {
		return FastcRenderedExpression{
			source: 'builtin__${tokens[0].lit}(v_fastc_enum_str_${c_type}(${argument}))'
			typ:    flat.source_type_void
		}
	}
	return FastcRenderedExpression{
		source: 'v_fastc_print_enum_${c_type}(${argument}, ${if tokens[0].lit == 'println' {
			'true'
		} else {
			'false'
		}})'
		typ:    flat.source_type_void
	}
}

fn (g &Parser) member_expression_refers_to_storage(tokens []FastcExpressionToken) bool {
	if tokens.len >= 3 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		if close := fastc_matching_rpar(tokens, 0) {
			if close == tokens.len - 1 {
				return g.member_expression_refers_to_storage(tokens[1..tokens.len - 1])
			}
		}
	}
	if tokens.len == 1 && tokens[0].tok == .name {
		return tokens[0].is_storage || tokens[0].lit in g.locals
			|| fastc_global_key(g.module_name, tokens[0].lit) in g.globals
	}
	if tokens.len >= 3 && tokens.last().tok == .name && tokens[tokens.len - 2].tok == .dot {
		receiver_tokens := tokens[..tokens.len - 2]
		receiver_type := (g.infer_expression_type(receiver_tokens) or { u32(0) })
		return fastc_is_pointer_type(g.underlying_alias_type(receiver_type))
			|| g.member_expression_refers_to_storage(receiver_tokens)
	}
	return false
}

// value_address_expression preserves local storage and gives computed values stable backing storage.
fn (g &Parser) value_address_expression(value_type_source_id u32, expression string, tokens []FastcExpressionToken) string {
	value_type := flat.source_type_name(value_type_source_id)
	if tokens.len >= 3 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		if close := fastc_matching_rpar(tokens, 0) {
			if close == tokens.len - 1 {
				return g.value_address_expression(value_type_source_id, expression, tokens[1..tokens.len - 1])
			}
		}
	}
	if g.member_expression_refers_to_storage(tokens) {
		return '&(${expression})'
	}
	if !fastc_expression_tokens_contain(tokens, .dotdot) {
		if indexed := g.render_array_access_expression(tokens) {
			if indexed.source == expression && !indexed.source.starts_with('builtin__string_at(') {
				return '&(${expression})'
			}
		}
	}
	needs_storage := expression.contains('({') || tokens.len == 0
		|| tokens[0].tok !in [.name, .mul]
		|| tokens.any(it.tok in [.lpar, .lcbr, .dotdot]
			|| (it.tok == .name && it.lit.contains('(')))
	if needs_storage {
		storage_type := fastc_output_c_type(value_type)
		return '(${storage_type} *)${fastc_box_expression(storage_type, expression)}'
	}
	return '&(${expression})'
}

fn (g &Parser) render_selfhost_print_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if !g.selfhost || tokens.len < 4 || tokens[0].tok != .name || tokens[0].lit !in [
		'print',
		'println',
		'eprint',
		'eprintln',
	] || tokens[1].tok != .lpar || tokens.last().tok != .rpar {
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
	if fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(argument_type))) == 'string' {
		return none
	}
	argument := g.render_call_argument_expression(call_arguments[0], argument_type) or {
		return none
	}
	argument_type_name := flat.source_type_name(g.underlying_alias_type(argument_type))
	if argument_type_name == 'Option' || (argument_type_name.ends_with('*')
		&& !g.is_reference_local_base(call_arguments[0], argument_type)
		&& !g.expression_uses_member_smartcast(call_arguments[0])) {
		// `&T` and `?T` values print as `&...` and `Option(...)`, even when `T`
		// has a `str()` method.
		string_value := g.print_value_string_expression(argument, argument_type,
			call_arguments[0]) or { return none }
		return FastcRenderedExpression{
			source: 'builtin__${tokens[0].lit}(${string_value})'
			typ:    flat.source_type_void
		}
	}
	method_key, embedded_path := g.resolve_method(argument_type, 'str')
	if embedded_path.len > 0 {
		return none
	}
	signature := g.functions[method_key] or {
		// Without a `str()` method (a sum type, struct, array or map), the value
		// prints the way `'${value}'` formats it.
		string_value := g.print_value_string_expression(argument, argument_type,
			call_arguments[0]) or { return none }
		return FastcRenderedExpression{
			source: 'builtin__${tokens[0].lit}(${string_value})'
			typ:    flat.source_type_void
		}
	}
	if signature.parameter_types.ids.len == 0 || signature.return_type != flat.source_type_string {
		return none
	}
	expected_receiver := signature.parameter_types.at(0)
	receiver_argument := if expected_receiver.ends_with('*') && !flat.source_type_name(argument_type).ends_with('*') {
		g.value_address_expression(flat.source_type_id(expected_receiver.trim_right('*')), argument, call_arguments[0])
	} else if !expected_receiver.ends_with('*') && flat.source_type_name(argument_type).ends_with('*') {
		'*(${argument})'
	} else {
		argument
	}
	method_c_name := g.c_method_name(method_key, signature.module_name, expected_receiver, 'str')
	string_value := '${method_c_name}(${receiver_argument})'
	return FastcRenderedExpression{
		source: 'builtin__${tokens[0].lit}(${string_value})'
		typ:    flat.source_type_void
	}
}

// print_value_string_expression returns the C string of `value`, a value of
// `value_type`, formatted as read_interpolated_string formats it: `&...` for a
// pointer, `Option(...)` for an option. A `mut` local or a smartcast, which C
// holds through a pointer, prints as its value.
fn (g &Parser) print_value_string_expression(value string, value_type u32, value_tokens []FastcExpressionToken) ?string {
	type_name_id := g.underlying_alias_type(value_type)
	type_name := flat.source_type_name(type_name_id)
	if type_name == '__v_result' || type_name.ends_with('**') {
		return none
	}
	mut w := unsafe { &Parser(g) }
	if type_name == 'Option' {
		option_helper := w.fastc_option_str_name(g.option_value_type_for_expression(value_tokens))?
		return '${option_helper}(${value})'
	}
	if type_name.ends_with('*') {
		if g.is_reference_local_base(value_tokens, value_type)
			|| g.expression_uses_member_smartcast(value_tokens) {
			return g.print_value_string_expression('(*(${value}))',
				flat.source_type_id(type_name[..type_name.len - 1]), value_tokens)
		}
		return w.fastc_reference_str_expression(value, type_name, value_tokens)
	}
	if element_type := fastc_fixed_array_element_type(type_name_id) {
		helper := w.fastc_fixed_array_str_name(type_name)?
		// A fixed array field or global is raw C array storage; a local is FastC's
		// wrapper struct.
		is_member_array := value_tokens.len >= 3 && value_tokens.last().tok == .name
			&& value_tokens[value_tokens.len - 2].tok == .dot
		is_global_array := value_tokens.len == 1 && value_tokens[0].tok == .name
			&& fastc_global_key(g.module_name, value_tokens[0].lit) in g.globals
		data_source := if is_member_array || is_global_array {
			value
		} else {
			'(${value}).data'
		}
		return '${helper}((${flat.source_type_name(element_type)} *)(${data_source}))'
	}
	return w.fastc_struct_field_str_expression(value, type_name)
}

fn (g &Parser) render_struct_literal_with_defaults(c_type_source_id u32, layout_type_source_id u32, explicit_initializers []string, rendered_fields []string, rendered_fields_by_name map[string]string) FastcRenderedExpression {
	c_type := flat.source_type_name(c_type_source_id)
	layout_type := flat.source_type_name(layout_type_source_id)
	base_type := fastc_trim_pointer_suffix(c_type)
	if g.c_global_static_initializer {
		literal := '(${base_type}){${if rendered_fields.len > 0 {
			rendered_fields.join(',')
		} else {
			'0'
		}}}'
		return FastcRenderedExpression{
			source: if c_type.ends_with('*') { '&${literal}' } else { literal }
			typ:    c_type_source_id
		}
	}
	mut assignments := []string{cap: rendered_fields.len}
	mut initializers := []string{}
	mut initialized_fields := map[string]bool{}
	for field in g.struct_field_info[layout_type] {
		if fastc_fixed_array_element_type(field.typ) == none {
			continue
		}
		if rendered_field := rendered_fields_by_name[field.name] {
			value := rendered_field.all_after('=')
			if value.starts_with('{') {
				initializers << rendered_field
			} else {
				name := fastc_c_identifier(field.name)
				assignments << 'memcpy(__vf_sd.${name}, (${value}).data, sizeof(__vf_sd.${name}));'
			}
			initialized_fields[rendered_field] = true
		}
	}
	for field in rendered_fields {
		if field in initialized_fields {
			continue
		}
		assignments << '__vf_sd${field};'
	}
	initializer := if initializers.len > 0 {
		'(${base_type}){${initializers.join(',')}}'
	} else {
		'(${base_type}){0}'
	}
	result := if c_type.ends_with('*') {
		'(${c_type})v_fastc_interface_box(&__vf_sd, sizeof(${base_type}))'
	} else {
		'__vf_sd'
	}
	return FastcRenderedExpression{
		source: '({ ${explicit_initializers.join(' ')} ${base_type} __vf_sd = ${initializer}; ${assignments.join(' ')} ${result}; })'
		typ:    c_type_source_id
	}
}

fn (g &Parser) render_empty_struct_initializer(c_type_source_id u32) string {
	c_type := flat.source_type_name(c_type_source_id)
	layout_type := fastc_trim_pointer_suffix(c_type)
	mut rendered_fields := []string{}
	mut rendered_fields_by_name := map[string]string{}
	// The struct's rendered defaults are consulted; the constants phase
	// re-parses a file that did so after they are ready.
	fastc_note_field_defaults_use(g)
	for field in g.struct_field_info[layout_type] {
		field_default := g.struct_field_initializer_default(field)
		if field_default == '' {
			continue
		}
		rendered_field := if g.c_global_static_initializer
			&& fastc_fixed_array_element_type(field.typ) != none {
			'.${fastc_c_identifier(field.name)}=${fastc_static_fixed_array_initializer(field_default)}'
		} else {
			'.${fastc_c_identifier(field.name)}=(${field_default})'
		}
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
	return g.render_struct_literal_with_defaults(c_type_source_id, flat.source_type_id(layout_type), empty_initializers, rendered_fields, rendered_fields_by_name).source
}

fn (g &Parser) struct_field_initializer_default(field FastcStructField) string {
	mut value := ''
	if field.default_value != '' {
		value = field.default_value
	} else if field.typ_text() == 'Option' {
		// An option field a literal leaves out is `none`; a zeroed Option would be a
		// present value with no payload.
		value = '(Option){.state=2}'
	} else if !fastc_is_pointer_type(g.underlying_alias_type(field.typ)) && g.struct_type_has_initializer_defaults(field.typ) {
		value = g.render_empty_struct_initializer(field.typ)
	}
	if value == '' {
		return ''
	}
	if !field.is_shared_pointer {
		return value
	}
	return '({ ${field.typ_text()} __vf_shared_field_value = (${value}); (${field.typ_text()}*)v_fastc_interface_box(&__vf_shared_field_value, sizeof(${field.typ_text()})); })'
}

fn (g &Parser) struct_type_has_initializer_defaults(c_type u32) bool {
	if fastc_is_pointer_type(g.underlying_alias_type(c_type)) {
		return false
	}
	layout_type := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(c_type)))
	if layout_type !in g.struct_field_info {
		return false
	}
	for field in g.struct_field_info[layout_type] {
		if field.default_value != '' || field.typ_text() == 'Option' {
			return true
		}
		if field.typ != c_type && g.struct_type_has_initializer_defaults(field.typ) {
			return true
		}
	}
	return false
}

// fastc_type_is_declared_struct reports whether `c_type` names a declared struct/union.
// (A missing key defaults to `.struct_`, the zero enum value, so the key must be present.)
fn (g &Parser) fastc_type_is_declared_struct(c_type u32) bool {
	key := g.semantic_type_key(c_type)
	return flat.source_type_name(key) in g.declared_kinds && g.declared_kinds[flat.source_type_name(key)] in [
		.struct_,
		.union_,
	]
}

fn (g &Parser) render_named_struct_initializer(c_type_source_id u32, fields [][]FastcExpressionToken) ?string {
	c_type := flat.source_type_name(c_type_source_id)
	mut tokens := [
		FastcExpressionToken{
			tok: .name
			lit: c_type
			typ: c_type_source_id
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
	mut rendered := rendered_expression
	mut changed := false
	if fields := g.struct_fields[fastc_trim_pointer_suffix(flat.source_type_name(c_type))] {
		for field_name, _ in fields.ids {
			for module_name in [g.module_name, 'builtin'] {
				constant_name := g.constants[fastc_constant_key(module_name, field_name)] or { '' }
				mut resolved_names := []string{}
				if constant_name != '' {
					resolved_names << constant_name
				}
				function_key := fastc_function_key(module_name, field_name)
				if function_key in g.functions || function_key in g.mono_functions {
					resolved_names << g.c_function_name_for_key(function_key)
				}
				for resolved_name in resolved_names {
					needle := '.${resolved_name}='
					replaced := fastc_replace(rendered, needle, '.${fastc_c_identifier(field_name)}=')
					if replaced.str != rendered.str {
						rendered = replaced
						changed = true
					}
				}
			}
		}
	} else {
		return none
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
	if assignment_index <= 0 || assignment_index + 1 >= tokens.len || tokens[assignment_index - 1].tok != .rsbr {
		return none
	}
	left := g.render_array_access_expression(tokens[..assignment_index]) or { return none }
	right := g.render_call_argument_expression(tokens[assignment_index + 1..], left.typ) or {
		return none
	}
	operator := tokens[assignment_index].tok
	source := if overloaded := g.render_overloaded_assignment(left.source, right, left.typ, operator) {
		overloaded
	} else if operator == .power_assign {
		g.render_power_assignment(left.source, right, left.typ) or { return none }
	} else if g.checked_target_division_enabled() && operator in [.div_assign, .mod_assign] {
		g.render_checked_integer_division_assignment(left.source, right, left.typ, operator)
	} else if operator == .right_shift_unsigned_assign {
		g.render_unsigned_right_shift_assignment(left.source, right, left.typ) or { return none }
	} else if g.translated && operator in [.left_shift_assign, .right_shift_assign] {
		g.render_guarded_shift_assignment(left.source, right, left.typ, operator) or { return none }
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
	if g.array_initializer_type(initializer_type_tokens) == none && g.map_initializer_type(initializer_type_tokens) == none {
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
	narrowed_target := if tokens[assignment_index].tok == .assign {
		g.assigned_narrowed_member(left_tokens)
	} else {
		none
	}
	// A declaration guard introduces a new binding; an existing local or
	// function with that name cannot supply the RHS's expected type.
	left_type := if target := narrowed_target {
		target.typ
	} else if tokens[assignment_index].tok == .decl_assign {
		g.infer_expression_type(tokens[assignment_index + 1..]) or { return none }
	} else {
		g.infer_expression_type(left_tokens) or { return none }
	}
	if left_type == 0 {
		return none
	}
	mut left := if target := narrowed_target { target.source } else { '' }
	if left == '' && left_tokens.len >= 4 && left_tokens.last().tok == .name && left_tokens[left_tokens.len - 2].tok == .dot && left_tokens[left_tokens.len - 3].tok == .rpar {
		// `recv.method(args).field = value`: the target is a field of a method-call result (a
		// pointer receiver like `table.sym(id)`), which the member-receiver renderer cannot spell.
		receiver_tokens := left_tokens[..left_tokens.len - 2]
		field := left_tokens.last().lit
		raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
		if call := g.render_method_call_expression(receiver_tokens, raw_receiver) {
			access := if flat.source_type_name(g.underlying_alias_type(call.typ)).ends_with('*') {
				'->'
			} else {
				'.'
			}
			left = '(${call.source})${access}${field}'
		}
	}
	if left == '' {
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
	}
	rhs_tokens := tokens[assignment_index + 1..]
	mut right := ''
	if rhs_tokens.len == 2 && rhs_tokens[0].tok == .lsbr && rhs_tokens[1].tok == .rsbr && fastc_trim_pointer_suffix(flat.source_type_name(left_type)).starts_with('Array_') {
		// An empty array literal `[]` has no element type of its own; assigned to an
		// array target it lowers to a typed empty array from the target's type. The
		// header must retain the element size so a later `<<` copies actual elements.
		array_type := fastc_trim_pointer_suffix(flat.source_type_name(left_type))
		element_type := g.array_element_type(flat.source_type_id(array_type)) or { return none }
		right = '((${array_type})builtin____new_array(0, 0, sizeof(${flat.source_type_name(fastc_normalize_inferred_type(element_type))})))'
	} else if g.selfhost && g.boolean_expression_has_narrowing(rhs_tokens) {
		// `x.f = a is T && a.g …`: the RHS is a flow-sensitive narrowing boolean, whose right
		// conjuncts read the smart-cast the left `is` registers. render_call_argument_expression
		// cannot register that (it is non-mut); route through the narrowing renderer, which
		// saves/restores the temporary member smart-casts itself.
		mut w := unsafe { &Parser(g) }
		if narrowed := w.render_narrowing_boolean_expression(rhs_tokens) {
			right = narrowed
		} else {
			right = g.render_call_argument_expression(rhs_tokens, left_type) or { return none }
		}
	} else {
		right = g.render_call_argument_expression(rhs_tokens, left_type) or { return none }
	}
	operator := tokens[assignment_index].tok
	option_payload_type := g.option_value_type_for_expression(left_tokens)
	if g.selfhost && operator == .assign && flat.source_type_name(left_type) in [
		'Option',
		'__v_result',
	] && option_payload_type != u32(0) {
		rhs_type := fastc_normalize_inferred_type(g.infer_expression_type(rhs_tokens) or { u32(0) })
		if flat.source_type_name(rhs_type) !in ['Option', '__v_result'] {
			if fastc_trim_pointer_suffix(flat.source_type_name(rhs_type)) == 'IError' {
				right = '(__v_result){.err=${right}, .state=1}'
			} else {
				// `right` already holds the fully lowered RHS; for a narrowing boolean it
				// carries the member smart-cast that a re-render through
				// render_call_argument_expression would drop, so reuse it as the payload.
				payload := if g.boolean_expression_has_narrowing(rhs_tokens) {
					right
				} else {
					g.render_call_argument_expression(rhs_tokens, option_payload_type) or {
						right
					}
				}
				right = g.option_success_expression(left_type, option_payload_type, payload)
			}
		}
	}
	if g.selfhost && operator == .assign && fastc_contains(flat.source_type_name(left_type), '_FASTC_ARRAY_OF_')
		&& g.fixed_array_uses_raw_storage(left_tokens) {
		// `s.arr = [N]T{}` zeroes a raw-storage fixed array; the empty `[N]T{}` literal is not
		// lowered to a value, so memset rather than memcpy from an unrendered right-hand side.
		if fastc_is_empty_fixed_array_literal(rhs_tokens) {
			return FastcRenderedExpression{
				source: 'memset(${left}, 0, sizeof(${left}))'
				typ:    left_type
			}
		}
		// The target is a raw-storage fixed-array member/global (a C `T[N]` array, which cannot be
		// assigned); copy the source array's bytes instead. A by-value wrapper source keeps its
		// array in `.data`, while another raw-storage source is already a bare array.
		right_data := if g.fixed_array_uses_raw_storage(rhs_tokens) {
			// The bare array, not the wrapper value an argument would be given.
			g.render_member_receiver(rhs_tokens) or { right }
		} else {
			'(${right}).data'
		}
		return FastcRenderedExpression{
			source: 'memcpy(${left}, ${right_data}, sizeof(${left}))'
			typ:    left_type
		}
	}
	source := g.render_assignment_operator_source(left, right, left_type, operator) or {
		return none
	}
	return FastcRenderedExpression{
		source: source
		typ:    left_type
	}
}

// assigned_narrowed_member is the target of `x.f = value` while `x.f` is narrowed by an `is`
// test: the boxed member itself, typed as its sum or interface type. As in cgen, the value is
// boxed into a new sum value, rather than written through the narrowed payload pointer, which
// would update a temporary, or a payload other copies share.
fn (g &Parser) assigned_narrowed_member(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if g.member_smartcasts.len == 0 {
		return none
	}
	path := fastc_indexed_member_chain_path(tokens) or { return none }
	if !path.contains('.') {
		return none
	}
	smartcast := g.member_smartcasts[path] or { return none }
	if smartcast.subject_source == '' || smartcast.subject_type == u32(0) {
		return none
	}
	subject_type := flat.source_type_name(smartcast.subject_type)
	if subject_type.ends_with('*') {
		return FastcRenderedExpression{
			source: '(*${smartcast.subject_source})'
			typ:    flat.source_type_id(subject_type[..subject_type.len - 1])
		}
	}
	return FastcRenderedExpression{
		source: smartcast.subject_source
		typ:    smartcast.subject_type
	}
}

// end_assigned_member_narrowing ends the narrowing of a member that the statement `tokens`
// assigned to, and of every member reached through it: as in V, the member has its sum or
// interface type again after the assignment.
fn (mut g Parser) end_assigned_member_narrowing(tokens []FastcExpressionToken) {
	if g.member_smartcasts.len == 0 {
		return
	}
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
	if assignment_index <= 0 || tokens[assignment_index].tok != .assign {
		return
	}
	_ := g.assigned_narrowed_member(tokens[..assignment_index]) or { return }
	path := fastc_indexed_member_chain_path(tokens[..assignment_index]) or { return }
	for narrowed_path in g.member_smartcasts.keys() {
		if narrowed_path == path || narrowed_path.starts_with(path + '.')
			|| narrowed_path.starts_with(path + '[') {
			g.member_smartcasts.delete(narrowed_path)
		}
	}
	g.type_memo.clear()
}

// render_assignment_operator_source spells `left op right` for the C lvalue `left` of type
// `left_type`. String concatenation, operator overloads, checked division and the guarded
// shifts are calls rather than the C operator.
fn (g &Parser) render_assignment_operator_source(left string, right string, left_type u32, operator token.Token) ?string {
	return if operator == .plus_assign && g.underlying_alias_type(left_type) == flat.source_type_string {
		'${left}=builtin__string_plus(${left},${right})'
	} else if overloaded := g.render_overloaded_assignment(left, right, left_type, operator) {
		overloaded
	} else if operator == .power_assign {
		g.render_power_assignment(left, right, left_type) or { return none }
	} else if g.checked_target_division_enabled() && operator in [.div_assign, .mod_assign] {
		g.render_checked_integer_division_assignment(left, right, left_type, operator)
	} else if operator == .right_shift_unsigned_assign {
		g.render_unsigned_right_shift_assignment(left, right, left_type) or { return none }
	} else if g.translated && operator in [.left_shift_assign, .right_shift_assign] {
		g.render_guarded_shift_assignment(left, right, left_type, operator) or { return none }
	} else {
		'${left}${operator.str()}${right}'
	}
}

fn (g &Parser) render_overloaded_assignment(target string, value string, target_type_source_id u32, assignment token.Token) ?string {
	target_type := flat.source_type_name(target_type_source_id)
	operator := match assignment {
		.plus_assign { '+' }
		.minus_assign { '-' }
		.mul_assign { '*' }
		.div_assign { '/' }
		.mod_assign { '%' }
		.power_assign { '**' }
		.or_assign { '|' }
		.xor_assign { '^' }
		else {
			return none
		}
	}
	method_key := g.method_function_key(target_type_source_id, operator)
	signature := g.functions[method_key] or { return none }
	if signature.parameter_types.ids.len < 2 || signature.is_disabled {
		return none
	}
	method_name := g.c_method_name(method_key, signature.module_name, signature.parameter_types.at(0), operator)
	return '({ ${target_type} *__vf_overloaded_assignment_target = &(${target}); *__vf_overloaded_assignment_target = ${method_name}(*__vf_overloaded_assignment_target,${value}); })'
}

fn (g &Parser) checked_target_division_enabled() bool {
	return g.selfhost && g.translated && g.prefs.target_libc_headers
}

fn (g &Parser) checked_integer_operation_type(operand_type u32) string {
	resolved := flat.source_type_name(g.underlying_alias_type(operand_type))
	return match resolved {
		'int', 'integer literal', 'negative integer literal' { fastc_output_c_type('int') }
		'char', 'i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'i64', 'u64', 'isize', 'uint', 'usize' {
			fastc_output_c_type(resolved)
		}
		'rune' { 'u32' }
		'unsigned int' { 'u32' }
		else { '' }
	}
}

fn (g &Parser) checked_integer_operand_type(tokens []FastcExpressionToken, inferred u32) u32 {
	value_tokens := fastc_strip_paren_tokens(tokens)
	if value_tokens.len == 1 && value_tokens[0].tok == .name {
		if local := g.locals[value_tokens[0].lit] {
			if local.is_reference && local.typ_text().ends_with('*') {
				value_type := flat.source_type_id(local.typ_text()[..local.typ_text().len - 1])
				if g.checked_integer_operation_type(value_type) != '' {
					return value_type
				}
			}
		}
	}
	return inferred
}

fn fastc_checked_integer_division_body(operator token.Token) string {
	message := if operator in [.mod, .mod_assign] { 'modulo by zero' } else { 'division by zero' }
	op := if operator in [.mod, .mod_assign] { '%' } else { '/' }
	// C promotes narrow signed integers before division. For wider signed
	// operands, the minimum value divided by -1 would instead be undefined.
	return 'if (__vf_div_rhs == 0) builtin__panic(_S("${message}")); if ((__vf_div_type)-1 < (__vf_div_type)0 && sizeof(__vf_div_type) >= 4 && __vf_div_rhs == (__vf_div_type)-1 && __vf_div_lhs == (__vf_div_type)((u64)1 << (sizeof(__vf_div_type) * 8 - 1))) builtin__panic(_S("integer overflow")); (__vf_div_type)(__vf_div_lhs ${op} __vf_div_rhs)'
}

fn (g &Parser) render_checked_integer_division_expression(left string, right string, result_type u32, operator token.Token) string {
	known_type := g.checked_integer_operation_type(result_type)
	// For C declarations without V metadata, typeof preserves their actual
	// integer representation without evaluating either operand. Reject other
	// representations at C compilation rather than emitting unchecked division.
	ct := if known_type != '' { known_type } else { '__typeof__((${left}) / (${right}))' }
	return '({ typedef ${ct} __vf_div_type; _Static_assert(__builtin_classify_type((__vf_div_type)0) == 1, "FastC checked division requires integer operands"); __vf_div_type __vf_div_lhs = (__vf_div_type)(${left}); __vf_div_type __vf_div_rhs = (__vf_div_type)(${right}); ${fastc_checked_integer_division_body(operator)}; })'
}

fn (g &Parser) render_checked_integer_division_assignment(target string, value string, target_type u32, operator token.Token) string {
	known_type := g.checked_integer_operation_type(target_type)
	ct := if known_type != '' { known_type } else { '__typeof__(*__vf_div_target)' }
	checked := '({ ${fastc_checked_integer_division_body(operator)}; })'
	// Capture the lvalue before the RHS, and read/write its storage once. This
	// also preserves volatile storage and side effects in pointer/index targets.
	return '({ __typeof__(&(${target})) __vf_div_target = &(${target}); typedef ${ct} __vf_div_type; _Static_assert(__builtin_classify_type((__vf_div_type)0) == 1, "FastC checked division requires integer operands"); __vf_div_type __vf_div_rhs = (__vf_div_type)(${value}); __vf_div_type __vf_div_lhs = (__vf_div_type)(*__vf_div_target); *__vf_div_target = ${checked}; })'
}

fn (g &Parser) shift_type_parts(operand_type u32) ?(string, string) {
	resolved_type := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(operand_type)))
	return match resolved_type {
		'char', 'i8', 'u8' { 'u8', '8' }
		'i16', 'u16' { 'u16', '16' }
		'i32', 'rune', 'u32', 'unsigned int' { 'u32', '32' }
		'i64', 'u64' { 'u64', '64' }
		'int' { 'u${g.prefs.target.pointer_bits}', '${g.prefs.target.pointer_bits}' }
		'isize', 'uint', 'usize' { 'usize', '${g.prefs.target.pointer_bits}' }
		else {
			return none
		}
	}
}

fn (g &Parser) render_guarded_shift_expression(left string, right string, left_type_source_id u32, operator token.Token) ?string {
	left_type := flat.source_type_name(left_type_source_id)
	operand_type := if left_type in ['integer literal', 'negative integer literal'] {
		if operator == .right_shift_unsigned { 'i32' } else { 'int' }
	} else {
		left_type
	}
	unsigned_type, bits := g.shift_type_parts(flat.source_type_id(operand_type)) or { return none }
	value_type := fastc_output_c_type(operand_type)
	result_type := if operator == .right_shift_unsigned { unsigned_type } else { value_type }
	shift_type := if operator in [.left_shift, .right_shift_unsigned] {
		unsigned_type
	} else {
		value_type
	}
	op := if operator == .left_shift { '<<' } else { '>>' }
	return '({ ${shift_type} __vf_shift_value = (${shift_type})(${left}); u64 __vf_shift_count = (u64)(${right}); __vf_shift_count >= ${bits} ? (${result_type})0 : (${result_type})(__vf_shift_value ${op} __vf_shift_count); })'
}

fn (g &Parser) render_guarded_shift_assignment(target string, value string, target_type_source_id u32, assignment token.Token) ?string {
	target_type := flat.source_type_name(target_type_source_id)
	op := if assignment == .left_shift_assign {
		token.Token.left_shift
	} else {
		token.Token.right_shift
	}
	shift := g.render_guarded_shift_expression('*__vf_shift_target', value, target_type_source_id, op) or { return none }
	ct := fastc_output_c_type(target_type)
	return '({ ${ct} *__vf_shift_target = &(${target}); *__vf_shift_target = (${ct})(${shift}); })'
}

fn (g &Parser) render_unsigned_right_shift_assignment(target string, value string, target_type_source_id u32) ?string {
	target_type := flat.source_type_name(target_type_source_id)
	unsigned_type, bits := g.shift_type_parts(target_type_source_id) or { return none }
	target_c_type := fastc_output_c_type(target_type)
	return '({ ${target_c_type} *__vf_unsigned_shift_target = &(${target}); ${unsigned_type} __vf_unsigned_shift_value = (${unsigned_type})(*__vf_unsigned_shift_target); u64 __vf_unsigned_shift_count = (u64)(${value}); *__vf_unsigned_shift_target = (${target_c_type})(__vf_unsigned_shift_count >= ${bits} ? (${unsigned_type})0 : (__vf_unsigned_shift_value >> __vf_unsigned_shift_count)); })'
}

fn fastc_overloaded_binary_precedence(tok token.Token) int {
	return match tok {
		// Keep these groups aligned with V's binding powers, not C's. In V,
		// `+ - | ^` share one level and `* / % << >> >>> &` share the next.
		// Parenthesizing the recursively rendered operands below then preserves
		// both V's grouping and left associativity in the generated C.
		.pipe, .xor, .plus, .minus { 1 }
		.amp, .left_shift, .right_shift, .right_shift_unsigned, .mul, .div, .mod { 2 }
		else { 0 }
	}
}

// fastc_token_ends_operand reports whether `tok` can end an operand, so that an operator
// after it is binary (`a - b`) rather than unary (`a & -b`, `x + &y`).
fn fastc_token_ends_operand(tok token.Token) bool {
	return tok in [.name, .number, .string, .char, .rpar, .rsbr, .rcbr, .key_true, .key_false,
		.key_nil, .key_none, .inc, .dec, .question, .not]
}

fn fastc_has_top_level_assignment(tokens []FastcExpressionToken) bool {
	mut depth := 0
	for item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok.is_assignment() {
			return true
		}
	}
	return false
}

fn fastc_has_top_level_bitwise_operator(tokens []FastcExpressionToken) bool {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && i > 0 && i + 1 < tokens.len && item.tok in [.amp, .pipe, .xor] {
			return true
		}
	}
	return false
}

fn (g &Parser) render_overloaded_comparison_expression(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, operator token.Token) ?FastcRenderedExpression {
	if operator in [.eq, .ne] {
		pointer_type := g.borrowed_object_pointer_comparison_type(left_tokens, right_tokens) or {
			g.borrowed_object_pointer_comparison_type(right_tokens, left_tokens) or { u32(0) }
		}
		if pointer_type != 0 {
			left := g.render_call_argument_expression(left_tokens, pointer_type) or { return none }
			right := g.render_call_argument_expression(right_tokens, pointer_type) or { return none }
			return FastcRenderedExpression{
				source: '((${left})${operator.str()}(${right}))'
				typ:    flat.source_type_id('bool')
			}
		}
	}
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
	argument_type := (g.infer_expression_type(argument_tokens) or { u32(0) })
	// Equality against `nil` is pointer identity even when the pointee type has
	// overloaded `==`. Calling that overload would pass NULL as the second
	// receiver and dereference it (for example `&ast.Scope == unsafe { nil }`).
	if operator in [.eq, .ne] && (receiver_type == flat.source_type_nil || argument_type == flat.source_type_nil) {
		return none
	}
	method_key := g.method_function_key(receiver_type, method_operator)
	if method_key !in g.functions {
		return none
	}
	signature := g.functions[method_key]
	if signature.parameter_types.ids.len < 2 || signature.is_disabled {
		return none
	}
	receiver := g.render_call_argument_expression(receiver_tokens, signature.parameter_types.ids[0]) or {
		return none
	}
	argument := g.render_call_argument_expression(argument_tokens, signature.parameter_types.ids[1]) or {
		return none
	}
	method_c_name := g.c_method_name(method_key, signature.module_name, signature.parameter_types.at(0), method_operator)
	call := '${method_c_name}(${receiver},${argument})'
	return FastcRenderedExpression{
		source: if negate { '!(${call})' } else { call }
		typ:    flat.source_type_bool
	}
}

fn (g &Parser) borrowed_object_pointer_comparison_type(reference_tokens []FastcExpressionToken, other_tokens []FastcExpressionToken) ?u32 {
	reference := fastc_strip_paren_tokens(reference_tokens)
	if reference.len != 1 || reference[0].tok != .name {
		return none
	}
	local := g.locals[reference[0].lit] or { return none }
	if !local.is_reference {
		return none
	}
	storage := g.underlying_alias_type(local.typ)
	key := g.semantic_type_key(storage)
	if !flat.source_type_name(storage).ends_with('*') || flat.source_type_name(key) !in g.declared_kinds
		|| g.declared_kinds[flat.source_type_name(key)] !in [.struct_, .union_] {
		return none
	}
	other := fastc_strip_paren_tokens(other_tokens)
	if other.len == 1 && other[0].tok == .name {
		if other_local := g.locals[other[0].lit] {
			if other_local.is_reference {
				return none
			}
		}
	}
	other_type := g.underlying_alias_type(g.infer_expression_type(other_tokens) or { return none })
	return if other_type == storage { local.typ } else { none }
}

fn (g &Parser) render_array_equality_comparison(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, operator token.Token) ?FastcRenderedExpression {
	if operator !in [.eq, .ne] {
		return none
	}
	// `a == []` asks whether `a` is empty.
	right_is_empty := right_tokens.len == 2 && right_tokens[0].tok == .lsbr && right_tokens[1].tok == .rsbr
	left_is_empty := left_tokens.len == 2 && left_tokens[0].tok == .lsbr && left_tokens[1].tok == .rsbr
	if right_is_empty != left_is_empty {
		array_tokens := if right_is_empty { left_tokens } else { right_tokens }
		array_type := fastc_normalize_inferred_type(g.infer_expression_type(array_tokens) or {
			return none
		})
		if fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(array_type))).starts_with('Array_') {
			array_source := g.render_call_argument_expression(array_tokens, array_type) or {
				return none
			}
			access := if fastc_is_pointer_type(array_type) { '->' } else { '.' }
			comparison := if operator == .eq { '==' } else { '!=' }
			return FastcRenderedExpression{
				source: '((${array_source})${access}len ${comparison} 0)'
				typ:    flat.source_type_bool
			}
		}
	}
	left_type := fastc_normalize_inferred_type(g.infer_expression_type(left_tokens) or {
		return none
	})
	right_type := fastc_normalize_inferred_type(g.infer_expression_type(right_tokens) or {
		return none
	})
	left_layout := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(left_type)))
	if left_type != right_type || (!left_layout.starts_with('Array_') && !left_layout.starts_with('FixedArray_')) {
		return none
	}
	element_type := g.array_element_type(left_type) or { return none }
	resolved_element := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(element_type)))
	element_storage_type := g.collection_storage_type(element_type)
	is_scalar := fastc_is_numeric_expression_type(flat.source_type_id(resolved_element)) || resolved_element == 'bool' || fastc_is_pointer_type(element_type) || g.underlying_enum_type_key(flat.source_type_name(g.semantic_type_key(element_type))) != none
	// Elements that are arrays or structs (`[][]u8`) compare as V compares them.
	is_composite := resolved_element != 'string' && !is_scalar && !left_layout.starts_with('FixedArray_')
		&& g.struct_equality_is_supported(element_type, []u32{})
	if resolved_element != 'string' && !is_scalar && !is_composite {
		return none
	}
	if left_layout.starts_with('FixedArray_') {
		length := fastc_fixed_array_length(left_layout) or { return none }
		left_data := g.render_fixed_array_equality_data(left_tokens, left_type) or { return none }
		right_data := g.render_fixed_array_equality_data(right_tokens, right_type) or {
			return none
		}
		element_comparison := if resolved_element == 'string' {
			'builtin__string_eq(__vf_array_eq_left[__vf_array_eq_index], __vf_array_eq_right[__vf_array_eq_index])'
		} else {
			'(__vf_array_eq_left[__vf_array_eq_index] == __vf_array_eq_right[__vf_array_eq_index])'
		}
		result := if operator == .ne { '!__vf_array_equal' } else { '__vf_array_equal' }
		return FastcRenderedExpression{
			source: '({ ${element_storage_type} *__vf_array_eq_left = (${element_storage_type} *)(${left_data}); ${element_storage_type} *__vf_array_eq_right = (${element_storage_type} *)(${right_data}); bool __vf_array_equal = true; for (int __vf_array_eq_index = 0; __vf_array_eq_index < ${length}; __vf_array_eq_index++) { if (!(${element_comparison})) { __vf_array_equal = false; break; } } ${result}; })'
			typ:    flat.source_type_bool
		}
	}
	left := g.render_call_argument_expression(left_tokens, left_type) or { return none }
	right := g.render_call_argument_expression(right_tokens, right_type) or { return none }
	element_comparison := if resolved_element == 'string' {
		'builtin__string_eq(((${flat.source_type_name(element_type)} *)__vf_array_eq_left.data)[__vf_array_eq_index], ((${flat.source_type_name(element_type)} *)__vf_array_eq_right.data)[__vf_array_eq_index])'
	} else if is_composite {
		'({ ${element_storage_type} __vf_element_left = ((${element_storage_type} *)__vf_array_eq_left.data)[__vf_array_eq_index]; ${element_storage_type} __vf_element_right = ((${element_storage_type} *)__vf_array_eq_right.data)[__vf_array_eq_index]; ${g.stored_equality_source('__vf_element_left', '__vf_element_right', element_type, []u32{}, 0)}; })'
	} else {
		'(((${element_storage_type} *)__vf_array_eq_left.data)[__vf_array_eq_index] == ((${element_storage_type} *)__vf_array_eq_right.data)[__vf_array_eq_index])'
	}
	result := if operator == .ne { '!__vf_array_equal' } else { '__vf_array_equal' }
	return FastcRenderedExpression{
		source: '({ ${flat.source_type_name(left_type)} __vf_array_eq_left = (${left}); ${flat.source_type_name(right_type)} __vf_array_eq_right = (${right}); bool __vf_array_equal = __vf_array_eq_left.len == __vf_array_eq_right.len; if (__vf_array_equal) { for (int __vf_array_eq_index = 0; __vf_array_eq_index < __vf_array_eq_left.len; __vf_array_eq_index++) { if (!(${element_comparison})) { __vf_array_equal = false; break; } } } ${result}; })'
		typ:    flat.source_type_bool
	}
}

fn (g &Parser) render_fixed_array_equality_data(tokens []FastcExpressionToken, c_type_source_id u32) ?string {
	c_type := flat.source_type_name(c_type_source_id)
	layout_type := fastc_trim_pointer_suffix(c_type)
	if literal := g.render_array_literal_argument(tokens, flat.source_type_id(layout_type)) {
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
		initializer_type := (g.array_initializer_type(tokens[..initializer_open]) or { u32(0) })
		if flat.source_type_name(initializer_type) == layout_type {
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

fn (g &Parser) narrowed_numeric_operand_type(tokens []FastcExpressionToken, inferred u32) u32 {
	if tokens.len >= 3 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		if close := fastc_matching_rpar(tokens, 0) {
			if close == tokens.len - 1 {
				return g.narrowed_numeric_operand_type(tokens[1..tokens.len - 1], inferred)
			}
		}
	}
	if tokens.len == 1 && tokens[0].tok == .name {
		if local := g.locals[tokens[0].lit] {
			if local.is_reference && local.smartcast_origin_type != 0 {
				local_type := flat.source_type_name(local.typ)
				value_type := fastc_trim_pointer_suffix(local_type)
				value_id := if value_type.len == local_type.len {
					local.typ
				} else {
					flat.source_type_id(value_type)
				}
				if fastc_is_numeric_expression_type(g.underlying_alias_type(value_id)) {
					return value_id
				}
			}
		}
	}
	return inferred
}

// fastc_numeric_arithmetic_has_no_overloads is shared by every file generator.
// Ambiguous primitive metadata disables the shortcut conservatively.
fn fastc_numeric_arithmetic_has_no_overloads(functions map[string]FastcFunctionSignature, declared_type_c_names flat.TypeTextBindings, alias_base_types flat.TypeTextBindings, struct_fields map[string]flat.TypeTextBindings) bool {
	for type_name in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64', 'uint',
		'usize', 'f32', 'f64'] {
		if declared_type_c_names.contains(type_name) || alias_base_types.contains(type_name)
			|| type_name in struct_fields {
			return false
		}
		for operator in ['+', '-', '*', '/', '%'] {
			if '${type_name}.${operator}' in functions {
				return false
			}
		}
	}
	return true
}

// plain_numeric_arithmetic_expression needs no special overloaded lowering.
// Keep mixed-type validation and later raw precedence handling in their usual paths.
fn (g &Parser) plain_numeric_arithmetic_expression(tokens []FastcExpressionToken) bool {
	if !g.numeric_ops_are_builtin || !g.selfhost || g.translated || tokens.len < 3 {
		return false
	}
	mut expect_operand := true
	mut depth := 0
	mut has_binary := false
	for i, item in tokens {
		if item.has_source() || item.is_mut_argument {
			return false
		}
		match item.tok {
			.name {
				if !expect_operand || item.lit == 'C' || item.lit in g.member_smartcasts {
					return false
				}
				// Calls, indexes and member chains cannot be plain numeric atoms.
				if i + 1 < tokens.len && tokens[i + 1].tok !in [.plus, .minus, .mul, .div, .mod,
					.rpar] {
					return false
				}
				local := g.locals[item.lit] or { return false }
				if local.is_reference || local.option_value_type != 0 || local.smartcast_origin_type != 0
					|| (item.typ != 0 && item.typ != local.typ)
					|| !fastc_plain_arithmetic_value_type(local.typ) {
					return false
				}
				expect_operand = false
			}
			.number {
				if !expect_operand || (item.typ != 0 && item.typ !in [
					fastc_type_integer_literal,
					fastc_type_negative_integer_literal,
					fastc_type_float_literal,
				]) {
					return false
				}
				expect_operand = false
			}
			.lpar {
				if !expect_operand {
					return false
				}
				depth++
			}
			.rpar {
				if expect_operand || depth == 0 {
					return false
				}
				depth--
			}
			.plus, .minus {
				if !expect_operand {
					expect_operand = true
					has_binary = true
				}
			}
			.mul, .div, .mod {
				if expect_operand {
					return false
				}
				expect_operand = true
				has_binary = true
			}
			else {
				return false
			}
		}
	}
	return has_binary && !expect_operand && depth == 0
}

// fastc_primitive_comparisons_have_no_overloads is computed once before file
// generation. Primitive metadata or comparison methods keep the full renderer.
fn fastc_primitive_comparisons_have_no_overloads(functions map[string]FastcFunctionSignature, declared_type_c_names flat.TypeTextBindings, alias_base_types flat.TypeTextBindings, struct_fields map[string]flat.TypeTextBindings) bool {
	for type_name in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64',
		'uint', 'usize', 'f32', 'f64'] {
		if declared_type_c_names.contains(type_name) || alias_base_types.contains(type_name)
			|| type_name in struct_fields {
			return false
		}
		for operator in ['==', '<'] {
			if '${type_name}.${operator}' in functions { return false }
		}
	}
	return true
}

// plain_primitive_comparison_expression proves that a complete three-token
// comparison needs no overloaded, boxed, string, enum or mixed-sign lowering.
fn (g &Parser) plain_primitive_comparison_expression(tokens []FastcExpressionToken) bool {
	if tokens.len != 3 || tokens[1].tok !in [.eq, .ne, .lt, .le, .gt, .ge]
		|| tokens[1].has_source() || tokens[1].is_mut_argument || tokens[1].typ != 0
		|| !g.primitive_cmps_builtin || !g.selfhost || g.translated
		|| g.member_smartcasts.len > 0 || g.in_mono_drain || g.in_generic_placeholder {
		return false
	}
	mut left_type := u32(0)
	mut right_type := u32(0)
	for index := 0; index < 3; index += 2 {
		item := tokens[index]
		if item.has_source() || item.is_mut_argument { return false }
		mut typ := u32(0)
		match item.tok {
			.name {
				if item.lit == 'C' || item.lit.starts_with('@')
					|| item.lit in g.generic_method_names {
					return false
				}
				local := g.locals[item.lit] or { return false }
				if local.is_reference || local.option_value_type != 0
					|| local.smartcast_origin_type != 0 || local.smartcast_origin_source != ''
					|| (item.typ != 0 && item.typ != local.typ) {
					return false
				}
				typ = local.typ
				if !fastc_primitive_member_comparison_value_type(typ) {
					return false
				}
			}
			.number {
				typ = fastc_number_expression_type(item.lit)
				if item.typ != 0 && item.typ != typ { return false }
			}
			.key_true, .key_false {
				if item.typ != 0 && item.typ != flat.source_type_bool { return false }
				typ = flat.source_type_bool
			}
			else { return false }
		}
		if index == 0 { left_type = typ } else { right_type = typ }
	}
	if left_type == flat.source_type_bool || right_type == flat.source_type_bool {
		return left_type == right_type && tokens[1].tok in [.eq, .ne]
	}
	if (fastc_is_unsigned_integer_type(left_type) && fastc_is_signed_integer_type(right_type))
		|| (fastc_is_unsigned_integer_type(right_type) && fastc_is_signed_integer_type(left_type)) {
		return false
	}
	return fastc_common_arithmetic_type(left_type, right_type) != 0
}

// plain_primitive_member_comparison_expression leaves only pointer-member
// spelling to lower for a flat primitive comparison with a direct local field.
fn (g &Parser) plain_primitive_member_comparison_expression(tokens []FastcExpressionToken) bool {
	if tokens.len !in [5, 7] || !g.primitive_cmps_builtin || !g.selfhost || g.translated
		|| g.member_smartcasts.len != 0 || g.in_mono_drain || g.in_generic_placeholder {
		return false
	}
	mut operator := 3
	if tokens.len == 5 && tokens[1].tok in [.eq, .ne, .lt, .le, .gt, .ge] {
		operator = 1
	}
	if tokens[operator].tok !in [.eq, .ne, .lt, .le, .gt, .ge]
		|| tokens[operator].typ != 0 {
		return false
	}
	for item in tokens {
		if item.has_source() || item.is_mut_argument || item.is_statement
			|| item.is_capturing_closure || item.is_storage || item.chan_element_type != 0
			|| item.option_value_type != 0 {
			return false
		}
	}
	left_type := g.primitive_member_comparison_operand_type(tokens[..operator])
	right_type := g.primitive_member_comparison_operand_type(tokens[operator + 1..])
	if left_type == 0 || right_type == 0 { return false }
	if left_type == flat.source_type_bool || right_type == flat.source_type_bool {
		return left_type == right_type && tokens[operator].tok in [.eq, .ne]
	}
	if (fastc_is_unsigned_integer_type(left_type) && fastc_is_signed_integer_type(right_type))
		|| (fastc_is_unsigned_integer_type(right_type) && fastc_is_signed_integer_type(left_type)) {
		return false
	}
	return fastc_common_arithmetic_type(left_type, right_type) != 0
}

@[inline]
fn fastc_primitive_member_comparison_value_type(typ u32) bool {
	if fastc_comparison_type_mask.enabled { return fastc_comparison_type_mask.contains(typ) }
	return typ in [flat.source_type_bool, flat.source_type_int, flat.source_type_i8,
		flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
		flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		flat.source_type_uint, flat.source_type_usize, flat.source_type_f32, flat.source_type_f64]
}

fn (g &Parser) primitive_member_comparison_operand_type(tokens []FastcExpressionToken) u32 {
	if tokens.len == 1 {
		item := tokens[0]
		if item.tok == .number {
			typ := fastc_number_expression_type(item.lit)
			return if item.typ == 0 || item.typ == typ { typ } else { u32(0) }
		}
		if item.tok in [.key_true, .key_false] {
			return if item.typ == 0 || item.typ == flat.source_type_bool {
				flat.source_type_bool
			} else {
				u32(0)
			}
		}
		if item.tok != .name || item.lit in ['C', 'chan', 'typeof']
			|| item.lit.starts_with('@') || item.lit in g.generic_method_names {
			return u32(0)
		}
		local := g.locals[item.lit] or { return u32(0) }
		if local.is_reference || local.option_value_type != 0 || local.smartcast_origin_type != 0
			|| local.smartcast_origin_source != '' || (item.typ != 0 && item.typ != local.typ)
			|| !fastc_primitive_member_comparison_value_type(local.typ) {
			return u32(0)
		}
		return local.typ
	}
	if tokens.len != 3 || tokens[0].tok != .name || tokens[1].tok != .dot
		|| tokens[2].tok != .name || tokens[1].typ != 0 || tokens[2].typ != 0 {
		return u32(0)
	}
	root := tokens[0]
	member := tokens[2]
	if root.lit in ['C', 'chan', 'typeof'] || root.lit.starts_with('@')
		|| member.lit.starts_with('@') || root.lit in g.imports
		|| root.lit in g.generic_method_names || member.lit in g.generic_method_names {
		return u32(0)
	}
	local := g.locals[root.lit] or { return u32(0) }
	// Legacy inference recognizes enum type spellings before the local's
	// concrete field metadata, including a local that shadows an enum alias.
	if g.is_enum_type_name(root.lit) { return u32(0) }
	if local.typ == 0 || local.option_value_type != 0 || local.smartcast_origin_type != 0
		|| local.smartcast_origin_source != '' || (root.typ != 0 && root.typ != local.typ) {
		return u32(0)
	}
	type_name := flat.source_type_name(local.typ)
	base_name := fastc_trim_pointer_suffix(type_name)
	if type_name.len - base_name.len > 1 || (local.is_reference && !type_name.ends_with('*'))
		|| base_name in ['Option', '__v_result'] || base_name.starts_with('FixedArray_')
		|| base_name.starts_with('Map_') || base_name in g.sum_types
		|| g.alias_base_types.type_id(base_name) != none {
		return u32(0)
	}
	if base_name.starts_with('Array_') {
		if member.lit !in ['len', 'cap'] { return u32(0) }
	} else {
		kind := g.declared_kinds[flat.source_type_name(g.semantic_type_key(local.typ))] or {
			return u32(0)
		}
		if kind != .struct_ { return u32(0) }
	}
	field := g.direct_struct_field_metadata(local.typ, member.lit) or { return u32(0) }
	if field.name != member.lit || !fastc_primitive_member_comparison_value_type(field.typ)
		|| field.storage_path.len != 0 || field.is_shared_pointer || field.is_function
		|| field.is_optional_function || field.option_value_type != 0
		|| field.generic_argument_type != 0 || field.chan_element_type != 0
		|| g.struct_direct_member_type(local.typ, member.lit) != field.typ {
		return u32(0)
	}
	return field.typ
}

fn (g &Parser) render_overloaded_binary_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 2 { return none }
	if g.plain_primitive_comparison_expression(tokens) || g.plain_numeric_arithmetic_expression(tokens) {
		return none
	}
	if g.selfhost {
		if callback_call := g.render_element_function_call(tokens) {
			return callback_call
		}
	}
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
	// Assignment has lower precedence than every binary operator. Its RHS is
	// rendered recursively by the assignment lowering, so do not split the full
	// expression at an overloaded operator that appears after `=`.
	if fastc_has_top_level_assignment(tokens) {
		return none
	}
	if boolean_index := fastc_lowest_precedence_operator_index(tokens, 0, tokens.len) {
		left_tokens := tokens[..boolean_index]
		right_tokens := tokens[boolean_index + 1..]
		operator := tokens[boolean_index].tok
		if operator in [.key_in, .not_in] {
			// Membership is not a C binary operator; splitting here would emit a raw `(x) in (y)`.
			// Bail so the dedicated membership handler lowers it (its subject renders any concat).
			return none
		}
		if operator in [.key_is, .not_is] {
			return g.render_boolean_is_expression(tokens)
		}
		if comparison := g.render_overloaded_comparison_expression(left_tokens, right_tokens, operator) {
			return comparison
		}
		if operator in [.eq, .ne] {
			if option_comparison := g.render_option_none_comparison(tokens) {
				return option_comparison
			}
			if struct_comparison := g.render_struct_comparison_expression(tokens) {
				return struct_comparison
			}
			if string_comparison := g.render_string_comparison_expression(tokens) {
				return string_comparison
			}
			if enum_comparison := g.render_enum_comparison_expression(tokens) {
				return enum_comparison
			}
		}
		// V's bitwise operators bind tighter than comparisons, while C's `&`, `|`,
		// and `^` bind looser. Preserve the V parse explicitly; otherwise
		// `x & 1 == 0` becomes `x & (1 == 0)` in C.
		if operator in [.eq, .ne, .lt, .le, .gt, .ge] && (fastc_has_top_level_bitwise_operator(left_tokens) || fastc_has_top_level_bitwise_operator(right_tokens)) {
			left_type := g.infer_expression_type(left_tokens) or { return none }
			right_type := g.infer_expression_type(right_tokens) or { return none }
			left := g.render_call_argument_expression(left_tokens, left_type) or { return none }
			right := g.render_call_argument_expression(right_tokens, right_type) or { return none }
			// A signed and an unsigned operand compare by value here too (`x & 1 == u64(1)`).
			left_layout := g.underlying_alias_type(left_type)
			if fastc_is_mixed_sign_comparison(left_layout, g.underlying_alias_type(right_type)) {
				if mixed := fastc_mixed_sign_comparison(operator, left, left_layout, right) {
					return FastcRenderedExpression{
						source: mixed
						typ:    flat.source_type_bool
					}
				}
			}
			return FastcRenderedExpression{
				source: '((${left})${operator.str()}(${right}))'
				typ:    flat.source_type_bool
			}
		}
		// A signed and an unsigned operand (`i64(-1) < u64(1)`) must not meet in a plain C
		// comparison, which converts the signed one to unsigned. An operand that renders
		// specially (a cast, a call) would otherwise reach the plain composition below.
		if operator in [.eq, .ne, .lt, .le, .gt, .ge] {
			if mixed := g.render_mixed_integer_comparison_expression(tokens) {
				return mixed
			}
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
			} else if left_special.typ != u32(0) {
				left_special.typ_text()
			} else {
				flat.source_type_name(g.infer_expression_type(left_tokens) or { u32(0) })
			}
			right_expected := if operator in [.and, .logical_or] {
				'bool'
			} else if right_special.typ != u32(0) {
				right_special.typ_text()
			} else {
				flat.source_type_name(g.infer_expression_type(right_tokens) or { u32(0) })
			}
			left := if left_special.source != '' {
				left_special.source
			} else {
				g.render_call_argument_expression(left_tokens, flat.source_type_id(left_expected)) or { return none }
			}
			right := if right_special.source != '' {
				right_special.source
			} else {
				g.render_call_argument_expression(right_tokens, flat.source_type_id(right_expected)) or { return none }
			}
			return FastcRenderedExpression{
				source: '((${left})${operator.str()}(${right}))'
				typ:    flat.source_type_bool
			}
		}
		return none
	}
	mut depth := 0
	mut operator_index := -1
	mut precedence := 100
	mut binary_operators := 0
	mut has_c_regrouped_operator := false
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && i > 0 && i + 1 < tokens.len {
			if item.tok in [.plus, .minus, .mul, .amp, .and]
				&& fastc_token_is_prefix_operator(tokens, i) {
				continue
			}
			candidate_precedence := fastc_overloaded_binary_precedence(item.tok)
			if candidate_precedence > 0 {
				binary_operators++
				if item.tok in [.amp, .pipe, .xor, .left_shift, .right_shift, .right_shift_unsigned] {
					has_c_regrouped_operator = true
				}
			}
			if candidate_precedence > 0 && candidate_precedence <= precedence {
				operator_index = i
				precedence = candidate_precedence
			}
		}
	}
	if operator_index < 1 {
		if tokens.len > 2 && tokens[0].tok == .minus
			&& fastc_expression_tokens_contain(tokens[1..], .not) {
			// `-i64(f()!)`, `-(f()!)`: render the operand in full, so its `!` propagation
			// is lowered, then negate it. The cast renderer below would copy it raw.
			operand_type := g.infer_expression_type(tokens[1..]) or { return none }
			operand := g.render_call_argument_expression(tokens[1..], operand_type) or {
				return none
			}
			return FastcRenderedExpression{
				source: '(-(${operand}))'
				typ:    operand_type
			}
		}
		if tokens.len > 1 && tokens[0].tok == .mul {
			inner_type := g.infer_expression_type(tokens[1..]) or { return none }
			if flat.source_type_name(inner_type).ends_with('*') {
				inner := g.render_call_argument_expression(tokens[1..], inner_type) or { return none }
				return FastcRenderedExpression{
					source: '*(${inner})'
					typ:    flat.source_type_id(flat.source_type_name(inner_type)[..flat.source_type_name(inner_type).len - 1])
				}
			}
		}
		if cast := g.render_cast_expression(tokens) {
			return cast
		}
		if generic_call := g.render_explicit_generic_call_expression(tokens) {
			return generic_call
		}
		if call := g.render_missing_call_arguments(tokens) {
			return call
		}
		if member := g.render_call_result_member_expression(tokens) {
			return member
		}
		return none
	}
	// C binds `+ - * / %` tighter than `<< >> &` and `& ^ |` looser than V does, so an
	// expression mixing them must be grouped explicitly (`a & b + c` is `(a & b) + c`).
	v_grouping := has_c_regrouped_operator && binary_operators > 1
	left_tokens := tokens[..operator_index]
	right_tokens := tokens[operator_index + 1..]
	checked_division := g.checked_target_division_enabled() && tokens[operator_index].tok in [
		.div,
		.mod,
	]
	left_type := g.infer_expression_type(left_tokens) or {
		if checked_division {
			u32(0)
		} else {
			return none
		}
	}
	operator := tokens[operator_index].tok.str()
	method_key := g.method_function_key(left_type, operator)
	if tokens[operator_index].tok == .left_shift && fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(left_type))).starts_with('Array_') {
		// `arr << x` is an append (render_append_expression), not a C left-shift;
		// splitting here would emit an invalid `(arr) << (x)`.
		return none
	}
	if method_key !in g.functions {
		right_type := g.infer_expression_type(right_tokens) or {
			if checked_division {
				u32(0)
			} else {
				return none
			}
		}
		left_value_type := if checked_division {
			g.checked_integer_operand_type(left_tokens, left_type)
		} else {
			g.narrowed_numeric_operand_type(left_tokens, left_type)
		}
		right_value_type := if checked_division {
			g.checked_integer_operand_type(right_tokens, right_type)
		} else {
			g.narrowed_numeric_operand_type(right_tokens, right_type)
		}
		// Mutable scalar smartcasts point at boxed storage. Read each value before
		// arithmetic, rather than dereferencing the result of pointer arithmetic.
		reference_arithmetic := flat.source_type_name(g.narrowed_numeric_operand_type(left_tokens, u32(0))) != '' || g.narrowed_numeric_operand_type(right_tokens, u32(0)) != u32(0)
		mut left_special := FastcRenderedExpression{}
		if rendered := g.render_overloaded_binary_expression(left_tokens) {
			left_special = rendered
		}
		mut right_special := FastcRenderedExpression{}
		if rendered := g.render_overloaded_binary_expression(right_tokens) {
			right_special = rendered
		}
		guarded_shift := g.translated && tokens[operator_index].tok in [.left_shift, .right_shift,
			.right_shift_unsigned]
		// An operand that propagates a result (`a.m()! + 1`) is lowered on its own.
		operand_propagates := (left_tokens.len > 1 && left_tokens.last().tok in [.not, .question]
			&& !fastc_trailing_not_marks_fixed_array_literal(left_tokens))
			|| (right_tokens.len > 1 && right_tokens.last().tok in [.not, .question]
				&& !fastc_trailing_not_marks_fixed_array_literal(right_tokens))
		if left_special.source == '' && right_special.source == '' && !guarded_shift && !checked_division && !reference_arithmetic && !v_grouping && !operand_propagates {
			return none
		}
		left := if left_special.source != '' {
			left_special.source
		} else {
			g.render_call_argument_expression(left_tokens, left_value_type) or { return none }
		}
		right := if right_special.source != '' {
			right_special.source
		} else {
			g.render_call_argument_expression(right_tokens, right_value_type) or { return none }
		}
		if checked_division {
			result_type := if fastc_is_integer_literal_expression_type(left_value_type) {
				right_value_type
			} else {
				left_value_type
			}
			return FastcRenderedExpression{
				source: g.render_checked_integer_division_expression(left, right, result_type, tokens[operator_index].tok)
				typ:    fastc_normalize_inferred_type(result_type)
			}
		}
		if guarded_shift {
			return FastcRenderedExpression{
				source: g.render_guarded_shift_expression(left, right, left_value_type, tokens[operator_index].tok) or { return none }
				typ:    fastc_normalize_inferred_type(left_value_type)
			}
		}
		// A `+` whose operands are strings is concatenation, not C pointer arithmetic. When
		// one side is itself a lowered string expression (`a + s[..n] + b`), the recursive
		// step above already rendered it, so the joining operator must still be string_plus.
		if operator == '+' && (fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(left_type))) == 'string' || fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(right_type))) == 'string') {
			return FastcRenderedExpression{
				source: 'builtin__string_plus((${left}),(${right}))'
				typ:    flat.source_type_string
			}
		}
		return FastcRenderedExpression{
			source: '((${left})${operator}(${right}))'
			typ:    (g.infer_expression_type(tokens) or { left_type })
		}
	}
	signature := g.functions[method_key]
	if signature.parameter_types.ids.len < 2 || signature.is_disabled {
		return none
	}
	left := g.render_call_argument_expression(left_tokens, signature.parameter_types.ids[0]) or {
		return none
	}
	right := g.render_call_argument_expression(right_tokens, signature.parameter_types.ids[1]) or {
		return none
	}
	method_c_name := g.c_method_name(method_key, signature.module_name, signature.parameter_types.at(0), operator)
	return FastcRenderedExpression{
		source: '${method_c_name}(${left},${right})'
		typ:    signature.return_type
	}
}

// fastc_contains_method_marker reports whether `rendered` contains a call of
// `name` on a receiver, i.e. `.name(` or `->name(`, without building the
// marker strings.
@[direct_array_access]
fn fastc_contains_method_marker(rendered string, name string) bool {
	if name.len == 0 {
		return false
	}
	mut from := 0
	for from + name.len < rendered.len {
		index := rendered.index_after_(name, from)
		if index < 0 {
			return false
		}
		// An occurrence at the start has no receiver before it; keep scanning
		// past it (and past any other non-call occurrence).
		end := index + name.len
		if index > 0 && end < rendered.len && rendered[end] == `(` {
			previous := rendered[index - 1]
			if previous == `.` || (previous == `>` && index > 1 && rendered[index - 2] == `-`) {
				return true
			}
		}
		from = index + 1
	}
	return false
}

// fastc_replace_member_chain replaces every occurrence of `needle` in `source` that is a whole
// token (not preceded or followed by an identifier character), so replacing a member chain such
// as `node.expr` does not corrupt a sibling field like `node.expr_type` that merely shares its
// prefix. A needle ending in `.` deliberately replaces a member-chain prefix.
fn fastc_replace_member_chain(source string, needle string, replacement string) string {
	if needle == '' {
		return source
	}
	mut result := ''
	mut i := 0
	for i < source.len {
		if i + needle.len <= source.len && source[i..i + needle.len] == needle {
			before_ok := i == 0 || !fastc_is_chain_identifier_char(source[i - 1])
			after_ok := needle.ends_with('.') || i + needle.len == source.len
				|| !fastc_is_chain_identifier_char(source[i + needle.len])
			if before_ok && after_ok {
				result += replacement
				i += needle.len
				continue
			}
		}
		result += source[i].ascii_str()
		i++
	}
	return result
}

fn fastc_is_chain_identifier_char(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
}

fn (g &Parser) render_pointer_member_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok != .dot || tokens[i + 1].tok != .name || i + 2 >= tokens.len || tokens[i + 2].tok != .lpar {
			continue
		}
		// A qualified function call can contain an embedded-field argument that
		// still needs promotion here. A real method call is rendered later with
		// its receiver and arguments, so preserve the old early exit for it.
		is_qualified_call := tokens[i - 1].tok == .name && (tokens[i - 1].lit in g.imports || tokens[i - 1].lit == 'C') && (i < 2 || tokens[i - 2].tok != .dot)
		if !is_qualified_call && fastc_contains_method_marker(rendered_expression, tokens[i + 1].lit) {
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
		root_type := g.infer_expression_type(tokens[start..start + 1]) or { continue }
		root_is_reference := if local := g.locals[item.lit] { local.is_reference } else { false }
		root_is_pointer := fastc_is_pointer_type(g.underlying_alias_type(root_type))
			|| root_is_reference
		chain_tokens := tokens[start..end]
		raw_chain := g.render_raw_expression_tokens(chain_tokens) or { continue }
		promoted_chain := g.render_member_receiver(chain_tokens) or { continue }
		// A value-rooted chain needs no `.`→`->` deref rewrite (the second loop below handles
		// pointer fields), but it still needs embedded-field promotion (`err.pos` →
		// `err.__embedded_0.pos`) when a field is reached through an embed, OR a correction when
		// the raw render spuriously deref'd a FIELD that shadows a pointer local (`node.left.len`
		// inside `for mut left in node.left` → `node.left->len`); in that case the raw and
		// member-receiver renders disagree, so proceed to replace the raw with the correct chain.
		if !root_is_pointer && !fastc_contains(promoted_chain, '__embedded_')
			&& raw_chain == promoted_chain {
			continue
		}
		mut needle := raw_chain
		if !fastc_contains(rendered, needle) {
			root_source := g.resolved_expression_name(item.lit, .unknown)
			pointer_chain := raw_chain.replace_once('${root_source}.', '${root_source}->')
			if fastc_contains(rendered, pointer_chain) {
				needle = pointer_chain
			}
		}
		if promoted_chain != needle && fastc_contains(rendered, needle)
			&& !(fastc_contains(promoted_chain, needle) && fastc_contains(rendered, promoted_chain)) {
			// Replace only a complete root chain: `node.expr` must not match a nested field
			// or the prefix of a sibling like `node.expr_type`. Skip when the promoted form
			// (which itself contains the raw chain, e.g. a smart-cast `((T*)(x.f)._object)`) is
			// already present — a method-call receiver render applied it, so replacing the raw
			// chain surviving inside it would double-wrap.
			replaced := fastc_replace_c_root_identifier(rendered, needle, promoted_chain)
			if replaced != rendered {
				rendered = replaced
				changed = true
			}
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
		mut receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		mut lowered_array_receiver := ''
		if receiver_tokens.len > 0 {
			if array_receiver := g.render_indexed_member_receiver(receiver_tokens) {
				lowered_array_receiver = array_receiver.source
				// General member inference can erase the reference marker from an
				// indexed `[]&T` element. The array lowering retains its exact type.
				if array_receiver.typ_text().ends_with('*') {
					receiver_type = array_receiver.typ
				}
			}
		}
		if g.declared_kinds[flat.source_type_name(g.semantic_type_key(receiver_type))] == .interface_ {
			if field := g.interface_fields['${flat.source_type_name(g.semantic_type_key(receiver_type))}.${tokens[i + 1].lit}'] {
				receiver_source := g.render_member_receiver(receiver_tokens) or {
					g.render_call_argument_expression(receiver_tokens, receiver_type) or { continue }
				}
				field_source := g.fastc_interface_field_switch_source(receiver_type, receiver_source,
					field.name, field.typ) or { continue }
				raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { receiver_source }
				for base in [receiver_source, raw_receiver] {
					for separator in ['.', '->'] {
						needle := '${base}${separator}${tokens[i + 1].lit}'
						replaced := fastc_replace_c_root_identifier(rendered, needle, field_source)
						if replaced != rendered {
							rendered = replaced
							changed = true
						}
					}
				}
				continue
			}
		}
		if tokens[i + 1].lit in ['len', 'cap'] {
			if fixed_length := fastc_fixed_array_length(flat.source_type_name(receiver_type).trim_right('*')) {
				raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { continue }
				receiver_source := g.render_member_receiver(receiver_tokens) or { raw_receiver }
				for base in [receiver_source, raw_receiver] {
					for separator in ['.', '->'] {
						needle := '${base}${separator}${tokens[i + 1].lit}'
						replaced := fastc_replace_c_root_identifier(rendered, needle, fixed_length)
						if replaced != rendered {
							rendered = replaced
							changed = true
						}
					}
				}
				continue
			}
		}
		if !flat.source_type_name(g.underlying_alias_type(receiver_type)).ends_with('*') {
			continue
		}
		receiver_source := if lowered_array_receiver != '' {
			lowered_array_receiver
		} else {
			g.render_member_receiver(receiver_tokens) or {
				g.render_membership_candidate(receiver_tokens, flat.source_type_id('')) or { continue }
			}
		}
		field_name := fastc_c_identifier(tokens[i + 1].lit)
		needle := '${receiver_source}.${field_name}'
		// A local receiver (`nodes.len`) must not match a same-named field suffix
		// (`store->a->nodes.len`), whose value/reference type comes from its owner.
		replaced := fastc_replace_c_root_identifier(rendered, needle, '${receiver_source}->${field_name}')
		if replaced != rendered {
			rendered = replaced
			changed = true
			continue
		}
		raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
		raw_needle := '${raw_receiver}.${field_name}'
		// Use the smartcast receiver (`member->…`) as the replacement base, not the
		// raw member spelling (`x.f->…`, which derefs a non-pointer).
		raw_replaced := fastc_replace_c_root_identifier(rendered, raw_needle, '${receiver_source}->${field_name}')
		if raw_receiver != '' && raw_replaced != rendered {
			rendered = raw_replaced
			changed = true
			continue
		}
		parenthesized_needle := ').${tokens[i + 1].lit}'
		if receiver_tokens.last().tok == .rpar && fastc_contains(rendered, parenthesized_needle) {
			rendered = fastc_replace(rendered, parenthesized_needle, ')->${tokens[i + 1].lit}')
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
	inferred_type := (g.infer_expression_type(tokens) or { u32(0) })
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
		if close <= open + 1
			|| fastc_expression_tokens_contain_range(tokens, open + 1, close, .dotdot) {
			continue
		}
		start := fastc_method_receiver_start(tokens, open)
		if start >= open {
			continue
		}
		base_tokens := tokens[start..open]
		base_type := g.infer_expression_type(base_tokens) or { continue }
		if g.map_type_of(base_type) != none {
			lookup_tokens := tokens[start..close + 1]
			lookup := g.render_map_expression(lookup_tokens) or { continue }
			raw_lookup := g.render_raw_expression_tokens(lookup_tokens) or { continue }
			replaced := fastc_replace(rendered, raw_lookup, lookup.source)
			if replaced.str != rendered.str {
				rendered = replaced
				changed = true
				continue
			}
			// The raw render leaves the key's method/function calls un-lowered, so it may not
			// match the buffer (`m[f(x)]` streams as `m[F(x)]`); rebuild the needle with the key
			// rendered the way the buffer holds it.
			// render_member_receiver spells the base the way the buffer holds it (a pointer field
			// derefs with `->`, which render_raw misses); pair it with the lowered key.
			base_src := if member := g.render_member_receiver(base_tokens) {
				member
			} else {
				g.render_raw_expression_tokens(base_tokens) or { continue }
			}
			key_source := g.render_membership_candidate(tokens[open + 1..close], flat.source_type_id('')) or { continue }
			for lowered_needle in [
				'${base_src}[${key_source}]',
				'(${base_src})[${key_source}]',
				'(*(${base_src}))[${key_source}]',
			] {
				lowered_replaced := fastc_replace(rendered, lowered_needle, lookup.source)
				if lowered_replaced.str != rendered.str {
					rendered = lowered_replaced
					changed = true
					break
				}
			}
			continue
		}
		is_array_pointer := flat.source_type_name(base_type).ends_with('*') && g.array_element_type(base_type) != none
		element_type := if base_type == flat.source_type_string {
			flat.source_type_u8
		} else if is_array_pointer {
			g.array_element_type(base_type) or { continue }
		} else if flat.source_type_name(base_type).ends_with('*') {
			flat.source_type_id(fastc_trim_pointer_suffix(flat.source_type_name(base_type)))
		} else {
			g.array_element_type(base_type) or { continue }
		}
		raw_base := g.render_raw_expression_tokens(base_tokens) or { continue }
		base_is_global_or_constant := base_tokens.len == 1 && (fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals || fastc_constant_key(g.module_name, base_tokens[0].lit) in g.constants || base_tokens[0].lit in g.constants)
		base_source := if base_is_global_or_constant {
			raw_base
		} else {
			g.render_member_receiver(base_tokens) or { raw_base }
		}
		index_source := g.render_membership_candidate(tokens[open + 1..close], flat.source_type_id('int')) or {
			continue
		}
		is_raw_fixed_array := fastc_trim_pointer_suffix(flat.source_type_name(base_type)).starts_with('FixedArray_') && (base_tokens.len > 1 || (base_tokens.len == 1 && fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals))
		replacement := if checked_access := g.render_array_access_expression(tokens[start..close + 1]) {
			checked_access.source
		} else if is_raw_fixed_array {
			'((${base_source})[${index_source}])'
		} else if flat.source_type_name(base_type).ends_with('*') && !is_array_pointer {
			'((${base_source})[${index_source}])'
		} else {
			array_value := if flat.source_type_name(base_type).ends_with('*') {
				'*(${base_source})'
			} else {
				base_source
			}
			'(*(${g.collection_storage_type(element_type)} *)builtin__array_get(${array_value}, ${index_source}))'
		}
		// The buffer holds the render_raw spelling of the base, so match that first; using the
		// member-receiver `base_source` as the primary needle lets a shorter un-mangled name
		// (`short[0]`) match INSIDE the mangled buffer text (`__vf_keyword_short[0]`) and
		// splice the replacement after the keyword prefix.
		mut needle := '${raw_base}[${index_source}]'
		if !fastc_contains(rendered, needle) {
			needle = '${base_source}[${index_source}]'
		}
		if !fastc_contains(rendered, needle) {
			// The streamed index can retain a source-level cast spelling (`int(i)`) while
			// render_membership_candidate has already normalized it to the platform C type.
			// Try that exact streamed spelling when locating the access to replace. If
			// the streamed index contains another array access (`int(ids[i])`), lower that
			// inner access while retaining the outer cast's streamed C spelling.
			if raw_index := g.render_raw_expression_tokens(tokens[open + 1..close]) {
				mut streamed_indexes := [raw_index]
				// The streaming reader can already have lowered a method nested in the
				// index (`nodes[int(a.child(i))]`). Match that intermediate spelling too.
				if method_index := g.render_method_call_expression(tokens[open + 1..close], raw_index) {
					streamed_indexes << method_index.source
				}
				if nested_index := g.render_nested_array_access_expression(tokens[open + 1..close], raw_index) {
					streamed_indexes << nested_index.source
				}
				for streamed_index in streamed_indexes {
					raw_needle := '${raw_base}[${streamed_index}]'
					member_needle := '${base_source}[${streamed_index}]'
					if fastc_contains(rendered, raw_needle) {
						needle = raw_needle
						break
					} else if fastc_contains(rendered, member_needle) {
						needle = member_needle
						break
					}
				}
			}
		}
		// Lower a trailing promoted field together with the indexed receiver. Replacing
		// only `items[i]` would leave the source field name on the physical embed wrapper.
		mut member_end := close + 1
		for member_end + 1 < tokens.len && tokens[member_end].tok == .dot
			&& tokens[member_end + 1].tok == .name {
			if member_end + 2 < tokens.len && tokens[member_end + 2].tok == .lpar { break }
			member_end += 2
		}
		if member_end > close + 1 {
			if member := g.render_indexed_member_receiver(tokens[start..member_end]) {
				mut member_needle := needle
				for field_index := close + 1; field_index < member_end; field_index += 2 {
					member_needle += '.' + fastc_c_identifier(tokens[field_index + 1].lit)
				}
				replaced := fastc_replace_c_root_identifier(rendered, member_needle, member.source)
				if replaced != rendered {
					rendered = replaced
					changed = true
					continue
				}
			}
		}
		// An element that is itself a pointer (`[]&TypeSymbol`) auto-dereferences on the
		// following member access, so `arr[i].field` lowers to `(elem)->field`, not `.field`.
		if flat.source_type_name(element_type).ends_with('*') && fastc_contains(rendered, '${needle}.') {
			rendered = fastc_replace_member_chain(rendered, '${needle}.', '${replacement}->')
			changed = true
		} else if fastc_contains(rendered, needle) {
			// For a root name, do not match the same text inside an already-lowered
			// string member access: lowering `str[i]` inside `((str).str[i])` again
			// produces the invalid `((str).((str).str[i]))`.
			replaced := if base_tokens.len == 1 && base_tokens[0].tok == .name {
				fastc_replace_c_root_identifier(rendered, needle, replacement)
			} else {
				fastc_replace_member_chain(rendered, needle, replacement)
			}
			if replaced != rendered {
				rendered = replaced
				changed = true
			}
		}
	}
	inferred_type := (g.infer_expression_type(tokens) or { u32(0) })
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    inferred_type
		}
	} else {
		none
	}
}

fn fastc_trim_expression_parentheses_bounds(tokens []FastcExpressionToken) (int, int) {
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar && tokens[end - 1].tok == .rpar {
		mut depth := 0
		mut close := -1
		for i := start; i < end; i++ {
			if tokens[i].tok == .lpar {
				depth++
			} else if tokens[i].tok == .rpar {
				depth--
				if depth == 0 {
					close = i
					break
				}
			}
		}
		if close != end - 1 {
			break
		}
		start++
		end--
	}
	return start, end
}

// render_refined_enum_logical_expression lowers `x is Enum && x == .value`.
// The right side is safe to unbox because C's `&&` preserves V's short-circuit
// refinement semantics.
fn (g &Parser) render_refined_enum_logical_expression(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken) ?FastcRenderedExpression {
	left_start, left_end := fastc_trim_expression_parentheses_bounds(left_tokens)
	if left_end - left_start < 3 || left_tokens[left_start].tok != .name || left_tokens[left_start + 1].tok != .key_is {
		return none
	}
	local_name := left_tokens[left_start].lit
	local := g.locals[local_name] or { return none }
	boxed_type := fastc_normalize_inferred_type(local.typ)
	if !g.is_boxed_type(boxed_type) {
		return none
	}
	target_type := (g.type_from_expression_tokens(left_tokens[left_start + 2..left_end]) or {
		return none
	})
	enum_type := flat.source_type_name(fastc_normalize_inferred_type(target_type)).trim_right('*')
	if g.declared_kinds[flat.source_type_name(g.semantic_type_key(flat.source_type_id(enum_type)))] != .enum_ {
		return none
	}
	right_start, right_end := fastc_trim_expression_parentheses_bounds(right_tokens)
	mut comparison_index := -1
	for i := right_start; i < right_end; i++ {
		item := right_tokens[i]
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
	comparison := right_tokens[comparison_index].tok.str()
	comparison_left_len := comparison_index - right_start
	comparison_right_len := right_end - comparison_index - 1
	mut enum_value := ''
	mut enum_first := false
	if comparison_left_len == 1 && right_tokens[right_start].tok == .name && right_tokens[right_start].lit == local_name && comparison_right_len == 2 && right_tokens[comparison_index + 1].tok == .dot && right_tokens[comparison_index + 2].tok == .name {
		enum_value = right_tokens[comparison_index + 2].lit
	} else if comparison_right_len == 1 && right_tokens[comparison_index + 1].tok == .name && right_tokens[comparison_index + 1].lit == local_name && comparison_left_len == 2 && right_tokens[right_start].tok == .dot && right_tokens[right_start + 1].tok == .name {
		enum_value = right_tokens[right_start + 1].lit
		enum_first = true
	} else {
		return none
	}
	subject := fastc_c_identifier(local_name)
	access := if flat.source_type_name(boxed_type).ends_with('*') { '->' } else { '.' }
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
		typ:    flat.source_type_bool
	}
}

fn (g &Parser) render_logical_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	// Split the looser `||` first, so method/static-call rewriting preserves
	// the precedence and short-circuit behavior of mixed boolean expressions.
	index := fastc_top_level_boolean_split(tokens, .logical_or) or {
		fastc_top_level_boolean_split(tokens, .and) or { return none }
	}
	operator := tokens[index].tok
	if operator == .and {
		if refined := g.render_refined_enum_logical_expression(tokens[..index], tokens[index + 1..]) {
			return refined
		}
	}
	left := g.render_call_argument_expression(tokens[..index], flat.source_type_id('bool')) or { return none }
	right := g.render_call_argument_expression(tokens[index + 1..], flat.source_type_id('bool')) or { return none }
	return FastcRenderedExpression{
		source: '((${left})${if operator == .and { '&&' } else { '||' }}(${right}))'
		typ:    flat.source_type_bool
	}
}

fn (g &Parser) equality_operator_key(typ u32) ?string {
	method_key := g.method_function_key(typ, '==')
	if signature := g.functions[method_key] {
		if !signature.is_disabled && signature.parameter_types.ids.len >= 2 {
			return method_key
		}
	}
	return none
}

// Array elements and nested struct fields honor the same equality overload as direct values.
fn (g &Parser) overloaded_stored_equality_source(left string, right string, typ u32, depth int) ?string {
	method_key := g.equality_operator_key(typ) or { return none }
	signature := g.functions[method_key]
	method_name := g.c_function_name_or(method_key, fastc_method_c_name(signature.module_name,
		signature.parameter_types.at(0), '=='))
	left_value := '__vf_overloaded_eq_left_${depth}'
	right_value := '__vf_overloaded_eq_right_${depth}'
	left_argument := if fastc_is_pointer_type(signature.parameter_types.ids[0]) {
		'&${left_value}'
	} else {
		left_value
	}
	right_argument := if fastc_is_pointer_type(signature.parameter_types.ids[1]) {
		'&${right_value}'
	} else {
		right_value
	}
	value_type := flat.source_type_name(typ)
	return '({ ${value_type} ${left_value} = (${left}); ${value_type} ${right_value} = (${right}); ${method_name}(${left_argument}, ${right_argument}); })'
}

fn (g &Parser) struct_equality_is_supported(typ u32, seen []u32) bool {
	if fastc_is_pointer_type(g.underlying_alias_type(typ)) {
		return true
	}
	layout_type := flat.source_type_id(fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(typ))))
	if _ := g.equality_operator_key(layout_type) {
		return true
	}
	if layout_type == flat.source_type_string || layout_type == flat.source_type_bool || fastc_is_numeric_expression_type(layout_type) {
		return true
	}
	if element_type_source_id := fastc_fixed_array_element_type(layout_type) {
		element_type := element_type_source_id

		length_source := fastc_fixed_array_length(flat.source_type_name(layout_type)) or { return false }
		_ = g.fixed_array_length_value(length_source) or { return false }
		return g.struct_equality_is_supported(element_type, seen)
	}
	if element_type_source_id := g.array_element_type(layout_type) {
		element_type := element_type_source_id

		return g.struct_equality_is_supported(element_type, seen)
	}
	if layout_type in [fastc_type_option, flat.source_type_id('__v_result'), fastc_type_array,
		fastc_type_map] || flat.source_type_name(layout_type).starts_with('Map_') {
		return false
	}
	type_key := g.semantic_type_key(layout_type)
	if flat.source_type_name(type_key) in g.declared_kinds && g.declared_kinds[flat.source_type_name(type_key)] == .enum_ {
		return true
	}
	if flat.source_type_name(type_key) !in g.declared_kinds || g.declared_kinds[flat.source_type_name(type_key)] != .struct_ || flat.source_type_name(layout_type) !in g.struct_field_info {
		return false
	}
	if layout_type in seen {
		return true
	}
	mut nested_seen := seen.clone()
	nested_seen << layout_type
	for field in g.struct_field_info[flat.source_type_name(layout_type)] {
		if !g.struct_equality_is_supported(field.typ, nested_seen) {
			return false
		}
	}
	return true
}

// stored_equality_source compares two values held as an array holds its elements: a fixed
// array there is a `FixedArray_*` wrapper, its own elements wrappers too, where a struct field
// is a raw C array. `depth` is the struct_equality_source array nesting it is inside of.
fn (g &Parser) stored_equality_source(left string, right string, typ u32, seen []u32, depth int) string {
	if !fastc_is_pointer_type(typ) {
		layout_type := g.underlying_alias_type(typ)
		if element_type_source_id := fastc_fixed_array_element_type(layout_type) {
			element_type := element_type_source_id

			length_source := fastc_fixed_array_length(flat.source_type_name(layout_type)) or { return 'false' }
			length := g.fixed_array_length_value(length_source) or { return 'false' }
			mut comparisons := []string{cap: length}
			for index in 0 .. length {
				comparisons << g.stored_equality_source('(${left}).data[${index}]', '(${right}).data[${index}]',
					element_type, seen, depth)
			}
			return if comparisons.len == 0 { 'true' } else { '(${comparisons.join(' && ')})' }
		}
	}
	return g.struct_equality_source(left, right, typ, seen, depth)
}

// struct_equality_source compares two values of type `typ` as V compares them. An array is
// compared in a loop whose temporaries are named by `depth`, so the loop comparing its
// elements, nested in it, does not shadow them.
fn (g &Parser) struct_equality_source(left string, right string, typ u32, seen []u32, depth int) string {
	if fastc_is_pointer_type(g.underlying_alias_type(typ)) {
		return '((${left}) == (${right}))'
	}
	layout_type := flat.source_type_id(fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(typ))))
	if layout_type == flat.source_type_string {
		return 'builtin__string_eq(${left}, ${right})'
	}
	if layout_type == flat.source_type_bool || fastc_is_numeric_expression_type(layout_type) {
		return '((${left}) == (${right}))'
	}
	if overloaded := g.overloaded_stored_equality_source(left, right, layout_type, depth) {
		return overloaded
	}
	if element_type_source_id := fastc_fixed_array_element_type(layout_type) {
		element_type := element_type_source_id

		length_source := fastc_fixed_array_length(flat.source_type_name(layout_type)) or { return 'false' }
		length := g.fixed_array_length_value(length_source) or { return 'false' }
		mut comparisons := []string{cap: length}
		for index in 0 .. length {
			comparisons << g.struct_equality_source('(${left})[${index}]', '(${right})[${index}]', element_type, seen, depth)
		}
		return if comparisons.len == 0 { 'true' } else { '(${comparisons.join(' && ')})' }
	}
	if element_type_source_id := g.array_element_type(layout_type) {
		element_type := element_type_source_id

		left_array := '__vf_array_eq_left_${depth}'
		right_array := '__vf_array_eq_right_${depth}'
		equal := '__vf_array_eq_equal_${depth}'
		index := '__vf_array_eq_index_${depth}'
		left_element := '((${g.collection_storage_type(element_type)} *)${left_array}.data)[${index}]'
		right_element := '((${g.collection_storage_type(element_type)} *)${right_array}.data)[${index}]'
		element_equality := g.stored_equality_source(left_element, right_element, element_type, seen,
			depth + 1)
		return '({ ${flat.source_type_name(layout_type)} ${left_array} = (${left}); ${flat.source_type_name(layout_type)} ${right_array} = (${right}); bool ${equal} = ${left_array}.len == ${right_array}.len; for (int ${index} = 0; ${equal} && ${index} < ${left_array}.len; ${index}++) { if (!(${element_equality})) { ${equal} = false; } } ${equal}; })'
	}
	type_key := g.semantic_type_key(layout_type)
	if flat.source_type_name(type_key) in g.declared_kinds && g.declared_kinds[flat.source_type_name(type_key)] == .enum_ {
		return '((${left}) == (${right}))'
	}
	if flat.source_type_name(type_key) !in g.declared_kinds || g.declared_kinds[flat.source_type_name(type_key)] != .struct_ || layout_type in seen {
		return '((${left}) == (${right}))'
	}
	mut nested_seen := seen.clone()
	nested_seen << layout_type
	mut comparisons := []string{}
	for field in g.struct_field_info[flat.source_type_name(layout_type)] {
		field_name := fastc_c_identifier(field.name)
		comparisons << g.struct_equality_source('(${left}).${field_name}', '(${right}).${field_name}', field.typ, nested_seen, depth)
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
				typ:    flat.source_type_bool
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
				g.render_call_argument_expression(left_tokens, flat.source_type_id('bool')) or { return none }
			}
			right := if right_comparison.source != '' {
				right_comparison.source
			} else {
				g.render_call_argument_expression(right_tokens, flat.source_type_id('bool')) or { return none }
			}
			return FastcRenderedExpression{
				source: '((${left}) ${if item.tok == .and { '&&' } else { '||' }} (${right}))'
				typ:    flat.source_type_bool
			}
		}
	}
	if tokens.len > 1 && tokens[0].tok == .not {
		inner := g.render_struct_comparison_expression(tokens[1..]) or { return none }
		return FastcRenderedExpression{
			source: '!(${inner.source})'
			typ:    flat.source_type_bool
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
			left_layout := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(left_type)))
			right_layout := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(right_type)))
			if left_layout == right_layout && left_layout in g.sum_types {
				// Sum-type equality (`value == Primitive(Null{})`): the boxed struct
				// cannot be compared with C `==`, so compare the variant tag (and, for a
				// scalar variant cast on the right, the unboxed value).
				if sum_eq := g.render_sum_type_equality(left_tokens, right_tokens, flat.source_type_id(left_layout)) {
					source := if item.tok == .ne { '!(${sum_eq})' } else { sum_eq }
					return FastcRenderedExpression{
						source: source
						typ:    flat.source_type_bool
					}
				}
				// Two sum-type VALUES (`node.stmt != ast.empty_stmt`, neither a variant cast):
				// compare the runtime tag, then the concrete variant's contents.
				if sum_eq := g.render_sum_type_value_equality(left_tokens, right_tokens, flat.source_type_id(left_layout)) {
					source := if item.tok == .ne { '!(${sum_eq})' } else { sum_eq }
					return FastcRenderedExpression{
						source: source
						typ:    flat.source_type_bool
					}
				}
				return none
			}
			if left_layout == right_layout && left_layout.starts_with('Map_') {
				// `m1 == m2` deep-compares maps; C `==` on the erased `map` struct is
				// invalid, so route through the builtin key/value comparison.
				left := g.render_comparison_operand(left_tokens, left_type) or { return none }
				right := g.render_comparison_operand(right_tokens, right_type) or { return none }
				equality := 'builtin__map_map_eq((${left}), (${right}))'
				result := if item.tok == .ne { '!(${equality})' } else { equality }
				return FastcRenderedExpression{
					source: result
					typ:    flat.source_type_bool
				}
			}
			left_key := g.semantic_type_key(flat.source_type_id(left_layout))
			if left_layout != right_layout || flat.source_type_name(left_key) !in g.declared_kinds || g.declared_kinds[flat.source_type_name(left_key)] != .struct_ || !g.struct_equality_is_supported(left_type, []u32{}) {
				return none
			}
			left := g.render_comparison_operand(left_tokens, left_type) or { return none }
			right := g.render_comparison_operand(right_tokens, right_type) or { return none }
			equality := g.struct_equality_source('__vf_eq_left', '__vf_eq_right', left_type, []u32{}, 0)
			result := if item.tok == .ne { '!(${equality})' } else { equality }
			return FastcRenderedExpression{
				source: '({ ${flat.source_type_name(left_type)} __vf_eq_left = (${left}); ${flat.source_type_name(right_type)} __vf_eq_right = (${right}); ${result}; })'
				typ:    flat.source_type_bool
			}
		}
	}
	return none
}

// render_sum_type_equality lowers `left == SumType(operand)` (both of sum type
// `sum_type`) to a variant-tag comparison on the left's boxed `_typ`, plus the
// unboxed value for a scalar/string variant. A fieldless or struct variant matches
// on the tag alone. Returns none when the right side is not a variant cast.
fn (g &Parser) render_sum_type_equality(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, sum_type_source_id u32) ?string {
	sum_type := flat.source_type_name(sum_type_source_id)
	mut operand_start := -1
	mut open_index := -1
	if right_tokens.len >= 3 && right_tokens[0].tok == .name && right_tokens[1].tok == .lpar {
		operand_start = 2
		open_index = 1
	} else if right_tokens.len >= 5 && right_tokens[0].tok == .name && right_tokens[1].tok == .dot && right_tokens[2].tok == .name && right_tokens[3].tok == .lpar {
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
	if variant_type == u32(0) {
		return none
	}
	left := g.render_comparison_operand(left_tokens, sum_type_source_id) or { return none }
	left_temp := '__vf_sum_left'
	tag := '${left_temp}._typ == __v_typeid_${flat.source_type_name(variant_type)}'
	mut comparison := tag
	if variant_type == flat.source_type_string {
		operand := g.render_call_argument_expression(operand_tokens, variant_type) or {
			return none
		}
		comparison = '(${tag}) && builtin__string_eq(*(string *)${left_temp}._object, ${operand})'
	} else if fastc_primitive_c_type(flat.source_type_name(variant_type)) != none {
		operand := g.render_call_argument_expression(operand_tokens, variant_type) or {
			return none
		}
		comparison = '(${tag}) && (*(${flat.source_type_name(variant_type)} *)${left_temp}._object == (${operand}))'
	}
	return '({ ${sum_type} ${left_temp} = (${left}); ${comparison}; })'
}

// render_sum_type_value_equality lowers `a == b` where both `a` and `b` are values of the same
// sum type (`node.stmt == ast.empty_stmt`). The boxed `{_object,_typ}` struct cannot be
// compared with C `==`, so the tags are compared first and, when equal, the concrete variant's
// contents through a switch. A variant whose element-wise equality is not expressible (e.g. it
// holds an option/map/further sum type) falls back to a tag-only match.
fn (g &Parser) render_sum_type_value_equality(left_tokens []FastcExpressionToken, right_tokens []FastcExpressionToken, sum_type_source_id u32) ?string {
	sum_type := flat.source_type_name(sum_type_source_id)
	left := g.render_comparison_operand(left_tokens, sum_type_source_id) or { return none }
	right := g.render_comparison_operand(right_tokens, sum_type_source_id) or { return none }
	a := '__vf_sv_a'
	b := '__vf_sv_b'
	mut cases := []string{}
	for variant_source_id in g.sum_type_leaf_variants(sum_type_source_id) {
		variant := variant_source_id

		mut concrete := 'true'
		if g.struct_equality_is_supported(variant, []u32{}) {
			left_value := '(*(${flat.source_type_name(variant)} *)${a}._object)'
			right_value := '(*(${flat.source_type_name(variant)} *)${b}._object)'
			concrete = g.struct_equality_source(left_value, right_value, variant, []u32{}, 0)
		} else if fastc_primitive_c_type(flat.source_type_name(variant)) != none {
			concrete = '(*(${flat.source_type_name(variant)} *)${a}._object == *(${flat.source_type_name(variant)} *)${b}._object)'
		}
		cases << 'case __v_typeid_${flat.source_type_name(variant)}: __vf_sv_eq = (${concrete}); break;'
	}
	if cases.len == 0 {
		return none
	}
	return '({ ${sum_type} ${a} = (${left}); ${sum_type} ${b} = (${right}); bool __vf_sv_eq; if (${a}._typ != ${b}._typ) { __vf_sv_eq = false; } else { switch (${a}._typ) { ${cases.join(' ')} default: __vf_sv_eq = true; break; } } __vf_sv_eq; })'
}

// render_as_cast_expression lowers `<boxed> as Type`. A boxed sum-type / interface
// value shares the `{_object, _typ}` layout and dispatches by `_typ`, so a
// downcast to ANOTHER interface / sum type just re-boxes the same object under the
// target type, and a cast to a CONCRETE type unboxes the stored object. Returns none
// unless `as` is the top-level operator, the right side is a declared type, and the
// left operand is a boxed value.
// render_as_cast_member_access renders `(X as T).field.field…` — a parenthesized
// `as` cast followed by a member-field chain. The cast target supplies the type for
// each field lookup. Method calls / index accesses on the cast result are left to
// the other renderers (returns none).
fn (g &Parser) render_as_cast_member_access(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 3 || tokens[0].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar(tokens, 0) or { return none }
	if close + 1 >= tokens.len {
		return none
	}
	inner := tokens[1..close]
	as_index := fastc_bare_as_cast_index(inner, 0, inner.len) or { return none }
	cast := g.render_as_cast_expression(inner) or { return none }
	mut source := '(${cast.source})'
	mut current_type := cast.typ
	// Track the equivalent plain member path (`subject.field…`, dropping the redundant `as T`
	// cast) so a member smart-cast registered on it — `subject.field is Variant`, narrowing the
	// field to a variant whose method the cast's un-narrowed field type lacks — overrides the
	// cast's field read.
	mut member_path := fastc_member_chain_path(inner, 0, as_index) or { '' }
	mut i := close + 1
	for i < tokens.len {
		if tokens[i].tok == .lsbr {
			// A trailing element index on the cast member chain (`(x as T).val[0]`): the base
			// `(x as T).val` is rendered above; lower the string byte / dynamic-array element
			// access here so it does not reach the raw renderer as a mangled `.valbuiltin_…`.
			close_index := fastc_matching_delimiter(tokens, i, .lsbr, .rsbr) or { return none }
			if close_index != tokens.len - 1 || close_index <= i + 1
				|| fastc_expression_tokens_contain_range(tokens, i + 1, close_index, .dotdot) {
				return none
			}
			index_source := g.render_membership_candidate(tokens[i + 1..close_index], flat.source_type_id('int')) or {
				return none
			}
			layout := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(current_type)))
			if layout == 'string' {
				string_source := if flat.source_type_name(current_type).ends_with('*') {
					'*(${source})'
				} else {
					source
				}
				return FastcRenderedExpression{
					source: 'builtin__string_at(${string_source}, ${index_source})'
					typ:    flat.source_type_u8
				}
			}
			if layout.starts_with('Array_') {
				element_type := g.array_element_type(current_type) or { return none }
				array_value := if flat.source_type_name(current_type).ends_with('*') {
					'*(${source})'
				} else {
					source
				}
				return FastcRenderedExpression{
					source: '(*(${g.collection_storage_type(element_type)} *)builtin__array_get(${array_value}, ${index_source}))'
					typ:    element_type
				}
			}
			return none
		}
		if i + 1 >= tokens.len || tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		if i + 2 < tokens.len && tokens[i + 2].tok == .lpar {
			return none
		}
		field := g.struct_field_metadata(current_type, tokens[i + 1].lit) or { return none }
		for storage_name in field.storage_path {
			separator := if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
				'->'
			} else {
				'.'
			}
			source += separator + fastc_c_identifier(storage_name)
			current_type = g.struct_direct_member_type(current_type, storage_name)
			if current_type == 0 {
				return none
			}
		}
		separator := if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
			'->'
		} else {
			'.'
		}
		field_source := source + separator + fastc_c_identifier(field.name)
		source = if field.is_shared_pointer { '*(${field_source})' } else { field_source }
		current_type = field.typ
		if member_path != '' {
			member_path += '.' + tokens[i + 1].lit
			if smartcast := g.member_smartcasts[member_path] {
				source = smartcast.source
				current_type = smartcast.typ
			}
		}
		i += 2
	}
	return FastcRenderedExpression{
		source: source
		typ:    current_type
	}
}

// rewrite_embedded_as_casts replaces each `(expr as T)` / `(expr as T).field` group that is
// embedded inside a larger expression (`a && (x.info as Struct).parent_type != 0`) with a
// single synthetic token carrying the pre-rendered cast as its `source`. The stand-alone
// `as`-cast paths only fire when the whole expression is the cast, so without this a cast used
// as a boolean/comparison operand would reach the raw renderer with `as` left intact. Returns
// none when the expression has no embedded `as` cast to lower.
fn (g &Parser) rewrite_embedded_as_casts(tokens []FastcExpressionToken) ?[]FastcExpressionToken {
	if !fastc_expression_tokens_contain(tokens, .key_as) {
		return none
	}
	mut result := tokens.clone()
	mut changed := false
	for _ in 0 .. 64 {
		mut rewrote := false
		mut i := 0
		for i < result.len {
			if result[i].tok != .lpar {
				i++
				continue
			}
			// Only a grouping paren can introduce a cast; a `(` right after a name / `)` / `]`
			// is a call or index whose argument may itself hold an `as` (`conv(x as T)`).
			if i > 0 && result[i - 1].tok in [.name, .rpar, .rsbr] {
				i++
				continue
			}
			close := fastc_matching_rpar(result, i) or {
				i++
				continue
			}
			if fastc_bare_as_cast_index(result, i + 1, close) == none {
				i++
				continue
			}
			inner := result[i + 1..close].clone()
			mut end := close + 1
			for end + 1 < result.len && result[end].tok == .dot && result[end + 1].tok == .name && !(end + 2 < result.len && result[end + 2].tok == .lpar) {
				end += 2
			}
			// A trailing element index on the cast member access (`(x as T).val[0]`) is part of
			// the group — render_as_cast_member_access lowers it to a string/array access — so
			// consume it too, provided nothing indexes/dots the result further.
			if end > close + 1 && end < result.len && result[end].tok == .lsbr {
				if index_close := fastc_matching_delimiter(result, end, .lsbr, .rsbr) {
					if index_close + 1 >= result.len || result[index_close + 1].tok !in [
						.dot,
						.lsbr,
					] {
						end = index_close + 1
					}
				}
			}
			if end - i == result.len {
				// The whole expression is the cast; leave it to the stand-alone paths.
				i = end
				continue
			}
			rendered := if end > close + 1 {
				g.render_as_cast_member_access(result[i..end]) or {
					i = end
					continue
				}
			} else {
				g.render_as_cast_expression(inner) or {
					i = end
					continue
				}
			}
			mut next := result[..i].clone()
			next << FastcExpressionToken{
				tok:    .name
				source: fastc_token_source(rendered.source)
				typ:    rendered.typ
			}
			next << result[end..]
			result = next.clone()
			changed = true
			rewrote = true
			break
		}
		if !rewrote {
			break
		}
	}
	if !changed {
		return none
	}
	return result
}

fn (g &Parser) render_as_cast_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	return g.render_as_cast_expression_with_reference(tokens, false)
}

fn (g &Parser) render_as_cast_expression_with_reference(tokens []FastcExpressionToken, reference bool) ?FastcRenderedExpression {
	if tokens.len >= 3 && tokens[0].tok == .lpar {
		if close := fastc_matching_rpar(tokens, 0) {
			if close == tokens.len - 1 {
				inner := g.render_as_cast_expression_with_reference(tokens[1..close], reference)?
				return FastcRenderedExpression{
					source: '(${inner.source})'
					typ:    inner.typ
				}
			}
		}
	}
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
	// Assignment binds looser than `as`, so `x = y as T` parses as `x = (y as T)`, not
	// `(x = y) as T`. A top-level assignment left of the `as` means this cast belongs to the
	// assignment's RHS; bail so the assignment handler renders the target and lowers the cast
	// on the RHS alone (otherwise the whole `x = y` is mistaken for the cast operand).
	mut assign_depth := 0
	for item in left_tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { assign_depth++ }
			.rpar, .rsbr, .rcbr { assign_depth-- }
			else {
				if assign_depth == 0 && item.tok.is_assignment() {
					return none
				}
			}
		}
	}
	target_c := g.type_from_expression_tokens(tokens[as_index + 1..]) or { return none }
	inferred_left := g.infer_expression_type(left_tokens) or { return none }
	left_type := fastc_normalize_inferred_type(inferred_left)
	if flat.source_type_name(left_type).trim_right('*') == flat.source_type_name(target_c) {
		if reference && left_tokens.len == 1 && left_tokens[0].tok == .name {
			if local := g.locals[left_tokens[0].lit] {
				if local.smartcast_origin_source != '' {
					access := if local.smartcast_origin_type_text().ends_with('*') {
						'->'
					} else {
						'.'
					}
					return FastcRenderedExpression{
						source: '((${flat.source_type_name(target_c)} *)${local.smartcast_origin_source}${access}_object)'
						typ:    flat.source_type_id(flat.source_type_name(target_c) + '*')
					}
				}
			}
		}
		// `x as T` where `x` is already `T` (e.g. through a smartcast): an identity
		// cast, so just yield the value (dereferencing a smartcast pointer). A member/local
		// chain must render through render_member_receiver so a live member smart-cast (a
		// narrowed `right` reached as a variant pointer) is honored rather than the raw name.
		left_source := if member := g.render_member_receiver(left_tokens) {
			member
		} else {
			g.render_call_argument_expression(left_tokens, left_type) or { return none }
		}
		source := if reference {
			if flat.source_type_name(left_type).ends_with('*') {
				left_source
			} else {
				'&(${left_source})'
			}
		} else if flat.source_type_name(left_type).ends_with('*') {
			'(*(${left_source}))'
		} else {
			left_source
		}
		return FastcRenderedExpression{
			source: source
			typ:    flat.source_type_id(flat.source_type_name(target_c) + if reference {
				'*'
			} else {
				''
			})
		}
	}
	if !g.is_boxed_type(left_type) {
		return none
	}
	left_source := g.render_call_argument_expression(left_tokens, left_type) or { return none }
	access := if flat.source_type_name(left_type).ends_with('*') { '->' } else { '.' }
	src := '__vf_as_src'
	if g.is_boxed_type(target_c) {
		return FastcRenderedExpression{
			source: '({ ${flat.source_type_name(left_type)} ${src} = (${left_source}); (${flat.source_type_name(target_c)}){._object = ${src}${access}_object, ._typ = ${src}${access}_typ}; })'
			typ:    target_c
		}
	}
	return FastcRenderedExpression{
		source: '({ ${flat.source_type_name(left_type)} ${src} = (${left_source}); ${if reference {
			''
		} else {
			'*'
		}}((${flat.source_type_name(target_c)} *)${src}${access}_object); })'
		typ:    flat.source_type_id(flat.source_type_name(target_c) + if reference {
			'*'
		} else {
			''
		})
	}
}

fn (g &Parser) render_enum_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.eq, .ne, .lt, .gt, .le, .ge] && i > 0 && i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			mut left_type := (g.infer_expression_type(left_tokens) or { u32(0) })
			mut right_type := (g.infer_expression_type(right_tokens) or { u32(0) })
			if left_type == u32(0) && left_tokens.len > 2 && left_tokens[left_tokens.len - 2].tok == .dot && left_tokens.last().tok == .name {
				receiver_type := (g.infer_expression_type(left_tokens[..left_tokens.len - 2]) or { u32(0) })
				left_type = g.struct_member_type(receiver_type, left_tokens.last().lit)
			}
			if right_type == u32(0) && right_tokens.len > 2 && right_tokens[right_tokens.len - 2].tok == .dot && right_tokens.last().tok == .name {
				receiver_type := (g.infer_expression_type(right_tokens[..right_tokens.len - 2]) or { u32(0) })
				right_type = g.struct_member_type(receiver_type, right_tokens.last().lit)
			}
			if g.declared_kinds[flat.source_type_name(g.semantic_type_key(left_type))] == .enum_ && right_tokens.len == 2 && right_tokens[0].tok == .dot && right_tokens[1].tok == .name {
				enum_type := flat.source_type_name(left_type).trim_right('*')
				// A subject narrowed by `is` (`value is Kind && value == .one`) is typed and
				// rendered as a pointer to its payload; the comparison needs the enum value.
				left := g.render_call_argument_expression(left_tokens, flat.source_type_id(enum_type)) or {
					return none
				}
				return FastcRenderedExpression{
					source: '((${left}) ${item.tok.str()} (${enum_type}__${right_tokens[1].lit}))'
					typ:    flat.source_type_bool
				}
			}
			if g.declared_kinds[flat.source_type_name(g.semantic_type_key(right_type))] == .enum_ && left_tokens.len == 2 && left_tokens[0].tok == .dot && left_tokens[1].tok == .name {
				enum_type := flat.source_type_name(right_type).trim_right('*')
				right := g.render_call_argument_expression(right_tokens, flat.source_type_id(enum_type)) or {
					return none
				}
				return FastcRenderedExpression{
					source: '((${enum_type}__${left_tokens[1].lit}) ${item.tok.str()} (${right}))'
					typ:    flat.source_type_bool
				}
			}
		}
	}
	return none
}

fn (g &Parser) render_option_none_comparison(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	stripped := fastc_strip_paren_tokens(tokens)
	if stripped.len < tokens.len {
		return g.render_option_none_comparison(stripped)
	}
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
				left_nested := g.render_option_none_comparison(left_tokens)
				right_nested := g.render_option_none_comparison(right_tokens)
				if left_nested != none || right_nested != none {
					left := g.render_call_argument_expression(left_tokens, flat.source_type_id('bool')) or { return none }
					right := g.render_call_argument_expression(right_tokens, flat.source_type_id('bool')) or { return none }
					return FastcRenderedExpression{
						source: '((${left}) ${item.tok.str()} (${right}))'
						typ:    flat.source_type_bool
					}
				}
				return none
			}
			value_tokens := if left_is_none { right_tokens } else { left_tokens }
			value := g.render_call_argument_expression(value_tokens, flat.source_type_id('Option')) or { return none }
			operator := if item.tok == .eq { '==' } else { '!=' }
			return FastcRenderedExpression{
				source: '((${value}).state ${operator} 2)'
				typ:    flat.source_type_bool
			}
		}
	}
	return none
}

// render_nil_comparison lowers `ptr == nil` / `ptr != nil` (`unsafe { nil }` renders to `NULL`).
// The pointer operand must be compared as a pointer, not auto-dereferenced to its pointee value;
// a `mut T` reference parameter is already the C pointer, so it is rendered without the deref.
fn (g &Parser) render_nil_comparison(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.eq, .ne {
				if depth != 0 || i == 0 || i + 1 >= tokens.len {
					continue
				}
				left_tokens := tokens[..i]
				right_tokens := tokens[i + 1..]
				left_is_nil := left_tokens.len == 1 && left_tokens[0].tok == .key_nil
				right_is_nil := right_tokens.len == 1 && right_tokens[0].tok == .key_nil
				if left_is_nil == right_is_nil {
					return none
				}
				value_tokens := if left_is_nil { right_tokens } else { left_tokens }
				if !fastc_tokens_have_top_level_boolean_or_membership(tokens)
					&& fastc_expression_tokens_contain(value_tokens, .lcbr) {
					typ := g.infer_expression_type(value_tokens) or { return none }
					layout := g.underlying_alias_type(typ)
					if layout == flat.source_type_voidptr || fastc_is_pointer_type(layout) {
						// Lower the literal prefix once, then read its pointer-valued member chain.
						mut literal_end := -1
						for j, value in value_tokens {
							if value.tok == .lcbr {
								literal_end = fastc_matching_delimiter(value_tokens, j, .lcbr, .rcbr) or { return none }
								break
							}
						}
						if literal_end < 0 || literal_end + 1 >= value_tokens.len { return none }
						literal := g.render_struct_literal_expression(value_tokens[..literal_end + 1]) or { return none }
						mut member_tokens := [FastcExpressionToken{
							tok:    .name
							source: fastc_token_source('(${literal.source})')
							typ:    literal.typ
						}]
						member_tokens << value_tokens[literal_end + 1..]
						pointer := g.render_member_receiver(member_tokens) or { return none }
						operator := if item.tok == .eq { '==' } else { '!=' }
						return FastcRenderedExpression{
							source: '((${pointer}) ${operator} NULL)'
							typ:    flat.source_type_bool
						}
					}
				}
				// Only a bare `mut T` reference parameter needs special handling: it is a C `T*`,
				// but the raw renderer auto-dereferences it, so `node == nil` becomes an invalid
				// `*node == NULL`. Every other operand (a member chain, an already-pointer local)
				// is rendered correctly by the existing comparison paths, so leave those alone.
				if !(value_tokens.len == 1 && value_tokens[0].tok == .name && (g.locals[value_tokens[0].lit] or { FastcLocal{} }).is_reference) {
					return none
				}
				pointer := g.resolved_root_expression_name(value_tokens[0].lit)
				operator := if item.tok == .eq { '==' } else { '!=' }
				return FastcRenderedExpression{
					source: '((${pointer}) ${operator} NULL)'
					typ:    flat.source_type_bool
				}
			}
			else {}
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
			g.render_comparison_operand(left_tokens, flat.source_type_id('')) or { return none }
		}
		right_source := if right_special.source != '' {
			right_special.source
		} else {
			g.render_comparison_operand(right_tokens, flat.source_type_id('')) or { return none }
		}
		return FastcRenderedExpression{
			source: '(${left_source}${if item.tok == .and { '&&' } else { '||' }}${right_source})'
			typ:    flat.source_type_bool
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
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0 || i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		left_type := g.infer_expression_type(left_tokens) or { return none }
		right_type := g.infer_expression_type(right_tokens) or { return none }
		if fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(left_type))) != 'string' || fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(right_type))) != 'string' {
			return none
		}
		left_source := g.render_comparison_operand(left_tokens, flat.source_type_id('string')) or { return none }
		right_source := g.render_comparison_operand(right_tokens, flat.source_type_id('string')) or { return none }
		source := match item.tok {
			.eq { 'builtin__string_eq(${left_source},${right_source})' }
			.ne { '!builtin__string_eq(${left_source},${right_source})' }
			.lt { 'builtin__string_lt(${left_source},${right_source})' }
			.gt { 'builtin__string_lt(${right_source},${left_source})' }
			.le { '!builtin__string_lt(${right_source},${left_source})' }
			.ge { '!builtin__string_lt(${left_source},${right_source})' }
			else {
				return none
			}
		}
		return FastcRenderedExpression{
			source: source
			typ:    flat.source_type_bool
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

// fastc_tokens_have_top_level_boolean_or_membership reports whether tokens contain, at paren
// depth 0, a boolean connective (`&&`/`||`) or a set-membership operator (`in`/`!in`). Such an
// operand is a full boolean sub-expression rather than a plain comparison operand, so it must be
// rendered through render_call_argument_expression to lower its nested membership/logical parts.
fn fastc_tokens_have_top_level_boolean_or_membership(tokens []FastcExpressionToken) bool {
	if tokens.len > 1 && tokens[0].tok == .not {
		return fastc_tokens_have_top_level_boolean_or_membership(tokens[1..])
	}
	// Strip a fully-wrapping paren pair first: `(a && b)` is still a boolean sub-expression,
	// so its connective must be seen even though the raw tokens nest it one level deep.
	inner := fastc_strip_paren_tokens(tokens)
	mut depth := 0
	for item in inner {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			else {
				if depth == 0 && item.tok in [.and, .logical_or, .key_in, .not_in] {
					return true
				}
			}
		}
	}
	return false
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
				typ:    flat.source_type_bool
			}
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
		} else if fastc_tokens_have_top_level_boolean_or_membership(left_tokens) {
			// This side is itself a boolean sub-expression (a nested `&&`/`||` chain or a
			// set-membership test), not a plain comparison operand. render_comparison_operand
			// would emit it verbatim, leaving a raw `x !in [...]` in the C output; route it
			// through the full expression renderer so its membership/logical parts lower.
			g.render_call_argument_expression(left_tokens, flat.source_type_id('bool')) or { return none }
		} else {
			g.render_comparison_operand(left_tokens, flat.source_type_id('bool')) or { return none }
		}
		right_source := if right_special.source != '' {
			right_special.source
		} else if fastc_tokens_have_top_level_boolean_or_membership(right_tokens) {
			g.render_call_argument_expression(right_tokens, flat.source_type_id('bool')) or { return none }
		} else {
			g.render_comparison_operand(right_tokens, flat.source_type_id('bool')) or { return none }
		}
		return FastcRenderedExpression{
			source: '((${left_source})${if item.tok == .and { '&&' } else { '||' }}(${right_source}))'
			typ:    flat.source_type_bool
		}
	}
	if tokens.len > 1 && tokens[0].tok == .not {
		inner := g.render_mixed_integer_comparison_expression(tokens[1..]) or { return none }
		return FastcRenderedExpression{
			source: '!(${inner.source})'
			typ:    flat.source_type_bool
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
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0 || i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		left_inferred_type := g.infer_expression_type(left_tokens) or { return none }
		right_inferred_type := g.infer_expression_type(right_tokens) or { return none }
		left_type := g.underlying_alias_type(left_inferred_type)
		right_type := g.underlying_alias_type(right_inferred_type)
		if !fastc_is_mixed_sign_comparison(left_type, right_type) {
			return none
		}
		left_source := g.render_comparison_operand(left_tokens, left_type) or { return none }
		right_source := g.render_comparison_operand(right_tokens, right_type) or { return none }
		return FastcRenderedExpression{
			source: fastc_mixed_sign_comparison(item.tok, left_source, left_type, right_source) or {
				return none
			}
			typ:    flat.source_type_bool
		}
	}
	return none
}

// fastc_is_mixed_sign_comparison reports a comparison of a wide unsigned and a signed integer
// operand (both types with aliases resolved), which plain C would compare as unsigned.
fn fastc_is_mixed_sign_comparison(left_type u32, right_type u32) bool {
	return (fastc_is_wide_unsigned_integer_type(left_type) && fastc_is_signed_integer_type(right_type))
		|| (fastc_is_wide_unsigned_integer_type(right_type) && fastc_is_signed_integer_type(left_type))
}

// fastc_mixed_sign_comparison compares a wide unsigned and a signed integer operand by value,
// as cgen does, through the `v_fastc_us_*` helpers; `left_type` (aliases resolved) tells which
// operand is the unsigned one.
fn fastc_mixed_sign_comparison(operator token.Token, left string, left_type u32, right string) ?string {
	left_is_unsigned := fastc_is_wide_unsigned_integer_type(left_type)
	mut operation := match operator {
		.eq { 'eq' }
		.ne { 'ne' }
		.gt { 'gt' }
		.lt { 'lt' }
		.ge { 'ge' }
		.le { 'le' }
		else { return none }
	}
	unsigned_source := if left_is_unsigned { left } else { right }
	signed_source := if left_is_unsigned { right } else { left }
	if !left_is_unsigned {
		operation = match operation {
			'gt' { 'lt' }
			'lt' { 'gt' }
			'ge' { 'le' }
			'le' { 'ge' }
			else { operation }
		}
	}
	return 'v_fastc_us_${operation}((u64)(${unsigned_source}), (i64)(${signed_source}))'
}

// narrowed_subject_value_type is the value type of an operand that is exactly a subject
// narrowed by `is`, which renders as a pointer to its variant (`((i64 *)tmp._object)`);
// none for any other operand. An option local narrowed by `!= none` renders as its payload
// value already, even when that value is a pointer.
fn (g &Parser) narrowed_subject_value_type(tokens []FastcExpressionToken) ?u32 {
	if g.member_smartcasts.len == 0 { return none }
	path := fastc_indexed_member_chain_path(tokens) or { return none }
	smartcast := g.member_smartcasts[path] or { return none }
	smartcast_type := flat.source_type_name(smartcast.typ)
	if !smartcast_type.ends_with('*') { return none }
	if local := g.locals[path] {
		if local.option_value_type != u32(0) { return none }
	}
	if smartcast.value_type != u32(0) { return smartcast.value_type }
	return flat.source_type_id(smartcast_type[..smartcast_type.len - 1])
}

// fastc_narrowed_variant_value reads the value of a narrowed variant from its variant pointer.
// A pointer-like variant (`voidptr`) is stored directly in `_object`, so that pointer is the
// value; any other variant is boxed behind it.
fn fastc_narrowed_variant_value(source string, value_type u32) string {
	if fastc_is_pointer_type(value_type) {
		return '((${fastc_output_c_type(flat.source_type_name(value_type))})(${source}))'
	}
	return '*(${source})'
}

// render_guard_comparison renders a top-level comparison guard (`expr.kind == .constant`)
// by rendering each operand through render_comparison_operand, which reads any active
// smart-cast on the left and resolves an enum-shorthand right operand against the left
// operand's type. Returns none when there is no top-level comparison operator.
fn (g &Parser) render_guard_comparison(tokens []FastcExpressionToken) ?string {
	mut depth := 0
	mut op_index := -1
	mut op := token.Token.unknown
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.eq, .ne, .lt, .gt, .le, .ge {
				if depth == 0 && op_index < 0 {
					op_index = i
					op = item.tok
				}
			}
			else {}
		}
	}
	if op_index <= 0 || op_index + 1 >= tokens.len {
		return none
	}
	left_tokens := tokens[..op_index]
	mut left := if fastc_tokens_have_top_level_boolean_or_membership(left_tokens) {
		g.render_call_argument_expression(left_tokens, flat.source_type_id('bool')) or { return none }
	} else {
		g.render_comparison_operand(left_tokens, flat.source_type_id('')) or { return none }
	}
	mut left_type := (g.infer_expression_type(left_tokens) or { u32(0) })
	// Flow-sensitive member smartcasts can render the operand correctly while the
	// generic inference still sees the boxed receiver. Recover the concrete field
	// type so a shorthand RHS such as `.mul` resolves against its enum.
	if member_type_source_id := g.infer_member_access_type(left_tokens, 0, left_tokens.len) {
		member_type := member_type_source_id

		left_type = member_type
	}
	// A subject narrowed earlier in this condition (`cv is i64 && cv != io_core`) renders as
	// its variant pointer; the comparison is of the variant's value.
	if value_type := g.narrowed_subject_value_type(left_tokens) {
		left = fastc_narrowed_variant_value(left, value_type)
		left_type = value_type
	}
	right_tokens := tokens[op_index + 1..]
	mut right := if fastc_tokens_have_top_level_boolean_or_membership(right_tokens) {
		g.render_call_argument_expression(right_tokens, flat.source_type_id('bool')) or { return none }
	} else {
		g.render_comparison_operand(right_tokens, left_type) or { return none }
	}
	if value_type := g.narrowed_subject_value_type(right_tokens) {
		right = fastc_narrowed_variant_value(right, value_type)
	}
	if fastc_normalize_inferred_type(left_type) == flat.source_type_string {
		if op == .eq {
			return 'builtin__string_eq(${left}, ${right})'
		}
		if op == .ne {
			return '(!builtin__string_eq(${left}, ${right}))'
		}
	}
	c_op := match op {
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		else {
			return none
		}
	}
	return '((${left}) ${c_op} (${right}))'
}

// fastc_tokens_contain_chained_index reports whether tokens contain an index applied directly
// to another index's result (`fields[0][0]`, a string element). The raw renderer leaves the
// second `[i]` as an invalid C index on the non-array value, so such an expression must be
// lowered through render_array_access (via render_guard_comparison / render_comparison_operand).
// fastc_tokens_have_top_level_assignment reports whether tokens contain an assignment operator
// at paren/bracket/brace depth 0 (`x = …`, `x += …`), i.e. the expression is really a statement
// assigning to `x`, not a bare value.
fn fastc_tokens_have_top_level_assignment(tokens []FastcExpressionToken) bool {
	mut depth := 0
	for item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			else {
				if depth == 0 && item.tok.is_assignment() {
					return true
				}
			}
		}
	}
	return false
}

fn fastc_tokens_contain_chained_index(tokens []FastcExpressionToken) bool {
	for i in 1 .. tokens.len {
		if tokens[i].tok == .lsbr && tokens[i - 1].tok == .rsbr {
			return true
		}
	}
	return false
}

// fastc_tokens_contain_nested_propagation reports whether a `!` result/option propagation
// appears inside a parenthesized call argument (a `.not` at paren depth > 0 immediately
// following a `)`), as in `f(g()!)`. The raw renderer leaves such a `!` as a stray C
// operator; the expression must be lowered so the argument's Option is unwrapped.
fn fastc_tokens_contain_nested_propagation(tokens []FastcExpressionToken) bool {
	mut depth := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.not {
				if depth > 0 && i > 0 && tokens[i - 1].tok == .rpar {
					return true
				}
			}
			else {}
		}
	}
	return false
}

// fastc_tokens_are_plain_call reports whether tokens form a single function or method call
// (`f(a)`, `recv.m(a)`) with no trailing member access or index on its result. Such an
// operand must be lowered through render_missing_call_arguments so its arguments receive the
// value/pointer coercions (auto-deref of a `&T` argument to a `T` parameter, `!`/`?`
// propagation) that the raw renderer, which streams the tokens verbatim, cannot apply.
fn fastc_tokens_are_plain_call(tokens []FastcExpressionToken) bool {
	if tokens.len < 3 || tokens.last().tok != .rpar {
		return false
	}
	for i in 1 .. tokens.len {
		if tokens[i].tok != .lpar || tokens[i - 1].tok != .name {
			continue
		}
		close := fastc_matching_rpar(tokens, i) or { continue }
		if close != tokens.len - 1 {
			continue
		}
		name_index := i - 1
		if name_index == 0 {
			return true
		}
		if name_index >= 2 && tokens[name_index - 1].tok == .dot {
			return fastc_method_receiver_start(tokens, name_index - 1) == 0
		}
		return false
	}
	return false
}

// fastc_comparison_operand_is_plain_call reports whether the left or right operand of the
// first top-level comparison in `tokens` is a plain function/method call (see
// fastc_tokens_are_plain_call), so the comparison should be lowered through
// render_guard_comparison to coerce that call's arguments.
fn fastc_comparison_operand_is_plain_call(tokens []FastcExpressionToken) bool {
	mut depth := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.eq, .ne, .lt, .gt, .le, .ge {
				if depth == 0 {
					return fastc_tokens_are_plain_call(tokens[..i]) || fastc_tokens_are_plain_call(tokens[i + 1..])
				}
			}
			else {}
		}
	}
	return false
}

fn (g &Parser) render_comparison_operand(tokens []FastcExpressionToken, expected_type_source_id u32) ?string {
	if g.selfhost && fastc_tokens_contain_chained_index(tokens) {
		// The operand is itself a comparison with a chained index (`a[i][j] == c`, e.g. the right
		// side of an `&&`); render it through render_guard_comparison so the nested string/array
		// index lowers rather than leaving a raw C `(...)[j]`.
		if guard := g.render_guard_comparison(tokens) {
			return guard
		}
	}
	if g.selfhost && g.expression_uses_member_smartcast(tokens) {
		// A member-smartcast chain (`x.f.g` where `x.f is T`) must read through the
		// concrete-variant pointer, not the boxed member's source spelling.
		if member_source := g.render_member_receiver(tokens) {
			return member_source
		}
	}
	if g.selfhost && tokens.len > 1 && tokens.last().tok == .not && !fastc_trailing_not_marks_fixed_array_literal(tokens) {
		if propagation := g.render_option_propagation(tokens[..tokens.len - 1]) {
			return propagation.source
		}
	}
	if g.selfhost {
		if common_field := g.render_sumtype_common_field_access(tokens) {
			return common_field.source
		}
	}
	if member := g.render_call_result_member_expression(tokens) {
		return member.source
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if integer_comparison := g.render_mixed_integer_comparison_expression(tokens) {
		return integer_comparison.source
	}
	// The operand may itself be an enum comparison (`sym(x).kind == .multi_return`, e.g. one
	// side of an `&&`); resolve its `.member` shorthand against the enum type rather than
	// leaving a raw C `.member`.
	if g.selfhost {
		if enum_comparison := g.render_enum_comparison_expression(tokens) {
			return enum_comparison.source
		}
	}
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation.source
	}
	if struct_literal := g.render_struct_literal_expression(tokens) {
		return struct_literal.source
	}
	if map_expression := g.render_map_expression(tokens) {
		return map_expression.source
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access.source
	}
	if method_call := g.render_method_call_expression(tokens, raw) {
		// A method can return an array that is indexed before a trailing member read
		// (`node.generic_params()[0].len`). Lower that index after substituting the
		// method's C call spelling, otherwise raw C indexes the array wrapper struct.
		if nested_array := g.render_nested_array_access_expression(tokens, method_call.source) {
			return nested_array.source
		}
		// A trailing field on a pointer-returning method call (`table.sym(x).cname`) is left as a
		// raw `.field` by the method renderer; promote it to `->field` so C does not access a
		// field on a pointer rvalue.
		if pointer_members := g.render_pointer_member_access_expression(tokens, method_call.source) {
			return pointer_members.source
		}
		return method_call.source
	}
	if call := g.render_missing_call_arguments(tokens) {
		return call.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		// Pointer-member rendering may rewrite the receiver before a dynamic-array
		// field indexed by a nested method call. Finish lowering that index instead
		// of returning a raw C subscript on the array wrapper.
		if nested_array := g.render_nested_array_access_expression(tokens, pointer_members.source) {
			return nested_array.source
		}
		return pointer_members.source
	}
	return g.render_membership_candidate(tokens, expected_type_source_id)
}

// render_enum_flag_shorthand_combination resolves a flag-enum combination written with
// leading-dot shorthands against a known enum type, e.g. `.integer | .unsigned` with
// expected type `Properties` -> `(Properties__integer | Properties__unsigned)`. Every
// operand must be a `.member` shorthand and every separator a bitwise flag operator; any
// other shape returns none so the caller falls back to the ordinary renderer.
fn (g &Parser) render_enum_flag_shorthand_combination(tokens []FastcExpressionToken, enum_type_source_id u32) ?string {
	enum_type := flat.source_type_name(enum_type_source_id)
	if tokens.len < 5 {
		return none
	}
	enum_c := enum_type.trim_right('*')
	mut out := ''
	mut i := 0
	for i < tokens.len {
		if tokens[i].tok != .dot || i + 1 >= tokens.len || !(tokens[i + 1].tok == .name || tokens[i + 1].tok.is_keyword()) {
			return none
		}
		out += '${enum_c}__${tokens[i + 1].lit}'
		i += 2
		if i == tokens.len {
			break
		}
		operator := match tokens[i].tok {
			.pipe { '|' }
			.amp { '&' }
			.xor { '^' }
			else {
				return none
			}
		}
		out += ' ${operator} '
		i++
	}
	return '(${out})'
}

// is_plain_aggregate_value_type identifies the nonboxed aggregate layouts
// whose local values already have their ordinary by-value C representation.
fn (g &Parser) is_plain_aggregate_value_type(typ u32) bool {
	type_name := flat.source_type_name(typ)
	if type_name == '' || fastc_is_pointer_type(typ)
		|| type_name in ['Option', '__v_result'] || type_name.starts_with('FixedArray_')
		|| type_name in g.sum_types || g.alias_base_types.type_id(type_name) != none {
		return false
	}
	if kind := g.declared_kinds[flat.source_type_name(g.semantic_type_key(typ))] {
		return kind == .struct_
	}
	return type_name.starts_with('Array_') || type_name.starts_with('Map_')
}

// render_selfhost_scalar_call_argument skips collection and coercion dispatch
// for primitive literals and exact-type local values with ordinary by-value storage.
fn (g &Parser) render_selfhost_scalar_call_argument(tokens []FastcExpressionToken, expected_type u32) ?string {
	if !g.selfhost || expected_type == 0 || tokens.len != 1 || tokens[0].has_source() {
		return none
	}
	is_scalar := fastc_simple_call_scalar_type(expected_type)
	if !is_scalar && fastc_is_pointer_type(expected_type) { return none }
	item := tokens[0]
	if is_scalar && item.typ == 0 && !item.is_mut_argument
		&& item.tok in [.number, .string, .char, .key_true, .key_false] {
		if literal := g.render_selfhost_simple_array_literal_item(tokens, expected_type) {
			return literal
		}
	}
	if item.tok != .name || item.is_mut_argument || item.lit == 'C'
		|| item.lit in g.member_smartcasts {
		return none
	}
	local := g.locals[item.lit] or { return none }
	if local.is_reference || local.option_value_type != 0 || local.smartcast_origin_type != 0
		|| (item.typ != 0 && item.typ != local.typ) || local.typ != expected_type {
		return none
	}
	if !is_scalar && !g.is_plain_aggregate_value_type(expected_type) { return none }
	return if local.c_name != '' { local.c_name } else { fastc_c_identifier(item.lit) }
}

// render_selfhost_plain_member_call_argument preserves the ordinary member
// spelling when its exact parameter type requires no storage conversion.
fn (g &Parser) render_selfhost_plain_member_call_argument(tokens []FastcExpressionToken, expected_type u32) ?string {
	if expected_type == 0 || tokens.len != 3 { return none }
	field := g.plain_local_direct_member_field(tokens) or { return none }
	if field.typ != expected_type { return none }
	is_scalar := fastc_simple_call_scalar_type(expected_type)
	if !is_scalar && !g.is_plain_aggregate_value_type(expected_type) { return none }
	// The legacy membership path finishes through render_member_receiver, which
	// uses the resolved root C name and sanitized direct field name.
	return '${g.local_c_name(tokens[0].lit)}.${fastc_c_identifier(field.name)}'
}

// can_reuse_special_expression_miss excludes lowerings that can register
// helpers, temporarily bind names, or consume temporary identifiers before failing.
fn (g &Parser) can_reuse_special_expression_miss(flags FastcTokenFlags) bool {
	return g.selfhost && !g.translated && g.member_smartcasts.len == 0
		&& !flags.has_lpar && !flags.has_lsbr && !flags.has_lcbr
		&& !flags.has_assignment && !flags.has_as && !flags.has_type_test
		&& !flags.has_typeof && !flags.has_not && !flags.has_question
		&& !flags.has_logical && !flags.has_membership && !flags.has_source_name
}

fn (g &Parser) render_call_argument_expression(tokens []FastcExpressionToken, expected_type_source_id u32) ?string {
	if scalar := g.render_selfhost_scalar_call_argument(tokens, expected_type_source_id) {
		return scalar
	}
	if member := g.render_selfhost_plain_member_call_argument(tokens, expected_type_source_id) {
		return member
	}
	expected_type := flat.source_type_name(expected_type_source_id)
	if fixed_array := g.render_fixed_array_layout_argument(tokens, expected_type_source_id) {
		return fixed_array
	}
	if fixed_array := g.render_fixed_array_dynamic_argument(tokens, expected_type_source_id) {
		return fixed_array
	}
	if g.selfhost && expected_type == 'voidptr'
		&& fastc_fixed_array_length(flat.source_type_name(g.underlying_alias_type(g.infer_expression_type(tokens) or { u32(0) })).trim_right('*')) != none
		&& g.fixed_array_uses_raw_storage(fastc_strip_paren_tokens(tokens)) {
		// A C array argument decays to its real element storage. Boxing it would copy
		// the caller's array and lose mutations performed by native memory functions.
		if raw := g.render_member_receiver(fastc_strip_paren_tokens(tokens)) {
			return '((voidptr)(${raw}))'
		}
	}
	if g.selfhost && tokens.len > 1 && tokens.last().tok == .question {
		// A trailing `?` propagates an option exactly like `!` propagates a result, and FastC
		// represents both with one `Option` type. read_expression normalizes only the top-level
		// `?`; do the same here so propagation is lowered inside call arguments and struct-literal
		// field values (`f(g()?, h()?)?`), not left as a raw C `?` (a stray ternary operator).
		last := tokens.last()
		mut normalized := tokens.clone()
		normalized[normalized.len - 1] = FastcExpressionToken{
			tok:             .not
			source:          last.source
			unsafe_depth:    last.unsafe_depth
			is_mut_argument: last.is_mut_argument
			is_statement:    last.is_statement
			lit:             last.lit
			typ:             last.typ
		}
		return g.render_call_argument_expression(normalized, expected_type_source_id)
	}
	if g.selfhost && tokens.len > 1 && tokens.last().tok == .not
		&& !fastc_trailing_not_marks_fixed_array_literal(tokens)
		&& fastc_or_operand_token_start(tokens[..tokens.len - 1]) == 0 {
		// A postfix propagation belongs to its final operand; the binary/concatenation
		// renderer owns a longer expression such as `"map[" + key()?`.
		// `x.m()!` as an operand (`assert a.b().m()! == 0`): lower the propagation here, as
		// render_comparison_operand does, rather than leave a raw `!` in the C. A value the
		// parameter boxes (a sum-type variant, an interface) is left to the paths below.
		value_type := fastc_normalize_inferred_type(g.option_value_type_for_expression(tokens[..tokens.len - 1]))
		expected_key := g.semantic_type_key(flat.source_type_id(expected_type.trim_right('*')))
		boxes := value_type != 0 && expected_type_source_id != 0
			&& (g.should_box_variant(expected_type_source_id, value_type)
				|| g.declared_kinds[flat.source_type_name(expected_key)] == .interface_
				|| fastc_trim_pointer_suffix(expected_type) in g.sum_types)
		if !boxes {
			if propagation := g.render_option_propagation(tokens[..tokens.len - 1]) {
				if value_type != 0 && expected_type.ends_with('*') && !flat.source_type_name(value_type).ends_with('*') {
					// A `&T` parameter given the propagated value: pass the address of
					// stable storage holding it.
					return g.value_address_expression(value_type, propagation.source, tokens)
				}
				return propagation.source
			}
		}
	}
	if tokens.len == 1 && tokens[0].has_source() {
		// A pre-rendered value (e.g. an interpolation) passed where a `voidptr` is expected (a
		// map `.delete(key)` / generic argument) must still be boxed to a pointer, exactly as the
		// non-synthetic path below does; otherwise a struct value is passed where `void*` is wanted.
		// A function value is already a pointer.
		if g.selfhost && expected_type == 'voidptr' && tokens[0].typ_text() !in ['', 'voidptr',
			'nil'] && !fastc_is_pointer_type(g.underlying_alias_type(tokens[0].typ))
			&& !g.is_function_value_type(tokens[0].typ) {
			actual := fastc_normalize_inferred_type(g.underlying_alias_type(tokens[0].typ))
			return '({ ${flat.source_type_name(actual)} __vf_generic_argument = (${tokens[0].source_text()}); v_fastc_interface_box(&__vf_generic_argument, sizeof(${flat.source_type_name(actual)})); })'
		}
		// An if/match-expression argument was pre-rendered to a ternary during streaming with the
		// call-argument expected type cleared, so its branches may still hold raw `.member` enum
		// shorthands (`f(if c { .arrow } else { .dot })`). The parameter enum type is only known
		// here, so resolve them now.
		if g.selfhost && tokens[0].source_text().contains('.') {
			enum_key := g.semantic_type_key(expected_type_source_id)
			if g.declared_kinds[flat.source_type_name(enum_key)] == .enum_ {
				return fastc_resolve_enum_shorthands_in_source(tokens[0].source_text(), fastc_c_declared_type_name(flat.source_type_name(enum_key)), g.enum_field_names[flat.source_type_name(enum_key)])
			}
		}
		// Pre-rendered option fallbacks still need the ordinary implicit borrow
		// when a reference parameter expects their exact payload type.
		actual := fastc_normalize_inferred_type(tokens[0].typ)
		if g.selfhost && expected_type.ends_with('*') && actual != flat.source_type_voidptr
			&& expected_type == flat.source_type_name(actual) + '*' {
			return g.value_address_expression(actual, tokens[0].source_text(), tokens)
		}
		// Other pre-rendered values already have the required representation.
		return tokens[0].source_text()
	}
	if g.selfhost && expected_type.starts_with('FixedArray_') && expected_type.ends_with('*')
		&& tokens.len > 1 && tokens[0].tok == .amp {
		operand := fastc_strip_paren_tokens(tokens[1..])
		if g.fixed_array_uses_raw_storage(operand)
			&& fastc_normalize_inferred_type(g.infer_expression_type(operand) or { u32(0) }) == flat.source_type_id(expected_type.trim_right('*')) {
			if raw := g.render_member_receiver(operand) {
				// The wrapper has the same layout as raw field/global storage. Preserve the
				// caller's address instead of taking the address of a copied wrapper value.
				return '((${expected_type})&(${raw}))'
			}
		}
	}
	if g.selfhost && tokens.len == 2 && tokens[0].tok == .amp && !tokens[0].is_mut_argument && tokens[1].tok == .name {
		if local := g.locals[tokens[1].lit] {
			if local.is_reference {
				return g.resolved_expression_name(tokens[1].lit, .unknown)
			}
		}
	}
	if tokens.len > 1 && tokens[0].tok == .amp && tokens[0].is_mut_argument {
		if g.selfhost && expected_type.ends_with('*')
			&& g.infer_expression_type(tokens[1..]) or { u32(0) } == expected_type_source_id {
			// A pointer supplied to a mutable pointee parameter already refers to
			// the caller's storage; an extra address would refer to the pointer slot.
			return g.render_call_argument_expression(tokens[1..], expected_type_source_id)
		}
		interface_type := expected_type.trim_right('*')
		if g.selfhost && expected_type.ends_with('*')
			&& g.declared_kinds[flat.source_type_name(g.semantic_type_key(flat.source_type_id(interface_type)))] == .interface_ {
			// `f(mut value)` for a `mut x Interface` parameter: the interface refers to the
			// caller's value itself, so the callee's mutations reach it (never a boxed copy).
			actual_type := fastc_normalize_inferred_type(g.infer_expression_type(tokens[1..]) or { u32(0) })
			actual_key := g.semantic_type_key(actual_type)
			if actual_type != u32(0) && !fastc_is_pointer_type(actual_type)
				&& g.declared_kinds[flat.source_type_name(actual_key)] == .struct_ {
				target := g.render_call_argument_expression(tokens[1..], actual_type) or {
					return none
				}
				return '&((${interface_type}){._object=(void*)(&(${target})), ._typ=__v_typeid_${fastc_c_declared_type_name(flat.source_type_name(actual_key))}})'
			}
		}
		if g.selfhost && expected_type == 'voidptr*' {
			actual_type := (g.infer_expression_type(tokens[1..]) or { u32(0) })
			if flat.source_type_name(actual_type) !in ['', 'voidptr'] {
				inner := g.render_call_argument_expression(tokens[1..], actual_type) or {
					return none
				}
				return '((voidptr *)(&(${inner})))'
			}
		}
		inner_expected_type := if expected_type.ends_with('*') {
			expected_type[..expected_type.len - 1]
		} else {
			expected_type
		}
		if view := g.render_fixed_array_mut_argument(tokens[1..], flat.source_type_id(inner_expected_type)) {
			return view
		}
		inner := g.render_call_argument_expression(tokens[1..], flat.source_type_id(inner_expected_type)) or {
			return none
		}
		if fastc_expression_tokens_contain_range(tokens, 1, tokens.len, .dotdot) {
			declaration_type := g.declaration_c_type(flat.source_type_id(inner_expected_type), inner)
			return '({ ${declaration_type} __vf_mut_argument = (${inner}); &__vf_mut_argument; })'
		}
		return '&(${inner})'
	}
	if tokens.len >= 2 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		close := fastc_matching_rpar(tokens, 0) or { -1 }
		if close == tokens.len - 1 {
			inner := g.render_call_argument_expression(tokens[1..tokens.len - 1], expected_type_source_id) or {
				return none
			}
			return '(${inner})'
		}
	}
	if g.selfhost && expected_type in ['Option', '__v_result'] {
		if tokens.len == 1 && tokens[0].tok == .key_none {
			return '(Option){.state=2}'
		}
		actual_type := fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { u32(0) })
		if flat.source_type_name(actual_type) !in ['', 'Option', '__v_result'] {
			value := g.render_call_argument_expression(tokens, actual_type) or { return none }
			return g.option_success_expression(expected_type_source_id, actual_type, value)
		}
	}
	if g.selfhost && expected_type == 'voidptr' && tokens.len > 1 && tokens[0].tok == .mul {
		operand_type := (g.infer_expression_type(tokens[1..]) or { u32(0) })
		if operand_type == flat.source_type_voidptr {
			// A generic pointer type is erased to `voidptr`; C cannot dereference it
			// without the missing concrete type. At another erased boundary, forward
			// that pointer representation directly.
			return g.render_call_argument_expression(tokens[1..], expected_type_source_id)
		}
	}
	if g.selfhost && tokens.len >= 5 && tokens[0].tok == .lsbr && tokens.last().tok == .rcbr {
		if fixed := g.render_fixed_array_value(tokens) {
			return fixed
		}
	}
	if g.selfhost && expected_type.starts_with('FixedArray_') && g.fixed_array_uses_raw_storage(tokens)
		&& flat.source_type_name(fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { u32(0) })) == expected_type.trim_right('*') {
		// A struct field or global stores a fixed array as a raw C array; a parameter takes
		// the wrapper struct of the same layout.
		if raw := g.render_member_receiver(tokens) {
			if expected_type.ends_with('*') {
				return '((${expected_type})&(${raw}))'
			}
			return '(*(${expected_type} *)(${raw}))'
		}
	}
	if array_literal := g.render_array_literal_argument(tokens, expected_type_source_id) {
		return array_literal.source
	}
	if map_literal := g.render_map_literal_argument(tokens, expected_type_source_id) {
		return map_literal.source
	}
	if tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name && g.declared_kinds[flat.source_type_name(g.semantic_type_key(expected_type_source_id))] == .enum_ {
		return '${expected_type.trim_right('*')}__${tokens[1].lit}'
	}
	if g.declared_kinds[flat.source_type_name(g.semantic_type_key(expected_type_source_id))] == .enum_ {
		if flag_combo := g.render_enum_flag_shorthand_combination(tokens, expected_type_source_id) {
			return flag_combo
		}
		// A flag expression mixing a value with a `.member` shorthand (`~Show.zero() ^ .name`):
		// render it normally, then rewrite the value-position shorthands against the enum type —
		// render_enum_flag_shorthand_combination handles only a pure `.a | .b` chain.
		mut has_flag_op := false
		for flag_tok in tokens {
			if flag_tok.tok in [.xor, .pipe, .amp] {
				has_flag_op = true
				break
			}
		}
		if has_flag_op && fastc_expression_tokens_contain(tokens, .dot) {
			// enum_field_names is keyed by the C type name (`flag__Show`), not the semantic key.
			enum_c := fastc_c_declared_type_name(flat.source_type_name(g.semantic_type_key(expected_type_source_id)))
			raw_flag := g.render_raw_expression_tokens(tokens) or { return none }
			rendered_flag := if special := g.render_special_expression(tokens, raw_flag) {
				special.source
			} else {
				raw_flag
			}
			resolved_flag := fastc_resolve_enum_shorthands_in_source(rendered_flag, enum_c, g.enum_field_names[enum_c])
			return g.fastc_resolve_flag_enum_statics(resolved_flag, enum_c)
		}
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
			if local.smartcast_origin_type != u32(0) && local.smartcast_origin_source != '' && expected_type != '' {
				origin_type := fastc_normalize_inferred_type(local.smartcast_origin_type)
				if expected_type == flat.source_type_name(origin_type) {
					return local.smartcast_origin_source
				}
				if expected_type.ends_with('*') && flat.source_type_name(origin_type) == expected_type.trim_right('*') {
					return '&(${local.smartcast_origin_source})'
				}
				if flat.source_type_name(origin_type).ends_with('*') && expected_type == flat.source_type_name(origin_type).trim_right('*') {
					return '*(${local.smartcast_origin_source})'
				}
			}
			if local.is_reference {
				if g.should_box_variant(expected_type_source_id, local.typ)
					|| (expected_type.ends_with('*')
						&& g.should_box_variant(flat.source_type_id(expected_type.trim_right('*')), local.typ)) {
					// A `mut value T` parameter is a C `T*`. Interface conversion must
					// preserve that pointer as the boxed object instead of auto-dereferencing
					// it to the value used by ordinary by-value arguments.
					return g.interface_value_expression(expected_type_source_id, local.typ, raw)
				}
				storage_type := local.typ
				if expected_type_source_id == storage_type {
					return raw
				}
				value_type := if flat.source_type_name(storage_type).ends_with('*') {
					flat.source_type_id(flat.source_type_name(storage_type)[..flat.source_type_name(storage_type).len - 1])
				} else {
					storage_type
				}
				if expected_type_source_id != 0 && (expected_type_source_id == value_type || (!fastc_is_pointer_type(expected_type_source_id) && fastc_selfhost_types_share_lowering_representation(value_type, expected_type_source_id))) {
					return '*(${raw})'
				}
				if fastc_is_pointer_type(expected_type_source_id) {
					return raw
				}
			} else if tokens[0].is_mut_argument && expected_type == flat.source_type_name(local.typ) + '*' {
				return '&(${raw})'
			}
		}
	}
	mut rendered := ''
	mut rendered_type := ''
	// A member/local smart-cast chain (`left` narrowed by `left is CS`) must read through the
	// concrete-variant pointer, not the boxed subject's raw spelling — otherwise a later box
	// (`Expr(left)`) stores the wrong object. Mirror render_comparison_operand.
	if g.selfhost && g.expression_uses_member_smartcast(tokens) {
		if member_source := g.render_member_receiver(tokens) {
			rendered = member_source
			rendered_type = flat.source_type_name(fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { u32(0) }))
		}
	}
	if rendered == '' {
		flags := fastc_special_token_flags(tokens)
		if special := g.render_special_expression_with_flags(tokens, raw, flags) {
			rendered = special.source
			rendered_type = flat.source_type_name(special.typ)
		} else if g.can_reuse_special_expression_miss(flags) {
			rendered = g.render_membership_candidate_with_raw(tokens, expected_type_source_id, raw, true) or { return none }
		} else {
			rendered = g.render_membership_candidate(tokens, expected_type_source_id) or { return none }
		}
	}
	rendered = g.render_constant_references(tokens, rendered)
	actual_type := if rendered_type != '' {
		flat.source_type_name(fastc_normalize_inferred_type(flat.source_type_id(rendered_type)))
	} else {
		flat.source_type_name(fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { u32(0) }))
	}
	// A function value is already a pointer.
	if expected_type == 'voidptr' && actual_type !in ['', 'voidptr', 'nil'] && !fastc_expression_is_zero(tokens) && !fastc_is_pointer_type(g.underlying_alias_type(flat.source_type_id(actual_type)))
		&& !g.is_function_value_type(flat.source_type_id(actual_type)) {
		box_value := '__vf_generic_argument'
		return '({ ${actual_type} ${box_value} = (${rendered}); v_fastc_interface_box(&${box_value}, sizeof(${actual_type})); })'
	}
	if actual_type == 'voidptr' && !fastc_is_pointer_type(g.underlying_alias_type(expected_type_source_id))
		&& (expected_type.trim_right('*') in g.struct_fields || expected_type.trim_right('*').starts_with('Array_') || expected_type.trim_right('*').starts_with('Map_') || expected_type.trim_right('*').starts_with('FixedArray_')) {
		return '*((${expected_type} *)(${rendered}))'
	}
	if expected_type == 'string' && fastc_trim_pointer_suffix(actual_type) == 'IError' {
		return 'builtin__IError_msg(${rendered})'
	}
	if g.selfhost && actual_type.ends_with('*') && expected_type == actual_type.trim_right('*') && g.expression_uses_member_smartcast(tokens) {
		// A member smart-cast (`node.args[0].expr is CallExpr`) supplies a variant POINTER; a
		// by-value parameter (`resolve_return_type(node.args[0].expr)`) needs the pointee value.
		// The `.lsbr` guard below would wrongly skip this because the chain indexes an array.
		return '*(${rendered})'
	}
	if actual_type.ends_with('*') && expected_type == actual_type.trim_right('*') && expected_type != 'voidptr' && !fastc_expression_tokens_contain(tokens, .lsbr) {
		// V automatically dereferences an explicit reference when a by-value
		// parameter is expected (`s &string` passed to `log_line(s string)`). A
		// slice of a mutable array is already a by-value array even though the
		// receiver local retains its pointer type during inference.
		return '*(${rendered})'
	}
	if expected_type.ends_with('*') && actual_type == 'voidptr' && rendered.trim_space() == 'NULL' {
		return 'NULL'
	}
	if expected_type.ends_with('*') && actual_type == 'voidptr' {
		// An erased pointer is already an address, including an explicit cast
		// in a C call. Taking its address would produce an invalid pointer rvalue.
		return '((${expected_type})(${rendered}))'
	}
	if expected_type.ends_with('*') && fastc_expression_is_zero(tokens) {
		return 'NULL'
	}
	// A `voidptr` converts to any pointer type as it is (`C.backtrace_symbols(voidptr(p), n)`
	// for an `&voidptr` parameter); taking its address would pass a different pointer.
	if expected_type.ends_with('*') && actual_type == expected_type.trim_right('*')
		&& actual_type != 'voidptr' {
		return g.value_address_expression(flat.source_type_id(actual_type), rendered, tokens)
	}
	if expected_type.ends_with('*') && g.should_box_variant(flat.source_type_id(expected_type.trim_right('*')), flat.source_type_id(actual_type)) {
		// A concrete value passed where a `&Interface` parameter is expected (`f(resolver
		// &IResolver)` called with a `Gen`): box it into an interface value with heap lifetime.
		// The pointer can escape through an assignment (`p := &Interface(value)`), so the
		// address of a statement-expression local would immediately dangle.
		iface_type := expected_type.trim_right('*')
		boxed := g.interface_value_expression(flat.source_type_id(iface_type), flat.source_type_id(actual_type), rendered)
		return '({ ${iface_type} __vf_iface_ref = ${boxed}; (${iface_type} *)v_fastc_interface_box(&__vf_iface_ref, sizeof(${iface_type})); })'
	}
	if g.should_box_variant(expected_type_source_id, flat.source_type_id(actual_type)) {
		return g.interface_value_expression(expected_type_source_id, flat.source_type_id(actual_type), rendered)
	}
	return rendered
}

fn (g &Parser) render_array_literal_argument(tokens []FastcExpressionToken, expected_type_source_id u32) ?FastcRenderedExpression {
	expected_type := flat.source_type_name(expected_type_source_id)
	array_type := fastc_trim_pointer_suffix(expected_type)
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
	element_type := g.array_element_type(flat.source_type_id(array_type)) or { return none }
	items := fastc_expression_list_items(tokens, 1, close) or { return none }
	if items.len == 0 {
		if is_fixed {
			return FastcRenderedExpression{
				source: '(${array_type}){0}'
				typ:    flat.source_type_id(array_type)
			}
		}
		// A dynamic empty array needs a real header carrying `element_size`, otherwise a later
		// `<<` push copies `len * 0` bytes into a NULL buffer and silently drops the elements.
		normalized_empty_element := g.collection_storage_type(fastc_normalize_inferred_type(element_type))
		return FastcRenderedExpression{
			source: '((${array_type})builtin____new_array(0, 0, sizeof(${normalized_empty_element})))'
			typ:    flat.source_type_id(array_type)
		}
	}
	mut rendered_items := []string{cap: items.len}
	if !is_fixed && items.any(it.len > 0 && it[0].tok == .ellipsis) {
		normalized_element := g.collection_storage_type(fastc_normalize_inferred_type(element_type))
		mut statements := ['${array_type} __vf_spread_array = (${array_type})builtin____new_array(0, 0, sizeof(${normalized_element}));']
		for i, item in items {
			if item.len > 1 && item[0].tok == .ellipsis {
				spread := g.render_call_argument_expression(item[1..], flat.source_type_id(array_type)) or { return none }
				statements << '${array_type} __vf_spread_${i} = (${spread}); builtin__array_push_many((array *)&__vf_spread_array, __vf_spread_${i}.data, __vf_spread_${i}.len);'
			} else {
				value := g.render_call_argument_expression(item, element_type) or { return none }
				statements << '${normalized_element} __vf_spread_${i} = (${value}); builtin__array_push((array *)&__vf_spread_array, &__vf_spread_${i});'
			}
		}
		return FastcRenderedExpression{
			source: '({ ${statements.join(' ')} __vf_spread_array; })'
			typ:    flat.source_type_id(array_type)
		}
	}
	for item in items {
		if simple := g.render_selfhost_simple_array_literal_item(item, element_type) {
			rendered_items << simple
		} else {
			rendered_items << g.render_call_argument_expression(item, element_type) or { return none }
		}
	}
	normalized_element := g.collection_storage_type(fastc_normalize_inferred_type(element_type))
	if is_fixed {
		c_array_type := fastc_array_initializer_c_type(array_type)
		w.fixed_array_types.set(c_array_type, array_type)
		return FastcRenderedExpression{
			source: '((${c_array_type}){.data={${rendered_items.join(',')}}})'
			typ:    flat.source_type_id(array_type)
		}
	}
	return FastcRenderedExpression{
		source: g.dynamic_array_literal_source(array_type, normalized_element, rendered_items)
		typ:    flat.source_type_id(array_type)
	}
}

fn (g &Parser) render_selfhost_simple_array_literal_item(tokens []FastcExpressionToken, expected_type_source_id u32) ?string {
	expected_type := flat.source_type_name(expected_type_source_id)
	if !g.selfhost || expected_type in ['Option', '__v_result', 'voidptr'] {
		return none
	}
	if tokens.len == 4 && tokens[0].tok == .name && tokens[1].tok == .lpar && tokens[3].tok == .rpar && !tokens[2].has_source() {
		cast_type := fastc_primitive_c_type(tokens[0].lit) or { return none }
		if cast_type != expected_type.trim_right('*') || expected_type.ends_with('*') {
			return none
		}
		inner := g.render_selfhost_simple_array_literal_item(tokens[2..3], flat.source_type_id(cast_type)) or {
			return none
		}
		return '((${fastc_output_c_type(cast_type)})(${inner}))'
	}
	if tokens.len != 1 || tokens[0].has_source() {
		return none
	}
	item := tokens[0]
	return match item.tok {
		.number {
			if expected_type.ends_with('*') {
				if item.lit == '0' {
					'NULL'
				} else {
					return none
				}
			} else {
				fastc_c_selfhost_number(item.lit)
			}
		}
		.string {
			literal := fastc_c_string(item.lit) or { return none }
			'_S(${literal})'
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
		else {
			return none
		}
	}
}

fn (g &Parser) render_function_value_expression(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens.last().tok != .name {
		return none
	}
	if tokens.len == 1 && tokens[0].lit in g.locals {
		return none
	}
	if tokens.len != 1 && !(tokens.len == 3 && tokens[0].tok == .name && tokens[1].tok == .dot && (tokens[0].lit in g.imports || tokens[0].lit == 'C')) {
		return none
	}
	function_key := g.function_key_for_call(tokens, tokens.len - 1)
	if function_key !in g.functions && function_key !in g.mono_functions {
		return none
	}
	if tokens.len == 1 && !function_key.contains('.')
		&& (tokens[0].lit in g.constants || tokens[0].lit in g.globals) {
		// A main-module constant or global that shares its name with a function is
		// the value of that name outside a call.
		return none
	}
	return '&${g.c_function_name_for_key(function_key)}'
}

fn (g &Parser) render_method_value_expression(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 || tokens.last().tok != .name || tokens[tokens.len - 2].tok != .dot {
		return none
	}
	// Only the complete expression may be a method value. Without this boundary check,
	// `a.len == b.len && a.str == b.str` is mistaken for a method value on the boolean
	// expression ending at `b`, and `.str` resolves to `bool.str`.
	if fastc_method_receiver_start(tokens, tokens.len - 2) != 0 {
		return none
	}
	receiver_tokens := tokens[..tokens.len - 2]
	receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
	if g.struct_member_type(receiver_type, tokens.last().lit) != u32(0) {
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
	if signature.parameter_types.ids.len == 0 {
		return none
	}
	method_c_name := g.c_method_name(method_key, signature.module_name, signature.parameter_types.at(0), tokens.last().lit)
	return '&${method_c_name}'
}

fn (g &Parser) render_map_literal_argument(tokens []FastcExpressionToken, expected_type_source_id u32) ?FastcRenderedExpression {
	if contextual := g.render_contextual_empty_map_initializer(tokens, expected_type_source_id) {
		return contextual
	}
	expected_type := flat.source_type_name(expected_type_source_id)
	map_type := fastc_trim_pointer_suffix(expected_type)
	key_type_source_id, value_type_source_id := g.map_key_value_types(flat.source_type_id(map_type)) or { return none }
	key_type := key_type_source_id
	value_type := value_type_source_id

	if tokens.len < 2 || tokens[0].tok != .lcbr || tokens.last().tok != .rcbr {
		return none
	}
	close := fastc_matching_delimiter(tokens, 0, .lcbr, .rcbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	entries := fastc_map_literal_entries(tokens, 1, close) or { return none }
	// The literal's type can be inferred (`println({'k': 1})`), so no declaration
	// registers its typedef.
	mut w := unsafe { &Parser(g) }
	fastc_register_composite_type(map_type, mut w.composite_types)
	hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
	mut statements := [
		'map __vf_argument_map = builtin__new_map(sizeof(${g.collection_runtime_type(key_type)}), sizeof(${g.collection_runtime_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn});',
	]
	for entry_index, entry in entries {
		colon := fastc_map_literal_entry_colon(entry)
		if colon <= 0 || colon + 1 >= entry.len {
			return none
		}
		key := g.render_call_argument_expression(entry[..colon], key_type) or { return none }
		value := g.render_call_argument_expression(entry[colon + 1..], value_type) or {
			return none
		}
		key_name := '__vf_argument_map_key_${entry_index}'
		value_name := '__vf_argument_map_value_${entry_index}'
		statements << '${g.collection_runtime_type(key_type)} ${key_name} = (${key});'
		statements << '${g.collection_runtime_type(value_type)} ${value_name} = (${value});'
		statements << 'builtin__map_set(&__vf_argument_map, &${key_name}, &${value_name});'
	}
	return FastcRenderedExpression{
		source: '({ ${statements.join(' ')} __vf_argument_map; })'
		typ:    flat.source_type_id(map_type)
	}
}

// fastc_map_literal_entry_colon returns the index of the `:` separating a map literal
// entry's key from its value, or -1.
fn fastc_map_literal_entry_colon(entry []FastcExpressionToken) int {
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
					return i
				}
			}
			else {}
		}
	}
	return -1
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

// is_boxed_type reports whether a type uses the boxed `{_object, _typ}`
// representation: interfaces and sum types. A concrete struct used where such a
// type is expected is boxed with its type id (see interface_value_expression).
fn (g &Parser) is_boxed_type(c_type u32) bool {
	if fastc_trim_pointer_suffix(flat.source_type_name(c_type)) in g.sum_types { return true }
	return g.declared_kinds[flat.source_type_name(g.semantic_type_key(c_type))] == .interface_
}

// sumtype_has_variant reports whether `sum_type` is a sum type that declares
// `variant` (a normalized C type name) as one of its variants. It mirrors the main
// C backend's `sumtype_has_variant` and is used to keep an array-valued variant of
// a recursive sum type (`type Value = []Value | int`) boxed as a single element
// rather than treated as a push-many append.
fn (g &Parser) sumtype_has_variant(sum_type u32, variant u32) bool {
	base := fastc_trim_pointer_suffix(flat.source_type_name(sum_type))
	if base !in g.sum_types { return false }
	return g.sum_type_variants['${base}|${flat.source_type_name(variant).trim_right('*')}']
}

// should_box_variant reports whether a value of `actual_type` used where the
// boxed `expected_type` is expected must be boxed: a concrete struct into an
// interface or sum type, or a primitive scalar into a sum type (interfaces have
// no primitive implementers, so primitives are only boxed for sum types).
fn (g &Parser) should_box_variant(expected_type u32, actual_type u32) bool {
	if actual_type == 0 || fastc_is_pointer_type(expected_type) || !g.is_boxed_type(expected_type) {
		return false
	}
	resolved_actual := g.underlying_alias_type(actual_type)
	if g.declared_kinds[flat.source_type_name(g.semantic_type_key(resolved_actual))] == .struct_ {
		return true
	}
	if fastc_trim_pointer_suffix(flat.source_type_name(expected_type)) in g.sum_types {
		// A primitive or composite (`Array_`/`Map_`) value, or a nested sum-type variant that
		// is itself a sum type (`Stmt` passed where `Node` is expected), boxed into a sum type.
		// Interfaces have no primitive/composite implementers, so this is sum-type only.
		normalized := fastc_trim_pointer_suffix(flat.source_type_name(fastc_normalize_inferred_type(actual_type)))
		return fastc_primitive_c_type(normalized) != none || normalized.starts_with('Array_') || normalized.starts_with('Map_') || g.sumtype_has_variant(expected_type, flat.source_type_id(normalized))
	}
	return false
}

// render_map_index_inc_dec_expression lowers `m[k]++` / `m[k]--`. A map value is not a C
// lvalue, so the entry is fetched (inserting a zero default when absent, matching V) and the
// increment applied through the returned pointer.
fn (g &Parser) render_map_index_inc_dec_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 5 || tokens.last().tok !in [.inc, .dec] {
		return none
	}
	op := if tokens.last().tok == .inc { '++' } else { '--' }
	index_tokens := tokens[..tokens.len - 1]
	if index_tokens.last().tok != .rsbr {
		return none
	}
	close := index_tokens.len - 1
	mut open := -1
	mut depth := 0
	for i := close; i >= 0; i-- {
		if index_tokens[i].tok == .rsbr {
			depth++
		} else if index_tokens[i].tok == .lsbr {
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
	base_tokens := index_tokens[..open]
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
	key_type := key_type_source_id
	value_type := value_type_source_id

	key_source := g.render_call_argument_expression(index_tokens[open + 1..close], key_type) or {
		return none
	}
	mut map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
		g.resolved_root_expression_name(base_tokens[0].lit)
	} else {
		g.render_member_receiver(base_tokens) or { return none }
	}
	if flat.source_type_name(map_type).ends_with('*') {
		map_source = '*(${map_source})'
	}
	key_storage_type := g.collection_runtime_type(key_type)
	value_storage_type := g.collection_storage_type(value_type)
	return FastcRenderedExpression{
		source: '({ ${key_storage_type} __vf_inc_key = (${key_source}); ${value_storage_type} *__vf_inc_value = (${value_storage_type} *)builtin__map_get_check((map *)&(${map_source}), &__vf_inc_key); if (__vf_inc_value == NULL) { ${value_storage_type} __vf_inc_zero = (${value_storage_type}){0}; builtin__map_set((map *)&(${map_source}), &__vf_inc_key, &__vf_inc_zero); __vf_inc_value = (${value_storage_type} *)builtin__map_get_check((map *)&(${map_source}), &__vf_inc_key); } (*__vf_inc_value)${op}; })'
		typ:    value_type
	}
}

fn (g &Parser) render_map_lookup_option_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	// Strip leading `(` that have no matching `)` within these tokens: a grouped `or`-operand
	// (`(m[k]` — the closing `)` sits after the `or { … }`) carries such an unmatched paren,
	// while a deref base (`(*g.m)[k]`) keeps its balanced `(...)` for the base logic below.
	mut start := 0
	for start < tokens.len && tokens[start].tok == .lpar {
		mut paren_depth := 0
		mut matched := false
		for i := start; i < tokens.len; i++ {
			if tokens[i].tok == .lpar {
				paren_depth++
			} else if tokens[i].tok == .rpar {
				paren_depth--
				if paren_depth == 0 {
					matched = true
					break
				}
			}
		}
		if matched {
			break
		}
		start++
	}
	// Only the slice header changes; the renderers read these caller-owned tokens.
	mut lookup_tokens := unsafe { tokens[start..] }
	address_of_value := lookup_tokens.len > 1 && lookup_tokens[0].tok in [.amp, .and] && fastc_token_is_prefix_operator(lookup_tokens, 0)
	if address_of_value {
		lookup_tokens = unsafe { lookup_tokens[1..] }
	}
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
	base_tokens := fastc_strip_paren_tokens(lookup_tokens[..open])
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
	key_type := key_type_source_id
	value_type := value_type_source_id

	// A nested-composite value type (`map[string]map[int][][]T` yields `Map_int_Array_Array_T`)
	// is spelled in the lookup but is otherwise unreferenced, so register it here to emit its
	// typedef.
	mut wc := unsafe { &Parser(g) }
	fastc_register_composite_type(flat.source_type_name(value_type), mut wc.composite_types)
	fastc_register_composite_type(flat.source_type_name(key_type), mut wc.composite_types)
	mut map_source := ''
	mut needs_receiver_temp := false
	if base_tokens.len == 1 && base_tokens[0].tok == .name {
		map_source = g.resolved_root_expression_name(base_tokens[0].lit)
	} else if base_tokens.len >= 2 && base_tokens[0].tok == .mul {
		// `(*g.m)[k]` reads through a pointer-to-map; render the pointee map value so the
		// `&(map)` the lookup takes below resolves back to the original pointer.
		inner := g.render_member_receiver(base_tokens[1..]) or {
			g.render_raw_expression_tokens(base_tokens[1..]) or { return none }
		}
		map_source = '*(${inner})'
	} else if base_tokens.len > 0 && base_tokens.last().tok == .rsbr && g.render_map_expression(base_tokens) != none {
		// A nested map read (`m[k1][k2]`): the base is itself a map lookup, an rvalue, so `&`
		// it through a temp rather than treating it as a member chain / raw C index.
		map_base := g.render_map_expression(base_tokens) or { return none }
		map_source = map_base.source
		needs_receiver_temp = true
	} else if member := g.render_member_receiver(base_tokens) {
		map_source = member
	} else if call := g.render_missing_call_arguments(base_tokens) {
		// A map produced by a call (`os.environ()[k]`) is an rvalue, so `&` needs a temp.
		map_source = call.source
		needs_receiver_temp = true
	} else if method_base := g.render_method_call_expression(base_tokens, g.render_raw_expression_tokens(base_tokens) or {
		''
	}) {
		// A map produced by a method call (`t.get_map()[k]`) is an rvalue too, so `&` needs a temp.
		map_source = method_base.source
		needs_receiver_temp = true
	} else {
		return none
	}
	if flat.source_type_name(map_type).ends_with('*') {
		map_source = '*(${map_source})'
	}
	key_source := g.render_membership_candidate(lookup_tokens[open + 1..lookup_tokens.len - 1], key_type) or { return none }
	option_value_type := if address_of_value {
		flat.source_type_id('${flat.source_type_name(value_type)}*')
	} else {
		value_type
	}
	option_result := if address_of_value {
		'__vf_mv == NULL ? (Option){.state=2} : ${g.option_success_expression(fastc_type_option, option_value_type, '__vf_mv')}'
	} else {
		'(Option){.data=__vf_mv, .state=__vf_mv == NULL ? 2 : 0}'
	}
	if needs_receiver_temp {
		return FastcRenderedExpression{
			source: '({ ${flat.source_type_name(map_type).trim_right('*')} __vf_map_receiver = (${map_source}); ${g.collection_storage_type(key_type)} __vf_k = (${key_source}); ${g.collection_storage_type(value_type)} *__vf_mv = (${g.collection_storage_type(value_type)} *)builtin__map_get_check((map *)&(__vf_map_receiver), &__vf_k); ${option_result}; })'
			typ:    option_value_type
		}
	}
	return FastcRenderedExpression{
		source: '({ ${g.collection_storage_type(key_type)} __vf_k = (${key_source}); ${g.collection_storage_type(value_type)} *__vf_mv = (${g.collection_storage_type(value_type)} *)builtin__map_get_check((map *)&(${map_source}), &__vf_k); ${option_result}; })'
		typ:    option_value_type
	}
}

// render_slice_option_expression lowers a bounds-checked slice used with `or` (`s[a..b] or
// {…}`) into the Option form: the substring/subarray on success, none when a or b is out of
// range. Handles string and dynamic-array receivers.
fn (g &Parser) render_slice_option_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut lead := 0
	for lead < tokens.len && tokens[lead].tok == .lpar {
		lead++
	}
	slice_tokens := tokens[lead..]
	if slice_tokens.len < 5 || slice_tokens.last().tok != .rsbr {
		return none
	}
	close := slice_tokens.len - 1
	mut open := -1
	mut depth := 0
	for i := close; i >= 0; i-- {
		if slice_tokens[i].tok == .rsbr {
			depth++
		} else if slice_tokens[i].tok == .lsbr {
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
	mut range_index := -1
	mut range_depth := 0
	for i := open + 1; i < close; i++ {
		match slice_tokens[i].tok {
			.lpar, .lsbr, .lcbr { range_depth++ }
			.rpar, .rsbr, .rcbr { range_depth-- }
			.dotdot {
				if range_depth == 0 {
					range_index = i
					break
				}
			}
			else {}
		}
	}
	if range_index < 0 {
		return none
	}
	base_tokens := slice_tokens[..open]
	base_type := g.infer_expression_type(base_tokens) or { return none }
	base_layout := g.underlying_alias_type(base_type)
	is_string := base_layout == flat.source_type_string
	is_array := fastc_trim_pointer_suffix(flat.source_type_name(base_type)).starts_with('Array_')
	if !is_string && !is_array {
		return none
	}
	base_source := g.render_call_argument_expression(base_tokens, base_type) or { return none }
	receiver_source := if flat.source_type_name(base_type).ends_with('*') {
		'*(${base_source})'
	} else {
		base_source
	}
	value_type := if is_string {
		flat.source_type_string
	} else {
		flat.source_type_id(fastc_trim_pointer_suffix(flat.source_type_name(base_type)))
	}
	low := if range_index == open + 1 {
		'0'
	} else {
		g.render_membership_candidate(slice_tokens[open + 1..range_index], flat.source_type_id('int')) or { return none }
	}
	high := if range_index + 1 == close {
		'__vf_slice_receiver.len'
	} else {
		g.render_membership_candidate(slice_tokens[range_index + 1..close], flat.source_type_id('int')) or { return none }
	}
	slice_value := if is_string {
		'builtin__string_substr(__vf_slice_receiver, __vf_slice_low, __vf_slice_high)'
	} else {
		'builtin__array_slice(__vf_slice_receiver, __vf_slice_low, __vf_slice_high)'
	}
	result := if g.prefs.target_libc_headers {
		'__vf_slice_ok ? ${g.option_success_expression(fastc_type_option, value_type, '__vf_slice_value')} : (Option){.state=2}'
	} else {
		'(Option){.data=__vf_slice_ok ? v_fastc_interface_box(&__vf_slice_value, sizeof(${flat.source_type_name(value_type)})) : NULL, .state=__vf_slice_ok ? 0 : 2}'
	}
	return FastcRenderedExpression{
		source: '({ ${flat.source_type_name(value_type)} __vf_slice_receiver = (${receiver_source}); int __vf_slice_low = (${low}); int __vf_slice_high = (${high}); bool __vf_slice_ok = __vf_slice_low >= 0 && __vf_slice_low <= __vf_slice_high && __vf_slice_high <= __vf_slice_receiver.len; ${flat.source_type_name(value_type)} __vf_slice_value = __vf_slice_ok ? (${slice_value}) : (${flat.source_type_name(value_type)}){0}; ${result}; })'
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
	base_layout := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(base_type)))
	base_source := g.render_call_argument_expression(base_tokens, base_type) or { return none }
	index_source := g.render_membership_candidate(lookup_tokens[open + 1..lookup_tokens.len - 1], flat.source_type_id('int')) or { return none }
	if base_layout == 'string' {
		// `s[i] or { … }`: a byte index that is bounds-checked to a `?u8` (V's
		// `at_with_check`), read from the `string`'s `.str`/`.len` payload.
		string_source := if flat.source_type_name(base_type).ends_with('*') {
			'*(${base_source})'
		} else {
			base_source
		}
		return FastcRenderedExpression{
			source: '({ int __vf_str_index = (${index_source}); string __vf_str = (${string_source}); bool __vf_str_missing = __vf_str_index < 0 || __vf_str_index >= __vf_str.len; u8 *__vf_str_value = __vf_str_missing ? NULL : (u8 *)(__vf_str.str + __vf_str_index); (Option){.data=__vf_str_value, .state=__vf_str_missing ? 2 : 0}; })'
			typ:    flat.source_type_u8
		}
	}
	if !base_layout.starts_with('Array_') {
		return none
	}
	element_type := g.array_element_type(base_type) or { return none }
	array_source := if flat.source_type_name(base_type).ends_with('*') {
		'*(${base_source})'
	} else {
		base_source
	}
	return FastcRenderedExpression{
		source: '({ ${fastc_output_c_type('int')} __vf_array_index = (${index_source}); ${flat.source_type_name(base_type).trim_right('*')} __vf_array = (${array_source}); bool __vf_array_missing = __vf_array_index < 0 || __vf_array_index >= __vf_array.len; ${g.collection_storage_type(element_type)} *__vf_array_value = __vf_array_missing ? NULL : (${g.collection_storage_type(element_type)} *)((byteptr)__vf_array.data + (usize)__vf_array_index * (usize)__vf_array.element_size); (Option){.data=__vf_array_value, .state=__vf_array_missing ? 2 : 0}; })'
		typ:    element_type
	}
}

fn fastc_map_runtime_functions(key_type_source_id u32, pointer_bits int) (string, string, string, string) {
	key_type := flat.source_type_name(key_type_source_id)
	if key_type == 'string' {
		return 'builtin__map_hash_string', 'builtin__map_eq_string', 'builtin__map_clone_string', 'builtin__map_free_string'
	}
	suffix := if key_type in ['i8', 'u8', 'char', 'bool'] {
		'1'
	} else if key_type in ['i16', 'u16'] {
		'2'
	} else if key_type in ['i64', 'u64', 'f64'] {
		'8'
	} else if key_type in ['i128', 'u128'] {
		'16'
	} else if key_type in ['isize', 'uint', 'usize'] || fastc_is_pointer_type(key_type_source_id) {
		if pointer_bits == 32 { '4' } else { '8' }
	} else {
		'4'
	}
	return 'builtin__map_hash_int_${suffix}', 'builtin__map_eq_int_${suffix}', 'builtin__map_clone_int_${suffix}', 'builtin__map_free_nop'
}

fn (g &Parser) map_runtime_functions(key_type_source_id u32) (string, string, string, string) {
	key_type := flat.source_type_name(key_type_source_id)
	mut resolved_type := g.underlying_alias_type(key_type_source_id)
	if enum_key := g.underlying_enum_type_key(flat.source_type_name(g.semantic_type_key(resolved_type))) {
		resolved_type = flat.source_type_id(if g.enum_flags[enum_key] { 'u64' } else { 'int' })
	} else if key_type == 'int' {
		// Only bare int is emitted at platform width. Enum and alias typedefs
		// retain their declared C storage width, which the key callbacks must use.
		resolved_type = flat.source_type_id(fastc_platform_int_c_type)
	}
	return fastc_map_runtime_functions(resolved_type, g.prefs.target.pointer_bits)
}

// map_lookup_missing_value_expression gives collections usable empty runtime headers and uses
// the ordinary C zero value for every other lookup value type.
fn (g &Parser) map_lookup_missing_value_expression(value_type_source_id u32) string {
	value_type := flat.source_type_name(value_type_source_id)
	zero := '(${value_type}){0}'
	normalized := fastc_normalize_inferred_type(value_type_source_id)
	if flat.source_type_name(normalized).ends_with('*') {
		return zero
	}
	map_type := g.underlying_alias_type(normalized)
	if flat.source_type_name(map_type).ends_with('*') {
		return zero
	}
	if flat.source_type_name(map_type).starts_with('Array_') {
		element_type := g.array_element_type(map_type) or { return zero }
		return '(${value_type})builtin____new_array(0, 0, sizeof(${g.collection_storage_type(element_type)}))'
	}
	key_type_source_id, nested_value_type_source_id := g.map_key_value_types(map_type) or { return zero }
	key_type := key_type_source_id
	nested_value_type := nested_value_type_source_id

	hash_fn, eq_fn, clone_fn, free_fn := g.map_runtime_functions(key_type)
	return '(${value_type})builtin__new_map(sizeof(${g.collection_runtime_type(key_type)}), sizeof(${g.collection_runtime_type(nested_value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn})'
}

fn (g &Parser) render_explicit_generic_call_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if g.fastc_element_call_open(tokens) > 0 {
		return none
	}
	mut normalized := []FastcExpressionToken{}
	mut changed := false
	mut i := 0
	for i < tokens.len {
		if i > 0 && tokens[i].tok == .lsbr && tokens[i - 1].tok == .name
			&& (tokens[i - 1].lit.contains('_mono_') || tokens[i - 1].lit in g.generic_method_names) {
			if close := fastc_matching_delimiter(tokens, i, .lsbr, .rsbr) {
				mut valid_types := false
				if type_arguments := fastc_call_arguments(tokens, i, close) {
					valid_types = type_arguments.len > 0
					for argument in type_arguments {
						if g.type_from_expression_tokens(argument) == none {
							valid_types = false
							break
						}
					}
				}
				if close + 1 < tokens.len && tokens[close + 1].tok == .lpar && valid_types {
					if !changed {
						normalized = []FastcExpressionToken{cap: tokens.len}
						normalized << tokens[..i]
					}
					i = close + 1
					changed = true
					continue
				}
			}
		}
		if changed {
			normalized << tokens[i]
		}
		i++
	}
	if !changed {
		return none
	}
	raw := g.render_raw_expression_tokens(normalized) or { return none }
	if special := g.render_special_expression(normalized, raw) {
		return special
	}
	if method := g.render_method_call_expression(normalized, raw) {
		return method
	}
	if call := g.render_missing_call_arguments(normalized) {
		return call
	}
	return FastcRenderedExpression{
		source: raw
		typ:    g.infer_expression_type(normalized) or { u32(0) }
	}
}

// append_value_borrows_storage mirrors the C backend's addressable append operands.
// Calls transfer their returned string, while local, field, and element reads need a copy.
fn (g &Parser) append_value_borrows_storage(expression []FastcExpressionToken, indexed bool) bool {
	tokens := fastc_mono_argument_value_tokens(expression)
	if tokens.len == 0 {
		return false
	}
	if tokens.len == 1 {
		return tokens[0].tok == .name && (!tokens[0].has_source() || tokens[0].is_storage)
	}
	if tokens[0].tok == .mul {
		return fastc_is_pointer_type(g.underlying_alias_type(g.infer_expression_type(tokens[1..]) or { u32(0) }))
	}
	if tokens.len >= 3 && tokens[tokens.len - 2].tok == .dot && tokens.last().tok == .name {
		base := tokens[..tokens.len - 2]
		base_type := g.underlying_alias_type(g.infer_expression_type(base) or { u32(0) })
		return fastc_is_pointer_type(base_type) || g.append_value_borrows_storage(base, false)
	}
	if tokens.last().tok == .rsbr {
		mut depth := 0
		for i := tokens.len - 1; i >= 0; i-- {
			if tokens[i].tok == .rsbr {
				depth++
			} else if tokens[i].tok == .lsbr {
				depth--
				if depth == 0 && i > 0 {
					base := tokens[..i]
					base_type := g.underlying_alias_type(g.infer_expression_type(base) or { u32(0) })
					// A range is a new value, but indexing an array range still borrows
					// an element from the original array's storage.
					if tokens[i + 1..tokens.len - 1].any(it.tok == .dotdot) {
						base_layout := fastc_trim_pointer_suffix(flat.source_type_name(base_type))
						return indexed && (base_layout.starts_with('Array_') || fastc_fixed_array_length(base_layout) != none)
							&& g.append_value_borrows_storage(base, false)
					}
					return (fastc_is_pointer_type(base_type) && !g.is_map_type(base_type) && base_type != flat.source_type_id('string*'))
						|| g.append_value_borrows_storage(base, true)
				}
			}
		}
	}
	if tokens.len >= 5 && tokens[tokens.len - 4].tok == .dot && tokens[tokens.len - 3].tok == .name
		&& tokens[tokens.len - 3].lit in ['first', 'last'] && tokens[tokens.len - 2].tok == .lpar
		&& tokens.last().tok == .rpar {
		base := tokens[..tokens.len - 4]
		base_type := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(g.infer_expression_type(base) or { u32(0) })))
		return base_type.starts_with('Array_') && g.append_value_borrows_storage(base, false)
	}
	return false
}

fn (g &Parser) render_array_append_value(tokens []FastcExpressionToken, element_type u32, source string) string {
	if g.selfhost && g.underlying_alias_type(element_type) == flat.source_type_string
		&& g.append_value_borrows_storage(tokens, false) {
		return 'builtin__string_clone(${source})'
	}
	return source
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
	left_array_type := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(left_type)))
	element_type := g.array_element_type(left_type) or { return none }
	right_tokens := tokens[operator_index + 1..]
	element_storage_type := g.collection_storage_type(element_type)
	right_type := g.infer_expression_type(right_tokens) or { return none }
	normalized_right := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(fastc_normalize_inferred_type(right_type))))
	// `[]T << []T` is push-many (append each element), but when the element type is a
	// sum type that lists `[]T` as a variant (a recursive sum type such as
	// `type Value = []Value | int`), the array must be boxed as a single element
	// instead. This mirrors the `sumtype_has_variant` guard the main C backend applies
	// before selecting push-many (see vlib/v/gen/c/infix.v).
	propagated_array := right_tokens.len > 1 && right_tokens.last().tok in [.not, .question]
		&& !fastc_trailing_not_marks_fixed_array_literal(right_tokens)
	is_array_append := normalized_right == left_array_type
		&& (!g.sumtype_has_variant(element_type, flat.source_type_id(normalized_right)) || propagated_array)
	separator := rendered_expression.index('<<') or { return none }
	left_tokens := tokens[..operator_index]
	mut left_source := rendered_expression[..separator]
	mut right_source := rendered_expression[separator + 2..]
	// The raw streamed value is only valid C for a plain operand. A boxed sum/interface
	// cast (`arr << Primitive(x)`), a top-level index/array-literal (`arr << type_idx['int']`,
	// `arr << [a, b]`), and any call (`arr << s.clone()`, `arr << f(x)`) need the argument
	// renderer, which boxes variants, lowers indexing, and routes method/function calls.
	// Render through it whenever it succeeds, keeping the raw form only as a fallback.
	expected_right_type := if is_array_append {
		left_array_type
	} else {
		flat.source_type_name(element_type)
	}
	if rerendered := g.render_call_argument_expression(right_tokens, flat.source_type_id(expected_right_type)) {
		right_source = rerendered
	}
	if !is_array_append {
		right_source = g.render_array_append_value(right_tokens, element_type, right_source)
	}
	temporary := '__vf_append_value'
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
			map_type := (g.infer_expression_type(base_tokens) or { u32(0) })
			if key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) {
				key_type := key_type_source_id
				value_type := value_type_source_id

				if value_type == left_type {
					key_source := g.render_call_argument_expression(left_tokens[open + 1..left_tokens.len - 1], key_type) or { return none }
					// The map to insert into is addressed as a `map *`. A nested base (`m[k1][k2]
					// << x`) is itself a map value, so get a mutable pointer to it (which inserts
					// an empty map when absent); a plain base is `&`-taken.
					mut map_address := ''
					if base_tokens.len > 1 && base_tokens.last().tok == .rsbr {
						if nested := g.render_mutable_map_value_pointer(base_tokens) {
							map_address = '(map *)(${nested.source})'
						}
					}
					if map_address == '' {
						mut map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
							g.globals[fastc_global_key(g.module_name, base_tokens[0].lit)] or {
								g.resolved_expression_name(base_tokens[0].lit, .unknown)
							}
						} else {
							g.render_member_receiver(base_tokens) or { return none }
						}
						if flat.source_type_name(map_type).ends_with('*') {
							map_source = '*(${map_source})'
						}
						map_address = '(map *)&(${map_source})'
					}
					// A missing entry whose value is itself an array must be created with a real
					// header carrying `element_size`; a `(Array_x){0}` has element_size 0, so the
					// following `<<` copies `len * 0` bytes and silently drops the value.
					map_empty := if fastc_trim_pointer_suffix(flat.source_type_name(value_type)).starts_with('Array_') {
						elem := g.array_element_type(value_type) or { return none }
						'((${flat.source_type_name(value_type)})builtin____new_array(0, 0, sizeof(${g.collection_storage_type(fastc_normalize_inferred_type(elem))})))'
					} else {
						'(${flat.source_type_name(value_type)}){0}'
					}
					left_source = '({ map *__vf_append_map_addr = ${map_address}; ${g.collection_runtime_type(key_type)} __vf_append_map_key = (${key_source}); ${flat.source_type_name(value_type)} *__vf_append_map_value = (${flat.source_type_name(value_type)} *)builtin__map_get_check(__vf_append_map_addr, &__vf_append_map_key); if (__vf_append_map_value == NULL) { ${flat.source_type_name(value_type)} __vf_append_map_empty = ${map_empty}; builtin__map_set(__vf_append_map_addr, &__vf_append_map_key, &__vf_append_map_empty); __vf_append_map_value = (${flat.source_type_name(value_type)} *)builtin__map_get_check(__vf_append_map_addr, &__vf_append_map_key); } __vf_append_map_value; })'
					map_push := if is_array_append {
						'${flat.source_type_name(value_type)} ${temporary} = (${right_source}); ${flat.source_type_name(value_type)} *__vf_append_map_target = ${left_source}; builtin__array_push_many((array *)__vf_append_map_target, ${temporary}.data, ${temporary}.len);'
					} else {
						'${element_storage_type} ${temporary} = (${right_source}); ${flat.source_type_name(value_type)} *__vf_append_map_target = ${left_source}; builtin__array_push((array *)__vf_append_map_target, &${temporary});'
					}
					return FastcRenderedExpression{
						source: '({ ${map_push} 0; })'
						typ:    flat.source_type_void
					}
				}
			}
			base_type := (g.infer_expression_type(base_tokens) or { u32(0) })
			base_layout := fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(base_type)))
			if base_layout.starts_with('Array_') {
				outer_element_type := (g.array_element_type(base_type) or { u32(0) })
				if fastc_normalize_inferred_type(outer_element_type) == fastc_normalize_inferred_type(left_type) {
					index_source := g.render_call_argument_expression(left_tokens[open + 1..left_tokens.len - 1], flat.source_type_id('int')) or { return none }
					base_source := if base_tokens.len == 1 {
						g.resolved_root_expression_name(base_tokens[0].lit)
					} else if nested_base := g.render_array_access_expression(base_tokens) {
						nested_base.source
					} else if raw_base := g.render_raw_expression_tokens(base_tokens) {
						g.render_member_receiver(base_tokens) or { raw_base }
					} else {
						return none
					}
					array_value := if flat.source_type_name(base_type).ends_with('*') {
						'*(${base_source})'
					} else {
						base_source
					}
					target := '(${flat.source_type_name(left_type).trim_right('*')} *)builtin__array_get(${array_value}, ${index_source})'
					if is_array_append {
						return FastcRenderedExpression{
							source: '({ ${flat.source_type_name(left_type).trim_right('*')} ${temporary} = (${right_source}); ${flat.source_type_name(left_type).trim_right('*')} *__vf_append_array_target = ${target}; builtin__array_push_many((array *)__vf_append_array_target, ${temporary}.data, ${temporary}.len); 0; })'
							typ:    flat.source_type_void
						}
					}
					return FastcRenderedExpression{
						source: '({ ${element_storage_type} ${temporary} = (${right_source}); ${flat.source_type_name(left_type).trim_right('*')} *__vf_append_array_target = ${target}; builtin__array_push((array *)__vf_append_array_target, &${temporary}); 0; })'
						typ:    flat.source_type_void
					}
				}
			}
		}
	}
	// `arr.last() << x` / `arr.last().field << x`: append to the last/first element of `arr` (or
	// a field of it) in place. `.last()`/`.first()` are not C lvalues, so target the element
	// through array_get and apply any trailing field chain to the element pointer.
	mut last_first_index := -1
	for k := 1; k + 2 < left_tokens.len; k++ {
		if left_tokens[k].tok == .name && left_tokens[k].lit in ['last', 'first'] && left_tokens[k - 1].tok == .dot && left_tokens[k + 1].tok == .lpar && left_tokens[k + 2].tok == .rpar {
			last_first_index = k
			break
		}
	}
	if last_first_index > 0 {
		method := left_tokens[last_first_index].lit
		base_tokens := left_tokens[..last_first_index - 1]
		suffix_tokens := left_tokens[last_first_index + 3..]
		base_type := (g.infer_expression_type(base_tokens) or { u32(0) })
		mut suffix := ''
		mut si := 0
		mut suffix_ok := true
		for si + 1 < suffix_tokens.len {
			if suffix_tokens[si].tok == .dot && suffix_tokens[si + 1].tok == .name {
				sep := if si == 0 { '->' } else { '.' }
				suffix += '${sep}${suffix_tokens[si + 1].lit}'
				si += 2
			} else {
				suffix_ok = false
				break
			}
		}
		if suffix_ok && fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(base_type))).starts_with('Array_') {
			if elem_type_source_id := g.array_element_type(base_type) {
				elem_type := elem_type_source_id

				base_source := if base_tokens.len == 1 {
					g.resolved_root_expression_name(base_tokens[0].lit)
				} else if nested_base := g.render_array_access_expression(base_tokens) {
					nested_base.source
				} else if raw_base := g.render_raw_expression_tokens(base_tokens) {
					g.render_member_receiver(base_tokens) or { raw_base }
				} else {
					return none
				}
				array_value := if flat.source_type_name(base_type).ends_with('*') {
					'*(${base_source})'
				} else {
					base_source
				}
				arr_tmp := '__vf_append_last_array'
				elem_tmp := '__vf_append_last_elem'
				norm_elem := g.collection_storage_type(fastc_normalize_inferred_type(elem_type))
				index := if method == 'last' { '${arr_tmp}.len - 1' } else { '0' }
				// No trailing field: the element itself is the array; otherwise the target is the
				// field reached through the element pointer.
				target := if suffix == '' {
					'(array *)${elem_tmp}'
				} else {
					'(array *)&(${elem_tmp}${suffix})'
				}
				push := if is_array_append {
					'builtin__array_push_many(${target}, ${temporary}.data, ${temporary}.len);'
				} else {
					'builtin__array_push(${target}, &${temporary});'
				}
				value_decl := if is_array_append {
					flat.source_type_name(left_type).trim_right('*')
				} else {
					element_storage_type
				}
				return FastcRenderedExpression{
					source: '({ array ${arr_tmp} = (${array_value}); ${norm_elem} *${elem_tmp} = (${norm_elem} *)builtin__array_get(${arr_tmp}, ${index}); ${value_decl} ${temporary} = (${right_source}); ${push} 0; })'
					typ:    flat.source_type_void
				}
			}
		}
	}
	// A member-chain target that indexes a dynamic array mid-chain (`b.files[i].errors`)
	// cannot be spelled with raw C indexing; the member receiver renderer lowers it.
	if left_tokens.len > 1 {
		if member_left := g.render_member_receiver(left_tokens) {
			left_source = member_left
		}
	}
	if is_array_append {
		array_target := if flat.source_type_name(left_type).ends_with('*') {
			'(array *)(${left_source})'
		} else {
			'(array *)&(${left_source})'
		}
		return FastcRenderedExpression{
			source: '({ ${flat.source_type_name(left_type).trim_right('*')} ${temporary} = (${right_source}); builtin__array_push_many(${array_target}, ${temporary}.data, ${temporary}.len); 0; })'
			typ:    flat.source_type_void
		}
	}
	return FastcRenderedExpression{
		source: '({ ${g.declaration_c_type(element_type, right_source)} ${temporary} = (${right_source}); builtin__array_push((array *)&(${left_source}), &${temporary}); 0; })'
		typ:    flat.source_type_void
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
					operand_type := g.concatenation_operand_type(tokens[operand_start..i])
					string_operands << fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(operand_type))) == 'string'
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
	last_operand_type := g.concatenation_operand_type(tokens[operand_start..])
	string_operands << fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(last_operand_type))) == 'string'
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
			part := g.render_concatenation_operand(tokens[operand_start..i]) or {
				return none
			}
			parts << part
			operand_start = i + 1
		}
	}
	last_part := g.render_concatenation_operand(tokens[operand_start..]) or { return none }
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
		typ:    flat.source_type_string
	}
}

// concatenation_operand_type returns the value type of one `+` operand in a string
// concatenation, resolving a trailing `?`/`!` option/result propagation to the propagated
// value type (`part(a)? + part(b)?` concatenates two strings, not two `Option`s).
fn (g &Parser) concatenation_operand_type(tokens []FastcExpressionToken) u32 {
	if tokens.len > 1 && tokens.last().tok in [.question, .not] {
		inner := tokens[..tokens.len - 1]
		value_type := g.option_value_type_for_expression(inner)
		if value_type != u32(0) {
			return value_type
		}
		return g.infer_expression_type(inner) or { u32(0) }
	}
	return g.infer_expression_type(tokens) or { u32(0) }
}

// render_concatenation_operand renders one `+` operand of a string concatenation. An operand
// ending in a `?`/`!` propagation is lowered through the argument path (which turns the
// propagation into its unwrap stmt-expr); a plain operand keeps the comparison-operand path.
fn (g &Parser) render_concatenation_operand(tokens []FastcExpressionToken) ?string {
	if tokens.len > 1 && tokens.last().tok in [.question, .not] {
		return g.render_call_argument_expression(tokens, flat.source_type_id('string'))
	}
	return g.render_comparison_operand(tokens, flat.source_type_id('string'))
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
	if call_member := g.render_call_result_member_expression(inner_tokens) {
		inner_source = call_member.source
	} else if defaulted_call := g.render_missing_call_arguments(inner_tokens) {
		// A plain `f(a, b)` call renders each argument through render_call_argument_expression,
		// which lowers nested propagation (`f(x.m()?, y.m()?)?`) and fills defaults. The raw
		// method-call rewrite below only rewrites method-call arguments in place, leaving a
		// stray `?` (a C ternary) in them, so prefer the argument-aware path for whole calls.
		inner_source = defaulted_call.source
	} else if explicit_generic := g.render_explicit_generic_call_expression(inner_tokens) {
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
	temporary := '__vf_op'
	failure := if g.in_main {
		deferred := g.deferred_scopes_source()
		message := if (flat.source_type_name(g.infer_expression_type(inner_tokens) or { u32(0) })) == '__v_result' {
			'builtin__IError_msg(${temporary}.err)'
		} else {
			'_S("none")'
		}
		'${deferred} builtin__panic_result_not_set(${message});'
	} else if g.return_type_text() in ['Option', '__v_result'] {
		'return ${temporary};'
	} else if g.prefs.is_test {
		g.test_propagation_failure(temporary, (flat.source_type_name(g.infer_expression_type(inner_tokens) or { u32(0) })) == '__v_result')
	} else {
		'return 1;'
	}
	value := if flat.source_type_name(value_type) in ['', 'void'] {
		'0'
	} else {
		g.option_value_expression(value_type, temporary)
	}
	return FastcRenderedExpression{
		source: '({ ${g.option_container_c_type(inner_tokens, inner_source)} ${temporary} = (${inner_source}); if (${temporary}.state) { ${failure} } ${value}; })'
		typ:    value_type
	}
}

// render_propagation_before_member lowers `x()?.field` / `x()?.method(a)`: the `?`/`!`
// propagates the option from `x()`, unwrapping it to the value, and the trailing member chain
// then applies to that value. Returns none when there is no such mid-expression propagation.
fn (g &Parser) render_propagation_before_member(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len > 3 && tokens[0].tok == .lpar {
		close := fastc_matching_rpar(tokens, 0) or { return none }
		if close + 1 < tokens.len && tokens[close + 1].tok == .dot {
			inner := fastc_strip_paren_tokens(tokens[1..close])
			if inner.len > 1 && inner.last().tok in [.not, .question]
				&& !fastc_trailing_not_marks_fixed_array_literal(inner) {
				value := g.render_option_propagation(inner[..inner.len - 1]) or { return none }
				mut chained := [FastcExpressionToken{
					tok:    .name
					source: fastc_token_source('(${value.source})')
					typ:    value.typ
				}]
				chained << tokens[close + 1..]
				return FastcRenderedExpression{
					source: g.render_call_argument_expression(chained, flat.source_type_id('')) or { return none }
					typ:    (g.infer_expression_type(chained) or { u32(0) })
				}
			}
		}
	}
	mut depth := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.question, .not {
				if depth == 0 && i > 0 && i + 1 < tokens.len && tokens[i + 1].tok == .dot && !fastc_trailing_not_marks_fixed_array_literal(tokens[..i + 1]) {
					unwrapped := g.render_option_propagation(tokens[..i]) or { return none }
					if unwrapped.typ_text() in ['', 'void'] {
						return none
					}
					synth := FastcExpressionToken{
						tok:    .name
						source: fastc_token_source('(${unwrapped.source})')
						typ:    unwrapped.typ
					}
					mut chained := [synth]
					chained << tokens[i + 1..]
					source := g.render_call_argument_expression(chained, flat.source_type_id('')) or { return none }
					return FastcRenderedExpression{
						source: source
						typ:    (g.infer_expression_type(chained) or { u32(0) })
					}
				}
			}
			else {}
		}
	}
	return none
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
		replaced := fastc_replace(rendered, needle, propagation.source)
		if replaced.str != rendered.str {
			rendered = replaced
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
		typ:    (g.infer_expression_type(tokens) or { u32(0) })
	}
}

fn (g &Parser) render_method_receiver_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len >= 3 && tokens[0].tok == .lpar {
		if close := fastc_matching_rpar(tokens, 0) {
			if close == tokens.len - 1 {
				inner := g.render_method_receiver_expression(tokens[1..close])?
				return FastcRenderedExpression{
					source: '(${inner.source})'
					typ:    inner.typ
				}
			}
		}
	}
	if tokens.len > 1 && tokens.last().tok == .not && !(tokens[0].tok == .lsbr && tokens[tokens.len - 2].tok == .rsbr) {
		// A `!`-propagated receiver (`f()!.m()`): unwrap the result before the call.
		if propagation := g.render_option_propagation(tokens[..tokens.len - 1]) {
			return propagation
		}
	}
	receiver_type := g.infer_expression_type(tokens) or { return none }
	if tokens.len == 1 && tokens[0].has_source() {
		// A pre-rendered synthetic receiver (e.g. an `or {}`-unwrap `({ ... })` used as
		// `expr or { ... }.method()`) carries its full rendered form as its `source`;
		// use it directly so the method call binds to it.
		return FastcRenderedExpression{
			source: tokens[0].source_text()
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
	// A method receiver may be a member `as`-cast (`(sym.info as GenericInst).types.any(…)`);
	// lower it here so the `as` does not reach the raw renderer intact.
	if g.selfhost && tokens.len > 0 && tokens[0].tok == .lpar {
		if as_member := g.render_as_cast_member_access(tokens) {
			return as_member
		}
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
	if g.selfhost {
		// A call receiver may omit trailing defaulted / `@[params]` arguments
		// (`new_suggestion(a, b).say(...)`); fill them here so the raw renderer below does
		// not emit the call with too few arguments.
		if defaulted := g.render_missing_call_arguments(tokens) {
			return defaulted
		}
	}
	if raw := g.render_raw_expression_tokens(tokens) {
		if source := g.render_method_call_expression(tokens, raw) {
			return source
		}
	}
	if source := g.render_membership_candidate(tokens, flat.source_type_id('')) {
		return FastcRenderedExpression{
			source: source
			typ:    receiver_type
		}
	}
	return none
}

// render_sumtype_common_field_access lowers `x.f` where `x` is a boxed sum type and `f`
// is a field common to every variant (V's "common sum-type field"). FastC boxes only a
// `void*` payload, so the field is read through a runtime switch on the type tag that
// casts the payload to the matched variant. Returns none unless `tokens` is exactly such
// a member chain.
// fastc_sumtype_field_switch_source renders the `({ … switch(_typ) … })` statement-expression
// that reads the common field `field_name` (type `field_type`) from the boxed sum type
// `receiver_type` (already rendered as `receiver_source`).
fn (g &Parser) fastc_sumtype_field_switch_source(receiver_type_source_id u32, receiver_source string, field_name string, field_type_source_id u32) ?string {
	receiver_type := flat.source_type_name(receiver_type_source_id)
	field_type := flat.source_type_name(field_type_source_id)
	access := if receiver_type.ends_with('*') { '->' } else { '.' }
	subject := '__vf_sumfield'
	result_var := '__vf_sumfield_result'
	mut cases := []string{}
	for variant_source_id in g.sum_type_leaf_variants(receiver_type_source_id) {
		variant := variant_source_id

		field := g.struct_field_metadata(variant, field_name) or { return none }
		member := '((${flat.source_type_name(variant)} *)${subject}${access}_object)->${fastc_c_identifier(field.name)}'
		cases << 'case __v_typeid_${flat.source_type_name(variant)}: ${result_var} = ${member}; break;'
	}
	return '({ ${receiver_type} ${subject} = (${receiver_source}); ${field_type} ${result_var}; switch (${subject}${access}_typ) { ${cases.join(' ')} default: memset(&${result_var}, 0, sizeof(${result_var})); break; } ${result_var}; })'
}

// fastc_interface_field_switch_source selects an interface field's concrete storage through the
// boxed object. Dereferencing the selected pointer outside the switch preserves an lvalue for
// chained assignments and mutable arguments. Enumerate every declared struct/union whose
// field of the same name has the interface field's type — only the real implementers' tags ever
// occur at runtime, so any coincidental extra case is dead. Mirrors the sum-type field switch.
fn (g &Parser) fastc_interface_field_switch_source(receiver_type_source_id u32, receiver_source string, field_name string, field_type_source_id u32) ?string {
	receiver_type := flat.source_type_name(receiver_type_source_id)
	access := if receiver_type.ends_with('*') { '->' } else { '.' }
	subject := '__vf_ifacefield'
	result_var := '__vf_ifacefield_pointer'
	normalized_field_type := fastc_normalize_inferred_type(field_type_source_id)
	storage_type := fastc_output_c_type(flat.source_type_name(normalized_field_type))
	mut cases := []string{}
	for type_key, kind in g.declared_kinds {
		if kind !in [.struct_, .union_] {
			continue
		}
		c_type := fastc_c_declared_type_name(type_key)
		field := g.struct_field_metadata(flat.source_type_id(c_type), field_name) or { continue }
		if field.storage_path.len > 0 || fastc_normalize_inferred_type(field.typ) != normalized_field_type {
			continue
		}
		member := '((${c_type} *)${subject}${access}_object)->${fastc_c_identifier(field.name)}'
		cases << 'case __v_typeid_${c_type}: ${result_var} = &(${member}); break;'
	}
	if cases.len == 0 {
		return none
	}
	failure := if g.selfhost {
		'builtin__panic(_S("invalid interface type")); __builtin_unreachable();'
	} else {
		'abort();'
	}
	return '(*({ ${receiver_type} ${subject} = (${receiver_source}); ${storage_type} *${result_var}; switch (${subject}${access}_typ) { ${cases.join(' ')} default: ${failure} } ${result_var}; }))'
}

fn (g &Parser) render_sumtype_common_field_access(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 3 || tokens.last().tok != .name || tokens[tokens.len - 2].tok != .dot {
		return none
	}
	// Phase 1: the LAST field is the common field. Its receiver may be ANY paren-balanced
	// expression, including a method call / index (`node.stmts.first().pos`).
	receiver_tokens := tokens[..tokens.len - 2]
	mut receiver_depth := 0
	for item in receiver_tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { receiver_depth++ }
			.rpar, .rsbr, .rcbr { receiver_depth-- }
			else {}
		}
	}
	if receiver_depth == 0 {
		if receiver_type_source_id := g.infer_expression_type(receiver_tokens) {
			receiver_type := receiver_type_source_id

			if field_type_source_id := g.sumtype_common_field_type(receiver_type, tokens.last().lit) {
				field_type := field_type_source_id

				// A plain member chain renders through render_member_receiver; a call/index
				// receiver is rendered as a general expression.
				receiver_source := if member := g.render_member_receiver(receiver_tokens) {
					member
				} else {
					g.render_call_argument_expression(receiver_tokens, receiver_type) or {
						return none
					}
				}
				if switch_source := g.fastc_sumtype_field_switch_source(receiver_type, receiver_source, tokens.last().lit, field_type) {
					return FastcRenderedExpression{
						source: switch_source
						typ:    field_type
					}
				}
			}
		}
	}
	// Phase 2: the common field sits MID-CHAIN in a plain member chain (`expr.or_expr.kind`),
	// with plain field accesses following it. Only a fully name/dot chain qualifies.
	if tokens[0].tok != .name {
		return none
	}
	mut current_type := g.infer_expression_type(tokens[..1]) or { return none }
	mut split := -1
	mut common_field := ''
	mut field_type := ''
	mut i := 1
	for i + 1 < tokens.len {
		if tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		field := tokens[i + 1].lit
		if ft_source_id := g.sumtype_common_field_type(current_type, field) {
			ft := ft_source_id

			split = i
			common_field = field
			field_type = flat.source_type_name(ft)
			break
		}
		next_type := g.struct_member_type(current_type, field)
		if next_type == u32(0) {
			return none
		}
		current_type = next_type
		i += 2
	}
	// A common field at the very end was already handled by phase 1; require a real suffix.
	if split < 0 || split + 2 >= tokens.len {
		return none
	}
	mid_receiver_tokens := tokens[..split]
	mid_receiver_type := g.infer_expression_type(mid_receiver_tokens) or { return none }
	receiver_source := g.render_member_receiver(mid_receiver_tokens) or { return none }
	switch_source := g.fastc_sumtype_field_switch_source(mid_receiver_type, receiver_source, common_field, flat.source_type_id(field_type)) or { return none }
	mut suffix := ''
	mut suffix_type := field_type
	mut j := split + 2
	for j + 1 < tokens.len {
		if tokens[j].tok != .dot || tokens[j + 1].tok != .name {
			return none
		}
		f := g.struct_field_metadata(flat.source_type_id(suffix_type), tokens[j + 1].lit) or { return none }
		separator := if suffix_type.ends_with('*') { '->' } else { '.' }
		suffix += separator + fastc_c_identifier(f.name)
		suffix_type = f.typ_text()
		j += 2
	}
	return FastcRenderedExpression{
		source: '(${switch_source})${suffix}'
		typ:    flat.source_type_id(suffix_type)
	}
}

// render_sumtype_common_field_assignment lowers `x.field = value` where `x` is a boxed sum type
// and `field` is common to every variant. The boxed value stores the field in the concrete
// variant behind `_object`, so the assignment dispatches on the runtime tag and writes through
// the matched variant pointer (which the copied box shares), mirroring the read helper.
fn (g &Parser) render_sumtype_common_field_assignment(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut assign_idx := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.assign {
				if depth == 0 {
					assign_idx = i
					break
				}
			}
			else {}
		}
	}
	if assign_idx <= 0 || assign_idx + 1 >= tokens.len {
		return none
	}
	lhs := tokens[..assign_idx]
	if lhs.len < 3 || lhs.last().tok != .name || lhs[lhs.len - 2].tok != .dot {
		return none
	}
	field_name := lhs.last().lit
	receiver_tokens := lhs[..lhs.len - 2]
	mut rdepth := 0
	for item in receiver_tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { rdepth++ }
			.rpar, .rsbr, .rcbr { rdepth-- }
			else {}
		}
	}
	if rdepth != 0 {
		return none
	}
	receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
	field_type := g.sumtype_common_field_type(receiver_type, field_name) or { return none }
	receiver_source := if member := g.render_member_receiver(receiver_tokens) {
		member
	} else {
		g.render_call_argument_expression(receiver_tokens, receiver_type) or { return none }
	}
	value_source := g.render_call_argument_expression(tokens[assign_idx + 1..], field_type) or {
		return none
	}
	access := if flat.source_type_name(receiver_type).ends_with('*') { '->' } else { '.' }
	subject := '__vf_sumfield_assign'
	value_var := '__vf_sumfield_assign_value'
	mut cases := []string{}
	for variant_source_id in g.sum_type_leaf_variants(receiver_type) {
		variant := variant_source_id

		field := g.struct_field_metadata(variant, field_name) or { return none }
		member := '((${flat.source_type_name(variant)} *)${subject}${access}_object)->${fastc_c_identifier(field.name)}'
		cases << 'case __v_typeid_${flat.source_type_name(variant)}: ${member} = ${value_var}; break;'
	}
	source := '({ ${flat.source_type_name(receiver_type)} ${subject} = (${receiver_source}); ${flat.source_type_name(field_type)} ${value_var} = (${value_source}); switch (${subject}${access}_typ) { ${cases.join(' ')} default: break; } (void)0; })'
	return FastcRenderedExpression{
		source: source
		typ:    flat.source_type_void
	}
}

// render_common_field_comparison_expression renders a top-level comparison in which an
// operand reads a common sum-type field (`node.typ_text() == ast.invalid_type`). Such operands
// lower to a switch statement-expression, which the ordinary raw comparison path cannot
// splice in, so both operands are rendered through render_comparison_operand here.
fn (g &Parser) render_common_field_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut op_index := -1
	mut op := token.Token.unknown
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			.eq, .ne, .lt, .gt, .le, .ge {
				if depth == 0 && op_index < 0 {
					op_index = i
					op = item.tok
				}
			}
			else {}
		}
	}
	if op_index <= 0 || op_index + 1 >= tokens.len {
		return none
	}
	left_tokens := tokens[..op_index]
	right_tokens := tokens[op_index + 1..]
	if g.render_sumtype_common_field_access(left_tokens) == none && g.render_sumtype_common_field_access(right_tokens) == none {
		return none
	}
	left_type := (g.infer_expression_type(left_tokens) or { u32(0) })
	right_type := (g.infer_expression_type(right_tokens) or { u32(0) })
	left_source := g.render_comparison_operand(left_tokens, right_type) or { return none }
	right_source := g.render_comparison_operand(right_tokens, left_type) or { return none }
	if fastc_normalize_inferred_type(left_type) == flat.source_type_string || fastc_normalize_inferred_type(right_type) == flat.source_type_string {
		if op == .eq {
			return FastcRenderedExpression{
				source: 'builtin__string_eq(${left_source}, ${right_source})'
				typ:    flat.source_type_bool
			}
		}
		if op == .ne {
			return FastcRenderedExpression{
				source: '(!builtin__string_eq(${left_source}, ${right_source}))'
				typ:    flat.source_type_bool
			}
		}
	}
	c_op := match op {
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		else {
			return none
		}
	}
	return FastcRenderedExpression{
		source: '((${left_source}) ${c_op} (${right_source}))'
		typ:    flat.source_type_bool
	}
}

fn (g &Parser) render_member_receiver(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 {
		return none
	}
	mut source := ''
	mut current_type := u32(0)
	mut i := 1
	// A field of a call's result (`f(x).len`) reads from the rendered call.
	call_close := fastc_member_root_call_close(tokens, 0, tokens.len) or { -1 }
	if call_close >= 0 {
		call_tokens := tokens[..call_close + 1]
		// A free call renders its arguments against the parameter types.
		call := g.render_member_root_expression(call_tokens) or { return none }
		source = '(${call.source})'
		current_type = call.typ
		i = call_close + 1
	} else if tokens[0].tok != .name {
		return none
	} else if tokens[0].has_source() {
		// A synthetic base token (an `x()?`-unwrap folded upstream: `get()!.field`) carries its C
		// spelling and type in `.source`/`.typ_text()`, not `.lit`; use them so the member chain reads from
		// the real unwrapped value instead of an empty base.
		source = tokens[0].source_text()
		current_type = tokens[0].typ
	} else {
		source = g.resolved_expression_name(tokens[0].lit, .unknown)
		current_type = g.infer_expression_type(tokens[..1]) or { return none }
	}
	mut member_path := if i == 1 { tokens[0].lit } else { '()' }
	mut multi_variants := []u32{}
	mut multi_tag_source := ''
	mut multi_object_source := ''
	// A smart-cast on the bare subject itself (`x is T`, narrowing the whole local) also
	// applies before the first field access.
	if smartcast := g.member_smartcasts[member_path] {
		source = smartcast.source
		current_type = smartcast.typ
		multi_variants = smartcast.variants.ids.clone()
		multi_tag_source = smartcast.tag_source
		multi_object_source = smartcast.object_source
	}
	for i < tokens.len {
		if tokens[i].tok == .lsbr {
			// A dynamic-array element inside the chain (`b.files[i].errors`) is reached
			// through the erased `.data` storage, so C indexing on the `array` struct itself
			// is invalid — cast to the element pointer first.
			close := fastc_matching_delimiter(tokens, i, .lsbr, .rsbr) or { return none }
			layout := flat.source_type_id(fastc_trim_pointer_suffix(flat.source_type_name(g.underlying_alias_type(current_type))))
			if fixed_element_source_id := fastc_fixed_array_element_type(layout) {
				fixed_element := fixed_element_source_id

				// A fixed array is indexed in place: raw C storage (a field or global)
				// directly, a wrapper value (a local or parameter) through `.data`.
				index_source := g.render_membership_candidate(tokens[i + 1..close], flat.source_type_id('int')) or {
					return none
				}
				storage := if g.fixed_array_uses_raw_storage(tokens[..i]) {
					'(${source})'
				} else if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
					'(${source})->data'
				} else {
					'(${source}).data'
				}
				fixed_length := fastc_fixed_array_length(flat.source_type_name(layout)) or { '' }
				source = if g.selfhost && fixed_length != '' {
					'${storage}[builtin__v_fixed_index(${index_source}, ${fixed_length})]'
				} else {
					'${storage}[${index_source}]'
				}
				current_type = fixed_element
				member_path += '[]'
				i = close + 1
				continue
			}
			if !flat.source_type_name(layout).starts_with('Array_') {
				pointer_type := g.underlying_alias_type(current_type)
				if flat.source_type_name(pointer_type).ends_with('*') {
					index_source := g.render_membership_candidate(tokens[i + 1..close], flat.source_type_int) or {
						return none
					}
					source = '(${source})[${index_source}]'
					current_type = flat.source_type_id(flat.source_type_name(pointer_type)[..flat.source_type_name(pointer_type).len - 1])
					member_path += '[]'
					i = close + 1
					continue
				}
				return none
			}
			element_type := g.array_element_type(current_type) or { return none }
			index_source := g.render_membership_candidate(tokens[i + 1..close], flat.source_type_id('int')) or {
				return none
			}
			data_separator := if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
				'->'
			} else {
				'.'
			}
			source = '((${g.collection_storage_type(element_type)} *)(${source})${data_separator}data)[${index_source}]'
			current_type = element_type
			member_path += '[]'
			// A smart-cast on the indexed element itself (`node.left[0] is Ident`, keyed
			// `node.left[]`) applies before a FOLLOWING field access (`node.left[0].name`), just
			// as one on a `.field` subject does below. When the index is the last segment (the
			// chain IS the smart-cast subject, e.g. a `node.left[i].method()` receiver), leave the
			// boxed value in place so the caller's own variant unwrap is not applied twice.
			if close + 1 < tokens.len {
				if smartcast := g.member_smartcasts[member_path] {
					source = smartcast.source
					current_type = smartcast.typ
					multi_variants = smartcast.variants.ids.clone()
					multi_tag_source = smartcast.tag_source
					multi_object_source = smartcast.object_source
				}
			}
			i = close + 1
			continue
		}
		if i + 1 >= tokens.len || tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		if multi_variants.len > 1 {
			projected := g.render_multi_variant_field_projection(multi_variants, multi_tag_source, multi_object_source, tokens[i + 1].lit) or { return none }
			source = projected.source
			current_type = projected.typ
			multi_variants = []
			member_path += '.' + tokens[i + 1].lit
			if smartcast := g.member_smartcasts[member_path] {
				source = smartcast.source
				current_type = smartcast.typ
				multi_variants = smartcast.variants.ids.clone()
				multi_tag_source = smartcast.tag_source
				multi_object_source = smartcast.object_source
			}
			i += 2
			continue
		}
		if g.declared_kinds[flat.source_type_name(g.semantic_type_key(current_type))] == .interface_ {
			// An interface FIELD (`iface.file`) is not a member of the boxed `{_object,_typ,…}`
			// struct; read it through a runtime `_typ` switch over the interface's implementers.
			if iface_field := g.interface_fields['${flat.source_type_name(g.semantic_type_key(current_type))}.${tokens[i + 1].lit}'] {
				source = g.fastc_interface_field_switch_source(current_type, source, iface_field.name, iface_field.typ) or { return none }
				current_type = fastc_normalize_inferred_type(iface_field.typ)
				member_path += '.' + tokens[i + 1].lit
				i += 2
				continue
			}
		}
		if tokens[i + 1].lit == 'len' && g.is_map_type(current_type) {
			separator := if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
				'->'
			} else {
				'.'
			}
			source += '${separator}data->count'
			current_type = flat.source_type_int
			member_path += '.len'
			i += 2
			continue
		}
		field := g.struct_field_metadata(current_type, tokens[i + 1].lit) or { return none }
		for storage_name in field.storage_path {
			separator := if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
				'->'
			} else {
				'.'
			}
			source += separator + fastc_c_identifier(storage_name)
			current_type = g.struct_direct_member_type(current_type, storage_name)
			if current_type == 0 {
				return none
			}
		}
		separator := if fastc_is_pointer_type(g.underlying_alias_type(current_type)) {
			'->'
		} else {
			'.'
		}
		field_source := source + separator + fastc_c_identifier(field.name)
		source = if field.is_shared_pointer { '*(${field_source})' } else { field_source }
		current_type = field.typ
		member_path += '.' + tokens[i + 1].lit
		if smartcast := g.member_smartcasts[member_path] {
			source = smartcast.source
			current_type = smartcast.typ
			multi_variants = smartcast.variants.ids.clone()
			multi_tag_source = smartcast.tag_source
			multi_object_source = smartcast.object_source
		}
		i += 2
	}
	return source
}

// render_multi_variant_field_projection reads a field shared by every type in a grouped
// sum-type match arm. The structs can place that field at different offsets, so select its
// address using the runtime tag instead of casting every variant to the first arm type.
fn (g &Parser) render_multi_variant_field_projection(variants []u32, tag_source string, object_source string, field_name string) ?FastcRenderedExpression {
	if variants.len < 2 || tag_source == '' || object_source == '' {
		return none
	}
	mut field_type := u32(0)
	mut pointer_source := ''
	for i := variants.len - 1; i >= 0; i-- {
		variant := variants[i]
		field := g.struct_field_metadata(variant, field_name) or { return none }
		if field_type == 0 {
			field_type = field.typ
			pointer_source = '((${fastc_output_c_type(flat.source_type_name(field_type))} *)0)'
		} else if field.typ != field_type {
			return none
		}
		mut receiver_type := flat.source_type_id(flat.source_type_name(variant) + '*')
		mut member := '((${flat.source_type_name(variant)} *)(${object_source}))'
		for storage_name in field.storage_path {
			separator := if fastc_is_pointer_type(g.underlying_alias_type(receiver_type)) {
				'->'
			} else {
				'.'
			}
			member += separator + fastc_c_identifier(storage_name)
			receiver_type = g.struct_direct_member_type(receiver_type, storage_name)
			if receiver_type == 0 {
				return none
			}
		}
		separator := if fastc_is_pointer_type(g.underlying_alias_type(receiver_type)) {
			'->'
		} else {
			'.'
		}
		field_pointer := '&(${member}${separator}${fastc_c_identifier(field.name)})'
		pointer_source = '((${tag_source}) == __v_typeid_${flat.source_type_name(variant)} ? ${field_pointer} : ${pointer_source})'
	}
	return FastcRenderedExpression{
		source: '(*(${pointer_source}))'
		typ:    field_type
	}
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

// value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &FastcMapAssignmentWrap) value_type_text() string {
	return flat.source_type_name(value.value_type)
}

// is_function_value_type reports a function value's type: `fn (...) T`, or an alias of one
// (`type Handler = fn (voidptr)`), which C holds as a function pointer.
fn (g &Parser) is_function_value_type(typ u32) bool {
	type_name := flat.source_type_name(typ)
	if type_name.starts_with('fn ') || type_name.starts_with('fn(') { return true }
	return type_name in g.functions || flat.source_type_name(g.semantic_type_key(typ)) in g.functions || type_name in g.mono_functions
}

// render_fixed_array_value renders `[N]T{}` (zeroed) and `[N]T{init: value}` (every
// element `value`, which may not read `index`) as a value of the fixed-array struct.
fn (g &Parser) render_fixed_array_value(tokens []FastcExpressionToken) ?string {
	close := fastc_matching_delimiter(tokens, 0, .lsbr, .rsbr) or { return none }
	mut open := close + 1
	for open < tokens.len && tokens[open].tok != .lcbr {
		open++
	}
	if close < 2 || open >= tokens.len || open == close + 1
		|| fastc_matching_delimiter(tokens, open, .lcbr, .rcbr) or { -1 } != tokens.len - 1 {
		return none
	}
	typ := fastc_normalize_inferred_type(g.infer_expression_type(tokens) or { return none })
	element_type := fastc_fixed_array_element_type(typ) or { return none }
	length := fastc_fixed_array_length(flat.source_type_name(typ)) or { return none }
	body := tokens[open + 1..tokens.len - 1]
	if body.len == 0 {
		return '((${flat.source_type_name(typ)}){0})'
	}
	if body.len < 3 || body[0].tok != .name || body[0].lit != 'init' || body[1].tok != .colon {
		return none
	}
	for item in body[2..] {
		if item.tok == .name && item.lit == 'index' {
			return none
		}
	}
	value := g.render_call_argument_expression(body[2..], element_type) or { return none }
	return '({ ${flat.source_type_name(typ)} __vf_fixed_value; for (int __vf_fixed_index = 0; __vf_fixed_index < ${length}; __vf_fixed_index++) { __vf_fixed_value.data[__vf_fixed_index] = (${value}); } __vf_fixed_value; })'
}
