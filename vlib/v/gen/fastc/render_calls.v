module fastc

import v.flat
import strings

fn fastc_member_root_call_close(tokens []FastcExpressionToken, start int, end int) ?int {
	if end - start < 3 || tokens[start].tok !in [.name, .amp, .and, .lpar] {
		return none
	}
	mut open := if tokens[start].tok == .lpar { start } else { -1 }
	if open < 0 {
		for i in start + 1 .. end {
			if tokens[i].tok == .lpar {
				open = i
				break
			}
			if tokens[i].tok !in [.name, .dot, .amp, .and] {
				return none
			}
		}
	}
	if open < 0 {
		return none
	}
	close := fastc_matching_rpar_before(tokens, open, end) or { return none }
	if close + 1 >= end || tokens[close + 1].tok !in [.dot, .lsbr] {
		return none
	}
	return close
}

fn (g &Parser) render_member_root_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if cast := g.render_cast_expression(tokens) {
		return cast
	}
	if tokens.len > 0 && tokens[0].tok == .lpar {
		typ := g.infer_expression_type(tokens) or { return none }
		return FastcRenderedExpression{
			source: g.render_call_argument_expression(tokens, typ) or { return none }
			typ:    typ
		}
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	return g.render_static_call_expression(tokens, raw) or {
		g.render_missing_call_arguments(tokens) or {
			g.render_method_call_expression(tokens, raw) or { return none }
		}
	}
}

fn (g &Parser) render_call_result_member_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	// Propagation is lowered by the caller, after rendering the member call itself.
	mut propagation_depth := 0
	for item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			propagation_depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			propagation_depth--
		} else if propagation_depth == 0 && item.tok in [.not, .question] {
			return none
		}
	}
	if tokens.len > 1 && tokens[0].has_source() && tokens[0].typ != 0 {
		if source := g.render_member_receiver(tokens) {
			return FastcRenderedExpression{
				source: source
				typ:    g.infer_member_access_type(tokens, 0, tokens.len) or { return none }
			}
		}
	}
	close := fastc_member_root_call_close(tokens, 0, tokens.len) or { return none }
	// Compound expressions render their operands separately, preserving boolean
	// result types and the short-circuit evaluation order.
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && (item.tok.is_assignment()
			|| (item.tok.is_infix() && !fastc_token_is_prefix_operator(tokens, i))) {
			return none
		}
	}
	base_tokens := tokens[..close + 1]
	call := g.render_member_root_expression(base_tokens) or { return none }
	// Fold the fully typed call before lowering fields or methods on its result.
	mut members := [FastcExpressionToken{
		tok:        .name
		lit:        call.source
		source:     fastc_token_source('(' + call.source + ')')
		typ:        call.typ
		is_storage: g.member_expression_refers_to_storage(base_tokens)
	}]
	members << tokens[close + 1..]
	if indexed := g.render_array_access_expression(members) {
		return indexed
	}
	member_source := g.render_raw_expression_tokens(members) or { return none }
	if method := g.render_method_call_expression(members, member_source) { return method }
	source := g.render_member_receiver(members) or { return none }
	return FastcRenderedExpression{
		source: source
		typ:    g.infer_expression_type(members) or { return none }
	}
}

fn fastc_expression_has_pointer_type_sizeof(tokens []FastcExpressionToken) bool {
	if tokens.len < 5 {
		return false
	}
	for i in 0 .. tokens.len - 2 {
		if tokens[i].tok == .key_sizeof && tokens[i + 1].tok == .lpar
			&& tokens[i + 2].tok in [.amp, .and, .lsbr] {
			return true
		}
	}
	return false
}

fn (g &Parser) render_pointer_type_sizeof(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .key_sizeof || tokens[1].tok != .lpar
		|| tokens.last().tok != .rpar {
		return none
	}
	operand := tokens[2..tokens.len - 1]
	if operand.len > 0 && operand[0].tok == .lsbr {
		mut array_type := g.array_initializer_type(operand) or { return none }
		if !flat.source_type_query(array_type).has(.c_fixed_array_prefix) { return none }
		mut dimensions := ''
		for flat.source_type_query(array_type).has(.c_fixed_array_prefix) {
			length := fastc_fixed_array_length_id(array_type) or { return none }
			dimensions += '[${length}]'
			array_type = fastc_fixed_array_element_type(array_type) or { return none }
		}
		return FastcRenderedExpression{
			source: 'sizeof(${fastc_output_c_type(flat.source_type_name(array_type))}${dimensions})'
			typ:    flat.source_type_id('int')
		}
	}
	ctype := g.type_from_expression_tokens(operand) or { return none }
	return FastcRenderedExpression{
		source: 'sizeof(${fastc_output_c_type(flat.source_type_name(ctype))})'
		typ:    flat.source_type_int
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
	if close_bracket + 4 != tokens.len - 1 || tokens[close_bracket + 1].tok != .lpar || tokens[close_bracket + 2].tok != .rpar || tokens[close_bracket + 3].tok != .dot || tokens[tokens.len - 1].tok != .name {
		return none
	}
	type_tokens := tokens[2..close_bracket]
	if type_tokens.len != 1 || type_tokens[0].tok != .name {
		return none
	}
	type_name := type_tokens[0].lit
	match tokens[tokens.len - 1].lit {
		'idx' {
			idx := fastc_builtin_type_idx(flat.source_type_id(type_name)) or { return none }
			return FastcRenderedExpression{
				source: idx.str()
				typ:    flat.source_type_int
			}
		}
		'name' {
			return FastcRenderedExpression{
				source: '_S("${type_name}")'
				typ:    flat.source_type_string
			}
		}
		else {
			return none
		}
	}
}

fn (g &Parser) render_typeof_name_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 6 || tokens[0].lit != 'typeof' || tokens[1].tok != .lpar || tokens[tokens.len - 2].tok != .dot || tokens.last().lit != 'name' {
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
				type_name = fastc_trim_pointer_type(type_name)
			}
		}
	}
	if type_name == u32(0) {
		return none
	}
	return FastcRenderedExpression{
		source: '_S("${g.typeof_display_name(type_name)}")'
		typ:    flat.source_type_string
	}
}

// typeof_display_name spells a FastC C type (`Point*`, `Array_int`,
// `strings__Builder`) the way `typeof(x).name` does in V (`&Point`, `[]int`,
// `strings.Builder`). Literal expressions are `int literal` and `f64`, as in cgen.
fn (g &Parser) typeof_display_name(typ_id u32) string {
	typ := flat.source_type_name(typ_id)
	if flat.source_type_query(typ_id).has(.c_pointer_suffix) {
		return '&' + g.typeof_display_name(flat.source_type_syntax(typ_id).c_pointer_base())
	}
	if typ_id == fastc_type_integer_literal || typ_id == fastc_type_negative_integer_literal {
		return 'int literal'
	}
	if typ_id == fastc_type_float_literal {
		return 'f64'
	}
	if fastc_primitive_c_type_id(typ_id) != none {
		return typ
	}
	if flat.source_type_query(typ_id).has(.c_array_prefix) {
		if element := g.array_element_type(typ_id) {
			return '[]' + g.typeof_display_name(element)
		}
	}
	if flat.source_type_query(typ_id).has(.c_map_prefix) {
		if key, value := g.map_key_value_types(typ_id) {
			return 'map[' + g.typeof_display_name(key) + ']' +
				g.typeof_display_name(value)
		}
	}
	if length := fastc_fixed_array_length_id(typ_id) {
		if element := fastc_fixed_array_element_type(typ_id) {
			count := g.fixed_array_length_value(length) or { return typ }
			return '[${count}]' + g.typeof_display_name(element)
		}
	}
	key_id := g.semantic_type_key(typ_id)
	key := flat.source_type_name(key_id)
	if key_id in g.declared_types {
		syntax := flat.source_type_syntax(key_id)
		if syntax.dotted_parts().len <= 1 { return key }
		name := flat.source_type_name(syntax.unqualified())
		if syntax.qualifier() in [flat.source_type_id('main'), flat.source_type_id('builtin')] {
			return name
		}
		return flat.source_type_name(flat.source_type_syntax(syntax.qualifier()).unqualified()) + '.' + name
	}
	return typ
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
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0 || i + 1 >= tokens.len {
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
			left = g.render_comparison_operand(left_tokens, flat.source_type_id('int')) or { return none }
		}
		mut right := ''
		if right_value := right_typeof {
			right = right_value.source
		} else {
			right = g.render_comparison_operand(right_tokens, flat.source_type_id('int')) or { return none }
		}
		return FastcRenderedExpression{
			source: '((${left})${item.tok.str()}(${right}))'
			typ:    flat.source_type_bool
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
	mut c_type := u32(0)
	// The qualified-type check below reads the token where the type name begins; skip a leading
	// `&`/`&&` pointer prefix or a `?` option-cast prefix (`?flat.NodeId(none)`) so a module
	// qualifier (`flat.`) is recognized rather than mistaken for a `receiver.method(` call.
	type_qualifier_start := if tokens.len > 0 && tokens[0].tok in [.amp, .and, .question] {
		1
	} else {
		0
	}
	for i, item in tokens {
		if item.tok != .lpar || i <= type_start {
			continue
		}
		if i >= 2 && tokens[i - 2].tok == .dot
			&& g.type_from_expression_tokens(tokens[type_start..i]) == none {
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
		if i == type_qualifier_start + 3 && tokens[type_qualifier_start].tok == .name && tokens[type_qualifier_start].lit == 'C' && ('C.${tokens[type_qualifier_start + 2].lit}' in g.functions || !fastc_call_has_one_argument(tokens, i)) {
			return none
		}
		c_type = (g.type_from_expression_tokens(tokens[type_start..i]) or { u32(0) })
		if c_type == u32(0) && i == 3 && tokens[0].tok == .name && tokens[0].lit == 'C' && tokens[1].tok == .dot && tokens[2].tok == .name && tokens[2].lit.len > 0 && tokens[2].lit[0].is_capital() && 'C.${tokens[2].lit}' !in g.functions {
			c_type = flat.source_type_id(tokens[2].lit)
		}
		if c_type != u32(0) {
			open = i
		}
		break
	}
	if open <= 0 || c_type == u32(0) {
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
				typ:    fastc_type_option
			}
		}
		inner_type := (g.infer_expression_type(inner_tokens) or { u32(0) })
		if inner_type == fastc_type_result {
			return none
		}
		inner := g.render_call_argument_expression(inner_tokens, flat.source_type_id(if inner_type == fastc_type_option {
			'Option'
		} else {
			flat.source_type_name(c_type)
		})) or { return none }
		return FastcRenderedExpression{
			source: if inner_type == fastc_type_option {
				inner
			} else {
				g.option_success_expression(flat.source_type_id('Option'), c_type, inner)
			}
			typ:    fastc_type_option
		}
	}
	// A conversion into a boxed sum type (`Expr(EmptyExpr(0))`) is a box, not a C cast: the
	// concrete variant value is stored behind `_object` with its own type id. Casting the
	// variant straight to the `{_object,_typ}` struct is invalid C, and matters for
	// primitive-alias variants (`type EmptyExpr = u8`) that cannot be reinterpret-cast at all.
	if g.selfhost && !fastc_is_pointer_type(c_type) && flat.source_type_name(fastc_trim_pointer_type(c_type)) in g.sum_types {
		inner_type := (g.infer_expression_type(inner_tokens) or { u32(0) })
		variant := fastc_trim_pointer_type(fastc_normalize_inferred_type(inner_type))
		if variant != 0 && variant != fastc_trim_pointer_type(c_type) && g.sumtype_has_variant(c_type, variant) {
			// A member/local chain must render through render_member_receiver so a live member
			// smart-cast (`if left is CS { Expr(left) }`) supplies the concrete variant pointer
			// (`((CS *)left->_object)`) rather than the boxed subject itself.
			boxed_inner := if member := g.render_member_receiver(inner_tokens) {
				member
			} else {
				g.render_call_argument_expression(inner_tokens, inner_type) or { return none }
			}
			object := if fastc_is_pointer_type(inner_type) {
				'(void*)(${boxed_inner})'
			} else {
				fastc_box_expression(flat.source_type_name(variant), boxed_inner)
			}
			return FastcRenderedExpression{
				source: '(${flat.source_type_name(c_type)}){._object=${object}, ._typ=__v_typeid_${flat.source_type_name(variant)}}'
				typ:    c_type
			}
		}
	}
	// A conversion into an alias of a fixed array (`Gf([16]i64{})`) only renames the C struct
	// both types spell, and C cannot cast to a struct; `[N]T{}` is the zero value.
	aliased := g.underlying_alias_type(c_type)
	if native_pointer := g.render_native_function_pointer(inner_tokens, c_type) {
		return FastcRenderedExpression{
			source: '((${fastc_output_c_type(flat.source_type_name(c_type))})(${native_pointer}))'
			typ:    c_type
		}
	}
	if !flat.source_type_query(c_type).has(.c_pointer_suffix) && aliased != c_type && flat.source_type_query(aliased).has(.c_fixed_array_prefix) {
		if inner_tokens.len >= 5 && inner_tokens[0].tok == .lsbr && inner_tokens[2].tok == .rsbr
			&& inner_tokens[inner_tokens.len - 2].tok == .lcbr && inner_tokens.last().tok == .rcbr {
			return FastcRenderedExpression{
				source: '((${fastc_output_c_type(flat.source_type_name(c_type))}){0})'
				typ:    c_type
			}
		}
		aliased_value := g.render_call_argument_expression(inner_tokens, aliased) or { return none }
		return FastcRenderedExpression{
			source: '(${aliased_value})'
			typ:    c_type
		}
	}
	inner_type := g.infer_expression_type(inner_tokens) or { u32(0) }
	if g.declared_kinds[g.semantic_type_key(c_type)] == .interface_ {
		inner := g.render_call_argument_expression(inner_tokens, inner_type) or { return none }
		return FastcRenderedExpression{
			source: g.interface_value_expression(c_type, inner_type, inner)
			typ:    c_type
		}
	}
	if g.semantic_type_key(c_type) in g.declared_kinds
		&& g.declared_kinds[g.semantic_type_key(c_type)] in [
			.struct_,
			.union_,
		]
		&& g.is_boxed_type(inner_type) {
		inner := g.render_call_argument_expression(inner_tokens, inner_type) or { return none }
		separator := if fastc_is_pointer_type(g.underlying_alias_type(inner_type)) {
			'->'
		} else {
			'.'
		}
		pointer_type := if flat.source_type_query(c_type).has(.c_pointer_suffix) {
			fastc_output_c_type(flat.source_type_name(c_type))
		} else {
			fastc_output_c_type(flat.source_type_name(c_type)) + '*'
		}
		pointer := '((${pointer_type})((${inner})${separator}_object))'
		return FastcRenderedExpression{
			source: if flat.source_type_query(c_type).has(.c_pointer_suffix) {
				pointer
			} else {
				'*(${pointer})'
			}
			typ:    c_type
		}
	}
	// Explicit pointer and integer address casts preserve the operand's bits.
	// Passing the destination type here could auto-dereference a pointer operand.
	is_pointer_cast := fastc_is_pointer_type(aliased)
	is_integer_cast := fastc_is_integer_type(aliased)
	mut inner_expected_type := if is_pointer_cast
		|| (is_integer_cast && fastc_is_pointer_type(g.underlying_alias_type(inner_type))) {
		inner_type
	} else {
		c_type
	}
	if is_integer_cast && !g.in_mono_drain && !g.in_generic_placeholder {
		operand := fastc_strip_paren_tokens(inner_tokens)
		if operand.len == 1 && operand[0].tok == .name {
			if local := g.locals[operand[0].lit] {
				if local.is_reference && flat.source_type_query(local.typ).has(.c_pointer_suffix) {
					value_type := flat.source_type_syntax(local.typ).c_pointer_base()
					if fastc_is_integer_type(g.underlying_alias_type(value_type)) {
						// Concrete mutable scalar parameters are a FastC extension;
						// integer conversions retain their existing value semantics.
						inner_expected_type = value_type
					}
				}
			}
		}
	}
	inner := g.render_call_argument_expression(inner_tokens, inner_expected_type) or { return none }
	return FastcRenderedExpression{
		source: '((${fastc_output_c_type(flat.source_type_name(c_type))})(${inner}))'
		typ:    c_type
	}
}

// render_c_interface_object_address lowers the C-interop escape hatch used to free
// an interface object: `&C.mod__Interface(value)._object`. The raw `C.Type(value)`
// spelling denotes a pointer reinterpretation here; leaving it untouched makes C
// parse `Type` as a function call.
fn (g &Parser) render_c_interface_object_address(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len != 9 || tokens[0].tok != .amp || tokens[1].tok != .name || tokens[1].lit != 'C' || tokens[2].tok != .dot || tokens[3].tok != .name || tokens[4].tok != .lpar || tokens[6].tok != .rpar || tokens[7].tok != .dot || tokens[8].tok != .name || tokens[8].lit != '_object' {
		return none
	}
	close := fastc_matching_rpar(tokens, 4) or { return none }
	if close != 6 {
		return none
	}
	c_type := tokens[3].lit
	type_key := if c_type_id := flat.source_type_existing_id(c_type) {
		g.semantic_type_key(c_type_id)
	} else {
		g.declared_type_c_names.type_id(c_type) or { return none }
	}
	if g.declared_kinds[type_key] != .interface_ {
		return none
	}
	inner := g.render_call_argument_expression(tokens[5..6], flat.source_type_id('${c_type}*')) or { return none }
	return FastcRenderedExpression{
		source: '&(((${c_type} *)(${inner}))->_object)'
		typ:    flat.source_type_id('voidptr*')
	}
}

fn (g &Parser) render_c_struct_sizeof(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len != 6 || tokens[0].tok != .key_sizeof || tokens[1].tok != .lpar || tokens[2].tok != .name || tokens[2].lit != 'C' || tokens[3].tok != .dot || tokens[4].tok != .name || tokens[5].tok != .rpar {
		return none
	}
	c_name := tokens[4].lit
	if !fastc_source_type_key_exists(g.declared_types, '#Cstruct#${c_name}') {
		return none
	}
	return FastcRenderedExpression{
		source: 'sizeof(struct ${c_name})'
		typ:    flat.source_type_int
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
		if tokens[i].tok != .name || tokens[i].lit !in ['has', 'set', 'clear'] || tokens[i - 1].tok != .dot || i + 2 >= tokens.len || tokens[i + 1].tok != .lpar {
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
			if tokens[argument_index].tok == .dot && argument_index + 1 < call_end && tokens[argument_index + 1].tok == .name {
				// The raw renderer mangles a `.member` shorthand whose name is a C keyword
				// (`.unsigned` -> `.__vf_keyword_unsigned`); match that spelling so the
				// needle still finds the call. The enum constant itself keeps the raw name.
				raw_argument.write_string('.${fastc_c_identifier(tokens[argument_index + 1].lit)}')
				c_argument.write_string('${fastc_c_declared_type_name(receiver_key)}__${tokens[argument_index + 1].lit}')
				argument_index += 2
				continue
			}
			item := tokens[argument_index]
			piece := if item.tok in [.name, .number] { item.lit } else { item.tok.str() }
			raw_argument.write_string(piece)
			c_argument.write_string(if item.tok == .name {
				g.resolved_expression_name(item.lit, tokens[argument_index - 1].tok)
			} else {
				piece
			})
			argument_index++
		}
		method := tokens[i].lit
		raw_receiver_source := fastc_take_string(mut receiver)
		receiver_source := g.render_member_receiver(receiver_tokens) or { raw_receiver_source }
		raw_argument_source := fastc_take_string(mut raw_argument)
		c_argument_source := fastc_take_string(mut c_argument)
		mut needle := '${raw_receiver_source}.${method}(${c_argument_source})'
		if !fastc_contains(rendered, needle) {
			needle = '${receiver_source}.${method}(${c_argument_source})'
		}
		if !fastc_contains(rendered, needle) {
			needle = '${raw_receiver_source}.${method}(${raw_argument_source})'
		}
		if !fastc_contains(rendered, needle) {
			needle = '${receiver_source}.${method}(${raw_argument_source})'
		}
		replacement := match method {
			'has' { '((${receiver_source} & ${c_argument_source}) != 0)' }
			'set' { '((${receiver_source}) |= (${c_argument_source}))' }
			else { '((${receiver_source}) &= ~(${c_argument_source}))' }
		}
		if receiver_start == 0 && call_end == tokens.len - 1 {
			// A standalone flag-method call (`t.a.nodes.flags.set(.nogrow)` as a whole expression):
			// return the reconstructed operation directly. render_raw's buffer can spell a pointer
			// FIELD receiver inconsistently (`t->a.nodes` not `t->a->nodes`) so no needle matches;
			// render_member_receiver spells it correctly.
			return FastcRenderedExpression{
				source: replacement
				typ:    (if method == 'has' { flat.source_type_bool } else { flat.source_type_void })
			}
		}
		if !fastc_contains(rendered, needle) {
			continue
		}
		rendered = fastc_replace(rendered, needle, replacement)
		if method == 'has' {
			result_type = 'bool'
		}
		changed = true
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    flat.source_type_id(result_type)
		}
	} else {
		none
	}
}

fn (g &Parser) render_static_call_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
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
			replaced := fastc_replace(rendered, raw_call, disabled_call)
			if replaced.str != rendered.str {
				rendered = replaced
				changed = true
			}
			continue
		}
		type_key := function_key.all_before_last('.')
		owner := fastc_c_declared_type_name(type_key)
		method_c_name := g.c_function_name_or(function_key, fastc_method_c_name_for_key(type_key, tokens[i].lit))
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
		if named_start >= 0 && named_start == signature.parameter_types.ids.len - 1 && named_start <= call_args.len && (signature.last_parameter_is_params || g.fastc_type_is_declared_struct(signature.parameter_types.ids[named_start])) {
			mut rendered_arguments := []string{cap: signature.parameter_types.ids.len}
			mut arguments_ok := true
			for argument_index, argument in call_args[..named_start] {
				rendered_argument := g.render_call_argument_expression(argument, signature.parameter_types.ids[argument_index]) or {
					arguments_ok = false
					break
				}
				rendered_arguments << rendered_argument
			}
			if arguments_ok {
				named_initializer := g.render_named_struct_initializer(signature.parameter_types.ids[named_start], call_args[named_start..]) or { '' }
				if named_initializer != '' {
					rendered_arguments << named_initializer
					call_source := '${method_c_name}(${rendered_arguments.join(',')})'
					if call_start == 0 && call_end == tokens.len - 1 {
						return FastcRenderedExpression{
							source: call_source
							typ:    signature.return_type
						}
					}
					raw_call := g.render_raw_expression_tokens(tokens[call_start..call_end + 1]) or {
						continue
					}
					replaced := fastc_replace(rendered, raw_call, call_source)
					if replaced.str != rendered.str {
						rendered = replaced
						changed = true
						continue
					}
				}
			}
		}
		if call_args.len == signature.parameter_types.ids.len {
			mut rendered_arguments := []string{cap: call_args.len}
			mut arguments_ok := true
			for argument_index, argument in call_args {
				rendered_argument := g.render_call_argument_expression(argument, signature.parameter_types.ids[argument_index]) or {
					arguments_ok = false
					break
				}
				rendered_arguments << rendered_argument
			}
			if arguments_ok {
				call_source := '${method_c_name}(${rendered_arguments.join(',')})'
				if call_start == 0 && call_end == tokens.len - 1 {
					return FastcRenderedExpression{
						source: call_source
						typ:    signature.return_type
					}
				}
				raw_call := g.render_raw_expression_tokens(tokens[call_start..call_end + 1]) or {
					continue
				}
				replaced := fastc_replace(rendered, raw_call, call_source)
				if replaced.str != rendered.str {
					rendered = replaced
					changed = true
					continue
				}
			}
		}
		mut needle := '${owner}.${tokens[i].lit}('
		if !fastc_contains(rendered, needle) {
			needle = '${owner}__${tokens[i].lit}('
		}
		if !fastc_contains(rendered, needle) {
			continue
		}
		rendered = fastc_replace(rendered, needle, '${method_c_name}(')
		changed = true
	}
	if !changed {
		return none
	}
	// Rewriting an embedded static call must still lower its enclosing call.
	if method := g.render_method_call_expression(tokens, rendered) {
		return method
	}
	if call := g.render_missing_call_arguments(tokens) {
		return call
	}
	return FastcRenderedExpression{
		source: rendered
		typ:    g.infer_expression_type(tokens) or { u32(0) }
	}
}

fn (g &Parser) method_function_key(receiver_type u32, name string) string {
	if by_name := g.method_key_memo[receiver_type] {
		if cached := by_name[name] {
			return cached
		}
	}
	key := g.method_function_key_impl(receiver_type, name)
	mut w := unsafe { &Parser(g) }
	if receiver_type !in w.method_key_memo {
		w.method_key_memo[receiver_type] = map[string]string{}
	}
	w.method_key_memo[receiver_type][name] = key
	return key
}

fn (g &Parser) method_function_key_impl(receiver_type u32, name string) string {
	// Literal-only arithmetic keeps its precise inference category for assignment
	// validation, but method lookup needs the concrete default V type. Without
	// this, `(8 * 1024 * 1024).str()` is left as raw C `.str()` syntax.
	method_receiver_type := fastc_normalize_inferred_type(receiver_type)
	direct_key := '${flat.source_type_name(g.semantic_type_key(method_receiver_type))}.${name}'
	if direct_key in g.functions {
		return direct_key
	}
	// A type alias (`type GitlyDb = sqlite.DB`) inherits its base type's methods, so a
	// method missing on the alias resolves against the underlying type.
	base := g.underlying_alias_type(method_receiver_type)
	if base != method_receiver_type {
		base_key := '${flat.source_type_name(g.semantic_type_key(base))}.${name}'
		if base_key in g.functions {
			return base_key
		}
	}
	if g.selfhost && name in ['keys', 'values'] && 'map.${name}' in g.functions {
		return 'map.${name}'
	}
	mut layout_type_id := fastc_trim_pointer_type(g.map_type_through_alias(method_receiver_type))
	if flat.source_type_query(layout_type_id).has(.c_array_prefix) {
		layout_type_id = fastc_type_array
	} else if flat.source_type_query(layout_type_id).has(.c_map_prefix) {
		if 'map.${name}' in g.functions {
			return 'map.${name}'
		}
		layout_type_id = fastc_type_map
	}
	if fields := g.struct_fields[flat.source_type_name(layout_type_id)] {
		if fields.contains('data') && fields.contains('len') && fields.contains('cap') && 'array.${name}' in g.functions {
			return 'array.${name}'
		}
	}
	return direct_key
}

// resolve_method finds a method for `receiver_type`, promoting through embedded
// fields (`__embedded_N`) when the receiver has no direct method of that name (as
// V does with struct embedding). It returns the resolved function key and the
// member-access path to the embedded field whose type owns the method (empty for
// a direct method).
fn (g &Parser) resolve_method(receiver_type u32, name string) (string, []string) {
	direct := g.method_function_key(receiver_type, name)
	if direct in g.functions {
		return direct, []string{}
	}
	layout_type := flat.source_type_name(fastc_trim_pointer_type(receiver_type))
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

fn (g &Parser) specialized_method_return_type(receiver_type u32, method_key string, signature FastcFunctionSignature) u32 {
	if method_key in ['map.keys', 'map.values'] {
		key_type, value_type := g.map_key_value_types(receiver_type) or { return signature.return_type }
		element_type := if method_key == 'map.keys' { key_type } else { value_type }
		return fastc_array_c_type_id(element_type)
	}
	if method_key.starts_with('array.') {
		if element_type := g.array_element_type(receiver_type) {
			method_name := method_key.all_after_last('.')
			if method_name in ['first', 'last', 'pop', 'pop_left', 'get', 'get_unsafe', 'get_i64',
				'get_u64', 'get_ni', 'get_with_check', 'get_with_check_i64', 'get_with_check_u64',
				'get_with_check_ni'] {
				return element_type
			}
			if signature.return_type == fastc_type_array {
				return fastc_array_c_type_id(element_type)
			}
		}
	}
	if method_key.starts_with('map.') && signature.return_type == fastc_type_map {
		base := fastc_trim_pointer_type(receiver_type)
		if flat.source_type_query(base).has(.c_map_prefix) {
			return base
		}
	}
	return signature.return_type
}

fn (g &Parser) render_interface_cast_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens.last().tok != .rpar {
		return none
	}
	// The cast type may be unqualified (`Sum(value)`, type at token 0, `(` at 1) or
	// qualified (`mod.Sum(value)`, type at token 2, `(` at 3).
	mut type_key := u32(0)
	mut open_index := 0
	if tokens[0].tok == .name && tokens[1].tok == .lpar {
		type_key = g.resolve_declared_type_key(tokens[0].lit) or { return none }
		open_index = 1
	} else if tokens.len >= 6 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name && tokens[3].tok == .lpar && tokens[0].lit in g.imports {
		qualified_key := fastc_type_key(g.imports[tokens[0].lit], tokens[2].lit)
		if !fastc_source_type_key_exists(g.declared_types, qualified_key) {
			return none
		}
		type_key = flat.source_type_id(qualified_key)
		open_index = 3
	} else {
		return none
	}
	interface_type := fastc_c_declared_type_name(flat.source_type_name(type_key))
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
	if actual_type == u32(0) {
		return none
	}
	inner_tokens := tokens[open_index + 1..close]
	// A composite operand (array / map literal) does not survive as raw streamed
	// text (`[1,2,3]` is not valid C), so render it through the argument path.
	// A struct literal likewise needs its designated-initializer lowering before boxing.
	// Scalars and pointers keep the already-streamed spelling.
	actual_base_query_type_id := fastc_trim_pointer_type(actual_type)
	inner_source := if struct_literal := g.render_struct_literal_expression(inner_tokens) {
		struct_literal.source
	} else if flat.source_type_query(actual_base_query_type_id).has(.c_array_prefix) || flat.source_type_query(actual_base_query_type_id).has(.c_map_prefix) {
		g.render_call_argument_expression(inner_tokens, actual_type) or { return none }
	} else if g.selfhost && g.expression_uses_member_smartcast(inner_tokens) {
		// A narrowed subject (`Expr(left)` where `left is CS`) must box the concrete-variant
		// pointer the smart-cast supplies, not the boxed subject's raw streamed text.
		g.render_member_receiver(inner_tokens) or {
			rendered_expression[prefix.len..rendered_expression.len - 2]
		}
	} else if g.selfhost && fastc_bare_as_cast_index(inner_tokens, 0, inner_tokens.len) != none {
		// An `as`-cast operand (`AsmArg(x as AsmRegister)`): the raw streamed text keeps the V
		// `as` keyword, which is not valid C, so lower it through the as-cast renderer.
		if as_expr := g.render_as_cast_expression(inner_tokens) {
			as_expr.source
		} else {
			g.render_call_argument_expression(inner_tokens, actual_type) or {
				rendered_expression[prefix.len..rendered_expression.len - 2]
			}
		}
	} else if g.selfhost && fastc_expression_tokens_contain(inner_tokens, .lpar) {
		// A call operand (`Stmt(f(node.arr[0]))`): the raw streamed text leaves the call's
		// arguments un-lowered (e.g. an array index on a match-cast member stays an invalid C
		// `[0]` on the array header), so render it through the argument path.
		g.render_call_argument_expression(inner_tokens, actual_type) or {
			rendered_expression[prefix.len..rendered_expression.len - 2]
		}
	} else {
		rendered_expression[prefix.len..rendered_expression.len - 2]
	}
	if g.declared_kinds[g.semantic_type_key(actual_type)] == .interface_ {
		// A mutable interface parameter is stored by pointer. Resolve its storage
		// before copying the carrier so the concrete object and tag survive the cast.
		interface_source := g.render_call_argument_expression(inner_tokens, actual_type) or {
			inner_source
		}
		return FastcRenderedExpression{
			source: g.interface_value_expression(flat.source_type_id(interface_type), actual_type, interface_source)
			typ:    flat.source_type_id(interface_type)
		}
	}
	mut box_type := actual_type
	if fastc_is_pointer_type(actual_type) && inner_tokens.len == 1 && inner_tokens[0].tok == .name {
		if local := g.locals[inner_tokens[0].lit] {
			// `Expr(node)` where `node` is a `mut T` parameter (a C `T*`): the streamed operand
			// is auto-dereferenced to the pointee value (`*(node)`), so box that value rather than
			// letting interface_value_expression's pointer branch cast a struct value to `void*`.
			if local.is_reference && local.typ == actual_type {
				box_type = fastc_trim_pointer_type(actual_type)
			}
		}
	}
	return FastcRenderedExpression{
		source: g.interface_value_expression(flat.source_type_id(interface_type), box_type, inner_source)
		typ:    flat.source_type_id(interface_type)
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
	key_type_source_id, value_type_source_id := g.map_key_value_types(map_type) or { return none }
	key_type := key_type_source_id
	value_type := value_type_source_id

	key_source := g.render_call_argument_expression(tokens[open + 1..tokens.len - 1], key_type) or {
		return none
	}
	mut map_address := ''
	if nested := g.render_mutable_map_value_pointer(base_tokens) {
		if nested.typ != fastc_trim_pointer_type(map_type) {
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
		map_address = if flat.source_type_query(map_type).has(.c_pointer_suffix) {
			map_source
		} else {
			'&(${map_source})'
		}
	}
	empty_value := g.map_lookup_missing_value_expression(value_type)
	key_storage_type := g.collection_runtime_type(key_type)
	value_storage_type := g.collection_storage_type(value_type)
	return FastcRenderedExpression{
		source: '({ ${key_storage_type} __vf_nested_map_key = (${key_source}); ${value_storage_type} *__vf_nested_map_value = (${value_storage_type} *)builtin__map_get_check((map *)(${map_address}), &__vf_nested_map_key); if (__vf_nested_map_value == NULL) { ${value_storage_type} __vf_nested_map_empty = ${empty_value}; builtin__map_set((map *)(${map_address}), &__vf_nested_map_key, &__vf_nested_map_empty); __vf_nested_map_value = (${value_storage_type} *)builtin__map_get_check((map *)(${map_address}), &__vf_nested_map_key); } __vf_nested_map_value; })'
		typ:    value_type
	}
}
