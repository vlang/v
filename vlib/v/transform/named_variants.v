module transform

import v.flat
import v.types

// named_variant_binding_name returns the payload binding of a `match` branch
// pattern, `n` in `Expr.Count(n)`, or '' when the pattern binds nothing.
fn (t &Transformer) named_variant_binding_name(cond_id flat.NodeId) string {
	if int(cond_id) < 0 || int(cond_id) >= t.a.nodes.len {
		return ''
	}
	binding_id := types.named_variant_pattern_binding(t.a, t.a.node(cond_id)) or { return '' }
	return t.a.node(binding_id).value
}

// named_variant_binding_decls returns the declaration `n := subject.payload`
// for a single pattern `match` branch that binds a payload, `Expr.Count(n)`.
// It is transformed as the first statement of the branch body, while the
// subject is narrowed to the variant, so `n` is an ordinary immutable local.
fn (mut t Transformer) named_variant_binding_decls(match_expr_id flat.NodeId, branch flat.Node) []flat.NodeId {
	if isnil(t.tc) || branch.value == 'else' || t.count_conds(branch) != 1 {
		return []flat.NodeId{}
	}
	cond_id := t.a.child(&branch, 0)
	binding := t.named_variant_binding_name(cond_id)
	if binding.len == 0 {
		return []flat.NodeId{}
	}
	subject := t.expr_key(match_expr_id)
	sc := t.match_type_smartcast_context(match_expr_id, cond_id) or { return []flat.NodeId{} }
	payload_type := t.tc.named_variant_payload_type(sc.variant_name) or {
		return []flat.NodeId{}
	}
	if subject.len == 0 {
		return []flat.NodeId{}
	}
	payload_name := t.tc.type_name(payload_type)
	base := t.make_ident(subject)
	t.set_node_typ(int(base), sc.variant_name)
	payload := t.make_selector_op(base, flat.named_variant_payload_field, payload_name, .dot)
	return [t.make_decl_assign_typed(binding, payload, payload_name)]
}

// named_variant_display_short returns the source spelling of a hidden variant
// struct without its module, `Expr.Count` for `mod.Expr@variant@Count`.
fn named_variant_display_short(variant string) string {
	marker := variant.index(flat.named_variant_marker) or { return variant }
	prefix := variant[..marker]
	dot := prefix.last_index_u8(`.`)
	short := if dot >= 0 { variant[dot + 1..] } else { variant }
	return flat.demangle_named_variants(short)
}

// named_variant_str stringifies a hidden variant struct value as
// `Expr.Count(3)`, `Expr.Str('text')` or `Expr.Void`.
fn (mut t Transformer) named_variant_str(expr flat.NodeId, variant string, is_ref bool) flat.NodeId {
	display := named_variant_display_short(variant)
	if isnil(t.tc) {
		return t.make_string_literal(display)
	}
	payload_type := t.tc.named_variant_payload_type(variant) or {
		return t.make_string_literal(display)
	}
	// Keep the payload's variant on the stack, as ordinary struct formatting
	// does, so an array of the enclosing sum is not mistaken for a direct cycle.
	t.stringify_stack << variant
	defer {
		t.stringify_stack.delete_last()
	}
	payload_name := t.tc.type_name(payload_type)
	mut value := expr
	if is_ref {
		value = t.make_prefix(.mul, expr)
		t.set_node_typ(int(value), variant)
	}
	payload := t.make_selector_op(value, flat.named_variant_payload_field, payload_name, .dot)
	mut text := t.wrap_string_conversion(payload, payload_name)
	clean_payload := t.normalize_type_alias(payload_name)
	if clean_payload == 'string' {
		text = t.string_plus(t.string_plus(t.make_string_literal("'"), text), t.make_string_literal("'"))
	} else if clean_payload == 'rune' {
		text = t.string_plus(t.string_plus(t.make_string_literal('`'), text), t.make_string_literal('`'))
	}
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), text), t.make_string_literal(')'))
}

// demangle_named_variant_literal makes a type name string literal, such as the
// result of `typeof(x).name`, spell a sum type variant as `Expr.Count`.
fn (mut t Transformer) demangle_named_variant_literal(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && flat.is_named_variant_type_name(node.value) {
		return t.make_string_literal(flat.demangle_named_variants(node.value))
	}
	return id
}
