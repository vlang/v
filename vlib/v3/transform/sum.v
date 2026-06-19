module transform

import v3.flat

fn (t &Transformer) trim_pointer_type(typ string) string {
	if typ.starts_with('&') {
		return typ[1..]
	}
	return typ
}

fn (t &Transformer) resolve_variant(sum_name string, variant string) string {
	if variant.contains('.') {
		return variant
	}
	resolved_sum := t.resolve_sum_name(sum_name)
	if resolved_sum.contains('.') {
		return '${resolved_sum.all_before_last('.')}.${variant}'
	}
	if sum_name.contains('.') {
		return '${sum_name.all_before_last('.')}.${variant}'
	}
	return variant
}

fn (t &Transformer) resolve_sum_name(sum_name string) string {
	if sum_name in t.sum_types {
		return sum_name
	}
	if sum_name.contains('.') {
		short_sum := sum_name.all_after_last('.')
		if short_sum in t.sum_types {
			return short_sum
		}
	}
	if !sum_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qsum := '${t.cur_module}.${sum_name}'
		if qsum in t.sum_types {
			return qsum
		}
	}
	if !isnil(t.tc) {
		if sum_name in t.tc.sum_types {
			return sum_name
		}
		if sum_name.contains('.') {
			short_sum := sum_name.all_after_last('.')
			if short_sum in t.tc.sum_types {
				return short_sum
			}
		}
		if !sum_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qsum := '${t.cur_module}.${sum_name}'
			if qsum in t.tc.sum_types {
				return qsum
			}
		}
	}
	return sum_name
}

fn (t &Transformer) sum_type_index(sum_name string, variant string) int {
	resolved_sum := t.resolve_sum_name(sum_name)
	variants := t.sum_types[resolved_sum] or {
		if !isnil(t.tc) {
			tc_variants := t.tc.sum_types[resolved_sum] or { return 0 }
			return sum_type_index_in_variants(tc_variants, variant)
		}
		return 0
	}
	return sum_type_index_in_variants(variants, variant)
}

fn sum_type_index_in_variants(variants []string, variant string) int {
	short_variant := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	for i, v in variants {
		short_v := if v.contains('.') { v.all_after_last('.') } else { v }
		if v == variant || short_v == short_variant {
			return i + 1
		}
	}
	return 0
}

fn (mut t Transformer) transform_is_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	expr_type := t.node_type(expr_id)
	clean_type := t.trim_pointer_type(expr_type)
	if clean_type !in t.sum_types {
		return t.make_bool_literal(true)
	}
	new_expr := t.transform_expr(expr_id)
	tag := t.make_selector_op(new_expr, 'typ', 'int', if expr_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	idx := t.make_int_literal(t.sum_type_index(clean_type, node.value))
	return t.make_infix(.eq, tag, idx)
}

fn (mut t Transformer) transform_as_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	expr_type := t.node_type(expr_id)
	clean_type := t.trim_pointer_type(expr_type)
	if clean_type !in t.sum_types {
		return t.transform_expr(expr_id)
	}
	sc_key := t.expr_key(expr_id)
	if sc_key.len > 0 {
		if sc := t.find_smartcast(sc_key) {
			mut sc_variant := sc.variant_name
			if sc.variant_name.contains('.') {
				sc_variant = sc.variant_name.all_after_last('.')
			}
			mut target_variant := node.value
			if node.value.contains('.') {
				target_variant = node.value.all_after_last('.')
			}
			if sc_variant == target_variant {
				return t.transform_expr(expr_id)
			}
		}
	}
	qv := t.resolve_variant(clean_type, node.value)
	field := t.sum_field_name(qv)
	new_expr := t.transform_expr(expr_id)
	use_ptr := t.variant_references_sum(qv, clean_type)
	field_typ := if use_ptr { '&${qv}' } else { qv }
	field_sel := t.make_selector_op(new_expr, field, field_typ, if expr_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	if use_ptr {
		return t.make_prefix(.mul, field_sel)
	}
	return field_sel
}

fn (mut t Transformer) wrap_sum_return_expr(expr_id flat.NodeId) flat.NodeId {
	if t.cur_fn_ret_type.len == 0 || t.cur_fn_ret_type !in t.sum_types {
		return t.transform_expr(expr_id)
	}
	expr := t.a.nodes[int(expr_id)]
	expr_type := t.node_type(expr_id)
	mut variant := expr_type
	if expr.kind == .struct_init || expr.kind == .cast_expr {
		variant = expr.value
	}
	if variant.len == 0 {
		return t.transform_expr(expr_id)
	}
	short_variant := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	mut matches := false
	for v in t.sum_types[t.cur_fn_ret_type] {
		short_v := if v.contains('.') { v.all_after_last('.') } else { v }
		if v == variant || short_v == short_variant {
			matches = true
			break
		}
	}
	if !matches {
		return t.transform_expr(expr_id)
	}
	inner := t.transform_expr(expr_id)
	start := t.a.children.len
	t.a.children << inner
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          t.cur_fn_ret_type
		children_start: start
		children_count: 1
		typ:            t.cur_fn_ret_type
	})
}
