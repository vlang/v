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
	if resolved_variant := t.sum_variant_name(resolved_sum, variant) {
		return resolved_variant
	}
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
	if !sum_name.contains('.') {
		mut found := ''
		for key, _ in t.sum_types {
			if key.contains('.') && key.all_after_last('.') == sum_name {
				if found.len > 0 && found != key {
					found = ''
					break
				}
				found = key
			}
		}
		if found.len > 0 {
			return found
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
		if !sum_name.contains('.') {
			mut found := ''
			for key, _ in t.tc.sum_types {
				if key.contains('.') && key.all_after_last('.') == sum_name {
					if found.len > 0 && found != key {
						found = ''
						break
					}
					found = key
				}
			}
			if found.len > 0 {
				return found
			}
		}
	}
	return sum_name
}

fn (t &Transformer) is_sum_type_name(name string) bool {
	if name.len == 0 {
		return false
	}
	resolved := t.resolve_sum_name(name)
	return resolved in t.sum_types
}

fn (t &Transformer) is_interface_type_name(name string) bool {
	if name.len == 0 || isnil(t.tc) {
		return false
	}
	clean := t.trim_pointer_type(t.normalize_type_alias(name))
	if clean == 'IError' || clean in t.tc.interface_names {
		return true
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		return '${t.cur_module}.${clean}' in t.tc.interface_names
	}
	return false
}

fn (t &Transformer) interface_variant_type(variant string) string {
	if variant.contains('.') {
		return variant
	}
	if variant in t.structs {
		return variant
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qvariant := '${t.cur_module}.${variant}'
		if qvariant in t.structs {
			return qvariant
		}
	}
	for name, _ in t.structs {
		if name.contains('.') && name.all_after_last('.') == variant {
			return name
		}
	}
	return variant
}

fn (t &Transformer) smartcast_target_type(sc SmartcastContext) string {
	if t.is_interface_type_name(sc.sum_type_name) {
		return t.interface_variant_type(sc.variant_name)
	}
	return t.resolve_variant(sc.sum_type_name, sc.variant_name)
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
	clean_type0 := t.trim_pointer_type(expr_type)
	clean_type := if clean_type0 in t.sum_types {
		clean_type0
	} else {
		t.find_sum_type_for_variant(node.value)
	}
	if t.is_interface_type_name(clean_type0) {
		new_expr := t.transform_expr(expr_id)
		object := t.make_selector_op(new_expr, '_object', 'voidptr', if expr_type.starts_with('&') {
			.arrow
		} else {
			.dot
		})
		return t.make_infix(.ne, object, t.a.add(.nil_literal))
	}
	if clean_type.len == 0 || clean_type !in t.sum_types {
		return t.make_bool_literal(true)
	}
	new_expr := t.transform_expr(expr_id)
	if check := t.make_sum_type_pattern_check(new_expr, expr_type, clean_type, node.value) {
		return check
	}
	return t.make_bool_literal(true)
}

fn (mut t Transformer) make_sum_is_check(expr flat.NodeId, expr_type string, sum_name string, variant string) flat.NodeId {
	tag := t.make_selector_op(expr, 'typ', 'int', if expr_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	idx := t.make_int_literal(t.sum_type_index(sum_name, variant))
	return t.make_infix(.eq, tag, idx)
}

fn (t &Transformer) sum_variant_path(sum_name string, variant string) []string {
	mut visited := map[string]bool{}
	return t.sum_variant_path_inner(sum_name, variant, mut visited)
}

fn (t &Transformer) sum_variant_path_inner(sum_name string, variant string, mut visited map[string]bool) []string {
	resolved_sum := t.resolve_sum_name(t.trim_pointer_type(sum_name))
	if resolved_sum.len == 0 || resolved_sum in visited {
		return []string{}
	}
	visited[resolved_sum] = true
	if direct := t.sum_variant_name(resolved_sum, variant) {
		return [direct]
	}
	variants := t.sum_types[resolved_sum] or { return []string{} }
	for direct in variants {
		direct_sum := t.resolve_sum_name(t.trim_pointer_type(direct))
		if direct_sum == resolved_sum || direct_sum !in t.sum_types {
			continue
		}
		nested := t.sum_variant_path_inner(direct_sum, variant, mut visited)
		if nested.len == 0 {
			continue
		}
		mut path := []string{cap: nested.len + 1}
		path << direct
		path << nested
		return path
	}
	return []string{}
}

fn (mut t Transformer) make_sum_type_pattern_check(expr flat.NodeId, expr_type string, sum_name string, variant string) ?flat.NodeId {
	resolved_sum := t.resolve_sum_name(t.trim_pointer_type(sum_name))
	path := t.sum_variant_path(resolved_sum, variant)
	if path.len == 0 {
		return none
	}
	mut current := expr
	mut current_type := expr_type
	mut current_sum := resolved_sum
	mut chain := flat.empty_node
	for i, path_variant in path {
		cmp := t.make_sum_is_check(current, current_type, current_sum, path_variant)
		if int(chain) < 0 {
			chain = cmp
		} else {
			chain = t.make_infix(.logical_and, chain, cmp)
		}
		if i == path.len - 1 {
			break
		}
		qv := t.resolve_variant(current_sum, path_variant)
		use_ptr := t.variant_references_sum(qv, current_sum)
		field_type := if use_ptr { '&${qv}' } else { qv }
		current = t.make_selector_op(current, t.sum_field_name(qv), field_type, if current_type.starts_with('&') {
			.arrow
		} else {
			.dot
		})
		current_type = field_type
		current_sum = t.resolve_sum_name(t.trim_pointer_type(qv))
	}
	return chain
}

fn (mut t Transformer) transform_as_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	expr_type := t.node_type(expr_id)
	clean_type0 := t.trim_pointer_type(expr_type)
	clean_type := if clean_type0 in t.sum_types {
		clean_type0
	} else {
		t.find_sum_type_for_variant(node.value)
	}
	if clean_type.len == 0 || clean_type !in t.sum_types {
		return t.transform_expr(expr_id)
	}
	qv := t.resolve_variant(clean_type, node.value)
	if qv.len > 0 && t.normalize_type_alias(clean_type0) == t.normalize_type_alias(qv) {
		return t.transform_expr(expr_id)
	}
	sc_key := t.expr_key(expr_id)
	if sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		mut matched_contexts := []SmartcastContext{}
		for i, sc in contexts {
			mut sc_variant := sc.variant_name
			if sc.variant_name.contains('.') {
				sc_variant = sc.variant_name.all_after_last('.')
			}
			mut target_variant := node.value
			if node.value.contains('.') {
				target_variant = node.value.all_after_last('.')
			}
			if sc_variant == target_variant {
				end := i + 1
				matched_contexts = contexts[..end].clone()
			}
		}
		if matched_contexts.len > 0 {
			base := t.make_plain_expr_for_smartcast(expr_id)
			return t.apply_smartcast_contexts(base, t.original_expr_type(expr_id), matched_contexts)
		}
	}
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
	was_return_expr := t.in_return_expr
	t.in_return_expr = true
	result := if t.cur_fn_ret_type.len == 0 || t.cur_fn_ret_type !in t.sum_types {
		t.transform_expr(expr_id)
	} else {
		t.wrap_sum_value(expr_id, t.cur_fn_ret_type)
	}
	t.in_return_expr = was_return_expr
	return result
}

fn (mut t Transformer) wrap_sum_value(expr_id flat.NodeId, target_sum string) flat.NodeId {
	resolved_sum := t.resolve_sum_name(target_sum)
	if resolved_sum.len == 0 || resolved_sum !in t.sum_types {
		return t.transform_expr(expr_id)
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .if_expr {
		branch_type := t.if_expr_branch_result_type(expr)
		if t.if_expr_branch_overrides_sum_target(branch_type, resolved_sum) {
			return t.transform_expr(expr_id)
		}
		if lowered := t.try_expand_if_expr_value_for_type(expr_id, expr, resolved_sum) {
			return lowered
		}
	}
	expr_type := t.node_type(expr_id)
	mut variant := expr_type
	mut expr_smartcast := SmartcastContext{}
	key := t.expr_key(expr_id)
	if key.len > 0 {
		if sc := t.find_smartcast(key) {
			expr_smartcast = sc
		}
	}
	has_expr_smartcast := expr_smartcast.expr_name.len > 0
	if has_expr_smartcast && t.resolve_sum_name(expr_smartcast.sum_type_name) == resolved_sum {
		variant = t.resolve_variant(expr_smartcast.sum_type_name, expr_smartcast.variant_name)
	}
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		inner_type := t.node_type(t.a.child(&expr, 0))
		if inner_type.starts_with('&') {
			variant = inner_type[1..]
		}
	}
	if expr.kind == .struct_init || expr.kind == .cast_expr {
		variant = expr.value
	} else if expr.kind == .as_expr && expr.value.len > 0 {
		variant = expr.value
	} else if expr.kind == .assoc && expr.value.len > 0 {
		variant = expr.value
	} else if expr.kind == .assoc && expr.children_count > 0 {
		variant = t.node_type(t.a.child(&expr, 0))
	}
	if expr.kind !in [.assoc, .as_expr, .prefix] && !has_expr_smartcast
		&& t.resolve_sum_name(expr_type) == resolved_sum {
		return t.transform_expr(expr_id)
	}
	if expr_type.starts_with('&') && t.resolve_sum_name(expr_type[1..]) == resolved_sum {
		inner := t.transform_expr(expr_id)
		deref := t.make_prefix(.mul, inner)
		t.a.nodes[int(deref)].typ = resolved_sum
		return deref
	}
	if variant.len == 0 {
		return t.transform_expr(expr_id)
	}
	mut clean_variant := if variant.starts_with('&') { variant[1..] } else { variant }
	if clean_variant.starts_with('ptr') && clean_variant.len > 3 && clean_variant[3..].contains('.') {
		clean_variant = clean_variant[3..]
	}
	if clean_variant.starts_with('ptr') && clean_variant.len > 3
		&& clean_variant[3..].contains('__') {
		clean_variant = clean_variant[3..].replace('__', '.')
	}
	short_variant := if clean_variant.contains('.') {
		clean_variant.all_after_last('.')
	} else {
		clean_variant
	}
	mut matches := false
	mut matched_variant := clean_variant
	for v in t.sum_types[resolved_sum] {
		short_v := if v.contains('.') { v.all_after_last('.') } else { v }
		if v == clean_variant || short_v == short_variant {
			matches = true
			matched_variant = v
			break
		}
	}
	if !matches {
		return t.transform_expr(expr_id)
	}
	ref_variant := t.variant_references_sum(matched_variant, resolved_sum)
	mut pointer_variant_child := flat.empty_node
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		pointer_variant_child = t.a.child(&expr, 0)
	}
	inner := if int(pointer_variant_child) >= 0 && ref_variant {
		t.transform_expr(pointer_variant_child)
	} else {
		t.transform_expr(expr_id)
	}
	if expr_type.starts_with('&') {
		return t.make_sum_literal(resolved_sum, matched_variant, inner)
	}
	if int(pointer_variant_child) >= 0 && ref_variant {
		return t.make_sum_literal(resolved_sum, matched_variant, inner)
	}
	if ref_variant {
		ref_inner := t.ensure_sum_variant_ref(inner, matched_variant)
		return t.make_sum_literal(resolved_sum, matched_variant, ref_inner)
	}
	start := t.a.children.len
	t.a.children << inner
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          resolved_sum
		children_start: start
		children_count: 1
		typ:            resolved_sum
	})
}

fn (mut t Transformer) ensure_sum_variant_ref(value flat.NodeId, variant string) flat.NodeId {
	mut value_type := t.node_type(value)
	if value_type.starts_with('&') {
		return value
	}
	clean_variant := if variant.starts_with('&') { variant[1..] } else { variant }
	if value_type.len == 0 {
		value_type = clean_variant
	}
	if t.expr_can_take_address(value) || t.a.nodes[int(value)].kind == .struct_init {
		ref := t.make_prefix(.amp, value)
		t.a.nodes[int(ref)].typ = '&${clean_variant}'
		return ref
	}
	tmp_name := t.new_temp('sum_val')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, value_type)
	ref := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.a.nodes[int(ref)].typ = '&${clean_variant}'
	return ref
}

fn (mut t Transformer) make_sum_literal(sum_name string, variant string, value flat.NodeId) flat.NodeId {
	qvariant := t.resolve_variant(sum_name, variant)
	typ_field := t.make_sum_literal_field('typ', t.make_int_literal(t.sum_type_index(sum_name,
		qvariant)), 'int')
	value_type := if t.variant_references_sum(qvariant, sum_name) && !qvariant.starts_with('&') {
		'&${qvariant}'
	} else {
		qvariant
	}
	value_field := t.make_sum_literal_field(t.sum_field_name(qvariant), value, value_type)
	start := t.a.children.len
	t.a.children << typ_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          sum_name
		typ:            sum_name
	})
}

fn (mut t Transformer) make_sum_literal_field(name string, value flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		children_start: start
		children_count: 1
		value:          name
		typ:            typ
	})
}
