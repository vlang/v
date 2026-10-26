module transform

import v.flat
import v.gen.c.naming { sum_field_name }
import v.types

const expr_array_type_id = flat.source_type_id('array')
const expr_map_type_id = flat.source_type_id('map')
const expr_string_pointer_type_id = flat.source_type_id('&string')

// transform_infix_string_ops transforms transform infix string ops data for transform.
fn (mut t Transformer) transform_infix_string_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	match node.op {
		.plus, .eq, .ne, .lt, .gt, .le, .ge {}
		else {
			return none
		}
	}

	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	if node.op in [.eq, .ne] && (t.expr_is_char_const(lhs_id) || t.expr_is_char_const(rhs_id)) {
		return none
	}
	lhs_raw_type_source_id := t.node_type(lhs_id)

	rhs_raw_type_source_id := t.node_type(rhs_id)

	lhs_clean_type_source_id := t.normalize_type_alias(lhs_raw_type_source_id)
	lhs_clean_type_comparison_type_id := lhs_clean_type_source_id

	rhs_clean_type_source_id := t.normalize_type_alias(rhs_raw_type_source_id)
	rhs_clean_type_comparison_type_id := rhs_clean_type_source_id

	if _ := t.operator_alias_type_for_operand(lhs_id, node.op) {
		return none
	}
	lhs_is_string_ptr := t.equality_expr_is_string_pointer(lhs_id, lhs_clean_type_source_id)
	rhs_is_string_ptr := t.equality_expr_is_string_pointer(rhs_id, rhs_clean_type_source_id)
	if node.op == .plus && (lhs_is_string_ptr || rhs_is_string_ptr) {
		return none
	}
	if node.op in [.eq, .ne]
		&& ((lhs_is_string_ptr && !t.expr_or_selector_base_has_smartcast(lhs_id))
			|| (rhs_is_string_ptr && !t.expr_or_selector_base_has_smartcast(rhs_id))) {
		return none
	}

	is_string := t.is_string_type(lhs_id) || t.is_string_type(rhs_id) || lhs_is_string_ptr
		|| rhs_is_string_ptr

	if !is_string {
		return none
	}

	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut new_lhs := t.transform_expr(lhs_id)
	lhs_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut new_rhs := t.transform_expr(rhs_id)
	rhs_pending := t.pending_stmts.clone()
	t.pending_stmts = outer_pending
	for stmt in lhs_pending {
		t.pending_stmts << stmt
	}
	for stmt in rhs_pending {
		t.pending_stmts << stmt
	}
	if !t.validate_specialized_comparison_operands(node, lhs_id, rhs_id, new_lhs, new_rhs) {
		return t.make_empty()
	}
	if lhs_is_string_ptr {
		new_lhs = t.make_prefix(.mul, new_lhs)
	}
	if rhs_is_string_ptr {
		new_rhs = t.make_prefix(.mul, new_rhs)
	}
	if node.op == .plus && lhs_clean_type_comparison_type_id in [
		flat.source_type_char,
		flat.source_type_rune,
	] {
		new_lhs = t.wrap_string_conversion(new_lhs, lhs_clean_type_source_id)
	}
	if node.op == .plus && rhs_clean_type_comparison_type_id in [
		flat.source_type_char,
		flat.source_type_rune,
	] {
		new_rhs = t.wrap_string_conversion(new_rhs, rhs_clean_type_source_id)
	}

	mut result := flat.empty_node
	result_type := (if node.op == .plus { flat.source_type_string } else { flat.source_type_bool })
	match node.op {
		.plus {
			lhs := t.a.nodes[int(new_lhs)]
			rhs := t.a.nodes[int(new_rhs)]
			if lhs.kind == .string_literal && rhs.kind == .string_literal {
				result = t.make_string_literal(lhs.value + rhs.value)
			} else {
				result = t.make_call('string__plus', [new_lhs, new_rhs])
			}
		}
		.eq {
			result = t.make_call('string__eq', [new_lhs, new_rhs])
		}
		.ne {
			eq_call := t.make_call('string__eq', [new_lhs, new_rhs])
			start := t.a.children.len
			t.a.children << eq_call
			result = t.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: start
				children_count: 1
			})
		}
		.lt {
			result = t.make_call('string__lt', [new_lhs, new_rhs])
		}
		.gt {
			// a > b  ->  string__lt(b, a)
			result = t.make_call('string__lt', [new_rhs, new_lhs])
		}
		.le {
			// a <= b  ->  !(b < a)  ->  !string__lt(rhs, lhs)
			lt_call := t.make_call('string__lt', [new_rhs, new_lhs])
			start := t.a.children.len
			t.a.children << lt_call
			result = t.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: start
				children_count: 1
			})
		}
		.ge {
			// a >= b  ->  !(a < b)  ->  !string__lt(lhs, rhs)
			lt_call := t.make_call('string__lt', [new_lhs, new_rhs])
			start := t.a.children.len
			t.a.children << lt_call
			result = t.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: start
				children_count: 1
			})
		}
		else {
			return none
		}
	}

	if lhs_pending.len > 0 || rhs_pending.len > 0 {
		tmp_name := t.new_temp(if result_type == flat.source_type_string {
			'str_expr'
		} else {
			'str_cmp'
		})
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, result, result_type)
		return t.make_ident(tmp_name)
	}
	return result
}

fn (t &Transformer) expr_is_char_const(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .char_literal {
		return true
	}
	mut name := ''
	if node.kind == .ident {
		name = node.value
	} else if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident {
			name = '${base.value}.${node.value}'
		}
	}
	if name == '' || isnil(t.tc) {
		return false
	}
	key := t.const_type_key_in_context(name, t.cur_module, t.cur_file) or { return false }
	expr_id := t.tc.const_exprs[key] or { return false }
	return t.a.nodes[int(expr_id)].kind == .char_literal
}

fn (t &Transformer) expr_or_selector_base_has_smartcast(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	if t.expr_has_smartcast(id) {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector && node.children_count > 0 {
		return t.expr_or_selector_base_has_smartcast(t.a.child(&node, 0))
	}
	return false
}

// transform_infix_array_ops transforms transform infix array ops data for transform.
fn (mut t Transformer) transform_infix_array_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 || node.op !in [.eq, .ne] {
		return none
	}
	if comparison := t.transform_translated_array_pointer_comparison(node) {
		return comparison
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_raw_type_source_id_2 := t.node_type(lhs_id)

	rhs_raw_type_source_id_2 := t.node_type(rhs_id)

	mut effective_lhs_raw_type := lhs_raw_type_source_id_2
	mut effective_rhs_raw_type := rhs_raw_type_source_id_2
	checker_lhs_type_source_id := t.raw_checker_node_type(lhs_id)
	checker_lhs_type_source_id_3 := checker_lhs_type_source_id

	checker_rhs_type_source_id := t.raw_checker_node_type(rhs_id)
	checker_rhs_type_source_id_2 := checker_rhs_type_source_id

	if !flat.source_type_shape(t.membership_container_type(effective_lhs_raw_type)).has(.array_type)
		&& flat.source_type_shape(t.membership_container_type(checker_lhs_type_source_id)).has(.array_type) {
		effective_lhs_raw_type = checker_lhs_type_source_id_3
	}
	if !flat.source_type_shape(t.membership_container_type(effective_rhs_raw_type)).has(.array_type)
		&& flat.source_type_shape(t.membership_container_type(checker_rhs_type_source_id)).has(.array_type) {
		effective_rhs_raw_type = checker_rhs_type_source_id_2
	}
	if !flat.source_type_shape(t.membership_container_type(effective_lhs_raw_type)).has(.array_type) {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .or_expr && lhs.children_count > 0 {
			_, value_type_source_id_5 := t.or_expr_types(t.a.child(&lhs, 0), lhs.typ)

			if flat.source_type_shape(t.membership_container_type(value_type_source_id_5)).has(.array_type) {
				effective_lhs_raw_type = value_type_source_id_5
			}
		}
	}
	if !flat.source_type_shape(t.membership_container_type(effective_rhs_raw_type)).has(.array_type) {
		rhs := t.a.nodes[int(rhs_id)]
		if rhs.kind == .or_expr && rhs.children_count > 0 {
			_, value_type_source_id_6 := t.or_expr_types(t.a.child(&rhs, 0), rhs.typ)

			if flat.source_type_shape(t.membership_container_type(value_type_source_id_6)).has(.array_type) {
				effective_rhs_raw_type = value_type_source_id_6
			}
		}
	}
	if !t.is_fixed_array_type(t.membership_container_type(effective_lhs_raw_type))
		&& t.is_fixed_array_type(t.membership_container_type(checker_lhs_type_source_id)) {
		effective_lhs_raw_type = checker_lhs_type_source_id_3
	}
	if !t.is_fixed_array_type(t.membership_container_type(effective_lhs_raw_type)) {
		if map_value_type_source_id := t.array_comparison_map_index_value_type(lhs_id) {
			effective_lhs_raw_type = map_value_type_source_id
		}
	}
	if !t.is_fixed_array_type(t.membership_container_type(effective_rhs_raw_type))
		&& t.is_fixed_array_type(t.membership_container_type(checker_rhs_type_source_id)) {
		effective_rhs_raw_type = checker_rhs_type_source_id_2
	}
	if !t.is_fixed_array_type(t.membership_container_type(effective_rhs_raw_type)) {
		if map_value_type_source_id := t.array_comparison_map_index_value_type(rhs_id) {
			effective_rhs_raw_type = map_value_type_source_id
		}
	}
	// An `is` smartcast narrows the operand, but an index/selector node keeps its
	// declared sum type. Prefer the narrowed type, so that
	// `if a[0] is [3]f64 { a[0] == [1.0, 1.5, 2.0]! }` lowers as a fixed array
	// comparison, instead of falling through to the sum type's equality helper -
	// which would then be handed a raw fixed array and emit uncompilable C.
	if !t.is_fixed_array_type(t.membership_container_type(effective_lhs_raw_type)) {
		smartcast_lhs_type_source_id := t.smartcast_node_type(lhs_id)
		smartcast_lhs_type_source_id_2 := smartcast_lhs_type_source_id

		if t.is_fixed_array_type(t.membership_container_type(smartcast_lhs_type_source_id)) {
			effective_lhs_raw_type = smartcast_lhs_type_source_id_2
		}
	}
	if !t.is_fixed_array_type(t.membership_container_type(effective_rhs_raw_type)) {
		smartcast_rhs_type_source_id := t.smartcast_node_type(rhs_id)
		smartcast_rhs_type_source_id_2 := smartcast_rhs_type_source_id

		if t.is_fixed_array_type(t.membership_container_type(smartcast_rhs_type_source_id)) {
			effective_rhs_raw_type = smartcast_rhs_type_source_id_2
		}
	}
	lhs_is_array_ptr := t.equality_type_is_array_pointer(effective_lhs_raw_type)
	rhs_is_array_ptr := t.equality_type_is_array_pointer(effective_rhs_raw_type)
	if lhs_is_array_ptr && rhs_is_array_ptr {
		return none
	}
	if _ := t.operator_alias_type_for_operand(lhs_id, node.op) {
		return none
	}
	mut lhs_type := t.membership_container_type(effective_lhs_raw_type)
	mut rhs_type := t.membership_container_type(effective_rhs_raw_type)
	mut lhs_is_fixed := t.is_fixed_array_type(lhs_type)
	mut rhs_is_fixed := t.is_fixed_array_type(rhs_type)
	mut fixed_lhs_as_array := flat.empty_node
	mut fixed_rhs_as_array := flat.empty_node
	if lhs_is_fixed || rhs_is_fixed {
		if (lhs_is_fixed && rhs_is_fixed)
			|| (lhs_is_fixed && t.expr_can_be_fixed_array_literal(rhs_id))
			|| (rhs_is_fixed && t.expr_can_be_fixed_array_literal(lhs_id)) {
			mut fixed_type := t.resolved_fixed_array_canonical_type(lhs_type)
			if !lhs_is_fixed {
				fixed_type = t.resolved_fixed_array_canonical_type(rhs_type)
			}
			new_lhs := t.transform_expr_for_type(lhs_id, fixed_type)
			new_rhs := t.transform_expr_for_type(rhs_id, fixed_type)
			eq_call := t.make_membership_eq_expr(new_lhs, new_rhs, fixed_type)
			if node.op == .ne {
				return t.make_prefix(.not, eq_call)
			}
			return eq_call
		}
		if lhs_is_fixed && flat.source_type_shape(rhs_type).has(.array_type) {
			fixed_type_source_id := t.resolved_fixed_array_canonical_type(lhs_type)
			fixed_type := fixed_type_source_id
			if fixed_array_elem_type(fixed_type) == t.array_elem_type(rhs_type) {
				fixed_lhs_as_array = t.fixed_array_value_to_array(lhs_id, fixed_type_source_id, rhs_type)
				lhs_type = rhs_type
				lhs_is_fixed = false
			} else {
				return none
			}
		} else if rhs_is_fixed && flat.source_type_shape(lhs_type).has(.array_type) {
			fixed_type_source_id_2 := t.resolved_fixed_array_canonical_type(rhs_type)
			fixed_type := fixed_type_source_id_2
			if fixed_array_elem_type(fixed_type) == t.array_elem_type(lhs_type) {
				fixed_rhs_as_array = t.fixed_array_value_to_array(rhs_id, fixed_type_source_id_2, lhs_type)
				rhs_type = lhs_type
				rhs_is_fixed = false
			} else {
				return none
			}
		} else {
			return none
		}
	}
	if !(flat.source_type_shape(lhs_type).has(.array_type) || lhs_type == expr_array_type_id) || !(flat.source_type_shape(rhs_type).has(.array_type)
		|| rhs_type == expr_array_type_id) {
		return none
	}
	mut elem_type := u32(0)
	if flat.source_type_shape(lhs_type).has(.array_type) {
		elem_type = flat.source_type_syntax(lhs_type).raw_base
	} else if flat.source_type_shape(rhs_type).has(.array_type) {
		elem_type = flat.source_type_syntax(rhs_type).raw_base
	}
	if elem_type == 0 {
		elem_type = flat.source_type_int
	}
	mut lhs_target_type := t.contextual_array_comparison_type(lhs_id, lhs_type, rhs_type)
	mut rhs_target_type := t.contextual_array_comparison_type(rhs_id, rhs_type, lhs_type)
	mut literal_elem_resolved := false
	if nested_elem_source_id := t.array_comparison_literal_elem_type(lhs_id) {
		elem_type = nested_elem_source_id
		lhs_type = flat.source_type_composition_one(.array, nested_elem_source_id)
		lhs_target_type = lhs_type
		literal_elem_resolved = true
	}
	if nested_elem_source_id := t.array_comparison_literal_elem_type(rhs_id) {
		elem_type = nested_elem_source_id
		rhs_type = flat.source_type_composition_one(.array, nested_elem_source_id)
		rhs_target_type = rhs_type
		literal_elem_resolved = true
	}
	mut new_lhs := if int(fixed_lhs_as_array) >= 0 {
		fixed_lhs_as_array
	} else if flat.source_type_shape(lhs_target_type).has(.array_type) {
		t.transform_expr_for_type(lhs_id, lhs_target_type)
	} else {
		t.transform_expr(lhs_id)
	}
	mut new_rhs := if int(fixed_rhs_as_array) >= 0 {
		fixed_rhs_as_array
	} else if flat.source_type_shape(rhs_target_type).has(.array_type) {
		t.transform_expr_for_type(rhs_id, rhs_target_type)
	} else {
		t.transform_expr(rhs_id)
	}
	new_lhs = t.preserve_array_comparison_deref(lhs_id, new_lhs, lhs_type)
	new_rhs = t.preserve_array_comparison_deref(rhs_id, new_rhs, rhs_type)
	new_lhs_type_source_id := t.membership_container_type(t.node_type(new_lhs))
	new_rhs_type_source_id := t.membership_container_type(t.node_type(new_rhs))
	// Keep an already resolved nested element type. Generic method calls can retain a
	// coarse scalar return type on the transformed node even though contextual typing
	// resolved the comparison itself to `[][]T`.
	if !literal_elem_resolved && !flat.source_type_shape(elem_type).has(.array_type) && flat.source_type_shape(new_lhs_type_source_id).has(.array_type) {
		elem_type = flat.source_type_syntax(new_lhs_type_source_id).raw_base
		lhs_type = new_lhs_type_source_id
	} else if !literal_elem_resolved && !flat.source_type_shape(elem_type).has(.array_type)
		&& flat.source_type_shape(new_rhs_type_source_id).has(.array_type) {
		elem_type = flat.source_type_syntax(new_rhs_type_source_id).raw_base
		rhs_type = new_rhs_type_source_id
	}
	if lhs_is_array_ptr && flat.source_type_shape(t.node_type(new_lhs)).has(.pointer) {
		new_lhs = t.make_prefix(.mul, new_lhs)
	}
	if rhs_is_array_ptr && flat.source_type_shape(t.node_type(new_rhs)).has(.pointer) {
		new_rhs = t.make_prefix(.mul, new_rhs)
	}
	eq_call := if t.array_elem_needs_element_eq(elem_type) {
		t.make_array_elementwise_eq_call(new_lhs, new_rhs, elem_type, lhs_type, rhs_type, node)
	} else if flat.source_type_shape(elem_type).has(.array_type) {
		t.make_call_typed('__v3_array_eq_array', [new_lhs, new_rhs,
			t.make_int_literal(array_nested_eq_depth(lhs_type))], flat.source_type_bool)
	} else if elem_type == flat.source_type_string {
		t.make_call_typed('__v3_array_eq_string', [new_lhs, new_rhs], flat.source_type_bool)
	} else {
		t.make_call_typed('__v3_array_eq_raw', [new_lhs, new_rhs, t.make_sizeof_type(elem_type)], flat.source_type_bool)
	}
	if node.op == .ne {
		return t.make_prefix(.not, eq_call)
	}
	return eq_call
}

fn (mut t Transformer) transform_translated_array_pointer_comparison(node flat.Node) ?flat.NodeId {
	if isnil(t.tc) {
		return none
	}
	file := t.a.source_files[node.pos.id] or { return none }
	if !t.tc.translated_files[file.name] {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs_type := types.unalias_type(t.tc.resolve_type(lhs_id))
	rhs_type := types.unalias_type(t.tc.resolve_type(rhs_id))
	if !((lhs_type is types.ArrayFixed && rhs_type is types.Pointer)
		|| (rhs_type is types.ArrayFixed && lhs_type is types.Pointer)) {
		return none
	}
	// C array decay compares addresses, including pointers to nested fixed arrays.
	lhs := t.translated_array_pointer_comparison_operand(lhs_id, lhs_type)
	rhs := t.translated_array_pointer_comparison_operand(rhs_id, rhs_type)
	return t.make_infix(node.op, lhs, rhs)
}

fn (t &Transformer) translated_fixed_array_pointer_lvalue(id flat.NodeId, other_id flat.NodeId) bool {
	if isnil(t.tc) || int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	file := t.a.source_files[t.a.nodes[int(id)].pos.id] or { return false }
	if !t.tc.translated_files[file.name] {
		return false
	}
	return types.unalias_type(t.tc.resolve_type(id)) is types.ArrayFixed
		&& types.unalias_type(t.tc.resolve_type(other_id)) is types.Pointer
}

fn (mut t Transformer) translated_array_pointer_comparison_operand(id flat.NodeId, typ types.Type) flat.NodeId {
	mut value := t.transform_expr_preserving_pointer_value(id)
	if typ is types.ArrayFixed && t.expr_can_be_fixed_array_literal(id) {
		// Keep the literal's element type while C lowers it to a compound literal.
		elem_ptr_type := flat.source_type_composition_one(.pointer, typ.elem_type.source_type_id())
		value = t.make_cast(elem_ptr_type, value, elem_ptr_type)
	}
	return t.make_cast(flat.source_type_voidptr, value, flat.source_type_voidptr)
}

fn (t &Transformer) array_comparison_literal_elem_type(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	first_id := t.a.child(&node, 0)
	first := t.a.nodes[int(first_id)]
	if first.kind !in [.array_literal, .array_init] {
		scalar := if first.kind == .prefix && first.op in [.plus, .minus]
			&& first.children_count == 1 {
			t.a.child_node(&first, 0)
		} else {
			&first
		}
		return match scalar.kind {
			.string_literal, .string_interp {
				flat.source_type_string
			}
			.char_literal {
				flat.source_type_rune
			}
			.float_literal {
				if scalar.typ == flat.source_type_f32 {
					flat.source_type_f32
				} else {
					flat.source_type_f64
				}
			}
			.bool_literal {
				flat.source_type_bool
			}
			.cast_expr {
				clean_source_id := t.normalize_type_alias(scalar.source_value_id())
				if clean_source_id != 0 && clean_source_id != flat.source_type_unknown {
					clean_source_id
				} else {
					none
				}
			}
			else {
				none
			}
		}
	}
	for candidate in [t.raw_checker_node_type(first_id), t.node_type(first_id), first.typ] {
		clean_source_id_2 := t.membership_container_type(t.normalize_type_alias(candidate))
		clean_shape := flat.source_type_shape(clean_source_id_2)
		if clean_shape.has(.array_type) {
			return clean_source_id_2
		}
	}
	if first.kind == .array_literal && first.children_count > 0 {
		inner_source_id := t.array_literal_elem_type(first)
		if inner_source_id != 0 && inner_source_id != flat.source_type_unknown {
			return flat.source_type_composition_one(.array, inner_source_id)
		}
	}
	return none
}

fn (mut t Transformer) preserve_array_comparison_deref(source_id flat.NodeId, transformed_id flat.NodeId, array_type_id u32) flat.NodeId {
	if int(source_id) < 0 || int(source_id) >= t.a.nodes.len || int(transformed_id) < 0
		|| int(transformed_id) >= t.a.nodes.len {
		return transformed_id
	}
	source := t.a.nodes[int(source_id)]
	transformed := t.a.nodes[int(transformed_id)]
	if source.kind != .prefix || source.op != .mul || source.children_count == 0
		|| (transformed.kind == .prefix && transformed.op == .mul) {
		return transformed_id
	}
	source_child_type_shape := flat.source_type_shape(t.node_type(t.a.child(&source, 0)))
	if !source_child_type_shape.has(.pointer) {
		return transformed_id
	}
	deref := t.make_prefix(.mul, transformed_id)
	t.set_node_typ(int(deref), array_type_id)
	return deref
}

fn (t &Transformer) array_comparison_map_index_value_type(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .index || node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&node, 0)
	base := t.a.nodes[int(base_id)]
	mut base_type := (if base.kind == .ident {
		t.var_type(base.value)
	} else {
		u32(0)
	})
	if base_type == 0 || base_type == flat.source_type_unknown {
		base_type = t.raw_checker_node_type(base_id)
	}
	if base_type == 0 || base_type == flat.source_type_unknown {
		base_type = t.node_type(base_id)
	}
	_, value_type_source_id := t.map_type_parts(t.normalize_type_alias(base_type))
	value_type_source_id_4 := value_type_source_id

	if t.is_fixed_array_type(value_type_source_id) {
		return value_type_source_id_4
	}
	return none
}

fn (t &Transformer) contextual_array_comparison_type(id flat.NodeId, own_type_id u32, other_type_id u32) u32 {
	if int(id) < 0 || !flat.source_type_shape(own_type_id).has(.array_type)
		|| !flat.source_type_shape(other_type_id).has(.array_type) {
		return own_type_id
	}
	node := t.a.nodes[int(id)]
	if node.kind != .array_literal {
		return own_type_id
	}
	own_elem := flat.source_type_syntax(own_type_id).raw_base
	other_elem := flat.source_type_syntax(other_type_id).raw_base
	if t.is_sum_type_name(other_elem) && t.sum_target_accepts_variant_type(other_elem, own_elem) {
		return other_type_id
	}
	other_iface_source_id := t.resolve_interface_type_name(other_elem)
	if other_iface_source_id != 0 {
		if !t.array_append_interface_has_requirements(other_iface_source_id) {
			return other_type_id
		}
		own_concrete_id := t.trim_pointer_type(t.normalize_type_alias(own_elem))
		if !isnil(t.tc) && t.tc.named_type_implements_interface(own_concrete_id, other_iface_source_id) {
			return other_type_id
		}
	}
	other_syntax := flat.source_type_syntax(other_elem)
	if !other_syntax.generic_app_valid {
		return own_type_id
	}
	if flat.source_type_syntax(own_elem).unqualified == flat.source_type_syntax(other_syntax.generic_app_base).unqualified {
		return other_type_id
	}
	return own_type_id
}

fn (t &Transformer) expr_can_be_fixed_array_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .array_literal {
		return true
	}
	if node.kind == .postfix && node.op == .not && node.children_count == 1 {
		child := t.a.nodes[int(t.a.child(&node, 0))]
		return child.kind == .array_literal
	}
	return false
}

// transform_infix_map_ops transforms transform infix map ops data for transform.
fn (mut t Transformer) transform_infix_map_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 || node.op !in [.eq, .ne] {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_type_source_id_3 := t.map_comparison_expr_type(lhs_id)

	rhs_type_source_id_2 := t.map_comparison_expr_type(rhs_id)

	if t.equality_type_is_map_pointer(lhs_type_source_id_3) && t.equality_type_is_map_pointer(rhs_type_source_id_2) {
		return none
	}
	mut lhs_map_type := t.clean_map_type(lhs_type_source_id_3)
	mut rhs_map_type := t.clean_map_type(rhs_type_source_id_2)
	if !flat.source_type_shape(lhs_map_type).has(.map_type) && flat.source_type_shape(rhs_map_type).has(.map_type)
		&& t.is_empty_map_init(lhs_id) {
		lhs_map_type = rhs_map_type
	}
	if !flat.source_type_shape(rhs_map_type).has(.map_type) && flat.source_type_shape(lhs_map_type).has(.map_type)
		&& t.is_empty_map_init(rhs_id) {
		rhs_map_type = lhs_map_type
	}
	if !flat.source_type_shape(lhs_map_type).has(.map_type) || !flat.source_type_shape(rhs_map_type).has(.map_type) {
		return none
	}
	_, lhs_value_type_source_id_2 := t.map_type_parts(lhs_map_type)
	lhs_value_type_comparison_type_id := lhs_value_type_source_id_2

	_, rhs_value_type_source_id_2 := t.map_type_parts(rhs_map_type)
	rhs_value_type_comparison_type_id := rhs_value_type_source_id_2

	// A result/option wrapper can retain the checker's partial `map[K]` spelling
	// after its generic callee has been specialized to `map[K]V`. Prefer the
	// complete peer type so equality still uses V's element semantics (not a raw
	// byte comparison of string/array/map descriptors).
	map_type := (if (lhs_value_type_comparison_type_id in [u32(0), flat.source_type_void,
		flat.source_type_unknown] || t.generic_arg_is_unresolved(lhs_value_type_source_id_2))
		&& rhs_value_type_comparison_type_id !in [u32(0), flat.source_type_void,
			flat.source_type_unknown]
		&& !t.generic_arg_is_unresolved(rhs_value_type_source_id_2) {
		rhs_map_type
	} else {
		lhs_map_type
	})
	mut new_lhs := t.transform_expr_for_type(lhs_id, map_type)
	mut new_rhs := t.transform_expr_for_type(rhs_id, map_type)
	if t.transformed_map_equality_operand_needs_deref(new_lhs, lhs_type_source_id_3) {
		new_lhs = t.make_prefix(.mul, new_lhs)
	}
	if t.transformed_map_equality_operand_needs_deref(new_rhs, rhs_type_source_id_2) {
		new_rhs = t.make_prefix(.mul, new_rhs)
	}
	_, value_type_source_id_2 := t.map_type_parts(map_type)

	eq_call := if value_type_source_id_2 != 0 && t.map_value_needs_element_eq(value_type_source_id_2) {
		t.make_map_elementwise_eq_call(new_lhs, new_rhs, map_type, node)
	} else {
		t.make_call_typed('map_map_eq', [new_lhs, new_rhs], flat.source_type_bool)
	}
	if node.op == .ne {
		return t.make_prefix(.not, eq_call)
	}
	return eq_call
}

fn (t &Transformer) transformed_map_equality_operand_needs_deref(id flat.NodeId, original_type_id u32) bool {
	original_type_shape := flat.source_type_shape(original_type_id)
	if !original_type_shape.has(.pointer) {
		return false
	}
	typ_source_id := t.node_type(id)
	typ_shape := flat.source_type_shape(typ_source_id)
	if typ_source_id == 0 {
		return true
	}
	return typ_shape.has(.pointer) && flat.source_type_shape(t.clean_map_type(typ_source_id)).has(.map_type)
}

fn (mut t Transformer) map_comparison_expr_type(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind == .or_expr && node.children_count > 0 {
		_, value_type_source_id_7 := t.or_expr_types(t.a.child(&node, 0), node.typ)
		value_type_source_id_8 := value_type_source_id_7

		if flat.source_type_shape(t.clean_map_type(value_type_source_id_8)).has(.map_type) {
			return value_type_source_id_8
		}
		inner_type_source_id := t.map_comparison_expr_type(t.a.child(&node, 0))

		if flat.source_type_shape(t.clean_map_type(inner_type_source_id)).has(.map_type) {
			return inner_type_source_id
		}
	}
	if node.kind == .call {
		concrete_source_id := t.concrete_generic_call_return_type(id, node)
		concrete_source_id_2 := concrete_source_id

		if concrete_source_id != 0 && flat.source_type_shape(t.clean_map_type(concrete_source_id)).has(.map_type) {
			return concrete_source_id_2
		}
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name_id := typ.source_type_id()
			if name_id != 0 && flat.source_type_shape(t.clean_map_type(name_id)).has(.map_type) {
				return name_id
			}
		}
	}
	mut typ := t.node_type(id)
	if typ == 0 && node.kind == .map_init {
		typ = if node.source_value_id() != 0 { node.source_value_id() } else { node.typ }
	}
	return typ
}

fn (t &Transformer) is_empty_map_init(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .map_init && node.children_count == 0
}

// expr_is_plain_lvalue reports whether the expression is an addressable chain
// of idents/selectors — no calls, indexes or other temporaries at any level.
fn (t &Transformer) expr_is_plain_lvalue(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value.len > 0
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			return t.expr_is_plain_lvalue(t.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (mut t Transformer) transform_infix_interface_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 || node.op !in [.eq, .ne] {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	mut lhs_type := t.node_type(lhs_id)
	mut rhs_type := t.node_type(rhs_id)
	lhs_smartcast_source_id := t.smartcast_node_type(lhs_id)
	lhs_smartcast_source_id_2 := lhs_smartcast_source_id

	if lhs_smartcast_source_id != 0 {
		lhs_type = lhs_smartcast_source_id_2
	}
	rhs_smartcast_source_id := t.smartcast_node_type(rhs_id)
	rhs_smartcast_source_id_2 := rhs_smartcast_source_id

	if rhs_smartcast_source_id != 0 {
		rhs_type = rhs_smartcast_source_id_2
	}
	if lhs_type == 0 {
		lhs_type = t.checker_node_type(lhs_id)
	}
	if rhs_type == 0 {
		rhs_type = t.checker_node_type(rhs_id)
	}
	lhs_depth, _ := t.pointer_type_depth_and_base(t.normalize_type_alias(lhs_type))
	rhs_depth, _ := t.pointer_type_depth_and_base(t.normalize_type_alias(rhs_type))
	if lhs_depth > 1 || rhs_depth > 1 {
		return none
	}
	lhs_iface_source_id := t.resolve_interface_type_name(lhs_type)
	rhs_iface_source_id := t.resolve_interface_type_name(rhs_type)
	if lhs_iface_source_id == 0 && rhs_iface_source_id == 0 {
		return none
	}
	if rhs_iface_source_id == 0 && t.expr_is_nil_like(rhs_id) {
		if nil_cmp := t.interface_nil_compare_expr(lhs_id, lhs_type, node.op) {
			return nil_cmp
		}
	}
	if lhs_iface_source_id == 0 && t.expr_is_nil_like(lhs_id) {
		if nil_cmp := t.interface_nil_compare_expr(rhs_id, rhs_type, node.op) {
			return nil_cmp
		}
	}
	iface := if lhs_iface_source_id != 0 { lhs_iface_source_id } else { rhs_iface_source_id }
	if t.is_builtin_ierror_interface_name(iface)
		&& (t.a.node(lhs_id).kind == .none_expr || t.a.node(rhs_id).kind == .none_expr) {
		value_id := if t.a.node(lhs_id).kind == .none_expr { rhs_id } else { lhs_id }
		value := t.transform_expr_for_type(value_id, iface)
		stable := t.stable_transformed_expr_for_reuse(value, iface, 'ierr_none')
		typ := t.make_selector(stable, '_typ', flat.source_type_int)
		eq := t.make_ierror_none_type_check(typ, iface)
		if node.op == .ne {
			return t.make_prefix(.not, eq)
		}
		return eq
	}
	lhs := if lhs_iface_source_id == 0 && t.is_builtin_ierror_interface_name(iface) {
		t.make_interface_literal_from_expr(lhs_id, iface, false) or {
			t.transform_expr_for_type(lhs_id, iface)
		}
	} else {
		t.transform_expr_for_type(lhs_id, iface)
	}
	rhs := if rhs_iface_source_id == 0 && t.is_builtin_ierror_interface_name(iface) {
		t.make_interface_literal_from_expr(rhs_id, iface, false) or {
			t.transform_expr_for_type(rhs_id, iface)
		}
	} else {
		t.transform_expr_for_type(rhs_id, iface)
	}
	concrete_type_source_id := t.interface_box_concrete_type(lhs) or {
		t.interface_box_concrete_type(rhs) or {
			// The boxed concrete type is unknown at compile time. For IError
			// specifically, compare concrete type and observable identity (msg +
			// code) via the always-emitted C helpers, matching how error values compare in
			// practice (`assert err == other_err`). The helpers take addresses,
			// so a transformed non-lvalue operand is spilled to a temporary first.
			if iface in [flat.source_type_id('IError'), flat.source_type_id('builtin.IError')] {
				lhs_err := if t.expr_is_plain_lvalue(lhs) {
					lhs
				} else {
					t.stable_transformed_expr_for_reuse(lhs, iface, 'ierr_eq_lhs')
				}
				rhs_err := if t.expr_is_plain_lvalue(rhs) {
					rhs
				} else {
					t.stable_transformed_expr_for_reuse(rhs, iface, 'ierr_eq_rhs')
				}
				lhs_addr := t.make_prefix(.amp, lhs_err)
				rhs_addr := t.make_prefix(.amp, rhs_err)
				lhs_typ := t.make_selector(lhs_err, '_typ', flat.source_type_int)
				rhs_typ := t.make_selector(rhs_err, '_typ', flat.source_type_int)
				type_eq := t.make_infix(.eq, lhs_typ, rhs_typ)
				lhs_msg := t.make_call_typed('IError__msg', [lhs_addr], flat.source_type_string)
				rhs_msg := t.make_call_typed('IError__msg', [rhs_addr], flat.source_type_string)
				msg_eq := t.make_call_typed('string__eq', [lhs_msg, rhs_msg], flat.source_type_bool)
				lhs_code := t.make_call_typed('IError__code', [lhs_addr], flat.source_type_int)
				rhs_code := t.make_call_typed('IError__code', [rhs_addr], flat.source_type_int)
				code_eq := t.make_infix(.eq, lhs_code, rhs_code)
				err_eq := t.make_infix(.logical_and, type_eq, t.make_infix(.logical_and, msg_eq, code_eq))
				if node.op == .ne {
					return t.make_prefix(.not, err_eq)
				}
				return err_eq
			}
			eq := t.make_interface_semantic_eq_expr(lhs, rhs, iface, []EqualityTypeVisit{})
			if node.op == .ne {
				return t.make_prefix(.not, eq)
			}
			return eq
		}
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, iface, 'iface_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, iface, 'iface_eq_rhs')
	lhs_typ := t.make_selector(lhs_value, '_typ', flat.source_type_int)
	rhs_typ := t.make_selector(rhs_value, '_typ', flat.source_type_int)
	if iface in [flat.source_type_id('IError'), flat.source_type_id('builtin.IError')] && flat.source_type_syntax(concrete_type_source_id).unqualified == flat.source_type_id('None__') {
		lhs_none := t.make_ierror_none_type_check(lhs_typ, iface)
		rhs_none := t.make_ierror_none_type_check(rhs_typ, iface)
		eq := t.make_infix(.logical_and, lhs_none, rhs_none)
		if node.op == .ne {
			return t.make_prefix(.not, eq)
		}
		return eq
	}
	type_eq := t.make_infix(.eq, lhs_typ, rhs_typ)
	concrete_pointer := flat.source_type_composition_one(.pointer, concrete_type_source_id)
	lhs_object := t.make_cast(concrete_pointer, t.make_selector(lhs_value, '_object', flat.source_type_voidptr), concrete_pointer)
	rhs_object := t.make_cast(concrete_pointer, t.make_selector(rhs_value, '_object', flat.source_type_voidptr), concrete_pointer)
	lhs_concrete := t.make_prefix(.mul, lhs_object)
	t.set_node_typ(int(lhs_concrete), concrete_type_source_id)
	rhs_concrete := t.make_prefix(.mul, rhs_object)
	t.set_node_typ(int(rhs_concrete), concrete_type_source_id)
	value_eq := t.make_membership_eq_expr(lhs_concrete, rhs_concrete, concrete_type_source_id)
	eq := t.make_infix(.logical_and, type_eq, value_eq)
	if node.op == .ne {
		return t.make_prefix(.not, eq)
	}
	return eq
}

fn (mut t Transformer) interface_nil_compare_expr(value_id flat.NodeId, value_type_id u32, op flat.Op) ?flat.NodeId {
	value_type_shape := flat.source_type_shape(value_type_id)
	if (t.resolve_interface_type_name(value_type_id)) == 0 {
		return none
	}
	value := t.transform_expr(value_id)
	lhs := if value_type_shape.has(.pointer) {
		value
	} else {
		t.make_selector(value, '_object', flat.source_type_voidptr)
	}
	return t.make_infix(op, lhs, t.a.add(.nil_literal))
}

fn (t &Transformer) interface_box_concrete_type(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if concrete_source_id := t.interface_var_concrete_types.type_id(node.value) {
			concrete_source_id_3 := concrete_source_id

			return concrete_source_id_3
		}
	}
	if node.kind != .struct_init || !t.is_interface_type(node.source_value_id()) {
		return none
	}
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.value != '_object' || field.children_count == 0 {
			continue
		}
		typ := t.node_type(t.a.child(field, 0))
		if flat.source_type_shape(typ).has(.pointer) {
			pointee := flat.source_type_syntax(typ).raw_base
			if pointee != 0 {
				return pointee
			}
		}
	}
	return none
}

// transform_infix_struct_ops transforms transform infix struct ops data for transform.
fn (mut t Transformer) transform_infix_struct_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	if ptr_value_eq := t.transform_pointer_value_struct_eq(node, lhs_id, rhs_id) {
		return ptr_value_eq
	}
	if node.op in [.eq, .ne]
		&& (t.infix_operand_is_pointer(lhs_id) || t.infix_operand_is_pointer(rhs_id)) {
		return none
	}
	mut lhs_type := t.node_type(lhs_id)
	checker_lhs_type_source_id_2 := t.checker_node_type(lhs_id)
	lhs_key := t.expr_key(lhs_id)
	mut has_smartcast := false
	if lhs_key.len > 0 {
		if sc := t.find_smartcast(lhs_key) {
			has_smartcast = true
			lhs_type = t.smartcast_target_type(sc)
		}
	}
	if lhs_type == 0
		|| (t.generic_arg_is_unresolved(lhs_type) && !t.generic_arg_is_unresolved(checker_lhs_type_source_id_2)) {
		lhs_type = checker_lhs_type_source_id_2
	}
	lhs_node := t.a.nodes[int(lhs_id)]
	lhs_is_mut_value_param := lhs_node.kind == .ident && t.mut_param_values[lhs_node.value]
	lhs_is_pointer := flat.source_type_shape(lhs_type).has(.pointer) || flat.source_type_shape(checker_lhs_type_source_id_2).has(.pointer)
		|| lhs_is_mut_value_param
	if lhs_is_pointer {
		if node.op in [.eq, .ne] {
			return none
		}
		if flat.source_type_shape(lhs_type).has(.pointer) {
			lhs_type = flat.source_type_syntax(lhs_type).raw_base
		}
	}
	mut struct_type := u32(0)
	mut is_alias_operator := false
	if alias_type_source_id := t.operator_alias_type_for_operand(lhs_id, node.op) {
		alias_type_source_id_2 := alias_type_source_id

		struct_type = alias_type_source_id_2
		is_alias_operator = true
	} else {
		struct_type = t.struct_lookup_name(lhs_type)
	}
	if struct_type == 0 {
		// Generic-struct instance operand (e.g. `Vec4[f32]`): keep the instance type
		// so the operator lowers to the monomorphized method (`vec.Vec4[f32].+`).
		struct_type = t.generic_struct_instance_name(lhs_type)
	}
	if struct_type == 0 {
		return none
	}
	// V1's concrete generic symbols do not inherit the generic base's equality
	// method when generating the structural helper used by `!=`. Keep direct
	// generic `==` overloaded, but let `!=` compare the concrete fields.
	is_generic_struct_instance := flat.source_type_syntax(struct_type).generic_app_valid
	generic_ne_uses_structural_eq := node.op == .ne && is_generic_struct_instance
	// Skip the checker/transformer agreement guard for generic-struct instances:
	// they resolve reliably, and an alias name (`SimdFloat4`) vs the resolved form
	// (`vec.Vec4[f32]`) would otherwise spuriously fail the comparison.
	if !t.validating_generic_spec && checker_lhs_type_source_id_2 != 0 && !has_smartcast && !lhs_is_pointer
		&& !flat.source_type_shape(struct_type).has(.bracketed) && !is_alias_operator {
		checker_struct_type := t.struct_lookup_name(checker_lhs_type_source_id_2)
		if checker_struct_type != 0 && checker_struct_type != struct_type {
			return none
		}
		if checker_struct_type == 0 && checker_lhs_type_source_id_2 != lhs_type {
			return none
		}
	}
	if !generic_ne_uses_structural_eq {
		if call_info := t.struct_operator_call_info_for_operand(struct_type, node.op, is_alias_operator) {
			if t.is_disabled_fn_name(call_info.name) {
				ret_type_source_id_3 := t.struct_operator_return_type(call_info.name)

				if ret_type_source_id_3 == 0 || ret_type_source_id_3 == flat.source_type_void {
					return t.make_empty()
				}
				return t.zero_value_for_type(ret_type_source_id_3)
			}
			mut lhs := t.transform_expr(lhs_id)
			loaded_heap_value := lhs_node.kind == .ident && lhs_node.value in t.heaped_amp_locals
				&& !t.infix_operand_is_pointer(lhs)
			if lhs_is_pointer && !loaded_heap_value {
				// Ordinary reads already load heap storage. A source-level pointer
				// still needs its own pointee load before the value operator call.
				lhs = t.make_prefix(.mul, lhs)
				t.set_node_typ(int(lhs), lhs_type)
			}
			rhs := t.transform_expr(t.a.children[node.children_start + 1])
			mut call_lhs := lhs
			mut call_rhs := rhs
			if call_info.reverse {
				call_lhs = t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'op_lhs')
				call_rhs = t.stable_transformed_expr_for_reuse(rhs, t.node_type(t.a.children[node.children_start + 1]), 'op_rhs')
			}
			args := if call_info.reverse {
				[call_rhs, call_lhs]
			} else {
				[call_lhs, call_rhs]
			}
			t.mark_struct_operator_used_name(call_info.name)
			ret_type_source_id := t.infix_struct_operator_result_type(node, struct_type)
			ret_type_source_id_5 := ret_type_source_id

			call := t.make_call_typed(call_info.name, args, (if ret_type_source_id != 0 {
				ret_type_source_id_5
			} else {
				node.typ
			}))
			if call_info.negate {
				return t.make_prefix(.not, call)
			}
			return call
		}
	}
	if node.op != .eq && node.op != .ne {
		return none
	}
	if generic_ne_uses_structural_eq || !t.has_struct_operator_fn(struct_type, '==') {
		lhs := t.stable_expr_for_reuse(lhs_id)
		rhs_expr := t.transform_expr_for_type(rhs_id, lhs_type)
		rhs := t.stable_transformed_expr_for_reuse(rhs_expr, lhs_type, 'eq_rhs')
		if field_eq := t.make_struct_field_eq_expr(lhs, rhs, struct_type) {
			if node.op == .ne {
				return t.make_prefix(.not, field_eq)
			}
			return field_eq
		}
		cmp := t.make_call_typed('C.memcmp', [t.make_prefix(.amp, lhs), t.make_prefix(.amp, rhs),
			t.make_sizeof_type(struct_type)], flat.source_type_int)
		return t.make_infix(node.op, cmp, t.make_int_literal(0))
	}
	if eq_fn := t.struct_operator_fn_name(struct_type, '==') {
		if t.is_disabled_fn_name(eq_fn) {
			return t.make_bool_literal(node.op == .ne)
		}
		lhs := t.transform_expr(lhs_id)
		rhs := t.transform_expr(t.a.children[node.children_start + 1])
		t.mark_struct_operator_used_name(eq_fn)
		eq_call := t.make_call_typed(eq_fn, [lhs, rhs], flat.source_type_bool)
		if node.op == .ne {
			return t.make_prefix(.not, eq_call)
		}
		return eq_call
	}
	return none
}

fn (mut t Transformer) transform_pointer_value_struct_eq(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) ?flat.NodeId {
	if node.op !in [.eq, .ne] {
		return none
	}
	// Pointer-backed value locals (for example a struct heap-promoted because it is
	// later passed to a `voidptr` parameter) still have value semantics here. Treating
	// their storage pointer as a language-level pointer adds a second dereference when
	// the value equality is lowered.
	lhs_is_ptr := t.infix_operand_is_language_pointer(lhs_id)
	rhs_is_ptr := t.infix_operand_is_language_pointer(rhs_id)
	if !lhs_is_ptr && !rhs_is_ptr {
		return none
	}
	mut lhs_type := t.infix_operand_pointer_type(lhs_id) or { t.node_type(lhs_id) }
	mut rhs_type := t.infix_operand_pointer_type(rhs_id) or { t.node_type(rhs_id) }
	lhs_node := t.a.nodes[int(lhs_id)]
	rhs_node := t.a.nodes[int(rhs_id)]
	if lhs_node.kind == .ident && t.pointer_value_rvalues[lhs_node.value]
		&& flat.source_type_shape(lhs_type).has(.pointer) {
		lhs_type = flat.source_type_syntax(lhs_type).raw_base
	}
	if rhs_node.kind == .ident && t.pointer_value_rvalues[rhs_node.value]
		&& flat.source_type_shape(rhs_type).has(.pointer) {
		rhs_type = flat.source_type_syntax(rhs_type).raw_base
	}
	lhs_clean_source_id := t.trim_pointer_type(lhs_type)
	rhs_clean_source_id := t.trim_pointer_type(rhs_type)
	lhs_struct := t.struct_lookup_name(lhs_clean_source_id)
	rhs_struct := t.struct_lookup_name(rhs_clean_source_id)
	if lhs_struct == 0 || rhs_struct == 0 {
		return none
	}
	if lhs_struct != rhs_struct && flat.source_type_syntax(lhs_struct).unqualified != flat.source_type_syntax(rhs_struct).unqualified {
		return none
	}
	if lhs_is_ptr && rhs_is_ptr {
		if t.infix_operand_requests_pointer_identity(lhs_id)
			|| t.infix_operand_requests_pointer_identity(rhs_id) {
			// A user-defined equality operator takes precedence over the identity
			// semantics requested by reference parameters or explicit addresses.
			mut operator_type := lhs_struct
			mut is_alias_operator := false
			if alias_type_source_id := t.operator_alias_type_for_operand(lhs_id, node.op) {
				operator_type = alias_type_source_id
				is_alias_operator = true
			}
			if call_info := t.struct_operator_call_info_for_operand(operator_type, node.op, is_alias_operator) {
				if t.is_disabled_fn_name(call_info.name) {
					return t.make_bool_literal(node.op == .ne)
				}
				lhs := t.transform_expr_preserving_pointer_value(lhs_id)
				rhs := t.transform_expr_preserving_pointer_value(rhs_id)
				args := if call_info.reverse { [rhs, lhs] } else { [lhs, rhs] }
				t.mark_struct_operator_used_name(call_info.name)
				call := t.make_call_typed(call_info.name, args, flat.source_type_bool)
				if call_info.negate {
					return t.make_prefix(.not, call)
				}
				return call
			}
			return none
		}
		return t.transform_struct_pointer_eq(node, lhs_id, rhs_id, lhs_type, rhs_type, lhs_clean_source_id, rhs_clean_source_id)
	}
	lhs := if lhs_is_ptr {
		value := t.make_prefix(.mul, t.transform_expr(lhs_id))
		t.set_node_typ(int(value), lhs_clean_source_id)
		value
	} else {
		t.transform_expr(lhs_id)
	}
	rhs := if rhs_is_ptr {
		value := t.make_prefix(.mul, t.transform_expr(rhs_id))
		t.set_node_typ(int(value), rhs_clean_source_id)
		value
	} else {
		t.transform_expr(rhs_id)
	}
	if eq := t.transform_transformed_struct_eq(node, lhs, rhs) {
		return eq
	}
	return none
}

fn (t &Transformer) infix_operand_requests_pointer_identity(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .ident {
		return t.var_is_ref_param(node.value)
	}
	if node.kind == .paren && node.children_count == 1 {
		child_id := t.source_expr_child(id, node, 0) or { return false }
		return t.infix_operand_requests_pointer_identity(child_id)
	}
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return false
	}
	child_id := t.source_expr_child(id, node, 0) or { return false }
	return t.source_expr_is_plain_lvalue(child_id)
}

fn (t &Transformer) source_expr_is_plain_lvalue(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .ident {
		return node.value.len > 0
	}
	if node.kind == .prefix {
		if node.op != .mul || node.children_count != 1 {
			return false
		}
		child_id := t.source_expr_child(id, node, 0) or { return false }
		return t.source_expr_is_plain_lvalue(child_id)
	}
	if node.kind !in [.selector, .index, .paren] || node.children_count == 0 {
		return false
	}
	if node.kind == .index && node.value == 'range' {
		return false
	}
	child_id := t.source_expr_child(id, node, 0) or { return false }
	return t.source_expr_is_plain_lvalue(child_id)
}

fn (t &Transformer) source_expr_child(id flat.NodeId, node &flat.Node, child_idx int) ?flat.NodeId {
	if child_idx < 0 || child_idx >= int(node.children_count) {
		return none
	}
	child_id := t.a.child(node, child_idx)
	if int(child_id) >= 0 {
		return child_id
	}
	// In-place transform passes can consume a source child slot. Source nodes are
	// built post-order, so recover the requested direct child from the immutable
	// source-parent index. Walking backwards visits direct children right-to-left.
	target_from_right := int(node.children_count) - 1 - child_idx
	mut direct_child_from_right := 0
	for source_child := int(id) - 1; source_child >= 0; source_child-- {
		if t.source_parent_id(source_child) != int(id) {
			continue
		}
		if direct_child_from_right == target_from_right {
			return flat.NodeId(source_child)
		}
		direct_child_from_right++
	}
	return none
}

fn (mut t Transformer) transform_struct_pointer_eq(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type_id u32, rhs_type_id u32, lhs_clean u32, rhs_clean u32) ?flat.NodeId {
	pending_base := t.pending_stmts.len
	lhs_ptr := t.stable_transformed_expr_for_reuse(t.transform_expr_preserving_pointer_value(lhs_id), lhs_type_id, 'ptr_eq_lhs')
	rhs_ptr := t.stable_transformed_expr_for_reuse(t.transform_expr_preserving_pointer_value(rhs_id), rhs_type_id, 'ptr_eq_rhs')
	result_name := t.new_temp('ptr_eq')
	// Compare addresses through voidptr casts so a later transform pass does not
	// auto-dereference a mutable pointer-value local in the synthesized identity checks.
	lhs_addr := t.make_cast(flat.source_type_voidptr, lhs_ptr, flat.source_type_voidptr)
	rhs_addr := t.make_cast(flat.source_type_voidptr, rhs_ptr, flat.source_type_voidptr)
	same_ptr := t.make_infix(.eq, lhs_addr, rhs_addr)
	t.pending_stmts << t.make_decl_assign_typed(result_name, same_ptr, flat.source_type_bool)
	lhs_not_nil := t.make_infix(.ne, lhs_addr, t.a.add(.nil_literal))
	rhs_not_nil := t.make_infix(.ne, rhs_addr, t.a.add(.nil_literal))
	both_not_nil := t.make_infix(.logical_and, lhs_not_nil, rhs_not_nil)
	not_same_ptr := t.make_prefix(.not, t.make_ident(result_name))
	compare_values := t.make_infix(.logical_and, not_same_ptr, both_not_nil)
	lhs_value := t.make_prefix(.mul, lhs_ptr)
	rhs_value := t.make_prefix(.mul, rhs_ptr)
	t.set_node_typ(int(lhs_value), lhs_clean)
	t.set_node_typ(int(rhs_value), rhs_clean)
	pending_start := t.pending_stmts.len
	eq_node := flat.Node{
		kind: .infix
		op:   .eq
		typ:  flat.source_type_bool
		pos:  node.pos
	}
	value_eq := t.transform_transformed_struct_eq(eq_node, lhs_value, rhs_value) or {
		t.pending_stmts = t.pending_stmts[..pending_base].clone()
		return none
	}
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body_stmts << t.make_assign(t.make_ident(result_name), value_eq)
	t.pending_stmts << t.make_if(compare_values, t.make_block(body_stmts), t.make_empty())
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	if node.op == .ne {
		not_result := t.make_prefix(.not, result)
		t.set_node_typ(int(not_result), flat.source_type_bool)
		return not_result
	}
	return result
}

fn (mut t Transformer) transform_transformed_struct_eq(node flat.Node, lhs flat.NodeId, rhs flat.NodeId) ?flat.NodeId {
	if node.op in [.eq, .ne] && (t.infix_operand_is_pointer(lhs) || t.infix_operand_is_pointer(rhs)) {
		return none
	}
	mut lhs_type := t.node_type(lhs)
	if flat.source_type_shape(lhs_type).has(.pointer) {
		lhs_type = flat.source_type_syntax(lhs_type).raw_base
	}
	mut rhs_type := t.node_type(rhs)
	if flat.source_type_shape(rhs_type).has(.pointer) {
		rhs_type = flat.source_type_syntax(rhs_type).raw_base
	}
	lhs_struct_type := t.struct_lookup_name(lhs_type)
	rhs_struct_type := t.struct_lookup_name(rhs_type)
	if lhs_struct_type == 0 || rhs_struct_type == 0 {
		return none
	}
	if lhs_struct_type != rhs_struct_type
		&& flat.source_type_syntax(lhs_struct_type).unqualified != flat.source_type_syntax(rhs_struct_type).unqualified {
		return none
	}
	if call_info := t.struct_operator_call_info(lhs_struct_type, node.op) {
		if t.is_disabled_fn_name(call_info.name) {
			ret_type_source_id_4 := t.struct_operator_return_type(call_info.name)

			if ret_type_source_id_4 == 0 || ret_type_source_id_4 == flat.source_type_void {
				return t.make_empty()
			}
			return t.zero_value_for_type(ret_type_source_id_4)
		}
		call_lhs := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'op_lhs')
		call_rhs := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'op_rhs')
		args := if call_info.reverse {
			[call_rhs, call_lhs]
		} else {
			[call_lhs, call_rhs]
		}
		t.mark_struct_operator_used_name(call_info.name)
		ret_type_source_id_2 := t.infix_struct_operator_result_type(node, lhs_struct_type)
		ret_type_source_id_6 := ret_type_source_id_2

		call := t.make_call_typed(call_info.name, args, (if ret_type_source_id_2 != 0 {
			ret_type_source_id_6
		} else {
			node.typ
		}))
		if call_info.negate {
			return t.make_prefix(.not, call)
		}
		return call
	}
	if node.op != .eq && node.op != .ne {
		return none
	}
	if eq_fn := t.struct_operator_fn_name(lhs_struct_type, '==') {
		if t.is_disabled_fn_name(eq_fn) {
			return t.make_bool_literal(node.op == .ne)
		}
		call_lhs := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'eq_lhs')
		call_rhs := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'eq_rhs')
		t.mark_struct_operator_used_name(eq_fn)
		eq_call := t.make_call_typed(eq_fn, [call_lhs, call_rhs], flat.source_type_bool)
		if node.op == .ne {
			return t.make_prefix(.not, eq_call)
		}
		return eq_call
	}
	cmp_lhs := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'eq_lhs')
	cmp_rhs := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'eq_rhs')
	if field_eq := t.make_struct_field_eq_expr(cmp_lhs, cmp_rhs, lhs_struct_type) {
		if node.op == .ne {
			return t.make_prefix(.not, field_eq)
		}
		return field_eq
	}
	cmp := t.make_call_typed('C.memcmp', [t.make_prefix(.amp, cmp_lhs), t.make_prefix(.amp, cmp_rhs),
		t.make_sizeof_type(lhs_struct_type)], flat.source_type_int)
	return t.make_infix(node.op, cmp, t.make_int_literal(0))
}

fn (t &Transformer) infix_operand_is_pointer(id flat.NodeId) bool {
	return t.infix_operand_pointer_type(id) != none
}

fn (t &Transformer) infix_operand_is_language_pointer(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .ident && t.pointer_value_rvalues[node.value] {
		// Some value locals use pointer storage (mutable captures, `for mut` bindings,
		// and heap-promoted locals). Remove that storage indirection, but retain a
		// real pointer layer from the language type (`for mut p in []&T` is stored
		// as `&&T`, while `p` still has the value type `&T`).
		return flat.source_type_shape(t.var_type(node.value)).has(.pointer_chain) && t.infix_operand_is_pointer(id)
	}
	return t.infix_operand_is_pointer(id)
}

fn (t &Transformer) infix_operand_pointer_type(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .nil_literal {
		return flat.source_type_id('&void')
	}
	if flat.source_type_shape(node.typ).has(.pointer) {
		return node.typ
	}
	if node.kind == .ident && flat.source_type_shape(t.var_type(node.value)).has(.pointer) {
		return t.var_type(node.value)
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name_id := typ.source_type_id()
			if typ is types.Pointer {
				return name_id
			}
		}
		resolved := t.tc.resolve_type(id)
		resolved_name := resolved.source_type_id()
		if resolved is types.Pointer {
			return resolved_name
		}
		// A postfix option unwrap (`value?`) is represented as an `or_expr`. Its
		// result metadata can lose the pointer wrapper after generic specialization,
		// while the source still retains the precise `?&T` type.
		if node.kind == .or_expr && node.children_count > 0 {
			source := t.tc.expr_type(t.a.child(&node, 0)) or {
				t.tc.resolve_type(t.a.child(&node, 0))
			}
			if source is types.OptionType {
				base_name := source.base_type.source_type_id()
				if source.base_type is types.Pointer {
					return base_name
				}
			}
			if source is types.ResultType {
				base_name := source.base_type.source_type_id()
				if source.base_type is types.Pointer {
					return base_name
				}
			}
		}
	}
	return none
}

fn (t &Transformer) checker_node_type(id flat.NodeId) u32 {
	if isnil(t.tc) || int(id) < 0 {
		return u32(0)
	}
	name_source_id := t.raw_checker_node_type(id)

	if name_source_id == 0 || name_source_id == flat.source_type_void {
		return u32(0)
	}
	return t.normalize_type_alias(name_source_id)
}

// smartcast_node_type is the type `id` was narrowed to by an enclosing `is` check,
// or '' when it is not smartcast.
fn (t &Transformer) smartcast_node_type(id flat.NodeId) u32 {
	if isnil(t.tc) || int(id) < 0 {
		return u32(0)
	}
	name := t.tc.smartcast_type_name(id)
	if name == 0 || name == flat.source_type_void {
		return u32(0)
	}
	return name
}

fn (t &Transformer) raw_checker_node_type(id flat.NodeId) u32 {
	if isnil(t.tc) || int(id) < 0 {
		return u32(0)
	}
	mut name_source_id := u32(0)
	if typ := t.tc.expr_type_reference(id) {
		name_source_id = t.tc.type_name(typ)
	} else {
		typ := t.tc.resolve_type(id)
		name_source_id = t.tc.type_name(typ)
	}

	if name_source_id == 0 || name_source_id == flat.source_type_void {
		return u32(0)
	}
	return name_source_id
}

fn (t &Transformer) raw_alias_type_for_expr(id flat.NodeId) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		// A collection method call can retain its receiver alias in the parser's
		// node annotation (`DottedKey.last()` annotated as `DottedKey`) even though
		// the checker resolved its result to the element type (`string`). Do not let
		// that stale annotation make an enclosing literal an array of the alias.
		raw_type_source_id := t.raw_checker_node_type(id)

		if raw_type_source_id == 0 {
			return u32(0)
		}
		clean_source_id_2 := t.trim_pointer_type(raw_type_source_id)

		if t.is_type_alias_name(clean_source_id_2) {
			return raw_type_source_id
		}
		return u32(0)
	}
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		if raw_child_type_source_id := t.raw_var_type_for_expr(child_id) {
			clean_child_type_source_id := t.trim_pointer_type(raw_child_type_source_id)

			if t.is_type_alias_name(clean_child_type_source_id)
				|| (t.is_optional_type_name(clean_child_type_source_id)
					&& t.is_type_alias_name(t.optional_base_type(clean_child_type_source_id))) {
				return flat.source_type_composition_one(.pointer, raw_child_type_source_id)
			}
		}
	}
	if raw_var_type_source_id := t.raw_var_type_for_expr(id) {
		raw_var_type_source_id_2 := raw_var_type_source_id

		clean_var_type_source_id := t.trim_pointer_type(raw_var_type_source_id)

		if t.is_type_alias_name(clean_var_type_source_id) {
			return raw_var_type_source_id_2
		}
	}
	raw_type_source_id_2 := t.raw_checker_node_type(id)
	raw_type_shape := flat.source_type_shape(raw_type_source_id_2)
	if raw_type_source_id_2 == 0 {
		return u32(0)
	}
	is_ptr := raw_type_shape.has(.pointer)
	clean_source_id := t.trim_pointer_type(raw_type_source_id_2)

	if t.is_type_alias_name(clean_source_id) {
		return raw_type_source_id_2
	}
	if node.kind == .infix && node.children_count > 0 {
		lhs_id := t.a.child(&node, 0)
		if lhs_type_source_id := t.raw_var_type_for_expr(lhs_id) {
			lhs_type_source_id_4 := lhs_type_source_id

			clean_lhs_type_source_id := t.trim_pointer_type(lhs_type_source_id)

			if t.is_type_alias_name(clean_lhs_type_source_id)
				&& t.normalize_type_alias(clean_lhs_type_source_id) == t.normalize_type_alias(clean_source_id) {
				return lhs_type_source_id_4
			}
		}
	}
	if is_ptr {
		return u32(0)
	}
	return u32(0)
}

fn (t &Transformer) is_type_alias_name(type_id u32) bool {
	if isnil(t.tc) || type_id == 0 {
		return false
	}
	if !isnil(t.type_alias_name_cache) {
		mut cache := t.type_alias_name_cache
		if !same_transform_text(cache.module, t.cur_module) {
			cache.module = t.cur_module
			cache.entries.clear()
			cache.last_name = 0
			cache.last_value = 0
		}
		cached := cache.get(type_id)
		if cached != 0 {
			return cached > 0
		}
		result := t.is_type_alias_name_uncached(type_id)
		cache.put(type_id, if result { i8(1) } else { i8(-1) })
		return result
	}
	return t.is_type_alias_name_uncached(type_id)
}

fn (t &Transformer) is_type_alias_name_uncached(name_id u32) bool {
	if t.tc.type_aliases.contains(flat.source_type_name(name_id)) { return true }
	syntax := flat.source_type_syntax(name_id)
	if syntax.generic_app_valid && syntax.generic_app_base != name_id {
		return t.is_type_alias_name_uncached(syntax.generic_app_base)
	}
	has_dot := flat.source_type_shape(name_id).has(.dotted)
	if !has_dot && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), name_id)
		return flat.source_type_name(qualified) in t.tc.type_aliases.ids
	}
	if !has_dot {
		for alias, _ in t.tc.type_aliases.ids {
			if flat.source_type_syntax(flat.source_type_id(alias)).unqualified == name_id {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) operator_alias_type_for_operand(id flat.NodeId, op flat.Op) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .index && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		if raw_base_source_id := t.raw_var_type_for_expr(base_id) {
			mut elem := flat.source_type_syntax(raw_base_source_id).trimmed
			mut is_pointer_element := false
			if flat.source_type_shape(elem).has(.mutable_type) {
				elem = flat.source_type_syntax(flat.source_type_syntax(elem).raw_base).trimmed
			}
			if flat.source_type_shape(elem).has(.pointer) {
				elem = flat.source_type_syntax(flat.source_type_syntax(elem).raw_base).trimmed
				is_pointer_element = true
			}
			shape := flat.source_type_shape(elem)
			if shape.has(.array_type) {
				elem = flat.source_type_syntax(flat.source_type_syntax(elem).raw_base).trimmed
			} else if shape.has(.map_type) || shape.has(.leading_bracket) {
				syntax := flat.source_type_syntax(elem)
				if syntax.first_bracket_closed {
					elem = flat.source_type_syntax(syntax.first_bracket_base).trimmed
				}
			} else if !is_pointer_element {
				return none
			}
			clean_source_id := t.trim_pointer_type(elem)
			for candidate in [clean_source_id, t.normalize_type_alias(clean_source_id)]! {
				if candidate != 0 && t.is_type_alias_name(candidate) {
					if _ := t.struct_operator_call_info_any(candidate, op) {
						return candidate
					}
				}
			}
			return none
		}
	}
	mut candidates := [3]u32{}
	mut candidate_count := 0
	raw_type_source_id := t.raw_checker_node_type(id)
	if raw_type_source_id != 0 {
		candidates[candidate_count] = raw_type_source_id
		candidate_count++
	}
	if node.typ != 0 {
		candidates[candidate_count] = node.typ
		candidate_count++
	}
	if node.kind == .cast_expr {
		operand_type_source_id := node.source_value_id()
		if operand_type_source_id != 0 {
			candidates[candidate_count] = operand_type_source_id
			candidate_count++
		}
	}
	for i in 0 .. candidate_count {
		clean_source_id_5 := t.trim_pointer_type(candidates[i])

		if clean_source_id_5 == 0 || !t.is_type_alias_name(clean_source_id_5) {
			continue
		}
		if _ := t.struct_operator_call_info_any(clean_source_id_5, op) {
			return clean_source_id_5
		}
	}
	return none
}

// StructOperatorCallInfo stores struct operator call info metadata used by transform.
struct StructOperatorCallInfo {
	name    string
	reverse bool
	negate  bool
}

// struct_operator_call_info supports struct operator call info handling for Transformer.
fn (t &Transformer) struct_operator_call_info(struct_type_id u32, op flat.Op) ?StructOperatorCallInfo {
	if op_name := struct_operator_symbol(op) {
		if method_name := t.struct_operator_fn_name(struct_type_id, op_name) {
			return StructOperatorCallInfo{
				name: method_name
			}
		}
	}
	match op {
		.gt {
			if method_name := t.struct_operator_fn_name(struct_type_id, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
				}
			}
		}
		.ge {
			if method_name := t.struct_operator_fn_name(struct_type_id, '<') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		.le {
			if method_name := t.struct_operator_fn_name(struct_type_id, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
					negate:  true
				}
			}
		}
		.ne {
			if method_name := t.struct_operator_fn_name(struct_type_id, '==') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) struct_operator_call_info_any(struct_type_id u32, op flat.Op) ?StructOperatorCallInfo {
	if op_name := struct_operator_symbol(op) {
		if method_name := t.struct_operator_fn_name_any(struct_type_id, op_name) {
			return StructOperatorCallInfo{
				name: method_name
			}
		}
	}
	match op {
		.gt {
			if method_name := t.struct_operator_fn_name_any(struct_type_id, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
				}
			}
		}
		.ge {
			if method_name := t.struct_operator_fn_name_any(struct_type_id, '<') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		.le {
			if method_name := t.struct_operator_fn_name_any(struct_type_id, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
					negate:  true
				}
			}
		}
		.ne {
			if method_name := t.struct_operator_fn_name_any(struct_type_id, '==') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) struct_operator_call_info_for_operand(struct_type_id u32, op flat.Op, is_alias_operator bool) ?StructOperatorCallInfo {
	if is_alias_operator {
		return t.struct_operator_call_info_any(struct_type_id, op)
	}
	return t.struct_operator_call_info(struct_type_id, op)
}

// struct_operator_symbol supports struct operator symbol handling for transform.
fn struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus {
			return '+'
		}
		.minus {
			return '-'
		}
		.mul {
			return '*'
		}
		.power {
			return '**'
		}
		.div {
			return '/'
		}
		.mod {
			return '%'
		}
		.eq {
			return '=='
		}
		.ne {
			return '!='
		}
		.lt {
			return '<'
		}
		.gt {
			return '>'
		}
		.le {
			return '<='
		}
		.ge {
			return '>='
		}
		else {}
	}

	return none
}

// struct_operator_fn_name supports struct operator fn name handling for Transformer.
fn (t &Transformer) struct_operator_fn_name(struct_type_id u32, op_name string) ?string {
	require_used := op_name in ['==', '!=']
	return t.struct_operator_fn_name_with_usage(struct_type_id, op_name, require_used)
}

fn (t &Transformer) struct_operator_fn_name_any(struct_type_id u32, op_name string) ?string {
	return t.struct_operator_fn_name_with_usage(struct_type_id, op_name, false)
}

fn (t &Transformer) struct_operator_fn_name_with_usage(struct_type_id u32, op_name string, require_used bool) ?string {
	if !isnil(t.tc) {
		if info := t.tc.c_struct_operator_call_info(t.tc.parse_type(struct_type_id), op_name, t.cur_file, t.cur_module) {
			if t.is_known_operator_fn_name(info.name, require_used) { return info.name }
		}
	}
	for receiver in t.operator_receiver_candidates(struct_type_id) {
		method_name := '${flat.source_type_name(receiver)}.${op_name}'
		if t.is_known_operator_fn_name(method_name, require_used) {
			return method_name
		}
		if name := t.generic_struct_operator_fn_name(receiver, op_name) {
			return name
		}
	}
	return none
}

fn (t &Transformer) operator_receiver_candidates(struct_type_id u32) []u32 {
	mut candidates := []u32{cap: 2}
	if struct_type_id == 0 { return candidates }
	candidates << struct_type_id
	if !flat.source_type_shape(struct_type_id).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
		'main',
		'builtin',
	] {
		candidates << flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), struct_type_id)
	}
	return candidates
}

// generic_struct_instance_name returns the resolved generic-struct instance type
// for `type_name` (its base is a known generic struct), else ''. A type alias to a
// generic instance (`SimdFloat4` -> `vec.Vec4[f32]`) is normalized first, so an
// operand typed by its alias still lowers to the monomorphized operator.
fn (t &Transformer) generic_struct_instance_name(type_name_id u32) u32 {
	if isnil(t.tc) { return 0 }
	normalized := t.normalize_type_alias(type_name_id)
	syntax := flat.source_type_syntax(normalized)
	if !syntax.generic_app_valid { return 0 }
	base := syntax.generic_app_base
	if _ := t.generic_struct_params_for_base_id(base) {
		// Qualify imported bases so operator lowering uses the materialized struct.
		resolved := t.struct_lookup_name(base)
		if resolved != 0 && resolved != base {
			return flat.source_type_composition_two(.concat, resolved, syntax.generic_app_legacy_suffix)
		}
		return normalized
	}
	return 0
}

// generic_struct_operator_fn_name handles operator overloads on a generic-struct
// instance (e.g. `Vec4[f32] + Vec4[f32]`). The operator is declared on the generic
// form (`Vec4[T].+`) and specialized by the monomorphizer. Keep the operator
// spelling in the lowered call so signature lookup can distinguish `.+` from an
// ordinary `.plus` method with the same C spelling.
fn (t &Transformer) generic_struct_operator_fn_name(struct_type_id u32, op_name string) ?string {
	if isnil(t.tc) { return none }
	syntax := flat.source_type_syntax(struct_type_id)
	if !syntax.generic_app_valid { return none }
	base := syntax.generic_app_base
	for candidate in [base, flat.source_type_syntax(base).unqualified] {
		params := t.generic_struct_params_for_base_id(candidate) or { continue }
		if params.len == 0 { continue }
		generic_key := '${flat.source_type_name(candidate)}[${params.join(', ')}].${op_name}'
		if generic_key in t.tc.fn_ret_types || generic_key in t.tc.fn_param_types {
			receiver := if flat.source_type_query(struct_type_id).has(.main_prefix) {
				syntax.main_base
			} else {
				struct_type_id
			}
			return '${flat.source_type_name(receiver)}.${op_name}'
		}
	}
	return none
}

fn (t &Transformer) generic_struct_params_for_base(base string) ?[]string {
	return t.generic_struct_params_for_base_id(flat.source_type_id(base))
}

fn (t &Transformer) generic_struct_params_for_base_id(base u32) ?[]string {
	if isnil(t.tc) || base == 0 { return none }
	if params := t.tc.struct_generic_params[flat.source_type_name(base)] { return params }
	short := flat.source_type_syntax(base).unqualified
	if params := t.tc.struct_generic_params[flat.source_type_name(short)] { return params }
	if !flat.source_type_shape(base).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
		'main',
		'builtin',
	] {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
		if params := t.tc.struct_generic_params[flat.source_type_name(qualified)] { return params }
	}
	mut found := []string{}
	for name, params in t.tc.struct_generic_params {
		if flat.source_type_syntax(flat.source_type_id(name)).unqualified != short { continue }
		if found.len > 0 && found != params { return none }
		found = params.clone()
	}
	if found.len > 0 { return found }
	return none
}

fn (t &Transformer) is_known_operator_fn_name(name string, require_used bool) bool {
	if !t.is_known_fn_name(name) {
		return false
	}
	if !require_used || !t.has_any_used_fns() {
		return true
	}
	return t.used_fn_contains_name(name) || t.used_fn_contains_name(c_name(name))
}

// has_struct_operator_fn reports whether has struct operator fn applies in transform.
fn (t &Transformer) has_struct_operator_fn(struct_type_id u32, op_name string) bool {
	if _ := t.struct_operator_fn_name(struct_type_id, op_name) {
		return true
	}
	return false
}

// struct_operator_return_type supports struct operator return type handling for Transformer.
fn (t &Transformer) struct_operator_return_type(fn_name string) u32 {
	if ret_source_id := t.fn_ret_types.type_id(fn_name) {
		return t.normalize_type_alias(ret_source_id)
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[fn_name] {
			return t.normalize_type_alias(ret.source_type_id())
		}
	}
	if fn_name.contains('.') {
		receiver := fn_name.all_before_last('.')
		op_name := fn_name.all_after_last('.')
		if _ := struct_operator_name_to_op(op_name) {
			if ret_source_id := t.generic_struct_operator_return_type_by_name(flat.source_type_id(receiver), op_name) {
				return ret_source_id
			}
		}
	}
	return u32(0)
}

// infix_struct_operator_result_type
// supports helper handling in transform.
fn (t &Transformer) infix_struct_operator_result_type(node flat.Node, lhs_type_in_id u32) u32 {
	if node.children_count < 2 { return 0 }
	// `lhs_type_in_id` is the already-resolved source spelling of the left operand.
	// Reuse it so operator-overload checks do not resolve the operand again.
	lhs_type := if flat.source_type_shape(lhs_type_in_id).has(.pointer) {
		flat.source_type_syntax(lhs_type_in_id).raw_base
	} else {
		lhs_type_in_id
	}
	struct_type := t.struct_lookup_name(lhs_type)
	if struct_type == 0 {
		// Generic-struct instance (`Vec4[f32]`): the specialized operator method is
		// not registered until monomorphization, so derive the result from the
		// generic operator's (substituted) return type.
		instance := t.generic_struct_instance_name(lhs_type)
		return t.generic_struct_operator_return_type(instance, node.op) or { u32(0) }
	}
	if call_info := t.struct_operator_call_info(struct_type, node.op) {
		return t.struct_operator_return_type(call_info.name)
	}
	return 0
}

// generic_struct_operator_return_type returns the result type of an operator on a
// generic-struct instance (`Vec4[f32]`), derived from the generic operator's
// declared return type (`Vec4[T].- -> Vec4[T]`) with the instance's type arguments
// substituted (`-> Vec4[f32]`), qualified with the struct's module so the outer
// expression resolves to the monomorphized operator.
fn (t &Transformer) generic_struct_operator_return_type(struct_type_id u32, op flat.Op) ?u32 {
	if struct_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	op_name := struct_operator_symbol(op) or { return none }
	return t.generic_struct_operator_return_type_by_name(struct_type_id, op_name)
}

fn (t &Transformer) generic_struct_operator_return_type_by_name(struct_type_id u32, op_name string) ?u32 {
	if struct_type_id == 0 || isnil(t.tc) { return none }
	syntax := flat.source_type_syntax(struct_type_id)
	if !syntax.generic_app_valid { return none }
	base := syntax.generic_app_base
	params := t.generic_struct_params_for_base_id(base) or { return none }
	generic_key := '${flat.source_type_name(base)}[${params.join(', ')}].${op_name}'
	mut ret := u32(0)
	if r := t.fn_ret_types.type_id(generic_key) {
		ret = r
	} else if r := t.tc.fn_ret_types[generic_key] {
		ret = r.source_type_id()
	} else {
		return none
	}
	substituted := substitute_generic_type_text_with_params(ret, syntax.generic_app_args, params)
	return_syntax := flat.source_type_syntax(substituted)
	rbase := return_syntax.generic_app_base
	base_syntax := flat.source_type_syntax(base)
	if return_syntax.generic_app_valid && flat.source_type_shape(base).has(.dotted)
		&& !flat.source_type_shape(rbase).has(.dotted) && rbase == base_syntax.unqualified {
		return flat.source_type_composition_two(.qualified, base_syntax.qualifier, substituted)
	}
	return substituted
}

fn struct_operator_name_to_op(op_name string) ?flat.Op {
	match op_name {
		'+' {
			return .plus
		}
		'-' {
			return .minus
		}
		'*' {
			return .mul
		}
		'/' {
			return .div
		}
		'%' {
			return .mod
		}
		'==' {
			return .eq
		}
		'!=' {
			return .ne
		}
		'<' {
			return .lt
		}
		'>' {
			return .gt
		}
		'<=' {
			return .le
		}
		'>=' {
			return .ge
		}
		else {}
	}

	return none
}

// transform_infix_sum_ops transforms transform infix sum ops data for transform.
fn (mut t Transformer) transform_infix_sum_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.op !in [.eq, .ne] || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	mut lhs_type := t.node_type(lhs_id)
	mut rhs_type := t.node_type(rhs_id)
	lhs_is_ptr := flat.source_type_shape(lhs_type).has(.pointer)
	rhs_is_ptr := flat.source_type_shape(rhs_type).has(.pointer)
	if lhs_is_ptr {
		lhs_type = flat.source_type_syntax(lhs_type).raw_base
	}
	if rhs_is_ptr {
		rhs_type = flat.source_type_syntax(rhs_type).raw_base
	}
	lhs_type = t.normalize_type_alias(lhs_type)
	rhs_type = t.normalize_type_alias(rhs_type)
	if !t.is_sum_type_name(lhs_type) && t.generic_arg_is_unresolved(lhs_type) {
		lhs_original := t.normalize_type_alias(t.trim_pointer_type(t.original_expr_type(lhs_id)))
		if t.is_sum_type_name(lhs_original) {
			lhs_type = lhs_original
		}
	}
	if !t.is_sum_type_name(rhs_type) && t.generic_arg_is_unresolved(rhs_type) {
		rhs_original := t.normalize_type_alias(t.trim_pointer_type(t.original_expr_type(rhs_id)))
		if t.is_sum_type_name(rhs_original) {
			rhs_type = rhs_original
		}
	}
	// A specialized generic call can retain its open `T` return type on the
	// call node, even though the surrounding comparison has already resolved
	// the other operand to the concrete sum type.  The checker has validated
	// the comparison, so one concrete sum operand is enough to provide the
	// lowering type for both sides.
	lhs_is_sum := t.is_sum_type_name(lhs_type)
	rhs_is_sum := t.is_sum_type_name(rhs_type)
	if !lhs_is_sum && !rhs_is_sum {
		return none
	}
	if lhs_is_sum != rhs_is_sum {
		unresolved_type := (if lhs_is_sum {
			rhs_type
		} else {
			lhs_type
		})
		sum_type := (if lhs_is_sum {
			lhs_type
		} else {
			rhs_type
		})
		if !t.generic_arg_is_unresolved(unresolved_type)
			&& !t.sum_target_accepts_variant_type(sum_type, unresolved_type) {
			return none
		}
	}
	sum_type_source_id := t.sum_eq_type_for_operands(lhs_type, rhs_type)

	if sum_type_source_id == 0 {
		return none
	}
	mut lhs := t.stable_expr_for_reuse(lhs_id)
	mut rhs := t.stable_expr_for_reuse(rhs_id)
	if lhs_is_ptr {
		lhs = t.make_prefix(.mul, lhs)
		t.set_node_typ(int(lhs), lhs_type)
	}
	if rhs_is_ptr {
		rhs = t.make_prefix(.mul, rhs)
		t.set_node_typ(int(rhs), rhs_type)
	}
	eq := t.make_sum_semantic_eq_expr(lhs, rhs, sum_type_source_id, []EqualityTypeVisit{})
	if node.op == .ne {
		return t.make_prefix(.not, t.make_paren(eq))
	}
	return eq
}

fn (t &Transformer) sum_eq_type_for_operands(lhs_type_id u32, rhs_type_id u32) u32 {
	for candidate in [lhs_type_id, rhs_type_id] {
		if flat.source_type_shape(candidate).has(.bracketed) {
			if variants_source_ids := t.sum_eq_variants(candidate) {
				if variants_source_ids.len > 0 {
					return candidate
				}
			}
		}
	}
	for candidate in [lhs_type_id, rhs_type_id] {
		if t.is_sum_type_name(candidate) {
			return t.resolve_sum_name(candidate)
		}
	}
	return u32(0)
}

// expr_is_bare_nil reports whether an expression yields literally `nil`, seeing through
// the wrappers it is usually written behind. `nil` is only reachable in unsafe code,
// so `x == unsafe { nil }` is the normal spelling and the comparison has to recognise
// the block as the nil it yields. Unlike `expr_is_nil_like` this does not accept `0`,
// which for an option would turn `?int == 0` into a `none` test.
//
// A block is accepted on the strength of its last statement alone, so it may carry others
// before it. The lowering discards the operand entirely, so a caller that acts on this has
// to run `lower_discarded_nil_operand_effects` over the same node to keep them.
fn (t &Transformer) expr_is_bare_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .nil_literal {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.expr_is_bare_nil(t.a.child(&node, 0))
	}
	if node.kind == .block && node.children_count > 0 {
		return t.expr_is_bare_nil(t.a.child(&node, node.children_count - 1))
	}
	return false
}

// transform_infix_optional_none_ops supports transform_infix_optional_none_ops handling.
fn (mut t Transformer) transform_infix_optional_none_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.op !in [.eq, .ne] || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs := t.a.nodes[int(lhs_id)]
	rhs := t.a.nodes[int(rhs_id)]
	mut opt_id := flat.empty_node
	// the operand that yielded the `nil`, when the comparison was written that way
	mut nil_id := flat.empty_node
	if lhs.kind == .none_expr {
		opt_id = rhs_id
	} else if rhs.kind == .none_expr {
		opt_id = lhs_id
	} else if t.expr_is_bare_nil(lhs_id) && t.is_optional_type_name(t.node_type(rhs_id)) {
		// `nil == x` on a `?&T` behaves like `none == x`
		opt_id = rhs_id
		nil_id = lhs_id
	} else if t.expr_is_bare_nil(rhs_id) && t.is_optional_type_name(t.node_type(lhs_id)) {
		opt_id = lhs_id
		nil_id = rhs_id
	} else {
		mut lhs_type := t.raw_expr_type_without_smartcast(lhs_id)
		mut rhs_type := t.raw_expr_type_without_smartcast(rhs_id)
		if !t.is_optional_type_name(lhs_type) {
			lhs_type = t.node_type(lhs_id)
		}
		if !t.is_optional_type_name(rhs_type) {
			rhs_type = t.node_type(rhs_id)
		}
		if !t.is_optional_type_name(lhs_type) || !t.is_optional_type_name(rhs_type) {
			return none
		}
		// This operation compares the __v_option_T wrappers themselves. A payload
		// smartcast left by an earlier assignment must not leak into this comparison
		// or into a following wrapper comparison in the same logical condition.
		t.invalidate_smartcast_for_lvalue(lhs_id)
		t.invalidate_smartcast_for_lvalue(rhs_id)
		lhs_value := t.stable_optional_wrapper_expr_for_reuse(lhs_id, lhs_type, 'opt_eq_lhs')
		rhs_value := t.stable_optional_wrapper_expr_for_reuse(rhs_id, rhs_type, 'opt_eq_rhs')
		eq := t.make_optional_semantic_eq_expr(lhs_value, rhs_value, lhs_type, rhs_type, []EqualityTypeVisit{})
		if node.op == .ne {
			return t.make_prefix(.not, t.make_paren(eq))
		}
		return eq
	}
	if nil_id == lhs_id {
		// `unsafe { record(); nil } == opt`: the left operand is evaluated first.
		t.lower_discarded_nil_operand_effects(nil_id)
	}
	mut opt_type := t.normalize_type_alias(t.optional_result_expr_type_name(opt_id))
	if opt_type == 0 {
		opt_type = t.node_type(opt_id)
	}
	if !t.is_optional_type_name(opt_type) {
		opt_node := t.a.node(opt_id)
		if opt_node.kind == .ident {
			opt_type = t.raw_var_type(opt_node.value)
		}
	}
	if flat.source_type_shape(opt_type).has(.pointer) {
		base := flat.source_type_syntax(opt_type).raw_base
		if t.is_optional_type_name(base) {
			opt_type = t.qualify_optional_type(base)
		}
	}
	if !t.is_optional_type_name(opt_type) {
		outer_pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		transformed_opt := t.transform_expr(opt_id)
		transformed_pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		transformed_type_source_id := t.node_type(transformed_opt)
		transformed_type_source_id_3 := transformed_type_source_id

		if !t.is_optional_type_name(transformed_type_source_id) {
			t.pending_stmts = outer_pending
			return none
		}
		t.pending_stmts = outer_pending
		for stmt in transformed_pending {
			t.pending_stmts << stmt
		}
		opt_id = transformed_opt
		opt_type = transformed_type_source_id_3
	}
	mut opt_expr := t.transform_optional_wrapper_expr(opt_id)
	opt_expr = t.optional_source_value_expr(opt_id, opt_expr, opt_type)
	if nil_id == rhs_id && t.nil_operand_carries_statements(nil_id) {
		// `opt == unsafe { record(); nil }`: the option is the left operand, so it has to be
		// read before `record()` runs. Pin it to a temp first -- left in the expression, its
		// own evaluation would happen after the statements lowered just below it.
		pinned := t.new_temp('opt_nil_eq')
		t.pending_stmts << t.make_decl_assign_typed(pinned, opt_expr, opt_type)
		opt_expr = t.make_ident(pinned)
		t.lower_discarded_nil_operand_effects(nil_id)
	}
	ok := t.make_selector(opt_expr, 'ok', flat.source_type_bool)
	if node.op == .eq {
		return t.make_prefix(.not, ok)
	}
	return ok
}

// nil_operand_carries_statements reports whether a `nil` operand does anything besides
// yielding the nil, i.e. whether lowering it can be skipped entirely.
fn (t &Transformer) nil_operand_carries_statements(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.nil_operand_carries_statements(t.a.child(&node, 0))
	}
	if node.kind != .block || node.children_count == 0 {
		return false
	}
	if node.children_count > 1 {
		return true
	}
	return t.nil_operand_carries_statements(t.a.child(&node, 0))
}

// lower_discarded_nil_operand_effects evaluates the statements that a `nil` operand carries
// besides the `nil` itself. The comparison lowers to a test on the option's `ok` field and
// never mentions the operand again, so without this `opt == unsafe { record(); nil }` would
// silently drop the `record()` call.
fn (mut t Transformer) lower_discarded_nil_operand_effects(id flat.NodeId) {
	// Collect against an empty pending list. Lowering a statement drains whatever is
	// pending, so anything the caller already hoisted -- notably the temp the option was
	// pinned to -- would otherwise be swept into the block below and go out of scope
	// before the comparison that reads it.
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut stmts := []flat.NodeId{}
	t.collect_discarded_nil_operand_effects(id, mut stmts)
	t.drain_pending(mut stmts)
	t.pending_stmts = outer_pending
	if stmts.len == 0 {
		return
	}
	// The operand's own block scope is kept. Splicing the statements straight into the
	// enclosing list would move a block-local declaration up one scope, so
	// `opt == unsafe { local := 1; ... }` followed by an outer `local :=` would be two
	// declarations of the same name in one C scope and would not compile.
	t.pending_stmts << t.make_block(stmts)
}

// collect_discarded_nil_operand_effects gathers the statements a `nil` operand carries
// besides the `nil` itself, keeping one block per source block so that each level's
// declarations stay in their own scope.
fn (mut t Transformer) collect_discarded_nil_operand_effects(id flat.NodeId, mut stmts []flat.NodeId) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		t.collect_discarded_nil_operand_effects(t.a.child(&node, 0), mut stmts)
		return
	}
	if node.kind != .block || node.children_count == 0 {
		return
	}
	for index in 0 .. int(node.children_count) - 1 {
		lowered := t.transform_stmt(t.a.child(&node, index))
		// whatever this statement hoisted comes first, then the statement itself
		t.drain_pending(mut stmts)
		for stmt in lowered {
			stmts << stmt
		}
	}
	// The last statement is what yields the nil. It may itself be a nested block carrying
	// statements of its own, which belong in a nested scope rather than this one.
	mut nested := []flat.NodeId{}
	t.collect_discarded_nil_operand_effects(t.a.child(&node, int(node.children_count) - 1),
		mut nested)
	if nested.len > 0 {
		stmts << t.make_block(nested)
	}
}

// transform_optional_wrapper_expr preserves the __v_option_T wrapper when a prior
// payload assignment has left an option smartcast active for the same expression.
// Wrapper-level operations such as `x == none` and optional equality must inspect
// `.ok` on the wrapper, not on the smartcasted `.value` payload.
fn (mut t Transformer) transform_optional_wrapper_expr(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	mut source_id := id
	for int(source_id) >= 0 && int(source_id) < t.a.nodes.len {
		source := t.a.nodes[int(source_id)]
		if source.kind == .paren && source.children_count == 1 {
			source_id = t.a.child(&source, 0)
			continue
		}
		if source.kind != .selector || source.value != 'value' || source.children_count == 0
			|| transformed_option_unwrap_access_marker !in source.generic_params() {
			break
		}
		base_id := t.a.child(&source, 0)
		if !t.is_optional_type_name(t.raw_expr_type_without_smartcast(base_id)) {
			break
		}
		source_id = base_id
	}
	raw_source_type_source_id := t.raw_expr_type_without_smartcast(source_id)
	mut raw_type := raw_source_type_source_id
	if !t.is_optional_type_name(raw_type) {
		raw_type = t.optional_result_expr_type_name(source_id)
	}
	if t.is_optional_type_name(raw_type) {
		source := t.a.nodes[int(source_id)]
		if source.kind == .index {
			if lowered := t.try_lower_map_index_expr(source_id, source) {
				return t.transform_optional_wrapper_expr(lowered)
			}
			if source.op == .gated_index {
				if lowered := t.lower_gated_scalar_index(source) {
					return t.transform_optional_wrapper_expr(lowered)
				}
			}
			return t.transform_optional_wrapper_index_expr(source_id, source, raw_type)
		}
	}
	if t.is_optional_type_name(raw_type) && t.a.nodes[int(source_id)].kind in [
		.ident,
		.selector,
	] {
		// An optional sum variant gets its wrapper from the active smartcast, not
		// from the declared expression storage. Select that variant before testing
		// `.ok`; otherwise this would emit `.ok` on the outer sum struct.
		if !t.is_optional_type_name(raw_source_type_source_id) && t.has_smartcast(t.expr_key(source_id)) {
			return t.transform_expr(id)
		}
		// `source_id` is the wrapper expression with redundant payload selectors
		// removed. A field selector still needs normal lowering for promoted fields.
		if t.a.nodes[int(source_id)].kind == .selector {
			return t.transform_optional_wrapper_selector_expr(source_id, raw_type)
		}
		if view_param := t.mut_optional_view_param(t.a.nodes[int(source_id)].value) {
			return t.mut_optional_view_read(view_param)
		}
		return t.mark_optional_wrapper_expr(source_id, raw_type)
	}
	return t.transform_expr(id)
}

fn (mut t Transformer) transform_optional_wrapper_selector_expr(id flat.NodeId, raw_type_id u32) flat.NodeId {
	key := t.expr_key(id)
	saved_smartcasts := t.smartcast_stack.clone()
	saved_smartcast_event_id := t.smartcast_event_id
	if key.len > 0 {
		mut remaining_smartcasts := []SmartcastContext{cap: saved_smartcasts.len}
		for smartcast in saved_smartcasts {
			if smartcast.expr_name == key {
				continue
			}
			remaining_smartcasts << smartcast
		}
		t.clear_node_type_memo()
		t.smartcast_stack = remaining_smartcasts
	}
	// Lower promoted fields and their bases before marking the optional wrapper.
	transformed := t.transform_expr(id)
	t.clear_node_type_memo()
	t.smartcast_stack = t.restore_smartcasts_since(saved_smartcast_event_id, saved_smartcasts)
	return t.mark_optional_wrapper_expr(transformed, raw_type_id)
}

fn (mut t Transformer) transform_optional_wrapper_index_expr(id flat.NodeId, node flat.Node, raw_type_id u32) flat.NodeId {
	key := t.expr_key(id)
	saved_smartcasts := t.smartcast_stack.clone()
	saved_smartcast_event_id := t.smartcast_event_id
	if key.len > 0 {
		mut remaining_smartcasts := []SmartcastContext{cap: saved_smartcasts.len}
		for smartcast in saved_smartcasts {
			if smartcast.expr_name == key {
				continue
			}
			remaining_smartcasts << smartcast
		}
		t.clear_node_type_memo()
		t.smartcast_stack = remaining_smartcasts
	}
	transformed := t.transform_index_expr(id, node)
	t.clear_node_type_memo()
	t.smartcast_stack = t.restore_smartcasts_since(saved_smartcast_event_id, saved_smartcasts)
	return t.mark_optional_wrapper_expr(transformed, raw_type_id)
}

fn (mut t Transformer) mark_optional_wrapper_expr(id flat.NodeId, raw_type_id u32) flat.NodeId {
	t.set_node_typ(int(id), raw_type_id)
	mut params := t.a.nodes[int(id)].generic_params().clone()
	params << optional_wrapper_access_marker
	t.set_node_generic_params(int(id), params)
	return id
}

fn (mut t Transformer) stable_optional_wrapper_expr_for_reuse(id flat.NodeId, typ_id u32, prefix string) flat.NodeId {
	expr := t.transform_optional_wrapper_expr(id)
	tmp_name := t.new_temp(prefix)
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, typ_id)
	return t.make_ident(tmp_name)
}

// struct_lookup_name supports struct lookup name handling for Transformer.
fn (t &Transformer) struct_lookup_name(type_name_id u32) u32 {
	if type_name_id == 0 { return 0 }
	// Resolve aliases before consulting indexes whose short names may collide.
	if t.is_type_alias_name(type_name_id) {
		unalias := t.normalize_type_alias(type_name_id)
		if unalias != type_name_id { return t.struct_lookup_name(unalias) }
	}
	shape := flat.source_type_shape(type_name_id)
	syntax := flat.source_type_syntax(type_name_id)
	type_name := flat.source_type_name(type_name_id)
	if shape.has(.dotted) && type_name in t.enum_types { return 0 }
	qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), type_name_id)
	if !shape.has(.dotted) && (type_name in t.enum_types || flat.source_type_name(qualified) in t.enum_types)
		&& !t.bare_struct_name_is_local_to_current_module(type_name_id) {
		if selected := t.selective_import_struct_lookup_name(type_name_id) { return selected }
		if builtin := t.visible_builtin_struct_lookup_name(type_name_id) { return builtin }
		return 0
	}
	// Primitives, arrays and maps never name structs.
	if shape.has(.leading_bracket) || types.is_builtin_type_name_id(type_name_id) || shape.has(.map_type) {
		return 0
	}
	if syntax.generic_app_valid {
		base := syntax.generic_app_base
		resolved := t.struct_lookup_name(base)
		if resolved != 0 {
			if resolved != base {
				return flat.source_type_composition_two(.concat, resolved, syntax.generic_app_legacy_suffix)
			}
			return type_name_id
		}
	}
	if flat.source_type_query(type_name_id).has(.main_prefix) && !flat.source_type_shape(syntax.main_base).has(.dotted)
		&& !t.ident_is_import_alias('main') {
		if info := t.structs[flat.source_type_name(syntax.main_base)] {
			if info.module in ['', 'main'] { return syntax.main_base }
		}
	}
	if shape.has(.dotted) {
		if type_name in t.structs { return type_name_id }
		short := syntax.unqualified
		type_mod := flat.source_type_name(syntax.qualifier)
		if info := t.structs[flat.source_type_name(short)] {
			if info.module == type_mod { return short }
		}
		if info := t.structs[type_name] {
			if info.module == type_mod { return type_name_id }
		}
		checker := t.checker_struct_lookup_name(type_name_id)
		if checker != 0 { return checker }
		return 0
	}
	if info := t.structs[type_name] {
		if info.module.len == 0 || info.module == t.cur_module { return type_name_id }
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		if flat.source_type_name(qualified) in t.structs { return qualified }
	}
	return t.checker_struct_lookup_name(type_name_id)
}

fn (t &Transformer) selective_import_struct_lookup_name(name_id u32) ?u32 {
	if isnil(t.tc) || name_id == 0 || flat.source_type_shape(name_id).has(.dotted) || t.cur_file.len == 0 {
		return none
	}
	name := flat.source_type_name(name_id)
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
		return none
	} {
		if candidate in t.structs || candidate in t.tc.structs {
			return flat.source_type_id(candidate)
		}
	}
	return none
}

// selective_import_type_name_for_file resolves a bare type spelling through the
// selective imports of `file`. The same spelling can name different types in
// different files (`import model { Context }` in one file, `import veb` with its
// own `Context` in another), so callers that know the writing file must resolve
// the name there instead of through a global short-name index.
fn (t &Transformer) selective_import_type_name_for_file(file string, name_id u32) ?u32 {
	if isnil(t.tc) || name_id == 0 || flat.source_type_shape(name_id).has(.dotted) || file.len == 0 {
		return none
	}
	name := flat.source_type_name(name_id)
	for candidate in t.tc.file_selective_imports[file_import_key(file, name)] or { return none } {
		if candidate in t.structs || t.sum_types.contains(candidate) || candidate in t.enum_types
			|| candidate in t.tc.structs || t.tc.sum_types.contains(candidate)
			|| candidate in t.tc.enum_names || candidate in t.tc.interface_names
			|| t.tc.type_aliases.contains(candidate) {
			return flat.source_type_id(candidate)
		}
	}
	return none
}

// visible_builtin_struct_lookup_name resolves a globally visible builtin struct
// only when the current module or file does not shadow it with another type.
fn (t &Transformer) visible_builtin_struct_lookup_name(name_id u32) ?u32 {
	if isnil(t.tc) || name_id == 0 || flat.source_type_shape(name_id).has(.dotted) {
		return none
	}
	name := flat.source_type_name(name_id)
	if t.cur_file.len > 0
		&& file_import_key(t.cur_file, name) in t.tc.file_selective_imports {
		return none
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		local_name := flat.source_type_name(flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), name_id))
		if local_name in t.structs || local_name in t.enum_types || t.sum_types.contains(local_name)
			|| local_name in t.tc.structs || local_name in t.tc.enum_names
			|| t.tc.sum_types.contains(local_name) || t.tc.type_aliases.contains(local_name)
			|| local_name in t.tc.interface_names {
			return none
		}
	} else if t.cur_module != 'builtin'
		&& (name in t.tc.enum_names || t.tc.sum_types.contains(name) || t.tc.type_aliases.contains(name)
			|| name in t.tc.interface_names) {
		return none
	}
	checker_name := t.checker_struct_lookup_name(name_id)
	if checker_name != 0 && t.tc.struct_modules[flat.source_type_name(checker_name)] == 'builtin' {
		return checker_name
	}
	if info := t.structs[name] {
		if info.module == 'builtin' {
			return name_id
		}
	}
	return none
}

// transform_in_expr transforms transform in expr data for transform.
fn (mut t Transformer) transform_in_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	rhs := t.a.nodes[int(rhs_id)]

	is_not_in := node.value == '!in'

	mut result := id
	if rhs.kind == .range {
		// x in low..high  ->  x >= low && x < high
		if rhs.children_count >= 2 {
			// Route value `match`/`if` operands (the tested value and the range bounds)
			// through value lowering so a propagating branch tail is materialized as a
			// value instead of in a value-less statement context, e.g.
			// `x in (match node { ... lower(node)! ... }) .. 10`. `transform_value_operand`
			// is a no-op for the common non-branch operands.
			low_id := t.a.children[rhs.children_start]
			high_id := t.a.children[rhs.children_start + 1]
			// The tested value is evaluated first; if either bound hoists a value branch whose
			// prelude could mutate it, snapshot its source-order value before that prelude.
			bound_hoists := t.operand_hoists_value_branch(low_id)
				|| t.operand_hoists_value_branch(high_id)
			new_lhs := if t.is_value_match_or_if_operand(lhs_id) {
				t.transform_value_operand(lhs_id)
			} else if bound_hoists && t.operand_needs_ordering_snapshot(lhs_id) {
				t.snapshot_expr_for_reuse(lhs_id)
			} else {
				t.stable_expr_for_reuse(lhs_id)
			}
			// If the high bound hoists a value branch — directly or nested inside a compound
			// bound (`.. (1 + (match ...))`) — its materialization below queues prelude
			// statements; snapshot a value-bearing low bound first so it evaluates before them,
			// preserving low-before-high order, e.g.
			// `x in low_with_effect() .. (match node { ... high_with_effect()! ... })`.
			// A value-branch low is materialized in order by `transform_value_operand`.
			new_low := if !t.is_value_match_or_if_operand(low_id)
				&& t.operand_hoists_value_branch(high_id)
				&& t.operand_needs_ordering_snapshot(low_id) {
				t.snapshot_expr_for_reuse(low_id)
			} else {
				t.transform_value_operand(low_id)
			}
			new_high := t.transform_value_operand(high_id)

			ge_cmp := t.make_infix(.ge, new_lhs, new_low)
			lt_cmp := t.make_infix(.lt, new_lhs, new_high)
			result = t.make_infix(.logical_and, ge_cmp, lt_cmp)
		}
	} else if rhs.kind == .array_literal {
		if type_checks := t.lower_type_pattern_membership(lhs_id, rhs, is_not_in) {
			return type_checks
		}
		// x in [a, b, c]  ->  x == a || x == b || x == c
		if rhs.children_count == 0 {
			result = t.make_bool_literal(false)
		} else {
			elem_type_source_id := t.array_literal_membership_elem_type(lhs_id, rhs_id, rhs)

			new_lhs := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(lhs_id, elem_type_source_id), elem_type_source_id, 'in_lhs')
			mut or_chain := flat.empty_node
			for i in 0 .. rhs.children_count {
				elem_id := t.a.children[rhs.children_start + i]
				new_elem := t.transform_expr_for_type(elem_id, elem_type_source_id)
				eq_cmp := t.make_membership_eq_expr(new_lhs, new_elem, elem_type_source_id)
				if int(or_chain) < 0 {
					or_chain = eq_cmp
				} else {
					or_chain = t.make_infix(.logical_or, or_chain, eq_cmp)
				}
			}
			result = or_chain
		}
	} else {
		rhs_type_source_id := t.node_type(rhs_id)

		clean_rhs_type_source_id := t.membership_container_type(rhs_type_source_id)
		clean_rhs_type_source_id_2 := clean_rhs_type_source_id
		clean_rhs_shape := flat.source_type_shape(clean_rhs_type_source_id)
		rhs_is_ptr_array := t.membership_container_is_pointer_array(rhs_type_source_id)
		if clean_rhs_shape.has(.array_type) || clean_rhs_type_source_id == expr_array_type_id {
			if lowered_const := t.lower_const_string_array_membership_expr(rhs_id, lhs_id) {
				result = lowered_const
			} else if lowered := t.lower_array_membership_expr(rhs_id, lhs_id, rhs_type_source_id, false, node) {
				result = lowered
			} else {
				// dynamic array membership -> __v3_array_contains_int/string(arr, val)
				// (value-aware so a `match`/`if` container is materialized as a value)
				mut elem := if clean_rhs_shape.has(.array_type) {
					flat.source_type_syntax(clean_rhs_type_source_id).raw_base
				} else {
					u32(0)
				}
				if elem == 0 {
					elem = t.node_type(lhs_id)
				}
				// Evaluate the needle before materializing a value-branch container so a
				// side-effecting needle precedes the container's hoisted prelude.
				new_lhs := if t.is_value_match_or_if_operand(rhs_id) {
					t.snapshot_transformed_expr_for_reuse(t.transform_expr_for_type(lhs_id, elem), elem, 'in_lhs')
				} else {
					t.transform_expr_for_type(lhs_id, elem)
				}
				mut new_rhs := t.transform_value_operand(rhs_id)
				if rhs_is_ptr_array {
					new_rhs = t.make_prefix(.mul, new_rhs)
				}
				fn_name := array_contains_fn_name(elem)
				result = t.make_call_typed(fn_name, [new_rhs, new_lhs], flat.source_type_bool)
			}
		} else if rhs.kind in [.ident, .selector] && (rhs_type_source_id == 0 || rhs_type_source_id == flat.source_type_unknown) {
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_value_operand(rhs_id)
			mut elem := t.node_type(lhs_id)
			lhs := t.a.nodes[int(lhs_id)]
			if elem == 0 && lhs.kind == .selector {
				elem = t.selector_field_type(lhs)
			}
			if elem == 0 && rhs.kind == .selector {
				elem = t.selector_array_elem_type(rhs)
			}
			if elem != 0 {
				fn_name := array_contains_fn_name(elem)
				result = t.make_call_typed(fn_name, [new_rhs, new_lhs], flat.source_type_bool)
			}
		} else if t.is_fixed_array_type(clean_rhs_type_source_id) {
			if lowered := t.lower_array_membership_expr(rhs_id, lhs_id, rhs_type_source_id, false, node) {
				result = lowered
			} else {
				// fixed array membership -> __v3_fixed_array_contains_int/string(arr, len, val)
				// stabilize a side-effecting needle before a value-branch container hoists
				new_lhs := if t.is_value_match_or_if_operand(rhs_id) {
					t.snapshot_expr_for_reuse(lhs_id)
				} else {
					t.transform_expr(lhs_id)
				}
				new_rhs := t.transform_value_operand(rhs_id)
				elem := fixed_array_elem_type(clean_rhs_type_source_id_2)
				fn_name := fixed_array_contains_fn_name(elem)
				len_expr := t.make_fixed_array_len_expr(clean_rhs_type_source_id_2)
				result = t.make_call_typed(fn_name, [new_rhs, len_expr, new_lhs], flat.source_type_bool)
			}
		} else if clean_rhs_type_source_id == flat.source_type_string {
			// If the container hoists a value branch — directly or nested inside a compound
			// container (`... in wrap(match ...)`) — its materialization below hoists a
			// prelude; stabilize a side-effecting needle first so it evaluates before it,
			// e.g. `tr.needle() in (match n { First { tr.text_first(n)! } ... })`.
			new_lhs := if t.operand_hoists_value_branch(rhs_id) {
				t.snapshot_expr_for_reuse(lhs_id)
			} else {
				t.transform_expr(lhs_id)
			}
			new_rhs := t.transform_value_operand(rhs_id)
			fn_name := if t.node_type(lhs_id) == flat.source_type_u8 {
				'string__contains_u8'
			} else {
				'string__contains'
			}
			result = t.make_call_typed(fn_name, [new_rhs, new_lhs], flat.source_type_bool)
		} else if clean_rhs_shape.has(.map_type) || clean_rhs_type_source_id == expr_map_type_id {
			if lowered := t.lower_map_membership_expr(rhs_id, lhs_id, rhs_type_source_id) {
				result = lowered
			}
		} else {
			// Unknown containment is kept as in_expr so the backend can reject or
			// handle genuinely unresolved cases.
			// stabilize a side-effecting needle before a value-branch container hoists
			new_lhs := if t.is_value_match_or_if_operand(rhs_id) {
				t.snapshot_expr_for_reuse(lhs_id)
			} else {
				t.transform_expr(lhs_id)
			}
			new_rhs := t.transform_value_operand(rhs_id)
			in_start := t.a.children.len
			t.a.children << new_lhs
			t.a.children << new_rhs
			result = t.a.add_node(flat.Node{
				kind:           .in_expr
				op:             node.op
				children_start: in_start
				children_count: 2
				pos:            node.pos
				value:          'in'
				typ:            node.typ
			})
		}
	}

	if is_not_in && result != id {
		parenthesized := t.make_paren(result)
		start := t.a.children.len
		t.a.children << parenthesized
		return t.a.add_node(flat.Node{
			kind:           .prefix
			op:             .not
			children_start: start
			children_count: 1
		})
	}
	return result
}

fn (t &Transformer) array_literal_membership_elem_type(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs flat.Node) u32 {
	if rhs.children_count > 0 {
		first := t.a.nodes[int(t.a.children[rhs.children_start])]
		if first.kind == .enum_val && first.typ == 0 {
			// An unqualified enum shorthand has no standalone type; its membership
			// needle provides the context before the literal defaults to `[]int`.
			lhs_type_source_id := t.node_type(lhs_id)

			if lhs_type_source_id != 0 && lhs_type_source_id != flat.source_type_unknown {
				return lhs_type_source_id
			}
		}
	}
	// The flat literal type can lose a module qualifier in a large import graph
	// (`[]ast.Type` becoming `[]Type`) and collide with an unrelated local struct.
	// When the checker's literal element agrees with the needle after unaliasing,
	// retain the needle's authoritative spelling.
	lhs_type_source_id_2 := t.node_type(lhs_id)

	checker_rhs_type := t.membership_container_type(t.raw_checker_node_type(rhs_id))
	checker_elem := flat.source_type_syntax(checker_rhs_type).raw_base
	if lhs_type_source_id_2 != 0 && lhs_type_source_id_2 != flat.source_type_unknown
		&& flat.source_type_shape(checker_rhs_type).has(.array_type) && checker_elem != 0
		&& t.normalize_type_alias(lhs_type_source_id_2) == t.normalize_type_alias(checker_elem) {
		return lhs_type_source_id_2
	}
	rhs_type := t.membership_container_type(t.node_type(rhs_id))
	rhs_elem := flat.source_type_syntax(rhs_type).raw_base
	if flat.source_type_shape(rhs_type).has(.array_type) && rhs_elem != 0 { return rhs_elem }
	literal_elem := flat.source_type_syntax(rhs.typ).raw_base
	if flat.source_type_shape(rhs.typ).has(.array_type) && literal_elem != 0 { return literal_elem }
	// In `value in [.a, .b]`, the value determines which enum owns the
	// shorthand fields. Resolving the first shorthand on its own is ambiguous
	// in large programs where many enums contain the same field name.
	if lhs_type_source_id_2 != 0 && lhs_type_source_id_2 != flat.source_type_unknown {
		return lhs_type_source_id_2
	}
	if rhs.children_count > 0 {
		first_type_source_id := t.node_type(t.a.children[rhs.children_start])
		first_type_source_id_2 := first_type_source_id

		if first_type_source_id != 0 && first_type_source_id != flat.source_type_unknown {
			return first_type_source_id_2
		}
	}
	return flat.source_type_int
}

// lower_const_string_array_membership_expr lowers `needle in const_string_array` without
// materializing the const array as a dynamic heap array for each membership test.
fn (mut t Transformer) lower_const_string_array_membership_expr(base_id flat.NodeId, needle_id flat.NodeId) ?flat.NodeId {
	expr_id := t.const_expr_for_arg(base_id) or { return none }
	expr := t.a.nodes[int(expr_id)]
	if expr.kind != .array_literal || expr.children_count == 0 {
		return none
	}
	for i in 0 .. expr.children_count {
		child := t.a.nodes[int(t.a.child(&expr, i))]
		if child.kind != .string_literal {
			return none
		}
	}
	// Route the needle through typed value lowering (the container is a string array),
	// so a value `match`/`if` needle materializes its propagating arm as a value instead
	// of in a value-less statement context, e.g.
	// `(match node { First { get_first(node)! } ... }) in allowed_words`.
	needle := t.transform_expr_for_type(needle_id, flat.source_type_string)
	base_value := t.transform_expr(base_id)
	base_data := t.make_cast(expr_string_pointer_type_id, t.make_selector(base_value, 'data', flat.source_type_voidptr), expr_string_pointer_type_id)
	len_expr := t.make_int_literal(expr.children_count)
	return t.make_call_typed('__v3_fixed_array_contains_string', [base_data, len_expr, needle], flat.source_type_bool)
}

// lower_type_pattern_membership builds lower type pattern membership data for transform.
fn (mut t Transformer) lower_type_pattern_membership(lhs_id flat.NodeId, rhs flat.Node, is_not_in bool) ?flat.NodeId {
	if rhs.children_count == 0 {
		return none
	}
	mut patterns := []u32{cap: int(rhs.children_count)}
	mut sum_name_id := u32(0)
	if sc := t.find_smartcast_for_expr(lhs_id) {
		sum_name_id = t.trim_pointer_type(t.smartcast_target_type(sc))
	}
	if sum_name_id == 0 {
		sum_name_id = t.trim_pointer_type(t.original_expr_type(lhs_id))
	}
	if !t.is_sum_type_name(sum_name_id) {
		sum_name_id = t.trim_pointer_type(t.node_type(lhs_id))
	}
	for i in 0 .. rhs.children_count {
		elem_id := t.a.child(&rhs, i)
		elem := t.a.node(elem_id)
		if elem.kind == .selector && elem.children_count > 0 {
			if _ := t.enum_type_from_node(t.a.child(elem, 0)) {
				return none
			}
		}
		pattern_id := t.type_pattern_name(elem_id)
		if pattern_id == 0 {
			return none
		}
		if !t.is_sum_type_name(sum_name_id) {
			pattern_sum_source_id := t.find_sum_type_for_variant(pattern_id)
			if pattern_sum_source_id == 0 {
				return none
			}
			sum_name_id = pattern_sum_source_id
		} else if t.sum_variant_path(sum_name_id, pattern_id).len == 0 {
			return none
		}
		patterns << pattern_id
	}
	if !t.is_sum_type_name(sum_name_id) {
		return none
	}
	// A value-branch subject (`(match node { First { make_foo(node)! } ... }) in [Foo1, Foo3]`)
	// must be lowered as a typed value so its propagating arms are materialized into a temp;
	// plain `stable_expr_for_reuse` would lower it with `transform_expr` in a value-less
	// statement context and emit an empty expression.
	base := if t.is_value_match_or_if_operand(lhs_id) {
		t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(lhs_id, sum_name_id), sum_name_id, 'in_lhs')
	} else {
		t.stable_expr_for_reuse(lhs_id)
	}
	// A non-trivial lhs is materialized as a value temp above. Use that temp's
	// storage type for the tag checks; retaining the source pointer type here
	// makes the generated checks dereference the value temp a second time.
	mut lhs_type := t.node_type(base)
	if lhs_type == 0 || lhs_type == flat.source_type_unknown {
		lhs_type = t.original_expr_type(lhs_id)
	}
	mut chain := flat.empty_node
	for pattern_id in patterns {
		cmp := t.make_sum_type_pattern_check(base, lhs_type, sum_name_id, pattern_id) or { return none }
		if int(chain) < 0 {
			chain = cmp
		} else {
			chain = t.make_infix(.logical_or, chain, cmp)
		}
	}
	if is_not_in {
		return t.make_prefix(.not, t.make_paren(chain))
	}
	return chain
}

// type_pattern_name returns type pattern name data for Transformer.
fn (t &Transformer) type_pattern_name(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.fn_literal, .lambda_expr {
			return t.fn_value_type_name(id) or { u32(0) }
		}
		.ident {
			return node.source_value_id()
		}
		.selector {
			if node.children_count == 0 {
				return flat.source_type_id(node.value)
			}
			base_source_id := t.type_pattern_name(t.a.child(&node, 0))
			if base_source_id == 0 {
				return flat.source_type_id(node.value)
			}
			return flat.source_type_composition_two(.qualified, base_source_id, node.source_value_id())
		}
		.array_init {
			element := node.source_value_id()
			if element == 0 {
				return u32(0)
			}
			return flat.source_type_composition_one(.array, element)
		}
		.array_literal {
			element := node.source_value_id()
			if element == 0 {
				return u32(0)
			}
			return flat.source_type_fixed_array(int(node.children_count), '', element, false)
		}
		else {
			return u32(0)
		}
	}
}

// lower_array_membership_expr builds lower array membership expr data for transform.
fn (mut t Transformer) lower_array_membership_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type_id u32, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type_source_id := t.membership_container_type(base_type_id)
	clean_base_shape := flat.source_type_shape(clean_base_type_source_id)
	base_is_fixed := t.is_fixed_array_type(clean_base_type_source_id)
	if !clean_base_shape.has(.array_type) && clean_base_type_source_id != expr_array_type_id && !base_is_fixed {
		return none
	}
	mut elem_type := (if clean_base_shape.has(.array_type) {
		flat.source_type_syntax(clean_base_type_source_id).raw_base
	} else if base_is_fixed {
		fixed_array_elem_type(clean_base_type_source_id)
	} else {
		u32(0)
	})
	if elem_type == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type_id, clean_base_type_source_id)
		t.drain_pending(mut prefix)
	} else {
		// Resolve the container's concrete generic element before staging the needle,
		// while keeping the needle's pending statements first in source order.
		base = t.stable_array_expr_for_membership(base_id, base_type_id, clean_base_type_source_id)
	}
	elem_type = t.resolved_membership_element_type(base, elem_type)
	if receiver_first {
		needle = t.stable_membership_needle(needle_id, elem_type, 'contains_needle', false)
		t.drain_pending(mut prefix)
	} else {
		mut base_pending := []flat.NodeId{}
		t.drain_pending(mut base_pending)
		// `needle in container`: the needle is evaluated before the container in source order.
		// If the container hoists a value branch whose prelude can mutate a syntactically stable
		// needle (`x in (match node { First { change(mut x)! } ... })`), snapshot the needle's
		// source-order value so the membership loop reads it before that prelude runs.
		needle = t.stable_membership_needle(needle_id, elem_type, 'contains_needle',
			base_pending.len > 0 || t.operand_hoists_value_branch(base_id))
		t.drain_pending(mut prefix)
		prefix << base_pending
	}
	result_name := t.new_temp('contains')
	idx_name := t.new_temp('contains_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_bool_literal(false), flat.source_type_bool)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	len_expr := if base_is_fixed {
		t.make_fixed_array_len_expr(clean_base_type_source_id)
	} else {
		t.make_selector(base, 'len', flat.source_type_int)
	}
	cond := t.make_infix(.lt, t.make_ident(idx_name), len_expr)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := if base_is_fixed {
		t.make_index(base, t.make_ident(idx_name), elem_type)
	} else {
		t.array_get_value(base, t.make_ident(idx_name), elem_type)
	}
	pending_start := t.pending_stmts.len
	eq_expr := t.make_membership_eq_expr(elem_expr, needle, elem_type)
	mut loop_body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	assign_true := t.make_assign(t.make_ident(result_name), t.make_bool_literal(true))
	then_block := t.make_block_skip_scope_drops([assign_true])
	loop_body << t.make_if_with_skip_ownership_drops(eq_expr, then_block, t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_index_expr builds lower array index expr data for transform.
fn (mut t Transformer) lower_array_index_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type_id u32, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type_source_id := t.membership_container_type(base_type_id)
	clean_base_shape := flat.source_type_shape(clean_base_type_source_id)
	base_is_fixed := t.is_fixed_array_type(clean_base_type_source_id)
	if !clean_base_shape.has(.array_type) && clean_base_type_source_id != expr_array_type_id && !base_is_fixed {
		return none
	}
	mut elem_type := (if clean_base_shape.has(.array_type) {
		flat.source_type_syntax(clean_base_type_source_id).raw_base
	} else if base_is_fixed {
		fixed_array_elem_type(clean_base_type_source_id)
	} else {
		u32(0)
	})
	if elem_type == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type_id, clean_base_type_source_id)
		t.drain_pending(mut prefix)
	} else {
		base = t.stable_array_expr_for_membership(base_id, base_type_id, clean_base_type_source_id)
	}
	mut base_pending := []flat.NodeId{}
	if !receiver_first {
		t.drain_pending(mut base_pending)
	}
	elem_type = t.resolved_membership_element_type(base, elem_type)
	needle = t.stable_membership_needle(needle_id, elem_type, 'index_needle',
		!receiver_first && t.operand_hoists_value_branch(base_id))
	t.drain_pending(mut prefix)
	if !receiver_first {
		// The base was transformed first for its element type, but runs after the needle.
		prefix << base_pending
	}
	result_name := t.new_temp('index')
	idx_name := t.new_temp('index_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(-1), flat.source_type_int)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	len_expr := if base_is_fixed {
		t.make_fixed_array_len_expr(clean_base_type_source_id)
	} else {
		t.make_selector(base, 'len', flat.source_type_int)
	}
	cond := t.make_infix(.lt, t.make_ident(idx_name), len_expr)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := if base_is_fixed {
		t.make_index(base, t.make_ident(idx_name), elem_type)
	} else {
		t.array_get_value(base, t.make_ident(idx_name), elem_type)
	}
	pending_start := t.pending_stmts.len
	eq_expr := t.make_membership_eq_expr(elem_expr, needle, elem_type)
	mut loop_body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	not_found := t.make_infix(.lt, t.make_ident(result_name), t.make_int_literal(0))
	found_cond := t.make_infix(.logical_and, not_found, eq_expr)
	assign_idx := t.make_assign(t.make_ident(result_name), t.make_ident(idx_name))
	loop_body << t.make_if_with_skip_ownership_drops(found_cond, t.make_block_skip_scope_drops([assign_idx]), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_last_index_expr builds lower array last index expr data for transform.
fn (mut t Transformer) lower_array_last_index_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type_id u32, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type_source_id := t.membership_container_type(base_type_id)
	clean_base_shape := flat.source_type_shape(clean_base_type_source_id)
	base_is_fixed := t.is_fixed_array_type(clean_base_type_source_id)
	if !clean_base_shape.has(.array_type) && clean_base_type_source_id != expr_array_type_id && !base_is_fixed {
		return none
	}
	mut elem_type := (if clean_base_shape.has(.array_type) {
		flat.source_type_syntax(clean_base_type_source_id).raw_base
	} else if base_is_fixed {
		fixed_array_elem_type(clean_base_type_source_id)
	} else {
		u32(0)
	})
	if elem_type == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type_id, clean_base_type_source_id)
		t.drain_pending(mut prefix)
	} else {
		base = t.stable_array_expr_for_membership(base_id, base_type_id, clean_base_type_source_id)
	}
	mut base_pending := []flat.NodeId{}
	if !receiver_first {
		t.drain_pending(mut base_pending)
	}
	elem_type = t.resolved_membership_element_type(base, elem_type)
	needle = t.stable_membership_needle(needle_id, elem_type, 'last_index_needle',
		!receiver_first && t.operand_hoists_value_branch(base_id))
	t.drain_pending(mut prefix)
	if !receiver_first {
		prefix << base_pending
	}
	result_name := t.new_temp('last_index')
	idx_name := t.new_temp('last_index_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(-1), flat.source_type_int)
	len_expr := if base_is_fixed {
		t.make_fixed_array_len_expr(clean_base_type_source_id)
	} else {
		t.make_selector(base, 'len', flat.source_type_int)
	}
	init := t.make_decl_assign_typed(idx_name, t.make_infix(.minus, len_expr, t.make_int_literal(1)), flat.source_type_int)
	cond := t.make_infix(.ge, t.make_ident(idx_name), t.make_int_literal(0))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .dec))
	elem_expr := if base_is_fixed {
		t.make_index(base, t.make_ident(idx_name), elem_type)
	} else {
		t.array_get_value(base, t.make_ident(idx_name), elem_type)
	}
	pending_start := t.pending_stmts.len
	eq_expr := t.make_membership_eq_expr(elem_expr, needle, elem_type)
	mut loop_body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	not_found := t.make_infix(.lt, t.make_ident(result_name), t.make_int_literal(0))
	found_cond := t.make_infix(.logical_and, not_found, eq_expr)
	assign_idx := t.make_assign(t.make_ident(result_name), t.make_ident(idx_name))
	loop_body << t.make_if_with_skip_ownership_drops(found_cond, t.make_block_skip_scope_drops([assign_idx]), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// stable_array_expr_for_membership
// supports helper handling in transform.
fn (mut t Transformer) stable_array_expr_for_membership(id flat.NodeId, raw_type_id u32, clean_type_id u32) flat.NodeId {
	// Route a value `match`/`if` container through value lowering (e.g.
	// `needle in (match node { ... get_values(node)! ... })`); otherwise the propagating
	// arm tail is lowered in a value-less statement context and emits an empty expression.
	// `transform_value_operand` is a no-op for the common non-branch containers.
	mut expr := t.transform_value_operand(id)
	if t.membership_container_is_pointer_array(raw_type_id) {
		expr = t.make_prefix(.mul, expr)
	}
	mut storage_type := clean_type_id
	transformed_type := t.membership_container_type(t.node_type(expr))
	if transformed_type != storage_type && decl_type_is_usable(transformed_type)
		&& !t.generic_arg_is_unresolved(transformed_type) {
		storage_type = transformed_type
	}
	return t.stable_transformed_expr_for_reuse(expr, storage_type, 'in_arr')
}

fn (mut t Transformer) stable_membership_needle(id flat.NodeId, elem_type_id u32, prefix string, snapshot bool) flat.NodeId {
	if t.is_interface_type(elem_type_id) && !t.membership_type_is_pointer(elem_type_id) {
		// Resolve a generic needle's concrete type before equality boxes it as an interface.
		// Boxing the original expression can retain an unresolved generic payload type.
		return if snapshot {
			t.snapshot_expr_for_reuse(id)
		} else {
			t.stable_expr_for_reuse(id)
		}
	}
	expr := t.transform_expr_for_type(id, elem_type_id)
	return if snapshot {
		t.snapshot_transformed_expr_for_reuse(expr, elem_type_id, prefix)
	} else {
		t.stable_transformed_expr_for_reuse(expr, elem_type_id, prefix)
	}
}

fn (t &Transformer) resolved_membership_element_type(base flat.NodeId, fallback_id u32) u32 {
	base_type_source_id := t.membership_container_type(t.node_type(base))
	if flat.source_type_shape(base_type_source_id).has(.array_type) && decl_type_is_usable(base_type_source_id)
		&& !t.generic_arg_is_unresolved(base_type_source_id) {
		elem := flat.source_type_syntax(base_type_source_id).raw_base
		if elem != fallback_id { return elem }
	}
	return fallback_id
}

enum EqualityTypeVisitKind {
	struct_type
	interface_type
}

struct EqualityTypeVisit {
	typ  u32
	kind EqualityTypeVisitKind
}

// make_membership_eq_expr builds make membership eq expr data for transform.
fn (mut t Transformer) make_membership_eq_expr(lhs flat.NodeId, rhs flat.NodeId, elem_type_id u32) flat.NodeId {
	return t.make_membership_eq_expr_with_seen(lhs, rhs, elem_type_id, []EqualityTypeVisit{})
}

fn (mut t Transformer) make_membership_eq_expr_with_seen(lhs flat.NodeId, rhs flat.NodeId, elem_type_id u32, seen []EqualityTypeVisit) flat.NodeId {
	if t.membership_type_is_pointer(elem_type_id) {
		clean_ptr_source_id := t.membership_container_type(elem_type_id)

		if t.is_sum_type_name(clean_ptr_source_id) {
			lhs_value := t.make_prefix(.mul, lhs)
			t.set_node_typ(int(lhs_value), clean_ptr_source_id)
			rhs_value := t.make_prefix(.mul, rhs)
			t.set_node_typ(int(rhs_value), clean_ptr_source_id)
			return t.make_sum_semantic_eq_expr(lhs_value, rhs_value, clean_ptr_source_id, seen)
		}
		return t.make_infix(.eq, lhs, rhs)
	}
	mut clean := t.membership_container_type(elem_type_id)
	if clean != 0 {
		pointer_type := flat.source_type_composition_one(.pointer, clean)
		if t.node_type(lhs) == pointer_type {
			lhs_value := t.make_prefix(.mul, lhs)
			t.set_node_typ(int(lhs_value), clean)
			return t.make_membership_eq_expr_with_seen(lhs_value, rhs, elem_type_id, seen)
		}
		if t.node_type(rhs) == pointer_type {
			rhs_value := t.make_prefix(.mul, rhs)
			t.set_node_typ(int(rhs_value), clean)
			return t.make_membership_eq_expr_with_seen(lhs, rhs_value, elem_type_id, seen)
		}
	}
	if clean == flat.source_type_string {
		return t.make_call_typed('string__eq', [lhs, rhs], flat.source_type_bool)
	}
	map_type_source_id := t.clean_map_type(clean)
	map_type_shape := flat.source_type_shape(map_type_source_id)
	if map_type_shape.has(.map_type) {
		_, value_type_source_id_3 := t.map_type_parts(map_type_source_id)

		if value_type_source_id_3 != 0 && t.map_value_needs_element_eq(value_type_source_id_3) {
			src := if int(lhs) >= 0 && int(lhs) < t.a.nodes.len {
				t.a.nodes[int(lhs)]
			} else {
				flat.Node{}
			}
			return t.make_map_elementwise_eq_call_with_seen(lhs, rhs, map_type_source_id, src, seen)
		}
		return t.make_call_typed('map_map_eq', [lhs, rhs], flat.source_type_bool)
	}
	if flat.source_type_shape(clean).has(.array_type) {
		inner := flat.source_type_syntax(clean).raw_base
		if inner == flat.source_type_string {
			return t.make_call_typed('__v3_array_eq_string', [lhs, rhs], flat.source_type_bool)
		}
		if t.array_elem_needs_element_eq(inner) {
			src := if int(lhs) >= 0 && int(lhs) < t.a.nodes.len {
				t.a.nodes[int(lhs)]
			} else {
				flat.Node{}
			}
			return t.make_array_elementwise_eq_call_with_seen(lhs, rhs, inner, clean, clean, src, seen)
		}
		if flat.source_type_shape(inner).has(.array_type) {
			return t.make_call_typed('__v3_array_eq_array', [lhs, rhs,
				t.make_int_literal(array_nested_eq_depth(clean))], flat.source_type_bool)
		}
		return t.make_call_typed('__v3_array_eq_raw', [lhs, rhs, t.make_sizeof_type(inner)], flat.source_type_bool)
	}
	if t.is_fixed_array_type(clean) {
		clean = t.resolved_fixed_array_canonical_type(clean)
		if t.type_needs_semantic_eq(clean) {
			if fixed_eq := t.make_fixed_array_elementwise_eq_expr_with_seen(lhs, rhs, clean, seen) {
				return fixed_eq
			}
		}
		lhs_value := t.stable_transformed_expr_for_reuse(lhs, clean, 'fixed_eq_lhs')
		rhs_value := t.stable_transformed_expr_for_reuse(rhs, clean, 'fixed_eq_rhs')
		cmp := t.make_call_typed('C.memcmp', [t.fixed_array_memcmp_addr(lhs_value),
			t.fixed_array_memcmp_addr(rhs_value), t.make_sizeof_type(clean)], flat.source_type_int)
		return t.make_infix(.eq, cmp, t.make_int_literal(0))
	}
	if t.is_optional_type_name(clean) {
		return t.make_optional_semantic_eq_expr(lhs, rhs, clean, clean, seen)
	}
	if t.is_sum_type_name(clean) {
		return t.make_sum_semantic_eq_expr(lhs, rhs, clean, seen)
	}
	if t.is_interface_type(clean) {
		if rhs_boxed := t.box_membership_interface_eq_rhs(rhs, clean) {
			return t.make_interface_semantic_eq_expr(lhs, rhs_boxed, clean, seen)
		}
		return t.make_interface_semantic_eq_expr(lhs, rhs, clean, seen)
	}
	struct_type := t.struct_lookup_name(clean)
	if struct_type != 0 {
		struct_type_id := struct_type
		if method_name := t.struct_operator_fn_name_any(struct_type_id, '==') {
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, [lhs, rhs], flat.source_type_bool)
		}
		if EqualityTypeVisit{ typ: struct_type_id, kind: .struct_type } in seen {
			return t.make_recursive_struct_eq_expr(lhs, rhs, struct_type_id)
		}
		if field_eq := t.make_struct_field_eq_expr_with_seen(lhs, rhs, struct_type_id, seen) {
			return field_eq
		}
		cmp := t.make_call_typed('C.memcmp', [t.make_prefix(.amp, lhs), t.make_prefix(.amp, rhs),
			t.make_sizeof_type(struct_type_id)], flat.source_type_int)
		return t.make_infix(.eq, cmp, t.make_int_literal(0))
	}
	return t.make_infix(.eq, lhs, rhs)
}

// A repeated struct type needs a runtime call, rather than another inline
// expansion or a byte comparison of its strings and container descriptors.
fn (mut t Transformer) make_recursive_struct_eq_expr(lhs flat.NodeId, rhs flat.NodeId, struct_type_id u32) flat.NodeId {
	struct_type := flat.source_type_name(struct_type_id)
	helper_module := if t.sum_eq_helper_module.len > 0 {
		t.sum_eq_helper_module
	} else if t.validating_generic_spec {
		'main'
	} else if t.cur_module.len > 0 {
		t.cur_module
	} else {
		'main'
	}
	base := '__v3_struct_eq_${c_name(struct_type)}'
	helper := if helper_module == 'main' { '${base}__v3_program' } else { base }
	if helper !in t.sum_eq_types {
		t.sum_eq_types[helper] = SumEqRequest{
			struct_name:   struct_type_id
			module:        t.cur_module
			file:          t.cur_file
			helper_module: helper_module
		}
	}
	t.mark_fn_used_name(helper)
	return t.make_call_typed(helper, [lhs, rhs], flat.source_type_bool)
}

fn (mut t Transformer) box_membership_interface_eq_rhs(rhs flat.NodeId, interface_type_id u32) ?flat.NodeId {
	iface_source_id := t.resolve_interface_type_name(interface_type_id)

	if iface_source_id == 0 {
		return none
	}
	if rhs_iface := t.transform_interface_value_for_type(rhs, iface_source_id, true) {
		if rhs_iface != rhs {
			return rhs_iface
		}
	}
	return none
}

fn (mut t Transformer) make_sum_semantic_eq_expr(lhs flat.NodeId, rhs flat.NodeId, sum_type_id u32, seen []EqualityTypeVisit) flat.NodeId {
	_ = seen
	clean_sum_source_id, variants_source_ids_2 := t.sum_eq_type_and_variants(t.trim_pointer_type(sum_type_id)) or {
		return t.make_memcmp_eq_expr(lhs, rhs, sum_type_id, 'eq')
	}
	clean_sum_source_id_2 := clean_sum_source_id
	clean_sum := flat.source_type_name(clean_sum_source_id_2)

	if operator := t.struct_operator_fn_name_any(clean_sum_source_id, '==') {
		t.mark_fn_used_name(operator)
		return t.make_call_typed(operator, [lhs, rhs], flat.source_type_bool)
	}
	if variants_source_ids_2.len == 0 {
		return t.make_memcmp_eq_expr(lhs, rhs, sum_type_id, 'eq')
	}
	// A concrete generic specialization is emitted in main even while it is
	// transformed under its declaring module's type-resolution context.
	helper_module := if t.sum_eq_helper_module.len > 0 {
		t.sum_eq_helper_module
	} else if t.validating_generic_spec {
		'main'
	} else if t.cur_module.len > 0 {
		t.cur_module
	} else {
		'main'
	}
	helper := sum_eq_helper_name_in_module(clean_sum, helper_module)
	if helper !in t.sum_eq_types {
		t.sum_eq_types[helper] = SumEqRequest{
			sum_name:      clean_sum_source_id_2
			module:        t.cur_module
			file:          t.cur_file
			helper_module: helper_module
		}
	}
	t.mark_fn_used_name(helper)
	return t.make_call_typed(helper, [lhs, rhs], flat.source_type_bool)
}

fn (t &Transformer) sum_eq_type_and_variants(sum_type_id u32) ?(u32, []u32) {
	clean := t.normalize_type_alias(sum_type_id)
	if clean == 0 { return none }
	if variants := t.sum_eq_variants(clean) { return clean, variants }
	resolved := t.resolve_sum_name(clean)
	if resolved != clean {
		if variants := t.sum_eq_variants(resolved) { return resolved, variants }
	}
	return none
}

fn (t &Transformer) sum_eq_variants(sum_name_id u32) ?[]u32 {
	sum_name := flat.source_type_name(sum_name_id)
	if sum_name_id == u32(0) {
		return none
	}
	if variants_source_ids := t.sum_types.type_ids(sum_name) {
		return variants_source_ids.clone()
	}
	variants_source_ids := t.concrete_sum_variants_for_candidate(sum_name_id).clone()

	if variants_source_ids.len > 0 {
		return variants_source_ids.clone()
	}
	return none
}

// sum_eq_helper_name is the global C-level name of the synthesized deep-equality
// helper for a sum type.
pub fn sum_eq_helper_name(sum_name string) string {
	return '__v3_sum_eq_${c_name(sum_name)}'
}

// sum_eq_helper_name_in_module keeps program-specific helpers distinct from the
// same stable helper already supplied by an imported module-cache object.
fn sum_eq_helper_name_in_module(sum_name string, helper_module string) string {
	base := sum_eq_helper_name(sum_name)
	if helper_module == 'main' {
		return '${base}__v3_program'
	}
	return base
}

// synthesize_sum_eq_helpers generates requested sum and recursive struct
// equality helpers. Building one helper body can request more nested aggregate
// helpers, so this drains a worklist. Runs
// serially on the merged AST (workers only record names). Returns the names
// newly marked used while building the helper bodies (e.g. a payload struct's
// overloaded `==`), so the caller can run them through the late-used-fn pass —
// synthesis happens after that pass, so such functions would otherwise miss
// body transformation.
pub fn (mut t Transformer) synthesize_sum_eq_helpers() []string {
	old_module := t.cur_module
	old_file := t.cur_file
	old_helper_module := t.sum_eq_helper_module
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	old_temp_counter := t.temp_counter
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []string{}
		for helper, req in t.sum_eq_types {
			if helper in t.sum_eq_synthesized {
				continue
			}
			if helper in t.fn_ret_types.ids {
				// already synthesized by an earlier Transformer over this AST
				t.sum_eq_synthesized[helper] = true
				continue
			}
			if req.sum_name != 0 || req.struct_name != 0 {
				pending << helper
			}
		}
		if pending.len == 0 {
			break
		}
		pending.sort()
		for helper in pending {
			t.sum_eq_synthesized[helper] = true
			req := t.sum_eq_types[helper] or { SumEqRequest{} }
			t.cur_module = req.module
			t.cur_file = req.file
			t.sum_eq_helper_module = if req.helper_module.len > 0 {
				req.helper_module
			} else if req.module.len > 0 {
				req.module
			} else {
				'main'
			}
			if !isnil(t.tc) {
				t.tc.cur_module = req.module
				t.tc.cur_file = req.file
			}
			// A helper is a function of its own: number its temporaries from zero,
			// not from whatever the transform lowered before it.
			t.temp_counter = 0
			if req.struct_name != 0 {
				t.build_struct_eq_helper_fn(req.struct_name, helper)
			} else {
				t.build_sum_eq_helper_fn(req.sum_name, helper)
			}
		}
	}
	mut new_names := []string{}
	mut seen := map[string]bool{}
	for i in log_start .. t.used_fns_log.len {
		name := t.used_fns_log[i]
		if name.len > 0 && !seen[name] {
			seen[name] = true
			new_names << name
		}
	}
	if !was_log_active {
		t.used_fns_log_active = false
		t.used_fns_log = t.used_fns_log[..log_start].clone()
	}
	t.cur_module = old_module
	t.cur_file = old_file
	t.temp_counter = old_temp_counter
	t.sum_eq_helper_module = old_helper_module
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

fn (mut t Transformer) build_struct_eq_helper_fn(struct_type_id u32, helper string) {
	saved_pending := t.pending_stmts
	t.pending_stmts = []flat.NodeId{}
	param_a := t.a.add_node(flat.Node{
		kind:  .param
		value: '__struct_eq_a'
		typ:   struct_type_id
	})
	param_b := t.a.add_node(flat.Node{
		kind:  .param
		value: '__struct_eq_b'
		typ:   struct_type_id
	})
	lhs := t.make_ident('__struct_eq_a')
	t.set_node_typ(int(lhs), struct_type_id)
	rhs := t.make_ident('__struct_eq_b')
	t.set_node_typ(int(rhs), struct_type_id)
	eq := t.make_struct_field_eq_expr(lhs, rhs, struct_type_id) or {
		t.pending_stmts = saved_pending
		return
	}
	mut stmts := t.pending_stmts.clone()
	stmts << t.make_return(eq, flat.source_type_bool)
	t.pending_stmts = saved_pending
	helper_module := if t.sum_eq_helper_module.len > 0 { t.sum_eq_helper_module } else { 'main' }
	t.add_generated_fn_decl_context(helper_module)
	start := t.a.children.len
	t.a.children << param_a
	t.a.children << param_b
	t.a.children << stmts
	fn_decl := t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          helper
		typ:            flat.source_type_bool
		children_start: i32(start)
		children_count: flat.child_count(2 + stmts.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, helper_module, t.cur_file)
	t.register_sum_eq_helper_signature(helper, struct_type_id)
}

// build_sum_eq_helper_fn appends `fn __v3_sum_eq_<Sum>(a Sum, b Sum) bool` to the
// AST: tags must match, then the active variant's inline payload is selected and compared
// by value (recursing through the usual membership-eq machinery, which routes
// nested sum types back through their own helpers).
fn (mut t Transformer) build_sum_eq_helper_fn(clean_sum_id u32, helper string) {
	variants := t.sum_eq_variants(clean_sum_id) or { return }
	if variants.len == 0 {
		return
	}
	saved_pending := t.pending_stmts
	t.pending_stmts = []flat.NodeId{}
	param_a := t.a.add_node(flat.Node{
		kind:  .param
		value: '__sum_eq_a'
		typ:   clean_sum_id
	})
	param_b := t.a.add_node(flat.Node{
		kind:  .param
		value: '__sum_eq_b'
		typ:   clean_sum_id
	})
	mut stmts := []flat.NodeId{}
	lhs_value := t.make_ident('__sum_eq_a')
	t.set_node_typ(int(lhs_value), clean_sum_id)
	rhs_value := t.make_ident('__sum_eq_b')
	t.set_node_typ(int(rhs_value), clean_sum_id)
	tag_ne := t.make_infix(.ne, t.make_sum_tag_selector(lhs_value, .dot), t.make_sum_tag_selector(rhs_value, .dot))
	ret_false := t.make_return(t.make_bool_literal(false), flat.source_type_bool)
	stmts << t.make_if(tag_ne, t.make_block([ret_false]), t.make_empty())
	result_name := t.new_temp('sum_eq_res')
	stmts << t.make_decl_assign_typed(result_name, t.make_bool_literal(true), flat.source_type_bool)
	for variant in variants {
		qv_source_id := t.resolve_variant(clean_sum_id, variant)
		qv_source_id_2 := qv_source_id
		qv := flat.source_type_name(qv_source_id_2)
		if qv_source_id == 0 {
			continue
		}
		field := sum_field_name(qv)
		lhs_payload := t.make_selector_op(lhs_value, field, qv_source_id_2, .dot)
		rhs_payload := t.make_selector_op(rhs_value, field, qv_source_id_2, .dot)
		pending_start := t.pending_stmts.len
		payload_eq := if t.is_sum_type_name(qv_source_id) && t.resolve_sum_name(qv_source_id) == clean_sum_id {
			// a variant that aliases back to this same sum: recurse via the helper
			t.make_call_typed(helper, [lhs_payload, rhs_payload], flat.source_type_bool)
		} else {
			t.make_membership_eq_expr_with_seen(lhs_payload, rhs_payload, qv_source_id, []EqualityTypeVisit{})
		}
		mut body_stmts := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
		body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(payload_eq)), t.make_block([
			set_false,
		]), t.make_empty())
		guard := t.make_infix(.logical_and, t.make_ident(result_name), t.make_sum_is_check(lhs_value, clean_sum_id, clean_sum_id, variant))
		stmts << t.make_if(guard, t.make_block(body_stmts), t.make_empty())
	}
	ret_result := t.make_ident(result_name)
	t.set_node_typ(int(ret_result), flat.source_type_bool)
	stmts << t.make_return(ret_result, flat.source_type_bool)
	t.pending_stmts = saved_pending
	// Keep the helper in its request's output segment. This is normally the requesting
	// module, but program-specific generic specializations and their nested helpers use
	// main even though their bodies are resolved under the declaring module.
	helper_module := if t.sum_eq_helper_module.len > 0 { t.sum_eq_helper_module } else { 'main' }
	t.add_generated_fn_decl_context(helper_module)
	start := t.a.children.len
	t.a.children << param_a
	t.a.children << param_b
	for stmt in stmts {
		t.a.children << stmt
	}
	fn_decl := t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          helper
		typ:            flat.source_type_bool
		children_start: i32(start)
		children_count: flat.child_count(2 + stmts.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, helper_module, t.cur_file)
	t.register_sum_eq_helper_signature(helper, clean_sum_id)
}

fn (t &Transformer) sum_variant_is_direct_pointer(variant_id u32) bool {
	clean := t.trimmed_type_id(t.normalize_type_alias(variant_id))
	return flat.source_type_shape(clean).has(.pointer) || clean in [
		flat.source_type_voidptr,
		flat.source_type_byteptr,
		flat.source_type_charptr,
	]
}

fn (mut t Transformer) register_sum_eq_helper_signature(helper string, clean_sum_id u32) {
	t.set_fn_ret_type(helper, flat.source_type_bool)
	t.mark_fn_used_name(helper)
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_signatures()
		ret := t.tc.parse_type(flat.source_type_bool)
		params := [*t.tc.parse_type(clean_sum_id), *t.tc.parse_type(clean_sum_id)]
		t.tc.register_generated_fn_return_type(helper, ret)
		t.tc.register_generated_fn_param_types(helper, params.clone())
		t.tc.fn_variadic[helper] = false
		t.tc_signature_names_log << helper
	}
}

fn (mut t Transformer) make_memcmp_eq_expr(lhs flat.NodeId, rhs flat.NodeId, typ_id u32, prefix string) flat.NodeId {
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, typ_id, '${prefix}_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, typ_id, '${prefix}_rhs')
	cmp := t.make_call_typed('C.memcmp', [t.fixed_array_memcmp_addr(lhs_value),
		t.fixed_array_memcmp_addr(rhs_value), t.make_sizeof_type(typ_id)], flat.source_type_int)
	return t.make_infix(.eq, cmp, t.make_int_literal(0))
}

fn (mut t Transformer) fixed_array_memcmp_addr(expr flat.NodeId) flat.NodeId {
	if int(expr) >= 0 && int(expr) < t.a.nodes.len {
		node := t.a.nodes[int(expr)]
		if node.kind == .ident && t.fixed_array_param_values[node.value] {
			return expr
		}
	}
	return t.make_prefix(.amp, expr)
}

fn (mut t Transformer) make_optional_semantic_eq_expr(lhs flat.NodeId, rhs flat.NodeId, lhs_opt_type_id u32, rhs_opt_type_id u32, seen []EqualityTypeVisit) flat.NodeId {
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, lhs_opt_type_id, 'opt_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_opt_type_id, 'opt_eq_rhs')
	lhs_ok := t.make_selector(lhs_value, 'ok', flat.source_type_bool)
	rhs_ok := t.make_selector(rhs_value, 'ok', flat.source_type_bool)
	ok_same := t.make_infix(.eq, lhs_ok, rhs_ok)
	lhs_value_type_source_id := t.optional_base_type(lhs_opt_type_id)
	lhs_value_type_source_id_3 := lhs_value_type_source_id

	rhs_value_type_source_id := t.optional_base_type(rhs_opt_type_id)
	rhs_value_type_source_id_3 := rhs_value_type_source_id

	if lhs_value_type_source_id == 0 || lhs_value_type_source_id == flat.source_type_void {
		return ok_same
	}
	rhs_field_type := (if rhs_value_type_source_id == 0 {
		lhs_value_type_source_id_3
	} else {
		rhs_value_type_source_id_3
	})
	lhs_value_field := t.make_selector(lhs_value, 'value', lhs_value_type_source_id)
	rhs_value_field := t.make_selector(rhs_value, 'value', rhs_field_type)
	pending_start := t.pending_stmts.len
	lhs_payload_type := t.normalize_type_alias(lhs_value_type_source_id)
	rhs_payload_type_shape := flat.source_type_shape(t.normalize_type_alias(rhs_field_type))
	lhs_pointee := if flat.source_type_shape(lhs_payload_type).has(.pointer)
		&& rhs_payload_type_shape.has(.pointer) {
		flat.source_type_syntax(lhs_payload_type).raw_base
	} else {
		u32(0)
	}
	value_eq := if lhs_pointee != 0 && t.struct_lookup_name(lhs_pointee) != 0 {
		t.make_optional_pointer_payload_eq_expr(lhs_value_field, rhs_value_field, lhs_pointee, seen)
	} else {
		t.make_membership_eq_expr_with_seen(lhs_value_field, rhs_value_field, lhs_value_type_source_id, seen)
	}
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	if body_stmts.len == 0 {
		ok_or_value_eq := t.make_infix(.logical_or, t.make_prefix(.not, t.make_selector(lhs_value, 'ok', flat.source_type_bool)), value_eq)
		return t.make_infix(.logical_and, ok_same, ok_or_value_eq)
	}
	result_name := t.new_temp('opt_eq')
	t.pending_stmts << t.make_decl_assign_typed(result_name, ok_same, flat.source_type_bool)
	compare_values := t.make_infix(.logical_and, t.make_ident(result_name), t.make_selector(lhs_value, 'ok', flat.source_type_bool))
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(value_eq)), t.make_block([
		set_false,
	]), t.make_empty())
	t.pending_stmts << t.make_if(compare_values, t.make_block(body_stmts), t.make_empty())
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	return result
}

fn (mut t Transformer) make_optional_pointer_payload_eq_expr(lhs flat.NodeId, rhs flat.NodeId, pointee_type_id u32, seen []EqualityTypeVisit) flat.NodeId {
	ptr_same := t.make_infix(.eq, lhs, rhs)
	result_name := t.new_temp('opt_ptr_eq')
	t.pending_stmts << t.make_decl_assign_typed(result_name, ptr_same, flat.source_type_bool)
	lhs_non_nil := t.make_infix(.ne, lhs, t.a.add(.nil_literal))
	rhs_non_nil := t.make_infix(.ne, rhs, t.a.add(.nil_literal))
	both_non_nil := t.make_infix(.logical_and, lhs_non_nil, rhs_non_nil)
	compare_pointees := t.make_infix(.logical_and, t.make_prefix(.not, t.make_ident(result_name)), both_non_nil)
	lhs_value := t.make_prefix(.mul, lhs)
	t.set_node_typ(int(lhs_value), pointee_type_id)
	rhs_value := t.make_prefix(.mul, rhs)
	t.set_node_typ(int(rhs_value), pointee_type_id)
	pending_start := t.pending_stmts.len
	pointee_eq := t.make_membership_eq_expr_with_seen(lhs_value, rhs_value, pointee_type_id, seen)
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body_stmts << t.make_assign(t.make_ident(result_name), pointee_eq)
	t.pending_stmts << t.make_if(compare_pointees, t.make_block(body_stmts), t.make_empty())
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	return result
}

fn (t &Transformer) array_elem_needs_element_eq(elem_type_id u32) bool {
	if t.membership_type_is_pointer(elem_type_id) {
		return false
	}
	clean_source_id_6 := t.membership_container_type(elem_type_id)

	return clean_source_id_6 != flat.source_type_string && t.type_needs_semantic_eq(clean_source_id_6)
}

fn (t &Transformer) map_value_needs_element_eq(value_type_id u32) bool {
	if t.membership_type_is_pointer(value_type_id) {
		return false
	}
	clean_source_id := t.membership_container_type(value_type_id)
	clean_comparison_type_id_6 := clean_source_id

	// Float equality is semantic: +0.0 and -0.0 compare equal even though their
	// representations differ, so the raw map equality helper cannot compare them.
	if clean_comparison_type_id_6 in [flat.source_type_f32, flat.source_type_f64] {
		return true
	}
	if t.is_fixed_array_type(clean_source_id) {
		return true
	}
	return t.type_needs_semantic_eq(clean_source_id)
}

fn (t &Transformer) type_needs_semantic_eq(typ_id u32) bool {
	if t.membership_type_is_pointer(typ_id) {
		return false
	}
	clean_source_id := t.membership_container_type(typ_id)
	if clean_source_id == flat.source_type_string {
		return true
	}
	if flat.source_type_shape(t.clean_map_type(clean_source_id)).has(.map_type) {
		return true
	}
	if flat.source_type_shape(clean_source_id).has(.array_type) {
		// A dynamic array is a descriptor that points at its elements. Comparing the
		// descriptor bytes only compares storage addresses, even when its elements
		// are primitive and can themselves use raw equality.
		return true
	}
	if t.is_fixed_array_type(clean_source_id) {
		elem_type := fixed_array_elem_type(clean_source_id)
		return elem_type != 0 && t.type_needs_semantic_eq(elem_type)
	}
	if t.is_optional_type_name(clean_source_id) {
		return true
	}
	if t.is_interface_type(clean_source_id) {
		return true
	}
	if t.struct_lookup_name(clean_source_id) != 0 {
		return true
	}
	return t.is_sum_type_name(clean_source_id)
}

fn (mut t Transformer) make_array_elementwise_eq_call(lhs flat.NodeId, rhs flat.NodeId, elem_type_id u32, lhs_type_id u32, rhs_type_id u32, src flat.Node) flat.NodeId {
	return t.make_array_elementwise_eq_call_with_seen(lhs, rhs, elem_type_id, lhs_type_id, rhs_type_id, src, []EqualityTypeVisit{})
}

fn (mut t Transformer) make_array_elementwise_eq_call_with_seen(lhs flat.NodeId, rhs flat.NodeId, elem_type_id u32, lhs_type_id u32, rhs_type_id u32, src flat.Node, seen []EqualityTypeVisit) flat.NodeId {
	mut clean_elem := t.membership_container_type(elem_type_id)
	if t.is_fixed_array_type(clean_elem) {
		clean_elem = t.resolved_fixed_array_canonical_type(clean_elem)
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, lhs_type_id, 'arr_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_type_id, 'arr_eq_rhs')
	result_name := t.new_temp('arr_eq')
	idx_name := t.new_temp('arr_eq_idx')
	len_eq := t.make_infix(.eq, t.make_selector(lhs_value, 'len', flat.source_type_int), t.make_selector(rhs_value, 'len', flat.source_type_int))
	t.pending_stmts << t.make_decl_assign_typed(result_name, len_eq, flat.source_type_bool)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	in_bounds := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(lhs_value, 'len', flat.source_type_int))
	cond := t.make_infix(.logical_and, t.make_ident(result_name), in_bounds)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	lhs_elem := t.array_get_value(lhs_value, t.make_ident(idx_name), clean_elem)
	rhs_elem := t.array_get_value(rhs_value, t.make_ident(idx_name), clean_elem)
	pending_start := t.pending_stmts.len
	elem_eq := t.make_membership_eq_expr_with_seen(lhs_elem, rhs_elem, clean_elem, seen)
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(elem_eq)), t.make_block([
		set_false,
	]), t.make_empty())
	t.pending_stmts << t.make_for_stmt(init, cond, post, body_stmts, src)
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	return result
}

fn (mut t Transformer) make_fixed_array_elementwise_eq_expr(lhs flat.NodeId, rhs flat.NodeId, fixed_type_id u32) ?flat.NodeId {
	return t.make_fixed_array_elementwise_eq_expr_with_seen(lhs, rhs, fixed_type_id, []EqualityTypeVisit{})
}

fn (mut t Transformer) make_fixed_array_elementwise_eq_expr_with_seen(lhs flat.NodeId, rhs flat.NodeId, fixed_type_id u32, seen []EqualityTypeVisit) ?flat.NodeId {
	clean_fixed_type_source_id := t.resolved_fixed_array_canonical_type(fixed_type_id)
	clean_fixed_type_source_id_2 := clean_fixed_type_source_id
	elem_type := fixed_array_elem_type(clean_fixed_type_source_id_2)
	if elem_type == 0 {
		return none
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, clean_fixed_type_source_id, 'fixed_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, clean_fixed_type_source_id, 'fixed_eq_rhs')
	result_name := t.new_temp('fixed_eq')
	idx_name := t.new_temp('fixed_eq_idx')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_bool_literal(true), flat.source_type_bool)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_fixed_array_len_expr(clean_fixed_type_source_id_2))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	lhs_elem := t.make_index(lhs_value, t.make_ident(idx_name), elem_type)
	rhs_elem := t.make_index(rhs_value, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	elem_eq := t.make_membership_eq_expr_with_seen(lhs_elem, rhs_elem, elem_type, seen)
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(elem_eq)), t.make_block([
		set_false,
	]), t.make_empty())
	t.pending_stmts << t.make_for_stmt(init, t.make_infix(.logical_and, t.make_ident(result_name), cond), post, body_stmts, flat.Node{})
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	return result
}

fn (mut t Transformer) make_map_elementwise_eq_call(lhs flat.NodeId, rhs flat.NodeId, map_type_id u32, src flat.Node) flat.NodeId {
	return t.make_map_elementwise_eq_call_with_seen(lhs, rhs, map_type_id, src, []EqualityTypeVisit{})
}

fn (mut t Transformer) make_map_elementwise_eq_call_with_seen(lhs flat.NodeId, rhs flat.NodeId, map_type_id u32, src flat.Node, seen []EqualityTypeVisit) flat.NodeId {
	key_type_source_id, raw_value_type_source_id := t.map_type_parts(map_type_id)

	raw_value_type_source_id_2 := raw_value_type_source_id
	raw_value_type_source_id_3 := raw_value_type_source_id_2

	value_type := (if t.is_fixed_array_type(raw_value_type_source_id) {
		t.resolved_fixed_array_canonical_type(raw_value_type_source_id_2)
	} else {
		raw_value_type_source_id_3
	})
	if key_type_source_id == 0 || value_type == 0 {
		return t.make_call_typed('v3_map_map_eq', [lhs, rhs], flat.source_type_bool)
	}
	t.refine_contextual_map_ident_type(lhs, map_type_id)
	t.refine_contextual_map_ident_type(rhs, map_type_id)
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, map_type_id, 'map_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, map_type_id, 'map_eq_rhs')
	result_name := t.new_temp('map_eq')
	key_name := t.new_temp('map_eq_key')
	lhs_val_name := t.new_temp('map_eq_val')
	len_eq := t.make_infix(.eq, t.make_selector(lhs_value, 'len', flat.source_type_int), t.make_selector(rhs_value, 'len', flat.source_type_int))
	t.pending_stmts << t.make_decl_assign_typed(result_name, len_eq, flat.source_type_bool)
	t.set_var_type(key_name, t.map_key_storage_type(key_type_source_id))
	t.set_var_type(lhs_val_name, value_type)
	key_ident := t.make_ident(key_name)
	val_ident := t.make_ident(lhs_val_name)
	rhs_exists := t.make_map_exists_expr(rhs_value, map_type_id, key_name)
	mut body := []flat.NodeId{}
	rhs_val := t.make_map_lookup_value(rhs_value, map_type_id, key_name, value_type, mut body)
	pending_start := t.pending_stmts.len
	value_eq := t.make_membership_eq_expr_with_seen(t.make_ident(lhs_val_name), rhs_val, value_type, seen)
	body << t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	missing := t.make_prefix(.not, t.make_paren(rhs_exists))
	value_diff := t.make_prefix(.not, t.make_paren(value_eq))
	failed := t.make_infix(.logical_or, missing, value_diff)
	active := t.make_infix(.logical_and, t.make_ident(result_name), failed)
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body << t.make_if(active, t.make_block([set_false]), t.make_empty())
	start := t.a.children.len
	t.a.children << key_ident
	t.a.children << val_ident
	t.a.children << lhs_value
	for stmt in body {
		t.a.children << stmt
	}
	mut flags := flat.node_flag_skip_ownership_drops
	// Aggregate equality can call user operators that delete the current map key.
	// Keep its owned copy across those callbacks and the final existence lookup.
	clean_value_type_source_id := t.membership_container_type(value_type)

	if (clean_value_type_source_id == flat.source_type_string || !t.type_needs_semantic_eq(value_type))
		&& !(t.membership_type_is_pointer(value_type) && t.is_sum_type_name(clean_value_type_source_id)) {
		flags |= flat.node_flag_borrowed_map_key
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		pos:            src.pos
		value:          '3'
		flags:          flags
	})
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	return result
}

fn (mut t Transformer) refine_contextual_map_ident_type(id flat.NodeId, map_type_id u32) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return
	}
	_, expected_value_type_source_id := t.map_type_parts(map_type_id)
	expected_value_type_comparison_type_id := expected_value_type_source_id

	if expected_value_type_comparison_type_id in [u32(0), flat.source_type_void,
		flat.source_type_unknown]
		|| t.generic_arg_is_unresolved(expected_value_type_source_id) {
		return
	}
	actual_type_source_id := t.clean_map_type(t.var_type(node.value))

	_, actual_value_type_source_id := t.map_type_parts(actual_type_source_id)
	actual_value_type_comparison_type_id := actual_value_type_source_id

	if actual_value_type_comparison_type_id !in [u32(0), flat.source_type_void,
		flat.source_type_unknown]
		&& !t.generic_arg_is_unresolved(actual_value_type_source_id) {
		return
	}
	t.set_var_type(node.value, map_type_id)
	t.set_node_typ(int(id), map_type_id)
}

fn (mut t Transformer) make_struct_field_eq_expr(lhs flat.NodeId, rhs flat.NodeId, struct_type_id u32) ?flat.NodeId {
	return t.make_struct_field_eq_expr_with_seen(lhs, rhs, struct_type_id, []EqualityTypeVisit{})
}

fn (mut t Transformer) make_struct_field_eq_expr_with_seen(lhs flat.NodeId, rhs flat.NodeId, struct_type_id u32, seen []EqualityTypeVisit) ?flat.NodeId {
	// A main-module struct is spelled bare, which an imported module with a
	// same-named struct reads as its own: compare the fields of the `main.` lock.
	struct_type := t.lock_colliding_main_generic_type_text(struct_type_id, t.cur_module)
	info := t.lookup_struct_info(struct_type) or { return none }
	lhs_value := t.struct_equality_operand(lhs, struct_type, 'struct_eq_lhs')
	rhs_value := t.struct_equality_operand(rhs, struct_type, 'struct_eq_rhs')
	mut next_seen := seen.clone()
	next_seen << EqualityTypeVisit{ typ: struct_type_id, kind: .struct_type }
	mut eq := flat.empty_node
	for field in info.fields {
		field_type_source_id := t.lookup_struct_field_type(struct_type, field.name) or {
			(if field.typ != 0 {
				field.typ
			} else {
				field.raw_typ
			})
		}

		lhs_field := t.make_selector(lhs_value, field.name, field_type_source_id)
		rhs_field := t.make_selector(rhs_value, field.name, field_type_source_id)
		field_eq := if t.membership_type_is_pointer(field_type_source_id) {
			t.make_infix(.eq, lhs_field, rhs_field)
		} else {
			t.make_membership_eq_expr_with_seen(lhs_field, rhs_field, field_type_source_id, next_seen)
		}
		eq = if int(eq) < 0 { field_eq } else { t.make_infix(.logical_and, eq, field_eq) }
	}
	if int(eq) < 0 {
		return t.make_bool_literal(true)
	}
	return eq
}

// struct_equality_operand gives field comparisons stable storage, including
// implicit map/array defaults that are absent from a struct literal's children.
fn (mut t Transformer) struct_equality_operand(expr flat.NodeId, typ_id u32, prefix string) flat.NodeId {
	if t.expr_can_take_address(expr) || !t.compiler_default_clone_type_needs_work(typ_id) {
		return t.stable_transformed_expr_for_reuse(expr, typ_id, prefix)
	}
	tmp_name := t.new_temp(prefix)
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, typ_id)
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) make_interface_semantic_eq_expr(lhs flat.NodeId, rhs flat.NodeId, interface_type_id u32, seen []EqualityTypeVisit) flat.NodeId {
	iface_source_id := t.resolve_interface_type_name(interface_type_id)
	if iface_source_id == 0 || isnil(t.tc) {
		return t.make_memcmp_eq_expr(lhs, rhs, interface_type_id, 'iface_eq')
	}
	seen_key := EqualityTypeVisit{ typ: iface_source_id, kind: .interface_type }
	if seen_key in seen {
		return t.make_memcmp_eq_expr(lhs, rhs, iface_source_id, 'iface_eq_cycle')
	}
	mut next_seen := seen.clone()
	next_seen << seen_key
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, iface_source_id, 'iface_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, iface_source_id, 'iface_eq_rhs')
	lhs_typ := t.make_selector(lhs_value, '_typ', flat.source_type_int)
	rhs_typ := t.make_selector(rhs_value, '_typ', flat.source_type_int)
	lhs_zero_tag := t.make_infix(.eq, lhs_typ, t.make_int_literal(0))
	rhs_zero_tag := t.make_infix(.eq, rhs_typ, t.make_int_literal(0))
	zero_tags := t.make_infix(.logical_and, lhs_zero_tag, rhs_zero_tag)
	empty_eq := if t.is_builtin_ierror_interface_name(iface_source_id) {
		lhs_addr := t.make_prefix(.amp, lhs_value)
		rhs_addr := t.make_prefix(.amp, rhs_value)
		lhs_msg := t.make_call_typed('IError__msg', [lhs_addr], flat.source_type_string)
		rhs_msg := t.make_call_typed('IError__msg', [rhs_addr], flat.source_type_string)
		msg_eq := t.make_call_typed('string__eq', [lhs_msg, rhs_msg], flat.source_type_bool)
		lhs_code := t.make_call_typed('IError__code', [lhs_addr], flat.source_type_int)
		rhs_code := t.make_call_typed('IError__code', [rhs_addr], flat.source_type_int)
		code_eq := t.make_infix(.eq, lhs_code, rhs_code)
		t.make_infix(.logical_and, zero_tags, t.make_infix(.logical_and, msg_eq, code_eq))
	} else {
		lhs_empty := t.make_infix(.eq, t.make_selector(lhs_value, '_object', flat.source_type_voidptr), t.make_int_literal(0))
		rhs_empty := t.make_infix(.eq, t.make_selector(rhs_value, '_object', flat.source_type_voidptr), t.make_int_literal(0))
		t.make_infix(.logical_and, zero_tags, t.make_infix(.logical_and, lhs_empty, rhs_empty))
	}
	result_name := t.new_temp('iface_eq_payload')
	t.pending_stmts << t.make_decl_assign_typed(result_name, empty_eq, flat.source_type_bool)
	impl_names := if t.is_builtin_ierror_interface_name(iface_source_id) {
		t.tc.ierror_impl_names()
	} else {
		t.interface_impl_index_for_transform(iface_source_id).names.ids.clone()
	}
	for impl_id in impl_names {
		if !t.interface_boxed_type_marked(iface_source_id, impl_id) {
			continue
		}
		type_id := t.interface_impl_type_id(iface_source_id, impl_id) or { continue }
		impl_pointer_id := flat.source_type_composition_one(.pointer, impl_id)
		lhs_object := t.make_cast(impl_pointer_id, t.make_selector(lhs_value, '_object', flat.source_type_voidptr), impl_pointer_id)
		rhs_object := t.make_cast(impl_pointer_id, t.make_selector(rhs_value, '_object', flat.source_type_voidptr), impl_pointer_id)
		lhs_concrete := t.make_prefix(.mul, lhs_object)
		rhs_concrete := t.make_prefix(.mul, rhs_object)
		t.set_node_typ(int(lhs_concrete), impl_id)
		t.set_node_typ(int(rhs_concrete), impl_id)
		saved := t.pending_stmts.clone()
		t.pending_stmts.clear()
		value_eq := t.make_membership_eq_expr_with_seen(lhs_concrete, rhs_concrete, impl_id, next_seen)
		mut then_body := []flat.NodeId{}
		t.drain_pending(mut then_body)
		t.pending_stmts = saved
		then_body << t.make_assign(t.make_ident(result_name), value_eq)
		lhs_case := t.make_infix(.eq, lhs_typ, t.make_int_literal(type_id))
		rhs_case := t.make_infix(.eq, rhs_typ, t.make_int_literal(type_id))
		cond := t.make_infix(.logical_and, lhs_case, rhs_case)
		t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	}
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), flat.source_type_bool)
	return result
}

// selector_field_type supports selector field type handling for Transformer.
fn (t &Transformer) selector_field_type(node flat.Node) u32 {
	if node.value.len == 0 {
		return u32(0)
	}
	for _, info in t.structs {
		for field in info.fields {
			if field.name == node.value {
				return t.normalize_type_alias(field.typ)
			}
		}
	}
	return u32(0)
}

// selector_array_elem_type supports selector array elem type handling for Transformer.
fn (t &Transformer) selector_array_elem_type(node flat.Node) u32 {
	if node.value.len == 0 {
		return u32(0)
	}
	for _, info in t.structs {
		for field in info.fields {
			clean_typ := t.membership_container_type(field.typ)
			if field.name == node.value && flat.source_type_shape(clean_typ).has(.array_type) {
				return t.normalize_type_alias(flat.source_type_syntax(clean_typ).raw_base)
			}
		}
	}
	return u32(0)
}

// membership_container_type supports membership container type handling for Transformer.
fn (t &Transformer) membership_container_type(typ_id u32) u32 {
	mut clean := t.trimmed_type_id(t.normalize_type_alias(typ_id))
	for {
		shape := flat.source_type_shape(clean)
		if shape.has(.shared_type) {
			clean = t.unshared_type_id(clean)
			continue
		}
		if shape.has(.pointer) {
			clean = t.trimmed_type_id(t.trim_pointer_type(clean))
			continue
		}
		if shape.has(.variadic) {
			clean = t.variadic_array_type_id(clean)
			continue
		}
		break
	}
	return t.normalize_type_alias(clean)
}

fn (t &Transformer) membership_type_is_pointer(typ_id u32) bool {
	mut clean := t.trimmed_type_id(t.normalize_type_alias(typ_id))
	for {
		shape := flat.source_type_shape(clean)
		if shape.has(.shared_type) {
			clean = t.unshared_type_id(clean)
			continue
		}
		if shape.has(.mutable_type) {
			clean = t.trimmed_type_id(flat.source_type_syntax(clean).raw_base)
			continue
		}
		break
	}
	return flat.source_type_shape(clean).has(.pointer)
}

fn (t &Transformer) equality_pointer_type(typ_id u32) u32 {
	mut clean := t.trimmed_type_id(typ_id)
	for flat.source_type_shape(clean).has(.shared_type) {
		clean = t.unshared_type_id(clean)
	}
	if flat.source_type_shape(clean).has(.mutable_type) {
		return 0
	}
	clean = t.trimmed_type_id(t.normalize_type_alias(clean))
	for flat.source_type_shape(clean).has(.shared_type) {
		clean = t.unshared_type_id(clean)
	}
	return clean
}

fn (t &Transformer) equality_type_is_array_pointer(typ_id u32) bool {
	clean := t.equality_pointer_type(typ_id)
	if !flat.source_type_shape(clean).has(.pointer) {
		return false
	}
	container := t.membership_container_type(clean)
	return flat.source_type_shape(container).has(.array_type) || container == expr_array_type_id
}

fn (t &Transformer) equality_expr_is_string_pointer(id flat.NodeId, typ_id u32) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.string_literal, .string_interp] {
		return false
	}
	return t.equality_pointer_type(typ_id) == expr_string_pointer_type_id
}

fn (t &Transformer) equality_type_is_map_pointer(typ_id u32) bool {
	clean := t.equality_pointer_type(typ_id)
	return flat.source_type_shape(clean).has(.pointer)
		&& flat.source_type_shape(t.clean_map_type(clean)).has(.map_type)
}

// membership_container_is_pointer_array supports membership_container_is_pointer_array handling.
fn (t &Transformer) membership_container_is_pointer_array(typ_id u32) bool {
	mut clean := t.trimmed_type_id(t.normalize_type_alias(typ_id))
	mut shape := flat.source_type_shape(clean)
	for shape.has(.shared_type) {
		clean = t.unshared_type_id(clean)
		shape = flat.source_type_shape(clean)
	}
	if !shape.has(.pointer) {
		return false
	}
	container := t.membership_container_type(clean)
	return flat.source_type_shape(container).has(.array_type) || container == expr_array_type_id
}

// array_contains_fn_name reports whether array contains fn name applies in transform.
fn array_contains_fn_name(elem u32) string {
	return match elem {
		flat.source_type_string { '__v3_array_contains_string' }
		flat.source_type_u8 { '__v3_array_contains_u8' }
		else { '__v3_array_contains_int' }
	}
}

// fixed_array_contains_fn_name reports whether fixed array contains fn name applies in transform.
fn fixed_array_contains_fn_name(elem u32) string {
	return match elem {
		flat.source_type_string { '__v3_fixed_array_contains_string' }
		flat.source_type_u8 { '__v3_fixed_array_contains_u8' }
		else { '__v3_fixed_array_contains_int' }
	}
}

// stable_expr_for_reuse supports stable expr for reuse handling for Transformer.
fn (mut t Transformer) stable_expr_for_reuse(id flat.NodeId) flat.NodeId {
	expr := if _ := t.generated_variant_access_type(id) {
		id
	} else {
		t.transform_expr(id)
	}
	if t.is_stable_expr_for_reuse(expr) {
		return expr
	}
	tmp_name := t.new_temp('in_lhs')
	mut tmp_typ := t.node_type(expr)
	if tmp_typ == 0 {
		tmp_typ = t.node_type(id)
	}
	decl := t.make_decl_assign(tmp_name, expr)
	if tmp_typ != 0 {
		t.set_node_typ(int(decl), tmp_typ)
		t.set_var_type(tmp_name, tmp_typ)
	}
	t.pending_stmts << decl
	return t.make_ident(tmp_name)
}

// stable_transformed_expr_for_reuse
// supports helper handling in transform.
fn (mut t Transformer) stable_transformed_expr_for_reuse(expr flat.NodeId, typ_id u32, prefix string) flat.NodeId {
	if t.is_stable_expr_for_reuse(expr) {
		return expr
	}
	tmp_name := t.new_temp(prefix)
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, typ_id)
	return t.make_ident(tmp_name)
}

// materialize_comparison_literal preserves contextual element types when an operand's
// propagation or value branch requires the other operand to be evaluated into a temp.
fn (mut t Transformer) materialize_comparison_literal(op flat.Op, id flat.NodeId, peer flat.NodeId) ?flat.NodeId {
	if op !in [.eq, .ne] {
		return none
	}
	node := t.a.nodes[int(id)]
	target := t.membership_container_type(t.node_type(peer))
	shape := flat.source_type_shape(target)
	if (node.kind == .array_literal && shape.has(.array_type))
		|| (node.kind == .map_init && shape.has(.map_type)) {
		expr := t.transform_expr_for_type(id, target)
		return t.stable_transformed_expr_for_reuse(expr, target, 'comparison_literal')
	}
	return none
}

// snapshot_expr_for_reuse materializes `id` into a temp holding its current value, unless it is
// a pure constant (which cannot change, so needs no snapshot). Ordering guards use it to capture
// the source-order value of an operand that precedes a value branch whose hoisted prelude might
// mutate that operand's storage. Unlike stable_expr_for_reuse it does snapshot value-bearing
// lvalues (idents/selectors/indexes) rather than leaving them inline.
fn (mut t Transformer) snapshot_expr_for_reuse(id flat.NodeId) flat.NodeId {
	if t.is_ordering_snapshot_temp(id) {
		return id
	}
	source_id := t.unwrap_parens(id)
	pointer_type_source_id := t.lvalue_type(source_id)
	pointer_shape := flat.source_type_shape(pointer_type_source_id)
	source := t.a.nodes[int(source_id)]
	borrowed_pointer_value := source.kind == .ident && t.pointer_value_rvalues[source.value]
		&& !t.mut_param_values[source.value] && pointer_shape.has(.pointer_chain)
	preserve_pointer_value := pointer_shape.has(.pointer)
	// A block expression (notably `unsafe { ... }`) drains pending statements into
	// its own statement-expression. Do not let it capture snapshots belonging to
	// earlier call operands; those declarations must remain in the surrounding
	// scope where the eventual call can refer to them.
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	expr := if _ := t.generated_variant_access_type(id) {
		id
	} else if borrowed_pointer_value {
		// Mutable pointer-valued iteration stores a pointer to the entry slot.
		// Capture the pointee before a later argument can replace that entry.
		t.transform_expr(id)
	} else if preserve_pointer_value {
		t.transform_expr_preserving_pointer_value(id)
	} else {
		t.transform_expr(id)
	}
	expr_pending := t.pending_stmts.clone()
	t.pending_stmts = outer_pending
	t.pending_stmts << expr_pending
	if t.is_pure_constant_expr(expr) || t.is_ordering_snapshot_temp(expr) {
		return expr
	}
	tmp_name := t.new_temp('order_snapshot')
	mut tmp_typ := (if borrowed_pointer_value {
		flat.source_type_syntax(pointer_type_source_id).raw_base
	} else if preserve_pointer_value {
		pointer_type_source_id
	} else {
		t.node_type(expr)
	})
	if tmp_typ == 0 {
		tmp_typ = t.node_type(id)
	}
	decl := t.make_decl_assign(tmp_name, expr)
	if tmp_typ != 0 {
		t.set_node_typ(int(decl), tmp_typ)
		t.set_var_type(tmp_name, tmp_typ)
	}
	t.ordering_snapshot_names[tmp_name] = true
	t.pending_stmts << decl
	return t.make_ident(tmp_name)
}

// snapshot_transformed_expr_for_reuse is snapshot_expr_for_reuse for an already-transformed
// expression of known type.
fn (mut t Transformer) snapshot_transformed_expr_for_reuse(expr flat.NodeId, typ_id u32, prefix string) flat.NodeId {
	if t.is_pure_constant_expr(expr) || t.is_ordering_snapshot_temp(expr) {
		return expr
	}
	tmp_name := t.new_temp(prefix)
	t.ordering_snapshot_names[tmp_name] = true
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, typ_id)
	return t.make_ident(tmp_name)
}

// callee_base_is_not_a_runtime_value reports whether the base of a selector callee provably does
// not denote a value, which means it names a compile-time namespace that
// call_selector_base_is_namespace failed to recognize (`os.abs_path(...)`,
// `shapes.Box.make(...)`). Such a base has no storage, so a later branch prelude cannot change
// what it observes and leaving it inline forfeits no ordering guarantee — whereas spilling it
// declares a temp at a name no C declaration can use (`unknown __order_snapshot_0 = os;`,
// `void __order_snapshot_0 = shapes__Box;`).
//
// The test is deliberately positive rather than "the type text looks unusable". `node_type`
// answers `unknown` for anything it cannot resolve, an ordinary runtime operand included, so that
// spelling alone would silently drop the source-order guarantee the guards exist to provide.
// `void` is conclusive by itself because no value carries it. `unknown` counts only for a bare
// identifier that names nothing in scope: a runtime receiver is always a binding, and a generic
// clone still records one even where it has lost the expression type.
//
// Restricted to the callee-base position, the only operand a namespace can occupy.
fn (t &Transformer) callee_base_is_not_a_runtime_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	typ := t.trimmed_type_id(t.node_type(id))
	if typ == flat.source_type_void {
		return true
	}
	if typ != flat.source_type_unknown {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .ident && (t.raw_var_type(node.value)) == 0
}

// is_ordering_snapshot_temp reports whether `id` is an identifier naming a temp already created
// by a snapshot_*_for_reuse call. Such a temp holds a captured source-order value that no branch
// prelude mutates, so it must not be snapshotted again (which would recurse on a re-dispatch).
fn (t &Transformer) is_ordering_snapshot_temp(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .ident && node.value in t.ordering_snapshot_names
}

// operand_needs_ordering_snapshot reports whether a preceding operand must be snapshotted to
// preserve its source-order value before a later value branch's hoisted prelude runs: it is a
// value-bearing lvalue read (not a pure constant) and is not already a snapshot temp.
fn (t &Transformer) operand_needs_ordering_snapshot(id flat.NodeId) bool {
	return !t.is_pure_constant_expr(id) && !t.is_ordering_snapshot_temp(id)
}

// is_stable_expr_for_reuse reports whether is stable expr for reuse applies in transform.
fn (t &Transformer) is_stable_expr_for_reuse(id flat.NodeId) bool {
	if int(id) < 0 {
		return true
	}
	node := t.a.nodes[int(id)]
	return match node.kind {
		.ident, .int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.nil_literal, .none_expr, .enum_val, .sizeof_expr, .typeof_expr {
			true
		}
		.selector {
			node.children_count > 0 && t.is_stable_expr_for_reuse(t.a.children[node.children_start])
		}
		.field_init {
			node.children_count == 0
				|| t.is_stable_expr_for_reuse(t.a.children[node.children_start])
		}
		.struct_init {
			mut stable := true
			if info := t.lookup_struct_info(node.source_value_id()) {
				for field in info.fields {
					if t.is_fixed_array_type(t.normalize_type_alias(field.typ)) {
						// Fixed-array initialization can emit a C statement expression.
						// Its result and fields need storage before taking their address.
						stable = false
						break
					}
				}
			}
			for i in 0 .. node.children_count {
				if !t.is_stable_expr_for_reuse(t.a.child(&node, i)) {
					stable = false
					break
				}
			}
			stable
		}
		.cast_expr {
			node.children_count == 0
				|| t.is_stable_expr_for_reuse(t.a.children[node.children_start])
		}
		.index {
			node.children_count >= 2
				&& t.is_stable_expr_for_reuse(t.a.children[node.children_start])
				&& t.is_stable_expr_for_reuse(t.a.children[node.children_start + 1])
		}
		else {
			false
		}
	}
}

// is_pure_constant_expr reports whether `id`'s value cannot be changed by a later mutation of
// any variable — a literal, declared function reference, enum value, `sizeof`/`typeof`, or a
// cast/paren/struct/unary operator made only of such, or arithmetic on numeric literals.
// Unlike is_stable_expr_for_reuse it returns false for storage reads (idents, selectors,
// indexes): those are cheap to re-evaluate, but a branch prelude can mutate their storage, so
// reading them after the prelude yields a different value. Ordering guards use this to decide
// whether a preceding operand must be snapshotted to preserve its source-order value.
fn (t &Transformer) is_pure_constant_expr(id flat.NodeId) bool {
	if int(id) < 0 {
		return true
	}
	node := t.a.nodes[int(id)]
	return match node.kind {
		.ident {
			!t.static_assoc_ident_is_value(id) && t.ident_is_direct_function_callee(node.value)
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .nil_literal,
		.none_expr, .enum_val, .sizeof_expr, .typeof_expr, .empty {
			true
		}
		.cast_expr, .paren {
			node.children_count == 0 || t.is_pure_constant_expr(t.a.children[node.children_start])
		}
		.prefix {
			// A signed or negated constant, such as `-1.2` or `~0`.
			node.op in [.minus, .plus, .bit_not, .not] && node.children_count > 0
				&& t.is_pure_constant_expr(t.a.children[node.children_start])
		}
		.infix {
			// Arithmetic on numeric literals, such as `1.2 + 0.1`. Struct operands are
			// excluded: their operators are user methods.
			node.children_count == 2 && t.is_pure_constant_expr(t.a.child(&node, 0))
				&& t.is_pure_constant_expr(t.a.child(&node, 1))
				&& t.is_untyped_numeric_literal_expr(id)
		}
		.struct_init, .field_init {
			mut pure := true
			for i in 0 .. node.children_count {
				if !t.is_pure_constant_expr(t.a.child(&node, i)) {
					pure = false
					break
				}
			}
			pure
		}
		else {
			false
		}
	}
}

// transform_fixed_array_len transforms transform fixed array len data for transform.
fn (mut t Transformer) transform_fixed_array_len(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.value != 'len' || node.children_count == 0 {
		return none
	}
	base_id := t.a.children[node.children_start]
	base_type_source_id := t.node_type(base_id)
	base_type_source_id_2 := base_type_source_id

	if !t.is_fixed_array_type(base_type_source_id) {
		return none
	}
	return t.make_fixed_array_len_expr(base_type_source_id_2)
}

// clean_map_type transforms clean map type data for transform.
fn (t &Transformer) clean_map_type(typ_id u32) u32 {
	mut clean := t.trimmed_type_id(t.normalize_type_alias(typ_id))
	for {
		shape := flat.source_type_shape(clean)
		if shape.has(.shared_type) {
			clean = t.unshared_type_id(clean)
			continue
		}
		if shape.has(.pointer) {
			clean = t.trimmed_type_id(t.trim_pointer_type(clean))
			continue
		}
		break
	}
	return t.normalize_type_alias(clean)
}

// runtime_addr supports runtime addr handling for Transformer.
fn (mut t Transformer) runtime_addr(expr flat.NodeId, typ_id u32) flat.NodeId {
	typ_shape_2 := flat.source_type_shape(typ_id)
	if typ_shape_2.has(.pointer) {
		if addr := t.redundant_deref_addr(expr) {
			return addr
		}
		return expr
	}
	if int(expr) >= 0 && int(expr) < t.a.nodes.len {
		node := t.a.nodes[int(expr)]
		if node.kind == .cast_expr && node.children_count > 0 {
			child_id := t.a.child(&node, 0)
			child_type_source_id := t.node_type(child_id)

			if t.normalize_type_alias(typ_id) == t.normalize_type_alias(child_type_source_id) {
				return t.runtime_addr(child_id, child_type_source_id)
			}
		}
	}
	if !t.expr_can_take_address(expr) {
		if aligned := t.aligned_fixed_array_rvalue_address(expr, typ_id) {
			return aligned
		}
		name := t.new_temp('addr')
		t.pending_stmts << t.make_decl_assign_typed(name, expr, typ_id)
		return t.make_prefix(.amp, t.make_ident(name))
	}
	return t.make_prefix(.amp, expr)
}

// aligned_fixed_array_rvalue_address gives an over-aligned temporary durable backing.
// Existing addressable values keep their original storage and pointer identity.
fn (mut t Transformer) aligned_fixed_array_rvalue_address(value flat.NodeId, typ_id u32) ?flat.NodeId {
	if isnil(t.tc) {
		return none
	}
	typ := types.unalias_type(t.tc.parse_canonical_type_cached(typ_id))
	if typ !is types.ArrayFixed || !t.tc.requires_aligned_allocation(typ) {
		return none
	}
	// Reuse escaped-value promotion so the copy has the same allocator and lifetime policy.
	return t.heap_value(value, typ_id)
}

fn (t &Transformer) redundant_deref_addr(expr flat.NodeId) ?flat.NodeId {
	if int(expr) < 0 || int(expr) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(expr)]
	if node.kind != .prefix || node.op != .mul || node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	if int(child_id) < 0 || int(child_id) >= t.a.nodes.len {
		return none
	}
	child := t.a.nodes[int(child_id)]
	if child.kind == .prefix && child.op == .amp {
		return child_id
	}
	return none
}

// transform_enum_shorthand transforms transform enum shorthand data for transform.
fn (mut t Transformer) transform_enum_shorthand(id flat.NodeId, node flat.Node, expected_enum_id u32) flat.NodeId {
	resolved_enum_source_id := t.enum_type_name_for_expected(expected_enum_id, '')
	resolved_enum := flat.source_type_name(resolved_enum_source_id)
	if resolved_enum_source_id == 0 {
		return id
	}
	short_name := node.value.trim_left('.')
	if fields := t.enum_types[resolved_enum] {
		for f in fields {
			if f == short_name {
				return t.a.add_node(flat.Node{
					kind:  .enum_val
					value: '${resolved_enum}.${short_name}'
					typ:   resolved_enum_source_id
				})
			}
		}
	}
	return id
}

// enum_type_name_for_expected supports enum type name for expected handling for Transformer.
fn (t &Transformer) enum_type_name_for_expected(expected_enum_id u32, owner_mod string) u32 {
	if expected_enum_id == 0 { return 0 }
	mut clean_id := expected_enum_id
	if flat.source_type_shape(clean_id).has(.pointer) {
		clean_id = flat.source_type_syntax(clean_id).raw_base
	}
	clean := flat.source_type_name(clean_id)
	if clean in t.enum_types { return clean_id }
	if flat.source_type_shape(clean_id).has(.dotted) { return 0 }
	for mod_name in [owner_mod, t.cur_module] {
		if mod_name.len == 0 || mod_name == 'main' || mod_name == 'builtin' { continue }
		qname := '${mod_name}.${clean}'
		if qname in t.enum_types { return flat.source_type_id(qname) }
	}
	mut cache_key := u64(0)
	if !isnil(t.enum_expected_cache) {
		mut cache := t.enum_expected_cache
		cache_key = cache.key(clean_id, owner_mod, t.cur_module)
		if cached := cache.entries[cache_key] { return cached }
		if cache.misses[cache_key] { return 0 }
	}
	mut found := u32(0)
	for enum_name, _ in t.enum_types {
		if short_name_view(enum_name) != clean { continue }
		enum_id := flat.source_type_id(enum_name)
		if found != 0 && found != enum_id {
			if !isnil(t.enum_expected_cache) {
				mut cache := t.enum_expected_cache
				cache.misses[cache_key] = true
			}
			return 0
		}
		found = enum_id
	}
	if !isnil(t.enum_expected_cache) {
		mut cache := t.enum_expected_cache
		if found != 0 {
			cache.entries[cache_key] = found
		} else {
			cache.misses[cache_key] = true
		}
	}
	return found
}

// make_call builds make call data for transform.
pub fn (mut t Transformer) make_call(fn_name string, args []flat.NodeId) flat.NodeId {
	return t.make_call_typed(fn_name, args, u32(0))
}

// make_call_typed builds make call typed data for transform.
pub fn (mut t Transformer) make_call_typed(fn_name string, args []flat.NodeId, typ_id u32) flat.NodeId {
	fn_ident := t.make_ident(fn_name)
	call_id := t.make_call_expr_typed(fn_ident, args, typ_id)
	// `__v3_` calls are C-generator intrinsics, not declarations in the
	// semantic function table. Keep them unresolved so Cgen recognizes them.
	if !fn_name.starts_with('__v3_') {
		t.set_generated_resolved_call(call_id, fn_name)
	}
	if fn_name.starts_with('v3_') {
		t.mark_prelude_helper_callees(fn_name)
	}
	return call_id
}

// mark_prelude_helper_callees keeps the V functions that a C helper of the cgen
// prelude (`v3_*`, written by `vlib/v/gen/c/cleanc.v`) calls. Markused keeps them
// for the expressions that it can type, but a generic specialization is lowered
// after markused, so a helper call created there has to keep them itself.
fn (mut t Transformer) mark_prelude_helper_callees(helper string) {
	match helper {
		'v3_f64_fixed', 'v3_f64_trimmed' {
			t.mark_fn_used('f64.str')
		}
		'v3_map_str', 'v3_map_str_piece' {
			// The map formatter has a branch for every scalar kind, whatever the
			// map's own key and value types are.
			t.mark_fn_used('i64.str')
			t.mark_fn_used('u64.str')
			t.mark_fn_used('f64.str')
			t.mark_fn_used('rune.str')
		}
		'v3_char_string' {
			t.mark_fn_used('rune.str')
		}
		'v3_int_zpad' {
			t.mark_fn_used('int.str')
		}
		'v3_i64_zpad' {
			t.mark_fn_used('i64.str')
		}
		'v3_u64_zpad' {
			t.mark_fn_used('u64.str')
		}
		else {}
	}
}

fn (mut t Transformer) mark_fn_used(fn_name string) {
	if fn_name == '' || !t.has_any_used_fns() {
		return
	}
	t.mark_used_fn_key(fn_name)
	t.mark_used_fn_key(c_name(fn_name))
}

// make_call_expr_typed builds make call expr typed data for transform.
pub fn (mut t Transformer) make_call_expr_typed(fn_expr flat.NodeId, args []flat.NodeId, typ_id u32) flat.NodeId {
	start := t.a.children.len
	t.a.children << fn_expr
	for arg in args {
		t.a.children << arg
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		children_start: start
		children_count: flat.child_count(1 + args.len)
		typ:            typ_id
	})
}

// make_empty builds make empty data for transform.
pub fn (mut t Transformer) make_empty() flat.NodeId {
	return t.a.add(.empty)
}

// make_method_call builds make method call data for transform.
pub fn (mut t Transformer) make_method_call(receiver flat.NodeId, method_name string, args []flat.NodeId) flat.NodeId {
	// Build selector: receiver.method_name
	sel_start := t.a.children.len
	t.a.children << receiver
	selector := t.a.add_node(flat.Node{
		kind:           .selector
		value:          method_name
		children_start: sel_start
		children_count: 1
	})

	start := t.a.children.len
	t.a.children << selector
	for arg in args {
		t.a.children << arg
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		children_start: start
		children_count: flat.child_count(1 + args.len)
	})
}

// make_selector builds make selector data for transform.
pub fn (mut t Transformer) make_selector(base flat.NodeId, field string, typ_id u32) flat.NodeId {
	return t.make_selector_op(base, field, typ_id, .dot)
}

// make_selector_op builds make selector op data for transform.
pub fn (mut t Transformer) make_selector_op(base flat.NodeId, field string, typ_id u32, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << base
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             op
		children_start: start
		children_count: 1
		value:          field
		typ:            typ_id
	})
}

// make_sum_tag_selector builds an internal selector for a sumtype discriminator.
pub fn (mut t Transformer) make_sum_tag_selector(base flat.NodeId, op flat.Op) flat.NodeId {
	return t.make_selector_op(base, sum_type_tag_selector_field, flat.source_type_int, op)
}

// make_index builds make index data for transform.
pub fn (mut t Transformer) make_index(base flat.NodeId, index flat.NodeId, typ_id u32) flat.NodeId {
	start := t.a.children.len
	t.a.children << base
	t.a.children << index
	return t.a.add_node(flat.Node{
		kind:           .index
		children_start: start
		children_count: 2
		typ:            typ_id
	})
}

// make_cast builds make cast data for transform.
pub fn (mut t Transformer) make_cast(target_type_id u32, expr flat.NodeId, typ_id u32) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		children_start: start
		children_count: 1
		source_value:   target_type_id
		typ:            typ_id
	})
}

// make_postfix builds make postfix data for transform.
pub fn (mut t Transformer) make_postfix(expr flat.NodeId, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .postfix
		op:             op
		children_start: start
		children_count: 1
	})
}

// make_struct_init builds make struct init data for transform.
pub fn (mut t Transformer) make_struct_init(name_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:         .struct_init
		source_value: name_id
		typ:          name_id
	})
}

// make_array_init builds make array init data for transform.
pub fn (mut t Transformer) make_array_init(elem_type_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:         .array_init
		source_value: elem_type_id
		typ:          flat.source_type_composition_one(.array, elem_type_id)
	})
}

fn (mut t Transformer) make_fixed_array_init(fixed_type_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:         .array_init
		source_value: fixed_type_id
		typ:          fixed_type_id
	})
}

// make_map_init builds make map init data for transform.
pub fn (mut t Transformer) make_map_init(map_type_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:         .map_init
		source_value: map_type_id
		typ:          map_type_id
	})
}

// make_string_literal creates literal text without interpreting interpolation syntax.
pub fn (mut t Transformer) make_string_literal(value string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .string_literal
		value: value
		flags: if value.contains(r'${') { flat.node_flag_literal_interpolation_text } else { 0 }
	})
}

// make_int_literal builds make int literal data for transform.
pub fn (mut t Transformer) make_int_literal(value int) flat.NodeId {
	return t.a.add_val(.int_literal, '${value}')
}

// make_int_literal_typed creates an integer literal node with the given source type ID.
pub fn (mut t Transformer) make_int_literal_typed(value string, typ_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .int_literal
		value: value
		typ:   typ_id
	})
}

// make_float_literal builds make float literal data for transform.
pub fn (mut t Transformer) make_float_literal(value string) flat.NodeId {
	return t.a.add_val(.float_literal, value)
}

// make_float_literal_typed creates a floating-point literal node with the given source type ID.
pub fn (mut t Transformer) make_float_literal_typed(value string, typ_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .float_literal
		value: value
		typ:   typ_id
	})
}

// make_bool_literal builds make bool literal data for transform.
pub fn (mut t Transformer) make_bool_literal(value bool) flat.NodeId {
	return t.a.add_val(.bool_literal, if value { 'true' } else { 'false' })
}

// make_sizeof_type builds make sizeof type data for transform.
pub fn (mut t Transformer) make_sizeof_type(type_name_id u32) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:         .sizeof_expr
		source_value: type_name_id
		typ:          flat.source_type_uint
	})
}

// is_fixed_array_type reports whether a v-type string denotes a fixed array
// like `int[5]` (as opposed to a dynamic array `[]int` or a map `map[...]...`).
@[inline]
fn (t &Transformer) is_fixed_array_type(s_id u32) bool {
	query := flat.source_type_query(s_id)
	if query.has(.fixed_array_prefix) { return true }
	if !query.has(.fixed_array_postfix) { return false }
	return t.is_postfix_fixed_array_type(s_id)
}

fn (t &Transformer) is_postfix_fixed_array_type(type_id u32) bool {
	if !flat.source_type_shape(type_id).has(.bracketed) {
		return false
	}
	syntax := flat.source_type_syntax(type_id)
	length_id := if syntax.postfix_dims.len > 0 {
		syntax.postfix_dims[0]
	} else {
		syntax.first_bracket_length
	}
	length_syntax := flat.source_type_syntax(length_id)
	if length_syntax.decimal {
		// generic_app_parts rejects decimal bracket contents as fixed-array
		// lengths too, so avoid resolving the generic base before this result.
		return true
	}
	if !isnil(t.tc) && syntax.generic_app_valid
		&& t.type_name_is_known_generic_app_base(syntax.generic_app_base) {
		return false
	}
	// A postfix fixed-array name (`ArrayFixed.name()`) can carry a non-decimal length — a const
	// (`int[seg_count]`) or an expression (`int[segs + 1]`) — once the checker round-trips
	// `[n]int` to `int[n]`. Accept it when the bracket text resolves to an integer constant,
	// which distinguishes it from a generic instance (`vec.Vec4[f32]`, whose bracket is a type).
	if length_syntax.contains_comma || isnil(t.tc) {
		return false
	}
	len_text := flat.source_type_name(length_id)
	if t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) != none {
		return true
	}
	return t.const_type_key(len_text) != none
}

fn (t &Transformer) type_name_is_known_generic_app_base(base_id u32) bool {
	clean_id := transform_trimmed_type_id(base_id)
	if clean_id == 0 || isnil(t.tc) {
		return false
	}
	clean := flat.source_type_name(clean_id)
	if clean in t.tc.struct_generic_params || clean in t.tc.sum_generic_params {
		return true
	}
	if flat.source_type_shape(clean_id).has(.dotted) {
		short := flat.source_type_name(flat.source_type_syntax(clean_id).unqualified)
		return short in t.tc.struct_generic_params || short in t.tc.sum_generic_params
	}
	if t.cur_module.len > 0 {
		qname := flat.source_type_name(flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), clean_id))
		if qname in t.tc.struct_generic_params || qname in t.tc.sum_generic_params {
			return true
		}
	}
	return false
}

// fixed_array_len supports fixed array len handling for transform.
fn fixed_array_len(type_id u32) int {
	return flat.source_type_syntax(fixed_array_length_id(type_id)).integer_value
}

fn fixed_array_length_id(type_id u32) u32 {
	syntax := flat.source_type_syntax(type_id)
	if !flat.source_type_shape(type_id).has(.leading_bracket) && syntax.postfix_dims.len > 0 {
		return syntax.postfix_dims[0]
	}
	return syntax.first_bracket_length
}

// fixed_array_len_text supports text callers at declaration and fixture boundaries.
fn fixed_array_len_text(s string) string {
	return flat.source_type_name(fixed_array_length_id(flat.source_type_id(s)))
}

// fixed_array_elem_type supports fixed array elem type handling for transform.
fn fixed_array_elem_type(type_id u32) u32 {
	syntax := flat.source_type_syntax(type_id)
	if flat.source_type_shape(type_id).has(.leading_bracket) {
		return if syntax.first_bracket_closed { syntax.first_bracket_base } else { type_id }
	}
	if syntax.postfix_dims.len == 0 {
		return syntax.generic_base
	}
	mut out := syntax.postfix_base
	for i := syntax.postfix_dims.len - 1; i > 0; i-- {
		out = flat.source_type_composition_two(.fixed_array_suffix, syntax.postfix_dims[i], out)
	}
	return out
}

fn fixed_array_elem_type_bytes(s string) string {
	return flat.source_type_name(fixed_array_elem_type(flat.source_type_id(s)))
}

fn fixed_array_canonical_type(type_id u32) u32 {
	shape := flat.source_type_shape(type_id)
	if !shape.has(.leading_bracket) || shape.has(.array_type) {
		return type_id
	}
	elem_type := fixed_array_canonical_type(fixed_array_elem_type(type_id))
	length_id := fixed_array_length_id(type_id)
	// Postfix dimensions can bind inside composite elements (for example, to a
	// map's value type in `map[string]int[2]`). Keep prefix syntax when postfix
	// notation would change which type the dimension belongs to.
	kind := if fixed_array_elem_requires_prefix_syntax(elem_type) {
		flat.SourceTypeComposition.fixed_array
	} else {
		flat.SourceTypeComposition.fixed_array_suffix
	}
	return flat.source_type_composition_two(kind, length_id, elem_type)
}

fn fixed_array_elem_requires_prefix_syntax(elem_type u32) bool {
	shape := flat.source_type_shape(elem_type)
	return shape.has(.array_type) || shape.has(.map_type) || shape.has(.leading_bracket)
		|| shape.has(.option_type) || shape.has(.result_type) || shape.has(.pointer)
		|| shape.has(.channel_type) || shape.has(.shared_type) || flat.source_type_syntax(elem_type).function_space_prefix
}

fn (t &Transformer) resolved_fixed_array_canonical_type(type_id u32) u32 {
	clean_id := fixed_array_canonical_type(type_id)
	if !t.is_fixed_array_type(clean_id) {
		return clean_id
	}
	elem_type := fixed_array_elem_type(clean_id)
	resolved_elem_id := if t.is_fixed_array_type(elem_type) {
		t.resolved_fixed_array_canonical_type(elem_type)
	} else {
		elem_type
	}
	length_id := fixed_array_length_id(clean_id)
	prefix := fixed_array_elem_requires_prefix_syntax(resolved_elem_id)
	kind := if prefix {
		flat.SourceTypeComposition.fixed_array
	} else {
		flat.SourceTypeComposition.fixed_array_suffix
	}
	if !flat.source_type_syntax(length_id).decimal && !isnil(t.tc) {
		if v := t.tc.const_int_value_in_module(flat.source_type_name(length_id), t.cur_module, []string{}) {
			return flat.source_type_fixed_array(v, '', resolved_elem_id, !prefix)
		}
	}
	return flat.source_type_composition_two(kind, length_id, resolved_elem_id)
}

// is_decimal_text reports whether is decimal text applies in transform.
fn is_decimal_text(s string) bool {
	if s == '' {
		return false
	}
	for ch in s {
		if ch < `0` || ch > `9` {
			return false
		}
	}
	return true
}

// make_fixed_array_len_expr builds make fixed array len expr data for transform.
fn (mut t Transformer) make_fixed_array_len_expr(s_id u32) flat.NodeId {
	length_id := fixed_array_length_id(s_id)
	length_syntax := flat.source_type_syntax(length_id)
	if length_syntax.decimal {
		return t.make_int_literal(length_syntax.decimal_value)
	}
	len_text := flat.source_type_name(length_id)
	// Fold a const length — a bare const (`SEGS`) or an expression (`segs + 1`) — to
	// its integer value. A fixed array's `.len` is a compile-time constant; emitting
	// the raw expression as an ident would c_name-mangle it into an undeclared
	// identifier such as `segs_+_1`.
	if !isnil(t.tc) {
		if v := t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) {
			return t.make_int_literal(v)
		}
	}
	if flat.source_type_shape(length_id).has(.dotted) {
		base := flat.source_type_name(flat.source_type_syntax(length_id).qualifier)
		field := flat.source_type_name(flat.source_type_syntax(length_id).unqualified)
		return t.make_selector(t.make_ident(base), field, flat.source_type_int)
	}
	return t.make_ident(len_text)
}

// c_name returns the C identifier used for a V symbol or type name.
fn c_name(name string) string {
	return naming.c_name(name)
}

// lookup_c_name is c_name for a spelling that is only compared or used as a
// lookup key: the text may belong to the checker's memo of c_name.
@[inline]
fn (t &Transformer) lookup_c_name(name string) string {
	if isnil(t.tc) {
		return naming.c_name(name)
	}
	return t.tc.cached_c_name(name)
}
