module transform

import v3.flat
import v3.gen.c.naming
import v3.types

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
	lhs_raw_type := t.node_type(lhs_id)
	rhs_raw_type := t.node_type(rhs_id)
	lhs_clean_type := t.normalize_type_alias(lhs_raw_type)
	rhs_clean_type := t.normalize_type_alias(rhs_raw_type)
	if _ := t.operator_alias_type_for_operand(lhs_id, node.op) {
		return none
	}
	lhs_is_string_ptr := t.equality_expr_is_string_pointer(lhs_id, lhs_clean_type)
	rhs_is_string_ptr := t.equality_expr_is_string_pointer(rhs_id, rhs_clean_type)
	if node.op in [.plus, .eq, .ne] && (lhs_is_string_ptr || rhs_is_string_ptr) {
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

	mut result := flat.empty_node
	result_type := if node.op == .plus { 'string' } else { 'bool' }
	match node.op {
		.plus {
			lhs := t.a.nodes[int(new_lhs)]
			rhs := t.a.nodes[int(new_rhs)]
			if lhs.kind == .string_literal && rhs.kind == .string_literal {
				result = t.make_string_literal(lhs.value + rhs.value)
			} else {
				result = t.make_call('string__plus', arr2(new_lhs, new_rhs))
			}
		}
		.eq {
			result = t.make_call('string__eq', arr2(new_lhs, new_rhs))
		}
		.ne {
			eq_call := t.make_call('string__eq', arr2(new_lhs, new_rhs))
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
			result = t.make_call('string__lt', arr2(new_lhs, new_rhs))
		}
		.gt {
			// a > b  ->  string__lt(b, a)
			result = t.make_call('string__lt', arr2(new_rhs, new_lhs))
		}
		.le {
			// a <= b  ->  !(b < a)  ->  !string__lt(rhs, lhs)
			lt_call := t.make_call('string__lt', arr2(new_rhs, new_lhs))
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
			lt_call := t.make_call('string__lt', arr2(new_lhs, new_rhs))
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
		tmp_name := t.new_temp(if result_type == 'string' { 'str_expr' } else { 'str_cmp' })
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, result, result_type)
		return t.make_ident(tmp_name)
	}
	return result
}

// transform_infix_array_ops transforms transform infix array ops data for transform.
fn (mut t Transformer) transform_infix_array_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 || node.op !in [.eq, .ne] {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_raw_type := t.node_type(lhs_id)
	rhs_raw_type := t.node_type(rhs_id)
	lhs_is_array_ptr := t.equality_type_is_array_pointer(lhs_raw_type)
	rhs_is_array_ptr := t.equality_type_is_array_pointer(rhs_raw_type)
	if lhs_is_array_ptr && rhs_is_array_ptr {
		return none
	}
	if _ := t.operator_alias_type_for_operand(lhs_id, node.op) {
		return none
	}
	mut lhs_type := t.membership_container_type(lhs_raw_type)
	mut rhs_type := t.membership_container_type(rhs_raw_type)
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
		if lhs_is_fixed && rhs_type.starts_with('[]') {
			fixed_type := t.resolved_fixed_array_canonical_type(lhs_type)
			if fixed_array_elem_type(fixed_type) == rhs_type[2..] {
				fixed_lhs_as_array = t.fixed_array_value_to_array(lhs_id, fixed_type, rhs_type)
				lhs_type = rhs_type
				lhs_is_fixed = false
			} else {
				return none
			}
		} else if rhs_is_fixed && lhs_type.starts_with('[]') {
			fixed_type := t.resolved_fixed_array_canonical_type(rhs_type)
			if fixed_array_elem_type(fixed_type) == lhs_type[2..] {
				fixed_rhs_as_array = t.fixed_array_value_to_array(rhs_id, fixed_type, lhs_type)
				rhs_type = lhs_type
				rhs_is_fixed = false
			} else {
				return none
			}
		} else {
			return none
		}
	}
	if !(lhs_type.starts_with('[]') || lhs_type == 'array') || !(rhs_type.starts_with('[]')
		|| rhs_type == 'array') {
		return none
	}
	mut elem_type := ''
	if lhs_type.starts_with('[]') {
		elem_type = lhs_type[2..]
	} else if rhs_type.starts_with('[]') {
		elem_type = rhs_type[2..]
	}
	if elem_type.len == 0 {
		elem_type = 'int'
	}
	lhs_target_type := t.contextual_array_comparison_type(lhs_id, lhs_type, rhs_type)
	rhs_target_type := t.contextual_array_comparison_type(rhs_id, rhs_type, lhs_type)
	mut new_lhs := if int(fixed_lhs_as_array) >= 0 {
		fixed_lhs_as_array
	} else if lhs_target_type.starts_with('[]') {
		t.transform_expr_for_type(lhs_id, lhs_target_type)
	} else {
		t.transform_expr(lhs_id)
	}
	mut new_rhs := if int(fixed_rhs_as_array) >= 0 {
		fixed_rhs_as_array
	} else if rhs_target_type.starts_with('[]') {
		t.transform_expr_for_type(rhs_id, rhs_target_type)
	} else {
		t.transform_expr(rhs_id)
	}
	new_lhs_type := t.membership_container_type(t.node_type(new_lhs))
	new_rhs_type := t.membership_container_type(t.node_type(new_rhs))
	if new_lhs_type.starts_with('[]') {
		elem_type = new_lhs_type[2..]
		lhs_type = new_lhs_type
	} else if new_rhs_type.starts_with('[]') {
		elem_type = new_rhs_type[2..]
		rhs_type = new_rhs_type
	}
	if lhs_is_array_ptr && t.node_type(new_lhs).starts_with('&') {
		new_lhs = t.make_prefix(.mul, new_lhs)
	}
	if rhs_is_array_ptr && t.node_type(new_rhs).starts_with('&') {
		new_rhs = t.make_prefix(.mul, new_rhs)
	}
	eq_call := if t.array_elem_needs_element_eq(elem_type) {
		t.make_array_elementwise_eq_call(new_lhs, new_rhs, elem_type, lhs_type, rhs_type, node)
	} else if elem_type.starts_with('[]') {
		t.make_call_typed('array_eq_array', arr3(new_lhs, new_rhs,
			t.make_int_literal(array_nested_eq_depth(lhs_type))), 'bool')
	} else if elem_type == 'string' {
		t.make_call_typed('array_eq_string', arr2(new_lhs, new_rhs), 'bool')
	} else {
		t.make_call_typed('array_eq_raw', arr3(new_lhs, new_rhs, t.make_sizeof_type(elem_type)),
			'bool')
	}
	if node.op == .ne {
		return t.make_prefix(.not, eq_call)
	}
	return eq_call
}

fn (t &Transformer) contextual_array_comparison_type(id flat.NodeId, own_type string, other_type string) string {
	if int(id) < 0 || !own_type.starts_with('[]') || !other_type.starts_with('[]') {
		return own_type
	}
	node := t.a.nodes[int(id)]
	if node.kind != .array_literal {
		return own_type
	}
	own_elem := own_type[2..]
	other_elem := other_type[2..]
	other_base, _, other_is_generic := generic_app_parts(other_elem)
	if !other_is_generic {
		return own_type
	}
	if own_elem.all_after_last('.') == other_base.all_after_last('.') {
		return other_type
	}
	return own_type
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
	lhs_type := t.map_comparison_expr_type(lhs_id)
	rhs_type := t.map_comparison_expr_type(rhs_id)
	if t.equality_type_is_map_pointer(lhs_type) || t.equality_type_is_map_pointer(rhs_type) {
		return none
	}
	mut lhs_map_type := t.clean_map_type(lhs_type)
	mut rhs_map_type := t.clean_map_type(rhs_type)
	if !lhs_map_type.starts_with('map[') && rhs_map_type.starts_with('map[')
		&& t.is_empty_map_init(lhs_id) {
		lhs_map_type = rhs_map_type
	}
	if !rhs_map_type.starts_with('map[') && lhs_map_type.starts_with('map[')
		&& t.is_empty_map_init(rhs_id) {
		rhs_map_type = lhs_map_type
	}
	if !lhs_map_type.starts_with('map[') || !rhs_map_type.starts_with('map[') {
		return none
	}
	map_type := lhs_map_type
	mut new_lhs := t.transform_expr_for_type(lhs_id, map_type)
	mut new_rhs := t.transform_expr_for_type(rhs_id, map_type)
	if lhs_type.starts_with('&') {
		new_lhs = t.make_prefix(.mul, new_lhs)
	}
	if rhs_type.starts_with('&') {
		new_rhs = t.make_prefix(.mul, new_rhs)
	}
	_, value_type := t.map_type_parts(map_type)
	eq_call := if value_type.len > 0 && t.map_value_needs_element_eq(value_type) {
		t.make_map_elementwise_eq_call(new_lhs, new_rhs, map_type, node)
	} else {
		t.make_call_typed('v3_map_map_eq', arr2(new_lhs, new_rhs), 'bool')
	}
	if node.op == .ne {
		return t.make_prefix(.not, eq_call)
	}
	return eq_call
}

fn (mut t Transformer) map_comparison_expr_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		concrete := t.concrete_generic_call_return_type(id, node)
		if concrete.len > 0 && t.clean_map_type(concrete).starts_with('map[') {
			return concrete
		}
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 && t.clean_map_type(name).starts_with('map[') {
				return name
			}
		}
	}
	mut typ := t.node_type(id)
	if typ.len == 0 && node.kind == .map_init {
		typ = if node.value.len > 0 { node.value } else { node.typ }
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
	if lhs_type.len == 0 {
		lhs_type = t.checker_node_type(lhs_id)
	}
	if rhs_type.len == 0 {
		rhs_type = t.checker_node_type(rhs_id)
	}
	lhs_iface := t.resolve_interface_type_name(lhs_type)
	rhs_iface := t.resolve_interface_type_name(rhs_type)
	if lhs_iface.len == 0 && rhs_iface.len == 0 {
		return none
	}
	iface := if lhs_iface.len > 0 { lhs_iface } else { rhs_iface }
	lhs := t.transform_expr_for_type(lhs_id, iface)
	rhs := t.transform_expr_for_type(rhs_id, iface)
	concrete_type := t.interface_box_concrete_type(lhs) or {
		t.interface_box_concrete_type(rhs) or {
			// The boxed concrete type is unknown at compile time. For IError
			// specifically, compare concrete type and observable identity (msg +
			// code) via the always-emitted C helpers, matching how error values compare in
			// practice (`assert err == other_err`). The helpers take addresses,
			// so a transformed non-lvalue operand is spilled to a temporary first.
			if iface in ['IError', 'builtin.IError'] {
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
				lhs_typ := t.make_selector(lhs_err, '_typ', 'int')
				rhs_typ := t.make_selector(rhs_err, '_typ', 'int')
				type_eq := t.make_infix(.eq, lhs_typ, rhs_typ)
				lhs_msg := t.make_call_typed('IError__msg', arr1(lhs_addr), 'string')
				rhs_msg := t.make_call_typed('IError__msg', arr1(rhs_addr), 'string')
				msg_eq := t.make_call_typed('string__eq', arr2(lhs_msg, rhs_msg), 'bool')
				lhs_code := t.make_call_typed('IError__code', arr1(lhs_addr), 'int')
				rhs_code := t.make_call_typed('IError__code', arr1(rhs_addr), 'int')
				code_eq := t.make_infix(.eq, lhs_code, rhs_code)
				err_eq := t.make_infix(.logical_and, type_eq, t.make_infix(.logical_and, msg_eq,
					code_eq))
				if node.op == .ne {
					return t.make_prefix(.not, err_eq)
				}
				return err_eq
			}
			eq := t.make_interface_semantic_eq_expr(lhs, rhs, iface, []string{})
			if node.op == .ne {
				return t.make_prefix(.not, eq)
			}
			return eq
		}
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, iface, 'iface_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, iface, 'iface_eq_rhs')
	lhs_typ := t.make_selector(lhs_value, '_typ', 'int')
	rhs_typ := t.make_selector(rhs_value, '_typ', 'int')
	type_eq := t.make_infix(.eq, lhs_typ, rhs_typ)
	lhs_object := t.make_cast('&${concrete_type}',
		t.make_selector(lhs_value, '_object', 'voidptr'), '&${concrete_type}')
	rhs_object := t.make_cast('&${concrete_type}',
		t.make_selector(rhs_value, '_object', 'voidptr'), '&${concrete_type}')
	lhs_concrete := t.make_prefix(.mul, lhs_object)
	t.set_node_typ(int(lhs_concrete), concrete_type)
	rhs_concrete := t.make_prefix(.mul, rhs_object)
	t.set_node_typ(int(rhs_concrete), concrete_type)
	value_eq := t.make_membership_eq_expr(lhs_concrete, rhs_concrete, concrete_type)
	eq := t.make_infix(.logical_and, type_eq, value_eq)
	if node.op == .ne {
		return t.make_prefix(.not, eq)
	}
	return eq
}

fn (t &Transformer) interface_box_concrete_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if concrete := t.interface_var_concrete_types[node.value] {
			return concrete
		}
	}
	if node.kind != .struct_init || !t.is_interface_type(node.value) {
		return none
	}
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.value != '_object' || field.children_count == 0 {
			continue
		}
		typ := t.node_type(t.a.child(field, 0))
		if typ.starts_with('&') && typ.len > 1 {
			return typ[1..]
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
	checker_lhs_type := t.checker_node_type(lhs_id)
	lhs_key := t.expr_key(lhs_id)
	mut has_smartcast := false
	if lhs_key.len > 0 {
		if sc := t.find_smartcast(lhs_key) {
			has_smartcast = true
			lhs_type = t.smartcast_target_type(sc)
		}
	}
	if lhs_type.len == 0 {
		lhs_type = checker_lhs_type
	}
	lhs_node := t.a.nodes[int(lhs_id)]
	lhs_is_mut_value_param := lhs_node.kind == .ident && t.mut_param_values[lhs_node.value]
	lhs_is_pointer := lhs_type.starts_with('&') || checker_lhs_type.starts_with('&')
		|| lhs_is_mut_value_param
	if lhs_is_pointer {
		if node.op in [.eq, .ne] {
			return none
		}
		if lhs_type.starts_with('&') {
			lhs_type = lhs_type[1..]
		}
	}
	mut struct_type := t.struct_lookup_name(lhs_type)
	if struct_type.len == 0 {
		// Generic-struct instance operand (e.g. `Vec4[f32]`): keep the instance type
		// so the operator lowers to the monomorphized method (`vec__Vec4_f32__plus`).
		struct_type = t.generic_struct_instance_name(lhs_type)
	}
	mut is_alias_operator := false
	if struct_type.len == 0 {
		if alias_type := t.operator_alias_type_for_operand(lhs_id, node.op) {
			struct_type = alias_type
			is_alias_operator = true
		}
	}
	if struct_type.len == 0 {
		return none
	}
	// Skip the checker/transformer agreement guard for generic-struct instances:
	// they resolve reliably, and an alias name (`SimdFloat4`) vs the resolved form
	// (`vec.Vec4[f32]`) would otherwise spuriously fail the comparison.
	if checker_lhs_type.len > 0 && !has_smartcast && !lhs_is_pointer && !struct_type.contains('[')
		&& !is_alias_operator {
		checker_struct_type := t.struct_lookup_name(checker_lhs_type)
		if checker_struct_type.len > 0 && checker_struct_type != struct_type {
			return none
		}
		if checker_struct_type.len == 0 && checker_lhs_type != lhs_type {
			return none
		}
	}
	if call_info := t.struct_operator_call_info(struct_type, node.op) {
		if t.is_disabled_fn_name(call_info.name) {
			ret_type := t.struct_operator_return_type(call_info.name)
			if ret_type.len == 0 || ret_type == 'void' {
				return t.make_empty()
			}
			return t.zero_value_for_type(ret_type)
		}
		mut lhs := t.transform_expr(lhs_id)
		if lhs_is_pointer {
			lhs = t.make_prefix(.mul, lhs)
			t.set_node_typ(int(lhs), lhs_type)
		}
		rhs := t.transform_expr(t.a.children[node.children_start + 1])
		mut call_lhs := lhs
		mut call_rhs := rhs
		if call_info.reverse {
			call_lhs = t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'op_lhs')
			call_rhs = t.stable_transformed_expr_for_reuse(rhs, t.node_type(t.a.children[
				node.children_start + 1]), 'op_rhs')
		}
		args := if call_info.reverse {
			arr2(call_rhs, call_lhs)
		} else {
			arr2(call_lhs, call_rhs)
		}
		t.mark_fn_used_name(call_info.name)
		call := t.make_call_typed(call_info.name, args, node.typ)
		if call_info.negate {
			return t.make_prefix(.not, call)
		}
		return call
	}
	if node.op != .eq && node.op != .ne {
		return none
	}
	if !t.has_struct_operator_fn(struct_type, '==') {
		lhs := t.stable_expr_for_reuse(lhs_id)
		rhs_expr := t.transform_expr_for_type(rhs_id, lhs_type)
		rhs := t.stable_transformed_expr_for_reuse(rhs_expr, lhs_type, 'eq_rhs')
		if field_eq := t.make_struct_field_eq_expr(lhs, rhs, struct_type) {
			if node.op == .ne {
				return t.make_prefix(.not, field_eq)
			}
			return field_eq
		}
		cmp := t.make_call_typed('C.memcmp', arr3(t.make_prefix(.amp, lhs),
			t.make_prefix(.amp, rhs), t.make_sizeof_type(struct_type)), 'int')
		return t.make_infix(node.op, cmp, t.make_int_literal(0))
	}
	if eq_fn := t.struct_operator_fn_name(struct_type, '==') {
		if t.is_disabled_fn_name(eq_fn) {
			return t.make_bool_literal(node.op == .ne)
		}
		lhs := t.transform_expr(lhs_id)
		rhs := t.transform_expr(t.a.children[node.children_start + 1])
		t.mark_fn_used_name(eq_fn)
		eq_call := t.make_call_typed(eq_fn, arr2(lhs, rhs), 'bool')
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
	lhs_is_ptr := t.infix_operand_is_pointer(lhs_id)
	rhs_is_ptr := t.infix_operand_is_pointer(rhs_id)
	if !lhs_is_ptr && !rhs_is_ptr {
		return none
	}
	lhs_type := t.node_type(lhs_id)
	rhs_type := t.node_type(rhs_id)
	lhs_clean := if lhs_type.starts_with('&') { lhs_type[1..] } else { lhs_type }
	rhs_clean := if rhs_type.starts_with('&') { rhs_type[1..] } else { rhs_type }
	lhs_struct := t.struct_lookup_name(lhs_clean)
	rhs_struct := t.struct_lookup_name(rhs_clean)
	if lhs_struct.len == 0 || rhs_struct.len == 0 {
		return none
	}
	if lhs_struct != rhs_struct && lhs_struct.all_after_last('.') != rhs_struct.all_after_last('.') {
		return none
	}
	if lhs_is_ptr && rhs_is_ptr {
		return t.transform_struct_pointer_eq(node, lhs_id, rhs_id, lhs_type, rhs_type, lhs_clean,
			rhs_clean)
	}
	lhs := if lhs_is_ptr {
		t.make_prefix(.mul, t.transform_expr(lhs_id))
	} else {
		t.transform_expr(lhs_id)
	}
	rhs := if rhs_is_ptr {
		t.make_prefix(.mul, t.transform_expr(rhs_id))
	} else {
		t.transform_expr(rhs_id)
	}
	if eq := t.transform_transformed_struct_eq(node, lhs, rhs) {
		return eq
	}
	return none
}

fn (mut t Transformer) transform_struct_pointer_eq(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type string, rhs_type string, lhs_clean string, rhs_clean string) ?flat.NodeId {
	pending_base := t.pending_stmts.len
	lhs_ptr := t.stable_transformed_expr_for_reuse(t.transform_expr(lhs_id), lhs_type, 'ptr_eq_lhs')
	rhs_ptr := t.stable_transformed_expr_for_reuse(t.transform_expr(rhs_id), rhs_type, 'ptr_eq_rhs')
	result_name := t.new_temp('ptr_eq')
	same_ptr := t.make_infix(.eq, lhs_ptr, rhs_ptr)
	t.pending_stmts << t.make_decl_assign_typed(result_name, same_ptr, 'bool')
	lhs_not_nil := t.make_infix(.ne, lhs_ptr, t.a.add(.nil_literal))
	rhs_not_nil := t.make_infix(.ne, rhs_ptr, t.a.add(.nil_literal))
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
		typ:  'bool'
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
	t.set_node_typ(int(result), 'bool')
	if node.op == .ne {
		not_result := t.make_prefix(.not, result)
		t.set_node_typ(int(not_result), 'bool')
		return not_result
	}
	return result
}

fn (mut t Transformer) transform_transformed_struct_eq(node flat.Node, lhs flat.NodeId, rhs flat.NodeId) ?flat.NodeId {
	if node.op in [.eq, .ne] && (t.infix_operand_is_pointer(lhs) || t.infix_operand_is_pointer(rhs)) {
		return none
	}
	mut lhs_type := t.node_type(lhs)
	if lhs_type.starts_with('&') {
		lhs_type = lhs_type[1..]
	}
	mut rhs_type := t.node_type(rhs)
	if rhs_type.starts_with('&') {
		rhs_type = rhs_type[1..]
	}
	lhs_struct_type := t.struct_lookup_name(lhs_type)
	rhs_struct_type := t.struct_lookup_name(rhs_type)
	if lhs_struct_type.len == 0 || rhs_struct_type.len == 0 {
		return none
	}
	struct_type := lhs_struct_type
	if lhs_struct_type != rhs_struct_type
		&& lhs_struct_type.all_after_last('.') != rhs_struct_type.all_after_last('.') {
		return none
	}
	if call_info := t.struct_operator_call_info(struct_type, node.op) {
		if t.is_disabled_fn_name(call_info.name) {
			ret_type := t.struct_operator_return_type(call_info.name)
			if ret_type.len == 0 || ret_type == 'void' {
				return t.make_empty()
			}
			return t.zero_value_for_type(ret_type)
		}
		call_lhs := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'op_lhs')
		call_rhs := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'op_rhs')
		args := if call_info.reverse {
			arr2(call_rhs, call_lhs)
		} else {
			arr2(call_lhs, call_rhs)
		}
		t.mark_fn_used_name(call_info.name)
		call := t.make_call_typed(call_info.name, args, node.typ)
		if call_info.negate {
			return t.make_prefix(.not, call)
		}
		return call
	}
	if node.op != .eq && node.op != .ne {
		return none
	}
	if eq_fn := t.struct_operator_fn_name(struct_type, '==') {
		if t.is_disabled_fn_name(eq_fn) {
			return t.make_bool_literal(node.op == .ne)
		}
		call_lhs := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'eq_lhs')
		call_rhs := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'eq_rhs')
		t.mark_fn_used_name(eq_fn)
		eq_call := t.make_call_typed(eq_fn, arr2(call_lhs, call_rhs), 'bool')
		if node.op == .ne {
			return t.make_prefix(.not, eq_call)
		}
		return eq_call
	}
	cmp_lhs := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'eq_lhs')
	cmp_rhs := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'eq_rhs')
	if field_eq := t.make_struct_field_eq_expr(cmp_lhs, cmp_rhs, struct_type) {
		if node.op == .ne {
			return t.make_prefix(.not, field_eq)
		}
		return field_eq
	}
	cmp := t.make_call_typed('C.memcmp', arr3(t.make_prefix(.amp, cmp_lhs), t.make_prefix(.amp,
		cmp_rhs), t.make_sizeof_type(struct_type)), 'int')
	return t.make_infix(node.op, cmp, t.make_int_literal(0))
}

fn (t &Transformer) infix_operand_is_pointer(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .nil_literal || node.typ.starts_with('&') {
		return true
	}
	if node.kind == .ident && t.var_type(node.value).starts_with('&') {
		return true
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			if typ is types.Pointer {
				return true
			}
		}
		if t.tc.resolve_type(id) is types.Pointer {
			return true
		}
	}
	return false
}

fn (t &Transformer) checker_node_type(id flat.NodeId) string {
	if isnil(t.tc) || int(id) < 0 {
		return ''
	}
	name := t.raw_checker_node_type(id)
	if name.len == 0 || name == 'void' {
		return ''
	}
	return t.normalize_type_alias(name)
}

fn (t &Transformer) raw_checker_node_type(id flat.NodeId) string {
	if isnil(t.tc) || int(id) < 0 {
		return ''
	}
	typ := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	name := typ.name()
	if name.len == 0 || name == 'void' {
		return ''
	}
	return name
}

fn (t &Transformer) raw_alias_type_for_expr(id flat.NodeId) string {
	raw_type := t.raw_checker_node_type(id)
	if raw_type.len == 0 {
		return ''
	}
	is_ptr := raw_type.starts_with('&')
	clean := t.trim_pointer_type(raw_type)
	if t.is_type_alias_name(clean) {
		return raw_type
	}
	if is_ptr {
		return ''
	}
	return ''
}

fn (t &Transformer) is_type_alias_name(name string) bool {
	if isnil(t.tc) || name.len == 0 {
		return false
	}
	if name in t.tc.type_aliases {
		return true
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		return '${t.cur_module}.${name}' in t.tc.type_aliases
	}
	if !name.contains('.') {
		for aname, _ in t.tc.type_aliases {
			if aname.all_after_last('.') == name {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) operator_alias_type_for_operand(id flat.NodeId, op flat.Op) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	mut candidates := []string{cap: 3}
	raw_type := t.raw_checker_node_type(id)
	if raw_type.len > 0 {
		candidates << raw_type
	}
	if node.typ.len > 0 {
		candidates << node.typ
	}
	for candidate in candidates {
		clean := t.trim_pointer_type(candidate)
		if clean.len == 0 || !t.is_type_alias_name(clean) {
			continue
		}
		if _ := t.struct_operator_call_info(clean, op) {
			return clean
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
fn (t &Transformer) struct_operator_call_info(struct_type string, op flat.Op) ?StructOperatorCallInfo {
	if op_name := struct_operator_symbol(op) {
		if method_name := t.struct_operator_fn_name(struct_type, op_name) {
			return StructOperatorCallInfo{
				name: method_name
			}
		}
	}
	match op {
		.gt {
			if method_name := t.struct_operator_fn_name(struct_type, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
				}
			}
		}
		.ge {
			if method_name := t.struct_operator_fn_name(struct_type, '<') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		.le {
			if method_name := t.struct_operator_fn_name(struct_type, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
					negate:  true
				}
			}
		}
		.ne {
			if method_name := t.struct_operator_fn_name(struct_type, '==') {
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

// struct_operator_symbol supports struct operator symbol handling for transform.
fn struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus { return '+' }
		.minus { return '-' }
		.mul { return '*' }
		.div { return '/' }
		.mod { return '%' }
		.eq { return '==' }
		.ne { return '!=' }
		.lt { return '<' }
		.gt { return '>' }
		.le { return '<=' }
		.ge { return '>=' }
		else {}
	}

	return none
}

// struct_operator_fn_name supports struct operator fn name handling for Transformer.
fn (t &Transformer) struct_operator_fn_name(struct_type string, op_name string) ?string {
	require_used := op_name in ['==', '!=']
	return t.struct_operator_fn_name_with_usage(struct_type, op_name, require_used)
}

fn (t &Transformer) struct_operator_fn_name_any(struct_type string, op_name string) ?string {
	return t.struct_operator_fn_name_with_usage(struct_type, op_name, false)
}

fn (t &Transformer) struct_operator_fn_name_with_usage(struct_type string, op_name string, require_used bool) ?string {
	for receiver in t.operator_receiver_candidates(struct_type) {
		method_name := '${receiver}.${op_name}'
		if t.is_known_operator_fn_name(method_name, require_used) {
			return method_name
		}
		cmethod_name := c_name(method_name)
		if t.is_known_operator_fn_name(cmethod_name, require_used) {
			return cmethod_name
		}
		if name := t.generic_struct_operator_fn_name(receiver, op_name) {
			return name
		}
	}
	return none
}

fn (t &Transformer) operator_receiver_candidates(struct_type string) []string {
	mut candidates := []string{cap: 2}
	if struct_type.len == 0 {
		return candidates
	}
	candidates << struct_type
	if !struct_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${struct_type}'
	}
	return candidates
}

// generic_struct_instance_name returns the resolved generic-struct instance type
// for `type_name` (its base is a known generic struct), else ''. A type alias to a
// generic instance (`SimdFloat4` -> `vec.Vec4[f32]`) is normalized first, so an
// operand typed by its alias still lowers to the monomorphized operator.
fn (t &Transformer) generic_struct_instance_name(type_name string) string {
	if isnil(t.tc) {
		return ''
	}
	normalized := t.normalize_type_alias(type_name)
	base, _, ok := generic_app_parts(normalized)
	if !ok {
		return ''
	}
	if base in t.tc.struct_generic_params {
		return normalized
	}
	return ''
}

// generic_struct_operator_fn_name handles operator overloads on a generic-struct
// instance (e.g. `Vec4[f32] + Vec4[f32]`). The operator is declared on the generic
// form (`Vec4[T].+`) and specialized to `vec__Vec4_f32__plus` by the monomorphizer
// (which runs after this lowering). When the generic operator exists, anticipate
// the specialized C name so the infix lowers to that call.
fn (t &Transformer) generic_struct_operator_fn_name(struct_type string, op_name string) ?string {
	if isnil(t.tc) {
		return none
	}
	base, _, ok := generic_app_parts(struct_type)
	if !ok {
		return none
	}
	params := t.tc.struct_generic_params[base] or { return none }
	if params.len == 0 {
		return none
	}
	generic_key := '${base}[${params.join(', ')}].${op_name}'
	if generic_key in t.tc.fn_ret_types || generic_key in t.tc.fn_param_types {
		return c_name('${struct_type}.${op_name}')
	}
	return none
}

fn (t &Transformer) is_known_operator_fn_name(name string, require_used bool) bool {
	if !t.is_known_fn_name(name) {
		return false
	}
	if !require_used || t.used_fns.len == 0 {
		return true
	}
	return name in t.used_fns || c_name(name) in t.used_fns
}

// has_struct_operator_fn reports whether has struct operator fn applies in transform.
fn (t &Transformer) has_struct_operator_fn(struct_type string, op_name string) bool {
	if _ := t.struct_operator_fn_name(struct_type, op_name) {
		return true
	}
	return false
}

// struct_operator_return_type supports struct operator return type handling for Transformer.
fn (t &Transformer) struct_operator_return_type(fn_name string) string {
	if ret := t.fn_ret_types[fn_name] {
		return t.normalize_type_alias(ret)
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[fn_name] {
			return t.normalize_type_alias(ret.name())
		}
	}
	return ''
}

// infix_struct_operator_result_type
// supports helper handling in transform.
fn (t &Transformer) infix_struct_operator_result_type(node flat.Node, lhs_type_in string) string {
	if node.children_count < 2 {
		return ''
	}
	// `lhs_type_in` is the already-resolved type of the left operand; the caller passes
	// it so we don't re-resolve the operand (operator-overload checks run on every infix).
	lhs_type := if lhs_type_in.starts_with('&') { lhs_type_in[1..] } else { lhs_type_in }
	struct_type := t.struct_lookup_name(lhs_type)
	if struct_type.len == 0 {
		// Generic-struct instance (`Vec4[f32]`): the specialized operator method is
		// not registered until monomorphization, so derive the result from the
		// generic operator's (substituted) return type.
		if rt := t.generic_struct_operator_return_type(t.generic_struct_instance_name(lhs_type),
			node.op)
		{
			return rt
		}
		return ''
	}
	if call_info := t.struct_operator_call_info(struct_type, node.op) {
		ret_type := t.struct_operator_return_type(call_info.name)
		if ret_type.len > 0 {
			return ret_type
		}
	}
	return ''
}

// generic_struct_operator_return_type returns the result type of an operator on a
// generic-struct instance (`Vec4[f32]`), derived from the generic operator's
// declared return type (`Vec4[T].- -> Vec4[T]`) with the instance's type arguments
// substituted (`-> Vec4[f32]`), qualified with the struct's module so the outer
// expression resolves to the monomorphized operator.
fn (t &Transformer) generic_struct_operator_return_type(struct_type string, op flat.Op) ?string {
	if struct_type.len == 0 || isnil(t.tc) {
		return none
	}
	op_name := struct_operator_symbol(op) or { return none }
	full_base, args, ok := generic_app_parts(struct_type)
	if !ok {
		return none
	}
	params := t.tc.struct_generic_params[full_base] or { return none }
	generic_key := '${full_base}[${params.join(', ')}].${op_name}'
	mut ret := ''
	if r := t.fn_ret_types[generic_key] {
		ret = r
	} else if r := t.tc.fn_ret_types[generic_key] {
		ret = r.name()
	} else {
		return none
	}
	substituted := substitute_generic_type_text_with_params(ret, args, params)
	// Qualify a returned instance of the same struct family with the struct's module.
	rbase, _, rok := generic_app_parts(substituted)
	if rok && full_base.contains('.') && !rbase.contains('.')
		&& rbase == full_base.all_after_last('.') {
		return '${full_base.all_before_last('.')}.${substituted}'
	}
	return substituted
}

// transform_infix_sum_ops transforms transform infix sum ops data for transform.
fn (mut t Transformer) transform_infix_sum_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.op !in [.eq, .ne] || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_raw_type := t.node_type(lhs_id)
	rhs_raw_type := t.node_type(rhs_id)
	mut lhs_type := lhs_raw_type
	mut rhs_type := rhs_raw_type
	lhs_is_ptr := lhs_type.starts_with('&')
	rhs_is_ptr := rhs_type.starts_with('&')
	if lhs_is_ptr {
		lhs_type = lhs_type[1..]
	}
	if rhs_is_ptr {
		rhs_type = rhs_type[1..]
	}
	lhs_type = t.normalize_type_alias(lhs_type)
	rhs_type = t.normalize_type_alias(rhs_type)
	if !t.is_sum_type_name(lhs_type) || !t.is_sum_type_name(rhs_type) {
		return none
	}
	sum_type := if t.is_sum_type_name(lhs_type) {
		t.resolve_sum_name(lhs_type)
	} else if t.is_sum_type_name(rhs_type) {
		t.resolve_sum_name(rhs_type)
	} else {
		''
	}
	if sum_type.len == 0 {
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
	eq := t.make_sum_semantic_eq_expr(lhs, rhs, sum_type, []string{})
	if node.op == .ne {
		return t.make_prefix(.not, t.make_paren(eq))
	}
	return eq
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
	if lhs.kind == .none_expr {
		opt_id = rhs_id
	} else if rhs.kind == .none_expr {
		opt_id = lhs_id
	} else if lhs.kind == .nil_literal && t.is_optional_type_name(t.node_type(rhs_id)) {
		// `nil == x` on a `?&T` behaves like `none == x`
		opt_id = rhs_id
	} else if rhs.kind == .nil_literal && t.is_optional_type_name(t.node_type(lhs_id)) {
		opt_id = lhs_id
	} else {
		lhs_type := t.node_type(lhs_id)
		rhs_type := t.node_type(rhs_id)
		if !t.is_optional_type_name(lhs_type) || !t.is_optional_type_name(rhs_type) {
			return none
		}
		lhs_value := t.stable_expr_for_reuse(lhs_id)
		rhs_value := t.stable_expr_for_reuse(rhs_id)
		eq := t.make_optional_semantic_eq_expr(lhs_value, rhs_value, lhs_type, rhs_type, []string{})
		if node.op == .ne {
			return t.make_prefix(.not, t.make_paren(eq))
		}
		return eq
	}
	mut opt_type := t.optional_result_expr_type_name(opt_id)
	if opt_type.len == 0 {
		opt_type = t.node_type(opt_id)
	}
	if !t.is_optional_type_name(opt_type) {
		return none
	}
	ok := t.make_selector(t.transform_expr(opt_id), 'ok', 'bool')
	if node.op == .eq {
		return t.make_prefix(.not, ok)
	}
	return ok
}

// struct_lookup_name supports struct lookup name handling for Transformer.
fn (t &Transformer) struct_lookup_name(type_name string) string {
	if type_name.len == 0 {
		return ''
	}
	// Primitives, arrays and maps are never struct names. Bail before the qualified-name
	// concatenation below — this runs for every infix operand, so the saved allocation
	// matters. (Behaviour is unchanged: these always resolved to '' anyway.)
	first := type_name[0]
	if first == `[`
		|| (first >= `a` && first <= `z` && types.is_builtin_type_name(type_name))
		|| type_name.starts_with('map[') {
		return ''
	}
	base, _, has_generic_args := generic_app_parts(type_name)
	if has_generic_args {
		if t.struct_lookup_name(base).len > 0 {
			return type_name
		}
	}
	if type_name.contains('.') {
		if type_name in t.structs {
			return type_name
		}
		short_type := type_name.all_after_last('.')
		type_mod := type_name.all_before_last('.')
		if info := t.structs[short_type] {
			if info.module == type_mod {
				return short_type
			}
		}
		if info := t.structs[type_name] {
			if info.module == type_mod {
				return type_name
			}
		}
		return ''
	}
	if type_name in t.structs {
		if info := t.structs[type_name] {
			if info.module.len == 0 || info.module == t.cur_module {
				return type_name
			}
		}
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${type_name}'
		if qtype in t.structs {
			return qtype
		}
	}
	return ''
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
			new_lhs := t.stable_expr_for_reuse(lhs_id)
			low_id := t.a.children[rhs.children_start]
			high_id := t.a.children[rhs.children_start + 1]
			new_low := t.transform_expr(low_id)
			new_high := t.transform_expr(high_id)

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
			new_lhs := t.stable_expr_for_reuse(lhs_id)
			is_str := t.is_string_type(lhs_id)
			mut or_chain := flat.empty_node
			for i in 0 .. rhs.children_count {
				elem_id := t.a.children[rhs.children_start + i]
				new_elem := t.transform_expr(elem_id)
				eq_cmp := if is_str {
					t.make_call('string__eq', arr2(new_lhs, new_elem))
				} else {
					t.make_infix(.eq, new_lhs, new_elem)
				}
				if int(or_chain) < 0 {
					or_chain = eq_cmp
				} else {
					or_chain = t.make_infix(.logical_or, or_chain, eq_cmp)
				}
			}
			result = or_chain
		}
	} else {
		rhs_type := t.node_type(rhs_id)
		clean_rhs_type := t.membership_container_type(rhs_type)
		rhs_is_ptr_array := t.membership_container_is_pointer_array(rhs_type)
		if clean_rhs_type.starts_with('[]') || clean_rhs_type == 'array' {
			if lowered_const := t.lower_const_string_array_membership_expr(rhs_id, lhs_id) {
				result = lowered_const
			} else if lowered := t.lower_array_membership_expr(rhs_id, lhs_id, rhs_type, false,
				node)
			{
				result = lowered
			} else {
				// dynamic array membership -> array_contains_int/string(arr, val)
				new_lhs := t.transform_expr(lhs_id)
				mut new_rhs := t.transform_expr(rhs_id)
				if rhs_is_ptr_array {
					new_rhs = t.make_prefix(.mul, new_rhs)
				}
				mut elem := if clean_rhs_type.starts_with('[]') { clean_rhs_type[2..] } else { '' }
				if elem.len == 0 {
					elem = t.node_type(lhs_id)
				}
				fn_name := array_contains_fn_name(elem)
				result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
			}
		} else if rhs.kind in [.ident, .selector] && (rhs_type.len == 0 || rhs_type == 'unknown') {
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			mut elem := t.node_type(lhs_id)
			lhs := t.a.nodes[int(lhs_id)]
			if elem.len == 0 && lhs.kind == .selector {
				elem = t.selector_field_type(lhs)
			}
			if elem.len == 0 && rhs.kind == .selector {
				elem = t.selector_array_elem_type(rhs)
			}
			if elem.len > 0 {
				fn_name := array_contains_fn_name(elem)
				result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
			}
		} else if t.is_fixed_array_type(clean_rhs_type) {
			if lowered := t.lower_array_membership_expr(rhs_id, lhs_id, rhs_type, false, node) {
				result = lowered
			} else {
				// fixed array membership -> fixed_array_contains_int/string(arr, len, val)
				new_lhs := t.transform_expr(lhs_id)
				new_rhs := t.transform_expr(rhs_id)
				elem := fixed_array_elem_type(clean_rhs_type)
				fn_name := fixed_array_contains_fn_name(elem)
				len_expr := t.make_fixed_array_len_expr(clean_rhs_type)
				result = t.make_call_typed(fn_name, arr3(new_rhs, len_expr, new_lhs), 'bool')
			}
		} else if clean_rhs_type == 'string' {
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			fn_name := if t.node_type(lhs_id) in ['u8', 'byte'] {
				'string__contains_u8'
			} else {
				'string__contains'
			}
			result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
		} else if clean_rhs_type.starts_with('map[') || clean_rhs_type == 'map' {
			if lowered := t.lower_map_membership_expr(rhs_id, lhs_id, rhs_type) {
				result = lowered
			}
		} else {
			// Unknown containment is kept as in_expr so the backend can reject or
			// handle genuinely unresolved cases.
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
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
		start := t.a.children.len
		t.a.children << t.make_paren(result)
		return t.a.add_node(flat.Node{
			kind:           .prefix
			op:             .not
			children_start: start
			children_count: 1
		})
	}
	return result
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
	needle := t.transform_expr(needle_id)
	base_value := t.transform_expr(base_id)
	base_data := t.make_cast('&string', t.make_selector(base_value, 'data', 'voidptr'), '&string')
	len_expr := t.make_int_literal(expr.children_count)
	return t.make_call_typed('fixed_array_contains_string', arr3(base_data, len_expr, needle),
		'bool')
}

// lower_type_pattern_membership builds lower type pattern membership data for transform.
fn (mut t Transformer) lower_type_pattern_membership(lhs_id flat.NodeId, rhs flat.Node, is_not_in bool) ?flat.NodeId {
	if rhs.children_count == 0 {
		return none
	}
	mut patterns := []string{cap: int(rhs.children_count)}
	mut sum_name := t.trim_pointer_type(t.original_expr_type(lhs_id))
	if !t.is_sum_type_name(sum_name) {
		sum_name = t.trim_pointer_type(t.node_type(lhs_id))
	}
	for i in 0 .. rhs.children_count {
		elem_id := t.a.child(&rhs, i)
		pattern := t.type_pattern_name(elem_id)
		if pattern.len == 0 {
			return none
		}
		if !t.is_sum_type_name(sum_name) {
			pattern_sum := t.find_sum_type_for_variant(pattern)
			if pattern_sum.len == 0 {
				return none
			}
			sum_name = pattern_sum
		} else if t.sum_variant_path(sum_name, pattern).len == 0 {
			return none
		}
		patterns << pattern
	}
	if !t.is_sum_type_name(sum_name) {
		return none
	}
	lhs_type := t.original_expr_type(lhs_id)
	base := t.stable_expr_for_reuse(lhs_id)
	mut chain := flat.empty_node
	for pattern in patterns {
		cmp := t.make_sum_type_pattern_check(base, lhs_type, sum_name, pattern) or { return none }
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
fn (t &Transformer) type_pattern_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := t.type_pattern_name(t.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.array_init {
			if node.value.len == 0 {
				return ''
			}
			return '[]${node.value}'
		}
		.array_literal {
			if node.value.len == 0 {
				return ''
			}
			return '[${node.children_count}]${node.value}'
		}
		else {
			return ''
		}
	}
}

// lower_array_membership_expr builds lower array membership expr data for transform.
fn (mut t Transformer) lower_array_membership_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type string, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type := t.membership_container_type(base_type)
	base_is_fixed := t.is_fixed_array_type(clean_base_type)
	if !clean_base_type.starts_with('[]') && clean_base_type != 'array' && !base_is_fixed {
		return none
	}
	mut elem_type := if clean_base_type.starts_with('[]') {
		clean_base_type[2..]
	} else if base_is_fixed {
		fixed_array_elem_type(clean_base_type)
	} else {
		''
	}
	if elem_type.len == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type.len == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
		needle = t.stable_expr_for_reuse(needle_id)
	} else {
		needle = t.stable_expr_for_reuse(needle_id)
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('contains')
	idx_name := t.new_temp('contains_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_bool_literal(false), 'bool')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	len_expr := if base_is_fixed {
		t.make_fixed_array_len_expr(clean_base_type)
	} else {
		t.make_selector(base, 'len', 'int')
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
	then_block := t.make_block(arr1(assign_true))
	loop_body << t.make_if(eq_expr, then_block, t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_index_expr builds lower array index expr data for transform.
fn (mut t Transformer) lower_array_index_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type string, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type := t.membership_container_type(base_type)
	base_is_fixed := t.is_fixed_array_type(clean_base_type)
	if !clean_base_type.starts_with('[]') && clean_base_type != 'array' && !base_is_fixed {
		return none
	}
	mut elem_type := if clean_base_type.starts_with('[]') {
		clean_base_type[2..]
	} else if base_is_fixed {
		fixed_array_elem_type(clean_base_type)
	} else {
		''
	}
	if elem_type.len == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type.len == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
		needle = t.stable_expr_for_reuse(needle_id)
	} else {
		needle = t.stable_expr_for_reuse(needle_id)
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('index')
	idx_name := t.new_temp('index_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(-1), 'int')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	len_expr := if base_is_fixed {
		t.make_fixed_array_len_expr(clean_base_type)
	} else {
		t.make_selector(base, 'len', 'int')
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
	loop_body << t.make_if(found_cond, t.make_block(arr1(assign_idx)), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_last_index_expr builds lower array last index expr data for transform.
fn (mut t Transformer) lower_array_last_index_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type string, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type := t.membership_container_type(base_type)
	base_is_fixed := t.is_fixed_array_type(clean_base_type)
	if !clean_base_type.starts_with('[]') && clean_base_type != 'array' && !base_is_fixed {
		return none
	}
	mut elem_type := if clean_base_type.starts_with('[]') {
		clean_base_type[2..]
	} else if base_is_fixed {
		fixed_array_elem_type(clean_base_type)
	} else {
		''
	}
	if elem_type.len == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type.len == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
		needle = t.stable_expr_for_reuse(needle_id)
	} else {
		needle = t.stable_expr_for_reuse(needle_id)
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('last_index')
	idx_name := t.new_temp('last_index_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(-1), 'int')
	len_expr := if base_is_fixed {
		t.make_fixed_array_len_expr(clean_base_type)
	} else {
		t.make_selector(base, 'len', 'int')
	}
	init := t.make_decl_assign_typed(idx_name,
		t.make_infix(.minus, len_expr, t.make_int_literal(1)), 'int')
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
	loop_body << t.make_if(found_cond, t.make_block(arr1(assign_idx)), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// stable_array_expr_for_membership
// supports helper handling in transform.
fn (mut t Transformer) stable_array_expr_for_membership(id flat.NodeId, raw_type string, clean_type string) flat.NodeId {
	mut expr := t.transform_expr(id)
	if t.membership_container_is_pointer_array(raw_type) {
		expr = t.make_prefix(.mul, expr)
	}
	return t.stable_transformed_expr_for_reuse(expr, clean_type, 'in_arr')
}

// make_membership_eq_expr builds make membership eq expr data for transform.
fn (mut t Transformer) make_membership_eq_expr(lhs flat.NodeId, rhs flat.NodeId, elem_type string) flat.NodeId {
	return t.make_membership_eq_expr_with_seen(lhs, rhs, elem_type, []string{})
}

fn (mut t Transformer) make_membership_eq_expr_with_seen(lhs flat.NodeId, rhs flat.NodeId, elem_type string, seen []string) flat.NodeId {
	if t.membership_type_is_pointer(elem_type) {
		return t.make_infix(.eq, lhs, rhs)
	}
	mut clean := t.membership_container_type(elem_type)
	if clean.len > 0 {
		lhs_type := t.node_type(lhs)
		if lhs_type == '&${clean}' {
			lhs_value := t.make_prefix(.mul, lhs)
			t.set_node_typ(int(lhs_value), clean)
			return t.make_membership_eq_expr_with_seen(lhs_value, rhs, elem_type, seen)
		}
		rhs_type := t.node_type(rhs)
		if rhs_type == '&${clean}' {
			rhs_value := t.make_prefix(.mul, rhs)
			t.set_node_typ(int(rhs_value), clean)
			return t.make_membership_eq_expr_with_seen(lhs, rhs_value, elem_type, seen)
		}
	}
	if clean == 'string' {
		return t.make_call_typed('string__eq', arr2(lhs, rhs), 'bool')
	}
	map_type := t.clean_map_type(clean)
	if map_type.starts_with('map[') {
		_, value_type := t.map_type_parts(map_type)
		if value_type.len > 0 && t.map_value_needs_element_eq(value_type) {
			src := if int(lhs) >= 0 && int(lhs) < t.a.nodes.len {
				t.a.nodes[int(lhs)]
			} else {
				flat.Node{}
			}
			return t.make_map_elementwise_eq_call_with_seen(lhs, rhs, map_type, src, seen)
		}
		return t.make_call_typed('v3_map_map_eq', arr2(lhs, rhs), 'bool')
	}
	if clean.starts_with('[]') {
		inner := clean[2..]
		if inner == 'string' {
			return t.make_call_typed('array_eq_string', arr2(lhs, rhs), 'bool')
		}
		if t.array_elem_needs_element_eq(inner) {
			src := if int(lhs) >= 0 && int(lhs) < t.a.nodes.len {
				t.a.nodes[int(lhs)]
			} else {
				flat.Node{}
			}
			return t.make_array_elementwise_eq_call_with_seen(lhs, rhs, inner, clean, clean, src,
				seen)
		}
		if inner.starts_with('[]') {
			return t.make_call_typed('array_eq_array', arr3(lhs, rhs,
				t.make_int_literal(array_nested_eq_depth(clean))), 'bool')
		}
		return t.make_call_typed('array_eq_raw', arr3(lhs, rhs, t.make_sizeof_type(inner)), 'bool')
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
		cmp := t.make_call_typed('C.memcmp', arr3(t.make_prefix(.amp, lhs_value), t.make_prefix(.amp,
			rhs_value), t.make_sizeof_type(clean)), 'int')
		return t.make_infix(.eq, cmp, t.make_int_literal(0))
	}
	if t.is_optional_type_name(clean) {
		return t.make_optional_semantic_eq_expr(lhs, rhs, clean, clean, seen)
	}
	if t.is_sum_type_name(clean) {
		return t.make_sum_semantic_eq_expr(lhs, rhs, clean, seen)
	}
	if t.is_interface_type(clean) {
		return t.make_interface_semantic_eq_expr(lhs, rhs, clean, seen)
	}
	struct_type := t.struct_lookup_name(clean)
	if struct_type.len > 0 {
		if method_name := t.struct_operator_fn_name_any(struct_type, '==') {
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, arr2(lhs, rhs), 'bool')
		}
		if struct_type in seen {
			cmp := t.make_call_typed('C.memcmp', arr3(t.make_prefix(.amp, lhs),
				t.make_prefix(.amp, rhs), t.make_sizeof_type(struct_type)), 'int')
			return t.make_infix(.eq, cmp, t.make_int_literal(0))
		}
		if field_eq := t.make_struct_field_eq_expr_with_seen(lhs, rhs, struct_type, seen) {
			return field_eq
		}
		cmp := t.make_call_typed('C.memcmp', arr3(t.make_prefix(.amp, lhs),
			t.make_prefix(.amp, rhs), t.make_sizeof_type(struct_type)), 'int')
		return t.make_infix(.eq, cmp, t.make_int_literal(0))
	}
	return t.make_infix(.eq, lhs, rhs)
}

// make_sum_semantic_eq_expr compares two sum-type values by tag, then by the
// boxed payload of the active variant (deep equality, like V1). Raw memcmp on
// the sum struct compares payload POINTERS and is always false for distinct
// allocations. The comparison is delegated to a synthesized helper fn
// (__v3_sum_eq_<name>) so recursive types (an XML node tree, an AST) compare
// correctly at any depth — the helper is generated after the transform pass.
fn (mut t Transformer) make_sum_semantic_eq_expr(lhs flat.NodeId, rhs flat.NodeId, sum_type string, seen []string) flat.NodeId {
	_ = seen
	clean_sum := t.resolve_sum_name(t.trim_pointer_type(sum_type))
	variants := t.sum_types[clean_sum] or { return t.make_memcmp_eq_expr(lhs, rhs, sum_type, 'eq') }
	if variants.len == 0 {
		return t.make_memcmp_eq_expr(lhs, rhs, sum_type, 'eq')
	}
	helper := sum_eq_helper_name(clean_sum)
	if clean_sum !in t.sum_eq_types {
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
		t.sum_eq_types[clean_sum] = SumEqRequest{
			module:        t.cur_module
			file:          t.cur_file
			helper_module: helper_module
		}
	}
	t.mark_fn_used_name(helper)
	return t.make_call_typed(helper, arr2(lhs, rhs), 'bool')
}

// sum_eq_helper_name is the global C-level name of the synthesized deep-equality
// helper for a sum type.
pub fn sum_eq_helper_name(sum_name string) string {
	return '__v3_sum_eq_${c_name(sum_name)}'
}

// synthesize_sum_eq_helpers generates the fn_decl for every sum type whose
// equality helper was requested during the transform. Building one helper body
// can request helpers for nested sum types, so this drains a worklist. Runs
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
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []string{}
		for name, _ in t.sum_eq_types {
			if name in t.sum_eq_synthesized {
				continue
			}
			if sum_eq_helper_name(name) in t.fn_ret_types {
				// already synthesized by an earlier Transformer over this AST
				t.sum_eq_synthesized[name] = true
				continue
			}
			pending << name
		}
		if pending.len == 0 {
			break
		}
		pending.sort()
		for name in pending {
			t.sum_eq_synthesized[name] = true
			req := t.sum_eq_types[name] or { SumEqRequest{} }
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
			t.build_sum_eq_helper_fn(name)
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
	t.sum_eq_helper_module = old_helper_module
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

// build_sum_eq_helper_fn appends `fn __v3_sum_eq_<Sum>(a Sum, b Sum) bool` to the
// AST: tags must match, then the active variant's payload is unboxed and compared
// by value (recursing through the usual membership-eq machinery, which routes
// nested sum types back through their own helpers).
fn (mut t Transformer) build_sum_eq_helper_fn(clean_sum string) {
	variants := t.sum_types[clean_sum] or { return }
	if variants.len == 0 {
		return
	}
	helper := sum_eq_helper_name(clean_sum)
	saved_pending := t.pending_stmts
	t.pending_stmts = []flat.NodeId{}
	param_a := t.a.add_node(flat.Node{
		kind:  .param
		value: '__sum_eq_a'
		typ:   clean_sum
	})
	param_b := t.a.add_node(flat.Node{
		kind:  .param
		value: '__sum_eq_b'
		typ:   clean_sum
	})
	mut stmts := []flat.NodeId{}
	lhs_value := t.make_ident('__sum_eq_a')
	t.a.nodes[int(lhs_value)].typ = clean_sum
	rhs_value := t.make_ident('__sum_eq_b')
	t.a.nodes[int(rhs_value)].typ = clean_sum
	tag_ne := t.make_infix(.ne, t.make_sum_tag_selector(lhs_value, .dot),
		t.make_sum_tag_selector(rhs_value, .dot))
	ret_false := t.make_return(t.make_bool_literal(false), 'bool')
	stmts << t.make_if(tag_ne, t.make_block(arr1(ret_false)), t.make_empty())
	result_name := t.new_temp('sum_eq_res')
	stmts << t.make_decl_assign_typed(result_name, t.make_bool_literal(true), 'bool')
	for variant in variants {
		qv := t.resolve_variant(clean_sum, variant)
		if qv.len == 0 {
			continue
		}
		field := t.sum_field_name(qv)
		use_ptr := t.variant_references_sum(qv, clean_sum)
		field_typ := if use_ptr { '&${qv}' } else { qv }
		mut lhs_payload := t.make_selector_op(lhs_value, field, field_typ, .dot)
		mut rhs_payload := t.make_selector_op(rhs_value, field, field_typ, .dot)
		if use_ptr {
			lhs_payload = t.make_prefix(.mul, lhs_payload)
			t.a.nodes[int(lhs_payload)].typ = qv
			rhs_payload = t.make_prefix(.mul, rhs_payload)
			t.a.nodes[int(rhs_payload)].typ = qv
		}
		pending_start := t.pending_stmts.len
		payload_eq := if t.is_sum_type_name(qv) && t.resolve_sum_name(qv) == clean_sum {
			// a variant that aliases back to this same sum: recurse via the helper
			t.make_call_typed(helper, arr2(lhs_payload, rhs_payload), 'bool')
		} else {
			t.make_membership_eq_expr_with_seen(lhs_payload, rhs_payload, qv, []string{})
		}
		mut body_stmts := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
		body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(payload_eq)),
			t.make_block(arr1(set_false)), t.make_empty())
		guard := t.make_infix(.logical_and, t.make_ident(result_name), t.make_sum_is_check(lhs_value,
			clean_sum, clean_sum, variant))
		stmts << t.make_if(guard, t.make_block(body_stmts), t.make_empty())
	}
	ret_result := t.make_ident(result_name)
	t.a.nodes[int(ret_result)].typ = 'bool'
	stmts << t.make_return(ret_result, 'bool')
	t.pending_stmts = saved_pending
	// Keep the helper in its request's output segment. This is normally the requesting
	// module, but program-specific generic specializations and their nested helpers use
	// main even though their bodies are resolved under the declaring module.
	t.a.add_node(flat.Node{
		kind:  .module_decl
		value: if t.sum_eq_helper_module.len > 0 { t.sum_eq_helper_module } else { 'main' }
	})
	start := t.a.children.len
	t.a.children << param_a
	t.a.children << param_b
	for stmt in stmts {
		t.a.children << stmt
	}
	t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          helper
		typ:            'bool'
		children_start: i32(start)
		children_count: flat.child_count(2 + stmts.len)
	})
	t.register_sum_eq_helper_signature(helper, clean_sum)
}

fn (mut t Transformer) register_sum_eq_helper_signature(helper string, clean_sum string) {
	t.fn_ret_types[helper] = 'bool'
	t.mark_fn_used_name(helper)
	if !isnil(t.tc) {
		ret := t.tc.parse_type('bool')
		params := [t.tc.parse_type(clean_sum), t.tc.parse_type(clean_sum)]
		t.tc.fn_ret_types[helper] = ret
		t.tc.fn_param_types[helper] = params.clone()
		t.tc.fn_variadic[helper] = false
	}
}

fn (mut t Transformer) make_memcmp_eq_expr(lhs flat.NodeId, rhs flat.NodeId, typ string, prefix string) flat.NodeId {
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, typ, '${prefix}_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, typ, '${prefix}_rhs')
	cmp := t.make_call_typed('C.memcmp', arr3(t.make_prefix(.amp, lhs_value), t.make_prefix(.amp,
		rhs_value), t.make_sizeof_type(typ)), 'int')
	return t.make_infix(.eq, cmp, t.make_int_literal(0))
}

fn (mut t Transformer) make_optional_semantic_eq_expr(lhs flat.NodeId, rhs flat.NodeId, lhs_opt_type string, rhs_opt_type string, seen []string) flat.NodeId {
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, lhs_opt_type, 'opt_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_opt_type, 'opt_eq_rhs')
	lhs_ok := t.make_selector(lhs_value, 'ok', 'bool')
	rhs_ok := t.make_selector(rhs_value, 'ok', 'bool')
	ok_same := t.make_infix(.eq, lhs_ok, rhs_ok)
	lhs_value_type := t.optional_base_type(lhs_opt_type)
	rhs_value_type := t.optional_base_type(rhs_opt_type)
	if lhs_value_type.len == 0 || lhs_value_type == 'void' {
		return ok_same
	}
	rhs_field_type := if rhs_value_type.len == 0 { lhs_value_type } else { rhs_value_type }
	lhs_value_field := t.make_selector(lhs_value, 'value', lhs_value_type)
	rhs_value_field := t.make_selector(rhs_value, 'value', rhs_field_type)
	pending_start := t.pending_stmts.len
	value_eq := t.make_membership_eq_expr_with_seen(lhs_value_field, rhs_value_field,
		lhs_value_type, seen)
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	if body_stmts.len == 0 {
		ok_or_value_eq := t.make_infix(.logical_or, t.make_prefix(.not, t.make_selector(lhs_value,
			'ok', 'bool')), value_eq)
		return t.make_infix(.logical_and, ok_same, ok_or_value_eq)
	}
	result_name := t.new_temp('opt_eq')
	t.pending_stmts << t.make_decl_assign_typed(result_name, ok_same, 'bool')
	compare_values := t.make_infix(.logical_and, t.make_ident(result_name), t.make_selector(lhs_value,
		'ok', 'bool'))
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(value_eq)),
		t.make_block(arr1(set_false)), t.make_empty())
	t.pending_stmts << t.make_if(compare_values, t.make_block(body_stmts), t.make_empty())
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'bool')
	return result
}

fn (t &Transformer) array_elem_needs_element_eq(elem_type string) bool {
	if t.membership_type_is_pointer(elem_type) {
		return false
	}
	clean := t.membership_container_type(elem_type)
	return clean != 'string' && t.type_needs_semantic_eq(clean)
}

fn (t &Transformer) map_value_needs_element_eq(value_type string) bool {
	if t.membership_type_is_pointer(value_type) {
		return false
	}
	clean := t.membership_container_type(value_type)
	if t.is_fixed_array_type(clean) {
		return true
	}
	return t.type_needs_semantic_eq(clean)
}

fn (t &Transformer) type_needs_semantic_eq(typ string) bool {
	if t.membership_type_is_pointer(typ) {
		return false
	}
	clean := t.membership_container_type(typ)
	if clean == 'string' {
		return true
	}
	if t.clean_map_type(clean).starts_with('map[') {
		return true
	}
	if clean.starts_with('[]') {
		return t.type_needs_semantic_eq(clean[2..])
	}
	if t.is_fixed_array_type(clean) {
		elem_type := fixed_array_elem_type(clean)
		return elem_type.len > 0 && t.type_needs_semantic_eq(elem_type)
	}
	if t.is_optional_type_name(clean) {
		return true
	}
	if t.is_interface_type(clean) {
		return true
	}
	if t.struct_lookup_name(clean).len > 0 {
		return true
	}
	return t.is_sum_type_name(clean)
}

fn (mut t Transformer) make_array_elementwise_eq_call(lhs flat.NodeId, rhs flat.NodeId, elem_type string, lhs_type string, rhs_type string, src flat.Node) flat.NodeId {
	return t.make_array_elementwise_eq_call_with_seen(lhs, rhs, elem_type, lhs_type, rhs_type, src,
		[]string{})
}

fn (mut t Transformer) make_array_elementwise_eq_call_with_seen(lhs flat.NodeId, rhs flat.NodeId, elem_type string, lhs_type string, rhs_type string, src flat.Node, seen []string) flat.NodeId {
	mut clean_elem := t.membership_container_type(elem_type)
	if t.is_fixed_array_type(clean_elem) {
		clean_elem = t.resolved_fixed_array_canonical_type(clean_elem)
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'arr_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'arr_eq_rhs')
	result_name := t.new_temp('arr_eq')
	idx_name := t.new_temp('arr_eq_idx')
	len_eq := t.make_infix(.eq, t.make_selector(lhs_value, 'len', 'int'), t.make_selector(rhs_value,
		'len', 'int'))
	t.pending_stmts << t.make_decl_assign_typed(result_name, len_eq, 'bool')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	in_bounds := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(lhs_value, 'len', 'int'))
	cond := t.make_infix(.logical_and, t.make_ident(result_name), in_bounds)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	lhs_elem := t.array_get_value(lhs_value, t.make_ident(idx_name), clean_elem)
	rhs_elem := t.array_get_value(rhs_value, t.make_ident(idx_name), clean_elem)
	pending_start := t.pending_stmts.len
	elem_eq := t.make_membership_eq_expr_with_seen(lhs_elem, rhs_elem, clean_elem, seen)
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(elem_eq)),
		t.make_block(arr1(set_false)), t.make_empty())
	t.pending_stmts << t.make_for_stmt(init, cond, post, body_stmts, src)
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'bool')
	return result
}

fn (mut t Transformer) make_fixed_array_elementwise_eq_expr(lhs flat.NodeId, rhs flat.NodeId, fixed_type string) ?flat.NodeId {
	return t.make_fixed_array_elementwise_eq_expr_with_seen(lhs, rhs, fixed_type, []string{})
}

fn (mut t Transformer) make_fixed_array_elementwise_eq_expr_with_seen(lhs flat.NodeId, rhs flat.NodeId, fixed_type string, seen []string) ?flat.NodeId {
	clean_fixed_type := t.resolved_fixed_array_canonical_type(fixed_type)
	elem_type := fixed_array_elem_type(clean_fixed_type)
	if elem_type.len == 0 {
		return none
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, clean_fixed_type, 'fixed_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, clean_fixed_type, 'fixed_eq_rhs')
	result_name := t.new_temp('fixed_eq')
	idx_name := t.new_temp('fixed_eq_idx')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_bool_literal(true), 'bool')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_fixed_array_len_expr(clean_fixed_type))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	lhs_elem := t.make_index(lhs_value, t.make_ident(idx_name), elem_type)
	rhs_elem := t.make_index(rhs_value, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	elem_eq := t.make_membership_eq_expr_with_seen(lhs_elem, rhs_elem, elem_type, seen)
	mut body_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body_stmts << t.make_if(t.make_prefix(.not, t.make_paren(elem_eq)),
		t.make_block(arr1(set_false)), t.make_empty())
	t.pending_stmts << t.make_for_stmt(init,
		t.make_infix(.logical_and, t.make_ident(result_name), cond), post, body_stmts, flat.Node{})
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'bool')
	return result
}

fn (mut t Transformer) make_map_elementwise_eq_call(lhs flat.NodeId, rhs flat.NodeId, map_type string, src flat.Node) flat.NodeId {
	return t.make_map_elementwise_eq_call_with_seen(lhs, rhs, map_type, src, []string{})
}

fn (mut t Transformer) make_map_elementwise_eq_call_with_seen(lhs flat.NodeId, rhs flat.NodeId, map_type string, src flat.Node, seen []string) flat.NodeId {
	key_type, raw_value_type := t.map_type_parts(map_type)
	value_type := if t.is_fixed_array_type(raw_value_type) {
		t.resolved_fixed_array_canonical_type(raw_value_type)
	} else {
		raw_value_type
	}
	if key_type.len == 0 || value_type.len == 0 {
		return t.make_call_typed('v3_map_map_eq', arr2(lhs, rhs), 'bool')
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, map_type, 'map_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, map_type, 'map_eq_rhs')
	result_name := t.new_temp('map_eq')
	zero_name := t.new_temp('map_eq_zero')
	key_name := t.new_temp('map_eq_key')
	lhs_val_name := t.new_temp('map_eq_val')
	len_eq := t.make_infix(.eq, t.make_selector(lhs_value, 'len', 'int'), t.make_selector(rhs_value,
		'len', 'int'))
	t.pending_stmts << t.make_decl_assign_typed(result_name, len_eq, 'bool')
	t.pending_stmts << t.make_decl_assign_typed(zero_name, t.zero_value_for_type(value_type),
		value_type)
	t.set_var_type(key_name, t.map_key_storage_type(key_type))
	t.set_var_type(lhs_val_name, value_type)
	key_ident := t.make_ident(key_name)
	val_ident := t.make_ident(lhs_val_name)
	rhs_exists := t.make_map_exists_expr(rhs_value, map_type, key_name)
	rhs_val := t.make_map_get_expr(rhs_value, map_type, key_name, zero_name, value_type)
	pending_start := t.pending_stmts.len
	value_eq := t.make_membership_eq_expr_with_seen(t.make_ident(lhs_val_name), rhs_val,
		value_type, seen)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	missing := t.make_prefix(.not, t.make_paren(rhs_exists))
	value_diff := t.make_prefix(.not, t.make_paren(value_eq))
	failed := t.make_infix(.logical_or, missing, value_diff)
	active := t.make_infix(.logical_and, t.make_ident(result_name), failed)
	set_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
	body << t.make_if(active, t.make_block(arr1(set_false)), t.make_empty())
	start := t.a.children.len
	t.a.children << key_ident
	t.a.children << val_ident
	t.a.children << lhs_value
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		pos:            src.pos
		value:          '3'
	})
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'bool')
	return result
}

fn (mut t Transformer) make_struct_field_eq_expr(lhs flat.NodeId, rhs flat.NodeId, struct_type string) ?flat.NodeId {
	return t.make_struct_field_eq_expr_with_seen(lhs, rhs, struct_type, []string{})
}

fn (mut t Transformer) make_struct_field_eq_expr_with_seen(lhs flat.NodeId, rhs flat.NodeId, struct_type string, seen []string) ?flat.NodeId {
	info := t.lookup_struct_info(struct_type) or { return none }
	mut next_seen := seen.clone()
	next_seen << struct_type
	mut eq := flat.empty_node
	for field in info.fields {
		field_type := if field.typ.len > 0 { field.typ } else { field.raw_typ }
		lhs_field := t.make_selector(lhs, field.name, field_type)
		rhs_field := t.make_selector(rhs, field.name, field_type)
		field_eq := if t.membership_type_is_pointer(field_type) {
			t.make_infix(.eq, lhs_field, rhs_field)
		} else {
			t.make_membership_eq_expr_with_seen(lhs_field, rhs_field, field_type, next_seen)
		}
		eq = if int(eq) < 0 { field_eq } else { t.make_infix(.logical_and, eq, field_eq) }
	}
	if int(eq) < 0 {
		return t.make_bool_literal(true)
	}
	return eq
}

fn (mut t Transformer) make_interface_semantic_eq_expr(lhs flat.NodeId, rhs flat.NodeId, interface_type string, seen []string) flat.NodeId {
	iface := t.resolve_interface_type_name(interface_type)
	if iface.len == 0 || isnil(t.tc) {
		return t.make_memcmp_eq_expr(lhs, rhs, interface_type, 'iface_eq')
	}
	lhs_value := t.stable_transformed_expr_for_reuse(lhs, iface, 'iface_eq_lhs')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, iface, 'iface_eq_rhs')
	lhs_typ := t.make_selector(lhs_value, '_typ', 'int')
	rhs_typ := t.make_selector(rhs_value, '_typ', 'int')
	lhs_zero_tag := t.make_infix(.eq, lhs_typ, t.make_int_literal(0))
	rhs_zero_tag := t.make_infix(.eq, rhs_typ, t.make_int_literal(0))
	zero_tags := t.make_infix(.logical_and, lhs_zero_tag, rhs_zero_tag)
	empty_eq := if t.is_builtin_ierror_interface_name(iface) {
		lhs_addr := t.make_prefix(.amp, lhs_value)
		rhs_addr := t.make_prefix(.amp, rhs_value)
		lhs_msg := t.make_call_typed('IError__msg', arr1(lhs_addr), 'string')
		rhs_msg := t.make_call_typed('IError__msg', arr1(rhs_addr), 'string')
		msg_eq := t.make_call_typed('string__eq', arr2(lhs_msg, rhs_msg), 'bool')
		lhs_code := t.make_call_typed('IError__code', arr1(lhs_addr), 'int')
		rhs_code := t.make_call_typed('IError__code', arr1(rhs_addr), 'int')
		code_eq := t.make_infix(.eq, lhs_code, rhs_code)
		t.make_infix(.logical_and, zero_tags, t.make_infix(.logical_and, msg_eq, code_eq))
	} else {
		lhs_empty := t.make_infix(.eq, t.make_selector(lhs_value, '_object', 'voidptr'),
			t.make_int_literal(0))
		rhs_empty := t.make_infix(.eq, t.make_selector(rhs_value, '_object', 'voidptr'),
			t.make_int_literal(0))
		t.make_infix(.logical_and, zero_tags, t.make_infix(.logical_and, lhs_empty, rhs_empty))
	}
	result_name := t.new_temp('iface_eq_payload')
	t.pending_stmts << t.make_decl_assign_typed(result_name, empty_eq, 'bool')
	impl_names := if t.is_builtin_ierror_interface_name(iface) {
		t.tc.ierror_impl_names()
	} else {
		t.tc.interface_impl_names(iface)
	}
	for impl_name in impl_names {
		if !t.interface_boxed_type_marked(iface, impl_name) {
			continue
		}
		type_id := t.interface_impl_type_id(iface, impl_name) or { continue }
		lhs_object := t.make_cast('&${impl_name}',
			t.make_selector(lhs_value, '_object', 'voidptr'), '&${impl_name}')
		rhs_object := t.make_cast('&${impl_name}',
			t.make_selector(rhs_value, '_object', 'voidptr'), '&${impl_name}')
		lhs_concrete := t.make_prefix(.mul, lhs_object)
		rhs_concrete := t.make_prefix(.mul, rhs_object)
		t.set_node_typ(int(lhs_concrete), impl_name)
		t.set_node_typ(int(rhs_concrete), impl_name)
		saved := t.pending_stmts.clone()
		t.pending_stmts.clear()
		value_eq := t.make_membership_eq_expr_with_seen(lhs_concrete, rhs_concrete, impl_name, seen)
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
	t.set_node_typ(int(result), 'bool')
	return result
}

// selector_field_type supports selector field type handling for Transformer.
fn (t &Transformer) selector_field_type(node flat.Node) string {
	if node.value.len == 0 {
		return ''
	}
	for _, info in t.structs {
		for field in info.fields {
			if field.name == node.value {
				return t.normalize_type_alias(field.typ)
			}
		}
	}
	return ''
}

// selector_array_elem_type supports selector array elem type handling for Transformer.
fn (t &Transformer) selector_array_elem_type(node flat.Node) string {
	if node.value.len == 0 {
		return ''
	}
	for _, info in t.structs {
		for field in info.fields {
			clean_typ := t.membership_container_type(field.typ)
			if field.name == node.value && clean_typ.starts_with('[]') {
				return t.normalize_type_alias(clean_typ[2..])
			}
		}
	}
	return ''
}

// membership_container_type supports membership container type handling for Transformer.
fn (t &Transformer) membership_container_type(typ string) string {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('...') {
			clean = '[]' + clean[3..].trim_space()
			continue
		}
		break
	}
	return t.normalize_type_alias(clean)
}

fn (t &Transformer) membership_type_is_pointer(typ string) bool {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('mut ') {
			clean = clean[4..].trim_space()
			continue
		}
		break
	}
	return clean.starts_with('&')
}

fn (t &Transformer) equality_type_is_array_pointer(typ string) bool {
	mut clean := typ.trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if clean.starts_with('mut ') {
		return false
	}
	clean = t.normalize_type_alias(clean).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if !clean.starts_with('&') {
		return false
	}
	container := t.membership_container_type(clean)
	return container.starts_with('[]') || container == 'array'
}

fn (t &Transformer) equality_expr_is_string_pointer(id flat.NodeId, typ string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.string_literal, .string_interp] {
		return false
	}
	mut clean := typ.trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if clean.starts_with('mut ') {
		return false
	}
	clean = t.normalize_type_alias(clean).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	return clean == '&string'
}

fn (t &Transformer) equality_type_is_map_pointer(typ string) bool {
	mut clean := typ.trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if clean.starts_with('mut ') {
		return false
	}
	clean = t.normalize_type_alias(clean).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	return clean.starts_with('&') && t.clean_map_type(clean).starts_with('map[')
}

// membership_container_is_pointer_array supports membership_container_is_pointer_array handling.
fn (t &Transformer) membership_container_is_pointer_array(typ string) bool {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if !clean.starts_with('&') {
		return false
	}
	clean = t.membership_container_type(clean)
	return clean.starts_with('[]') || clean == 'array'
}

// array_contains_fn_name reports whether array contains fn name applies in transform.
fn array_contains_fn_name(elem string) string {
	return match elem {
		'string' { 'array_contains_string' }
		'u8', 'byte' { 'array_contains_u8' }
		else { 'array_contains_int' }
	}
}

// fixed_array_contains_fn_name reports whether fixed array contains fn name applies in transform.
fn fixed_array_contains_fn_name(elem string) string {
	return match elem {
		'string' { 'fixed_array_contains_string' }
		'u8', 'byte' { 'fixed_array_contains_u8' }
		else { 'fixed_array_contains_int' }
	}
}

// stable_expr_for_reuse supports stable expr for reuse handling for Transformer.
fn (mut t Transformer) stable_expr_for_reuse(id flat.NodeId) flat.NodeId {
	expr := t.transform_expr(id)
	if t.is_stable_expr_for_reuse(expr) {
		return expr
	}
	tmp_name := t.new_temp('in_lhs')
	mut tmp_typ := t.node_type(expr)
	if tmp_typ.len == 0 {
		tmp_typ = t.node_type(id)
	}
	decl := t.make_decl_assign(tmp_name, expr)
	if tmp_typ.len > 0 {
		t.set_node_typ(int(decl), tmp_typ)
		t.set_var_type(tmp_name, tmp_typ)
	}
	t.pending_stmts << decl
	return t.make_ident(tmp_name)
}

// stable_transformed_expr_for_reuse
// supports helper handling in transform.
fn (mut t Transformer) stable_transformed_expr_for_reuse(expr flat.NodeId, typ string, prefix string) flat.NodeId {
	if t.is_stable_expr_for_reuse(expr) {
		return expr
	}
	tmp_name := t.new_temp(prefix)
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, typ)
	return t.make_ident(tmp_name)
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

// transform_fixed_array_len transforms transform fixed array len data for transform.
fn (mut t Transformer) transform_fixed_array_len(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.value != 'len' || node.children_count == 0 {
		return none
	}
	base_id := t.a.children[node.children_start]
	base_type := t.node_type(base_id)
	if !t.is_fixed_array_type(base_type) {
		return none
	}
	return t.make_fixed_array_len_expr(base_type)
}

// clean_map_type transforms clean map type data for transform.
fn (t &Transformer) clean_map_type(typ string) string {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		break
	}
	return t.normalize_type_alias(clean)
}

// runtime_addr supports runtime addr handling for Transformer.
fn (mut t Transformer) runtime_addr(expr flat.NodeId, typ string) flat.NodeId {
	if typ.starts_with('&') {
		if addr := t.redundant_deref_addr(expr) {
			return addr
		}
		return expr
	}
	if !t.expr_can_take_address(expr) {
		stable := t.stable_transformed_expr_for_reuse(expr, typ, 'addr')
		return t.make_prefix(.amp, stable)
	}
	return t.make_prefix(.amp, expr)
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
fn (mut t Transformer) transform_enum_shorthand(id flat.NodeId, node flat.Node, expected_enum string) flat.NodeId {
	resolved_enum := t.enum_type_name_for_expected(expected_enum, '')
	if resolved_enum.len == 0 {
		return id
	}
	short_name := node.value.trim_left('.')
	if fields := t.enum_types[resolved_enum] {
		for f in fields {
			if f == short_name {
				return t.a.add_node(flat.Node{
					kind:  .enum_val
					value: '${resolved_enum}.${short_name}'
					typ:   resolved_enum
				})
			}
		}
	}
	return id
}

// enum_type_name_for_expected supports enum type name for expected handling for Transformer.
fn (t &Transformer) enum_type_name_for_expected(expected_enum string, owner_mod string) string {
	if expected_enum.len == 0 {
		return ''
	}
	mut clean := expected_enum
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean in t.enum_types {
		return clean
	}
	if clean.contains('.') {
		return ''
	}
	for mod_name in [owner_mod, t.cur_module] {
		if mod_name.len == 0 || mod_name == 'main' || mod_name == 'builtin' {
			continue
		}
		qname := '${mod_name}.${clean}'
		if qname in t.enum_types {
			return qname
		}
	}
	mut found := ''
	for enum_name, _ in t.enum_types {
		if enum_name.all_after_last('.') != clean {
			continue
		}
		if found.len > 0 && found != enum_name {
			return ''
		}
		found = enum_name
	}
	return found
}

// make_call builds make call data for transform.
pub fn (mut t Transformer) make_call(fn_name string, args []flat.NodeId) flat.NodeId {
	return t.make_call_typed(fn_name, args, '')
}

// make_call_typed builds make call typed data for transform.
pub fn (mut t Transformer) make_call_typed(fn_name string, args []flat.NodeId, typ string) flat.NodeId {
	fn_ident := t.make_ident(fn_name)
	return t.make_call_expr_typed(fn_ident, args, typ)
}

fn (mut t Transformer) mark_fn_used(fn_name string) {
	if fn_name.len == 0 || t.used_fns.len == 0 {
		return
	}
	t.mark_used_fn_key(fn_name)
	t.mark_used_fn_key(c_name(fn_name))
}

// make_call_expr_typed builds make call expr typed data for transform.
pub fn (mut t Transformer) make_call_expr_typed(fn_expr flat.NodeId, args []flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << fn_expr
	for arg in args {
		t.a.children << arg
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		children_start: start
		children_count: flat.child_count(1 + args.len)
		typ:            typ
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
pub fn (mut t Transformer) make_selector(base flat.NodeId, field string, typ string) flat.NodeId {
	return t.make_selector_op(base, field, typ, .dot)
}

// make_selector_op builds make selector op data for transform.
pub fn (mut t Transformer) make_selector_op(base flat.NodeId, field string, typ string, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << base
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             op
		children_start: start
		children_count: 1
		value:          field
		typ:            typ
	})
}

// make_sum_tag_selector builds an internal selector for a sumtype discriminator.
pub fn (mut t Transformer) make_sum_tag_selector(base flat.NodeId, op flat.Op) flat.NodeId {
	return t.make_selector_op(base, sum_type_tag_selector_field, 'int', op)
}

// make_index builds make index data for transform.
pub fn (mut t Transformer) make_index(base flat.NodeId, index flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << base
	t.a.children << index
	return t.a.add_node(flat.Node{
		kind:           .index
		children_start: start
		children_count: 2
		typ:            typ
	})
}

// make_cast builds make cast data for transform.
pub fn (mut t Transformer) make_cast(target_type string, expr flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		children_start: start
		children_count: 1
		value:          target_type
		typ:            typ
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
pub fn (mut t Transformer) make_struct_init(name string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .struct_init
		value: name
		typ:   name
	})
}

// make_array_init builds make array init data for transform.
pub fn (mut t Transformer) make_array_init(elem_type string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .array_init
		value: elem_type
		typ:   '[]${elem_type}'
	})
}

// make_map_init builds make map init data for transform.
pub fn (mut t Transformer) make_map_init(map_type string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .map_init
		value: map_type
		typ:   map_type
	})
}

// make_string_literal builds make string literal data for transform.
pub fn (mut t Transformer) make_string_literal(value string) flat.NodeId {
	return t.a.add_val(.string_literal, value)
}

// make_int_literal builds make int literal data for transform.
pub fn (mut t Transformer) make_int_literal(value int) flat.NodeId {
	return t.a.add_val(.int_literal, '${value}')
}

pub fn (mut t Transformer) make_int_literal_typed(value string, typ string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .int_literal
		value: value
		typ:   typ
	})
}

// make_float_literal builds make float literal data for transform.
pub fn (mut t Transformer) make_float_literal(value string) flat.NodeId {
	return t.a.add_val(.float_literal, value)
}

pub fn (mut t Transformer) make_float_literal_typed(value string, typ string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .float_literal
		value: value
		typ:   typ
	})
}

// make_bool_literal builds make bool literal data for transform.
pub fn (mut t Transformer) make_bool_literal(value bool) flat.NodeId {
	return t.a.add_val(.bool_literal, if value { 'true' } else { 'false' })
}

// make_sizeof_type builds make sizeof type data for transform.
pub fn (mut t Transformer) make_sizeof_type(type_name string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .sizeof_expr
		value: type_name
		typ:   'usize'
	})
}

// is_fixed_array_type reports whether a v-type string denotes a fixed array
// like `int[5]` (as opposed to a dynamic array `[]int` or a map `map[...]...`).
fn (t &Transformer) is_fixed_array_type(s string) bool {
	if s.starts_with('[]') || s.starts_with('map[') {
		return false
	}
	if s.starts_with('[') {
		return s.contains(']')
	}
	if !s.contains('[') || !s.ends_with(']') {
		return false
	}
	len_text := fixed_array_len_text(s)
	if is_decimal_text(len_text) {
		return true
	}
	// A postfix fixed-array name (`ArrayFixed.name()`) can carry a non-decimal length — a const
	// (`int[seg_count]`) or an expression (`int[segs + 1]`) — once the checker round-trips
	// `[n]int` to `int[n]`. Accept it when the bracket text resolves to an integer constant,
	// which distinguishes it from a generic instance (`vec.Vec4[f32]`, whose bracket is a type).
	if len_text.contains(',') || isnil(t.tc) {
		return false
	}
	return t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) != none
}

// fixed_array_len supports fixed array len handling for transform.
fn fixed_array_len(s string) int {
	return fixed_array_len_text(s).int()
}

// fixed_array_len_text supports fixed array len text handling for transform.
fn fixed_array_len_text(s string) string {
	if !s.starts_with('[') {
		_, dims := transform_postfix_fixed_array_parts(s)
		if dims.len > 0 {
			return dims[0]
		}
	}
	return s.all_after('[').all_before(']').trim_space()
}

// fixed_array_elem_type supports fixed array elem type handling for transform.
fn fixed_array_elem_type(s string) string {
	if s.starts_with('[') {
		return s.all_after(']')
	}
	elem, dims := transform_postfix_fixed_array_parts(s)
	if dims.len == 0 {
		return s.all_before('[')
	}
	mut out := elem
	for i := dims.len - 1; i > 0; i-- {
		out += '[${dims[i]}]'
	}
	return out
}

fn fixed_array_canonical_type(s string) string {
	if !s.starts_with('[') {
		return s
	}
	elem_type := fixed_array_canonical_type(fixed_array_elem_type(s))
	len_text := fixed_array_len_text(s)
	return '${elem_type}[${len_text}]'
}

fn (t &Transformer) resolved_fixed_array_canonical_type(s string) string {
	clean := fixed_array_canonical_type(s)
	if !t.is_fixed_array_type(clean) {
		return clean
	}
	elem_type := fixed_array_elem_type(clean)
	resolved_elem := if t.is_fixed_array_type(elem_type) {
		t.resolved_fixed_array_canonical_type(elem_type)
	} else {
		elem_type
	}
	len_text := fixed_array_len_text(clean)
	if is_decimal_text(len_text) || isnil(t.tc) {
		return '${resolved_elem}[${len_text}]'
	}
	if v := t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) {
		return '${resolved_elem}[${v}]'
	}
	return '${resolved_elem}[${len_text}]'
}

// is_decimal_text reports whether is decimal text applies in transform.
fn is_decimal_text(s string) bool {
	if s.len == 0 {
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
fn (mut t Transformer) make_fixed_array_len_expr(s string) flat.NodeId {
	len_text := fixed_array_len_text(s)
	if is_decimal_text(len_text) {
		return t.make_int_literal(len_text.int())
	}
	// Fold a const length — a bare const (`SEGS`) or an expression (`segs + 1`) — to
	// its integer value. A fixed array's `.len` is a compile-time constant; emitting
	// the raw expression as an ident would c_name-mangle it into an undeclared
	// identifier such as `segs_+_1`.
	if !isnil(t.tc) {
		if v := t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) {
			return t.make_int_literal(v)
		}
	}
	if len_text.contains('.') {
		base := len_text.all_before_last('.')
		field := len_text.all_after_last('.')
		return t.make_selector(t.make_ident(base), field, 'int')
	}
	return t.make_ident(len_text)
}

// c_name returns the C identifier used for a V symbol or type name.
fn c_name(name string) string {
	return naming.c_name(name)
}
