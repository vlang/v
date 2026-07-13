module transform

import v3.flat

// try_expand_if_guard detects an if-guard pattern where the condition is a
// decl_assign whose RHS is a call returning an optional (?T) or result (!T).
// When detected it expands:
//   if val := maybe_call() { body }
// into:
//   __or_tmp_N := maybe_call()
//   if !__or_tmp_N.is_error { val := __or_tmp_N.data; body... }
//
fn (mut t Transformer) try_expand_if_guard(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind != .if_expr || node.children_count < 2 {
		return none
	}
	cond_id := t.a.child(&node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	lhs_ids := t.multi_assign_lhs_ids(cond)
	if lhs_ids.len == 1 {
		if info := t.map_index_info(rhs_id) {
			return t.expand_map_index_if_guard(node, lhs.value, info)
		}
		if info := t.array_index_info(rhs_id) {
			return t.expand_array_index_if_guard(node, lhs.value, info)
		}
	}
	mut rhs_type := t.optional_result_expr_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type := t.optional_base_type(rhs_type)
	tmp_name := t.new_temp('if_guard')
	rhs_expr := t.transform_expr(rhs_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	tmp_decl := t.make_decl_assign_typed(tmp_name, rhs_expr, rhs_type)
	ok_cond := t.make_selector(t.make_ident(tmp_name), 'ok', 'bool')
	mut value_decls := []flat.NodeId{}
	if lhs_ids.len > 1 {
		if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
			for i, lhs_item_id in lhs_ids {
				lhs_item := t.a.nodes[int(lhs_item_id)]
				if lhs_item.kind != .ident || lhs_item.value == '_' {
					continue
				}
				field_type := rhs_types[i].name()
				payload := t.make_selector(t.make_ident(tmp_name), 'value', value_type)
				field := t.make_selector(payload, 'arg${i}', field_type)
				value_decls << t.make_decl_assign_typed(lhs_item.value, field, field_type)
			}
		}
	}
	if value_decls.len == 0 && lhs.value != '_' && value_type != 'void' {
		value_decls << t.make_decl_assign_typed(lhs.value, t.make_selector(t.make_ident(tmp_name),
			'value', value_type), value_type)
	}

	then_id := t.a.child(&node, 1)
	then_node := t.a.nodes[int(then_id)]
	if lhs_ids.len > 1 {
		if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
			for i, lhs_item_id in lhs_ids {
				lhs_item := t.a.nodes[int(lhs_item_id)]
				if lhs_item.kind == .ident && lhs_item.value.len > 0 && lhs_item.value != '_' {
					t.set_var_type(lhs_item.value, rhs_types[i].name())
				}
			}
		}
	} else {
		if lhs.value != '_' && value_type != 'void' {
			t.set_var_type(lhs.value, value_type)
		}
	}
	mut then_children := []flat.NodeId{}
	for value_decl in value_decls {
		then_children << value_decl
	}
	if then_node.kind == .block {
		then_children << t.transform_stmts(t.a.children_of(&then_node))
	} else {
		then_children << t.transform_stmt(then_id)
	}
	then_block := t.make_block(then_children)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		else_block = t.transform_if_guard_else_block(else_id, else_node, tmp_name)
	}
	mut expanded := []flat.NodeId{cap: prelude.len + 2}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << tmp_decl
	expanded << t.make_if(ok_cond, then_block, else_block)
	return expanded
}

// optional_result_expr_type_name supports optional result expr type name handling for Transformer.
fn (mut t Transformer) optional_result_expr_type_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	if !isnil(t.tc) {
		node := t.a.nodes[int(id)]
		if node.kind == .call {
			concrete_ret := t.concrete_generic_call_return_type(id, node)
			if t.is_optional_type_name(concrete_ret) {
				return concrete_ret
			}
			if name := t.tc.resolved_call_name(id) {
				if ret := t.tc.fn_ret_types[name] {
					ret_name := ret.name()
					if t.is_optional_type_name(ret_name) {
						return ret_name
					}
				}
			}
			ret_name := t.get_call_return_type(id, node)
			if t.is_optional_type_name(ret_name) {
				return ret_name
			}
		}
		if typ := t.tc.expr_type(id) {
			typ_name := typ.name()
			if t.is_optional_type_name(typ_name) {
				return typ_name
			}
		}
	}
	return t.node_type(id)
}

// transform_if_guard_else_block transforms transform if guard else block data for transform.
fn (mut t Transformer) transform_if_guard_else_block(else_id flat.NodeId, else_node flat.Node, err_source string) flat.NodeId {
	saved_var_types := t.var_types.clone()
	t.set_var_type('err', 'IError')
	mut children := []flat.NodeId{}
	err_value := if err_source.len > 0 {
		t.make_selector(t.make_ident(err_source), 'err', 'IError')
	} else {
		t.make_struct_init('IError')
	}
	children << t.make_decl_assign_typed('err', err_value, 'IError')
	if else_node.kind == .block {
		children << t.transform_stmts(t.a.children_of(&else_node))
	} else if else_node.kind == .if_expr {
		children << t.transform_else_if_expr(else_id, else_node)
	} else {
		children << t.transform_stmt(else_id)
	}
	t.restore_var_types(saved_var_types)
	return t.make_block(children)
}

// transform_else_if_expr transforms transform else if expr data for transform.
fn (mut t Transformer) transform_else_if_expr(else_id flat.NodeId, else_node flat.Node) flat.NodeId {
	if expanded := t.try_expand_if_guard(else_id, else_node) {
		return t.make_block(expanded)
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_if := t.transform_if_branches_with_smartcast(else_id, else_node)
	mut local_pending := t.pending_stmts.clone()
	t.pending_stmts = outer_pending
	if local_pending.len == 0 {
		return new_if
	}
	local_pending << new_if
	return t.make_block(local_pending)
}

// expand_map_index_if_guard builds expand map index if guard data for transform.
fn (mut t Transformer) expand_map_index_if_guard(node flat.Node, lhs_name string, info MapIndexInfo) ?[]flat.NodeId {
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	ptr_name := t.new_temp('map_ptr')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	key_expr := t.transform_expr_for_type(info.key_id, info.key_type)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(key_name, key_expr, info.key_storage_type)
	prelude << t.make_decl_assign_typed(ptr_name, t.make_map_get_check_expr(map_expr,
		info.base_type, key_name), 'voidptr')

	ptr_ident := t.make_ident(ptr_name)
	found_cond := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))
	ptr_value := t.make_prefix(.mul, t.make_cast('&${info.value_type}', t.make_ident(ptr_name),
		'&${info.value_type}'))
	value_decl := t.make_decl_assign_typed(lhs_name, ptr_value, info.value_type)

	then_id := t.a.child(&node, 1)
	then_node := t.a.nodes[int(then_id)]
	t.set_var_type(lhs_name, info.value_type)
	mut then_children := []flat.NodeId{}
	then_children << value_decl
	if then_node.kind == .block {
		then_children << t.transform_stmts(t.a.children_of(&then_node))
	} else {
		then_children << t.transform_stmt(then_id)
	}
	then_block := t.make_block(then_children)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		else_block = if else_node.kind == .block {
			t.make_block(t.transform_stmts(t.a.children_of(&else_node)))
		} else if else_node.kind == .if_expr {
			t.transform_else_if_expr(else_id, else_node)
		} else {
			t.make_block(t.transform_stmt(else_id))
		}
	}
	t.pending_stmts = outer_pending
	mut expanded := []flat.NodeId{cap: prelude.len + 1}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << t.make_if(found_cond, then_block, else_block)
	return expanded
}

// expand_array_index_if_guard builds expand array index if guard data for transform.
fn (mut t Transformer) expand_array_index_if_guard(node flat.Node, lhs_name string, info ArrayIndexInfo) ?[]flat.NodeId {
	array_expr := t.stable_expr_for_reuse(info.base_id)
	index_name := t.new_temp('arr_idx')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	index_expr := t.transform_expr(info.index_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(index_name, index_expr, 'int')

	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name),
		t.array_index_len_expr(info, array_expr))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)

	then_id := t.a.child(&node, 1)
	then_node := t.a.nodes[int(then_id)]
	saved_var_types := t.var_types.clone()
	mut then_children := []flat.NodeId{}
	mut guard_value_type := info.value_type
	mut value_expr := t.make_index(array_expr, t.make_ident(index_name), info.value_type)
	if t.is_optional_type_name(info.value_type) {
		guard_value_type = t.optional_base_type(t.qualify_optional_type(info.value_type))
		opt_name := t.new_temp('arr_opt')
		value_expr = t.make_selector(t.make_ident(opt_name), 'value', guard_value_type)
		then_children << t.make_decl_assign_typed(opt_name, t.make_index(array_expr,
			t.make_ident(index_name), info.value_type), info.value_type)
	}
	then_children << t.make_decl_assign_typed(lhs_name, value_expr, guard_value_type)
	t.set_var_type(lhs_name, guard_value_type)
	if then_node.kind == .block {
		then_children << t.transform_stmts(t.a.children_of(&then_node))
	} else {
		then_children << t.transform_stmt(then_id)
	}
	t.restore_var_types(saved_var_types)
	then_block := t.make_block(then_children)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		else_block = if else_node.kind == .block {
			t.make_block(t.transform_stmts(t.a.children_of(&else_node)))
		} else if else_node.kind == .if_expr {
			t.transform_else_if_expr(else_id, else_node)
		} else {
			t.make_block(t.transform_stmt(else_id))
		}
	}
	mut selected_block := then_block
	if t.is_optional_type_name(info.value_type) && then_children.len > 0 {
		opt_decl := then_children[0]
		opt_name := t.a.nodes[int(t.a.child(&t.a.nodes[int(opt_decl)], 0))].value
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', 'bool')
		mut inner_children := []flat.NodeId{}
		for i in 1 .. then_children.len {
			inner_children << then_children[i]
		}
		inner_if := t.make_if(ok_cond, t.make_block(inner_children), else_block)
		selected_block = t.make_block([opt_decl, inner_if])
	}
	t.pending_stmts = outer_pending
	mut expanded := []flat.NodeId{cap: prelude.len + 1}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << t.make_if(found_cond, selected_block, else_block)
	return expanded
}

// try_expand_if_expr_value detects an if-expression used as a value and
// lowers it to a mutable temporary so that the C backend sees a simple
// variable instead of a gcc statement-expression.
//
//   x := if cond { a } else { b }
// becomes:
//   mut __if_tmp_N := zero_value
//   if cond { __if_tmp_N = a } else { __if_tmp_N = b }
//   x := __if_tmp_N
fn (mut t Transformer) try_expand_if_expr_value(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.kind != .if_expr {
		return none
	}
	// An if-expression used as a value must have both then and else branches.
	if node.children_count < 3 {
		return none
	}
	mut result_type := t.if_expr_result_type(id, node)
	if guard_result_type := t.if_expr_guard_result_type(node) {
		result_type = guard_result_type
	}
	if result_type.len == 0 || result_type == 'void' {
		return none
	}
	return t.try_expand_if_expr_value_for_type(id, node, result_type)
}

fn (mut t Transformer) if_expr_guard_result_type(node flat.Node) ?string {
	if node.kind != .if_expr || node.children_count < 3 {
		return none
	}
	cond_id := t.a.child(&node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	mut rhs_type := t.optional_result_expr_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type := t.optional_base_type(rhs_type)
	saved_var_types := t.var_types.clone()
	if lhs.value != '_' && value_type != 'void' {
		t.set_var_type(lhs.value, value_type)
	}
	then_type := t.stmt_value_type(t.a.child(&node, 1))
	t.restore_var_types(saved_var_types)

	t.set_var_type('err', 'IError')
	else_id := t.a.child(&node, 2)
	else_node := t.a.nodes[int(else_id)]
	else_type := if else_node.kind == .if_expr {
		if nested := t.if_expr_guard_result_type(else_node) {
			nested
		} else {
			t.if_expr_result_type(else_id, else_node)
		}
	} else {
		t.stmt_value_type(else_id)
	}
	t.restore_var_types(saved_var_types)

	mut result := ''
	if then_type.len > 0 {
		result = t.merge_if_expr_types(result, t.normalize_type_alias(then_type))
	}
	if else_type.len > 0 {
		result = t.merge_if_expr_types(result, t.normalize_type_alias(else_type))
	}
	if result.len == 0 || result == 'void' || result == 'unknown' {
		return none
	}
	return result
}

// try_expand_if_expr_value_for_type
// supports helper handling in transform.
fn (mut t Transformer) try_expand_if_expr_value_for_type(id flat.NodeId, node flat.Node, result_type string) ?flat.NodeId {
	if node.kind != .if_expr || node.children_count < 3 || result_type.len == 0
		|| result_type == 'void' {
		return none
	}
	mut actual_result_type := result_type
	branch_type := t.if_expr_branch_result_type(node)
	if t.if_expr_branch_overrides_sum_target(branch_type, result_type) {
		actual_result_type = branch_type
	}
	tmp_name := t.new_temp('if_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	prelude << t.make_decl_assign_typed(tmp_name, t.zero_value_for_type(actual_result_type),
		actual_result_type)
	for stmt in t.build_if_value_chain(id, tmp_name, actual_result_type) {
		prelude << stmt
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	tmp := t.make_ident(tmp_name)
	t.set_node_typ(int(tmp), actual_result_type)
	return tmp
}

// if_expr_branch_overrides_sum_target supports if_expr_branch_overrides_sum_target handling.
fn (t &Transformer) if_expr_branch_overrides_sum_target(branch_type string, target_type string) bool {
	if branch_type.len == 0 || target_type.len == 0 {
		return false
	}
	if branch_type.starts_with('[]') {
		branch_elem := branch_type[2..]
		target_short := if target_type.contains('.') {
			target_type.all_after_last('.')
		} else {
			target_type
		}
		branch_short := if branch_elem.contains('.') {
			branch_elem.all_after_last('.')
		} else {
			branch_elem
		}
		if target_short == branch_short {
			return true
		}
	}
	resolved_target := t.resolve_sum_name(target_type)
	if resolved_target.len == 0 || resolved_target !in t.sum_types {
		return false
	}
	clean_branch := t.trim_pointer_type(branch_type)
	if t.sum_target_accepts_variant_type(resolved_target, clean_branch) {
		return false
	}
	branch_sum := t.resolve_sum_name(clean_branch)
	variant_sum := t.resolve_sum_name(t.find_sum_type_for_variant(clean_branch))
	return branch_sum != resolved_target && variant_sum != resolved_target
}

// if_expr_result_type supports if expr result type handling for Transformer.
fn (t &Transformer) if_expr_result_type(id flat.NodeId, node flat.Node) string {
	mut node_typ := ''
	if node.typ.len > 0 {
		typ := t.normalize_type_alias(node.typ)
		if typ !in ['array', 'map'] {
			node_typ = typ
		}
	}
	mut checked_typ := ''
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 {
				checked_typ = t.normalize_type_alias(name)
			}
		}
	}
	branch_typ := t.if_expr_branch_result_type(node)
	if branch_typ.starts_with('[]') && t.is_fixed_array_type(checked_typ)
		&& fixed_array_elem_type(checked_typ) == branch_typ[2..] {
		return branch_typ
	}
	if branch_typ.starts_with('[]') && t.is_fixed_array_type(node_typ)
		&& fixed_array_elem_type(node_typ) == branch_typ[2..] {
		return branch_typ
	}
	if branch_typ.starts_with('[]') && !checked_typ.starts_with('[]') && !node_typ.starts_with('[]') {
		return branch_typ
	}
	if t.if_expr_branch_type_overrides(branch_typ, checked_typ) {
		return branch_typ
	}
	if t.if_expr_branch_type_overrides(branch_typ, node_typ) {
		return branch_typ
	}
	if checked_typ.len > 0 && checked_typ !in ['array', 'map'] {
		return checked_typ
	}
	if node_typ.len > 0 {
		return node_typ
	}
	if branch_typ.len > 0 {
		return branch_typ
	}
	return ''
}

// if_expr_branch_type_overrides supports if expr branch type overrides handling for Transformer.
fn (t &Transformer) if_expr_branch_type_overrides(branch_typ string, stale_typ string) bool {
	if branch_typ.len == 0 || stale_typ.len == 0 || branch_typ == stale_typ {
		return false
	}
	if stale_typ in ['array', 'map', 'unknown'] {
		return true
	}
	if stale_typ in t.enum_types && branch_typ == 'int' {
		return false
	}
	if stale_typ.starts_with('&') && branch_typ.starts_with('&') {
		stale_inner := stale_typ[1..]
		branch_inner := branch_typ[1..]
		if stale_inner in ['int', 'void', 'unknown'] && branch_inner !in ['int', 'void', 'unknown'] {
			return true
		}
	}
	if stale_typ.starts_with('[]') || t.is_fixed_array_type(stale_typ) {
		return !branch_typ.starts_with('[]') && !t.is_fixed_array_type(branch_typ)
	}
	if stale_typ.starts_with('map[') {
		return !branch_typ.starts_with('map[')
	}
	return false
}

// if_expr_branch_result_type supports if expr branch result type handling for Transformer.
fn (t &Transformer) if_expr_branch_result_type(node flat.Node) string {
	mut result := ''
	if node.children_count >= 2 {
		then_smartcasts :=
			t.smartcast_contexts_from_is_exprs(t.extract_all_is_exprs(t.a.child(&node, 0)))
		then_type := t.stmt_value_type_with_smartcasts(t.a.child(&node, 1), then_smartcasts)
		if then_type.len > 0 {
			result = t.merge_if_expr_types(result, t.normalize_type_alias(then_type))
		}
	}
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		else_type := if else_node.kind == .if_expr {
			t.if_expr_result_type(else_id, else_node)
		} else {
			t.stmt_value_type(else_id)
		}
		if else_type.len > 0 {
			result = t.merge_if_expr_types(result, t.normalize_type_alias(else_type))
		}
	}
	return result
}

// smartcast_contexts_from_is_exprs converts smartcast contexts from is exprs data for transform.
fn (t &Transformer) smartcast_contexts_from_is_exprs(infos []IsExprInfo) []SmartcastContext {
	mut result := []SmartcastContext{cap: infos.len}
	for info in infos {
		result << SmartcastContext{
			expr_name:     info.expr_name
			variant_name:  info.variant_name
			sum_type_name: info.sum_type_name
		}
	}
	return result
}

// stmt_value_type_with_smartcasts
// supports helper handling in transform.
fn (t &Transformer) stmt_value_type_with_smartcasts(id flat.NodeId, contexts []SmartcastContext) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return ''
		}
		.expr_stmt {
			if node.children_count > 0 {
				return t.node_type_with_smartcasts(t.a.child(&node, node.children_count - 1),
					contexts)
			}
			return ''
		}
		.block {
			for i := node.children_count - 1; i >= 0; i-- {
				typ := t.stmt_value_type_with_smartcasts(t.a.child(&node, i), contexts)
				if typ.len > 0 {
					return typ
				}
			}
			return ''
		}
		else {
			return t.node_type_with_smartcasts(id, contexts)
		}
	}
}

// node_type_with_smartcasts supports node type with smartcasts handling for Transformer.
fn (t &Transformer) node_type_with_smartcasts(id flat.NodeId, contexts []SmartcastContext) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if sc := t.find_smartcast_in_context(node.value, contexts) {
				return t.smartcast_target_type(sc)
			}
			return t.node_type(id)
		}
		.selector {
			if node.children_count == 0 {
				return t.node_type(id)
			}
			full_key := t.expr_key(id)
			if full_key.len > 0 {
				if sc := t.find_smartcast_in_context(full_key, contexts) {
					return t.smartcast_target_type(sc)
				}
			}
			base_id := t.a.child(&node, 0)
			base_key := t.expr_key(base_id)
			if base_key.len > 0 {
				if sc := t.find_smartcast_in_context(base_key, contexts) {
					variant_type := t.qualify_variant(sc.variant_name, sc.sum_type_name)
					if ftyp := t.lookup_struct_field_type(variant_type, node.value) {
						return ftyp
					}
					if ftyp := t.lookup_struct_field_type(sc.variant_name, node.value) {
						return ftyp
					}
				}
			}
			base_type := t.node_type_with_smartcasts(base_id, contexts)
			clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if ftyp := t.lookup_struct_field_type(clean_base, node.value) {
				return ftyp
			}
			return t.node_type(id)
		}
		.prefix {
			if node.children_count == 0 {
				return t.node_type(id)
			}
			child_type := t.node_type_with_smartcasts(t.a.child(&node, 0), contexts)
			if node.op == .amp && child_type.len > 0 {
				return '&${child_type}'
			}
			if node.op == .mul && child_type.starts_with('&') {
				return child_type[1..]
			}
			if node.op == .not {
				return 'bool'
			}
			return t.node_type(id)
		}
		.paren {
			if node.children_count > 0 {
				return t.node_type_with_smartcasts(t.a.child(&node, 0), contexts)
			}
			return t.node_type(id)
		}
		.array_literal {
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ != 'array' {
					return typ
				}
			}
			if node.children_count > 0 {
				elem_type := t.node_type_with_smartcasts(t.a.child(&node, 0), contexts)
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
			return t.node_type(id)
		}
		.index {
			base_type := if node.children_count > 0 {
				t.node_type_with_smartcasts(t.a.child(&node, 0), contexts)
			} else {
				''
			}
			if node.value == 'range' && base_type.starts_with('[]') {
				return base_type
			}
			if base_type.starts_with('[]') {
				return base_type[2..]
			}
			return t.node_type(id)
		}
		.if_expr {
			return t.if_expr_result_type(id, node)
		}
		.match_stmt {
			return t.match_expr_type(node)
		}
		else {
			return t.node_type(id)
		}
	}
}

// find_smartcast_in_context resolves find smartcast in context information for transform.
fn (t &Transformer) find_smartcast_in_context(expr_name string, contexts []SmartcastContext) ?SmartcastContext {
	mut i := contexts.len - 1
	for i >= 0 {
		if contexts[i].expr_name == expr_name {
			return contexts[i]
		}
		i--
	}
	return t.find_smartcast(expr_name)
}

// merge_if_expr_types supports merge if expr types handling for Transformer.
fn (t &Transformer) merge_if_expr_types(current string, next string) string {
	if current.len == 0 {
		return next
	}
	if next.len == 0 || current == next {
		return current
	}
	if current == 'array' && next.starts_with('[]') {
		return next
	}
	if next == 'array' && current.starts_with('[]') {
		return current
	}
	if current.starts_with('[]') && t.is_fixed_array_type(next)
		&& current[2..] == fixed_array_elem_type(next) {
		return current
	}
	if next.starts_with('[]') && t.is_fixed_array_type(current)
		&& next[2..] == fixed_array_elem_type(current) {
		return next
	}
	if current.starts_with('[]') && !next.starts_with('[]') && current[2..] == next {
		return current
	}
	if next.starts_with('[]') && !current.starts_with('[]') && next[2..] == current {
		return next
	}
	return current
}

// build_if_value_chain builds if value chain data for transform.
fn (mut t Transformer) build_if_value_chain(if_id flat.NodeId, target_name string, target_type string) []flat.NodeId {
	if_node := t.a.nodes[int(if_id)]
	if if_node.kind != .if_expr || if_node.children_count < 2 {
		return []flat.NodeId{}
	}
	if guard_chain := t.build_if_value_guard_chain(if_node, target_name, target_type) {
		return guard_chain
	}
	cond_id := t.a.child(&if_node, 0)
	then_id := t.a.child(&if_node, 1)
	has_else := if_node.children_count >= 3

	all_is := t.extract_all_is_exprs(cond_id)
	new_cond := t.transform_and_chain_smartcasts(cond_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)

	for info in all_is {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	then_block := t.if_value_branch_block(then_id, target_name, target_type)
	for _ in all_is {
		t.pop_smartcast()
	}

	mut else_block := flat.empty_node
	if has_else {
		else_id := t.a.child(&if_node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			else_block = t.make_block(t.build_if_value_chain(else_id, target_name, target_type))
		} else {
			else_block = t.if_value_branch_block(else_id, target_name, target_type)
		}
	}
	result << t.make_if(new_cond, then_block, else_block)
	return result
}

// build_if_value_guard_chain builds if value guard chain data for transform.
fn (mut t Transformer) build_if_value_guard_chain(if_node flat.Node, target_name string, target_type string) ?[]flat.NodeId {
	if if_node.children_count < 3 {
		return none
	}
	cond_id := t.a.child(&if_node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	if info := t.map_index_info(rhs_id) {
		return t.build_map_index_if_value_guard_chain(if_node, lhs.value, info, target_name,
			target_type)
	}
	if info := t.array_index_info(rhs_id) {
		return t.build_array_index_if_value_guard_chain(if_node, lhs.value, info, target_name,
			target_type)
	}
	mut rhs_type := t.optional_result_expr_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type := t.optional_base_type(rhs_type)
	tmp_name := t.new_temp('if_guard')
	rhs_expr := t.transform_expr(rhs_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(tmp_name, rhs_expr, rhs_type)
	ok_cond := t.make_selector(t.make_ident(tmp_name), 'ok', 'bool')
	value_decl := t.make_decl_assign_typed(lhs.value, t.make_selector(t.make_ident(tmp_name),
		'value', value_type), value_type)

	saved_var_types := t.var_types.clone()
	t.set_var_type(lhs.value, value_type)
	then_id := t.a.child(&if_node, 1)
	then_block0 := t.if_value_branch_block(then_id, target_name, target_type)
	mut then_children := []flat.NodeId{cap: int(t.a.nodes[int(then_block0)].children_count) + 1}
	then_children << value_decl
	then_children << t.a.children_of(&t.a.nodes[int(then_block0)])
	then_block := t.make_block(then_children)
	t.restore_var_types(saved_var_types)

	else_id := t.a.child(&if_node, 2)
	else_node := t.a.nodes[int(else_id)]
	err_value := t.make_selector(t.make_ident(tmp_name), 'err', 'IError')
	err_decl := t.make_decl_assign_typed('err', err_value, 'IError')
	saved_else_var_types := t.var_types.clone()
	t.set_var_type('err', 'IError')
	else_block0 := if else_node.kind == .if_expr {
		t.make_block(t.build_if_value_chain(else_id, target_name, target_type))
	} else {
		t.if_value_branch_block(else_id, target_name, target_type)
	}
	t.restore_var_types(saved_else_var_types)
	mut else_children := []flat.NodeId{cap: int(t.a.nodes[int(else_block0)].children_count) + 1}
	else_children << err_decl
	else_children << t.a.children_of(&t.a.nodes[int(else_block0)])
	else_block := t.make_block(else_children)
	result << t.make_if(ok_cond, then_block, else_block)
	return result
}

// build_map_index_if_value_guard_chain supports build_map_index_if_value_guard_chain handling.
fn (mut t Transformer) build_map_index_if_value_guard_chain(if_node flat.Node, lhs_name string, info MapIndexInfo, target_name string, target_type string) []flat.NodeId {
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	ptr_name := t.new_temp('map_ptr')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	key_expr := t.transform_expr_for_type(info.key_id, info.key_type)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, key_expr, info.key_storage_type)
	result << t.make_decl_assign_typed(ptr_name, t.make_map_get_check_expr(map_expr,
		info.base_type, key_name), 'voidptr')
	ptr_ident := t.make_ident(ptr_name)
	found_cond := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))

	saved_var_types := t.var_types.clone()
	mut then_children := []flat.NodeId{}
	if lhs_name != '_' {
		ptr_value := t.make_prefix(.mul, t.make_cast('&${info.value_type}', t.make_ident(ptr_name),
			'&${info.value_type}'))
		then_children << t.make_decl_assign_typed(lhs_name, ptr_value, info.value_type)
		t.set_var_type(lhs_name, info.value_type)
	}
	then_id := t.a.child(&if_node, 1)
	then_block0 := t.if_value_branch_block(then_id, target_name, target_type)
	then_children << t.a.children_of(&t.a.nodes[int(then_block0)])
	then_block := t.make_block(then_children)
	t.restore_var_types(saved_var_types)

	else_id := t.a.child(&if_node, 2)
	else_node := t.a.nodes[int(else_id)]
	else_block := if else_node.kind == .if_expr {
		t.make_block(t.build_if_value_chain(else_id, target_name, target_type))
	} else {
		t.if_value_branch_block(else_id, target_name, target_type)
	}
	t.pending_stmts = outer_pending
	result << t.make_if(found_cond, then_block, else_block)
	return result
}

// build_array_index_if_value_guard_chain supports build_array_index_if_value_guard_chain handling.
fn (mut t Transformer) build_array_index_if_value_guard_chain(if_node flat.Node, lhs_name string, info ArrayIndexInfo, target_name string, target_type string) []flat.NodeId {
	array_expr := t.stable_expr_for_reuse(info.base_id)
	index_name := t.new_temp('arr_idx')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	index_expr := t.transform_expr(info.index_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(index_name, index_expr, 'int')
	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name),
		t.array_index_len_expr(info, array_expr))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)

	saved_var_types := t.var_types.clone()
	mut then_children := []flat.NodeId{}
	mut opt_decl := flat.empty_node
	mut opt_name := ''
	if lhs_name != '_' {
		mut value_type := info.value_type
		mut value := t.make_index(array_expr, t.make_ident(index_name), info.value_type)
		if t.is_optional_type_name(info.value_type) {
			value_type = t.optional_base_type(t.qualify_optional_type(info.value_type))
			opt_name = t.new_temp('arr_opt')
			opt_decl = t.make_decl_assign_typed(opt_name, t.make_index(array_expr,
				t.make_ident(index_name), info.value_type), info.value_type)
			value = t.make_selector(t.make_ident(opt_name), 'value', value_type)
		}
		then_children << t.make_decl_assign_typed(lhs_name, value, value_type)
		t.set_var_type(lhs_name, value_type)
	} else if t.is_optional_type_name(info.value_type) {
		opt_name = t.new_temp('arr_opt')
		opt_decl = t.make_decl_assign_typed(opt_name, t.make_index(array_expr,
			t.make_ident(index_name), info.value_type), info.value_type)
	}
	then_id := t.a.child(&if_node, 1)
	then_block0 := t.if_value_branch_block(then_id, target_name, target_type)
	then_children << t.a.children_of(&t.a.nodes[int(then_block0)])
	then_block := t.make_block(then_children)
	t.restore_var_types(saved_var_types)

	else_id := t.a.child(&if_node, 2)
	else_node := t.a.nodes[int(else_id)]
	else_block := if else_node.kind == .if_expr {
		t.make_block(t.build_if_value_chain(else_id, target_name, target_type))
	} else {
		t.if_value_branch_block(else_id, target_name, target_type)
	}
	t.pending_stmts = outer_pending
	mut selected_block := then_block
	if t.is_optional_type_name(info.value_type) && opt_name.len > 0 {
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', 'bool')
		inner_if := t.make_if(ok_cond, then_block, else_block)
		selected_block = t.make_block([opt_decl, inner_if])
	}
	result << t.make_if(found_cond, selected_block, else_block)
	return result
}

// if_value_branch_block supports if value branch block handling for Transformer.
fn (mut t Transformer) if_value_branch_block(branch_id flat.NodeId, target_name string, target_type string) flat.NodeId {
	if int(branch_id) < 0 {
		return t.make_block([]flat.NodeId{})
	}
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .if_expr {
		return t.make_block(t.build_if_value_chain(branch_id, target_name, target_type))
	}
	if branch.kind != .block {
		mut result := []flat.NodeId{}
		value := t.transform_if_branch_value(branch_id, target_type)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if branch.children_count == 0 {
		return t.make_block([]flat.NodeId{})
	}

	mut stmt_ids := []flat.NodeId{cap: int(branch.children_count)}
	for i in 0 .. branch.children_count {
		stmt_ids << t.a.child(&branch, i)
	}
	mut result := []flat.NodeId{}
	if stmt_ids.len > 1 {
		for stmt in t.transform_stmts(stmt_ids[..stmt_ids.len - 1]) {
			result << stmt
		}
	}

	tail_id := stmt_ids[stmt_ids.len - 1]
	tail := t.a.nodes[int(tail_id)]
	if tail.kind == .return_stmt {
		tail_stmts := t.transform_stmt(tail_id)
		t.drain_pending(mut result)
		for stmt in tail_stmts {
			result << stmt
		}
		return t.make_block(result)
	}
	if tail.kind == .expr_stmt && tail.children_count > 0 {
		inner_id := t.a.child(&tail, 0)
		inner := t.a.nodes[int(inner_id)]
		if inner.kind == .call && t.is_noreturn_call(inner_id) {
			tail_stmts := t.transform_stmt(tail_id)
			t.drain_pending(mut result)
			for stmt in tail_stmts {
				result << stmt
			}
			return t.make_block(result)
		}
		if t.node_type(inner_id) == 'void' {
			tail_stmts := t.transform_stmt(tail_id)
			t.drain_pending(mut result)
			for stmt in tail_stmts {
				result << stmt
			}
			return t.make_block(result)
		}
		value := t.transform_if_branch_value(inner_id, target_type)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if tail.kind == .block && t.stmt_value_type(tail_id).len > 0 {
		value := t.transform_if_branch_value(tail_id, target_type)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if tail.kind == .match_stmt {
		value := t.transform_if_branch_value(tail_id, target_type)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if tail.kind == .if_expr {
		value := t.transform_if_branch_value(tail_id, target_type)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if t.is_stmt_kind(tail.kind) {
		tail_stmts := t.transform_stmt(tail_id)
		t.drain_pending(mut result)
		for stmt in tail_stmts {
			result << stmt
		}
		return t.make_block(result)
	}
	value := t.transform_if_branch_value(tail_id, target_type)
	t.drain_pending(mut result)
	result << t.make_assign(t.make_ident(target_name), value)
	return t.make_block(result)
}

// transform_if_branch_value transforms transform if branch value data for transform.
fn (mut t Transformer) transform_if_branch_value(id flat.NodeId, target_type string) flat.NodeId {
	if t.is_sum_type_name(target_type) {
		return t.wrap_sum_value(id, target_type)
	}
	if converted := t.fixed_array_value_to_dynamic(id, target_type) {
		return converted
	}
	return t.transform_expr_for_type(id, target_type)
}

// transform_is_condition transforms an `x is Type` condition node into the
// common sum-type tag comparison used by all C emission paths.
fn (mut t Transformer) transform_is_condition(cond_id flat.NodeId) flat.NodeId {
	if int(cond_id) < 0 {
		return cond_id
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .is_expr {
		return cond_id
	}
	// is_expr: child[0] is the expression being checked, value is the type name.
	if cond.children_count < 1 {
		return cond_id
	}
	variant_name := cond.value
	if variant_name.len == 0 {
		return cond_id
	}
	return t.transform_is_expr(cond_id, cond)
}

// transform_and_chain_smartcasts handles conditions like
// `x is T && x.field > 0` where the second operand should see x
// narrowed to T through a smartcast.
//
// It walks a left-associative chain of .logical_and nodes. When a
// term is an is_expr it pushes a smartcast so subsequent terms
// (and the then-branch) see the narrowed type.
//
// Currently passes through unchanged but is structured to detect
// the pattern for future expansion.
fn (mut t Transformer) transform_and_chain_smartcasts(cond_id flat.NodeId) flat.NodeId {
	if int(cond_id) < 0 {
		return cond_id
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .decl_assign {
		return t.transform_if_guard_condition(cond)
	}
	if cond.kind != .infix || cond.op !in [.logical_and, .logical_or] {
		// Not an && chain -- preserve bare smartcast checks and fully transform
		// ordinary conditions.
		if cond.kind == .is_expr {
			return t.transform_is_condition(cond_id)
		}
		return t.transform_expr_for_type(cond_id, 'bool')
	}
	if cond.children_count < 2 {
		return cond_id
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	new_lhs := t.transform_and_chain_smartcasts(lhs_id)
	lhs_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	lhs_smartcasts := if cond.op == .logical_and {
		t.extract_all_is_exprs(lhs_id)
	} else {
		[]IsExprInfo{}
	}
	for info in lhs_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	mut new_rhs := t.transform_and_chain_smartcasts(rhs_id)
	rhs_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	for _ in lhs_smartcasts {
		t.pop_smartcast()
	}
	// The left operand always evaluates, so its pending statements may run before
	// the `&&`/`||`. The right operand is conditional, so any statements its
	// lowering produced (e.g. an `in [...]` / `or {}` temporary) must run only when
	// the right operand is actually reached — wrap them in a `({ ...; value; })`
	// statement-expression to preserve short-circuit evaluation.
	for stmt in lhs_pending {
		t.pending_stmts << stmt
	}
	if rhs_pending.len > 0 {
		mut block_stmts := rhs_pending.clone()
		block_stmts << t.make_expr_stmt(new_rhs)
		new_rhs = t.make_block(block_stmts)
		t.set_node_typ(int(new_rhs), 'bool')
	}
	if lhs.kind == .is_expr && new_lhs == lhs_id && new_rhs == rhs_id {
		return cond_id
	}
	return t.make_infix(cond.op, new_lhs, new_rhs)
}

// transform_if_guard_condition transforms transform if guard condition data for transform.
fn (mut t Transformer) transform_if_guard_condition(node flat.Node) flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 2 {
		return flat.empty_node
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 1 {
			new_children << t.transform_expr(child_id)
		} else {
			new_children << t.transform_lvalue(child_id)
		}
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           .decl_assign
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_if_branch_as_block(branch_id flat.NodeId) flat.NodeId {
	if int(branch_id) < 0 {
		return t.make_block([]flat.NodeId{})
	}
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .block {
		return t.make_block(t.transform_stmts(t.a.children_of(&branch)))
	}
	mut stmts := []flat.NodeId{}
	if t.is_stmt_kind_id(node_kind_id(branch)) {
		expanded := t.transform_stmt(branch_id)
		t.drain_pending(mut stmts)
		stmts << expanded
		return t.make_block(stmts)
	}
	expr := t.transform_expr(branch_id)
	t.drain_pending(mut stmts)
	if int(expr) >= 0 {
		stmts << t.make_expr_stmt(expr)
	}
	return t.make_block(stmts)
}

fn (mut t Transformer) transform_plain_if_branches(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .if_expr || node.children_count < 2 {
		return id
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	has_else := node.children_count >= 3
	else_id := if has_else { t.a.child(&node, 2) } else { flat.empty_node }

	new_cond_id := t.transform_expr_for_type(cond_id, 'bool')
	cond_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut new_then_id := flat.empty_node
	mut new_else_id := flat.empty_node
	if t.smartcast_stack.len == 0 {
		new_then_id = t.transform_if_branch_as_block(then_id)
		if has_else {
			else_node := t.a.nodes[int(else_id)]
			new_else_id = if else_node.kind == .if_expr {
				t.transform_else_if_expr(else_id, else_node)
			} else {
				t.transform_if_branch_as_block(else_id)
			}
		}
	} else {
		base_smartcasts := t.smartcast_stack.clone()
		base_invalidated := t.invalidated_smartcasts.clone()
		new_then_id = t.transform_if_branch_as_block(then_id)
		mut then_invalidated := t.invalidated_smartcasts.clone()
		t.smartcast_stack = base_smartcasts.clone()
		t.invalidated_smartcasts = base_invalidated.clone()
		if has_else {
			else_node := t.a.nodes[int(else_id)]
			new_else_id = if else_node.kind == .if_expr {
				t.transform_else_if_expr(else_id, else_node)
			} else {
				t.transform_if_branch_as_block(else_id)
			}
		}
		else_invalidated := t.invalidated_smartcasts.clone()
		for key, value in else_invalidated {
			if value {
				then_invalidated[key] = true
			}
		}
		t.invalidated_smartcasts = then_invalidated.move()
		t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
	}

	if_start := t.a.children.len
	t.a.children << new_cond_id
	t.a.children << new_then_id
	mut child_count := 2
	if has_else {
		t.a.children << new_else_id
		child_count = 3
	}
	new_if := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(child_count)
		typ:            node.typ
		pos:            node.pos
	})
	for pending in cond_pending {
		t.pending_stmts << pending
	}
	return new_if
}

// transform_if_branches_with_smartcast is the main if-expr handler that
// integrates smartcasting. When the condition contains an is_expr, it
// pushes a smartcast for the then-branch, transforms the body under
// that context, pops the smartcast, then transforms the else-block.
fn (mut t Transformer) transform_if_branches_with_smartcast(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .if_expr || node.children_count < 2 {
		return id
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	has_else := node.children_count >= 3
	else_id := if has_else { t.a.child(&node, 2) } else { flat.empty_node }

	all_is := t.extract_all_is_exprs(cond_id)
	all_none_eq := t.extract_none_eq_exprs(cond_id)
	cond_node := t.a.nodes[int(cond_id)]
	if all_is.len == 0 && all_none_eq.len == 0 && cond_node.kind != .decl_assign {
		return t.transform_plain_if_branches(id, node)
	}
	new_cond_id := t.transform_and_chain_smartcasts(cond_id)
	cond_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	// Transform then-block children under the smartcast context.
	saved_var_types := t.var_types.clone()
	then_base_smartcasts := t.smartcast_stack.clone()
	base_invalidated := t.invalidated_smartcasts.clone()
	for info in all_is {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	new_then_id := t.transform_if_branch_as_block(then_id)
	mut then_invalidated := t.invalidated_smartcasts.clone()
	t.smartcast_stack = then_base_smartcasts.clone()
	t.invalidated_smartcasts = base_invalidated.clone()
	t.restore_var_types(saved_var_types)

	// Transform else-block (no is-smartcast -- the is_expr was false here; a
	// `x == none` condition unwraps x in the else branch instead).
	mut new_else_id := flat.empty_node
	if has_else {
		else_base_smartcasts := t.smartcast_stack.clone()
		for info in all_none_eq {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			// else-if chain: recurse.
			new_else_id = t.transform_else_if_expr(else_id, else_node)
		} else {
			new_else_id = t.transform_if_branch_as_block(else_id)
		}
		t.smartcast_stack = else_base_smartcasts
		t.restore_var_types(saved_var_types)
	}
	else_invalidated := t.invalidated_smartcasts.clone()
	for key, value in else_invalidated {
		if value {
			then_invalidated[key] = true
		}
	}
	t.invalidated_smartcasts = then_invalidated.move()
	t.smartcast_stack = t.non_invalidated_smartcasts(then_base_smartcasts)

	// Rebuild the if_expr with (possibly) new children.
	if_start := t.a.children.len
	t.a.children << new_cond_id
	t.a.children << new_then_id
	mut child_count := 2
	if has_else {
		t.a.children << new_else_id
		child_count = 3
	}
	new_if := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(child_count)
		typ:            node.typ
		pos:            node.pos
	})
	for pending in cond_pending {
		t.pending_stmts << pending
	}
	return new_if
}

fn (t &Transformer) post_if_exit_smartcasts(id flat.NodeId) []IsExprInfo {
	if int(id) < 0 {
		return []IsExprInfo{}
	}
	node := t.a.nodes[int(id)]
	if node.kind != .if_expr || node.children_count < 2 {
		return []IsExprInfo{}
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	if info := t.negated_is_expr_info(cond_id) {
		if t.stmt_definitely_exits(then_id) {
			return [info]
		}
	}
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		if t.stmt_definitely_exits(else_id) {
			return t.extract_all_is_exprs(cond_id)
		}
	}
	return []IsExprInfo{}
}

fn (t &Transformer) negated_is_expr_info(cond_id flat.NodeId) ?IsExprInfo {
	if int(cond_id) < 0 {
		return none
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .prefix || cond.op != .not || cond.children_count == 0 {
		return none
	}
	inner_id := t.a.child(&cond, 0)
	if int(inner_id) < 0 {
		return none
	}
	inner := t.a.nodes[int(inner_id)]
	if inner.kind != .is_expr {
		return none
	}
	info := t.extract_is_expr(inner_id)
	if info.expr_name.len == 0 || info.sum_type_name.len == 0 || info.variant_name.len == 0 {
		return none
	}
	return info
}

fn (t &Transformer) stmt_definitely_exits(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return true
		}
		.block {
			for i in 0 .. node.children_count {
				if t.stmt_definitely_exits(t.a.child(&node, i)) {
					return true
				}
			}
			return false
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return t.stmt_definitely_exits(t.a.child(&node, 1))
				&& t.stmt_definitely_exits(t.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			mut has_else := false
			for i in 1 .. node.children_count {
				branch := t.a.child_node(&node, i)
				if branch.kind != .match_branch {
					return false
				}
				if branch.value == 'else' {
					has_else = true
				}
				body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
				mut branch_exits := false
				for j in body_start .. branch.children_count {
					if t.stmt_definitely_exits(t.a.child(branch, j)) {
						branch_exits = true
						break
					}
				}
				if !branch_exits {
					return false
				}
			}
			return has_else
		}
		else {
			return false
		}
	}
}

// --- helpers ---

// IsExprInfo stores is expr info metadata used by transform.
struct IsExprInfo {
	expr_name     string
	variant_name  string
	sum_type_name string
}

// extract_all_is_exprs supports extract all is exprs handling for Transformer.
fn (t &Transformer) extract_all_is_exprs(cond_id flat.NodeId) []IsExprInfo {
	mut result := []IsExprInfo{}
	t.collect_is_exprs(cond_id, mut result)
	return result
}

// collect_is_exprs updates collect is exprs state for transform.
fn (t &Transformer) collect_is_exprs(cond_id flat.NodeId, mut result []IsExprInfo) {
	if int(cond_id) < 0 {
		return
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count >= 1 {
		expr_id := t.a.child(&cond, 0)
		ek := t.expr_key(expr_id)
		if ek.len > 0 && cond.value.len > 0 {
			expr_type := t.original_expr_type(expr_id)
			stn := t.sum_type_for_is_expr(expr_type, cond.value)
			if stn.len > 0 {
				result << IsExprInfo{
					expr_name:     ek
					variant_name:  cond.value
					sum_type_name: stn
				}
			} else {
				if t.is_interface_type_name(expr_type) {
					result << IsExprInfo{
						expr_name:     ek
						variant_name:  cond.value
						sum_type_name: expr_type
					}
				}
			}
		}
	}
	// `x != none` (or `x != nil` for `?&T`) unwraps the option expr inside the
	// then-branch: record an option-unwrap context resolving x to its base type.
	if cond.kind == .infix && cond.op == .ne && cond.children_count >= 2 {
		if info := t.option_none_cmp_info(cond) {
			result << info
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		t.collect_is_exprs(t.a.child(&cond, 0), mut result)
		t.collect_is_exprs(t.a.child(&cond, 1), mut result)
	}
}

// option_none_cmp_info matches a `x != none` / `x == none` (also `nil`)
// comparison where x is an option and returns the option-unwrap context.
fn (t &Transformer) option_none_cmp_info(cond flat.Node) ?IsExprInfo {
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	rhs := t.a.nodes[int(rhs_id)]
	mut opt_id := flat.empty_node
	if rhs.kind == .none_expr || rhs.kind == .nil_literal {
		opt_id = lhs_id
	} else if lhs.kind == .none_expr || lhs.kind == .nil_literal {
		opt_id = rhs_id
	}
	if int(opt_id) < 0 {
		return none
	}
	ek := t.expr_key(opt_id)
	if ek.len == 0 {
		return none
	}
	expr_type := t.original_expr_type(opt_id)
	if !t.is_optional_type_name(expr_type) {
		return none
	}
	base := t.optional_base_type(expr_type)
	if base.len == 0 || base == 'void' {
		return none
	}
	return IsExprInfo{
		expr_name:     ek
		variant_name:  base
		sum_type_name: option_unwrap_marker
	}
}

// extract_none_eq_exprs returns option-unwrap contexts that apply to the ELSE
// branch of an if: `if x == none { ... } else { <x unwrapped here> }`.
fn (t &Transformer) extract_none_eq_exprs(cond_id flat.NodeId) []IsExprInfo {
	if int(cond_id) < 0 {
		return []IsExprInfo{}
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .infix && cond.op == .eq && cond.children_count >= 2 {
		if info := t.option_none_cmp_info(cond) {
			return [info]
		}
	}
	return []IsExprInfo{}
}

// extract_is_expr supports extract is expr handling for Transformer.
fn (t &Transformer) extract_is_expr(cond_id flat.NodeId) IsExprInfo {
	if int(cond_id) < 0 {
		return IsExprInfo{}
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count >= 1 {
		expr_id := t.a.child(&cond, 0)
		ek := t.expr_key(expr_id)
		if ek.len > 0 && cond.value.len > 0 {
			expr_type := t.original_expr_type(expr_id)
			return IsExprInfo{
				expr_name:     ek
				variant_name:  cond.value
				sum_type_name: t.sum_type_for_is_expr(expr_type, cond.value)
			}
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		// Check left side first (is_expr is typically on the left of &&).
		lhs_id := t.a.child(&cond, 0)
		info := t.extract_is_expr(lhs_id)
		if info.expr_name.len > 0 {
			return info
		}
		// Check right side as fallback.
		rhs_id := t.a.child(&cond, 1)
		return t.extract_is_expr(rhs_id)
	}
	return IsExprInfo{}
}

// sum_type_for_is_expr supports sum type for is expr handling for Transformer.
fn (t &Transformer) sum_type_for_is_expr(expr_type string, variant string) string {
	clean_expr_type := t.trim_pointer_type(expr_type)
	if _ := t.resolve_sum_variant_pattern_for_subject(clean_expr_type, variant) {
		return clean_expr_type
	}
	resolved_expr_sum := t.resolve_sum_name(clean_expr_type)
	if resolved_expr_sum in t.sum_types {
		if _ := t.sum_variant_name(resolved_expr_sum, variant) {
			return clean_expr_type
		}
	}
	return t.find_sum_type_for_variant(variant)
}

// find_sum_type_for_variant returns the sum type name that contains
// the given variant, or '' if none is found.
fn (t &Transformer) find_sum_type_for_variant(variant string) string {
	mut best := ''
	for sum_name, variants in t.sum_types {
		for v in variants {
			if t.variant_names_match(v, variant) {
				if sum_name.contains('.') {
					return sum_name
				}
				if best.len == 0 {
					best = sum_name
				}
			}
		}
	}
	return best
}
