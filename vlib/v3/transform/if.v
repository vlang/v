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
	mut rhs_type := t.node_type(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type := t.optional_base_type(rhs_type)
	tmp_name := t.new_temp('if_guard')
	tmp_decl := t.make_decl_assign_typed(tmp_name, t.transform_expr(rhs_id), rhs_type)
	ok_cond := t.make_selector(t.make_ident(tmp_name), 'ok', 'bool')
	value_decl := t.make_decl_assign_typed(lhs.value, t.make_selector(t.make_ident(tmp_name),
		'value', value_type), value_type)

	then_id := t.a.child(&node, 1)
	then_node := t.a.nodes[int(then_id)]
	t.set_var_type(lhs.value, value_type)
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
			t.transform_if_branches_with_smartcast(else_id, else_node)
		} else {
			t.make_block(t.transform_stmt(else_id))
		}
	}
	mut expanded := []flat.NodeId{cap: 2}
	expanded << tmp_decl
	expanded << t.make_if(ok_cond, then_block, else_block)
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
	if result_type.len == 0 || result_type == 'void' {
		return none
	}

	tmp_name := t.new_temp('if_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	prelude << t.make_decl_assign_typed(tmp_name, t.zero_value_for_type(result_type), result_type)
	for stmt in t.build_if_value_chain(id, tmp_name, result_type) {
		prelude << stmt
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	tmp := t.make_ident(tmp_name)
	t.a.nodes[int(tmp)].typ = result_type
	return tmp
}

fn (t &Transformer) if_expr_result_type(id flat.NodeId, node flat.Node) string {
	mut result_type := t.node_type(id)
	if result_type.len > 0 {
		return result_type
	}
	if node.typ.len > 0 {
		return t.normalize_type_alias(node.typ)
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 {
				return t.normalize_type_alias(name)
			}
		}
	}
	if node.children_count >= 2 {
		then_type := t.stmt_value_type(t.a.child(&node, 1))
		if then_type.len > 0 {
			return t.normalize_type_alias(then_type)
		}
	}
	if node.children_count >= 3 {
		else_type := t.stmt_value_type(t.a.child(&node, 2))
		if else_type.len > 0 {
			return t.normalize_type_alias(else_type)
		}
	}
	return ''
}

fn (mut t Transformer) build_if_value_chain(if_id flat.NodeId, target_name string, target_type string) []flat.NodeId {
	if_node := t.a.nodes[int(if_id)]
	if if_node.kind != .if_expr || if_node.children_count < 2 {
		return []flat.NodeId{}
	}
	cond_id := t.a.child(&if_node, 0)
	then_id := t.a.child(&if_node, 1)
	has_else := if_node.children_count >= 3

	all_is := t.extract_all_is_exprs(cond_id)
	new_cond := t.transform_expr(cond_id)
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
		value := t.transform_expr(branch_id)
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
		for stmt in t.transform_stmt(tail_id) {
			result << stmt
		}
		t.drain_pending(mut result)
		return t.make_block(result)
	}
	if tail.kind == .expr_stmt && tail.children_count > 0 {
		inner_id := t.a.child(&tail, 0)
		inner := t.a.nodes[int(inner_id)]
		if inner.kind == .call && t.is_noreturn_call(inner) {
			for stmt in t.transform_stmt(tail_id) {
				result << stmt
			}
			t.drain_pending(mut result)
			return t.make_block(result)
		}
		if t.node_type(inner_id) == 'void' {
			for stmt in t.transform_stmt(tail_id) {
				result << stmt
			}
			t.drain_pending(mut result)
			return t.make_block(result)
		}
		value := t.transform_expr(inner_id)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if t.is_stmt_kind(tail.kind) {
		for stmt in t.transform_stmt(tail_id) {
			result << stmt
		}
		t.drain_pending(mut result)
		return t.make_block(result)
	}
	value := t.transform_expr(tail_id)
	t.drain_pending(mut result)
	result << t.make_assign(t.make_ident(target_name), value)
	_ = target_type
	return t.make_block(result)
}

// transform_is_condition transforms an `x is Type` condition node. For sum
// types this will eventually become a tag comparison; for now the C gen already
// handles is_expr directly, so we pass through unchanged.
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
	// The node itself passes through -- C gen handles is_expr.
	return cond_id
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
	if cond.kind != .infix || cond.op != .logical_and {
		// Not an && chain -- preserve bare smartcast checks and fully transform
		// ordinary conditions.
		if cond.kind == .is_expr {
			return t.transform_is_condition(cond_id)
		}
		return t.transform_expr(cond_id)
	}
	if cond.children_count < 2 {
		return cond_id
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	new_lhs := t.transform_and_chain_smartcasts(lhs_id)
	lhs_smartcasts := t.extract_all_is_exprs(lhs_id)
	for info in lhs_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	new_rhs := t.transform_expr(rhs_id)
	for _ in lhs_smartcasts {
		t.pop_smartcast()
	}
	if lhs.kind == .is_expr && new_lhs == lhs_id && new_rhs == rhs_id {
		return cond_id
	}
	return t.make_infix(.logical_and, new_lhs, new_rhs)
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
	new_cond_id := t.transform_and_chain_smartcasts(cond_id)
	cond_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	cond := t.a.nodes[int(cond_id)]
	direct_ident_is := if cond.kind == .is_expr && cond.children_count > 0 {
		expr := t.a.child_node(&cond, 0)
		expr.kind == .ident
	} else {
		false
	}

	// Transform then-block children under the smartcast context.
	saved_var_types := t.var_types.clone()
	if !direct_ident_is {
		for info in all_is {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
	} else {
		for info in all_is {
			t.set_var_type(info.expr_name, t.resolve_variant(info.sum_type_name, info.variant_name))
		}
	}
	then_node := t.a.nodes[int(then_id)]
	mut new_then_id := then_id
	if then_node.kind == .block {
		child_ids := t.a.children_of(&then_node)
		new_children := t.transform_stmts(child_ids)
		block_start := t.a.children.len
		for c in new_children {
			t.a.children << c
		}
		new_then_id = t.a.add_node(flat.Node{
			kind:           .block
			children_start: block_start
			children_count: flat.child_count(new_children.len)
		})
	}

	if !direct_ident_is {
		for _ in all_is {
			t.pop_smartcast()
		}
	}
	t.var_types = saved_var_types

	// Transform else-block (no smartcast -- the is_expr was false here).
	mut new_else_id := flat.empty_node
	if has_else {
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			// else-if chain: recurse.
			new_else_id = t.transform_if_branches_with_smartcast(else_id, else_node)
		} else if else_node.kind == .block {
			child_ids := t.a.children_of(&else_node)
			new_children := t.transform_stmts(child_ids)
			block_start := t.a.children.len
			for c in new_children {
				t.a.children << c
			}
			new_else_id = t.a.add_node(flat.Node{
				kind:           .block
				children_start: block_start
				children_count: flat.child_count(new_children.len)
			})
		} else {
			new_else_id = else_id
		}
	}

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

// --- helpers ---

struct IsExprInfo {
	expr_name     string
	variant_name  string
	sum_type_name string
}

fn (t &Transformer) extract_all_is_exprs(cond_id flat.NodeId) []IsExprInfo {
	mut result := []IsExprInfo{}
	t.collect_is_exprs(cond_id, mut result)
	return result
}

fn (t &Transformer) collect_is_exprs(cond_id flat.NodeId, mut result []IsExprInfo) {
	if int(cond_id) < 0 {
		return
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count >= 1 {
		expr_id := t.a.child(&cond, 0)
		ek := t.expr_key(expr_id)
		if ek.len > 0 && cond.value.len > 0 {
			stn := t.find_sum_type_for_variant(cond.value)
			if stn.len > 0 {
				result << IsExprInfo{
					expr_name:     ek
					variant_name:  cond.value
					sum_type_name: stn
				}
			}
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		t.collect_is_exprs(t.a.child(&cond, 0), mut result)
		t.collect_is_exprs(t.a.child(&cond, 1), mut result)
	}
}

fn (t &Transformer) extract_is_expr(cond_id flat.NodeId) IsExprInfo {
	if int(cond_id) < 0 {
		return IsExprInfo{}
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count >= 1 {
		expr_id := t.a.child(&cond, 0)
		ek := t.expr_key(expr_id)
		if ek.len > 0 && cond.value.len > 0 {
			return IsExprInfo{
				expr_name:     ek
				variant_name:  cond.value
				sum_type_name: t.find_sum_type_for_variant(cond.value)
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

// find_sum_type_for_variant returns the sum type name that contains
// the given variant, or '' if none is found.
fn (t &Transformer) find_sum_type_for_variant(variant string) string {
	short := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	mut best := ''
	for sum_name, variants in t.sum_types {
		for v in variants {
			if v == variant || v == short {
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
