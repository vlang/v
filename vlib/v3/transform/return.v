module transform

import v3.flat

// transform_return_with_sumtype_wrap checks if a return statement returns a
// variant value where the function's return type is a sum type. In that case,
// the variant value may need to be wrapped in the sum type's tagged union.
//
// For now, passes through unchanged since the C gen handles wrapping currently.
// This is a hook for future sum type return wrapping at the transform level.
fn (mut t Transformer) transform_return_with_sumtype_wrap(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	// Check if current function returns a sum type
	if t.cur_fn_ret_type.len == 0 || t.cur_fn_ret_type !in t.sum_types {
		return arr1(id)
	}
	// Check if the return value is a struct init whose type is a variant
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .struct_init && child.value.len > 0 {
		variants := t.sum_types[t.cur_fn_ret_type]
		for v in variants {
			if v == child.value {
				// This is a variant being returned as a sum type.
				// For now, pass through - C gen handles the wrapping.
				// TODO: Generate explicit sum type wrapping here.
				return arr1(id)
			}
		}
	}
	return arr1(id)
}

// branch_tail_expr extracts the tail EXPRESSION id from a branch block,
// unwrapping a trailing `.expr_stmt` so we get the expression inside it
// rather than the statement wrapper.
fn (t &Transformer) branch_tail_expr(block_id flat.NodeId) flat.NodeId {
	if int(block_id) < 0 {
		return block_id
	}
	block := t.a.nodes[int(block_id)]
	if block.kind != .block || block.children_count == 0 {
		return block_id
	}
	last_id := t.a.child(&block, block.children_count - 1)
	last := t.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count > 0 {
		return t.a.child(&last, 0)
	}
	return last_id
}

// make_return builds a `return <val>` statement node with the given type.
fn (mut t Transformer) make_return(val flat.NodeId, ret_typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << val
	return t.a.add_node(flat.Node{
		kind:           .return_stmt
		children_start: start
		children_count: 1
		typ:            ret_typ
	})
}

// return_block_from_branch builds a block that keeps leading statements
// (transformed) and turns the tail expression of the branch into a `return`.
fn (mut t Transformer) return_block_from_branch(branch_id flat.NodeId, ret_typ string) flat.NodeId {
	branch := t.a.nodes[int(branch_id)]
	if branch.kind != .block {
		// single expression branch: just `return <expr>`
		mut all := []flat.NodeId{}
		ret_val := t.wrap_sum_return_expr(branch_id)
		t.drain_pending(mut all)
		all << t.make_return(ret_val, ret_typ)
		return t.make_block(all)
	}
	mut stmt_ids := []flat.NodeId{}
	for i in 0 .. branch.children_count {
		stmt_ids << t.a.child(&branch, i)
	}
	if stmt_ids.len == 0 {
		return t.make_block([]flat.NodeId{})
	}
	// all but the last are kept as statements (transformed); the last becomes a return
	lead := stmt_ids[..stmt_ids.len - 1].clone()
	new_lead := t.transform_stmts(lead)
	tail_expr := t.branch_tail_expr(branch_id)
	mut all := []flat.NodeId{}
	for s in new_lead {
		all << s
	}
	ret_val := t.wrap_sum_return_expr(tail_expr)
	t.drain_pending(mut all)
	ret := t.make_return(ret_val, ret_typ)
	all << ret
	return t.make_block(all)
}

// build_return_if_chain recursively converts an if_expr (possibly an else-if
// chain) into an if-statement whose branch tails are `return` statements.
fn (mut t Transformer) build_return_if_chain(if_id flat.NodeId, ret_typ string) flat.NodeId {
	if_node := t.a.nodes[int(if_id)]
	cond_id := t.a.child(&if_node, 0)
	new_cond := t.transform_expr(cond_id)
	then_id := t.a.child(&if_node, 1)
	then_block := t.return_block_from_branch(then_id, ret_typ)
	mut else_block := flat.empty_node
	if if_node.children_count >= 3 {
		else_id := t.a.child(&if_node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			// else-if chain: recurse, wrap resulting if-stmt in a block
			inner := t.build_return_if_chain(else_id, ret_typ)
			else_block = t.make_block(arr1(inner))
		} else {
			else_block = t.return_block_from_branch(else_id, ret_typ)
		}
	}
	return t.make_if(new_cond, then_block, else_block)
}

// try_expand_return_if detects a `return if cond { a } else { b }` pattern
// and expands it into `if cond { return a } else { return b }`.
// This simplification makes the C backend's job easier since it avoids
// needing statement-expressions for the if-as-value in the return position.
//
// Leading statements in each branch are preserved, and a trailing `.expr_stmt`
// is unwrapped so the inner expression (not the statement) is returned.
//
// Returns the expanded if-statement as a single-element array, or none if the
// pattern does not match. A plain `return if x {..}` with no else (if_expr with
// fewer than 3 children) is left unexpanded.
fn (mut t Transformer) try_expand_return_if(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	val_id := t.a.child(&node, 0)
	val_node := t.a.nodes[int(val_id)]
	if val_node.kind != .if_expr || val_node.children_count < 3 {
		return none
	}
	return arr1(t.build_return_if_chain(val_id, node.typ))
}

fn (mut t Transformer) match_branch_return_block(branch flat.Node, body_start_idx int, ret_typ string) flat.NodeId {
	mut body_ids := []flat.NodeId{}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	if body_ids.len == 0 {
		return t.make_block([]flat.NodeId{})
	}
	mut all := []flat.NodeId{}
	if body_ids.len > 1 {
		lead := body_ids[..body_ids.len - 1].clone()
		for stmt in t.transform_stmts(lead) {
			all << stmt
		}
	}
	tail_id := body_ids[body_ids.len - 1]
	tail := t.a.nodes[int(tail_id)]
	if tail.kind == .return_stmt {
		for stmt in t.transform_stmt(tail_id) {
			all << stmt
		}
		return t.make_block(all)
	}
	tail_expr := if tail.kind == .expr_stmt && tail.children_count > 0 {
		t.a.child(&tail, 0)
	} else {
		tail_id
	}
	ret_val := t.wrap_sum_return_expr(tail_expr)
	t.drain_pending(mut all)
	all << t.make_return(ret_val, ret_typ)
	return t.make_block(all)
}

fn (mut t Transformer) build_return_match_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int, ret_typ string) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'
	body_start_idx := if is_else { 0 } else { t.count_conds(branch) }

	mut sc_pushed := 0
	if !is_else {
		n_conds := t.count_conds(branch)
		if n_conds == 1 {
			cond_val := t.a.nodes[int(t.a.child(&branch, 0))]
			if cond_val.kind == .ident && t.is_sum_variant(cond_val.value) {
				subj := t.expr_key(match_expr_id)
				sum_name := t.find_sum_type_for_variant(cond_val.value)
				if subj.len > 0 && sum_name.len > 0 {
					t.push_smartcast(subj, cond_val.value, sum_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj && sum_name.len > 0 {
					t.push_smartcast(orig_subj, cond_val.value, sum_name)
					sc_pushed++
				}
			}
		}
	}

	body_block := t.match_branch_return_block(branch, body_start_idx, ret_typ)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}
	if is_else {
		return body_block
	}

	cond_id := t.build_match_cond(match_expr_id, branch)
	mut if_ids := []flat.NodeId{}
	if_ids << cond_id
	if_ids << body_block
	if idx + 1 < branches.len {
		if_ids << t.build_return_match_chain(match_expr_id, orig_expr_id, branches, idx + 1,
			ret_typ)
	}
	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(if_ids.len)
	})
}

// try_expand_return_match detects a `return match x { ... }` pattern and expands
// it into an if/else-if chain where every branch tail is an explicit return.
fn (mut t Transformer) try_expand_return_match(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	val_id := t.a.child(&node, 0)
	val := t.a.nodes[int(val_id)]
	if val.kind != .match_stmt || val.children_count < 2 {
		return none
	}
	match_expr_id := t.a.child(&val, 0)
	match_expr := t.a.nodes[int(match_expr_id)]
	needs_temp := match_expr.kind !in [.ident, .int_literal, .bool_literal, .string_literal,
		.char_literal]

	mut result := []flat.NodeId{}
	mut actual_expr_id := flat.empty_node
	if needs_temp {
		tmp_name := t.new_temp('match')
		match_type := t.node_type(match_expr_id)
		new_expr := t.transform_expr(match_expr_id)
		t.drain_pending(mut result)
		result << t.make_decl_assign_typed(tmp_name, new_expr, match_type)
		actual_expr_id = t.make_ident(tmp_name)
		t.a.nodes[int(actual_expr_id)].typ = match_type
		t.set_var_type(tmp_name, match_type)
	} else {
		actual_expr_id = t.transform_expr(match_expr_id)
		t.drain_pending(mut result)
	}

	mut branches := []flat.NodeId{}
	for i in 1 .. val.children_count {
		branches << t.a.child(&val, i)
	}
	result << t.build_return_match_chain(actual_expr_id, match_expr_id, branches, 0, node.typ)
	return result
}
