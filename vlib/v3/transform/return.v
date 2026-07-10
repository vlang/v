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
	return t.make_return_values(arr1(val), ret_typ)
}

fn (mut t Transformer) make_return_values(vals []flat.NodeId, ret_typ string) flat.NodeId {
	start := t.a.children.len
	for val in vals {
		t.a.children << val
	}
	return t.a.add_node(flat.Node{
		kind:           .return_stmt
		children_start: start
		children_count: flat.child_count(vals.len)
		typ:            ret_typ
	})
}

fn (t &Transformer) current_return_multi_count() int {
	if isnil(t.tc) || t.cur_fn_ret_type.len == 0 {
		return 0
	}
	items := multi_return_types_from_type(t.tc.parse_type(t.cur_fn_ret_type), 0) or { return 0 }
	return items.len
}

fn (mut t Transformer) return_values_from_ids(ids []flat.NodeId) []flat.NodeId {
	mut vals := []flat.NodeId{cap: ids.len}
	for i, id in ids {
		vals << t.transform_return_child(id, i, ids.len)
	}
	return vals
}

// return_expr_is_err supports return expr is err handling for Transformer.
fn (t &Transformer) return_expr_is_err(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .ident && node.value == 'err'
}

fn (mut t Transformer) try_return_direct_optional_expr(node flat.Node) ?[]flat.NodeId {
	if node.children_count != 1 || !t.is_optional_type_name(t.cur_fn_ret_type) {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .none_expr || !t.return_expr_is_optional_result(child_id) {
		return none
	}
	ret_type := t.qualify_optional_type(t.cur_fn_ret_type)
	expr_type := t.qualify_optional_type(t.optional_result_expr_type_name(child_id))
	if !t.optional_types_match(ret_type, expr_type) {
		return none
	}
	new_expr := t.transform_expr(child_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_return(new_expr, ret_type)
	return result
}

fn (t &Transformer) optional_types_match(a string, b string) bool {
	if !t.is_optional_type_name(a) || !t.is_optional_type_name(b) || a[0] != b[0] {
		return false
	}
	a_base := t.normalize_type_alias(t.optional_base_type(a))
	b_base := t.normalize_type_alias(t.optional_base_type(b))
	if a_base == b_base {
		return true
	}
	return a_base.all_after_last('.') == b_base.all_after_last('.')
}

// try_expand_return_optional_expr
// supports helper handling in transform.
fn (mut t Transformer) try_expand_return_optional_expr(node flat.Node) ?[]flat.NodeId {
	if node.children_count != 1 || !t.is_optional_type_name(t.cur_fn_ret_type) {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .none_expr {
		return none
	}
	if !t.return_expr_is_optional_result(child_id) {
		return none
	}
	expr_type0 := t.optional_result_expr_type_name(child_id)
	if !t.is_optional_type_name(expr_type0) {
		return none
	}
	ret_type := t.qualify_optional_type(t.cur_fn_ret_type)
	expr_type := t.qualify_optional_type(expr_type0)
	new_expr := t.transform_expr(child_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	tmp_name := t.new_temp('return_opt')
	result << t.make_decl_assign_typed(tmp_name, new_expr, expr_type)
	tmp_ident := t.make_ident(tmp_name)
	ok_cond := t.make_selector(tmp_ident, 'ok', 'bool')
	value := t.make_selector(t.make_ident(tmp_name), 'value', t.optional_base_type(expr_type))
	then_block := t.make_block(arr1(t.make_return(value, ret_type)))
	err_expr := t.make_selector(t.make_ident(tmp_name), 'err', 'IError')
	else_block := t.make_block(arr1(t.make_none_return_stmt_with_err_expr(err_expr)))
	result << t.make_if(ok_cond, then_block, else_block)
	return result
}

// return_expr_is_optional_result supports return expr is optional result handling for Transformer.
fn (t &Transformer) return_expr_is_optional_result(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call && !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			if ret := t.tc.fn_ret_types[name] {
				return t.is_optional_type_name(ret.name())
			}
		}
		return t.is_optional_type_name(t.get_call_return_type(id, node))
	}
	if node.kind == .ident {
		typ := t.var_type(node.value)
		return t.is_optional_type_name(typ) || t.is_optional_type_name(node.typ)
	}
	if node.kind in [.selector, .index, .if_expr, .match_stmt, .or_expr] {
		return t.is_optional_type_name(t.node_type(id))
	}
	return t.is_optional_type_name(node.typ)
}

// return_block_from_branch builds a block that keeps leading statements
// (transformed) and turns the tail expression of the branch into a `return`.
fn (mut t Transformer) return_block_from_branch(branch_id flat.NodeId, ret_typ string, extra_return_vals []flat.NodeId) flat.NodeId {
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .return_stmt {
		mut all := []flat.NodeId{}
		ret_stmts := t.transform_stmt(branch_id)
		t.drain_pending(mut all)
		for stmt in ret_stmts {
			all << stmt
		}
		return t.make_block(all)
	}
	if branch.kind != .block {
		// single expression branch: just `return <expr>`
		mut all := []flat.NodeId{}
		ret_vals := t.return_values_with_extra(branch_id, extra_return_vals)
		t.drain_pending(mut all)
		all << t.make_return_values(ret_vals, ret_typ)
		return t.make_block(all)
	}
	mut stmt_ids := []flat.NodeId{}
	for i in 0 .. branch.children_count {
		stmt_ids << t.a.child(&branch, i)
	}
	if stmt_ids.len == 0 {
		return t.make_block([]flat.NodeId{})
	}
	tuple_count := t.current_return_multi_count() - extra_return_vals.len
	if tuple_count > 1 {
		if parts := t.tuple_block_parts(branch_id, tuple_count) {
			mut all := t.transform_stmts(parts.prefix)
			mut ret_ids := parts.values.clone()
			for extra in extra_return_vals {
				ret_ids << extra
			}
			ret_vals := t.return_values_from_ids(ret_ids)
			t.drain_pending(mut all)
			all << t.make_return_values(ret_vals, ret_typ)
			return t.make_block(all)
		}
	}
	// all but the last are kept as statements (transformed); the last becomes a return
	lead := stmt_ids[..stmt_ids.len - 1].clone()
	new_lead := t.transform_stmts(lead)
	tail_id := stmt_ids[stmt_ids.len - 1]
	tail := t.a.nodes[int(tail_id)]
	tail_expr := t.branch_tail_expr(branch_id)
	mut all := []flat.NodeId{}
	for s in new_lead {
		all << s
	}
	if tail.kind == .return_stmt {
		ret_stmts := t.transform_stmt(tail_id)
		t.drain_pending(mut all)
		for stmt in ret_stmts {
			all << stmt
		}
		return t.make_block(all)
	}
	ret_vals := t.return_values_with_extra(tail_expr, extra_return_vals)
	t.drain_pending(mut all)
	ret := t.make_return_values(ret_vals, ret_typ)
	all << ret
	return t.make_block(all)
}

// build_return_if_chain recursively converts an if_expr (possibly an else-if
// chain) into an if-statement whose branch tails are `return` statements.
fn (mut t Transformer) build_return_if_chain(if_id flat.NodeId, ret_typ string, extra_return_vals []flat.NodeId) flat.NodeId {
	if_node := t.a.nodes[int(if_id)]
	cond_id := t.a.child(&if_node, 0)
	cond_smartcasts := t.extract_all_is_exprs(cond_id)
	new_cond := t.transform_and_chain_smartcasts(cond_id)
	mut cond_prelude := []flat.NodeId{}
	t.drain_pending(mut cond_prelude)
	then_id := t.a.child(&if_node, 1)
	for info in cond_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	then_block := t.return_block_from_branch(then_id, ret_typ, extra_return_vals)
	for _ in cond_smartcasts {
		t.pop_smartcast()
	}
	mut else_block := flat.empty_node
	if if_node.children_count >= 3 {
		else_id := t.a.child(&if_node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			// else-if chain: recurse, wrap resulting if-stmt in a block
			inner := t.build_return_if_chain(else_id, ret_typ, extra_return_vals)
			else_block = t.make_block(arr1(inner))
		} else {
			else_block = t.return_block_from_branch(else_id, ret_typ, extra_return_vals)
		}
	}
	new_if := t.make_if(new_cond, then_block, else_block)
	if cond_prelude.len == 0 {
		return new_if
	}
	cond_prelude << new_if
	return t.make_block(cond_prelude)
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
	if int(val_id) < 0 || int(val_id) >= t.a.nodes.len {
		return none
	}
	val_node := t.a.nodes[int(val_id)]
	if val_node.kind != .if_expr || val_node.children_count < 3 {
		return none
	}
	mut extra_return_vals := []flat.NodeId{}
	for i in 1 .. node.children_count {
		extra_return_vals << t.a.child(&node, i)
	}
	return arr1(t.build_return_if_chain(val_id, node.typ, extra_return_vals))
}

fn (t &Transformer) match_branch_tuple_parts(branch flat.Node, body_start_idx int, count int) ?TupleBlockParts {
	if count <= 1 || branch.children_count <= body_start_idx {
		return none
	}
	children := t.a.children_of(&branch).clone()
	mut values := []flat.NodeId{}
	mut prefix_end := children.len
	for i := children.len - 1; i >= body_start_idx; i-- {
		child_id := children[i]
		child := t.a.nodes[int(child_id)]
		if values.len == 0 && child.kind == .block {
			if nested := t.tuple_block_parts(child_id, count) {
				mut prefix := children[body_start_idx..i].clone()
				for prefix_id in nested.prefix {
					prefix << prefix_id
				}
				return TupleBlockParts{
					prefix: prefix
					values: nested.values.clone()
				}
			}
		}
		if child.kind != .expr_stmt || child.children_count == 0 {
			break
		}
		for j := int(child.children_count) - 1; j >= 0; j-- {
			values.prepend(t.a.child(&child, j))
			if values.len == count {
				break
			}
		}
		prefix_end = i
		if values.len == count {
			return TupleBlockParts{
				prefix: children[body_start_idx..prefix_end].clone()
				values: values.clone()
			}
		}
	}
	return none
}

// match_branch_return_block supports match branch return block handling for Transformer.
fn (mut t Transformer) match_branch_return_block(branch flat.Node, body_start_idx int, ret_typ string) flat.NodeId {
	mut body_ids := []flat.NodeId{}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	if body_ids.len == 0 {
		return t.make_block([]flat.NodeId{})
	}
	tuple_count := t.current_return_multi_count()
	if tuple_count > 1 {
		if parts := t.match_branch_tuple_parts(branch, body_start_idx, tuple_count) {
			mut all := t.transform_stmts(parts.prefix)
			ret_vals := t.return_values_from_ids(parts.values)
			t.drain_pending(mut all)
			all << t.make_return_values(ret_vals, ret_typ)
			return t.make_block(all)
		}
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
	tail_expr_node := t.a.nodes[int(tail_expr)]
	if tail_expr_node.kind in [.if_expr, .match_stmt] {
		nested_return := t.make_return(tail_expr, ret_typ)
		for stmt in t.transform_stmt(nested_return) {
			all << stmt
		}
		return t.make_block(all)
	}
	expected_ret := t.optional_base_type(ret_typ)
	actual_tail := if tail_expr_node.kind == .enum_val {
		t.transform_enum_shorthand(tail_expr, tail_expr_node, expected_ret)
	} else {
		tail_expr
	}
	// Convert a fixed-array branch value (e.g. a fixed-array const) to a dynamic
	// array when the function returns `[]T`, matching a plain `return <expr>`.
	ret_val := if converted := t.fixed_array_return_value(actual_tail) {
		converted
	} else {
		t.wrap_sum_return_expr(actual_tail)
	}
	t.drain_pending(mut all)
	all << t.make_return(ret_val, ret_typ)
	return t.make_block(all)
}

// build_return_match_chain builds return match chain data for transform.
fn (mut t Transformer) build_return_match_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int, ret_typ string) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'
	body_start_idx := if is_else { 0 } else { t.count_conds(branch) }
	if !is_else && t.match_branch_all_type_patterns(match_expr_id, branch)
		&& t.count_conds(branch) > 1 {
		return t.build_return_match_type_branch_chain(match_expr_id, orig_expr_id, branch,
			branches, idx, 0, ret_typ)
	}

	mut sc_pushed := 0
	if !is_else {
		n_conds := t.count_conds(branch)
		if n_conds == 1 {
			cond_val_id := t.a.child(&branch, 0)
			if sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) {
				subj := t.expr_key(match_expr_id)
				if subj.len > 0 {
					t.push_smartcast(subj, sc.variant_name, sc.sum_type_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj {
					t.push_smartcast(orig_subj, sc.variant_name, sc.sum_type_name)
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

// build_return_match_type_branch_chain supports build_return_match_type_branch_chain handling.
fn (mut t Transformer) build_return_match_type_branch_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branch flat.Node, branches []flat.NodeId, idx int, cond_idx int, ret_typ string) flat.NodeId {
	n_conds := t.count_conds(branch)
	if cond_idx >= n_conds {
		return if idx + 1 < branches.len {
			t.build_return_match_chain(match_expr_id, orig_expr_id, branches, idx + 1, ret_typ)
		} else {
			t.a.add(flat.NodeKind.empty)
		}
	}
	cond_val_id := t.a.child(&branch, cond_idx)
	variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) or {
		return t.build_return_match_chain(match_expr_id, orig_expr_id, branches, idx + 1, ret_typ)
	}
	is_start := t.a.children.len
	t.a.children << match_expr_id
	is_id := t.a.add_node(flat.Node{
		kind:           .is_expr
		value:          variant_name
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) or {
		SmartcastContext{
			variant_name:  variant_name
			sum_type_name: ''
		}
	}
	subj := t.expr_key(match_expr_id)
	if subj.len > 0 && sc.sum_type_name.len > 0 {
		t.push_smartcast(subj, sc.variant_name, sc.sum_type_name)
		sc_pushed++
	}
	orig_subj := t.expr_key(orig_expr_id)
	if orig_subj.len > 0 && orig_subj != subj && sc.sum_type_name.len > 0 {
		t.push_smartcast(orig_subj, sc.variant_name, sc.sum_type_name)
		sc_pushed++
	}
	body_block := t.match_branch_return_block(branch, n_conds, ret_typ)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_return_match_type_branch_chain(match_expr_id, orig_expr_id, branch,
		branches, idx, cond_idx + 1, ret_typ)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
	})
}

// try_expand_return_match detects a `return match x { ... }` pattern and expands
// it into an if/else-if chain where every branch tail is an explicit return.
fn (mut t Transformer) try_expand_return_match(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	val_id := t.a.child(&node, 0)
	if int(val_id) < 0 || int(val_id) >= t.a.nodes.len {
		return none
	}
	val := t.a.nodes[int(val_id)]
	if val.kind != .match_stmt || val.children_count < 2 {
		return none
	}
	match_expr_id := t.a.child(&val, 0)
	if int(match_expr_id) < 0 || int(match_expr_id) >= t.a.nodes.len {
		return none
	}
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
		t.set_node_typ(int(actual_expr_id), match_type)
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
