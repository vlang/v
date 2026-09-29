module transform

import v.flat

// Outside of `lock`/`rlock` blocks, single operations on a `shared` map lock the map by
// themselves (`types.SharedMapAutolock` lists the forms, and the checker accepts them as
// if they were written inside the block). The transformer inserts the blocks: a statement
// that changes the map becomes `lock m { stmt }`, and a read of it becomes the expression
// `rlock m { read }`. Both sides must agree on the forms, otherwise an operation that the
// checker accepted would run without the lock.

// shared_map_autolock_possible reports whether the program declares any `shared` storage.
@[inline]
fn (t &Transformer) shared_map_autolock_possible() bool {
	return t.has_shared_decls || t.source_parent_ids.len == 0
}

// try_autolock_shared_map_stmt wraps a statement that changes a `shared` map in a `lock`
// block. Otherwise it records the reads of `shared` maps in the expressions of the
// statement, so that transform_expr wraps them in `rlock` blocks, and transforms it.
fn (mut t Transformer) try_autolock_shared_map_stmt(id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if id == t.shared_map_reads_stmt {
		return none
	}
	if base_id := t.shared_map_write_autolock_base(node) {
		return t.lower_shared_array_append_autolock_stmt(base_id, id)
	}
	first := t.shared_map_read_ids.len
	t.collect_shared_map_reads_in_stmt(id, node)
	if t.shared_map_read_ids.len == first {
		return none
	}
	mut stmt_id := id
	if node.kind == .for_in_stmt {
		// The loop lowering reads the container (or the range bounds) directly, so replace
		// them with the `rlock` expressions, which are evaluated once, before the loop.
		mut children := t.a.children_of(&node).clone()
		mut replaced := false
		for i in 2 .. t.for_in_header_len(node) {
			if base_id := t.shared_map_read_locks[int(children[i])] {
				children[i] = t.shared_map_rlock_expr(children[i], base_id)
				replaced = true
			}
		}
		if replaced {
			stmt_id = t.copy_node_with_children(node, children)
		}
	}
	saved_stmt := t.shared_map_reads_stmt
	t.shared_map_reads_stmt = stmt_id
	result := t.transform_stmt(stmt_id)
	t.shared_map_reads_stmt = saved_stmt
	// the ids are only meaningful while this statement is transformed
	for read_id in t.shared_map_read_ids[first..] {
		t.shared_map_read_locks.delete(read_id)
	}
	t.shared_map_read_ids.trim(first)
	return result
}

// try_autolock_shared_map_read wraps a read of a `shared` map, recorded for the statement
// that is being transformed, in an `rlock` block.
fn (mut t Transformer) try_autolock_shared_map_read(id flat.NodeId) ?flat.NodeId {
	base_id := t.shared_map_read_locks[int(id)] or { return none }
	lock_id := t.shared_map_rlock_expr(id, base_id)
	return t.transform_lock_expr(lock_id, t.a.nodes[int(lock_id)])
}

// shared_map_rlock_expr returns `rlock base { id }`.
fn (mut t Transformer) shared_map_rlock_expr(id flat.NodeId, base_id flat.NodeId) flat.NodeId {
	mut typ := t.resolve_expr_type(id)
	if !decl_type_is_usable(typ) {
		typ = t.checker_expr_type_name(id) or { '' }
	}
	body := t.make_block([t.make_expr_stmt(id)])
	start := t.a.children.len
	t.a.children << base_id
	t.a.children << body
	return t.a.add_node(flat.Node{
		kind:           .lock_expr
		value:          'rlock'
		typ:            typ
		children_start: start
		children_count: 2
		pos:            t.a.nodes[int(id)].pos
	})
}

// shared_map_write_autolock_base returns the `shared` map that the statement changes:
// `m[k] = v`, `m[k] += v`, `m[k]++`, `m[k] << v`, `m.delete(k)` or `m.clear()`.
fn (t &Transformer) shared_map_write_autolock_base(node flat.Node) ?flat.NodeId {
	match node.kind {
		.assign, .selector_assign, .index_assign {
			if node.children_count == 2 {
				return t.shared_map_index_base(t.a.child(&node, 0))
			}
		}
		.expr_stmt {
			if node.children_count != 1 {
				return none
			}
			expr_id := t.a.child(&node, 0)
			expr := t.a.nodes[int(expr_id)]
			if expr.children_count == 0 {
				return none
			}
			if (expr.kind == .postfix && expr.op in [.inc, .dec])
				|| (expr.kind == .infix && expr.op == .left_shift) {
				return t.shared_map_index_base(t.a.child(&expr, 0))
			}
			if expr.kind == .call {
				callee := t.a.child_node(&expr, 0)
				if callee.kind == .selector && callee.value in ['delete', 'clear']
					&& callee.children_count > 0 {
					return t.shared_map_base(t.a.child(callee, 0))
				}
			}
		}
		else {}
	}
	return none
}

// collect_shared_map_reads_in_stmt records the reads of `shared` maps in the expressions
// of a statement. Nested statements are handled when they are transformed themselves.
fn (mut t Transformer) collect_shared_map_reads_in_stmt(id flat.NodeId, node flat.Node) {
	match node.kind {
		.expr_stmt {
			if node.children_count == 1 {
				t.collect_shared_map_reads(t.a.child(&node, 0), false, true)
			}
		}
		.return_stmt, .assert_stmt {
			for i in 0 .. node.children_count {
				t.collect_shared_map_reads(t.a.child(&node, i), false, false)
			}
		}
		.decl_assign, .assign, .selector_assign, .index_assign {
			for i in 0 .. node.children_count {
				// the targets are only walked for the reads in their indexes
				t.collect_shared_map_reads(t.a.child(&node, i), i % 2 == 0, false)
			}
		}
		.if_expr, .match_stmt {
			t.collect_shared_map_reads(id, false, true)
		}
		.for_stmt {
			if node.children_count > 1 {
				t.collect_shared_map_reads(t.a.child(&node, 1), false, false)
			}
		}
		.for_in_stmt {
			// the container, or the start and the end of a range
			binds_mut := t.for_in_binds_mut(node)
			for i in 2 .. t.for_in_header_len(node) {
				t.collect_shared_map_reads(t.a.child(&node, i), binds_mut, false)
			}
		}
		.select_stmt {
			t.collect_shared_map_reads(id, false, true)
		}
		else {}
	}
}

// for_in_header_len returns the number of children before the body of a `for ... in`:
// the key, the value, the container, and the end of a range.
fn (t &Transformer) for_in_header_len(node flat.Node) int {
	header := if node.value == '4' { 4 } else { 3 }
	return if header < node.children_count { header } else { int(node.children_count) }
}

// collect_shared_map_reads records the reads of `shared` maps in the expression `id`.
// `in_target` is set for the path of an assignment target (or of a `mut`/`&` operand):
// indexing a `shared` map there does not read it.
fn (mut t Transformer) collect_shared_map_reads(id flat.NodeId, in_target bool, is_stmt bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	target := in_target || node.is_mut
	match node.kind {
		.index {
			if !target {
				if base_id := t.shared_map_index_base(id) {
					t.record_shared_map_read(id, base_id)
					return
				}
			}
			for i in 0 .. node.children_count {
				t.collect_shared_map_reads(t.a.child(&node, i), target && i == 0, false)
			}
		}
		.or_expr {
			if node.children_count == 0 {
				return
			}
			index_id := t.a.child(&node, 0)
			if !t.a.nodes[int(index_id)].is_mut {
				if base_id := t.shared_map_index_base(index_id) {
					t.record_shared_map_read(id, base_id)
					return
				}
			}
			// an `m[k] or {}` that is not locked as a whole is not locked at all
			t.collect_shared_map_reads(index_id, true, false)
			if node.children_count > 1 {
				t.collect_shared_map_reads_in_block(t.a.child(&node, 1), true)
			}
		}
		.in_expr {
			if node.children_count == 2 {
				if base_id := t.shared_map_base(t.a.child(&node, 1)) {
					t.record_shared_map_read(id, base_id)
					return
				}
			}
			t.collect_shared_map_reads_in_children(node)
		}
		.selector {
			if node.children_count == 0 {
				return
			}
			base_id := t.a.child(&node, 0)
			if node.value == 'len' {
				if map_id := t.shared_map_base(base_id) {
					t.record_shared_map_read(id, map_id)
					return
				}
			}
			t.collect_shared_map_reads(base_id, target, false)
		}
		.call {
			if node.children_count == 0 {
				return
			}
			callee := t.a.child_node(&node, 0)
			if callee.kind == .selector && callee.children_count > 0 {
				if node.children_count == 1 && callee.value in ['keys', 'values', 'clone'] {
					if base_id := t.shared_map_base(t.a.child(callee, 0)) {
						t.record_shared_map_read(id, base_id)
						return
					}
				}
			}
			t.collect_shared_map_reads_in_children(node)
		}
		.paren {
			for i in 0 .. node.children_count {
				t.collect_shared_map_reads(t.a.child(&node, i), target, is_stmt)
			}
		}
		.prefix {
			for i in 0 .. node.children_count {
				t.collect_shared_map_reads(t.a.child(&node, i), node.op == .amp, false)
			}
		}
		.postfix {
			for i in 0 .. node.children_count {
				t.collect_shared_map_reads(t.a.child(&node, i), true, false)
			}
		}
		.infix {
			appends := is_stmt && node.op == .left_shift
			for i in 0 .. node.children_count {
				t.collect_shared_map_reads(t.a.child(&node, i), appends && i == 0, false)
			}
		}
		.if_expr {
			if node.children_count == 0 {
				return
			}
			cond := t.a.nodes[int(t.a.child(&node, 0))]
			if cond.kind == .decl_assign {
				// `if v := m[k] {` is not locked, but the reads in its other parts are
				for i in 0 .. cond.children_count {
					t.collect_shared_map_reads(t.a.child(&cond, i), true, false)
				}
			} else {
				t.collect_shared_map_reads(t.a.child(&node, 0), false, false)
			}
			for i in 1 .. node.children_count {
				branch_id := t.a.child(&node, i)
				if int(branch_id) < 0 {
					continue
				}
				if t.a.nodes[int(branch_id)].kind == .if_expr {
					t.collect_shared_map_reads(branch_id, false, is_stmt)
				} else if !is_stmt {
					// the value of a branch is not lowered as a statement
					t.collect_shared_map_reads_in_block(branch_id, true)
				}
			}
		}
		.match_stmt {
			if node.children_count > 0 {
				t.collect_shared_map_reads(t.a.child(&node, 0), false, false)
			}
			for i in 1 .. node.children_count {
				// the value of a branch is not lowered as a statement
				t.collect_shared_map_reads_in_block(t.a.child(&node, i), !is_stmt)
			}
		}
		.select_stmt {
			// Only the reads in the case headers (channels, sent values and timeouts) are
			// locked, not the channel operations. The bodies are lowered as statements.
			for i in 0 .. node.children_count {
				branch := t.a.nodes[int(t.a.child(&node, i))]
				if branch.kind != .select_branch || branch.value == 'else' {
					continue
				}
				is_recv := branch.children_count > 1
					&& t.a.child_node(&branch, 1).kind == .prefix
					&& t.a.child_node(&branch, 1).op == .arrow
				if is_recv {
					// the target of `x := <-ch` / `x = <-ch`, then the receive
					t.collect_shared_map_reads(t.a.child(&branch, 0), true, false)
					t.collect_shared_map_reads(t.a.child(&branch, 1), false, false)
				} else if branch.children_count > 0 {
					t.collect_shared_map_reads(t.a.child(&branch, 0), false, false)
				}
			}
		}
		.block, .match_branch, .lock_expr, .fn_literal, .lambda_expr, .select_branch,
		.comptime_if, .comptime_for, .sql_expr, .asm_stmt, .typeof_expr,
		.sizeof_expr, .offsetof_expr, .defer_stmt, .decl_assign, .assign, .selector_assign,
		.index_assign, .expr_stmt, .for_stmt, .for_in_stmt, .return_stmt {
		}
		else {
			t.collect_shared_map_reads_in_children(node)
		}
	}
}

// collect_shared_map_reads_in_block records the reads in the patterns of a `match` branch
// and, with `with_body`, in the statements of the branch or block.
fn (mut t Transformer) collect_shared_map_reads_in_block(id flat.NodeId, with_body bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind !in [.block, .match_branch] {
		return
	}
	body_start := if node.kind == .match_branch && node.value != 'else' {
		node.value.int()
	} else {
		0
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		if i < body_start {
			t.collect_shared_map_reads(child_id, false, false)
		} else if with_body {
			t.collect_shared_map_reads_in_stmt(child_id, t.a.nodes[int(child_id)])
		}
	}
}

fn (mut t Transformer) record_shared_map_read(id flat.NodeId, base_id flat.NodeId) {
	t.shared_map_read_locks[int(id)] = base_id
	t.shared_map_read_ids << int(id)
}

fn (mut t Transformer) collect_shared_map_reads_in_children(node flat.Node) {
	for i in 0 .. node.children_count {
		t.collect_shared_map_reads(t.a.child(&node, i), false, false)
	}
}

fn (t &Transformer) for_in_binds_mut(node flat.Node) bool {
	for i in 0 .. 2 {
		if i < node.children_count {
			child_id := t.a.child(&node, i)
			if int(child_id) >= 0 && t.a.nodes[int(child_id)].is_mut {
				return true
			}
		}
	}
	return false
}

// shared_map_index_base returns the `shared` map that `m[k]` or `m[k1][k2]...` indexes.
fn (t &Transformer) shared_map_index_base(id flat.NodeId) ?flat.NodeId {
	mut cur := id
	for int(cur) >= 0 && int(cur) < t.a.nodes.len {
		node := t.a.nodes[int(cur)]
		if node.kind != .index || node.children_count < 2 || node.op == .gated_index {
			return none
		}
		base_id := t.a.child(&node, 0)
		if int(base_id) >= 0 && t.a.nodes[int(base_id)].kind != .index {
			return t.shared_map_base(base_id)
		}
		cur = base_id
	}
	return none
}

// shared_map_base returns `id` when it is a `shared` map variable, or a `shared` map field
// reached through fields of a variable, which can be locked without evaluating anything.
fn (t &Transformer) shared_map_base(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value.len == 0 {
			return none
		}
		mut typ := t.raw_var_type(node.value)
		if typ.len == 0 {
			typ = t.current_module_global_type(node.value) or { '' }
		}
		if typ.len == 0 && t.local_decl_is_shared_before(node.value, id) {
			typ = 'shared ' + (t.checker_expr_type_name(id) or { '' })
		}
		if t.is_shared_map_type_text(typ) {
			return id
		}
		return none
	}
	if node.kind == .selector && node.children_count > 0 && t.shared_field_names[node.value] {
		base_id := t.a.child(&node, 0)
		mut base := t.a.nodes[int(base_id)]
		for base.kind == .selector && base.children_count > 0 {
			base = t.a.nodes[int(t.a.child(&base, 0))]
		}
		if base.kind != .ident {
			return none
		}
		typ := t.raw_selector_field_type(id) or {
			mut base_type := t.original_expr_type(base_id)
			if base_type.len == 0 {
				base_type = t.node_type(base_id)
			}
			t.promoted_field_raw_type(base_type.trim_left('&'), node.value, 0) or { return none }
		}
		if t.is_shared_map_type_text(typ) {
			return id
		}
	}
	return none
}

// promoted_field_raw_type returns the declared type of a field promoted from an embedded
// struct of `struct_type`.
fn (t &Transformer) promoted_field_raw_type(struct_type string, field_name string, depth int) ?string {
	if depth > 8 {
		return none
	}
	info := t.lookup_struct_info(struct_type) or { return none }
	if field := info.field(field_name) {
		return if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
	}
	for field in info.fields {
		if field.is_embedded {
			if typ := t.promoted_field_raw_type(field.typ.trim_left('&'), field_name, depth + 1) {
				return typ
			}
		}
	}
	return none
}

fn (t &Transformer) is_shared_map_type_text(typ string) bool {
	mut clean := typ.trim_space()
	if !clean.starts_with('shared ') {
		return false
	}
	clean = clean['shared '.len..].trim_space()
	for clean.starts_with('&') {
		clean = clean[1..]
	}
	return clean.starts_with('map[') || t.normalize_type_alias(clean).starts_with('map[')
}
