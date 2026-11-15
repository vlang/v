module transform

import v.flat
import v.types { unalias_type }

// Bounds checks of `a[i]` in `for i in low .. a.len { ... }`.
//
// When `low` is a non-negative literal, `a` is an immutable local array or string
// and the loop body cannot write `a` or `i`, every `a[i]` evaluated by the body
// has `low <= i < a.len`: the condition `i < a.len` is checked before each
// iteration, and nothing can change `a.len` or `i` until the next one. Such reads
// are marked with node_flag_in_range_index, and cgen omits their bounds check.

// range_for_in_len_target returns `a` for a loop `for i in low .. a.len` whose
// reads `a[i]` cannot be out of range, or none when that cannot be shown.
fn (t &Transformer) range_for_in_len_target(low_id flat.NodeId, high_id flat.NodeId, loop_name string, body_ids []flat.NodeId) ?string {
	if t.cur_fn_is_generic || isnil(t.tc) || int(low_id) < 0 || int(high_id) < 0 {
		return none
	}
	low := *t.a.node(low_id)
	if low.kind != .int_literal || low.value.len == 0 || !low.value.bytes().all(it.is_digit()) {
		return none
	}
	high := *t.a.node(high_id)
	if high.kind != .selector || high.value != 'len' || high.children_count != 1 {
		return none
	}
	base := t.a.child_node(&high, 0)
	name := base.value
	if base.kind != .ident || base.is_mut || name.len == 0 || name == '_' || name == loop_name {
		return none
	}
	// Mutable bindings can be changed through aliases that the loop body does not
	// name. Globals can be changed by any call.
	if !t.var_is_immutable(name) || t.range_index_name_is_global(name) {
		return none
	}
	base_type := unalias_type(t.tc.parse_type(t.var_type(name)))
	if base_type !is types.Array && base_type !is types.String {
		return none
	}
	for id in body_ids {
		if !t.range_index_body_keeps_bounds(id, name, loop_name) {
			return none
		}
	}
	return name
}

fn (t &Transformer) range_index_name_is_global(name string) bool {
	if _ := t.globals.type_id(name) {
		return true
	}
	if t.cur_module.len > 0 {
		if _ := t.globals.type_id('${t.cur_module}.${name}') {
			return true
		}
	}
	return false
}

// range_index_body_keeps_bounds reports whether the source statement `id` leaves
// the array `name` and the loop variable `loop_name` unchanged, does not bind
// either name again, and cannot run after the loop condition stops holding.
fn (t &Transformer) range_index_body_keeps_bounds(id flat.NodeId, name string, loop_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return true
	}
	node := *t.a.node(id)
	match node.kind {
		// A deferred block runs after the loop, and a label can be entered by `goto`
		// without checking the loop condition. Inline assembly can write anything.
		.defer_stmt, .goto_stmt, .label_stmt, .asm_stmt {
			return false
		}
		.ident {
			if node.is_mut && (node.value == name || node.value == loop_name) {
				return false
			}
		}
		.prefix {
			// `&a`, `&i` or `&a.len`. The address of an element, `&a[i]`, cannot
			// change either.
			if node.op == .amp && node.children_count > 0
				&& t.range_index_is_name_or_field(t.a.child(&node, 0), name, loop_name) {
				return false
			}
		}
		.postfix {
			if (node.op == .inc || node.op == .dec) && node.children_count > 0
				&& t.range_index_root_is(t.a.child(&node, 0), name, loop_name) {
				return false
			}
		}
		.infix {
			// `a << x` appends; `a[i] << 2` is a shift of an element.
			if node.op == .left_shift && node.children_count > 0
				&& t.range_index_is_name(t.a.child(&node, 0), name, loop_name) {
				return false
			}
		}
		.assign, .decl_assign {
			for i in 0 .. t.multi_assign_lhs_count(node) {
				if t.range_index_root_is(t.multi_assign_lhs_id(node, i), name, loop_name) {
					return false
				}
			}
		}
		.selector_assign, .index_assign {
			if node.children_count > 0
				&& t.range_index_root_is(t.a.child(&node, 0), name, loop_name) {
				return false
			}
		}
		.for_in_stmt {
			// The key and value bindings of a nested loop.
			for i in 0 .. 2 {
				if i < node.children_count
					&& t.range_index_root_is(t.a.child(&node, i), name, loop_name) {
					return false
				}
			}
		}
		.param {
			value := node.value_text()
			if value == name || value == loop_name {
				return false
			}
		}
		.lambda_expr {
			// Lambda parameters are identifiers, not `.param` nodes. Array
			// map/filter lowering keeps their names but removes the lambda scope.
			for i := 0; i + 1 < int(node.children_count); i++ {
				if t.range_index_is_name(t.a.child(&node, i), name, loop_name) {
					return false
				}
			}
		}
		else {}
	}
	for i in 0 .. node.children_count {
		if !t.range_index_body_keeps_bounds(t.a.child(&node, i), name, loop_name) {
			return false
		}
	}
	return true
}

// range_index_is_name reports whether `id` is `name` or `loop_name`, possibly in parentheses.
fn (t &Transformer) range_index_is_name(id flat.NodeId, name string, loop_name string) bool {
	return t.range_index_name_path(id, name, loop_name, false)
}

// range_index_is_name_or_field also accepts a field of either name, such as `a.len`.
fn (t &Transformer) range_index_is_name_or_field(id flat.NodeId, name string, loop_name string) bool {
	return t.range_index_name_path(id, name, loop_name, true)
}

fn (t &Transformer) range_index_name_path(id flat.NodeId, name string, loop_name string, fields bool) bool {
	mut cur := id
	for int(cur) >= 0 && int(cur) < t.a.nodes.len {
		node := *t.a.node(cur)
		if node.kind == .ident {
			return node.value == name || node.value == loop_name
		}
		if !(node.kind == .paren || (fields && node.kind == .selector)) || node.children_count == 0 {
			return false
		}
		cur = t.a.child(&node, 0)
	}
	return false
}

// range_index_root_is reports whether the lvalue `id` is rooted at `name` or `loop_name`.
fn (t &Transformer) range_index_root_is(id flat.NodeId, name string, loop_name string) bool {
	mut cur := id
	for int(cur) >= 0 && int(cur) < t.a.nodes.len {
		node := *t.a.node(cur)
		match node.kind {
			.ident {
				return node.value == name || node.value == loop_name
			}
			.selector, .index, .paren, .prefix, .postfix, .cast_expr, .as_expr {
				if node.children_count == 0 {
					return false
				}
				cur = t.a.child(&node, 0)
			}
			else {
				return false
			}
		}
	}
	return false
}

// mark_in_range_indexes flags the reads `name[loop_name]` in the lowered loop body.
// Function literals are skipped: they can run after the loop.
fn (mut t Transformer) mark_in_range_indexes(id flat.NodeId, name string, loop_name string) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := *t.a.node(id)
	if node.kind in [.fn_literal, .lambda_expr] {
		return
	}
	if node.kind == .index && node.children_count == 2 && node.op != .gated_index
		&& node.value != 'range' && !node.is_in_range_index() {
		base := t.a.child_node(&node, 0)
		index := t.a.child_node(&node, 1)
		if base.kind == .ident && base.value == name && index.kind == .ident
			&& index.value == loop_name {
			mut marked := node
			marked.flags |= flat.node_flag_in_range_index
			t.set_node(int(id), marked)
		}
	}
	for i in 0 .. node.children_count {
		t.mark_in_range_indexes(t.a.child(&node, i), name, loop_name)
	}
}
