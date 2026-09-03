module transform

import v3.flat
import v3.types

struct ArrayMapLoopPointerExit {
	origins     map[string]bool
	label       string
	defer_count int
	is_continue bool
	is_goto     bool
}

struct ArrayMapReturnPointerExit {
	origins     map[string]bool
	defer_count int
}

// make_array_new_call builds make array new call data for transform.
fn (mut t Transformer) make_array_new_call(elem_type string, len_expr flat.NodeId, cap_expr flat.NodeId) flat.NodeId {
	// `[]shared T` stores pointers to lock wrappers, not inline T values.
	storage_size_type := if elem_type.trim_space().starts_with('shared ') {
		'&void'
	} else {
		elem_type
	}
	return t.make_call_typed('array_new', [t.make_sizeof_type(storage_size_type), len_expr, cap_expr], '[]${elem_type}')
}

fn shared_array_inner_type_text(raw string) ?string {
	mut clean := raw.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	if !clean.starts_with('[]') {
		return none
	}
	elem := clean[2..].trim_space()
	if elem.starts_with('shared ') {
		return elem[7..].trim_space()
	}
	return none
}

fn (t &Transformer) shared_array_lhs_inner_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return shared_array_inner_type_text(t.raw_var_type(node.value))
	} else if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		mut base_type := t.raw_expr_type_without_smartcast(base_id)
		if base_type.len == 0 {
			base_type = t.original_expr_type(base_id)
		}
		raw, owner_type := t.lookup_struct_field_raw_type_with_owner(t.trim_pointer_type(base_type), node.value) or { return none }
		inner := shared_array_inner_type_text(raw) or { return none }
		return t.normalize_field_type(inner, owner_type)
	} else {
		return shared_array_inner_type_text(node.typ)
	}
}

fn (t &Transformer) ownership_array_repeat_call_expands(node flat.Node) bool {
	if node.children_count != 2 || isnil(t.tc) {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'repeat' || fn_node.children_count == 0 {
		return false
	}
	base_type := t.normalize_type_alias(t.node_type(t.a.child(fn_node, 0)).trim_left('&'))
	if !base_type.starts_with('[]') {
		return false
	}
	elem := t.tc.parse_type(base_type[2..])
	if !t.tc.ownership_type_requires_destruction(elem) {
		return false
	}
	return t.tc.ownership_default_clone_missing_method(elem) == none
}

fn (t &Transformer) interface_array_literal_repeat_can_expand(base_id flat.NodeId, count_id flat.NodeId, base_type string) bool {
	if isnil(t.tc) || !base_type.starts_with('[]') {
		return false
	}
	elem_type := base_type[2..]
	if elem_type !in t.tc.interface_names && t.tc.qualify_name(elem_type) !in t.tc.interface_names {
		return false
	}
	base := t.a.nodes[int(base_id)]
	count_node := t.a.nodes[int(count_id)]
	if base.kind != .array_literal || count_node.kind != .int_literal {
		return false
	}
	count := count_node.value.int()
	return count >= 0 && count <= 32 && t.array_repeat_literal_can_duplicate(base)
}

fn (t &Transformer) interface_array_literal_repeat_call_expands(node flat.Node) bool {
	if node.children_count != 2 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'repeat' || fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(fn_node, 0)
	return t.interface_array_literal_repeat_can_expand(base_id, t.a.child(&node, 1), t.node_type(base_id))
}

fn (mut t Transformer) try_lower_array_repeat_call(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'repeat' || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	count_id := t.a.child(&node, 1)
	if expanded := t.try_expand_interface_array_literal_repeat(base_id, count_id, base_type) {
		return expanded
	}
	clean_base_type := t.normalize_type_alias(base_type.trim_left('&'))
	if !isnil(t.tc) && clean_base_type.starts_with('[]') {
		elem_type := clean_base_type[2..]
		elem := t.tc.parse_type(elem_type)
		if t.tc.ownership_type_requires_destruction(elem) {
			// The checker reports the missing clone method. Do not lower the rejected
			// repeat to byte copies while processing the invalid program.
			if _ := t.tc.ownership_default_clone_missing_method(elem) {
				return t.make_empty()
			}
			return t.make_owned_array_repeat_value(base_id, count_id, clean_base_type)
		}
		base_is_owned_temporary := !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
		if base_is_owned_temporary {
			return t.make_plain_array_repeat_value(base_id, count_id, clean_base_type)
		}
	}
	depth := array_repeat_clone_depth(base_type)
	if depth == 0 {
		return none
	}
	base := t.transform_expr(base_id)
	count := t.transform_expr(count_id)
	selector := t.make_selector(base, 'repeat_to_depth', '')
	return t.make_call_expr_typed(selector, [count, t.make_int_literal(depth)], node.typ)
}

// make_plain_array_repeat_value preserves the repeated result before freeing the backing
// storage of a non-addressable source array materialized after ownership analysis. The
// repeated result owns the shallow-copied elements, so the source elements must not be dropped.
fn (mut t Transformer) make_plain_array_repeat_value(base_id flat.NodeId, count_id flat.NodeId, array_type string) flat.NodeId {
	source := t.transform_expr(base_id)
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type, 'plain_array_repeat_source')
	count := t.transform_expr_for_type(count_id, 'int')
	repeat_selector := t.make_selector(stable_source, 'repeat_to_depth', '')
	clone_depth := array_repeat_clone_depth(array_type)
	repeated := t.make_call_expr_typed(repeat_selector, [count, t.make_int_literal(clone_depth)], array_type)
	out_name := t.new_temp('plain_array_repeat')
	t.pending_stmts << t.make_decl_assign_typed(out_name, repeated, array_type)
	t.pending_stmts << t.make_expr_stmt(t.make_method_call(stable_source, 'free', []flat.NodeId{}))
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// make_owned_array_repeat_value replaces every byte-copied element in the runtime repeat
// result with an independent clone before ownership destruction can observe the array.
fn (mut t Transformer) make_owned_array_repeat_value(base_id flat.NodeId, count_id flat.NodeId, array_type string) flat.NodeId {
	elem_type := array_type[2..]
	base_type := t.node_type(base_id)
	// Classify the source before transforming it because literals can lower to addressable
	// synthetic identifiers that still need explicit destruction.
	source_is_owned_temporary := !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
	mut source := t.transform_expr(base_id)
	if base_type.starts_with('&') {
		source = t.make_prefix(.mul, source)
		t.set_node_typ(int(source), array_type)
	}
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type, 'owned_array_repeat_source')
	count := t.transform_expr_for_type(count_id, 'int')
	repeat_selector := t.make_selector(stable_source, 'repeat_to_depth', '')
	storage_repeat := t.make_call_expr_typed(repeat_selector, [count, t.make_int_literal(0)], array_type)
	out_name := t.new_temp('owned_array_repeat')
	idx_name := t.new_temp('owned_array_repeat_idx')
	t.pending_stmts << t.make_decl_assign_typed(out_name, storage_repeat, array_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(out_name), 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	shallow_elem := t.array_get_value(t.make_ident(out_name), t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(shallow_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

fn (mut t Transformer) try_expand_interface_array_literal_repeat(base_id flat.NodeId, count_id flat.NodeId, base_type string) ?flat.NodeId {
	if !t.interface_array_literal_repeat_can_expand(base_id, count_id, base_type) {
		return none
	}
	base := t.a.nodes[int(base_id)]
	count_node := t.a.nodes[int(count_id)]
	count := count_node.value.int()
	mut values := []flat.NodeId{cap: int(base.children_count) * count}
	for _ in 0 .. count {
		for i in 0 .. base.children_count {
			values << t.a.child(&base, i)
		}
	}
	lit := t.make_array_literal_typed(values, base_type)
	return t.transform_array_literal(lit, t.a.nodes[int(lit)])
}

fn (t &Transformer) array_repeat_literal_can_duplicate(node flat.Node) bool {
	for i in 0 .. node.children_count {
		if !t.array_repeat_expr_can_duplicate(t.a.child(&node, i)) {
			return false
		}
	}
	return true
}

fn (t &Transformer) array_repeat_expr_can_duplicate(id flat.NodeId) bool {
	node := t.a.nodes[int(id)]
	match node.kind {
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .ident, .enum_val, .nil_literal, .none_expr {
			return true
		}
		.paren, .prefix, .postfix, .cast_expr, .as_expr, .field_init, .array_literal, .struct_init {
			for i in 0 .. node.children_count {
				if !t.array_repeat_expr_can_duplicate(t.a.child(&node, i)) {
					return false
				}
			}
			return true
		}
		else {
			return false
		}
	}
}

fn array_repeat_clone_depth(typ string) int {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	mut depth := 0
	for clean.starts_with('[]') {
		depth++
		clean = clean[2..]
	}
	if depth <= 1 {
		return 0
	}
	return depth - 1
}

fn array_nested_eq_depth(typ string) int {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	mut depth := 0
	for clean.starts_with('[]') {
		depth++
		clean = clean[2..]
	}
	if depth <= 1 {
		return 1
	}
	return depth
}

fn (mut t Transformer) make_array_push_many_call(lhs_addr flat.NodeId, rhs flat.NodeId, rhs_type string) flat.NodeId {
	t.mark_fn_used('array__push_many')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'push_many')
	return t.make_call_typed('array__push_many', [lhs_addr,
		t.make_selector(rhs_value, 'data', 'voidptr'), t.make_selector(rhs_value, 'len', 'int')], 'void')
}

fn (mut t Transformer) make_array_insert_many_call(lhs_addr flat.NodeId, index flat.NodeId, rhs flat.NodeId, rhs_type string) flat.NodeId {
	if t.is_fixed_array_type(rhs_type) {
		return t.make_call_typed('array__insert_many', [lhs_addr, index, rhs,
			t.make_fixed_array_len_expr(rhs_type)], 'void')
	}
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'insert_many')
	return t.make_call_typed('array__insert_many', [lhs_addr, index,
		t.make_selector(rhs_value, 'data', 'voidptr'), t.make_selector(rhs_value, 'len', 'int')], 'void')
}

fn (mut t Transformer) finish_borrowed_array_insert_many_call(call flat.NodeId, rhs flat.NodeId, rhs_type string, cloned bool) flat.NodeId {
	if !cloned || t.is_fixed_array_type(rhs_type) {
		return call
	}
	// insert_many transfers the cloned element bytes. Run the insertion before freeing only
	// the temporary array's backing buffer; the destination now owns its elements.
	t.pending_stmts << t.make_expr_stmt(call)
	return t.make_method_call(rhs, 'free', []flat.NodeId{})
}

fn (mut t Transformer) transform_array_many_rhs(id flat.NodeId, node flat.Node, array_type string) flat.NodeId {
	if node.kind == .array_literal {
		return t.transform_array_literal_for_type(id, node, array_type) or { t.transform_expr(id) }
	}
	return t.transform_expr(id)
}

fn (mut t Transformer) make_array_clone_call(base_id flat.NodeId, base_type string) flat.NodeId {
	t.mark_fn_used('array__clone')
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	// Classify the source before transformation can lower a literal to an addressable temp.
	source_is_owned_temporary := clean_type.starts_with('[]') && !isnil(t.tc)
		&& !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type))
	mut receiver := t.transform_expr(base_id)
	transformed_receiver_type := t.node_type(receiver)
	effective_base_type := if transformed_receiver_type.starts_with('&')
		&& t.normalize_type_alias(transformed_receiver_type[1..]) == clean_type {
		transformed_receiver_type
	} else {
		base_type
	}
	if clean_type.starts_with('[]') && !isnil(t.tc) {
		elem_type := t.tc.parse_type(clean_type[2..])
		if !t.tc.ownership_type_requires_destruction(elem_type) {
			if source_is_owned_temporary {
				return t.make_array_clone_from_owned_temporary(receiver, clean_type)
			}
			return t.make_array_clone_value(receiver, effective_base_type)
		}
		// The checker rejects this call. Do not lower it to the unsafe raw clone while
		// processing the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(elem_type) {
			return receiver
		}
		if effective_base_type.starts_with('&') {
			receiver = t.make_prefix(.mul, receiver)
			t.set_node_typ(int(receiver), clean_type)
		}
		return t.make_compiler_default_array_clone_value(receiver, clean_type, source_is_owned_temporary)
	}
	return t.make_array_clone_value(receiver, effective_base_type)
}

// make_array_clone_from_owned_temporary saves a non-addressable source until its backing
// storage has been cloned, then destroys the generated source temp before returning the clone.
fn (mut t Transformer) make_array_clone_from_owned_temporary(source flat.NodeId, array_type string) flat.NodeId {
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type, 'array_clone_source')
	out_name := t.new_temp('array_clone')
	cloned := t.make_array_clone_value(stable_source, array_type)
	t.pending_stmts << t.make_decl_assign_typed(out_name, cloned, array_type)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
		stable_source,
	], 'void'))
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// make_array_reverse_call clones array storage and ownership-bearing elements before
// reversing the new array in place, so the source and result never share ownership.
fn (mut t Transformer) make_array_reverse_call(base_id flat.NodeId, base_type string) flat.NodeId {
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if clean_type.starts_with('[]') && !isnil(t.tc) {
		elem_type := t.tc.parse_type(clean_type[2..])
		if t.tc.ownership_type_requires_destruction(elem_type) {
			// The checker rejects this call. Do not mutate the source while processing the
			// invalid program.
			if _ := t.tc.ownership_default_clone_missing_method(elem_type) {
				mut receiver := t.transform_expr(base_id)
				if base_type.starts_with('&') {
					receiver = t.make_prefix(.mul, receiver)
					t.set_node_typ(int(receiver), clean_type)
				}
				return receiver
			}
		}
		clone := t.make_array_clone_call(base_id, base_type)
		stable_clone := t.stable_transformed_expr_for_reuse(clone, clean_type, 'owned_reverse')
		t.mark_fn_used('array__reverse_in_place')
		reverse := t.make_call_typed('array__reverse_in_place', [
			t.runtime_addr(stable_clone, clean_type),
		], 'void')
		t.pending_stmts << t.make_expr_stmt(reverse)
		return stable_clone
	}
	mut receiver := t.transform_expr(base_id)
	if base_type.starts_with('&') {
		receiver = t.make_prefix(.mul, receiver)
		t.set_node_typ(int(receiver), clean_type)
	}
	return t.make_call_typed('array__reverse', [receiver], clean_type)
}

fn (mut t Transformer) make_array_clone_value(receiver flat.NodeId, base_type string) flat.NodeId {
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	depth := array_repeat_clone_depth(clean_type)
	if depth > 0 {
		return t.make_call_typed('array__clone_to_depth', [
			t.runtime_addr(receiver, base_type),
			t.make_int_literal(depth),
		], clean_type)
	}
	return t.make_call_typed('array__clone', [t.runtime_addr(receiver, base_type)], clean_type)
}

fn (mut t Transformer) clone_nested_array_spread_value(spread flat.NodeId, spread_type string) flat.NodeId {
	if array_repeat_clone_depth(t.normalize_type_alias(spread_type)) > 0 {
		return t.make_array_clone_value(spread, spread_type)
	}
	return spread
}

fn (t &Transformer) array_init_elem_type_name(id flat.NodeId, node flat.Node) string {
	if node.value.starts_with('typeof(') {
		array_type := t.node_type(id)
		if array_type.starts_with('[]') {
			return array_type[2..]
		}
	}
	return node.value
}

// lower_array_init_to_runtime converts lower array init to runtime data for transform.
fn (mut t Transformer) lower_array_init_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	elem_value := t.array_init_elem_type_name(id, node)
	if elem_value.len == 0 {
		return id
	}
	clean_value := t.normalize_type_alias(elem_value)
	if t.is_fixed_array_type(clean_value) && !node.typ.starts_with('[]') {
		return id
	}
	elem_type := if node.typ.starts_with('[]') {
		node.typ[2..]
	} else if !elem_value.starts_with('[]') && clean_value.starts_with('[]') {
		clean_value[2..]
	} else {
		elem_value
	}
	mut len_expr := t.make_int_literal(0)
	mut cap_expr := t.make_int_literal(0)
	mut init_expr := flat.empty_node
	mut init_expr_id := flat.empty_node
	// Source (child) position of the last `len`/`cap` field whose value hoists a value branch
	// — directly or nested inside a compound field value (`cap: 1 + (match ...)`) — so an
	// earlier side-effecting `len`/`cap` field can be stabilized before that field hoists its
	// materialization prelude, preserving field evaluation order (both are evaluated into
	// `new_call` below; `init` is per-element in the loop body).
	mut last_lencap_branch := -1
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .field_init && child.children_count > 0 && child.value in [
			'len',
			'cap',
		] {
			if t.operand_hoists_value_branch(t.a.child(child, 0)) {
				last_lencap_branch = i
			}
		}
	}
	mut has_len := false
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .field_init && child.children_count > 0 {
			if child.value == 'len' {
				// Typed value lowering so a value `match`/`if` len field (e.g.
				// `[]int{len: match node { ... lower(node)! ... }}`) is materialized as a
				// value instead of lowering its propagating arm in a statement context.
				has_len = true
				mut val := t.transform_expr_for_type(t.a.child(child, 0), 'int')
				if i < last_lencap_branch && t.operand_needs_ordering_snapshot(val) {
					val = t.snapshot_transformed_expr_for_reuse(val, 'int', 'arr_len')
				}
				len_expr = val
			} else if child.value == 'cap' {
				mut val := t.transform_expr_for_type(t.a.child(child, 0), 'int')
				if i < last_lencap_branch && t.operand_needs_ordering_snapshot(val) {
					val = t.snapshot_transformed_expr_for_reuse(val, 'int', 'arr_cap')
				}
				cap_expr = val
			} else if child.value == 'init' {
				init_expr_id = t.a.child(child, 0)
			}
		}
	}
	new_call := t.make_array_new_call(elem_type, len_expr, cap_expr)
	// Capacity reserves storage but creates no elements. In particular, do not
	// synthesize a default-value fill loop merely because `{cap: n}` has a field
	// child: its runtime length is zero and no element initializer is needed.
	if node.children_count == 0 || !has_len {
		return new_call
	}
	if int(init_expr_id) < 0 {
		clean_elem_type := t.normalize_type_alias(elem_type)
		if clean_elem_type.starts_with('[]') {
			init_expr = t.make_array_new_call(clean_elem_type[2..], t.make_int_literal(0), t.make_int_literal(0))
		} else if clean_elem_type.starts_with('map[') {
			init_expr = t.zero_value_for_type(clean_elem_type)
		} else if default_value := t.make_struct_runtime_default_value(clean_elem_type) {
			init_expr = default_value
		} else {
			return new_call
		}
	}
	tmp_name := t.new_temp('arr_init')
	idx_name := t.new_temp('arr_idx')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, new_call, '[]${elem_type}')
	init_idx := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(tmp_name), 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_lhs := t.make_index(t.make_ident(tmp_name), t.make_ident(idx_name), elem_type)
	// `init:` expressions may reference the magic `index` variable, which V binds to
	// the current element index. Declare it inside the loop body so it resolves to the
	// generated loop counter instead of leaking to an external symbol (e.g. libc `index`).
	index_decl := t.make_decl_assign_typed('index', t.make_ident(idx_name), 'int')
	mut loop_body := []flat.NodeId{}
	loop_body << index_decl
	if int(init_expr_id) >= 0 {
		saved_pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		indexed_init := t.substitute_ident_expr(init_expr_id, 'index', t.make_ident(idx_name))
		// Typed value lowering so a value `match`/`if` init field is materialized as a value.
		init_expr = t.transform_expr_for_type(indexed_init, elem_type)
		// The source-level initializer is evaluated once for every generated element.
		// Keep a borrowed projection owned by its source by cloning it inside this loop,
		// so each element also receives independent owned storage.
		init_expr = t.clone_borrowed_projection(init_expr_id, init_expr, elem_type)
		init_pending := t.pending_stmts.clone()
		t.pending_stmts = saved_pending
		for stmt in init_pending {
			loop_body << stmt
		}
	}
	mut assign_value := init_expr
	clean_elem_type := t.normalize_type_alias(elem_type)
	if clean_elem_type.starts_with('[]') {
		if !t.expr_can_take_address(assign_value) {
			value_name := t.new_temp('arr_init_val')
			loop_body << t.make_decl_assign_typed(value_name, assign_value, clean_elem_type)
			assign_value = t.make_ident(value_name)
		}
		assign_value = t.make_array_clone_value(assign_value, clean_elem_type)
	}
	assign := t.make_assign(elem_lhs, assign_value)
	loop_body << assign
	// This synthetic default-fill loop carries no user code and owns nothing, so
	// it must not consume an ownership-drop loop-iteration slot. The checker only
	// indexed the source-level loops; if this transform-inserted loop claimed a
	// slot, the following real loop's per-iteration drops would be emitted here
	// (referencing a variable not in scope) and dropped from their real loop.
	for_id := t.make_for_stmt(init_idx, cond, post, loop_body, node)
	t.a.nodes[int(for_id)].skip_ownership_drops = true
	t.pending_stmts << for_id
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), '[]${elem_type}')
	return result
}

fn (mut t Transformer) make_struct_runtime_default_value(struct_type string) ?flat.NodeId {
	mut visited := map[string]bool{}
	return t.make_struct_runtime_default_value_guarded(struct_type, mut visited)
}

fn (mut t Transformer) make_struct_runtime_default_value_guarded(struct_type string, mut visited map[string]bool) ?flat.NodeId {
	// A reference-typed field defaults to nil, which the zeroed element already
	// is; expanding it would build `(T*){<struct defaults>}` — invalid C
	// (lookup_struct_info resolves `&mod.T` texts through its direct fallback).
	if struct_type.starts_with('&') {
		return none
	}
	if t.resolve_sum_name(struct_type) in t.sum_types {
		return none
	}
	// Name lookups can resolve cycles (e.g. same-named types across modules), so guard
	// the current expansion path against re-entering a type.
	if struct_type in visited {
		return none
	}
	visited[struct_type] = true
	defer {
		visited.delete(struct_type)
	}
	info := t.lookup_struct_info(struct_type) or { return none }
	mut field_ids := []flat.NodeId{}
	old_module := t.cur_module
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	defer {
		t.cur_module = old_module
	}
	for field in info.fields {
		field_type := t.lookup_struct_field_type(struct_type, field.name) or {
			if field.typ.len > 0 { field.typ } else { field.raw_typ }
		}
		clean_type := t.normalize_type_alias(field_type)
		// An `?T`/`!T` field with no explicit default is `none`/the zero value,
		// which the zeroed array element already provides. Never expand it into a
		// runtime default of its base struct: cross-module `normalize_type_alias`
		// can strip the `?`/`!`, which would otherwise emit the base struct's
		// fields into the optional wrapper (`(Optional_T){<T fields>}`).
		raw_field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		field_is_optional := field_type.starts_with('?') || field_type.starts_with('!')
			|| raw_field_type.starts_with('?') || raw_field_type.starts_with('!')
			|| clean_type.starts_with('?') || clean_type.starts_with('!')
		mut value := flat.empty_node
		if field_is_optional && int(field.default_expr) < 0 {
			continue
		}
		if int(field.default_expr) >= 0 {
			default_node := t.a.nodes[int(field.default_expr)]
			enum_field_type := t.enum_type_name_for_expected(field_type, info.module)
			sum_field_type := t.struct_field_sum_type(field_type, info.module)
			value = if default_node.kind == .enum_val && enum_field_type.len > 0 {
				t.transform_enum_shorthand(field.default_expr, default_node, enum_field_type)
			} else if sum_field_type.len > 0 {
				t.wrap_sum_value(field.default_expr, sum_field_type)
			} else {
				t.transform_expr_for_type(field.default_expr, field_type)
			}
		} else if clean_type.starts_with('map[') || clean_type.starts_with('[]') {
			value = t.zero_value_for_type(clean_type)
		} else if nested := t.make_struct_runtime_default_value_guarded(clean_type, mut visited) {
			value = nested
		}
		if int(value) < 0 {
			continue
		}
		start := t.a.children.len
		t.a.children << value
		field_ids << t.a.add_node(flat.Node{
			kind: .field_init
			children_start: start
			children_count: 1
			value: field.name
			typ: field_type
		})
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	return t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value: struct_type
		typ: struct_type
	})
}

fn (mut t Transformer) transform_owned_array_literal_element(elem_id flat.NodeId, elem_type string) flat.NodeId {
	value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(elem_id, elem_type)
	} else {
		t.transform_expr_for_type(elem_id, elem_type)
	}
	return t.clone_borrowed_projection(elem_id, value, elem_type)
}

// lower_array_literal_to_runtime converts lower array literal to runtime data for transform.
fn (mut t Transformer) lower_array_literal_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	if t.in_const_init {
		return id
	}
	if t.array_literal_can_emit_direct(node) {
		return id
	}
	array_type := if elem_type := t.array_literal_pointer_value_elem_type(node) {
		'[]${elem_type}'
	} else if checker_alias_type := t.array_literal_checker_alias_type(id) {
		checker_alias_type
	} else if alias_type := t.array_literal_alias_type(node) {
		alias_type
	} else {
		t.node_type(id)
	}
	if !array_type.starts_with('[]') {
		return id
	}
	elem_type := array_type[2..]
	tmp_name := t.new_temp('arr_lit')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(node.children_count)), array_type)
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		elem := t.a.nodes[int(elem_id)]
		if elem.kind == .prefix && elem.value == '...' && elem.children_count > 0 {
			spread_id := t.a.child(&elem, 0)
			t.append_array_literal_spread(tmp_name, spread_id, array_type, elem_type)
			continue
		}
		value_name := t.new_temp('arr_val')
		value := t.transform_owned_array_literal_element(elem_id, elem_type)
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		call := t.make_call_typed('array_push', [
			t.make_prefix(.amp, t.make_ident(tmp_name)),
			t.make_prefix(.amp, t.make_ident(value_name)),
		], 'void')
		t.pending_stmts << t.make_expr_stmt(call)
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// array_literal_can_emit_direct reports whether C can evaluate the literal elements
// without changing V's left-to-right expression ordering.
fn (t &Transformer) array_literal_can_emit_direct(node flat.Node) bool {
	if node.kind != .array_literal || node.children_count == 0 {
		return false
	}
	for i in 0 .. node.children_count {
		child := t.a.nodes[int(t.a.child(&node, i))]
		if child.kind !in [.ident, .int_literal, .float_literal, .bool_literal, .char_literal,
			.string_literal, .enum_val, .nil_literal, .none_expr] {
			return false
		}
	}
	return true
}

// append_array_literal_spread appends independent element clones when the destination
// array will own and destroy its elements. Plain-data spreads keep the runtime bulk copy.
fn (mut t Transformer) append_array_literal_spread(out_name string, spread_id flat.NodeId, array_type string, elem_type string) {
	source_is_owned_temporary := !t.expr_can_take_address(spread_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(array_type))
	mut needs_element_clone := false
	if !isnil(t.tc) {
		elem := t.tc.parse_type(elem_type)
		needs_element_clone = t.tc.ownership_type_requires_destruction(elem)
		if needs_element_clone {
			// The checker reports the missing clone method. Do not append shallow element
			// copies while processing the invalid spread.
			if _ := t.tc.ownership_default_clone_missing_method(elem) {
				return
			}
		}
	}
	mut spread := t.transform_expr_for_type(spread_id, array_type)
	spread_type := if t.node_type(spread_id).len > 0 {
		t.node_type(spread_id)
	} else {
		array_type
	}
	if !needs_element_clone {
		spread = t.clone_nested_array_spread_value(spread, spread_type)
		stable_source := t.stable_transformed_expr_for_reuse(spread, array_type, 'array_spread_source')
		call := t.make_array_push_many_call(t.make_prefix(.amp, t.make_ident(out_name)), stable_source, array_type)
		t.pending_stmts << t.make_expr_stmt(call)
		if source_is_owned_temporary {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				stable_source,
			], 'void'))
		}
		return
	}
	stable_source := t.stable_transformed_expr_for_reuse(spread, array_type, 'owned_array_spread_source')
	idx_name := t.new_temp('owned_array_spread_idx')
	value_name := t.new_temp('owned_array_spread_value')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(stable_source, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.array_get_value(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_decl_assign_typed(value_name, cloned_elem, elem_type)
	body << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(out_name)),
		t.make_prefix(.amp, t.make_ident(value_name)),
	], 'void'))
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
}

fn (t &Transformer) array_literal_checker_alias_type(id flat.NodeId) ?string {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	typ := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	name := typ.name()
	if !name.starts_with('[]') {
		return none
	}
	elem := name[2..]
	if local_elem := t.array_literal_local_struct_elem_name(t.a.nodes[int(id)]) {
		if elem.all_after_last('.') == local_elem {
			return '[]${local_elem}'
		}
	}
	source_name := t.a.nodes[int(id)].typ
	if elem.contains('.') && source_name.starts_with('[]')
		&& source_name[2..].all_after_last('.') == elem.all_after_last('.') {
		return name
	}
	if !t.generic_arg_is_alias_name(elem, t.cur_module) {
		return none
	}
	return '[]${t.array_literal_qualified_alias_name(elem)}'
}

fn (t &Transformer) array_literal_local_struct_elem_name(node flat.Node) ?string {
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	first_id := t.array_literal_alias_expr_id(t.a.child(&node, 0))
	first := t.a.nodes[int(first_id)]
	if first.kind == .struct_init {
		for candidate in [first.value, first.typ] {
			if t.bare_struct_name_is_local_to_current_module(candidate) {
				return candidate
			}
		}
	}
	if first.kind == .ident {
		raw_type := t.raw_var_type(first.value)
		if t.bare_struct_name_is_local_to_current_module(raw_type) {
			return raw_type
		}
	}
	return none
}

fn (t &Transformer) array_literal_qualified_alias_name(name string) string {
	clean := name.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return clean
	}
	if clean in t.tc.type_aliases {
		return clean
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.tc.type_aliases {
			return qname
		}
	}
	if clean.contains('.') {
		return clean
	}
	mut found := ''
	for alias, _ in t.tc.type_aliases {
		if alias.all_after_last('.') != clean {
			continue
		}
		if found.len > 0 && found != alias {
			return clean
		}
		found = alias
	}
	if found.len > 0 {
		return found
	}
	return clean
}

fn (t &Transformer) array_literal_alias_type(node flat.Node) ?string {
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	first_id := t.array_literal_alias_expr_id(t.a.child(&node, 0))
	first := t.a.nodes[int(first_id)]
	if first.kind == .prefix && first.value == '...' {
		return none
	}
	mut alias_name := t.raw_alias_type_for_expr(first_id)
	if alias_name.len == 0 {
		alias_name = t.array_literal_alias_expr_name(first)
	}
	if alias_name.len == 0 {
		return none
	}
	if !t.generic_arg_is_alias_name(alias_name, t.cur_module) {
		return none
	}
	return '[]${t.array_literal_qualified_alias_name(alias_name)}'
}

fn (t &Transformer) array_literal_alias_expr_id(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.array_literal_alias_expr_id(t.a.child(&node, 0))
	}
	return id
}

fn (t &Transformer) array_literal_alias_expr_name(node flat.Node) string {
	if node.kind in [.cast_expr, .as_expr] && node.value.len > 0 {
		return node.value
	}
	if node.kind == .call && node.children_count > 0 {
		name := t.generic_call_type_arg_name(t.a.child(&node, 0))
		if name.len > 0 {
			return name
		}
	}
	for candidate in [node.typ, node.value] {
		if candidate.len > 0 && t.generic_arg_is_alias_name(candidate, t.cur_module) {
			return candidate
		}
	}
	return ''
}

// transform_array_literal_for_type transforms transform array literal for type data for transform.
fn (mut t Transformer) transform_array_literal_for_type(id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if t.in_const_init {
		return none
	}
	target_array_type := t.normalize_type_alias(target_type)
	array_type := if target_array_type.starts_with('[]')
		&& t.is_sum_type_name(target_array_type[2..]) {
		target_array_type
	} else if checker_alias_type := t.array_literal_checker_alias_type(id) {
		checker_alias_type
	} else if alias_type := t.array_literal_alias_type(node) {
		alias_type
	} else {
		target_array_type
	}
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	if t.array_literal_can_emit_direct(node) {
		mut values := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			elem_id := t.a.child(&node, i)
			values << t.transform_owned_array_literal_element(elem_id, elem_type)
		}
		return t.make_array_literal_typed(values, array_type)
	}
	tmp_name := t.new_temp('arr_lit')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(node.children_count)), array_type)
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		elem := t.a.nodes[int(elem_id)]
		if elem.kind == .prefix && elem.value == '...' && elem.children_count > 0 {
			t.append_array_literal_spread(tmp_name, t.a.child(&elem, 0), array_type, elem_type)
			continue
		}
		value_name := t.new_temp('arr_val')
		value := t.transform_owned_array_literal_element(elem_id, elem_type)
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		call := t.make_call_typed('array_push', [
			t.make_prefix(.amp, t.make_ident(tmp_name)),
			t.make_prefix(.amp, t.make_ident(value_name)),
		], 'void')
		t.pending_stmts << t.make_expr_stmt(call)
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), array_type)
	return result
}

fn (mut t Transformer) transform_fixed_array_literal_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	fixed_type := t.normalize_type_alias(target_type)
	if !t.is_fixed_array_type(fixed_type) {
		return none
	}
	elem_type := fixed_array_elem_type(fixed_type)
	ordered_temps := t.fixed_array_literal_needs_ordered_temps(node)
	mut values := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		transformed := t.transform_expr_for_type(elem_id, elem_type)
		value := t.clone_borrowed_projection(elem_id, transformed, elem_type)
		if ordered_temps {
			tmp_name := t.new_temp('fixed_arr_val')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, elem_type)
			values << t.make_ident(tmp_name)
			continue
		}
		values << value
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (t &Transformer) fixed_array_literal_needs_ordered_temps(node flat.Node) bool {
	for i in 0 .. node.children_count {
		if t.fixed_array_literal_child_needs_ordered_temp(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) fixed_array_literal_child_needs_ordered_temp(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.if_expr, .match_stmt, .block, .or_expr] {
		return true
	}
	for i in 0 .. node.children_count {
		if t.fixed_array_literal_child_needs_ordered_temp(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) transform_fixed_array_init_expr(node flat.Node) ?flat.NodeId {
	fixed_type := t.resolved_fixed_array_canonical_type(t.normalize_type_alias(if node.typ.len > 0 {
		node.typ
	} else {
		node.value
	}))
	if !t.is_fixed_array_type(fixed_type) {
		return none
	}
	len := t.fixed_array_len_value(fixed_type) or { return none }
	elem_type := fixed_array_elem_type(fixed_type)
	if node.children_count == 0 {
		if !t.fixed_array_empty_init_needs_values(elem_type) {
			return none
		}
		return t.make_fixed_array_empty_literal(fixed_type, len, elem_type)
	}
	mut init_id := flat.empty_node
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.value == 'init' && child.children_count > 0 {
			init_id = t.a.child(&child, 0)
			break
		}
	}
	if int(init_id) < 0 {
		return none
	}
	mut values := []flat.NodeId{cap: len}
	for i in 0 .. len {
		indexed_init := t.substitute_ident_expr(init_id, 'index', t.make_int_literal(i))
		value := t.transform_expr_for_type(indexed_init, elem_type)
		values << t.clone_borrowed_projection(init_id, value, elem_type)
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (mut t Transformer) fixed_array_len_value(fixed_type string) ?int {
	len_text := fixed_array_len_text(fixed_type)
	if is_decimal_text(len_text) {
		return len_text.int()
	}
	if !isnil(t.tc) {
		return t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) or { none }
	}
	return none
}

fn (mut t Transformer) make_fixed_array_empty_literal(fixed_type string, len int, elem_type string) flat.NodeId {
	mut values := []flat.NodeId{cap: len}
	for _ in 0 .. len {
		values << t.fixed_array_empty_elem_value(elem_type)
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (mut t Transformer) fixed_array_empty_elem_value(elem_type string) flat.NodeId {
	clean_type := t.normalize_type_alias(elem_type)
	if t.is_fixed_array_type(clean_type) {
		fixed_type := t.resolved_fixed_array_canonical_type(clean_type)
		if len := t.fixed_array_len_value(fixed_type) {
			return t.make_fixed_array_empty_literal(fixed_type, len, fixed_array_elem_type(fixed_type))
		}
		return t.make_fixed_array_init(fixed_type)
	}
	if clean_type.starts_with('[]') {
		return t.make_array_init(clean_type[2..])
	}
	if clean_type.starts_with('map[') || clean_type.starts_with('chan ') {
		return t.zero_value_for_type(clean_type)
	}
	if default_value := t.make_struct_runtime_default_value(clean_type) {
		return default_value
	}
	return t.zero_value_for_type(clean_type)
}

fn (mut t Transformer) fixed_array_empty_init_needs_values(elem_type string) bool {
	mut visited := map[string]bool{}
	return t.fixed_array_empty_init_needs_values_guarded(elem_type, mut visited)
}

fn (mut t Transformer) fixed_array_empty_init_needs_values_guarded(elem_type string, mut visited map[string]bool) bool {
	clean_type := t.normalize_type_alias(elem_type)
	if clean_type.starts_with('[]') || clean_type.starts_with('map[')
		|| clean_type.starts_with('chan ') {
		return true
	}
	if t.is_fixed_array_type(clean_type) {
		fixed_type := t.resolved_fixed_array_canonical_type(clean_type)
		return t.fixed_array_empty_init_needs_values_guarded(fixed_array_elem_type(fixed_type), mut visited)
	}
	return t.struct_type_needs_runtime_default(clean_type, mut visited)
}

fn (mut t Transformer) struct_type_needs_runtime_default(struct_type string, mut visited map[string]bool) bool {
	if struct_type.starts_with('&') || t.resolve_sum_name(struct_type) in t.sum_types {
		return false
	}
	if struct_type in visited {
		return false
	}
	visited[struct_type] = true
	defer {
		visited.delete(struct_type)
	}
	info := t.lookup_struct_info(struct_type) or { return false }
	old_module := t.cur_module
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	defer {
		t.cur_module = old_module
	}
	for field in info.fields {
		if int(field.default_expr) >= 0 {
			return true
		}
		field_type := t.lookup_struct_field_type(struct_type, field.name) or {
			if field.typ.len > 0 { field.typ } else { field.raw_typ }
		}
		if t.fixed_array_empty_init_needs_values_guarded(field_type, mut visited) {
			return true
		}
	}
	return false
}

// transform_empty_array_init_for_type supports transform_empty_array_init_for_type handling.
fn (mut t Transformer) transform_empty_array_init_for_type(node flat.Node, target_type string) ?flat.NodeId {
	if node.value.len > 0 || node.children_count > 0 {
		return none
	}
	array_type := t.normalize_type_alias(target_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	return t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(0))
}

fn (mut t Transformer) transform_array_value_for_dynamic_target(value_id flat.NodeId, target_type string) ?flat.NodeId {
	if int(value_id) < 0 || target_type.len == 0 || isnil(t.tc) {
		return none
	}
	expected_name := t.normalize_type_alias(target_type).trim_space()
	if !expected_name.starts_with('[]') {
		return none
	}
	actual_name := t.normalize_type_alias(t.node_type(value_id)).trim_space()
	if actual_name.len == 0 {
		return none
	}
	expected_type := t.tc.parse_type(expected_name)
	actual_type := t.tc.parse_type(actual_name)
	expected_base := forwarded_return_unalias_type(expected_type)
	actual_base := forwarded_return_unalias_type(actual_type)
	if expected_base is types.Array {
		if actual_base is types.Array {
			if !t.forwarded_slot_conversion_supported(actual_base.elem_type, expected_base.elem_type) {
				return none
			}
			return t.convert_forwarded_array_to_dynamic(value_id, actual_type, actual_base.elem_type, expected_type, expected_base.elem_type, false)
		}
		if actual_base is types.ArrayFixed {
			if !t.forwarded_slot_conversion_supported(actual_base.elem_type, expected_base.elem_type) {
				return none
			}
			return t.convert_forwarded_array_to_dynamic(value_id, actual_type, actual_base.elem_type, expected_type, expected_base.elem_type, true)
		}
	}
	return none
}

// try_lower_array_append_stmt supports try lower array append stmt handling for Transformer.
fn (mut t Transformer) try_lower_array_append_or_stmt(node flat.Node) ?[]flat.NodeId {
	if node.kind != .or_expr || node.children_count < 2 {
		return none
	}
	append_id := t.a.child(&node, 0)
	append := t.a.nodes[int(append_id)]
	if append.kind != .infix || append.op != .left_shift || append.children_count < 2 {
		return none
	}
	rhs_id := t.a.child(&append, 1)
	expr_type, value_type := t.or_expr_types(rhs_id, node.typ)
	if !t.is_optional_type_name(expr_type) || value_type.len == 0 || value_type == 'void' {
		return none
	}
	or_start := t.a.children.len
	t.a.children << rhs_id
	t.a.children << t.a.child(&node, 1)
	rhs_or_id := t.a.add_node(flat.Node{
		kind: .or_expr
		op: node.op
		children_start: or_start
		children_count: 2
		pos: node.pos
		value: node.value
		typ: value_type
	})
	pending_start := t.pending_stmts.len
	unwrapped_rhs := t.lower_or_expr_to_temp(rhs_or_id, t.a.nodes[int(rhs_or_id)])
	rhs_pending := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	append_start := t.a.children.len
	t.a.children << t.a.child(&append, 0)
	t.a.children << unwrapped_rhs
	lowered_id := t.a.add_node(flat.Node{
		kind: .infix
		op: .left_shift
		children_start: append_start
		children_count: 2
		pos: append.pos
		value: append.value
		typ: append.typ
	})
	if lowered := t.try_lower_map_index_append_stmt_with_prelude(lowered_id, rhs_pending) {
		return lowered
	}
	for stmt in rhs_pending {
		t.pending_stmts << stmt
	}
	if lowered := t.try_lower_shared_array_append_autolock_stmt(lowered_id) {
		return lowered
	}
	if lowered := t.try_lower_array_append_stmt(lowered_id) {
		return lowered
	}
	// This helper is only a probe. A non-array `<<` expression must fall back to
	// normal `or` lowering without retaining the optional RHS prelude here.
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	return none
}

@[direct_array_access]
fn (mut t Transformer) try_lower_array_append_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	normalized_id := t.normalize_array_append_add_rhs(id)
	if int(normalized_id) < 0 {
		return none
	}
	node := t.a.nodes[int(normalized_id)]
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if lowered := t.try_lower_optional_array_append_stmt(node, lhs_id, rhs_id) {
		return lowered
	}
	mut lhs_type := t.lvalue_type(lhs_id)
	if !array_type_has_generic_placeholder(lhs_type) {
		lhs_type = t.normalize_type_alias(lhs_type)
	}
	mut array_type := t.clean_array_append_lhs_type(lhs_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	raw_rhs_type := t.node_type(rhs_id)
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	if !push_many && t.array_append_rhs_builtin_map_elem_matches(rhs_id, elem_type) {
		push_many = true
		rhs_type = array_type
	}
	rhs_is_sum_variant := !push_many
		&& t.array_append_rhs_is_sum_variant_value(rhs_id, raw_rhs_type, elem_type)
	if rhs_node.kind == .array_literal && t.array_append_literal_should_push_many(rhs_id, elem_type) {
		// `[]scalar << [a, b, c]` always appends the literal's elements. Retype the
		// literal from the destination so a mis-inferred element type (e.g. `[]int`
		// for `[f32_expr, ..]`) is corrected and the append stays a clean push_many,
		// instead of degrading to a single push of the whole array. (An array-typed
		// element is genuinely ambiguous, so leave that to the inferred decision.)
		push_many = true
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	} else if push_many && rhs_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	}

	mut result := []flat.NodeId{}
	mut lhs := t.transform_lvalue(lhs_id)
	// For an append whose RHS hoists a value `match`/`if` prelude — directly or nested inside
	// a compound RHS (`arrays[next(mut trace)] << wrap(match ...)`) — stabilize the LHS
	// lvalue's dynamic base/index components into temps first — without spilling the mutated
	// array value — so a side-effecting index (e.g. `arrays[next(mut trace)] << (match ...)`)
	// evaluates before the RHS prelude below, preserving source order.
	if t.operand_hoists_value_branch(rhs_id) {
		lhs = t.stabilize_transformed_lvalue_for_reuse(lhs)
	}
	t.drain_pending(mut result)
	mut rhs := flat.empty_node
	if !push_many {
		if !rhs_is_sum_variant {
			if t.array_append_elem_is_interface(elem_type) {
				rhs = t.transform_expr_for_type(rhs_id, elem_type)
			} else {
				if converted := t.transform_array_value_for_dynamic_target(rhs_id, array_type) {
					rhs = converted
					rhs_type = array_type
					push_many = true
				} else {
					rhs = if elem_type in t.sum_types
						|| t.resolve_sum_name(elem_type) in t.sum_types {
						t.wrap_sum_value(rhs_id, elem_type)
					} else {
						t.transform_expr_for_type(rhs_id, elem_type)
					}
				}
			}
		} else {
			rhs = if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
				t.wrap_sum_value(rhs_id, elem_type)
			} else {
				t.transform_expr_for_type(rhs_id, elem_type)
			}
		}
	} else {
		// Route a value `match`/`if` push-many RHS (an array-producing match, e.g.
		// `out << (match node { First { values_first(node)! } ... })`) through value
		// lowering so its propagating arm tail is materialized as a value instead of in a
		// value-less statement context. Other operands keep master's
		// `transform_array_many_rhs` (array-literal typing / ownership clone) handling.
		rhs = if t.is_value_match_or_if_operand(rhs_id) {
			t.transform_value_operand(rhs_id)
		} else {
			t.transform_array_many_rhs(rhs_id, rhs_node, array_type)
		}
	}
	if !push_many {
		rhs = t.coerce_transformed_expr_to_type(rhs, rhs_id, elem_type)
		cloned_append := t.clone_borrowed_array_append_value(rhs_id, rhs, elem_type)
		rhs = if cloned_append == rhs {
			t.clone_borrowed_projection(rhs_id, rhs, elem_type)
		} else {
			cloned_append
		}
	}
	mut borrowed_push_many_clone := false
	if push_many {
		cloned_rhs, cloned := t.clone_borrowed_array_append_many_value(rhs_id, rhs, rhs_type, elem_type)
		rhs = cloned_rhs
		borrowed_push_many_clone = cloned
	}
	t.drain_pending(mut result)
	if rhs_type.len == 0 {
		rhs_type = t.node_type(rhs)
		push_many = t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	}
	mut bulk_cleanup_name := ''
	mut bulk_cleanup_type := ''
	if push_many && t.expr_contains_local_closure_field_cleanup(rhs_id) {
		bulk_cleanup_name = t.new_temp('append_closures')
		bulk_cleanup_type = if rhs_type.len > 0 { rhs_type } else { array_type }
		t.set_var_type(bulk_cleanup_name, bulk_cleanup_type)
		result << t.make_decl_assign_typed(bulk_cleanup_name, rhs, bulk_cleanup_type)
		rhs = t.make_ident(bulk_cleanup_name)
		t.set_node_typ(int(rhs), bulk_cleanup_type)
	}

	lhs_addr := t.runtime_addr(lhs, lhs_type)
	if push_many {
		call := if t.is_fixed_array_type(rhs_type) {
			t.make_call_typed('array_push_many_ptr', [lhs_addr, rhs,
				t.make_fixed_array_len_expr(rhs_type)], 'void')
		} else {
			t.make_array_push_many_call(lhs_addr, rhs, rhs_type)
		}
		t.drain_pending(mut result)
		result << t.make_expr_stmt(call)
		if bulk_cleanup_name.len > 0 {
			base := t.make_ident(bulk_cleanup_name)
			t.set_node_typ(int(base), bulk_cleanup_type)
			t.append_local_closure_initializer_cleanups_for_value(base, rhs_id, bulk_cleanup_type, mut result)
		}
		if borrowed_push_many_clone && !t.is_fixed_array_type(rhs_type) {
			// push_many transfers the cloned element bytes. Free only the temporary array's
			// backing buffer; the destination now owns its elements.
			result << t.make_expr_stmt(t.make_method_call(rhs, 'free', []flat.NodeId{}))
		}
		return result
	}
	value_name := t.new_temp('arr_val')
	value_type := t.shared_array_lhs_inner_type(lhs_id) or { elem_type }
	result << t.make_decl_assign_typed(value_name, rhs, value_type)
	push_call := t.make_call_typed('array_push', [lhs_addr,
		t.make_prefix(.amp, t.make_ident(value_name))], 'void')
	if shared_inner := t.shared_array_lhs_inner_type(lhs_id) {
		t.set_node_value(int(push_call), 'shared_array_push:${shared_inner}')
	}
	result << t.make_expr_stmt(push_call)
	if int(id) in t.local_closure_field_cleanups {
		result << t.make_local_closure_cleanup_defer(value_name)
	}
	return result
}

fn (t &Transformer) expr_contains_local_closure_field_cleanup(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	if int(id) in t.local_closure_field_cleanups {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	for i in 0 .. node.children_count {
		if t.expr_contains_local_closure_field_cleanup(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

// normalize_array_append_add_rhs restores the append-specific grouping of
// `items << value + suffix` after `<<` gained its numeric shift precedence.
// Numeric shifts remain grouped before `+`; only a statement whose left-shift
// lhs is known to be an array is rotated to `items << (value + suffix)`.
fn (mut t Transformer) normalize_array_append_add_rhs(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind != .infix || node.children_count < 2 {
		return id
	}
	if node.op == .left_shift {
		lhs_id := t.a.child(&node, 0)
		lhs_type := t.clean_array_append_lhs_type(t.lvalue_type(lhs_id))
		return if lhs_type.starts_with('[]') { id } else { flat.empty_node }
	}
	if node.op !in [.plus, .minus] {
		return flat.empty_node
	}
	lhs_id := t.a.child(&node, 0)
	append_id := t.normalize_array_append_add_rhs(lhs_id)
	if int(append_id) < 0 {
		return flat.empty_node
	}
	append := t.a.nodes[int(append_id)]
	append_rhs := t.a.child(&append, 1)
	new_rhs_start := t.a.children.len
	t.a.children << append_rhs
	t.a.children << t.a.child(&node, 1)
	new_rhs := t.a.add_node(flat.Node{
		kind: .infix
		op: node.op
		children_start: new_rhs_start
		children_count: 2
		pos: node.pos
		value: node.value
		typ: node.typ
	})
	new_append_start := t.a.children.len
	t.a.children << t.a.child(&append, 0)
	t.a.children << new_rhs
	return t.a.add_node(flat.Node{
		kind: .infix
		op: .left_shift
		children_start: new_append_start
		children_count: 2
		pos: append.pos
		value: append.value
		typ: append.typ
	})
}

fn (mut t Transformer) try_lower_optional_array_append_stmt(_node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) ?[]flat.NodeId {
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return none
	}
	lhs_node := t.a.nodes[int(lhs_id)]
	if lhs_node.kind != .or_expr || lhs_node.children_count < 2 {
		return none
	}
	source_id := t.a.child(&lhs_node, 0)
	if !t.optional_selector_lvalue_source(source_id) {
		return none
	}
	expr_type, value_type := t.or_expr_types(source_id, lhs_node.typ)
	if !t.is_optional_type_name(expr_type) {
		return none
	}
	array_type := t.clean_array_append_lhs_type(value_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	raw_rhs_type := t.node_type(rhs_id)
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	if !push_many && t.array_append_rhs_builtin_map_elem_matches(rhs_id, elem_type) {
		push_many = true
		rhs_type = array_type
	}
	rhs_is_sum_variant := !push_many
		&& t.array_append_rhs_is_sum_variant_value(rhs_id, raw_rhs_type, elem_type)
	if rhs_node.kind == .array_literal && t.array_append_literal_should_push_many(rhs_id, elem_type) {
		push_many = true
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	} else if push_many && rhs_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	}

	mut result := []flat.NodeId{}
	source := t.transform_lvalue(source_id)
	t.drain_pending(mut result)
	not_ok := t.make_prefix(.not, t.make_selector(source, 'ok', 'bool'))
	guard_stmts := t.optional_selector_lvalue_guard_stmts(t.a.child(&lhs_node, 1), lhs_node.value, source)
	result << t.make_if(not_ok, t.make_or_else_block(lhs_node.value, guard_stmts), t.make_empty())

	// If the RHS hoists a value branch whose prelude can reassign the optional source
	// (`holder.values? << (match ... { holder.replace()! } ...)`), capture the optional's
	// value-array address before lowering the RHS, so the append targets the storage selected in
	// source order (consistent with the guard above) instead of re-reading the inline source
	// after the RHS prelude.
	mut captured_lhs_addr := flat.empty_node
	mut has_captured_addr := false
	if t.operand_hoists_value_branch(rhs_id) {
		addr := t.runtime_addr(t.make_selector(source, 'value', array_type), array_type)
		captured_lhs_addr = t.stable_transformed_expr_for_reuse(addr, '&${array_type}', 'opt_append_target')
		has_captured_addr = true
		t.drain_pending(mut result)
	}

	mut rhs := flat.empty_node
	if !push_many {
		if !rhs_is_sum_variant {
			if t.array_append_elem_is_interface(elem_type) {
				rhs = t.transform_expr_for_type(rhs_id, elem_type)
			} else {
				if converted := t.transform_array_value_for_dynamic_target(rhs_id, array_type) {
					rhs_type = array_type
					push_many = true
					rhs = converted
				} else {
					rhs = if elem_type in t.sum_types
						|| t.resolve_sum_name(elem_type) in t.sum_types {
						t.wrap_sum_value(rhs_id, elem_type)
					} else {
						t.transform_expr_for_type(rhs_id, elem_type)
					}
				}
			}
		} else {
			rhs = if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
				t.wrap_sum_value(rhs_id, elem_type)
			} else {
				t.transform_expr_for_type(rhs_id, elem_type)
			}
		}
	} else {
		// Route a value `match`/`if` push-many RHS (an array-producing match, e.g.
		// `out << (match node { First { values_first(node)! } ... })`) through value
		// lowering so its propagating arm tail is materialized as a value instead of in a
		// value-less statement context. Other operands keep master's
		// `transform_array_many_rhs` (array-literal typing / ownership clone) handling.
		rhs = if t.is_value_match_or_if_operand(rhs_id) {
			t.transform_value_operand(rhs_id)
		} else {
			t.transform_array_many_rhs(rhs_id, rhs_node, array_type)
		}
	}
	if !push_many {
		rhs = t.coerce_transformed_expr_to_type(rhs, rhs_id, elem_type)
		cloned_append := t.clone_borrowed_array_append_value(rhs_id, rhs, elem_type)
		rhs = if cloned_append == rhs {
			t.clone_borrowed_projection(rhs_id, rhs, elem_type)
		} else {
			cloned_append
		}
	}
	mut borrowed_push_many_clone := false
	if push_many {
		cloned_rhs, cloned := t.clone_borrowed_array_append_many_value(rhs_id, rhs, rhs_type, elem_type)
		rhs = cloned_rhs
		borrowed_push_many_clone = cloned
	}
	t.drain_pending(mut result)
	if rhs_type.len == 0 {
		rhs_type = t.node_type(rhs)
		push_many = t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	}

	lhs_addr := if has_captured_addr {
		captured_lhs_addr
	} else {
		t.runtime_addr(t.make_selector(source, 'value', array_type), array_type)
	}
	if push_many {
		call := if t.is_fixed_array_type(rhs_type) {
			t.make_call_typed('array_push_many_ptr', [lhs_addr, rhs,
				t.make_fixed_array_len_expr(rhs_type)], 'void')
		} else {
			t.make_array_push_many_call(lhs_addr, rhs, rhs_type)
		}
		t.drain_pending(mut result)
		result << t.make_expr_stmt(call)
		if borrowed_push_many_clone && !t.is_fixed_array_type(rhs_type) {
			result << t.make_expr_stmt(t.make_method_call(rhs, 'free', []flat.NodeId{}))
		}
		return result
	}
	value_name := t.new_temp('arr_val')
	result << t.make_decl_assign_typed(value_name, rhs, elem_type)
	result << t.make_expr_stmt(t.make_call_typed('array_push', [lhs_addr,
		t.make_prefix(.amp, t.make_ident(value_name))], 'void'))
	return result
}

fn (mut t Transformer) clone_borrowed_array_append_value(source_id flat.NodeId, value flat.NodeId, elem_type string) flat.NodeId {
	if isnil(t.tc) || !t.expr_can_take_address(source_id) {
		return value
	}
	// Ownership mode transfers an addressable by-value RHS into the appended
	// element. The non-ownership lowering clones it to preserve ordinary V
	// value semantics, but doing that here would leave the moved source owning
	// the same nested storage and make its later reinitialization free the
	// destination's data.
	if t.tc.ownership_expr_moves_storage(source_id, source_id) {
		return value
	}
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return value
	}
	return t.make_compiler_default_borrowed_clone_value(value, elem_type, true)
}

fn (mut t Transformer) clone_borrowed_array_append_many_value(source_id flat.NodeId, value flat.NodeId, array_type string, elem_type string) (flat.NodeId, bool) {
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return value, false
	}
	cloned := t.clone_borrowed_projection(source_id, value, array_type)
	return cloned, cloned != value
}

// clone_borrowed_projection clones `value` when ownership analysis decided the read at
// `source_id` copies borrowed storage (a field or slice read) rather than moving it. The
// decision is made in the checker so its move/drop bookkeeping stays consistent with the
// clone emitted here; this only materializes it.
fn (mut t Transformer) clone_borrowed_projection(source_id flat.NodeId, value flat.NodeId, typ string) flat.NodeId {
	if !t.borrowed_projection_clone_required(source_id, typ) {
		return value
	}
	if source_base_id := t.owned_rvalue_slice_source(source_id) {
		if cloned := t.clone_owned_rvalue_slice_projection(value, source_base_id, typ) {
			return cloned
		}
	}
	return t.make_compiler_default_borrowed_clone_value(value, typ, true)
}

// owned_rvalue_slice_source reports the base temporary of a slice that has no retained owner.
// Its transformed slice still needs the normal implicit clone, but the base must be destroyed
// explicitly once that clone is safe.
fn (t &Transformer) owned_rvalue_slice_source(id flat.NodeId) ?flat.NodeId {
	mut clean_id := id
	for int(clean_id) >= 0 && int(clean_id) < t.a.nodes.len {
		node := t.a.nodes[int(clean_id)]
		if node.kind in [.paren, .cast_expr, .expr_stmt] && node.children_count > 0 {
			clean_id = t.a.child(&node, 0)
			continue
		}
		if node.kind != .index || node.value != 'range' || node.children_count == 0 {
			return none
		}
		base_id := t.a.child(&node, 0)
		base_type := t.node_type(base_id)
		if base_type.starts_with('&') || t.expr_can_take_address(base_id) {
			return none
		}
		return base_id
	}
	return none
}

// clone_owned_rvalue_slice_projection clones a slice of an owned temporary, then destroys the
// materialized base. The ordinary borrowed-clone path intentionally omits source cleanup because
// named slice sources remain owned elsewhere; using it here abandons the rvalue's backing.
fn (mut t Transformer) clone_owned_rvalue_slice_projection(value flat.NodeId, source_base_id flat.NodeId, typ string) ?flat.NodeId {
	mut slice_id := value
	for int(slice_id) >= 0 && int(slice_id) < t.a.nodes.len {
		node := t.a.nodes[int(slice_id)]
		if node.kind in [.paren, .cast_expr, .expr_stmt] && node.children_count > 0 {
			slice_id = t.a.child(&node, 0)
			continue
		}
		break
	}
	if int(slice_id) < 0 || int(slice_id) >= t.a.nodes.len {
		return none
	}
	slice_node := t.a.nodes[int(slice_id)]
	if slice_node.kind != .index || slice_node.value != 'range' || slice_node.children_count == 0 {
		return none
	}
	base_type := t.node_type(source_base_id)
	transformed_base_id := t.a.child(&slice_node, 0)
	stable_base := t.stable_transformed_expr_for_reuse(transformed_base_id, base_type, 'owned_slice_source')
	mut children := []flat.NodeId{cap: int(slice_node.children_count)}
	children << stable_base
	for i in 1 .. slice_node.children_count {
		children << t.a.child(&slice_node, i)
	}
	stable_slice := t.copy_node_with_children(slice_node, children)
	cloned := t.make_compiler_default_borrowed_clone_value(stable_slice, typ, true)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [stable_base], 'void'))
	return cloned
}

fn (t &Transformer) borrowed_projection_clone_required(source_id flat.NodeId, typ string) bool {
	return !isnil(t.tc) && t.tc.ownership_expr_is_borrowed_projection(source_id)
		&& t.compiler_default_clone_type_needs_work(typ)
}

// clone_borrowed_assignment_value also handles a local pointer alias that may refer to the
// assignment target's indexed storage. Such replacements must be independent before the old
// target is destroyed.
fn (mut t Transformer) clone_borrowed_assignment_value(source_id flat.NodeId, value flat.NodeId, typ string) flat.NodeId {
	cloned := t.clone_borrowed_projection(source_id, value, typ)
	if cloned != value || isnil(t.tc) || !t.tc.ownership_expr_clones_borrowed_storage(source_id)
		|| !t.compiler_default_clone_type_needs_work(typ) {
		return cloned
	}
	return t.make_compiler_default_borrowed_clone_value(value, typ, true)
}

// clean_array_append_lhs_type transforms clean array append lhs type data for transform.
fn (t &Transformer) clean_array_append_lhs_type(typ string) string {
	mut clean := if array_type_has_generic_placeholder(typ) {
		typ.trim_space()
	} else {
		t.normalize_type_alias(typ).trim_space()
	}
	for {
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('atomic ') {
			clean = clean[7..].trim_space()
			continue
		}
		break
	}
	return clean
}

fn array_type_has_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if is_generic_placeholder_type_name(clean) {
		return true
	}
	if clean.starts_with('&') {
		return array_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('[]') {
		return array_type_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return false }
		return array_type_has_generic_placeholder(clean[4..bracket_end])
			|| array_type_has_generic_placeholder(clean[bracket_end + 1..])
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return false }
		return array_type_has_generic_placeholder(clean[bracket_end + 1..])
	}
	return false
}

// lower_array_prepend_call builds lower array prepend call data for transform.
fn (mut t Transformer) lower_array_prepend_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 2 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	raw_value_id := t.a.child(&node, 1)
	value_node := t.a.nodes[int(raw_value_id)]
	short_struct_value := if value_node.kind == .field_init {
		t.transform_trailing_field_init_struct_arg(node, 1, elem_type)
	} else {
		?flat.NodeId(none)
	}
	value_id := short_struct_value or { raw_value_id }
	raw_rhs_type := if short_struct_value != none { elem_type } else { t.node_type(value_id) }
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	transformed_value_node := t.a.nodes[int(value_id)]
	mut prepend_many := t.array_append_rhs_is_push_many(base_id, value_id, rhs_type, elem_type)
	if prepend_many && t.array_append_rhs_is_sum_variant_value(value_id, raw_rhs_type, elem_type) {
		prepend_many = false
	}
	if transformed_value_node.kind == .array_literal
		&& t.array_append_literal_should_push_many(value_id, elem_type) {
		prepend_many = true
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	} else if prepend_many && transformed_value_node.kind == .array_literal
		&& !rhs_type.starts_with('[]') {
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	}
	base := t.transform_lvalue(base_id)
	if prepend_many {
		mut value := t.transform_array_many_rhs(value_id, value_node, base_type)
		cloned_value, cloned := t.clone_borrowed_array_append_many_value(value_id, value, rhs_type, elem_type)
		value = cloned_value
		call := t.make_array_insert_many_call(t.runtime_addr(base, base_type), t.make_int_literal(0), value, rhs_type)
		return t.finish_borrowed_array_insert_many_call(call, value, rhs_type, cloned)
	}
	mut value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(value_id, elem_type)
	} else {
		t.transform_expr_for_type(value_id, elem_type)
	}
	value = t.coerce_transformed_expr_to_type(value, value_id, elem_type)
	cloned_value := t.clone_borrowed_array_append_value(value_id, value, elem_type)
	value = if cloned_value == value {
		t.clone_borrowed_projection(value_id, value, elem_type)
	} else {
		cloned_value
	}
	value_name := t.new_temp('arr_val')
	t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
	t.mark_fn_used('array__prepend')
	t.mark_fn_used('array__insert')
	t.mark_fn_used('array__needs_unique_shift')
	return t.make_call_typed('array__prepend', [t.runtime_addr(base, base_type),
		t.make_prefix(.amp, t.make_ident(value_name))], 'void')
}

// lower_array_insert_call builds lower array insert call data for transform.
fn (mut t Transformer) lower_array_insert_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 3 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	index_id := t.a.child(&node, 1)
	raw_value_id := t.a.child(&node, 2)
	value_node := t.a.nodes[int(raw_value_id)]
	short_struct_value := if value_node.kind == .field_init {
		t.transform_trailing_field_init_struct_arg(node, 2, elem_type)
	} else {
		?flat.NodeId(none)
	}
	value_id := short_struct_value or { raw_value_id }
	raw_rhs_type := if short_struct_value != none { elem_type } else { t.node_type(value_id) }
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	transformed_value_node := t.a.nodes[int(value_id)]
	mut insert_many := t.array_append_rhs_is_push_many(base_id, value_id, rhs_type, elem_type)
	if insert_many && t.array_append_rhs_is_sum_variant_value(value_id, raw_rhs_type, elem_type) {
		insert_many = false
	}
	if transformed_value_node.kind == .array_literal
		&& t.array_append_literal_should_push_many(value_id, elem_type) {
		insert_many = true
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	} else if insert_many && transformed_value_node.kind == .array_literal
		&& !rhs_type.starts_with('[]') {
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	}
	base := t.transform_lvalue(base_id)
	index := t.transform_expr_for_type(index_id, 'int')
	if insert_many {
		mut value := t.transform_array_many_rhs(value_id, value_node, base_type)
		cloned_value, cloned := t.clone_borrowed_array_append_many_value(value_id, value, rhs_type, elem_type)
		value = cloned_value
		call := t.make_array_insert_many_call(t.runtime_addr(base, base_type), index, value, rhs_type)
		return t.finish_borrowed_array_insert_many_call(call, value, rhs_type, cloned)
	}
	mut value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(value_id, elem_type)
	} else {
		t.transform_expr_for_type(value_id, elem_type)
	}
	value = t.coerce_transformed_expr_to_type(value, value_id, elem_type)
	cloned_value := t.clone_borrowed_array_append_value(value_id, value, elem_type)
	value = if cloned_value == value {
		t.clone_borrowed_projection(value_id, value, elem_type)
	} else {
		cloned_value
	}
	value_name := t.new_temp('arr_val')
	t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
	t.mark_fn_used('array__insert')
	t.mark_fn_used('array__needs_unique_shift')
	return t.make_call_typed('array__insert', [t.runtime_addr(base, base_type), index,
		t.make_prefix(.amp, t.make_ident(value_name))], 'void')
}

fn (mut t Transformer) lower_array_push_many_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 3 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	value_id := t.a.child(&node, 1)
	count_id := t.a.child(&node, 2)
	base := t.transform_lvalue(base_id)
	base_addr := t.runtime_addr(base, base_type)
	t.mark_fn_used('array__push_many')
	if t.push_many_count_is_type_name(count_id) {
		value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(value_id, elem_type)
		} else {
			t.transform_expr_for_type(value_id, elem_type)
		}
		value_name := t.new_temp('arr_val')
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		return t.make_call_typed('array_push_many_ptr', [base_addr,
			t.make_prefix(.amp, t.make_ident(value_name)), t.make_int_literal(1)], 'void')
	}
	value := t.transform_expr(value_id)
	count := t.transform_expr_for_type(count_id, 'int')
	return t.make_call_typed('array_push_many_ptr', [base_addr, value, count], 'void')
}

fn (t &Transformer) push_many_count_is_type_name(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .ident && node.value.len > 0 && node.value[0] >= `A` && node.value[0] <= `Z`
}

// array_append_rhs_is_push_many supports array append rhs is push many handling for Transformer.
fn (t &Transformer) array_append_rhs_is_push_many(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs_type string, elem_type string) bool {
	rhs_node := t.a.nodes[int(rhs_id)]
	if rhs_node.kind == .spawn_expr {
		return false
	}
	clean_rhs_type := rhs_type.trim_space()
	lhs_elem_is_interface := t.array_append_elem_is_interface(elem_type)
	if clean_rhs_type.starts_with('...') {
		return t.array_append_elem_types_match(clean_rhs_type[3..], elem_type)
	}
	if t.array_append_rhs_is_sum_array_variant(clean_rhs_type, elem_type) {
		return false
	}
	if clean_rhs_type.starts_with('[]') {
		if t.array_append_elem_types_match(clean_rhs_type[2..], elem_type) {
			return true
		}
		if declared_rhs_type := t.array_append_ident_type(rhs_id) {
			if declared_rhs_type.starts_with('...') {
				return t.array_append_elem_types_match(declared_rhs_type[3..], elem_type)
			}
			if declared_rhs_type.starts_with('[]') {
				return t.array_append_elem_types_match(declared_rhs_type[2..], elem_type)
			}
		}
		return false
	}
	if t.is_fixed_array_type(clean_rhs_type) {
		return t.array_append_elem_types_match(fixed_array_elem_type(clean_rhs_type), elem_type)
	}
	if !isnil(t.tc) {
		if rhs_resolved := t.tc.expr_type(rhs_id) {
			rhs_clean := types.unwrap_pointer(rhs_resolved)
			if rhs_clean is types.Array {
				if t.array_append_rhs_is_sum_array_variant(types.Type(rhs_clean).name(), elem_type) {
					return false
				}
				return t.array_append_elem_types_match(rhs_clean.elem_type.name(), elem_type)
			}
			if rhs_clean is types.ArrayFixed {
				if t.array_append_rhs_is_sum_array_variant(types.Type(rhs_clean).name(), elem_type) {
					return false
				}
				return t.array_append_elem_types_match(rhs_clean.elem_type.name(), elem_type)
			}
		}
		if lhs_elem_is_interface {
			return false
		}
		if lhs_resolved := t.tc.expr_type(lhs_id) {
			lhs_clean := types.unwrap_pointer(lhs_resolved)
			if lhs_clean is types.Array && clean_rhs_type in ['array', 'Array'] {
				return t.tc.c_type(lhs_clean.elem_type) == 'void*'
			}
		}
	}
	if lhs_elem_is_interface {
		return false
	}
	if clean_rhs_type in ['array', 'Array'] {
		return t.array_append_elem_c_type(elem_type) !in ['array', 'Array']
	}
	return false
}

fn (t &Transformer) array_append_rhs_is_sum_variant_value(rhs_id flat.NodeId, rhs_type string, elem_type string) bool {
	if !t.is_sum_type_name(elem_type) {
		return false
	}
	if t.array_append_rhs_builtin_map_elem_matches(rhs_id, elem_type) {
		return false
	}
	mut clean_rhs := rhs_type.trim_space()
	if clean_rhs.starts_with('!') || clean_rhs.starts_with('?') {
		clean_rhs = clean_rhs[1..].trim_space()
	}
	if clean_rhs.starts_with('[]') && t.array_append_elem_types_match(clean_rhs[2..], elem_type) {
		return false
	}
	candidate := t.array_append_rhs_variant_candidate(rhs_id, rhs_type)
	if candidate.len == 0 {
		return false
	}
	resolved_sum := t.resolve_sum_name(elem_type)
	if resolved_sum.len == 0 {
		return false
	}
	if _ := t.sum_variant_name(resolved_sum, candidate) {
		return true
	}
	return false
}

fn (t &Transformer) array_append_rhs_is_builtin_map_call(rhs_id flat.NodeId) bool {
	if isnil(t.tc) || int(rhs_id) < 0 {
		return false
	}
	node := t.a.nodes[int(rhs_id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value != 'map' || callee.children_count == 0 {
		return false
	}
	if resolved := t.tc.resolved_call_name(rhs_id) {
		if resolved != 'array.map' {
			return false
		}
	}
	// Cloned comptime/generic calls can lose their call-id annotation. In that case,
	// require both the preserved receiver type and the inferred return type to be arrays.
	receiver_id := t.a.child(callee, 0)
	receiver_type := t.tc.resolve_type(receiver_id)
	mut receiver_is_array := array_append_semantic_type_is_array(receiver_type)
	if !receiver_is_array {
		original_receiver_type := t.trim_pointer_type(t.original_expr_type(receiver_id))
		receiver_is_array = original_receiver_type.starts_with('[]')
			|| t.is_fixed_array_type(original_receiver_type)
	}
	if !receiver_is_array {
		return false
	}
	rhs_type := t.tc.resolve_type(rhs_id)
	if array_append_semantic_type_is_array(rhs_type) {
		return true
	}
	if resolved_rhs_type := t.array_map_call_type_name(rhs_id, node) {
		return resolved_rhs_type.starts_with('[]')
	}
	return false
}

fn (t &Transformer) array_append_rhs_builtin_map_elem_matches(rhs_id flat.NodeId, elem_type string) bool {
	if !t.array_append_rhs_is_builtin_map_call(rhs_id) {
		return false
	}
	node := t.a.nodes[int(rhs_id)]
	if result_type := t.array_map_call_type_name(rhs_id, node) {
		clean := t.normalize_type_alias(result_type)
		if clean.starts_with('[]') {
			return t.array_append_elem_types_match(clean[2..], elem_type)
		}
	}
	if !isnil(t.tc) {
		resolved := types.unwrap_all_pointers(t.tc.resolve_type(rhs_id))
		if resolved is types.Array {
			return t.array_append_elem_types_match(resolved.elem_type.name(), elem_type)
		}
		if resolved is types.ArrayFixed {
			return t.array_append_elem_types_match(resolved.elem_type.name(), elem_type)
		}
	}
	return false
}

fn array_append_semantic_type_is_array(typ types.Type) bool {
	clean := types.unwrap_all_pointers(typ)
	if clean is types.Alias {
		return array_append_semantic_type_is_array(clean.base_type)
	}
	return clean is types.Array || clean is types.ArrayFixed
}

fn (t &Transformer) array_append_rhs_variant_candidate(rhs_id flat.NodeId, rhs_type string) string {
	if int(rhs_id) < 0 {
		return ''
	}
	node := t.a.nodes[int(rhs_id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.array_append_rhs_variant_candidate(t.a.child(&node, 0), rhs_type)
	}
	if node.kind in [.cast_expr, .struct_init, .as_expr, .assoc] && node.value.len > 0 {
		return node.value
	}
	if node.kind == .call && node.children_count > 0 {
		name := t.generic_call_type_arg_name(t.a.child(&node, 0))
		if name.len > 0 {
			return name
		}
	}
	if node.kind == .ident && node.value.len > 0 {
		raw_type := t.raw_var_type(node.value)
		if raw_type.len > 0 {
			return raw_type
		}
	}
	return rhs_type
}

fn (t &Transformer) array_append_literal_should_push_many(rhs_id flat.NodeId, elem_type string) bool {
	if int(rhs_id) < 0 {
		return false
	}
	node := t.a.nodes[int(rhs_id)]
	if node.kind != .array_literal {
		return false
	}
	if t.array_append_elem_is_interface(elem_type) {
		return t.array_append_literal_children_match_elem(rhs_id, elem_type)
	}
	if t.array_append_rhs_is_sum_array_variant(t.node_type(rhs_id), elem_type) {
		return false
	}
	if !elem_type.starts_with('[]') && !t.is_fixed_array_type(elem_type) {
		return true
	}
	// With an array-shaped destination element, an empty literal is the single empty
	// element (`mut a := [][]int{}; a << []`), not a zero-element spread.
	if node.children_count == 0 {
		return false
	}
	return t.array_append_literal_children_match_elem(rhs_id, elem_type)
}

fn (t &Transformer) array_append_rhs_is_sum_array_variant(rhs_type string, elem_type string) bool {
	resolved_sum := t.resolve_sum_name(elem_type)
	variants := t.sum_types[resolved_sum] or { return false }
	mut clean_rhs := rhs_type.trim_space()
	if clean_rhs.starts_with('...') {
		clean_rhs = clean_rhs[3..].trim_space()
	}
	if clean_rhs.len == 0 {
		return false
	}
	// An array with exactly the destination's element type is the push-many
	// form (`[]Value << []Value`), even when `[]Value` also appears recursively
	// as a variant of `Value`. Distinct array variants such as `[]int` appended
	// to `[]Any` remain single sum-type elements.
	if clean_rhs.starts_with('[]') && t.array_append_elem_types_match(clean_rhs[2..], elem_type) {
		return false
	}
	for variant in variants {
		if t.array_append_elem_types_match(clean_rhs, variant) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_append_elem_is_interface(elem_type string) bool {
	if t.is_builtin_ierror_interface_name(elem_type) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	mut candidates := [elem_type.trim_space(), t.normalize_type_alias(elem_type).trim_space()]
	for candidate in candidates.clone() {
		if candidate.len == 0 || candidate.contains('.') {
			continue
		}
		candidates << t.tc.qualify_name(candidate)
		candidates << 'main.${candidate}'
		candidates << 'builtin.${candidate}'
	}
	for candidate in candidates {
		if candidate.len > 0 && candidate in t.tc.interface_names {
			return true
		}
	}
	return t.is_interface_type(elem_type)
}

fn (t &Transformer) array_append_literal_children_match_elem(rhs_id flat.NodeId, elem_type string) bool {
	node := t.a.nodes[int(rhs_id)]
	clean_elem := t.normalize_type_alias(elem_type)
	if clean_elem.len == 0 {
		return false
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
			spread_id := t.a.child(&child, 0)
			spread_type := t.normalize_type_alias(t.node_type(spread_id))
			if spread_type.starts_with('[]')
				&& t.array_append_elem_types_match(spread_type[2..], clean_elem) {
				continue
			}
			return false
		}
		child_type := t.normalize_type_alias(t.node_type(child_id))
		if child.kind in [.array_literal, .array_init] && child.children_count == 0
			&& (clean_elem.starts_with('[]') || t.is_fixed_array_type(clean_elem)) {
			continue
		}
		if child.kind == .array_literal && clean_elem.starts_with('[]')
			&& t.array_append_literal_children_match_elem(child_id, clean_elem[2..]) {
			continue
		}
		if t.array_append_elem_types_match(child_type, clean_elem) {
			continue
		}
		if child_type.starts_with('&')
			&& t.array_append_elem_types_match(child_type[1..], clean_elem) {
			continue
		}
		if child_type.len == 0 && child.kind == .array_literal
			&& (clean_elem.starts_with('[]') || t.is_fixed_array_type(clean_elem)) {
			continue
		}
		return false
	}
	return true
}

// array_append_elem_types_match supports array append elem types match handling for Transformer.
fn (t &Transformer) array_append_elem_types_match(rhs_elem_type string, lhs_elem_type string) bool {
	rhs_raw := rhs_elem_type.trim_space()
	lhs_raw := lhs_elem_type.trim_space()
	if rhs_raw == lhs_raw {
		return true
	}
	rhs_clean := t.normalize_type_alias(rhs_elem_type)
	lhs_clean := t.normalize_type_alias(lhs_elem_type)
	if rhs_clean == lhs_clean {
		return true
	}
	if array_append_type_is_container_shape(rhs_clean)
		|| array_append_type_is_container_shape(lhs_clean) {
		return false
	}
	if isnil(t.tc) {
		return false
	}
	lhs_iface := t.resolve_interface_type_name(lhs_clean)
	if lhs_iface.len > 0 {
		if t.array_append_interface_has_requirements(lhs_iface) {
			rhs_concrete := t.trim_pointer_type(rhs_clean)
			if t.tc.named_type_implements_interface(rhs_concrete, lhs_iface) {
				return true
			}
		}
	}
	return t.array_append_elem_c_type(rhs_clean) == t.array_append_elem_c_type(lhs_clean)
}

fn (t &Transformer) array_append_interface_has_requirements(iface_name string) bool {
	if isnil(t.tc) {
		return false
	}
	if t.tc.interface_abstract_method_names(iface_name).len > 0 {
		return true
	}
	if (t.tc.interface_fields[iface_name] or { []types.StructField{} }).len > 0 {
		return true
	}
	for embed in t.tc.interface_embeds[iface_name] or { []string{} } {
		if t.array_append_interface_has_requirements(embed) {
			return true
		}
	}
	return false
}

fn array_append_type_is_container_shape(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') {
		return array_append_type_is_container_shape(clean[1..])
	}
	if clean.starts_with('shared ') {
		return array_append_type_is_container_shape(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return array_append_type_is_container_shape(clean[7..])
	}
	return clean.starts_with('[]') || clean.starts_with('map[')
		|| (clean.starts_with('[') && clean.contains(']'))
}

// array_append_ident_type supports array append ident type handling for Transformer.
fn (t &Transformer) array_append_ident_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	typ := t.var_type(node.value)
	if typ.len == 0 {
		return none
	}
	return typ
}

// array_append_elem_c_type supports array append elem c type handling for Transformer.
fn (t &Transformer) array_append_elem_c_type(typ string) string {
	if isnil(t.tc) {
		return typ
	}
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if !clean.contains('.') {
		for alias, target in t.tc.type_aliases {
			if alias.all_after_last('.') == clean {
				return t.tc.c_type(t.tc.parse_type(target))
			}
		}
	}
	return t.tc.c_type(t.tc.parse_type(clean))
}

// array_get_value supports array get value handling for Transformer.
fn (mut t Transformer) array_get_value(base flat.NodeId, index flat.NodeId, elem_type string) flat.NodeId {
	if t.array_get_base_is_fixed_array(base) {
		return t.make_index(base, index, elem_type)
	}
	get_call := t.make_call_typed('array_get', [t.array_get_runtime_base(base), index], 'voidptr')
	ptr := t.make_cast('&${elem_type}', get_call, '&${elem_type}')
	value := t.make_prefix(.mul, ptr)
	t.set_node_typ(int(value), elem_type)
	return value
}

// array_get_ptr supports array get ptr handling for Transformer.
fn (mut t Transformer) array_get_ptr(base flat.NodeId, index flat.NodeId, elem_type string) flat.NodeId {
	if t.array_get_base_is_fixed_array(base) {
		value := t.make_index(base, index, elem_type)
		ptr := t.make_prefix(.amp, value)
		t.set_node_typ(int(ptr), '&${elem_type}')
		return ptr
	}
	get_call := t.make_call_typed('array_get', [t.array_get_runtime_base(base), index], 'voidptr')
	return t.make_cast('&${elem_type}', get_call, '&${elem_type}')
}

fn (t &Transformer) array_get_base_is_fixed_array(base flat.NodeId) bool {
	node := t.a.node(base)
	mut base_type := if node.kind == .ident && t.var_type(node.value).len > 0 {
		t.var_type(node.value).trim_space()
	} else {
		t.node_type(base).trim_space()
	}
	if base_type.len == 0 {
		base_type = t.original_expr_type(base).trim_space()
	}
	if base_type.starts_with('&') {
		base_type = base_type[1..].trim_space()
	}
	if t.is_fixed_array_type(base_type) {
		return true
	}
	return false
}

fn (mut t Transformer) array_get_runtime_base(base flat.NodeId) flat.NodeId {
	base_type := t.node_type(base).trim_space()
	if base_type.starts_with('&') {
		node := t.a.nodes[int(base)]
		if t.array_get_base_is_shared_value_selector(base, node)
			|| t.array_get_base_is_shared_array_ident(base_type, node) {
			t.set_node_typ(int(base), base_type[1..])
			return base
		}
		value := t.make_prefix(.mul, base)
		t.set_node_typ(int(value), base_type[1..])
		return value
	}
	return base
}

fn (mut t Transformer) array_get_base_is_shared_value_selector(base flat.NodeId, node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 || node.value != 'val' {
		return false
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id).trim_space()
	if base_type.starts_with('shared ') || base_type.starts_with('&shared ')
		|| base_type.contains('__shared__') {
		return true
	}
	// During rlock lowering a shared array value field can be represented as `&[]T`.
	// Passing that field to `array_get` should use the selector storage directly; a
	// dereference would emit `*array->val`, which is not a valid C Array lvalue.
	return t.node_type(base).trim_space().starts_with('&[]')
}

fn (t &Transformer) array_get_base_is_shared_array_ident(base_type string, node flat.Node) bool {
	if !base_type.starts_with('&[]') || node.kind != .ident || node.value.len == 0 {
		return false
	}
	raw_type := t.raw_var_type(node.value).trim_space()
	return raw_type.starts_with('shared ')
}

// materialize_array_callback evaluates a direct filter/map callback once.
// Capturing literals and bound methods allocate runtime closure state, which must
// not be recreated for every loop element.
fn (mut t Transformer) materialize_array_callback(id flat.NodeId, prefix string) (flat.NodeId, []flat.NodeId) {
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	t.mark_local_method_value_receiver_borrows_in_expr(id)
	callback_type := t.fresh_runtime_closure_type(id) or { t.fn_value_type_name(id) or { '' } }
	callback := if callback_type.len > 0 {
		t.transform_expr_for_type(id, callback_type)
	} else {
		t.transform_expr(id)
	}
	mut setup := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	callback_node := t.a.nodes[int(callback)]
	if callback_node.kind == .ident {
		if t.expr_allocates_fresh_runtime_closure(id) {
			setup << t.make_local_closure_cleanup_defer(callback_node.value)
		}
		return callback, setup
	}
	resolved_callback_type := if callback_type.len > 0 {
		callback_type
	} else {
		t.node_type(callback)
	}
	callback_name := t.new_temp(prefix)
	setup << t.make_decl_assign_typed(callback_name, callback, resolved_callback_type)
	if t.expr_allocates_fresh_runtime_closure(id) {
		setup << t.make_local_closure_cleanup_defer(callback_name)
	}
	callback_ident := t.make_ident(callback_name)
	if resolved_callback_type.len > 0 {
		t.set_node_typ(int(callback_ident), resolved_callback_type)
	}
	return callback_ident, setup
}

fn (mut t Transformer) transform_array_predicate(predicate_id flat.NodeId, default_elem_name string, elem_type string, prefix string) (string, flat.NodeId, []flat.NodeId, []flat.NodeId) {
	predicate_node := t.a.nodes[int(predicate_id)]
	predicate_allocates_closure := t.expr_allocates_fresh_runtime_closure(predicate_id)
	predicate_is_fn_value := predicate_node.kind != .lambda_expr
		&& t.call_arg_is_fn_pointer_value(predicate_id, predicate_node)
	mut predicate_expr_id := predicate_id
	mut lambda_param := ''
	mut predicate_fn_name := ''
	if predicate_node.kind == .lambda_expr && predicate_node.children_count > 0 {
		predicate_expr_id = t.a.child(&predicate_node, predicate_node.children_count - 1)
		if predicate_node.children_count > 1 {
			param := t.a.child_node(&predicate_node, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	} else if fn_name := t.resolve_fn_value_expr(predicate_id, predicate_node) {
		predicate_fn_name = fn_name
	} else if predicate_node.kind == .ident {
		if ret_type := t.fn_value_return_type_name(predicate_id) {
			if ret_type == 'bool' {
				predicate_fn_name = predicate_node.value
			}
		}
	}
	elem_name := if lambda_param.len > 0 { lambda_param } else { default_elem_name }
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate_source := if lambda_param.len > 0 {
		predicate_expr_id
	} else {
		t.substitute_ident(predicate_expr_id, 'it', elem_name)
	}
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut callback_setup := []flat.NodeId{}
	predicate := if predicate_fn_name.len > 0 {
		t.make_call_typed(predicate_fn_name, [t.make_ident(elem_name)], 'bool')
	} else if predicate_is_fn_value || predicate_allocates_closure {
		fn_value, setup := t.materialize_array_callback(predicate_id, prefix)
		callback_setup = setup.clone()
		fn_value_node := t.a.nodes[int(fn_value)]
		if fn_value_node.kind == .ident {
			t.make_call_typed(fn_value_node.value, [t.make_ident(elem_name)], 'bool')
		} else {
			t.make_call_expr_typed(fn_value, [t.make_ident(elem_name)], 'bool')
		}
	} else {
		t.transform_expr(predicate_source)
	}
	predicate_pending := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	return elem_name, predicate, callback_setup, predicate_pending
}

// lower_array_filter_call builds lower array filter call data for transform.
fn (mut t Transformer) lower_array_filter_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	elem_needs_clone := !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
	if elem_needs_clone {
		// The checker reports the missing clone method. Do not lower the rejected call
		// to a shallow element copy while processing the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(elem_type)) {
			return t.make_empty()
		}
	}
	source_needs_drop := !t.expr_can_take_address(base_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_transformed_expr_for_reuse(t.transform_expr(base_id), base_type, 'filter_source')
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	out_name := t.new_temp('filter')
	idx_name := t.new_temp('filter_idx')
	prefix << t.make_decl_assign_typed(out_name, t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_selector(base, 'len', 'int')), base_type)
	mut cleanup_guard_name := ''
	if source_needs_drop {
		cleanup_guard_name = t.new_temp('filter_values_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drops := [
			t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void')),
			t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(out_name),
			], 'void')),
		]
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name), t.make_block(deferred_drops), t.make_empty())
		defer_body := t.make_block([guarded_drop])
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind: .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_name_default := t.new_temp('filter_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	predicate_id := t.a.child(&node, 1)
	predicate_node := t.a.nodes[int(predicate_id)]
	predicate_allocates_closure := t.expr_allocates_fresh_runtime_closure(predicate_id)
	predicate_is_fn_value := predicate_node.kind != .lambda_expr
		&& t.call_arg_is_fn_pointer_value(predicate_id, predicate_node)
	mut predicate_expr_id := predicate_id
	mut lambda_param := ''
	mut predicate_fn_name := ''
	if predicate_node.kind == .lambda_expr && predicate_node.children_count > 0 {
		predicate_expr_id = t.a.child(&predicate_node, predicate_node.children_count - 1)
		if predicate_node.children_count > 1 {
			param := t.a.child_node(&predicate_node, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	} else if fn_name := t.resolve_fn_value_expr(predicate_id, predicate_node) {
		predicate_fn_name = fn_name
	} else if predicate_node.kind == .ident {
		if ret_type := t.fn_value_return_type_name(predicate_id) {
			if ret_type == 'bool' {
				predicate_fn_name = predicate_node.value
			}
		}
	}
	elem_name := if lambda_param.len > 0 { lambda_param } else { elem_name_default }
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate_source := if lambda_param.len > 0 {
		predicate_expr_id
	} else {
		t.substitute_ident(predicate_expr_id, 'it', elem_name)
	}
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut callback_setup := []flat.NodeId{}
	predicate := if predicate_fn_name.len > 0 {
		t.make_call_typed(predicate_fn_name, [t.make_ident(elem_name)], 'bool')
	} else if predicate_is_fn_value || predicate_allocates_closure {
		fn_value, setup := t.materialize_array_callback(predicate_id, 'filter_callback')
		callback_setup = setup.clone()
		fn_value_node := t.a.nodes[int(fn_value)]
		if fn_value_node.kind == .ident {
			t.make_call_typed(fn_value_node.value, [t.make_ident(elem_name)], 'bool')
		} else {
			t.make_call_expr_typed(fn_value, [t.make_ident(elem_name)], 'bool')
		}
	} else {
		t.transform_expr(predicate_source)
	}
	predicate_pending := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	for stmt in callback_setup {
		prefix << stmt
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	for stmt in predicate_pending {
		loop_body << stmt
	}
	mut then_body := []flat.NodeId{}
	mut pushed_name := elem_name
	if elem_needs_clone {
		pending_start := t.pending_stmts.len
		cloned_elem := t.make_compiler_default_clone_value(t.make_ident(elem_name), elem_type, true)
		then_body = t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		pushed_name = t.new_temp('filter_value')
		then_body << t.make_decl_assign_typed(pushed_name, cloned_elem, elem_type)
	}
	push_call := t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(out_name)),
		t.make_prefix(.amp, t.make_ident(pushed_name)),
	], 'void')
	then_body << t.make_expr_stmt(push_call)
	then_block := t.make_block(then_body)
	loop_body << t.make_if(predicate, then_block, t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_needs_drop {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(out_name)
}

// lower_array_map_call builds lower array map call data for transform.
fn (mut t Transformer) lower_array_map_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	map_expr_id := t.a.child(&node, 1)
	map_expr := t.a.nodes[int(map_expr_id)]
	map_expr_is_dsl_bound_method := t.array_map_is_dsl_bound_method(map_expr)
	map_callback_allocates_closure := !map_expr_is_dsl_bound_method
		&& t.expr_allocates_fresh_runtime_closure(map_expr_id)
	map_expr_is_fn_value := !map_expr_is_dsl_bound_method && map_expr.kind != .lambda_expr
		&& t.call_arg_is_fn_pointer_value(map_expr_id, map_expr)
	mut map_source_id := map_expr_id
	mut lambda_param := ''
	if map_expr.kind == .lambda_expr && map_expr.children_count > 0 {
		map_source_id = t.a.child(&map_expr, map_expr.children_count - 1)
		if map_expr.children_count > 1 {
			param := t.a.child_node(&map_expr, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	}
	mut map_fn_name := ''
	elem_name := if lambda_param.len > 0 { lambda_param } else { t.new_temp('map_it') }
	mapped_source := if lambda_param.len > 0 {
		map_source_id
	} else {
		t.substitute_ident(map_source_id, 'it', elem_name)
	}
	// The implicit `it` binding denotes the source slot when its address is taken.
	// Binding a copied value here would return pointers to a loop-local temporary.
	mapper_takes_elem_address := lambda_param.len == 0 && (t.array_map_expr_takes_address_of_ident(mapped_source, elem_name) || t.array_map_expr_implicit_reference_can_escape(map_source_id, 'it'))
	elem_var_type := if mapper_takes_elem_address { '&${elem_type}' } else { elem_type }
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_var_type)
	checker_result_elem_type := t.checker_expr_type_name(map_expr_id) or { '' }
	checker_result_elem_type_is_usable := decl_type_is_usable(checker_result_elem_type)
		&& checker_result_elem_type != 'void'
	mut result_elem_type := if checker_result_elem_type_is_usable {
		checker_result_elem_type
	} else {
		t.node_type(map_expr_id)
	}
	mut direct_selector_type := ''
	mut mapped_source_node := t.a.nodes[int(mapped_source)]
	if mapped_source_node.kind == .map_init {
		inferred_map_type := t.infer_map_init_entry_type(mapped_source_node)
		if inferred_map_type.len > 0 {
			result_elem_type = inferred_map_type
			t.set_node_value(int(mapped_source), inferred_map_type)
			t.set_node_typ(int(mapped_source), inferred_map_type)
			mapped_source_node = t.a.nodes[int(mapped_source)]
		}
	}
	if mapped_source_node.kind == .selector {
		selector_type := t.lookup_struct_field_type(elem_type, mapped_source_node.value) or {
			t.resolve_selector_type(mapped_source_node)
		}
		if selector_type.len > 0 {
			direct_selector_type = selector_type
			result_elem_type = selector_type
		}
	}
	if fn_name := t.resolve_fn_value_expr(map_expr_id, map_expr) {
		map_fn_name = fn_name
		if ret := t.fn_ret_types[fn_name] {
			result_elem_type = ret
		} else if !isnil(t.tc) {
			if ret_type := t.tc.fn_ret_types[fn_name] {
				result_elem_type = t.normalize_type_alias(ret_type.name())
			}
		}
	} else if map_expr.kind == .ident {
		if ret_type := t.fn_value_return_type_name(map_expr_id) {
			map_fn_name = map_expr.value
			result_elem_type = ret_type
		}
	} else if map_expr_is_fn_value || map_callback_allocates_closure {
		if ret_type := t.fn_value_return_type_name(map_expr_id) {
			result_elem_type = ret_type
		}
	}
	if result_elem_type.len == 0 {
		result_elem_type = t.reliable_stringify_type(map_expr_id)
	}
	bound_method_info := t.array_map_bound_method_info(mapped_source_node, elem_name, elem_type, result_elem_type) or { BoundMethodArrayInfo{} }
	has_bound_method_array := bound_method_info.receiver_type.len > 0
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut callback_setup := []flat.NodeId{}
	had_pointer_value_lvalue := t.pointer_value_lvalues[elem_name] or { false }
	had_pointer_value_rvalue := t.pointer_value_rvalues[elem_name] or { false }
	if mapper_takes_elem_address {
		t.pointer_value_lvalues[elem_name] = true
		t.pointer_value_rvalues[elem_name] = true
	}
	mapped_expr := if map_fn_name.len > 0 {
		t.make_call_typed(map_fn_name, [t.make_ident(elem_name)], result_elem_type)
	} else if map_expr_is_fn_value || map_callback_allocates_closure {
		fn_value, setup := t.materialize_array_callback(map_expr_id, 'map_callback')
		callback_setup = setup.clone()
		fn_value_node := t.a.nodes[int(fn_value)]
		if fn_value_node.kind == .ident && result_elem_type.len > 0 {
			t.make_call_typed(fn_value_node.value, [t.make_ident(elem_name)], result_elem_type)
		} else {
			t.make_call_expr_typed(fn_value, [t.make_ident(elem_name)], result_elem_type)
		}
	} else if has_bound_method_array {
		t.make_cast(result_elem_type, t.make_cast('usize', t.make_ident(elem_name), 'usize'), result_elem_type)
	} else if mapped_source_node.kind == .map_init && result_elem_type.starts_with('map[') {
		t.transform_expr_for_type(mapped_source, result_elem_type)
	} else if decl_type_is_usable(result_elem_type) && result_elem_type != 'void' {
		// `it` substitution clones the mapper expression after checking. The cloned
		// node IDs have no checker cache entries, so keep the original map result
		// type as context for value expressions such as `if it.m() { ... } else { it }`.
		t.transform_expr_for_type(mapped_source, result_elem_type)
	} else {
		t.transform_expr(mapped_source)
	}
	if mapper_takes_elem_address {
		if had_pointer_value_lvalue {
			t.pointer_value_lvalues[elem_name] = true
		} else {
			t.pointer_value_lvalues.delete(elem_name)
		}
		if had_pointer_value_rvalue {
			t.pointer_value_rvalues[elem_name] = true
		} else {
			t.pointer_value_rvalues.delete(elem_name)
		}
	}
	mapped_pending := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mapped_type := t.node_type(mapped_expr)
	if decl_type_is_usable(mapped_type) && mapped_type != 'void'
		&& (!checker_result_elem_type_is_usable || t.active_specialization_args.len > 0) {
		result_elem_type = mapped_type
	}
	if direct_selector_type.len > 0 && map_fn_name.len == 0 {
		result_elem_type = direct_selector_type
	}
	if direct_selector_type.len == 0 {
		if mapped_expr_node := t.selector_expr_node(mapped_expr) {
			selector_type := t.resolve_selector_type(mapped_expr_node)
			if selector_type.len > 0 {
				result_elem_type = selector_type
			}
		}
	}
	if result_elem_type.len == 0 {
		result_elem_type = elem_type
	}
	opaque_mapper := map_fn_name.len > 0 || map_expr_is_fn_value || map_callback_allocates_closure
	mapper_returns_owned := !isnil(t.tc)
		&& t.tc.ownership_fn_value_returns_owned(map_expr_id, t.cur_fn_name, t.cur_module)
	mapped_borrows_elem := (opaque_mapper && !mapper_returns_owned)
		|| (t.array_map_expr_references_ident(mapped_source, elem_name)
			&& (isnil(t.tc) || !t.tc.ownership_expr_creates_owned_value(map_source_id)))
	mapped_result_needs_clone := mapped_borrows_elem && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(result_elem_type))
	if mapped_result_needs_clone {
		// The checker reports the missing clone method. Do not leave a shallow mapped
		// owner in the result while transforming the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(result_elem_type)) {
			return t.make_empty()
		}
	}
	out_type := '[]${result_elem_type}'
	base_id := t.a.child(&fn_node, 0)
	map_result_retains_elem_address := mapper_takes_elem_address && t.array_map_result_can_retain_element_address(result_elem_type) && t.array_map_expr_result_retains_element_address(map_source_id, 'it')
	map_side_effect_retains_elem_address := mapper_takes_elem_address && t.array_map_expr_side_effect_retains_element_address(map_source_id, 'it')
	source_needs_drop := !map_result_retains_elem_address && !map_side_effect_retains_elem_address && !t.expr_can_take_address(base_id) && !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_transformed_expr_for_reuse(t.transform_expr(base_id), base_type, 'map_source')
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	for stmt in callback_setup {
		prefix << stmt
	}
	out_name := t.new_temp('map')
	idx_name := t.new_temp('map_idx')
	prefix << t.make_decl_assign_typed(out_name, t.make_array_new_call(result_elem_type, t.make_int_literal(0), t.make_selector(base, 'len', 'int')), out_type)
	mut cleanup_guard_name := ''
	if source_needs_drop {
		cleanup_guard_name = t.new_temp('map_values_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drops := [
			t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void')),
			t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(out_name),
			], 'void')),
		]
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name), t.make_block(deferred_drops), t.make_empty())
		defer_body := t.make_block([guarded_drop])
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind: .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := if mapper_takes_elem_address {
		t.array_get_ptr(base, t.make_ident(idx_name), elem_type)
	} else {
		t.array_get_value(base, t.make_ident(idx_name), elem_type)
	}
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_var_type)
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	for stmt in mapped_pending {
		loop_body << stmt
	}
	value_name := t.new_temp('map_val')
	loop_body << t.make_decl_assign_typed(value_name, mapped_expr, result_elem_type)
	mut pushed_name := value_name
	if mapped_result_needs_clone {
		mapped_value := t.make_ident(value_name)
		t.set_node_typ(int(mapped_value), result_elem_type)
		pending_start := t.pending_stmts.len
		cloned_value := t.make_compiler_default_clone_value(mapped_value, result_elem_type, true)
		for stmt in t.pending_stmts[pending_start..].clone() {
			loop_body << stmt
		}
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		pushed_name = t.new_temp('map_cloned_val')
		loop_body << t.make_decl_assign_typed(pushed_name, cloned_value, result_elem_type)
	}
	loop_body << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(out_name)),
		t.make_prefix(.amp, t.make_ident(pushed_name)),
	], 'void'))
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_needs_drop {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), out_type)
	if has_bound_method_array {
		t.bound_method_arrays[t.bound_method_array_key(out_name)] = bound_method_info
	}
	return result
}

fn (t &Transformer) array_map_is_dsl_bound_method(node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base := t.a.child_node(&node, 0)
	return base.kind == .ident && base.value == 'it'
}

// array_map_expr_references_ident reports whether a mapped value reads the synthetic
// element binding. Such values remain borrowed from the consumed source unless their
// expression explicitly creates a fresh owner.
fn (t &Transformer) array_map_expr_references_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.array_map_expr_references_ident(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_map_expr_takes_address_of_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .prefix && node.op == .amp && node.children_count == 1
		&& t.array_map_lvalue_is_rooted_at_ident(t.a.child(&node, 0), name) {
		return true
	}
	for i in 0 .. node.children_count {
		if t.array_map_expr_takes_address_of_ident(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) array_map_call_implicitly_borrows_ident(id flat.NodeId, node flat.Node, name string) bool {
	if node.kind != .call || node.children_count == 0 || name.len == 0 || isnil(t.tc) {
		return false
	}
	call_name := t.call_name_for_node(id, node)
	params := t.call_param_types_for_node(call_name, node)
	param_offset := t.call_param_offset_for_node(call_name, node, params)
	callee := t.a.child_node(&node, 0)
	if param_offset == 1 && params.len > 0 && types.unalias_type(params[0]) is types.Pointer
		&& callee.kind == .selector && callee.children_count > 0
		&& t.array_map_lvalue_is_rooted_at_ident(t.a.child(callee, 0), name) {
		return true
	}
	for i in 1 .. node.children_count {
		param_idx := i - 1 + param_offset
		if param_idx < params.len && types.unalias_type(params[param_idx]) is types.Pointer && t.array_map_lvalue_is_rooted_at_ident(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) array_map_expr_implicit_reference_can_escape(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || name.len == 0 || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return false
	}
	if node.kind == .call && t.array_map_call_implicitly_borrows_ident(id, node, name) {
		result_type := t.checker_expr_type_name(id) or { t.node_type(id) }
		if t.array_map_result_can_retain_element_address(result_type) || t.tc.resolved_call_may_store_globally(id) {
			return true
		}
		if resolved_name := t.tc.resolved_call_name(id) {
			if resolved_name.starts_with('C.') {
				return true
			}
		} else {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.array_map_expr_implicit_reference_can_escape(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_map_lvalue_is_rooted_at_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value == name
	}
	if node.kind in [.selector, .index, .paren] && node.children_count > 0 {
		return t.array_map_lvalue_is_rooted_at_ident(t.a.child(&node, 0), name)
	}
	return false
}

fn (mut t Transformer) array_map_expr_result_retains_element_address(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		return t.array_map_lvalue_is_rooted_at_ident(t.a.child(&node, 0), name)
	}
	match node.kind {
		.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .field_init {
			return node.children_count > 0
				&& t.array_map_expr_result_retains_element_address(t.a.child(&node, 0), name)
		}
		.block, .match_branch {
			return t.array_map_block_result_retains_element_address(node, name)
		}
		.if_expr, .match_stmt {
			for i in 1 .. node.children_count {
				if t.array_map_expr_result_retains_element_address(t.a.child(&node, i), name) {
					return true
				}
			}
			return false
		}
		.comptime_if {
			if take_then := t.comptime_type_condition_value(node.value) {
				branch_idx := if take_then { 0 } else { 1 }
				return branch_idx < node.children_count && t.array_map_expr_result_retains_element_address(t.a.child(&node, branch_idx), name)
			}
			for i in 0 .. node.children_count {
				if t.array_map_expr_result_retains_element_address(t.a.child(&node, i), name) {
					return true
				}
			}
			return false
		}
		.or_expr {
			for i in 0 .. node.children_count {
				if t.array_map_expr_result_retains_element_address(t.a.child(&node, i), name) {
					return true
				}
			}
			return false
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			for source_arg in t.tc.ownership_call_result_source_args(id) {
				if t.array_map_expr_result_retains_element_address(source_arg, name) {
					return true
				}
			}
			if t.array_map_expr_is_call_projection(id) {
				return false
			}
			return t.array_map_selector_result_retains_element_address(t.a.child(&node, 0), node.value, name)
		}
		.index {
			return t.array_map_index_result_retains_element_address(node, name)
		}
		.call {
			call_type := t.checker_expr_type_name(id) or { t.node_type(id) }
			if !t.array_map_result_can_retain_element_address(call_type) {
				return false
			}
			if t.array_map_call_implicitly_borrows_ident(id, node, name) {
				return true
			}
			for source_arg in t.tc.ownership_call_result_source_args(id) {
				if t.array_map_expr_result_retains_element_address(source_arg, name) {
					return true
				}
			}
			return false
		}
		.fn_literal {
			return t.fn_literal_captures_name(id, name)
		}
		.struct_init, .array_literal, .array_init, .map_init, .assoc {
			for i in 0 .. node.children_count {
				if t.array_map_expr_result_retains_element_address(t.a.child(&node, i), name) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) array_map_lvalue_root_ident(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind in [.selector, .index, .paren, .cast_expr, .as_expr, .prefix] && node.children_count > 0 {
		return t.array_map_lvalue_root_ident(t.a.child(&node, 0))
	}
	return none
}

fn (t &Transformer) array_map_lvalue_local_path(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind in [.paren, .cast_expr, .as_expr, .prefix] && node.children_count > 0 {
		return t.array_map_lvalue_local_path(t.a.child(&node, 0))
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.array_map_lvalue_local_path(t.a.child(&node, 0)) or { return none }
		return '${base}.${node.value}'
	}
	if node.kind == .index && node.children_count > 1 {
		base := t.array_map_lvalue_local_path(t.a.child(&node, 0)) or { return none }
		index := t.a.child_node(&node, 1)
		return array_map_local_index_path(base, index)
	}
	return none
}

fn array_map_local_index_path(base string, index &flat.Node) string {
	if index.kind in [.int_literal, .string_literal, .char_literal, .bool_literal] {
		return '${base}[${array_map_local_index_component(index.kind, index.value)}]'
	}
	return '${base}[*]'
}

fn array_map_local_index_component(kind flat.NodeKind, value string) string {
	hex_digits := '0123456789abcdef'
	mut encoded := []u8{cap: value.len * 2}
	for byte in value.bytes() {
		encoded << hex_digits[byte >> 4]
		encoded << hex_digits[byte & 0x0f]
	}
	return '${kind}:${encoded.bytestr()}'
}

const array_map_local_pointer_pointee_prefix = '@pointee:'

fn array_map_local_pointer_pointee_marker(owner string, target string) string {
	return '${array_map_local_pointer_pointee_prefix}${owner}=>${target}'
}

fn array_map_local_pointer_pointee_owner(marker string) ?string {
	if !marker.starts_with(array_map_local_pointer_pointee_prefix) {
		return none
	}
	body := marker[array_map_local_pointer_pointee_prefix.len..]
	separator := body.index('=>') or { return none }
	return body[..separator]
}

fn array_map_local_pointer_pointee_target(marker string) ?string {
	if !marker.starts_with(array_map_local_pointer_pointee_prefix) {
		return none
	}
	body := marker[array_map_local_pointer_pointee_prefix.len..]
	separator := body.index('=>') or { return none }
	return body[separator + 2..]
}

fn array_map_local_pointer_pointee_targets(owner string, locals map[string]bool) []string {
	mut targets := []string{}
	for marker, _ in locals {
		marker_owner := array_map_local_pointer_pointee_owner(marker) or { continue }
		if marker_owner != owner {
			continue
		}
		target := array_map_local_pointer_pointee_target(marker) or { continue }
		if target !in targets {
			targets << target
		}
	}
	return targets
}

fn (t &Transformer) array_map_lvalue_local_paths(id flat.NodeId, locals map[string]bool) []string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return []
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return [node.value]
	}
	if node.kind in [.paren, .cast_expr, .as_expr, .prefix] && node.children_count > 0 {
		base_paths := t.array_map_lvalue_local_paths(t.a.child(&node, 0), locals)
		if node.kind != .prefix || node.op != .mul {
			return base_paths
		}
		mut pointee_paths := []string{}
		for base_path in base_paths {
			for target in array_map_local_pointer_pointee_targets(base_path, locals) {
				if target !in pointee_paths {
					pointee_paths << target
				}
			}
		}
		return if pointee_paths.len > 0 { pointee_paths } else { base_paths }
	}
	if node.kind == .selector && node.children_count > 0 {
		mut paths := []string{}
		for base in t.array_map_lvalue_local_paths(t.a.child(&node, 0), locals) {
			paths << '${base}.${node.value}'
		}
		return paths
	}
	if node.kind == .index && node.children_count > 1 {
		index := t.a.child_node(&node, 1)
		mut paths := []string{}
		for base in t.array_map_lvalue_local_paths(t.a.child(&node, 0), locals) {
			paths << array_map_local_index_path(base, index)
		}
		return paths
	}
	return []
}

fn array_map_local_path_is_projection(path string, base string) bool {
	return path == base || path.starts_with('${base}.') || path.starts_with('${base}[')
}

fn array_map_local_path_is_possible_projection(path string, base string) bool {
	if array_map_local_path_is_projection(path, base) {
		return true
	}
	mut path_pos := 0
	mut base_pos := 0
	for path_pos < path.len && base_pos < base.len {
		if path[path_pos] == `[` && base[base_pos] == `[` {
			path_end_offset := path[path_pos..].index(']') or { return false }
			base_end_offset := base[base_pos..].index(']') or { return false }
			path_end := path_pos + path_end_offset
			base_end := base_pos + base_end_offset
			path_index := path[path_pos + 1..path_end]
			base_index := base[base_pos + 1..base_end]
			if path_index != '*' && base_index != '*' && path_index != base_index {
				return false
			}
			path_pos = path_end + 1
			base_pos = base_end + 1
			continue
		}
		if path[path_pos] != base[base_pos] {
			return false
		}
		path_pos++
		base_pos++
	}
	return base_pos == base.len && (path_pos == path.len || path[path_pos] == `.` || path[path_pos] == `[`)
}

fn array_map_local_pointer_path(path string, root string, locals map[string]bool) string {
	mut pointer_path := root
	for local_path, external in locals {
		if local_path.starts_with(array_map_local_pointer_pointee_prefix) {
			continue
		}
		if !array_map_local_path_is_possible_projection(path, local_path) {
			continue
		}
		if local_path.len > pointer_path.len {
			pointer_path = local_path
		} else if local_path.len == pointer_path.len && external && !locals[pointer_path] {
			// A dynamic index read may resolve to any equally-specific element, so merge
			// their origins by keeping an external alias over a first-seen local one; that
			// stops a borrowed source from being freed while another slot still points at it.
			pointer_path = local_path
		}
	}
	return pointer_path
}

// `locals[path]` records whether a mapper-local pointer projection currently aliases
// storage rooted outside the mapper. A bare assignment to that pointer slot itself stays
// local; deeper selector/index writes and mutating calls follow the alias.
fn array_map_local_target_path_is_external(path string, elem_name string, locals map[string]bool, follow_local_pointer bool, mut seen map[string]bool) bool {
	root := array_map_local_path_root(path)
	if root == elem_name {
		return false
	}
	if root !in locals {
		return true
	}
	pointer_path := array_map_local_pointer_path(path, root, locals)
	if locals[pointer_path] && (follow_local_pointer || path != pointer_path) {
		return true
	}
	if !follow_local_pointer || pointer_path in seen {
		return false
	}
	seen[pointer_path] = true
	for target in array_map_local_pointer_pointee_targets(pointer_path, locals) {
		mut target_seen := seen.clone()
		if array_map_local_target_path_is_external(target, elem_name, locals, true, mut target_seen) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_map_side_effect_target_is_external(id flat.NodeId, elem_name string, locals map[string]bool, follow_local_pointer bool) bool {
	for path in t.array_map_lvalue_local_paths(id, locals) {
		mut seen := map[string]bool{}
		if array_map_local_target_path_is_external(path, elem_name, locals, follow_local_pointer, mut seen) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) array_map_pointer_alias_origin_is_external(id flat.NodeId, elem_name string, locals map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		return t.array_map_side_effect_target_is_external(t.a.child(&node, 0), elem_name, locals, false)
	}
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt] && node.children_count > 0 {
		return t.array_map_pointer_alias_origin_is_external(t.a.child(&node, 0), elem_name, locals)
	}
	if node.kind == .block && node.children_count > 0 {
		return t.array_map_pointer_alias_origin_is_external(t.a.child(&node, node.children_count - 1), elem_name, locals)
	}
	typ := t.checker_expr_type_name(id) or { t.node_type(id) }
	clean_type := t.normalize_type_alias(typ)
	if !clean_type.starts_with('&') && !clean_type.starts_with('chan ') {
		return false
	}
	if path := t.array_map_lvalue_local_path(id) {
		if root := t.array_map_lvalue_root_ident(id) {
			if root in locals {
				return locals[array_map_local_pointer_path(path, root, locals)]
			}
		}
	}
	root := t.array_map_lvalue_root_ident(id) or { return true }
	return root != elem_name && (root !in locals || locals[root])
}

fn (mut t Transformer) array_map_slice_backing_origin_is_external(id flat.NodeId, elem_name string, origins map[string]bool) ?bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .field_init] && node.children_count > 0 {
		return t.array_map_slice_backing_origin_is_external(t.a.child(&node, 0), elem_name, origins)
	}
	if node.kind in [.block, .match_branch] && node.children_count > 0 {
		return t.array_map_slice_backing_origin_is_external(t.a.child(&node, node.children_count - 1), elem_name, origins)
	}
	if node.kind != .index || node.value != 'range' || node.children_count < 2 {
		return none
	}
	result_type := t.normalize_type_alias(t.checker_expr_type_name(id) or { t.node_type(id) })
	if !result_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&node, 0)
	root := t.array_map_lvalue_root_ident(base_id) or { return true }
	if root == elem_name {
		return false
	}
	if root !in origins {
		return true
	}
	base_path := t.array_map_lvalue_local_path(base_id) or { return origins[root] }
	backing_path := '${base_path}[*]'
	return origins[array_map_local_pointer_path(backing_path, root, origins)]
}

fn array_map_local_path_with_assignment_wildcards(path string, assignment_path string) string {
	mut wildcard_indexes := []int{}
	mut assignment_pos := 0
	mut assignment_index := 0
	for assignment_pos < assignment_path.len {
		if assignment_path[assignment_pos] != `[` {
			assignment_pos++
			continue
		}
		end_offset := assignment_path[assignment_pos..].index(']') or { break }
		end := assignment_pos + end_offset
		if assignment_path[assignment_pos + 1..end] == '*' {
			wildcard_indexes << assignment_index
		}
		assignment_index++
		assignment_pos = end + 1
	}
	if wildcard_indexes.len == 0 {
		return path
	}
	mut result := ''
	mut path_pos := 0
	mut path_index := 0
	for path_pos < path.len {
		if path[path_pos] != `[` {
			result += path[path_pos].ascii_str()
			path_pos++
			continue
		}
		end_offset := path[path_pos..].index(']') or {
			result += path[path_pos..]
			break
		}
		end := path_pos + end_offset
		if path_index in wildcard_indexes {
			result += '[*]'
		} else {
			result += path[path_pos..end + 1]
		}
		path_index++
		path_pos = end + 1
	}
	return result
}

fn array_map_clear_local_pointer_origins(path string, mut locals map[string]bool) map[string]bool {
	mut stale := []string{}
	mut overlapping := map[string]bool{}
	has_wildcard := path.contains('[*]')
	for local_path, external in locals {
		marker_owner := array_map_local_pointer_pointee_owner(local_path) or { local_path }
		if array_map_local_path_is_projection(marker_owner, path) || (has_wildcard && array_map_local_path_is_possible_projection(marker_owner, path)) {
			stale << local_path
			if has_wildcard && marker_owner == local_path {
				merged_path := array_map_local_path_with_assignment_wildcards(local_path, path)
				overlapping[merged_path] = overlapping[merged_path] || external
			}
		}
	}
	for local_path in stale {
		locals.delete(local_path)
	}
	return overlapping
}

fn array_map_merge_overlapping_pointer_origins(overlapping map[string]bool, mut locals map[string]bool) {
	for path, external in overlapping {
		locals[path] = locals[path] || external
	}
}

fn array_map_local_path_root(path string) string {
	if owner := array_map_local_pointer_pointee_owner(path) {
		return array_map_local_path_root(owner)
	}
	for i, ch in path {
		if ch in [`.`, `[`] {
			return path[..i]
		}
	}
	return path
}

fn array_map_merge_local_pointer_origins(mut target map[string]bool, source map[string]bool, baseline map[string]bool) {
	for path, external in source {
		if array_map_local_path_root(path) in baseline {
			target[path] = target[path] || external
		}
	}
}

fn array_map_local_pointer_origins_equal(left map[string]bool, right map[string]bool) bool {
	if left.len != right.len {
		return false
	}
	for path, external in left {
		if path !in right || right[path] != external {
			return false
		}
	}
	return true
}

fn array_map_join_local_pointer_origin_states(mut target map[string]bool, source map[string]bool) bool {
	mut changed := false
	for path, external in source {
		if path !in target {
			target[path] = external
			changed = true
		} else if external && !target[path] {
			target[path] = true
			changed = true
		}
	}
	return changed
}

fn array_map_call_result_relative_source_suffix(target_suffix string, source_target_suffix string) ?string {
	if source_target_suffix.len == 0 {
		return target_suffix
	}
	if target_suffix == source_target_suffix || (target_suffix.len == source_target_suffix.len && array_map_local_path_is_possible_projection(target_suffix, source_target_suffix)) {
		return ''
	}
	if array_map_local_path_is_possible_projection(target_suffix, source_target_suffix) {
		return target_suffix[source_target_suffix.len..]
	}
	if array_map_local_path_is_possible_projection(source_target_suffix, target_suffix) {
		// A conservative aggregate target contains the mapped descendant.
		return ''
	}
	return none
}

fn (mut t Transformer) array_map_call_result_path_origin_is_external(source types.OwnershipCallResultSource, target_suffix string, elem_name string, origins map[string]bool) bool {
	relative_suffix := array_map_call_result_relative_source_suffix(target_suffix, source.target_suffix) or { return false }
	source_path := t.array_map_lvalue_local_path(source.arg_id) or {
		return t.array_map_pointer_alias_origin_is_external(source.arg_id, elem_name, origins)
	}
	root := t.array_map_lvalue_root_ident(source.arg_id) or { return true }
	if root == elem_name {
		return false
	}
	if root !in origins {
		return true
	}
	effective_path := source_path + source.source_suffix + relative_suffix
	return origins[array_map_local_pointer_path(effective_path, root, origins)]
}

fn (mut t Transformer) array_map_record_call_result_pointer_type_paths(path string, target_suffix string, typ types.Type, sources []types.OwnershipCallResultSource, elem_name string, origins map[string]bool, mut locals map[string]bool, mut seen map[string]bool) {
	match typ {
		types.Pointer, types.Channel, types.FnType, types.Interface {
			locals[path] = sources.len == 0 || sources.any(t.array_map_call_result_path_origin_is_external(it, target_suffix, elem_name, origins))
		}
		types.Alias, types.OptionType, types.ResultType {
			t.array_map_record_call_result_pointer_type_paths(path, target_suffix, typ.base_type, sources, elem_name, origins, mut locals, mut seen)
		}
		types.Array, types.ArrayFixed {
			t.array_map_record_call_result_pointer_type_paths('${path}[*]', '${target_suffix}[*]', typ.elem_type, sources, elem_name, origins, mut locals, mut seen)
		}
		types.Map {
			t.array_map_record_call_result_pointer_type_paths('${path}[*]', '${target_suffix}[*]', typ.value_type, sources, elem_name, origins, mut locals, mut seen)
		}
		types.Struct {
			if typ.name !in seen {
				seen[typ.name] = true
				for field in t.tc.struct_fields_for_type(typ.name) {
					t.array_map_record_call_result_pointer_type_paths('${path}.${field.name}', '${target_suffix}.${field.name}', field.typ, sources, elem_name, origins, mut locals, mut seen)
				}
				seen.delete(typ.name)
			}
		}
		types.SumType, types.MultiReturn {
			// The active variant/slot is runtime-dependent, so retain a conservative
			// aggregate root when ownership says an external argument reaches it.
			locals[path] = sources.len == 0 || sources.any(t.array_map_call_result_path_origin_is_external(it, target_suffix, elem_name, origins))
		}
		else {}
	}
}

fn (mut t Transformer) array_map_record_local_pointer_origins(path string, id flat.NodeId, elem_name string, origins map[string]bool, mut locals map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .field_init] && node.children_count > 0 {
		t.array_map_record_local_pointer_origins(path, t.a.child(&node, 0), elem_name, origins, mut locals)
		return
	}
	if node.kind in [.block, .match_branch] && node.children_count > 0 {
		t.array_map_record_local_pointer_origins(path, t.a.child(&node, node.children_count - 1), elem_name, origins, mut locals)
		return
	}
	if node.kind == .comptime_if {
		if take_then := t.comptime_type_condition_value(node.value) {
			branch_idx := if take_then { 0 } else { 1 }
			if branch_idx < node.children_count {
				t.array_map_record_local_pointer_origins(path, t.a.child(&node, branch_idx), elem_name, origins, mut locals)
			} else {
				locals[path] = false
			}
			return
		}
	}
	if node.kind in [.if_expr, .match_stmt, .or_expr, .comptime_if] {
		locals[path] = false
		branch_start := if node.kind in [.if_expr, .match_stmt] { 1 } else { 0 }
		for i in branch_start .. node.children_count {
			mut branch_origins := map[string]bool{}
			t.array_map_record_local_pointer_origins(path, t.a.child(&node, i), elem_name, origins, mut branch_origins)
			for branch_path, external in branch_origins {
				locals[branch_path] = locals[branch_path] || external
			}
		}
		return
	}
	typ := t.checker_expr_type_name(id) or { t.node_type(id) }
	clean_type := t.normalize_type_alias(typ)
	if clean_type.starts_with('&') || clean_type.starts_with('chan ') {
		locals[path] = t.array_map_pointer_alias_origin_is_external(id, elem_name, origins)
		t.array_map_record_local_pointer_pointees(path, id, origins, mut locals)
		return
	}
	locals[path] = false
	if external := t.array_map_slice_backing_origin_is_external(id, elem_name, origins) {
		// A range-index view keeps the source array's backing allocation. Track the
		// wildcard storage path so writes through a local slice still reach external
		// storage without treating a later reassignment of the slice itself as external.
		locals['${path}[*]'] = external
	}
	if node.kind == .call {
		sources := t.tc.ownership_call_result_sources(id)
		mut seen := map[string]bool{}
		t.array_map_record_call_result_pointer_type_paths(path, '', t.tc.resolve_type(id), sources, elem_name, origins, mut locals, mut seen)
		return
	}
	if node.kind == .assoc && node.children_count > 0 {
		t.array_map_record_local_pointer_origins(path, t.a.child(&node, 0), elem_name, origins, mut locals)
		for i in 1 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind == .field_init && field.value.len > 0 && field.children_count > 0 {
				field_path := '${path}.${field.value}'
				array_map_clear_local_pointer_origins(field_path, mut locals)
				t.array_map_record_local_pointer_origins(field_path, t.a.child(field, 0), elem_name, origins, mut locals)
			}
		}
		return
	}
	if node.kind == .struct_init {
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind == .field_init && field.value.len > 0 && field.children_count > 0 {
				t.array_map_record_local_pointer_origins('${path}.${field.value}', t.a.child(field, 0), elem_name, origins, mut locals)
			}
		}
		return
	}
	if node.kind == .array_literal {
		for i in 0 .. node.children_count {
			index_path := array_map_local_index_component(.int_literal, i.str())
			t.array_map_record_local_pointer_origins('${path}[${index_path}]', t.a.child(&node, i), elem_name, origins, mut locals)
		}
		return
	}
	if node.kind == .array_init {
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind == .field_init && field.value == 'init' && field.children_count > 0 {
				t.array_map_record_local_pointer_origins('${path}[*]', t.a.child(field, 0), elem_name, origins, mut locals)
			}
		}
		return
	}
	if node.kind == .map_init {
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			key := t.a.child_node(&node, i)
			entry_path := array_map_local_index_path(path, key)
			t.array_map_record_local_pointer_origins(entry_path, t.a.child(&node, i + 1), elem_name, origins, mut locals)
		}
		return
	}
	if source_path := t.array_map_lvalue_local_path(id) {
		for local_path, external in origins {
			if local_path != source_path && array_map_local_path_is_projection(local_path, source_path) {
				suffix := local_path[source_path.len..]
				locals['${path}${suffix}'] = external
			}
		}
	}
}

fn (mut t Transformer) array_map_record_local_pointer_pointees(path string, id flat.NodeId, origins map[string]bool, mut locals map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .field_init] && node.children_count > 0 {
		t.array_map_record_local_pointer_pointees(path, t.a.child(&node, 0), origins, mut locals)
		return
	}
	if node.kind in [.block, .match_branch] && node.children_count > 0 {
		t.array_map_record_local_pointer_pointees(path, t.a.child(&node, node.children_count - 1), origins, mut locals)
		return
	}
	if node.kind == .comptime_if {
		if take_then := t.comptime_type_condition_value(node.value) {
			branch_idx := if take_then { 0 } else { 1 }
			if branch_idx < node.children_count {
				t.array_map_record_local_pointer_pointees(path, t.a.child(&node, branch_idx), origins, mut locals)
			}
			return
		}
	}
	if node.kind in [.if_expr, .match_stmt, .or_expr, .comptime_if] {
		branch_start := if node.kind in [.if_expr, .match_stmt] { 1 } else { 0 }
		for i in branch_start .. node.children_count {
			t.array_map_record_local_pointer_pointees(path, t.a.child(&node, i), origins, mut locals)
		}
		return
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		for target in t.array_map_lvalue_local_paths(t.a.child(&node, 0), origins) {
			locals[array_map_local_pointer_pointee_marker(path, target)] = false
		}
		return
	}
	for source_path in t.array_map_lvalue_local_paths(id, origins) {
		for target in array_map_local_pointer_pointee_targets(source_path, origins) {
			locals[array_map_local_pointer_pointee_marker(path, target)] = false
		}
	}
}

fn (mut t Transformer) array_map_update_local_pointer_origins(stmt_id flat.NodeId, elem_name string, mut locals map[string]bool) {
	mut loop_exits := []ArrayMapLoopPointerExit{}
	mut return_exits := []ArrayMapReturnPointerExit{}
	t.array_map_update_local_pointer_origins_flow(stmt_id, elem_name, mut locals, mut loop_exits, mut return_exits, '', 0)
}

fn (mut t Transformer) array_map_block_goto_label_origins(stmt flat.Node, elem_name string, initial map[string]bool, active_defer_count int) map[string]map[string]bool {
	mut labels := map[string]bool{}
	for i in 0 .. stmt.children_count {
		child := t.a.child_node(&stmt, i)
		if child.kind == .label_stmt && child.value.len > 0 {
			labels[child.value] = true
		}
	}
	if labels.len == 0 {
		return map[string]map[string]bool{}
	}
	mut incoming := map[string]map[string]bool{}
	for {
		mut scoped := initial.clone()
		mut continues := true
		mut pass_exits := []ArrayMapLoopPointerExit{}
		mut pass_returns := []ArrayMapReturnPointerExit{}
		for i in 0 .. stmt.children_count {
			child_id := t.a.child(&stmt, i)
			child := t.a.nodes[int(child_id)]
			if child.kind == .label_stmt {
				if label_origins := incoming[child.value] {
					if continues {
						array_map_join_local_pointer_origin_states(mut scoped, label_origins)
					} else {
						scoped = label_origins.clone()
					}
					continues = true
				}
			}
			if !continues {
				continue
			}
			mut child_loop_label := ''
			if child.kind in [.for_stmt, .for_in_stmt] && i > 0 {
				previous := t.a.child_node(&stmt, i - 1)
				if previous.kind == .label_stmt {
					child_loop_label = previous.value
				}
			}
			continues = t.array_map_update_local_pointer_origins_flow(child_id, elem_name, mut scoped, mut pass_exits, mut pass_returns, child_loop_label, active_defer_count)
		}
		mut changed := false
		for exit in pass_exits {
			if !exit.is_goto || exit.label !in labels {
				continue
			}
			mut merged := map[string]bool{}
			if existing := incoming[exit.label] {
				merged = existing.clone()
			}
			if array_map_join_local_pointer_origin_states(mut merged, exit.origins) {
				changed = true
			}
			incoming[exit.label] = merged.clone()
		}
		if !changed {
			return incoming
		}
	}
	return incoming
}

fn array_map_loop_iteration_child_indices(stmt flat.Node) []int {
	if stmt.kind != .for_stmt || stmt.children_count < 3 {
		mut indices := []int{cap: int(stmt.children_count)}
		for i in 0 .. stmt.children_count {
			indices << int(i)
		}
		return indices
	}
	mut indices := []int{cap: int(stmt.children_count) - 1}
	indices << 1
	for i in 3 .. stmt.children_count {
		indices << int(i)
	}
	indices << 2
	return indices
}

fn (mut t Transformer) array_map_loop_pointer_origin_fixed_point(stmt flat.Node, elem_name string, initial map[string]bool, loop_label string, active_defer_count int) map[string]bool {
	mut loop_origins := initial.clone()
	for {
		mut next_origins := loop_origins.clone()
		mut pass_origins := loop_origins.clone()
		mut pass_exits := []ArrayMapLoopPointerExit{}
		mut pass_returns := []ArrayMapReturnPointerExit{}
		mut continues := true
		for i in array_map_loop_iteration_child_indices(stmt) {
			if !continues {
				break
			}
			continues = t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut pass_origins, mut pass_exits, mut pass_returns, '', active_defer_count)
		}
		if continues {
			array_map_merge_local_pointer_origins(mut next_origins, pass_origins, loop_origins)
		}
		for exit in pass_exits {
			if exit.is_continue && (exit.label.len == 0 || exit.label == loop_label) {
				mut continue_origins := exit.origins.clone()
				if stmt.kind == .for_stmt && stmt.children_count >= 3 {
					mut post_exits := []ArrayMapLoopPointerExit{}
					mut post_returns := []ArrayMapReturnPointerExit{}
					t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 2), elem_name, mut continue_origins, mut post_exits, mut post_returns, '', active_defer_count)
				}
				array_map_merge_local_pointer_origins(mut next_origins, continue_origins, loop_origins)
			}
		}
		if array_map_local_pointer_origins_equal(loop_origins, next_origins) {
			return loop_origins
		}
		loop_origins = next_origins.move()
	}
	return loop_origins
}

fn (mut t Transformer) array_map_update_local_pointer_origins_flow(stmt_id flat.NodeId, elem_name string, mut locals map[string]bool, mut loop_exits []ArrayMapLoopPointerExit, mut return_exits []ArrayMapReturnPointerExit, loop_label string, active_defer_count int) bool {
	if int(stmt_id) < 0 || int(stmt_id) >= t.a.nodes.len {
		return true
	}
	stmt := t.a.nodes[int(stmt_id)]
	if stmt.kind in [.break_stmt, .continue_stmt] {
		loop_exits << ArrayMapLoopPointerExit{
			origins: locals.clone()
			label: stmt.value
			defer_count: active_defer_count
			is_continue: stmt.kind == .continue_stmt
		}
		return false
	}
	if stmt.kind == .goto_stmt {
		loop_exits << ArrayMapLoopPointerExit{
			origins: locals.clone()
			label: stmt.value
			defer_count: active_defer_count
			is_goto: true
		}
		return false
	}
	if stmt.kind == .return_stmt {
		for i in 0 .. stmt.children_count {
			if !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut locals, mut loop_exits, mut return_exits, loop_label, active_defer_count) {
				return false
			}
		}
		return_exits << ArrayMapReturnPointerExit{
			origins: locals.clone()
			defer_count: active_defer_count
		}
		return false
	}
	if stmt.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt] && stmt.children_count > 0 {
		return t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 0), elem_name, mut locals, mut loop_exits, mut return_exits, loop_label, active_defer_count)
	}
	if stmt.kind in [.block, .match_branch, .select_branch] {
		label_origins := t.array_map_block_goto_label_origins(stmt, elem_name, locals, active_defer_count)
		mut scoped := locals.clone()
		mut declared := map[string]bool{}
		mut continues := true
		mut block_exits := []ArrayMapLoopPointerExit{}
		for i in 0 .. stmt.children_count {
			child_id := t.a.child(&stmt, i)
			child := t.a.nodes[int(child_id)]
			if child.kind == .label_stmt {
				if incoming := label_origins[child.value] {
					if continues {
						array_map_join_local_pointer_origin_states(mut scoped, incoming)
					} else {
						scoped = incoming.clone()
					}
					continues = true
				}
			}
			if !continues {
				continue
			}
			if child.kind == .decl_assign {
				for j := 0; j + 1 < int(child.children_count); j += 2 {
					lhs := t.a.child_node(&child, j)
					if lhs.kind == .ident {
						declared[lhs.value] = true
					}
				}
			}
			mut child_loop_label := ''
			if child.kind in [.for_stmt, .for_in_stmt] && i > 0 {
				previous := t.a.child_node(&stmt, i - 1)
				if previous.kind == .label_stmt {
					child_loop_label = previous.value
				}
			}
			continues = t.array_map_update_local_pointer_origins_flow(child_id, elem_name, mut scoped, mut block_exits, mut return_exits, child_loop_label, active_defer_count)
		}
		for exit in block_exits {
			if !exit.is_goto || exit.label !in label_origins {
				loop_exits << exit
			}
		}
		if continues {
			for path, external in scoped {
				root := array_map_local_path_root(path)
				if root in locals && root !in declared {
					locals[path] = external
				}
			}
		}
		return continues
	}
	if stmt.kind == .comptime_if {
		if take_then := t.comptime_type_condition_value(stmt.value) {
			branch_idx := if take_then { 0 } else { 1 }
			if branch_idx < stmt.children_count {
				return t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, branch_idx), elem_name, mut locals, mut loop_exits, mut return_exits, '', active_defer_count)
			}
			return true
		}
		// An unresolved comptime condition compiles exactly one branch, but which one is
		// unknown here. Conservatively union both branches so a pointer rebind to external
		// storage in either branch is preserved instead of dropped.
		before := locals.clone()
		mut merged := map[string]bool{}
		mut continues := stmt.children_count <= 1
		for name, origin in before {
			merged[name] = if stmt.children_count > 1 { false } else { origin }
		}
		for i in 0 .. stmt.children_count {
			mut branch := before.clone()
			if t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut branch, mut loop_exits, mut return_exits, '', active_defer_count) {
				continues = true
				array_map_merge_local_pointer_origins(mut merged, branch, before)
			}
		}
		if continues {
			locals = merged.move()
		}
		return continues
	}
	if stmt.kind == .or_expr {
		if stmt.children_count == 0 {
			return true
		}
		// The source is evaluated on both paths. A continuing fallback is conditional, so
		// preserve every pointer origin possible after either success or fallback execution.
		if !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 0), elem_name, mut locals, mut loop_exits, mut return_exits, '', active_defer_count) {
			return false
		}
		before := locals.clone()
		mut merged := before.clone()
		for i in 1 .. stmt.children_count {
			mut fallback := before.clone()
			if t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut fallback, mut loop_exits, mut return_exits, '', active_defer_count) {
				array_map_merge_local_pointer_origins(mut merged, fallback, before)
			}
		}
		locals = merged.move()
		return true
	}
	if stmt.kind == .infix && stmt.children_count > 0 {
		if stmt.op in [.logical_and, .logical_or] {
			// The left operand is always evaluated, while every later operand is
			// conditional. Preserve pointer origins from both the short-circuit path
			// and the path that evaluates the remaining operands.
			if !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 0), elem_name, mut locals, mut loop_exits, mut return_exits, '', active_defer_count) {
				return false
			}
			for i in 1 .. stmt.children_count {
				before := locals.clone()
				mut evaluated := before.clone()
				if t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut evaluated, mut loop_exits, mut return_exits, '', active_defer_count) {
					array_map_merge_local_pointer_origins(mut locals, evaluated, before)
				}
			}
			return true
		}
		for i in 0 .. stmt.children_count {
			if !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut locals, mut loop_exits, mut return_exits, '', active_defer_count) {
				return false
			}
		}
		return true
	}
	if stmt.kind == .if_expr {
		if stmt.children_count > 0 && !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 0), elem_name, mut locals, mut loop_exits, mut return_exits, '', active_defer_count) {
			return false
		}
		before := locals.clone()
		mut merged := map[string]bool{}
		mut continues := stmt.children_count <= 2
		for name, origin in before {
			merged[name] = if stmt.children_count > 2 { false } else { origin }
		}
		for i in 1 .. stmt.children_count {
			mut branch := before.clone()
			if t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut branch, mut loop_exits, mut return_exits, '', active_defer_count) {
				continues = true
				array_map_merge_local_pointer_origins(mut merged, branch, before)
			}
		}
		if continues {
			locals = merged.move()
		}
		return continues
	}
	if stmt.kind == .match_stmt {
		if stmt.children_count > 0 && !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 0), elem_name, mut locals, mut loop_exits, mut return_exits, '', active_defer_count) {
			return false
		}
		before := locals.clone()
		mut merged := map[string]bool{}
		mut continues := false
		for name, _ in before {
			merged[name] = false
		}
		for i in 1 .. stmt.children_count {
			mut branch := before.clone()
			if t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut branch, mut loop_exits, mut return_exits, '', active_defer_count) {
				continues = true
				array_map_merge_local_pointer_origins(mut merged, branch, before)
			}
		}
		if continues {
			locals = merged.move()
		}
		return continues
	}
	if stmt.kind == .select_stmt {
		before := locals.clone()
		mut merged := map[string]bool{}
		mut continues := false
		for name, _ in before {
			merged[name] = false
		}
		for i in 0 .. stmt.children_count {
			mut branch := before.clone()
			if t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut branch, mut loop_exits, mut return_exits, '', active_defer_count) {
				continues = true
				array_map_merge_local_pointer_origins(mut merged, branch, before)
			}
		}
		if continues {
			locals = merged.move()
		}
		return continues
	}
	if stmt.kind in [.for_stmt, .for_in_stmt] {
		before := locals.clone()
		mut exits := []ArrayMapLoopPointerExit{}
		mut continues := true
		mut loop_entry := before.clone()
		if stmt.kind == .for_stmt && stmt.children_count > 0 {
			continues = t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 0), elem_name, mut loop_entry, mut exits, mut return_exits, '', active_defer_count)
		}
		mut loop_origins := t.array_map_loop_pointer_origin_fixed_point(stmt, elem_name, loop_entry, loop_label, active_defer_count)
		for i in array_map_loop_iteration_child_indices(stmt) {
			if !continues {
				break
			}
			continues = t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut loop_origins, mut exits, mut return_exits, '', active_defer_count)
		}
		locals = loop_entry.clone()
		for name, origin in loop_entry {
			locals[name] = origin || (continues && loop_origins[name])
		}
		if continues {
			array_map_merge_local_pointer_origins(mut locals, loop_origins, loop_entry)
		}
		for exit in exits {
			if exit.is_goto {
				loop_exits << exit
			} else if exit.label.len == 0 || exit.label == loop_label {
				mut exit_origins := exit.origins.clone()
				if exit.is_continue && stmt.kind == .for_stmt && stmt.children_count >= 3 {
					mut post_exits := []ArrayMapLoopPointerExit{}
					mut post_returns := []ArrayMapReturnPointerExit{}
					t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, 2), elem_name, mut exit_origins, mut post_exits, mut post_returns, '', active_defer_count)
				}
				array_map_merge_local_pointer_origins(mut locals, exit_origins, loop_entry)
			} else {
				loop_exits << exit
			}
		}
		return true
	}
	if stmt.kind == .call && stmt.children_count > 0 {
		// Evaluate the callee and physical arguments first. Nested calls can rebind a
		// pointer that the outer call or a later statement observes.
		mut argument_origins := map[int]map[string]bool{}
		for i in 0 .. stmt.children_count {
			if !t.array_map_update_local_pointer_origins_flow(t.a.child(&stmt, i), elem_name, mut locals, mut loop_exits, mut return_exits, loop_label, active_defer_count) {
				return false
			}
			// Argument values and mut lvalues are captured left-to-right. Keep their
			// origins before a later argument can rebind the same local pointer.
			argument_origins[i] = locals.clone()
		}
		call_name := t.call_name_for_node(stmt_id, stmt)
		params := t.call_param_types_for_node(call_name, stmt)
		param_offset := t.call_param_offset_for_node(call_name, stmt, params)
		callee := t.a.child_node(&stmt, 0)
		for i in 1 .. stmt.children_count {
			target_id := t.a.child(&stmt, i)
			target := t.a.nodes[int(target_id)]
			if !target.is_mut || types.unalias_type(t.tc.resolve_type(target_id)) !is types.Pointer {
				continue
			}
			target_param_idx := i - 1 + param_offset
			source_param_idxs := t.tc.call_param_storage_source_params(stmt_id, target_param_idx)
			if source_param_idxs.len == 0 {
				continue
			}
			target_snapshot := if i in argument_origins {
				argument_origins[i].clone()
			} else {
				locals.clone()
			}
			for target_path in t.array_map_lvalue_local_paths(target_id, target_snapshot) {
				target_root := array_map_local_path_root(target_path)
				if target_root !in locals {
					continue
				}
				overlapping := array_map_clear_local_pointer_origins(target_path, mut locals)
				mut replacement_origins := map[string]bool{}
				for source_param_idx in source_param_idxs {
					mut source_id := flat.empty_node
					mut source_child_idx := -1
					if param_offset == 1 && source_param_idx == 0 && callee.kind == .selector && callee.children_count > 0 {
						source_id = t.a.child(callee, 0)
						source_child_idx = 0
					} else {
						child_idx := source_param_idx - param_offset + 1
						if child_idx >= 1 && child_idx < stmt.children_count {
							source_id = t.a.child(&stmt, child_idx)
							source_child_idx = child_idx
						}
					}
					if int(source_id) >= 0 {
						source_origins := if source_child_idx in argument_origins {
							argument_origins[source_child_idx].clone()
						} else {
							locals.clone()
						}
						t.array_map_record_local_pointer_origins(target_path, source_id, elem_name, source_origins, mut replacement_origins)
					}
				}
				for path, external in replacement_origins {
					locals[path] = locals[path] || external
				}
				array_map_merge_overlapping_pointer_origins(overlapping, mut locals)
			}
		}
		return true
	}
	if stmt.kind == .decl_assign {
		mut rhs_ids := []flat.NodeId{cap: int(stmt.children_count) / 2}
		mut rhs_origins := map[int]map[string]bool{}
		for i := 0; i + 1 < int(stmt.children_count); i += 2 {
			rhs_id := t.a.child(&stmt, i + 1)
			if !t.array_map_update_local_pointer_origins_flow(rhs_id, elem_name, mut locals, mut loop_exits, mut return_exits, loop_label, active_defer_count) {
				return false
			}
			pair_idx := rhs_ids.len
			rhs_ids << rhs_id
			rhs_origins[pair_idx] = locals.clone()
		}
		for pair_idx, rhs_id in rhs_ids {
			i := pair_idx * 2
			lhs := t.a.child_node(&stmt, i)
			if lhs.kind == .ident && lhs.value.len > 0 {
				overlapping := array_map_clear_local_pointer_origins(lhs.value, mut locals)
				t.array_map_record_local_pointer_origins(lhs.value, rhs_id, elem_name, rhs_origins[pair_idx], mut locals)
				array_map_merge_overlapping_pointer_origins(overlapping, mut locals)
			}
		}
		return true
	}
	if stmt.kind in [.assign, .selector_assign, .index_assign] {
		mut rhs_ids := []flat.NodeId{cap: int(stmt.children_count) / 2}
		mut rhs_origins := map[int]map[string]bool{}
		for i := 0; i + 1 < int(stmt.children_count); i += 2 {
			rhs_id := t.a.child(&stmt, i + 1)
			if !t.array_map_update_local_pointer_origins_flow(rhs_id, elem_name, mut locals, mut loop_exits, mut return_exits, loop_label, active_defer_count) {
				return false
			}
			pair_idx := rhs_ids.len
			rhs_ids << rhs_id
			rhs_origins[pair_idx] = locals.clone()
		}
		for pair_idx, rhs_id in rhs_ids {
			i := pair_idx * 2
			lhs_id := t.a.child(&stmt, i)
			for path in t.array_map_lvalue_local_paths(lhs_id, locals) {
				root := array_map_local_path_root(path)
				if root in locals {
					overlapping := array_map_clear_local_pointer_origins(path, mut locals)
					t.array_map_record_local_pointer_origins(path, rhs_id, elem_name, rhs_origins[pair_idx], mut locals)
					array_map_merge_overlapping_pointer_origins(overlapping, mut locals)
				}
			}
		}
	}
	return true
}

fn (mut t Transformer) array_map_side_effect_source_retains_element_address(id flat.NodeId, elem_name string, block flat.Node, before_idx int) bool {
	if before_idx >= 0 {
		mut seen := map[string]bool{}
		return t.array_map_block_value_retains_element_address(block, before_idx, id, elem_name, mut seen)
	}
	return t.array_map_expr_result_retains_element_address(id, elem_name)
}

fn (mut t Transformer) array_map_call_side_effect_retains_element_address(id flat.NodeId, node flat.Node, elem_name string, locals map[string]bool, block flat.Node, before_idx int) bool {
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return false
	}
	call_name := t.call_name_for_node(id, node)
	params := t.call_param_types_for_node(call_name, node)
	param_offset := t.call_param_offset_for_node(call_name, node, params)
	callee := t.a.child_node(&node, 0)
	implicitly_borrows_elem := t.array_map_call_implicitly_borrows_ident(id, node, elem_name)
	if t.tc.resolved_call_may_store_globally(id) {
		if implicitly_borrows_elem {
			return true
		}
		if param_offset == 1 && callee.kind == .selector && callee.children_count > 0 && t.array_map_side_effect_source_retains_element_address(t.a.child(callee, 0), elem_name, block, before_idx) {
			return true
		}
		for i in 1 .. node.children_count {
			if t.array_map_side_effect_source_retains_element_address(t.a.child(&node, i), elem_name, block, before_idx) {
				return true
			}
		}
	}
	mut call_has_opaque_body := true
	if resolved_name := t.tc.resolved_call_name(id) {
		call_has_opaque_body = resolved_name.starts_with('C.')
	}
	if call_has_opaque_body {
		// A function value or external C function has no visible body or attributes
		// at this call site. Any pointer-bearing argument or captured callee state may
		// therefore escape even when it is not `mut`.
		if implicitly_borrows_elem {
			return true
		}
		for i in 0 .. node.children_count {
			if t.array_map_side_effect_source_retains_element_address(t.a.child(&node, i), elem_name, block, before_idx) {
				return true
			}
		}
	}
	if !call_has_opaque_body {
		// A resolved wrapper can invoke or store a callback without exposing where the
		// callback's captured pointers flow, so treat capture-bearing callbacks as sinks.
		mut globally_storing_callback_params := []int{}
		for i in 1 .. node.children_count {
			arg_id := t.a.child(&node, i)
			if types.unalias_type(t.tc.resolve_type(arg_id)) !is types.FnType {
				continue
			}
			if t.array_map_side_effect_source_retains_element_address(arg_id, elem_name, block, before_idx) {
				return true
			}
			if t.tc.fn_value_may_store_globally(arg_id) {
				globally_storing_callback_params << i - 1 + param_offset
			}
		}
		if globally_storing_callback_params.len > 0 {
			// A callback can receive a mapped pointer through another wrapper parameter,
			// even when the callback expression itself captures no mapper state.
			for i in 1 .. node.children_count {
				arg_id := t.a.child(&node, i)
				if types.unalias_type(t.tc.resolve_type(arg_id)) !is types.FnType && t.array_map_side_effect_source_retains_element_address(arg_id, elem_name, block, before_idx) {
					source_param_idx := i - 1 + param_offset
					for callback_param_idx in globally_storing_callback_params {
						if t.tc.call_param_flows_to_callback(id, callback_param_idx, source_param_idx) {
							return true
						}
					}
				}
			}
		}
	}
	// Mut lvalues are captured after each preceding physical argument has been
	// evaluated. Preserve that origin state so an earlier rebind is visible when
	// classifying a later target.
	mut argument_origins := map[int]map[string]bool{}
	mut evaluation_locals := locals.clone()
	for i in 0 .. node.children_count {
		t.array_map_update_local_pointer_origins(t.a.child(&node, i), elem_name, mut evaluation_locals)
		argument_origins[i] = evaluation_locals.clone()
	}
	mut target_param_idxs := []int{}
	mut target_ids := []flat.NodeId{}
	mut target_origins := []map[string]bool{}
	if param_offset == 1 && callee.kind == .selector && callee.children_count > 0 && t.tc.mut_receiver_methods[call_name] {
		target_param_idxs << 0
		target_ids << t.a.child(callee, 0)
		target_origins << (argument_origins[0] or { locals }).clone()
	}
	for i in 1 .. node.children_count {
		arg_id := t.a.child(&node, i)
		if t.a.nodes[int(arg_id)].is_mut {
			target_param_idxs << i - 1 + param_offset
			target_ids << arg_id
			target_origins << (argument_origins[i] or { locals }).clone()
		}
	}
	for target_i, target_param_idx in target_param_idxs {
		if !t.array_map_side_effect_target_is_external(target_ids[target_i], elem_name, target_origins[target_i], true) {
			continue
		}
		for source_param_idx in t.tc.call_param_storage_source_params(id, target_param_idx) {
			mut source_id := flat.empty_node
			if param_offset == 1 && source_param_idx == 0 && callee.kind == .selector && callee.children_count > 0 {
				source_id = t.a.child(callee, 0)
			} else {
				child_idx := source_param_idx - param_offset + 1
				if child_idx >= 1 && child_idx < node.children_count {
					source_id = t.a.child(&node, child_idx)
				}
			}
			if int(source_id) >= 0 && t.array_map_side_effect_source_retains_element_address(source_id, elem_name, block, before_idx) {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) array_map_expr_side_effect_retains_element_address(id flat.NodeId, elem_name string) bool {
	locals := map[string]bool{}
	return t.array_map_expr_side_effect_retains_element_address_in_scope(id, elem_name, locals, flat.Node{}, -1)
}

fn (mut t Transformer) array_map_deferred_side_effect_retains_element_address(deferred []flat.NodeId, elem_name string, mut locals map[string]bool) bool {
	for i := deferred.len - 1; i >= 0; i-- {
		body_id := deferred[i]
		if t.array_map_expr_side_effect_retains_element_address_in_scope(body_id, elem_name, locals, flat.Node{}, -1) {
			return true
		}
		t.array_map_update_local_pointer_origins(body_id, elem_name, mut locals)
	}
	return false
}

fn (mut t Transformer) array_map_expr_side_effect_retains_element_address_in_scope(id flat.NodeId, elem_name string, locals map[string]bool, block flat.Node, before_idx int) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return false
	}
	if node.kind in [.block, .match_branch, .select_branch, .for_stmt, .for_in_stmt] {
		mut scoped := locals.clone()
		mut child_indices := array_map_loop_iteration_child_indices(node)
		if node.kind in [.for_stmt, .for_in_stmt] {
			mut loop_label := ''
			if before_idx > 0 && before_idx < block.children_count {
				previous := t.a.child_node(&block, before_idx - 1)
				if previous.kind == .label_stmt {
					loop_label = previous.value
				}
			}
			if node.kind == .for_stmt && node.children_count > 0 {
				init_id := t.a.child(&node, 0)
				if t.array_map_expr_side_effect_retains_element_address_in_scope(init_id, elem_name, scoped, node, 0) {
					return true
				}
				mut init_exits := []ArrayMapLoopPointerExit{}
				mut init_returns := []ArrayMapReturnPointerExit{}
				t.array_map_update_local_pointer_origins_flow(init_id, elem_name, mut scoped, mut init_exits, mut init_returns, '', 0)
			}
			scoped = t.array_map_loop_pointer_origin_fixed_point(node, elem_name, scoped, loop_label, 0)
			child_indices = array_map_loop_iteration_child_indices(node)
		}
		label_origins := if node.kind in [.block, .match_branch, .select_branch] {
			t.array_map_block_goto_label_origins(node, elem_name, scoped, 0)
		} else {
			map[string]map[string]bool{}
		}
		mut deferred := []flat.NodeId{}
		mut loop_exits := []ArrayMapLoopPointerExit{}
		mut return_exits := []ArrayMapReturnPointerExit{}
		mut continues := true
		for i in child_indices {
			stmt_id := t.a.child(&node, i)
			if int(stmt_id) < 0 || int(stmt_id) >= t.a.nodes.len {
				continue
			}
			stmt := t.a.nodes[int(stmt_id)]
			if stmt.kind == .label_stmt {
				if incoming := label_origins[stmt.value] {
					if continues {
						array_map_join_local_pointer_origin_states(mut scoped, incoming)
					} else {
						scoped = incoming.clone()
					}
					continues = true
				}
			}
			if !continues {
				continue
			}
			if stmt.kind == .defer_stmt && stmt.children_count > 0 {
				deferred << t.a.child(&stmt, 0)
				continue
			}
			if t.array_map_expr_side_effect_retains_element_address_in_scope(stmt_id, elem_name, scoped, node, int(i)) {
				return true
			}
			continues = t.array_map_update_local_pointer_origins_flow(stmt_id, elem_name, mut scoped, mut loop_exits, mut return_exits, '', deferred.len)
		}
		for exit in loop_exits {
			if exit.is_goto && exit.label in label_origins {
				continue
			}
			mut exit_state := exit.origins.clone()
			if t.array_map_deferred_side_effect_retains_element_address(deferred[..exit.defer_count], elem_name, mut exit_state) {
				return true
			}
		}
		for exit in return_exits {
			mut exit_state := exit.origins.clone()
			if t.array_map_deferred_side_effect_retains_element_address(deferred[..exit.defer_count], elem_name, mut exit_state) {
				return true
			}
		}
		if continues {
			return t.array_map_deferred_side_effect_retains_element_address(deferred, elem_name, mut scoped)
		}
		return false
	}
	if node.kind == .comptime_if {
		if take_then := t.comptime_type_condition_value(node.value) {
			branch_idx := if take_then { 0 } else { 1 }
			return branch_idx < node.children_count && t.array_map_expr_side_effect_retains_element_address_in_scope(t.a.child(&node, branch_idx), elem_name, locals, block, before_idx)
		}
	}
	if node.kind in [.if_expr, .match_stmt] && node.children_count > 0 {
		condition_id := t.a.child(&node, 0)
		if t.array_map_expr_side_effect_retains_element_address_in_scope(condition_id, elem_name, locals, block, before_idx) {
			return true
		}
		mut branch_locals := locals.clone()
		t.array_map_update_local_pointer_origins(condition_id, elem_name, mut branch_locals)
		for i in 1 .. node.children_count {
			if t.array_map_expr_side_effect_retains_element_address_in_scope(t.a.child(&node, i), elem_name, branch_locals, block, before_idx) {
				return true
			}
		}
		return false
	}
	if t.array_map_call_side_effect_retains_element_address(id, node, elem_name, locals, block, before_idx) {
		return true
	}
	if node.kind == .return_stmt {
		for i in 0 .. node.children_count {
			if t.array_map_side_effect_source_retains_element_address(t.a.child(&node, i), elem_name, block, before_idx) {
				return true
			}
		}
	}
	if node.kind == .spawn_expr && node.children_count > 0 {
		call := t.a.child_node(&node, 0)
		if call.kind == .call {
			for i in 0 .. call.children_count {
				if t.array_map_side_effect_source_retains_element_address(t.a.child(call, i), elem_name, block, before_idx) {
					return true
				}
			}
		}
	}
	if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
		array_id := t.a.child(&node, 0)
		value_id := t.a.child(&node, 1)
		if t.array_map_side_effect_target_is_external(array_id, elem_name, locals, false) && t.array_map_side_effect_source_retains_element_address(value_id, elem_name, block, before_idx) {
			return true
		}
	}
	if node.kind == .infix && node.op == .arrow && node.children_count >= 2 {
		channel_id := t.a.child(&node, 0)
		value_id := t.a.child(&node, 1)
		if t.array_map_side_effect_target_is_external(channel_id, elem_name, locals, true) && t.array_map_side_effect_source_retains_element_address(value_id, elem_name, block, before_idx) {
			return true
		}
	}
	if node.kind == .assign && node.op == .assign && node.children_count >= 4 {
		lhs_count := t.multi_assign_lhs_count(node)
		rhs_count := t.multi_assign_rhs_count(node)
		if lhs_count == rhs_count && rhs_count > 1 {
			// Plain multi-assignment stages every RHS before applying any LHS. Snapshot
			// those effects first so a rebind in an early RHS is visible to every target.
			mut assignment_locals := locals.clone()
			mut rhs_origins := []map[string]bool{cap: rhs_count}
			for i in 0 .. rhs_count {
				rhs_id := t.multi_assign_rhs_id(node, i)
				if t.array_map_expr_side_effect_retains_element_address_in_scope(rhs_id, elem_name, assignment_locals, block, before_idx) {
					return true
				}
				t.array_map_update_local_pointer_origins(rhs_id, elem_name, mut assignment_locals)
				rhs_origins << assignment_locals.clone()
			}
			for i in 0 .. lhs_count {
				lhs_id := t.multi_assign_lhs_id(node, i)
				rhs_id := t.multi_assign_rhs_id(node, i)
				if t.array_map_expr_side_effect_retains_element_address_in_scope(lhs_id, elem_name, assignment_locals, block, before_idx) {
					return true
				}
				t.array_map_update_local_pointer_origins(lhs_id, elem_name, mut assignment_locals)
				if t.array_map_side_effect_target_is_external(lhs_id, elem_name, assignment_locals, false) && t.array_map_side_effect_source_retains_element_address(rhs_id, elem_name, block, before_idx) {
					return true
				}
				for path in t.array_map_lvalue_local_paths(lhs_id, assignment_locals) {
					root := array_map_local_path_root(path)
					if root in assignment_locals {
						overlapping := array_map_clear_local_pointer_origins(path, mut assignment_locals)
						t.array_map_record_local_pointer_origins(path, rhs_id, elem_name, rhs_origins[i], mut assignment_locals)
						array_map_merge_overlapping_pointer_origins(overlapping, mut assignment_locals)
					}
				}
			}
			return false
		}
	}
	if node.kind in [.assign, .selector_assign, .index_assign] {
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			lhs_id := t.a.child(&node, i)
			if t.array_map_side_effect_target_is_external(lhs_id, elem_name, locals, false) && t.array_map_side_effect_source_retains_element_address(t.a.child(&node, i + 1), elem_name, block, before_idx) {
				return true
			}
		}
	}
	mut child_locals := locals.clone()
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if t.array_map_expr_side_effect_retains_element_address_in_scope(child_id, elem_name, child_locals, block, before_idx) {
			return true
		}
		t.array_map_update_local_pointer_origins(child_id, elem_name, mut child_locals)
	}
	return false
}

fn (t &Transformer) array_map_expr_is_call_projection(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .selector, .index] && node.children_count > 0 {
		return t.array_map_expr_is_call_projection(t.a.child(&node, 0))
	}
	return node.kind == .call
}

fn (mut t Transformer) array_map_block_result_retains_element_address(node flat.Node, name string) bool {
	if node.children_count == 0 {
		return false
	}
	mut seen := map[string]bool{}
	return t.array_map_block_value_retains_element_address(node, int(node.children_count) - 1, t.a.child(&node, node.children_count - 1), name, mut seen)
}

fn (mut t Transformer) array_map_block_expr_result_retains_element_address(block flat.Node, before_idx int, id flat.NodeId, name string, mut seen map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .field_init {
			return node.children_count > 0 && t.array_map_block_expr_result_retains_element_address(block, before_idx, t.a.child(&node, 0), name, mut seen)
		}
		.selector {
			return node.children_count > 0 && t.array_map_block_selector_result_retains_element_address(block, before_idx, t.a.child(&node, 0), node.value, name, mut seen)
		}
		.index {
			return node.children_count > 0 && t.array_map_block_index_result_retains_element_address(block, before_idx, node, name, mut seen)
		}
		.comptime_if {
			if take_then := t.comptime_type_condition_value(node.value) {
				branch_idx := if take_then { 0 } else { 1 }
				return branch_idx < node.children_count && t.array_map_block_expr_result_retains_element_address(block, before_idx, t.a.child(&node, branch_idx), name, mut seen)
			}
			for i in 0 .. node.children_count {
				mut child_seen := seen.clone()
				if t.array_map_block_expr_result_retains_element_address(block, before_idx, t.a.child(&node, i), name, mut child_seen) {
					return true
				}
			}
			return false
		}
		.fn_literal {
			for i in 0 .. node.children_count {
				capture_id := t.a.child(&node, i)
				capture := t.a.nodes[int(capture_id)]
				if capture.kind != .ident || capture.value.len == 0 || capture.value in t.active_generic_params {
					continue
				}
				if capture.value == name {
					return true
				}
				mut capture_seen := seen.clone()
				if t.array_map_block_value_retains_element_address(block, before_idx, capture_id, name, mut capture_seen) {
					return true
				}
			}
			return false
		}
		.struct_init, .array_literal, .array_init, .map_init, .assoc {
			for i in 0 .. node.children_count {
				mut child_seen := seen.clone()
				if t.array_map_block_expr_result_retains_element_address(block, before_idx, t.a.child(&node, i), name, mut child_seen) {
					return true
				}
			}
			return false
		}
		else {
			return t.array_map_expr_result_retains_element_address(id, name)
		}
	}
}

fn (mut t Transformer) array_map_block_value_retains_element_address(node flat.Node, before_idx int, id flat.NodeId, name string, mut seen map[string]bool) bool {
	mut result_id := id
	mut result := t.a.nodes[int(result_id)]
	for result.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .field_init] {
		if result.children_count == 0 {
			break
		}
		result_id = t.a.child(&result, 0)
		result = t.a.nodes[int(result_id)]
	}
	if result.kind == .selector && result.children_count > 0 {
		return t.array_map_block_selector_result_retains_element_address(node, before_idx, t.a.child(&result, 0), result.value, name, mut seen)
	}
	if result.kind == .index && result.children_count > 0 {
		return t.array_map_block_index_result_retains_element_address(node, before_idx, result, name, mut seen)
	}
	if result.kind != .ident || result.value.len == 0 {
		return t.array_map_block_expr_result_retains_element_address(node, before_idx, result_id, name, mut seen)
	}
	if result.value in seen {
		return false
	}
	seen[result.value] = true
	for offset in 1 .. before_idx + 1 {
		stmt_idx := before_idx - offset
		stmt_id := t.a.child(&node, stmt_idx)
		stmt := t.a.nodes[int(stmt_id)]
		if stmt.kind == .decl_assign && stmt.children_count == 2 {
			lhs := t.a.child_node(stmt, 0)
			if lhs.kind == .ident && lhs.value == result.value {
				return t.array_map_block_value_retains_element_address(node, stmt_idx, t.a.child(stmt, 1), name, mut seen)
			}
		}
		if stmt.kind == .assign {
			for i := 0; i + 1 < int(stmt.children_count); i += 2 {
				lhs := t.a.child_node(stmt, i)
				if lhs.kind == .ident && lhs.value == result.value {
					return t.array_map_block_value_retains_element_address(node, stmt_idx, t.a.child(stmt, i + 1), name, mut seen)
				}
			}
		}
		if t.array_map_nested_assignment_retains_element_address(stmt_id, result.value, node, stmt_idx, name, seen) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_map_block_stmt_declares_name(stmt flat.Node, name string) bool {
	if stmt.kind != .decl_assign {
		return false
	}
	for i := 0; i + 1 < int(stmt.children_count); i += 2 {
		lhs := t.a.child_node(&stmt, i)
		if lhs.kind == .ident && lhs.value == name {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_map_block_scope_limit(node flat.Node, name string) int {
	for stmt_idx in 0 .. node.children_count {
		if t.array_map_block_stmt_declares_name(t.a.child_node(&node, stmt_idx), name) {
			return int(stmt_idx)
		}
	}
	return int(node.children_count)
}

fn (mut t Transformer) array_map_mutating_call_retains_element_address(id flat.NodeId, node flat.Node, target string, block flat.Node, before_idx int, name string, seen map[string]bool) bool {
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return false
	}
	call_name := t.call_name_for_node(id, node)
	params := t.call_param_types_for_node(call_name, node)
	param_offset := t.call_param_offset_for_node(call_name, node, params)
	mut target_param_idxs := []int{}
	callee := t.a.child_node(&node, 0)
	if param_offset == 1 && callee.kind == .selector && callee.children_count > 0 {
		receiver_id := t.a.child(callee, 0)
		if t.array_map_lvalue_is_rooted_at_ident(receiver_id, target) && t.tc.mut_receiver_methods[call_name] {
			target_param_idxs << 0
		}
	}
	for i in 1 .. node.children_count {
		arg_id := t.a.child(&node, i)
		arg := t.a.nodes[int(arg_id)]
		if arg.is_mut && t.array_map_lvalue_is_rooted_at_ident(arg_id, target) {
			target_param_idx := i - 1 + param_offset
			if target_param_idx !in target_param_idxs {
				target_param_idxs << target_param_idx
			}
		}
	}
	if target_param_idxs.len == 0 {
		return false
	}
	for target_param_idx in target_param_idxs {
		for source_param_idx in t.tc.call_param_storage_source_params(id, target_param_idx) {
			i := source_param_idx - param_offset + 1
			if i < 1 || i >= node.children_count {
				continue
			}
			mut arg_seen := seen.clone()
			if t.array_map_block_value_retains_element_address(block, before_idx, t.a.child(&node, i), name, mut arg_seen) {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) array_map_nested_assignment_retains_element_address(id flat.NodeId, target string, block flat.Node, before_idx int, name string, seen map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if t.array_map_mutating_call_retains_element_address(id, node, target, block, before_idx, name, seen) {
		return true
	}
	if node.kind in [.block, .match_branch] {
		scope_limit := t.array_map_block_scope_limit(node, target)
		for offset in 0 .. scope_limit {
			stmt_idx := scope_limit - 1 - offset
			stmt_id := t.a.child(&node, stmt_idx)
			stmt := t.a.nodes[int(stmt_id)]
			if stmt.kind == .assign {
				for i := 0; i + 1 < int(stmt.children_count); i += 2 {
					lhs := t.a.child_node(stmt, i)
					if lhs.kind == .ident && lhs.value == target {
						mut branch_seen := seen.clone()
						return t.array_map_block_value_retains_element_address(node, stmt_idx, t.a.child(stmt, i + 1), name, mut branch_seen)
					}
				}
			}
			if t.array_map_nested_assignment_retains_element_address(stmt_id, target, node, stmt_idx, name, seen) {
				return true
			}
		}
		return false
	}
	if node.kind == .assign {
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			lhs := t.a.child_node(&node, i)
			if lhs.kind == .ident && lhs.value == target {
				mut branch_seen := seen.clone()
				if t.array_map_block_value_retains_element_address(block, before_idx, t.a.child(&node, i + 1), name, mut branch_seen) {
					return true
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.array_map_nested_assignment_retains_element_address(t.a.child(&node, i), target, block, before_idx, name, seen) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_map_selector_lhs_targets_field(lhs flat.Node, target string, field_name string) bool {
	if lhs.kind != .selector || lhs.value != field_name || lhs.children_count == 0 {
		return false
	}
	mut base := t.a.child_node(&lhs, 0)
	for base.kind in [.paren, .cast_expr, .as_expr] {
		if base.children_count == 0 {
			return false
		}
		base = t.a.child_node(base, 0)
	}
	return base.kind == .ident && base.value == target
}

fn (t &Transformer) array_map_selector_lhs_targets_path(lhs flat.Node, target string, field_path []string) bool {
	if field_path.len == 0 {
		return false
	}
	mut current := lhs
	mut path_idx := field_path.len
	for path_idx > 0 {
		for current.kind in [.paren, .cast_expr, .as_expr] {
			if current.children_count == 0 {
				return false
			}
			current = t.a.nodes[int(t.a.child(&current, 0))]
		}
		if current.kind != .selector || current.children_count == 0 {
			return false
		}
		path_idx--
		if current.value != field_path[path_idx] {
			return false
		}
		current = t.a.nodes[int(t.a.child(&current, 0))]
	}
	for current.kind in [.paren, .cast_expr, .as_expr] {
		if current.children_count == 0 {
			return false
		}
		current = t.a.nodes[int(t.a.child(&current, 0))]
	}
	return current.kind == .ident && current.value == target
}

fn (mut t Transformer) array_map_block_selector_path_retains_element_address(block flat.Node, before_idx int, base_id flat.NodeId, field_path []string, elem_name string, mut seen map[string]bool) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len || field_path.len == 0 {
		return false
	}
	mut source_id := base_id
	mut source := t.a.nodes[int(source_id)]
	mut resolved_path := field_path.clone()
	for {
		if source.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt] && source.children_count > 0 {
			source_id = t.a.child(&source, 0)
			source = t.a.nodes[int(source_id)]
			continue
		}
		if source.kind == .selector && source.children_count > 0 {
			resolved_path.prepend(source.value)
			source_id = t.a.child(&source, 0)
			source = t.a.nodes[int(source_id)]
			continue
		}
		break
	}
	if source.kind == .ident && source.value.len > 0 {
		if source.value in seen {
			return false
		}
		seen[source.value] = true
		for offset in 1 .. before_idx + 1 {
			stmt_idx := before_idx - offset
			stmt := t.a.child_node(&block, stmt_idx)
			if stmt.kind == .decl_assign && stmt.children_count == 2 {
				lhs := t.a.child_node(stmt, 0)
				if lhs.kind == .ident && lhs.value == source.value {
					return t.array_map_block_selector_path_retains_element_address(block, stmt_idx, t.a.child(stmt, 1), resolved_path, elem_name, mut seen)
				}
			}
			if stmt.kind in [.assign, .selector_assign] {
				for i := 0; i + 1 < int(stmt.children_count); i += 2 {
					lhs := t.a.child_node(stmt, i)
					if lhs.kind == .ident && lhs.value == source.value {
						return t.array_map_block_selector_path_retains_element_address(block, stmt_idx, t.a.child(stmt, i + 1), resolved_path, elem_name, mut seen)
					}
					if t.array_map_selector_lhs_targets_path(lhs, source.value, resolved_path) {
						return t.array_map_block_value_retains_element_address(block, stmt_idx, t.a.child(stmt, i + 1), elem_name, mut seen)
					}
				}
			}
		}
	}
	return t.array_map_selector_path_retains_element_address(source_id, resolved_path, 0, elem_name)
}

fn (mut t Transformer) array_map_selector_path_retains_element_address(base_id flat.NodeId, field_path []string, field_idx int, elem_name string) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return false
	}
	if field_idx >= field_path.len {
		return t.array_map_expr_result_retains_element_address(base_id, elem_name)
	}
	mut source_id := base_id
	mut source := t.a.nodes[int(source_id)]
	for source.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt] {
		if source.children_count == 0 {
			break
		}
		source_id = t.a.child(&source, 0)
		source = t.a.nodes[int(source_id)]
	}
	if source.kind in [.struct_init, .assoc] {
		for i in 0 .. source.children_count {
			field_id := t.a.child(&source, i)
			field := t.a.nodes[int(field_id)]
			if field.kind == .field_init && field.value == field_path[field_idx] && field.children_count > 0 {
				return t.array_map_selector_path_retains_element_address(t.a.child(&field, 0), field_path, field_idx + 1, elem_name)
			}
		}
		if source.kind == .assoc && source.children_count > 0 {
			return t.array_map_selector_path_retains_element_address(t.a.child(&source, 0), field_path, field_idx, elem_name)
		}
		return false
	}
	return t.array_map_expr_result_retains_element_address(source_id, elem_name)
}

fn (mut t Transformer) array_map_block_selector_result_retains_element_address(block flat.Node, before_idx int, base_id flat.NodeId, field_name string, elem_name string, mut seen map[string]bool) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return false
	}
	mut source_id := base_id
	mut source := t.a.nodes[int(source_id)]
	for source.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt] {
		if source.children_count == 0 {
			break
		}
		source_id = t.a.child(&source, 0)
		source = t.a.nodes[int(source_id)]
	}
	if source.kind == .selector && source.children_count > 0 {
		return t.array_map_block_selector_path_retains_element_address(block, before_idx, source_id, [
			field_name,
		], elem_name, mut seen)
	}
	if source.kind == .ident && source.value.len > 0 {
		if source.value in seen {
			return false
		}
		seen[source.value] = true
		for offset in 1 .. before_idx + 1 {
			stmt_idx := before_idx - offset
			stmt_id := t.a.child(&block, stmt_idx)
			stmt := t.a.nodes[int(stmt_id)]
			if stmt.kind == .decl_assign && stmt.children_count == 2 {
				lhs := t.a.child_node(stmt, 0)
				if lhs.kind == .ident && lhs.value == source.value {
					return t.array_map_block_selector_result_retains_element_address(block, stmt_idx, t.a.child(stmt, 1), field_name, elem_name, mut seen)
				}
			}
			if stmt.kind in [.assign, .selector_assign] {
				for i := 0; i + 1 < int(stmt.children_count); i += 2 {
					lhs := t.a.child_node(stmt, i)
					if lhs.kind == .ident && lhs.value == source.value {
						return t.array_map_block_selector_result_retains_element_address(block, stmt_idx, t.a.child(stmt, i + 1), field_name, elem_name, mut seen)
					}
					if t.array_map_selector_lhs_targets_field(lhs, source.value, field_name) {
						return t.array_map_block_value_retains_element_address(block, stmt_idx, t.a.child(stmt, i + 1), elem_name, mut seen)
					}
				}
			}
			if t.array_map_nested_selector_assignment_retains_element_address(stmt_id, source.value, block, stmt_idx, field_name, elem_name, seen) {
				return true
			}
		}
	}
	return t.array_map_selector_result_retains_element_address(source_id, field_name, elem_name)
}

fn (mut t Transformer) array_map_nested_selector_assignment_retains_element_address(id flat.NodeId, target string, block flat.Node, before_idx int, field_name string, elem_name string, seen map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if t.array_map_mutating_call_retains_element_address(id, node, target, block, before_idx, elem_name, seen) {
		return true
	}
	if node.kind in [.block, .match_branch] {
		scope_limit := t.array_map_block_scope_limit(node, target)
		for offset in 0 .. scope_limit {
			stmt_idx := scope_limit - 1 - offset
			stmt_id := t.a.child(&node, stmt_idx)
			stmt := t.a.nodes[int(stmt_id)]
			if stmt.kind in [.assign, .selector_assign] {
				for i := 0; i + 1 < int(stmt.children_count); i += 2 {
					lhs := t.a.child_node(stmt, i)
					if lhs.kind == .ident && lhs.value == target {
						mut branch_seen := seen.clone()
						return t.array_map_block_selector_result_retains_element_address(node, stmt_idx, t.a.child(stmt, i + 1), field_name, elem_name, mut branch_seen)
					}
					if t.array_map_selector_lhs_targets_field(lhs, target, field_name) {
						mut branch_seen := seen.clone()
						return t.array_map_block_value_retains_element_address(node, stmt_idx, t.a.child(stmt, i + 1), elem_name, mut branch_seen)
					}
				}
			}
			if t.array_map_nested_selector_assignment_retains_element_address(stmt_id, target, node, stmt_idx, field_name, elem_name, seen) {
				return true
			}
		}
		return false
	}
	if node.kind in [.assign, .selector_assign] {
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			lhs := t.a.child_node(&node, i)
			if lhs.kind == .ident && lhs.value == target {
				mut branch_seen := seen.clone()
				if t.array_map_block_selector_result_retains_element_address(block, before_idx, t.a.child(&node, i + 1), field_name, elem_name, mut branch_seen) {
					return true
				}
			}
			if t.array_map_selector_lhs_targets_field(lhs, target, field_name) {
				mut branch_seen := seen.clone()
				if t.array_map_block_value_retains_element_address(block, before_idx, t.a.child(&node, i + 1), elem_name, mut branch_seen) {
					return true
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.array_map_nested_selector_assignment_retains_element_address(t.a.child(&node, i), target, block, before_idx, field_name, elem_name, seen) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) array_map_block_index_result_retains_element_address(block flat.Node, before_idx int, index_node flat.Node, elem_name string, mut seen map[string]bool) bool {
	base_id := t.a.child(&index_node, 0)
	index_id := if index_node.children_count > 1 {
		t.a.child(&index_node, 1)
	} else {
		flat.empty_node
	}
	return t.array_map_block_index_base_retains_element_address(block, before_idx, base_id, index_id, elem_name, mut seen)
}

fn (t &Transformer) array_map_index_lhs_targets_index(lhs flat.Node, target string, index_id flat.NodeId) bool {
	if lhs.kind != .index || lhs.children_count < 2 {
		return false
	}
	mut base := t.a.child_node(&lhs, 0)
	for base.kind in [.paren, .cast_expr, .as_expr] {
		if base.children_count == 0 {
			return false
		}
		base = t.a.child_node(base, 0)
	}
	if base.kind != .ident || base.value != target {
		return false
	}
	if int(index_id) < 0 || int(index_id) >= t.a.nodes.len {
		return true
	}
	lhs_index := t.a.child_node(&lhs, 1)
	result_index := t.a.nodes[int(index_id)]
	if lhs_index.kind == .int_literal && result_index.kind == .int_literal && is_decimal_text(lhs_index.value) && is_decimal_text(result_index.value) {
		return lhs_index.value.int() == result_index.value.int()
	}
	// Dynamic indices can select the same slot at runtime.
	return true
}

fn (mut t Transformer) array_map_block_index_base_retains_element_address(block flat.Node, before_idx int, base_id flat.NodeId, index_id flat.NodeId, elem_name string, mut seen map[string]bool) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return false
	}
	mut source_id := base_id
	mut source := t.a.nodes[int(source_id)]
	for source.kind in [.paren, .cast_expr, .as_expr, .dump_expr, .expr_stmt, .postfix] {
		if source.children_count == 0 {
			break
		}
		source_id = t.a.child(&source, 0)
		source = t.a.nodes[int(source_id)]
	}
	if source.kind == .ident && source.value.len > 0 {
		if source.value in seen {
			return false
		}
		seen[source.value] = true
		for offset in 1 .. before_idx + 1 {
			stmt_idx := before_idx - offset
			stmt_id := t.a.child(&block, stmt_idx)
			stmt := t.a.nodes[int(stmt_id)]
			if stmt.kind == .decl_assign && stmt.children_count == 2 {
				lhs := t.a.child_node(stmt, 0)
				if lhs.kind == .ident && lhs.value == source.value {
					return t.array_map_block_index_base_retains_element_address(block, stmt_idx, t.a.child(stmt, 1), index_id, elem_name, mut seen)
				}
			}
			if stmt.kind in [.assign, .index_assign] {
				for i := 0; i + 1 < int(stmt.children_count); i += 2 {
					lhs := t.a.child_node(stmt, i)
					if lhs.kind == .ident && lhs.value == source.value {
						return t.array_map_block_index_base_retains_element_address(block, stmt_idx, t.a.child(stmt, i + 1), index_id, elem_name, mut seen)
					}
					if t.array_map_index_lhs_targets_index(lhs, source.value, index_id) {
						return t.array_map_block_value_retains_element_address(block, stmt_idx, t.a.child(stmt, i + 1), elem_name, mut seen)
					}
				}
			}
			if t.array_map_nested_index_assignment_retains_element_address(stmt_id, source.value, block, stmt_idx, index_id, elem_name, seen) {
				return true
			}
		}
	}
	if source.kind == .array_literal && int(index_id) >= 0 {
		index := t.a.nodes[int(index_id)]
		if index.kind == .int_literal {
			selected := index.value.int()
			if selected >= 0 && selected < int(source.children_count) {
				return t.array_map_expr_result_retains_element_address(t.a.child(&source, selected), elem_name)
			}
		}
	}
	return t.array_map_expr_result_retains_element_address(source_id, elem_name)
}

fn (mut t Transformer) array_map_nested_index_assignment_retains_element_address(id flat.NodeId, target string, block flat.Node, before_idx int, index_id flat.NodeId, elem_name string, seen map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if t.array_map_mutating_call_retains_element_address(id, node, target, block, before_idx, elem_name, seen) {
		return true
	}
	if node.kind in [.block, .match_branch] {
		scope_limit := t.array_map_block_scope_limit(node, target)
		for offset in 0 .. scope_limit {
			stmt_idx := scope_limit - 1 - offset
			stmt_id := t.a.child(&node, stmt_idx)
			stmt := t.a.nodes[int(stmt_id)]
			if stmt.kind in [.assign, .index_assign] {
				for i := 0; i + 1 < int(stmt.children_count); i += 2 {
					lhs := t.a.child_node(stmt, i)
					if lhs.kind == .ident && lhs.value == target {
						mut branch_seen := seen.clone()
						return t.array_map_block_index_base_retains_element_address(node, stmt_idx, t.a.child(stmt, i + 1), index_id, elem_name, mut branch_seen)
					}
					if t.array_map_index_lhs_targets_index(lhs, target, index_id) {
						mut branch_seen := seen.clone()
						return t.array_map_block_value_retains_element_address(node, stmt_idx, t.a.child(stmt, i + 1), elem_name, mut branch_seen)
					}
				}
			}
			if t.array_map_nested_index_assignment_retains_element_address(stmt_id, target, node, stmt_idx, index_id, elem_name, seen) {
				return true
			}
		}
		return false
	}
	if node.kind in [.assign, .index_assign] {
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			lhs := t.a.child_node(&node, i)
			if lhs.kind == .ident && lhs.value == target {
				mut branch_seen := seen.clone()
				if t.array_map_block_index_base_retains_element_address(block, before_idx, t.a.child(&node, i + 1), index_id, elem_name, mut branch_seen) {
					return true
				}
			}
			if t.array_map_index_lhs_targets_index(lhs, target, index_id) {
				mut branch_seen := seen.clone()
				if t.array_map_block_value_retains_element_address(block, before_idx, t.a.child(&node, i + 1), elem_name, mut branch_seen) {
					return true
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.array_map_nested_index_assignment_retains_element_address(t.a.child(&node, i), target, block, before_idx, index_id, elem_name, seen) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) array_map_index_result_retains_element_address(node flat.Node, name string) bool {
	if node.children_count == 0 {
		return false
	}
	base_id := t.a.child(&node, 0)
	if node.children_count > 1 {
		base := t.a.nodes[int(base_id)]
		index := t.a.child_node(&node, 1)
		if base.kind == .array_literal && index.kind == .int_literal {
			selected := index.value.int()
			if selected >= 0 && selected < int(base.children_count) {
				return t.array_map_expr_result_retains_element_address(t.a.child(&base, selected), name)
			}
		}
	}
	return t.array_map_expr_result_retains_element_address(base_id, name)
}

fn (mut t Transformer) array_map_selector_result_retains_element_address(base_id flat.NodeId, field_name string, elem_name string) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return false
	}
	base := t.a.nodes[int(base_id)]
	if base.kind in [.paren, .cast_expr, .as_expr, .expr_stmt] && base.children_count > 0 {
		return t.array_map_selector_result_retains_element_address(t.a.child(&base, 0), field_name, elem_name)
	}
	if base.kind in [.struct_init, .assoc] {
		for i in 0 .. base.children_count {
			field_id := t.a.child(&base, i)
			field := t.a.nodes[int(field_id)]
			if field.kind == .field_init && field.value == field_name && field.children_count > 0 {
				return t.array_map_expr_result_retains_element_address(t.a.child(&field, 0), elem_name)
			}
		}
		if base.kind == .assoc && base.children_count > 0 {
			return t.array_map_selector_result_retains_element_address(t.a.child(&base, 0), field_name, elem_name)
		}
		return false
	}
	return t.array_map_expr_result_retains_element_address(base_id, elem_name)
}

fn (t &Transformer) array_map_result_can_retain_element_address(type_name string) bool {
	if type_name.len == 0 {
		return false
	}
	if isnil(t.tc) {
		clean := t.normalize_type_alias(type_name)
		return clean.starts_with('&') || clean in ['voidptr', 'byteptr', 'charptr']
	}
	mut seen := map[string]bool{}
	return t.array_map_type_can_hold_pointer(t.tc.parse_type(type_name), mut seen)
}

fn (t &Transformer) array_map_type_can_hold_pointer(typ types.Type, mut seen map[string]bool) bool {
	return match typ {
		types.Pointer { true }
		types.Alias {
			t.array_map_type_can_hold_pointer(typ.base_type, mut seen)
		}
		types.OptionType {
			t.array_map_type_can_hold_pointer(typ.base_type, mut seen)
		}
		types.ResultType {
			t.array_map_type_can_hold_pointer(typ.base_type, mut seen)
		}
		types.Array {
			t.array_map_type_can_hold_pointer(typ.elem_type, mut seen)
		}
		types.ArrayFixed {
			t.array_map_type_can_hold_pointer(typ.elem_type, mut seen)
		}
		types.Channel {
			t.array_map_type_can_hold_pointer(typ.elem_type, mut seen)
		}
		types.Map {
			t.array_map_type_can_hold_pointer(typ.key_type, mut seen)
				|| t.array_map_type_can_hold_pointer(typ.value_type, mut seen)
		}
		types.Struct {
			if typ.name in seen {
				false
			} else {
				seen[typ.name] = true
				mut has_pointer := false
				for field in t.tc.struct_fields_for_type(typ.name) {
					if t.array_map_type_can_hold_pointer(field.typ, mut seen) {
						has_pointer = true
						break
					}
				}
				has_pointer
			}
		}
		types.SumType {
			if typ.name in seen {
				false
			} else {
				seen[typ.name] = true
				mut has_pointer := false
				for variant in t.concrete_sum_variants_for_candidate(typ.name) {
					if t.array_map_type_can_hold_pointer(t.tc.parse_type(variant), mut seen) {
						has_pointer = true
						break
					}
				}
				has_pointer
			}
		}
		types.MultiReturn {
			mut has_pointer := false
			for item in typ.types {
				if t.array_map_type_can_hold_pointer(item, mut seen) {
					has_pointer = true
					break
				}
			}
			has_pointer
		}
		types.FnType, types.Interface { true }
		else { false }
	}
}

fn (t &Transformer) resolve_fn_value_expr(id flat.NodeId, node flat.Node) ?string {
	if !isnil(t.tc) {
		if name := t.tc.resolved_fn_value_name(id) {
			return name
		}
	}
	if node.kind == .ident {
		return t.resolve_fn_value_ident(node.value)
	}
	if node.kind == .selector {
		return t.resolve_fn_value_selector(node)
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.resolve_fn_value_expr(t.a.child(&node, 0), t.a.child_node(&node, 0))
	}
	return none
}

fn (t &Transformer) resolve_fn_value_selector(node flat.Node) ?string {
	if node.children_count == 0 || node.value.len == 0 || isnil(t.tc) {
		return none
	}
	base := t.a.child_node(&node, 0)
	if base.kind == .ident {
		if t.var_type(base.value).len > 0 {
			return none
		}
		if resolved := t.resolve_static_fn_value_for_type(base.value, node.value) {
			return resolved
		}
		key := '${base.value}.${node.value}'
		if key in t.tc.fn_ret_types {
			return key
		}
		return none
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(base, 0)
		if inner.kind == .ident {
			type_name := '${inner.value}.${base.value}'
			if resolved := t.resolve_static_fn_value_for_type(type_name, node.value) {
				return resolved
			}
			key := '${type_name}.${node.value}'
			if key in t.tc.fn_ret_types {
				return key
			}
		}
	}
	return none
}

fn (t &Transformer) resolve_static_fn_value_for_type(type_name string, method string) ?string {
	if type_name.len == 0 || method.len == 0 || isnil(t.tc) {
		return none
	}
	mut candidates := []string{}
	t.add_static_fn_value_type_candidate(mut candidates, type_name)
	if !type_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		t.add_static_fn_value_type_candidate(mut candidates, '${t.cur_module}.${type_name}')
	}
	for candidate in candidates {
		key := '${candidate}.${method}'
		if key in t.tc.fn_ret_types {
			return key
		}
	}
	return none
}

fn (t &Transformer) add_static_fn_value_type_candidate(mut candidates []string, name string) {
	if name.len == 0 || isnil(t.tc) {
		return
	}
	if name !in candidates {
		candidates << name
	}
	if target := t.tc.type_aliases[name] {
		if target !in candidates {
			candidates << target
		}
	}
}

fn (t &Transformer) array_map_bound_method_info(node flat.Node, elem_name string, elem_type string, result_elem_type string) ?BoundMethodArrayInfo {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	if !(result_elem_type.starts_with('fn()') || result_elem_type.starts_with('fn ()')) {
		return none
	}
	base := t.a.child_node(&node, 0)
	if base.kind != .ident || base.value != elem_name {
		return none
	}
	receiver_type := t.bound_builtin_method_receiver_type(elem_type, node.value) or { return none }
	return_type := t.local_fn_value_return_type_from_type(result_elem_type) or { return none }
	if return_type == 'void' {
		return none
	}
	return BoundMethodArrayInfo{
		receiver_type: receiver_type
		fn_type: result_elem_type
		method: node.value
		return_type: return_type
	}
}

fn (t &Transformer) bound_builtin_method_receiver_type(elem_type string, method string) ?string {
	if method !in ['hex', 'hex_full'] {
		return none
	}
	mut clean := t.normalize_type_alias(elem_type)
	if clean.starts_with('&') {
		return none
	}
	if clean == 'byte' {
		clean = 'u8'
	}
	if clean in ['u8', 'i8', 'u16', 'i16', 'u32', 'int', 'u64', 'i64', 'rune'] {
		return clean
	}
	return none
}

fn (t &Transformer) fn_value_return_type_name(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .cast_expr, .as_expr, .expr_stmt {
			if node.children_count == 1 {
				return t.fn_value_return_type_name(t.a.child(&node, 0))
			}
		}
		.block, .match_branch {
			if node.children_count > 0 {
				return t.fn_value_return_type_name(t.a.child(&node, node.children_count - 1))
			}
		}
		.if_expr, .match_stmt {
			if node.children_count > 1 {
				return t.fn_value_return_type_name(t.a.child(&node, 1))
			}
		}
		else {}
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			if ret := fn_value_return_type_from_type(typ) {
				return t.normalize_type_alias(ret)
			}
		}
		if node.kind == .fn_literal || node.kind == .lambda_expr {
			typ := t.tc.resolve_type(id)
			if ret := fn_value_return_type_from_type(typ) {
				return t.normalize_type_alias(ret)
			}
		}
	}
	mut typ := t.checker_expr_type_name(id) or { '' }
	if typ.len == 0 {
		typ = node.typ
	}
	if typ.len == 0 && node.kind == .ident {
		typ = t.var_type(node.value)
	}
	if typ.len == 0 || isnil(t.tc) {
		return none
	}
	parsed := t.tc.parse_type(typ)
	if parsed is types.FnType {
		name := parsed.return_type.name()
		if name.len > 0 {
			return t.normalize_type_alias(name)
		}
	}
	return none
}

fn fn_value_return_type_from_type(typ types.Type) ?string {
	if typ is types.FnType {
		return typ.return_type.name()
	}
	if typ is types.Alias {
		if typ.base_type is types.FnType {
			return typ.base_type.return_type.name()
		}
	}
	return none
}

// selector_expr_node supports selector expr node handling for Transformer.
fn (t &Transformer) selector_expr_node(id flat.NodeId) ?flat.Node {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector {
		return node
	}
	return none
}

// substitute_ident supports substitute ident handling for Transformer.
fn (mut t Transformer) substitute_ident(id flat.NodeId, name string, replacement string) flat.NodeId {
	if int(id) < 0 || name.len == 0 || replacement.len == 0 || name == replacement {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		new_id := t.make_ident(replacement)
		if t.a.nodes[int(new_id)].typ.len == 0 {
			t.set_node_typ(int(new_id), node.typ)
		}
		return new_id
	}
	if node.kind == .lambda_expr && node.children_count > 1 {
		first := t.a.child_node(&node, 0)
		if first.kind == .ident && first.value == name {
			return id
		}
	}
	if node.kind == .call && node.children_count > 1 {
		fn_id := t.a.child(&node, 0)
		fn_node := t.a.nodes[int(fn_id)]
		if fn_node.kind == .selector && fn_node.value in ['any', 'all', 'count', 'filter', 'map'] {
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << t.substitute_ident(fn_id, name, replacement)
			for i in 1 .. node.children_count {
				new_children << t.a.child(&node, i)
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind: node.kind
				op: node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos: node.pos
				value: node.value
				typ: node.typ
			})
		}
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		new_children << t.substitute_ident(t.a.child(&node, i), name, replacement)
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: node.pos
		value: node.value
		typ: node.typ
		payload: flat.node_payload(node.generic_params().clone())
	})
}

fn (mut t Transformer) substitute_ident_expr(id flat.NodeId, name string, replacement flat.NodeId) flat.NodeId {
	if int(id) < 0 || name.len == 0 || int(replacement) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return replacement
	}
	if node.kind == .lambda_expr && node.children_count > 1 {
		first := t.a.child_node(&node, 0)
		if first.kind == .ident && first.value == name {
			return id
		}
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if name == 'index' && node.kind == .array_init && child.kind == .field_init
			&& child.value == 'init' {
			new_children << child_id
			continue
		}
		new_children << t.substitute_ident_expr(child_id, name, replacement)
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: node.pos
		value: node.value
		typ: node.typ
		payload: flat.node_payload(node.generic_params().clone())
	})
}

fn (mut t Transformer) infer_map_init_entry_type(node flat.Node) string {
	if node.kind != .map_init || node.children_count < 2 {
		return ''
	}
	key_type := t.map_init_entry_value_type(t.a.child(&node, 0))
	value_type := t.map_init_entry_value_type(t.a.child(&node, 1))
	if key_type.len == 0 || value_type.len == 0 {
		return ''
	}
	return 'map[${key_type}]${value_type}'
}

fn (mut t Transformer) map_init_entry_value_type(id flat.NodeId) string {
	mut typ := t.array_literal_child_value_type(id)
	if !t.generic_arg_is_unresolved(typ) || int(id) < 0 || int(id) >= t.a.nodes.len {
		return typ
	}
	node := t.a.nodes[int(id)]
	if node.kind == .map_init {
		inferred := t.infer_map_init_entry_type(node)
		if inferred.len > 0 && !t.generic_arg_is_unresolved(inferred) {
			return inferred
		}
	}
	if node.kind == .call {
		concrete := t.concrete_generic_call_return_type(id, node)
		if concrete.len > 0 && !t.generic_arg_is_unresolved(concrete) {
			if spec := t.generic_call_spec_cache[int(id)] {
				decls := t.cached_generic_fn_decls()
				if decl := decls[spec.decl_key] {
					display := t.specialized_fn_return_display_type_text(decl, spec.args)
					if display.len > 0 && !t.generic_arg_is_unresolved(display) {
						return display
					}
				}
			}
			typ = concrete
		}
	}
	return typ
}

fn (t &Transformer) is_array_transform_call(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	if fn_node.value !in ['filter', 'map', 'sorted'] {
		return false
	}
	base_type := t.normalize_type_alias(t.node_type(t.a.child(&fn_node, 0)))
	return base_type.starts_with('[]')
}

// lower_array_count_call builds lower array count call data for transform.
fn (mut t Transformer) lower_array_count_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	source_is_owned_temporary := !t.expr_can_take_address(base_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_transformed_expr_for_reuse(t.transform_expr(base_id), base_type, 'count_source')
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('count')
	idx_name := t.new_temp('count_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(0), 'int')
	mut cleanup_guard_name := ''
	if source_is_owned_temporary {
		cleanup_guard_name = t.new_temp('count_source_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drop := t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name), t.make_block([
			deferred_drop,
		]), t.make_empty())
		defer_body := t.make_block([guarded_drop])
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind: .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	default_elem_name := t.new_temp('count_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	predicate_id := t.a.child(&node, 1)
	elem_name, predicate, callback_setup, predicate_pending := t.transform_array_predicate(predicate_id, default_elem_name, elem_type, 'count_callback')
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	for stmt in callback_setup {
		prefix << stmt
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	for stmt in predicate_pending {
		loop_body << stmt
	}
	inc := t.make_assign_op(t.make_ident(result_name), t.make_int_literal(1), .plus_assign)
	loop_body << t.make_if(predicate, t.make_block([inc]), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_any_all_call builds lower array any all call data for transform.
fn (mut t Transformer) lower_array_any_all_call(node flat.Node, fn_node flat.Node, base_type string, method string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	source_is_owned_temporary := !t.expr_can_take_address(base_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_transformed_expr_for_reuse(t.transform_expr(base_id), base_type, '${method}_source')
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp(method)
	idx_name := t.new_temp('${method}_idx')
	default_value := if method == 'all' {
		t.make_bool_literal(true)
	} else {
		t.make_bool_literal(false)
	}
	prefix << t.make_decl_assign_typed(result_name, default_value, 'bool')
	mut cleanup_guard_name := ''
	if source_is_owned_temporary {
		cleanup_guard_name = t.new_temp('${method}_source_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drop := t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name), t.make_block([
			deferred_drop,
		]), t.make_empty())
		defer_body := t.make_block([guarded_drop])
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind: .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	default_elem_name := t.new_temp('${method}_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	predicate_id := t.a.child(&node, 1)
	elem_name, predicate, callback_setup, predicate_pending := t.transform_array_predicate(predicate_id, default_elem_name, elem_type, '${method}_callback')
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	for stmt in callback_setup {
		prefix << stmt
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	for stmt in predicate_pending {
		loop_body << stmt
	}
	if method == 'all' {
		not_predicate := t.make_prefix(.not, t.make_paren(predicate))
		assign_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
		loop_body << t.make_if(not_predicate, t.make_block([assign_false]), t.make_empty())
	} else {
		assign_true := t.make_assign(t.make_ident(result_name), t.make_bool_literal(true))
		loop_body << t.make_if(predicate, t.make_block([assign_true]), t.make_empty())
	}
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_sort_call builds lower array sort call data for transform.
fn (mut t Transformer) lower_array_sort_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if !clean_base_type.starts_with('[]') && !(clean_base_type.starts_with('&')
		&& clean_base_type[1..].starts_with('[]')) {
		return none
	}
	if node.children_count > 2 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.transform_lvalue(base_id)
	clean_type := if clean_base_type.starts_with('&') {
		clean_base_type[1..]
	} else {
		clean_base_type
	}
	t.set_node_typ(int(base), clean_type)
	elem_type := clean_type[2..]
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(base, elem_type, node, cmp_id)
	return t.make_empty()
}

fn transform_unshared_receiver_type(typ string) string {
	mut prefix := ''
	mut clean := typ.trim_space()
	if clean.starts_with('&') {
		prefix = '&'
		clean = clean[1..].trim_space()
	}
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	return prefix + clean
}

// lower_array_sorted_call builds lower array sorted call data for transform.
fn (mut t Transformer) lower_array_sorted_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if node.children_count > 2 || !clean_base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_array_clone_call(base_id, clean_base_type)
	t.set_var_type(clone_name, clean_base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, clean_base_type)
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(t.make_ident(clone_name), clean_base_type[2..], node, cmp_id)
	return t.make_ident(clone_name)
}

// lower_array_sort_with_compare_call builds lower array sort with compare call data for transform.
fn (mut t Transformer) lower_array_sort_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if node.children_count != 2
		|| (!clean_base_type.starts_with('[]') && !(clean_base_type.starts_with('&')
			&& clean_base_type[1..].starts_with('[]'))) {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.transform_lvalue(base_id)
	clean_type := if clean_base_type.starts_with('&') {
		clean_base_type[1..]
	} else {
		clean_base_type
	}
	t.set_node_typ(int(base), clean_type)
	elem_type := clean_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.pending_stmts << t.make_array_compare_sort_stmt(base, elem_type, node, cmp)
	return t.make_empty()
}

// lower_array_sorted_with_compare_call supports lower_array_sorted_with_compare_call handling.
fn (mut t Transformer) lower_array_sorted_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if node.children_count != 2 || !clean_base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_array_clone_call(base_id, clean_base_type)
	elem_type := clean_base_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.set_var_type(clone_name, clean_base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, clean_base_type)
	t.pending_stmts << t.make_array_compare_sort_stmt(t.make_ident(clone_name), elem_type, node, cmp)
	return t.make_ident(clone_name)
}

// stable_array_compare_fn supports stable array compare fn handling for Transformer.
fn (mut t Transformer) stable_array_compare_fn(cmp_id flat.NodeId, elem_type string) flat.NodeId {
	if int(cmp_id) >= 0 && t.a.nodes[int(cmp_id)].kind == .lambda_expr {
		return cmp_id
	}
	cmp_type := t.array_compare_fn_type(cmp_id, elem_type)
	cmp := t.transform_expr(cmp_id)
	return t.stable_transformed_expr_for_reuse(cmp, cmp_type, 'sort_cmp')
}

fn (t &Transformer) array_compare_fn_type(cmp_id flat.NodeId, elem_type string) string {
	default_type := 'fn (&${elem_type}, &${elem_type}) int'
	if isnil(t.tc) || int(cmp_id) < 0 || int(cmp_id) >= t.a.nodes.len {
		return default_type
	}
	cmp_node := t.a.nodes[int(cmp_id)]
	raw_type := if cmp_node.kind == .ident {
		t.raw_var_type(cmp_node.value)
	} else {
		t.raw_checker_node_type(cmp_id)
	}
	if raw_type.len == 0 {
		return default_type
	}
	if types.unalias_type(t.tc.parse_type(raw_type)) is types.FnType {
		return raw_type
	}
	return default_type
}

// make_array_default_sort_stmt builds make array default sort stmt data for transform.
fn (mut t Transformer) make_array_default_sort_stmt(base flat.NodeId, elem_type string, src flat.Node, cmp_id flat.NodeId) flat.NodeId {
	if int(cmp_id) < 0 {
		if helper := t.array_default_sort_runtime_helper(elem_type) {
			base_addr := t.make_prefix(.amp, base)
			return t.make_expr_stmt(t.make_call_typed(helper, [base_addr], 'void'))
		}
	}
	i_name := t.new_temp('sort_i')
	j_name := t.new_temp('sort_j')
	tmp_name := t.new_temp('sort_tmp')
	t.set_var_type(i_name, 'int')
	t.set_var_type(j_name, 'int')
	t.set_var_type(tmp_name, elem_type)
	init := t.make_decl_assign_typed(i_name, t.make_int_literal(1), 'int')
	cond := t.make_infix(.lt, t.make_ident(i_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(i_name), .inc))
	j_decl := t.make_decl_assign_typed(j_name, t.make_ident(i_name), 'int')
	inner_cond := t.make_infix(.logical_and, t.make_infix(.gt, t.make_ident(j_name), t.make_int_literal(0)), t.array_sort_less_expr(base, elem_type, j_name, cmp_id))
	tmp_decl := t.make_decl_assign_typed(tmp_name, t.make_index(base, t.make_ident(j_name), elem_type), elem_type)
	prev_idx := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_cur := t.make_index_assign(t.make_index(base, t.make_ident(j_name), elem_type), t.make_index(base, prev_idx, elem_type))
	prev_idx2 := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_prev := t.make_index_assign(t.make_index(base, prev_idx2, elem_type), t.make_ident(tmp_name))
	dec_j := t.make_expr_stmt(t.make_postfix(t.make_ident(j_name), .dec))
	inner_body := [tmp_decl, assign_cur, assign_prev, dec_j]
	inner_for := t.make_for_stmt(t.make_empty(), inner_cond, t.make_empty(), inner_body, src)
	return t.make_for_stmt(init, cond, post, [j_decl, inner_for], src)
}

fn (t &Transformer) array_default_sort_runtime_helper(elem_type string) ?string {
	clean := t.normalize_type_alias(elem_type)
	if clean in ['int', 'i8', 'i16', 'i64', 'u8', 'u16', 'u32', 'u64', 'isize', 'usize', 'f32',
		'f64', 'rune', 'char'] {
		return 'v3_array_sort_${clean}'
	}
	return none
}

// make_array_compare_sort_stmt builds make array compare sort stmt data for transform.
fn (mut t Transformer) make_array_compare_sort_stmt(base flat.NodeId, elem_type string, src flat.Node, cmp flat.NodeId) flat.NodeId {
	i_name := t.new_temp('sort_i')
	j_name := t.new_temp('sort_j')
	tmp_name := t.new_temp('sort_tmp')
	t.set_var_type(i_name, 'int')
	t.set_var_type(j_name, 'int')
	t.set_var_type(tmp_name, elem_type)
	init := t.make_decl_assign_typed(i_name, t.make_int_literal(1), 'int')
	cond := t.make_infix(.lt, t.make_ident(i_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(i_name), .inc))
	j_decl := t.make_decl_assign_typed(j_name, t.make_ident(i_name), 'int')
	inner_cond := t.make_infix(.logical_and, t.make_infix(.gt, t.make_ident(j_name), t.make_int_literal(0)), t.array_sort_compare_less_expr(base, elem_type, j_name, cmp))
	tmp_decl := t.make_decl_assign_typed(tmp_name, t.make_index(base, t.make_ident(j_name), elem_type), elem_type)
	prev_idx := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_cur := t.make_index_assign(t.make_index(base, t.make_ident(j_name), elem_type), t.make_index(base, prev_idx, elem_type))
	prev_idx2 := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_prev := t.make_index_assign(t.make_index(base, prev_idx2, elem_type), t.make_ident(tmp_name))
	dec_j := t.make_expr_stmt(t.make_postfix(t.make_ident(j_name), .dec))
	inner_body := [tmp_decl, assign_cur, assign_prev, dec_j]
	inner_for := t.make_for_stmt(t.make_empty(), inner_cond, t.make_empty(), inner_body, src)
	return t.make_for_stmt(init, cond, post, [j_decl, inner_for], src)
}

// array_sort_less_expr supports array sort less expr handling for Transformer.
fn (mut t Transformer) array_sort_less_expr(base flat.NodeId, elem_type string, idx_name string, cmp_id flat.NodeId) flat.NodeId {
	cur := t.make_index(base, t.make_ident(idx_name), elem_type)
	prev := t.make_index(base, t.make_infix(.minus, t.make_ident(idx_name), t.make_int_literal(1)), elem_type)
	if int(cmp_id) >= 0 {
		cmp_node := t.a.nodes[int(cmp_id)]
		if cmp_node.kind == .lambda_expr && cmp_node.children_count >= 3 {
			if cmp := t.array_sort_lambda_expr(cmp_node, cur, prev, elem_type, elem_type) {
				return cmp
			}
		}
		if cmp := t.array_sort_simple_operator_expr(cmp_node, cur, prev, elem_type) {
			return cmp
		}
		old_a := t.var_type('a')
		old_b := t.var_type('b')
		t.set_var_type('a', elem_type)
		t.set_var_type('b', elem_type)
		raw_cmp := t.substitute_array_sort_vars(cmp_id, cur, prev)
		cmp := t.transform_expr(raw_cmp)
		if old_a.len > 0 {
			t.set_var_type('a', old_a)
		} else {
			t.unset_var_type('a')
		}
		if old_b.len > 0 {
			t.set_var_type('b', old_b)
		} else {
			t.unset_var_type('b')
		}
		return cmp
	}
	if elem_type == 'string' {
		return t.make_call_typed('string__lt', [cur, prev], 'bool')
	}
	if cmp := t.array_sort_struct_less_expr(cur, prev, elem_type) {
		return cmp
	}
	return t.make_infix(.lt, cur, prev)
}

fn (mut t Transformer) array_sort_struct_less_expr(cur flat.NodeId, prev flat.NodeId, elem_type string) ?flat.NodeId {
	mut struct_type := t.struct_lookup_name(elem_type)
	if struct_type.len == 0 {
		struct_type = t.generic_struct_instance_name(elem_type)
	}
	if struct_type.len == 0 {
		return none
	}
	call_info := t.struct_operator_call_info(struct_type, .lt) or { return none }
	args := if call_info.reverse { [prev, cur] } else { [cur, prev] }
	t.mark_fn_used_name(call_info.name)
	call := t.make_call_typed(call_info.name, args, 'bool')
	if call_info.negate {
		return t.make_prefix(.not, call)
	}
	return call
}

fn (mut t Transformer) array_sort_simple_operator_expr(node flat.Node, cur flat.NodeId, prev flat.NodeId, elem_type string) ?flat.NodeId {
	if node.kind != .infix || node.children_count < 2 {
		return none
	}
	lhs_node := t.a.child_node(&node, 0)
	rhs_node := t.a.child_node(&node, 1)
	if lhs_node.kind != .ident || rhs_node.kind != .ident {
		return none
	}
	if lhs_node.value !in ['a', 'b'] || rhs_node.value !in ['a', 'b'] {
		return none
	}
	struct_type := t.struct_lookup_name(elem_type)
	if struct_type.len == 0 {
		return none
	}
	call_info := t.struct_operator_call_info(struct_type, node.op) or { return none }
	lhs := if lhs_node.value == 'a' { cur } else { prev }
	rhs := if rhs_node.value == 'a' { cur } else { prev }
	args := if call_info.reverse { [rhs, lhs] } else { [lhs, rhs] }
	t.mark_fn_used_name(call_info.name)
	call := t.make_call_typed(call_info.name, args, node.typ)
	if call_info.negate {
		return t.make_prefix(.not, call)
	}
	return call
}

// array_sort_compare_less_expr supports array sort compare less expr handling for Transformer.
fn (mut t Transformer) array_sort_compare_less_expr(base flat.NodeId, elem_type string, idx_name string, cmp flat.NodeId) flat.NodeId {
	cur := t.make_index(base, t.make_ident(idx_name), elem_type)
	prev := t.make_index(base, t.make_infix(.minus, t.make_ident(idx_name), t.make_int_literal(1)), elem_type)
	cmp_cur_type, cmp_prev_type := t.array_sort_compare_arg_types(cmp, elem_type)
	cur_arg := if cmp_cur_type == elem_type { cur } else { t.make_prefix(.amp, cur) }
	prev_arg := if cmp_prev_type == elem_type { prev } else { t.make_prefix(.amp, prev) }
	if int(cmp) >= 0 {
		cmp_node := t.a.nodes[int(cmp)]
		if cmp_node.kind == .lambda_expr && cmp_node.children_count >= 3 {
			if call_value := t.array_sort_lambda_expr(cmp_node, cur_arg, prev_arg, cmp_cur_type, cmp_prev_type) {
				return t.make_infix(.lt, call_value, t.make_int_literal(0))
			}
		}
	}
	call := t.make_call_expr_typed(cmp, [cur_arg, prev_arg], 'int')
	return t.make_infix(.lt, call, t.make_int_literal(0))
}

fn (t &Transformer) array_sort_compare_arg_types(cmp flat.NodeId, elem_type string) (string, string) {
	default_type := '&${elem_type}'
	if isnil(t.tc) || int(cmp) < 0 {
		return default_type, default_type
	}
	resolved_elem := types.unalias_type(t.tc.parse_type(elem_type))
	if resolved_elem !is types.Pointer {
		return default_type, default_type
	}
	cmp_node := t.a.nodes[int(cmp)]
	raw_type := if cmp_node.kind == .ident {
		t.raw_var_type(cmp_node.value)
	} else {
		t.raw_checker_node_type(cmp)
	}
	if raw_type.len == 0 {
		return default_type, default_type
	}
	cmp_type := types.unalias_type(t.tc.parse_type(raw_type))
	if cmp_type is types.FnType && cmp_type.params.len >= 2 {
		first_type := if types.unalias_type(cmp_type.params[0]).name() == resolved_elem.name() {
			elem_type
		} else {
			default_type
		}
		second_type := if types.unalias_type(cmp_type.params[1]).name() == resolved_elem.name() {
			elem_type
		} else {
			default_type
		}
		return first_type, second_type
	}
	return default_type, default_type
}

fn (mut t Transformer) array_sort_lambda_expr(node flat.Node, a_expr flat.NodeId, b_expr flat.NodeId, a_type string, b_type string) ?flat.NodeId {
	if node.kind != .lambda_expr || node.children_count < 3 {
		return none
	}
	first := t.a.child_node(&node, 0)
	second := t.a.child_node(&node, 1)
	if first.kind != .ident || second.kind != .ident || first.value.len == 0
		|| second.value.len == 0 {
		return none
	}
	body_id := t.a.child(&node, node.children_count - 1)
	old_a := t.var_type(first.value)
	old_b := t.var_type(second.value)
	t.set_var_type(first.value, a_type)
	t.set_var_type(second.value, b_type)
	raw_cmp := t.substitute_array_sort_vars_named(body_id, first.value, second.value, a_expr, b_expr)
	cmp := t.transform_expr(raw_cmp)
	if old_a.len > 0 {
		t.set_var_type(first.value, old_a)
	} else {
		t.unset_var_type(first.value)
	}
	if old_b.len > 0 {
		t.set_var_type(second.value, old_b)
	} else {
		t.unset_var_type(second.value)
	}
	return cmp
}

// substitute_array_sort_vars supports substitute array sort vars handling for Transformer.
fn (mut t Transformer) substitute_array_sort_vars(id flat.NodeId, a_expr flat.NodeId, b_expr flat.NodeId) flat.NodeId {
	return t.substitute_array_sort_vars_named(id, 'a', 'b', a_expr, b_expr)
}

fn (mut t Transformer) substitute_array_sort_vars_named(id flat.NodeId, a_name string, b_name string, a_expr flat.NodeId, b_expr flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value == a_name {
			return a_expr
		}
		if node.value == b_name {
			return b_expr
		}
		return id
	}
	if node.children_count == 0 {
		return id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.substitute_array_sort_vars_named(t.a.child(&node, i), a_name, b_name, a_expr, b_expr)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: node.typ
	})
}

// make_index_assign builds make index assign data for transform.
fn (mut t Transformer) make_index_assign(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind: .index_assign
		op: .assign
		children_start: start
		children_count: 2
	})
}
