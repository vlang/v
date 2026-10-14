module types

import v.flat

// Until transform materializes an inferred anonymous declaration, its field
// expressions are still the authority for selectors on the local value.
fn (tc &TypeChecker) inferred_anonymous_struct_field_type(base_id flat.NodeId, name string) ?Type {
	value_id := tc.inferred_anonymous_struct_field_expr(base_id, name, 0) or { return none }
	typ := tc.resolve_type(value_id)
	if typ is Unknown || typ is Void || type_contains_unknown(typ) {
		return none
	}
	return typ
}

fn (tc &TypeChecker) inferred_anonymous_struct_field_expr(base_id flat.NodeId, name string, depth int) ?flat.NodeId {
	if depth > 64 || !tc.valid_node_id(base_id) {
		return none
	}
	node := tc.a.node(base_id)
	match node.kind {
		.ident {
			rhs := tc.inferred_anonymous_local_initializer(base_id) or { return none }
			return tc.inferred_anonymous_struct_field_expr(rhs, name, depth + 1)
		}
		.paren, .prefix {
			if node.children_count == 1 && (node.kind == .paren || node.op == .amp) {
				return tc.inferred_anonymous_struct_field_expr(tc.a.child(node, 0), name, depth + 1)
			}
		}
		.selector {
			if node.children_count == 1 {
				inner := tc.inferred_anonymous_struct_field_expr(tc.a.child(node, 0), node.value, depth + 1) or { return none }
				return tc.inferred_anonymous_struct_field_expr(inner, name, depth + 1)
			}
		}
		.struct_init {
			if node.value != 'struct' { return none }
			for i in 0 .. node.children_count {
				field := tc.a.child_node(node, i)
				if field.kind == .field_init && field.value == name && field.children_count == 1 {
					return tc.a.child(field, 0)
				}
			}
		}
		else {}
	}
	return none
}

fn (tc &TypeChecker) inferred_anonymous_local_initializer(use_id flat.NodeId) ?flat.NodeId {
	use := tc.a.node(use_id)
	mut current := use_id
	mut enclosing := tc.direct_parent_id(current)
	for _ in 0 .. 128 {
		if !tc.valid_node_id(enclosing) { break }
		node := tc.a.node(enclosing)
		if node.kind == .call && node.children_count > 1 && current != tc.a.child(node, 0) {
			dsl_name := tc.unresolved_array_dsl_call_name(*node)
			is_sort := is_array_sort_dsl_call_name(dsl_name)
			if (use.value == 'it' && tc.call_binds_implicit_it(*node))
				|| (use.value in ['a', 'b'] && is_sort && tc.call_receiver_array_type(*node) != none) {
				callee := tc.a.child_node(node, 0)
				return tc.inferred_anonymous_array_element(tc.a.child(callee, 0), 0)
			}
		}
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			for i in 0 .. node.children_count {
				param := tc.a.child_node(node, i)
				if param.kind == .param && param.value == use.value { return none }
			}
			if enclosing == flat.NodeId(tc.fn_context.node_id) { break }
		}
		current = enclosing
		enclosing = tc.direct_parent_id(current)
	}
	if rhs := tc.local_decl_rhs_before(use.value, use_id) {
		decl := tc.direct_parent_id(rhs)
		owner := tc.direct_parent_id(decl)
		mut parent := tc.direct_parent_id(use_id)
		for _ in 0 .. 128 {
			if !tc.valid_node_id(parent) { break }
			if parent == owner { return rhs }
			next := tc.direct_parent_id(parent)
			if next == parent { break }
			parent = next
		}
	}
	// The index is local to one checked function. A captured binding belongs to
	// an outer lexical block, and a nearer sibling declaration is not visible.
	mut parent := tc.direct_parent_id(use_id)
	for _ in 0 .. 128 {
		if !tc.valid_node_id(parent) { break }
		scope := tc.a.node(parent)
		if scope.kind in [.block, .fn_decl, .fn_literal, .lambda_expr] {
			mut best := flat.empty_node
			mut best_offset := -1
			for i in 0 .. scope.children_count {
				decl := tc.a.child_node(scope, i)
				if decl.kind != .decl_assign { continue }
				for j := 0; j + 1 < int(decl.children_count); j += 2 {
					lhs := tc.a.child_node(decl, j)
					if lhs.kind == .ident && lhs.value == use.value && lhs.pos.id == use.pos.id
						&& lhs.pos.offset < use.pos.offset && lhs.pos.offset > best_offset {
						best = tc.a.child(decl, j + 1)
						best_offset = lhs.pos.offset
					}
				}
			}
			if best != flat.empty_node { return best }
		}
		next := tc.direct_parent_id(parent)
		if next == parent { break }
		parent = next
	}
	return none
}

fn (tc &TypeChecker) inferred_anonymous_array_element(id flat.NodeId, depth int) ?flat.NodeId {
	if depth > 64 || !tc.valid_node_id(id) { return none }
	node := tc.a.node(id)
	match node.kind {
		.ident {
			rhs := tc.inferred_anonymous_local_initializer(id) or { return none }
			return tc.inferred_anonymous_array_element(rhs, depth + 1)
		}
		.paren {
			if node.children_count == 1 {
				return tc.inferred_anonymous_array_element(tc.a.child(node, 0), depth + 1)
			}
		}
		.array_literal {
			if node.children_count > 0 { return tc.a.child(node, 0) }
		}
		.call {
			if node.children_count > 0 && tc.call_receiver_array_type(*node) != none {
				callee := tc.a.child_node(node, 0)
				if callee.kind == .selector && callee.value in ['filter', 'sorted', 'clone'] {
					return tc.inferred_anonymous_array_element(tc.a.child(callee, 0), depth + 1)
				}
			}
		}
		else {}
	}
	return none
}
