module types

import v.flat

// The check leaves the body of a generic function to its instances (see
// check_generic_fn_body): it keeps no type for the nodes there, and a question
// about a local had only its written form to go by, which says nothing of
// `x := sep + '!'`. A question in such a body checks it once more, in a fork,
// with its type parameters open, as the check of a constrained body starts:
// what does not depend on them gets the type that every instance gives it, and
// those types answer the questions of the process. What depends on them is
// still answered from the declarations (see vls_generic_value_text).

// vls_type_generic_body types, once, the body of the generic function around
// the node `id`.
fn (mut tc TypeChecker) vls_type_generic_body(id flat.NodeId) {
	mut fn_id := id
	for _ in 0 .. 4096 {
		if !tc.valid_node_id(fn_id) || tc.a.node(fn_id).kind == .fn_decl {
			break
		}
		fn_id = tc.vls_parent_id(fn_id)
	}
	// A function that the nodes added for a left-out branch parse again is
	// typed as the one that was checked.
	fn_id = tc.vls_twins[int(fn_id)] or { fn_id }
	if !tc.valid_node_id(fn_id) || int(fn_id) >= tc.vls_added_start
		|| tc.vls_typed_bodies[int(fn_id)] {
		return
	}
	tc.vls_typed_bodies[int(fn_id)] = true
	node := *tc.a.node(fn_id)
	generic_params := tc.infer_decl_generic_param_names(node)
	// A declaration of a `.vh` file has no body.
	if node.kind != .fn_decl || generic_params.len == 0 || node.is_mut {
		return
	}
	// The context that the check of a function starts from (see
	// check_fn_decl_semantics).
	saved_context := tc.fn_context
	saved_return := tc.cur_fn_ret_type
	saved_scope := tc.cur_scope
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	tc.fn_context = new_function_check_context()
	tc.fn_context.generic_params = generic_params
	tc.vls_enter_file(int(node.pos.id))
	tc.cur_scope = tc.file_scope
	return_text := if node.typ.ends_with('?') && !node.typ.starts_with('?') {
		node.typ.trim_right('?')
	} else {
		node.typ
	}
	tc.cur_fn_ret_type = tc.parse_type(return_text)
	tc.fn_context.return_type = tc.cur_fn_ret_type
	w := tc.checked_generic_fn_body(node, int(fn_id), map[string]string{})
	tc.fn_context = saved_context
	tc.cur_fn_ret_type = saved_return
	tc.cur_scope = saved_scope
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	// Only what depends on no type parameter: with them open, the fork types what
	// does as best it can, `[]void` for `xs.map(it.name.len)` of `xs []T`. A node
	// depends on them when it names one of them, or a value that does, or when a
	// node below it does (see subtree_depends_on_type_params): the nodes come in
	// the order of a walk from the top, and are decided from the last one up.
	mut params := map[string]bool{}
	for param in generic_params {
		params[param] = true
	}
	dependent := tc.type_param_dependent_names(node, params)
	mut order := []flat.NodeId{}
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		stack << tc.a.child(&node, i)
	}
	for stack.len > 0 {
		current := stack.pop()
		if !tc.valid_node_id(current) {
			continue
		}
		order << current
		n := tc.a.node(current)
		for i in 0 .. n.children_count {
			stack << tc.a.child(n, i)
		}
	}
	mut depends := map[int]bool{}
	for i := order.len - 1; i >= 0; i-- {
		current := order[i]
		n := tc.a.node(current)
		mut depending := (n.kind == .ident && n.value in dependent)
			|| type_text_names_any(n.typ, params)
			|| (n.kind !in [.string_literal, .char_literal, .int_literal, .float_literal]
				&& type_text_names_any(n.value, params))
		for j in 0 .. n.children_count {
			if depending {
				break
			}
			depending = depends[int(tc.a.child(n, j))]
		}
		depends[int(current)] = depending
		if depending {
			continue
		}
		if typ := w.expr_type(current) {
			if !type_contains_unknown(typ) && typ !is Void {
				tc.vls_body_types[int(current)] = typ
			}
		}
	}
}
