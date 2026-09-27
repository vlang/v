module types

import v.flat

// A check (`-check`) checks the body of a generic function as the body of any
// other function: what does not depend on its type parameters is reported as
// it is in a function without them, `asd` alone on a line, `takes_int('s')`,
// `return 5` in a function that returns a string. What does depend on them is
// left to the checks against their constraints (checker_generic_constraints.v)
// and to the instances of the function: with its type parameters open the
// checker does not type it, and what it would say there would not be about the
// program.

// check_generic_fn_body checks the body of the generic function `node`, whose
// type parameters are `params`, and keeps the diagnostics of the statements
// that do not depend on them. The body is checked by a fork of the checker, as
// a worker of the parallel check with no range of its own: all it learns stays
// in its private caches. With the type parameters open, `xs.map(it.name.len)`
// of `xs []T` is a `[]void` to it, where the questions about the body infer a
// `[]int`.
fn (mut tc TypeChecker) check_generic_fn_body(node flat.Node, fn_idx int, params map[string]bool) {
	mut w := tc.fork_for_parallel_check()
	w.fn_context.generic_params = tc.fn_context.generic_params.clone()
	w.fn_context.return_type = tc.fn_context.return_type
	w.fn_context.node_id = fn_idx
	w.fn_context.concrete_generic_receiver_specialization =
		tc.fn_context.concrete_generic_receiver_specialization
	w.cur_fn_ret_type = tc.cur_fn_ret_type
	w.cur_fn_node_id = fn_idx
	w.index_local_decl_rhs(flat.NodeId(fn_idx))
	$if ownership ? {
		w.ownership_begin_fn(node)
	}
	w.push_scope()
	for i in 0 .. node.children_count {
		param_id := tc.a.child(&node, i)
		w.insert_fn_param_binding(param_id, tc.a.node(param_id))
	}
	w.insert_implicit_veb_ctx(node)
	w.check_fn_body(node)
	w.pop_scope()
	$if ownership ? {
		w.ownership_end_fn()
	}
	if w.errors.len == 0 && w.notices.len == 0 {
		return
	}
	dependent := tc.type_param_dependent_names(node, params)
	mut statements := map[int]bool{}
	for err in w.errors {
		if !tc.diagnostic_depends_on_type_params(err.node, fn_idx, dependent, params, mut
			statements)
		{
			tc.errors << err
		}
	}
	for notice in w.notices {
		if !tc.diagnostic_depends_on_type_params(notice.node, fn_idx, dependent, params, mut
			statements)
		{
			tc.notices << notice
		}
	}
}

// type_param_dependent_names returns the names whose values depend on the type
// parameters `params` of the generic function `fn_node`: the parameters
// themselves, the parameters of the function whose types name one, and the
// locals declared from a value that depends on them, as `ys := xs.filter(..)`
// for `xs []T`. V has no shadowing, so a name stands for one binding in the
// whole body.
fn (tc &TypeChecker) type_param_dependent_names(fn_node flat.Node, params map[string]bool) map[string]bool {
	mut names := params.clone()
	mut roots := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		child := tc.a.node(child_id)
		if child.kind == .param {
			if child.value.len > 0 && type_text_names_any(child.typ, params) {
				names[child.value] = true
			}
		} else {
			roots << child_id
		}
	}
	// A declaration can take its value from one that comes later in the walk,
	// in a loop: the walk goes on while it finds new names.
	for _ in 0 .. 64 {
		mut changed := false
		mut stack := roots.clone()
		for stack.len > 0 {
			id := stack.pop()
			if !tc.valid_node_id(id) {
				continue
			}
			node := tc.a.node(id)
			match node.kind {
				.decl_assign {
					if tc.any_child_depends(node, 0, names, params) {
						changed = tc.add_declared_names(node, 0, node.children_count, mut
							names) || changed
					}
				}
				.for_in_stmt {
					// `for k, v in xs {`: the names of the loop, then what it walks.
					if node.children_count > 2 && tc.any_child_depends(node, 2, names, params) {
						changed = tc.add_declared_names(node, 0, 2, mut names) || changed
					}
				}
				.fn_literal, .lambda_expr {
					for i in 0 .. node.children_count {
						param := tc.a.child_node(node, i)
						if param.kind == .param && param.value.len > 0 && param.value !in names
							&& type_text_names_any(param.typ, params) {
							names[param.value] = true
							changed = true
						}
					}
				}
				else {}
			}
			for i in 0 .. node.children_count {
				stack << tc.a.child(node, i)
			}
		}
		if !changed {
			break
		}
	}
	return names
}

// any_child_depends reports whether a child of `node` from `first` on, but a
// block, depends on `names` or `params` (see subtree_depends_on_type_params).
fn (tc &TypeChecker) any_child_depends(node &flat.Node, first int, names map[string]bool, params map[string]bool) bool {
	for i in first .. node.children_count {
		child_id := tc.a.child(node, i)
		if tc.valid_node_id(child_id) && tc.a.node(child_id).kind != .block
			&& tc.subtree_depends_on_type_params(child_id, names, params) {
			return true
		}
	}
	return false
}

// add_declared_names adds the names the children of `node` from `first` to
// `end` declare, and reports whether one was new.
fn (tc &TypeChecker) add_declared_names(node &flat.Node, first int, end int, mut names map[string]bool) bool {
	mut added := false
	for i in first .. int_min(end, node.children_count) {
		child := tc.a.child_node(node, i)
		if child.kind == .ident && child.value.len > 0 && child.value != '_'
			&& child.value !in names {
			names[child.value] = true
			added = true
		}
	}
	return added
}

// diagnostic_depends_on_type_params reports whether the diagnostic at `id`, in
// the body of the function `fn_idx`, is about a statement that depends on its
// type parameters: one that names them, or a name in `names`. A diagnostic
// outside a statement of the body depends on them too: it is not said.
fn (tc &TypeChecker) diagnostic_depends_on_type_params(id flat.NodeId, fn_idx int, names map[string]bool, params map[string]bool, mut statements map[int]bool) bool {
	statement := tc.enclosing_body_statement(id, fn_idx) or { return true }
	if depends := statements[int(statement)] {
		return depends
	}
	depends := tc.subtree_depends_on_type_params(statement, names, params)
	statements[int(statement)] = depends
	return depends
}

// enclosing_body_statement returns the statement of the body of the function
// `fn_idx`, or of a block in it, that holds `id`.
fn (tc &TypeChecker) enclosing_body_statement(id flat.NodeId, fn_idx int) ?flat.NodeId {
	mut current := id
	for _ in 0 .. 4096 {
		if !tc.valid_node_id(current) || int(current) == fn_idx {
			return none
		}
		parent := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent) {
			return none
		}
		if int(parent) == fn_idx {
			// A parameter is not a statement of the body.
			if tc.a.node(current).kind == .param {
				return none
			}
			return current
		}
		if tc.a.node(parent).kind == .block {
			return current
		}
		current = parent
	}
	return none
}

// subtree_depends_on_type_params reports whether the node `id`, or a node below
// it, names a type parameter of `params` or a name of `names`: an identifier,
// or the text of a type, `T{}`, `[]T{}`, `x as T`, `$if T is f64 {`.
fn (tc &TypeChecker) subtree_depends_on_type_params(id flat.NodeId, names map[string]bool, params map[string]bool) bool {
	mut stack := [id]
	for stack.len > 0 {
		current := stack.pop()
		if !tc.valid_node_id(current) {
			continue
		}
		node := tc.a.node(current)
		if node.kind == .ident && node.value in names {
			return true
		}
		if type_text_names_any(node.typ, params) {
			return true
		}
		if node.kind !in [.string_literal, .char_literal, .int_literal, .float_literal]
			&& type_text_names_any(node.value, params) {
			return true
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return false
}

// type_text_names_any reports whether `text` has a name of `names` among the
// names it is written with: `T`, `[]T`, `map[string]T`, `Box[T]`, `fn (T) bool`.
fn type_text_names_any(text string, names map[string]bool) bool {
	mut start := -1
	for i := 0; i <= text.len; i++ {
		is_name_byte := i < text.len && (text[i].is_letter() || text[i].is_digit() || text[i] == `_`)
		if is_name_byte {
			if start < 0 {
				start = i
			}
			continue
		}
		if start >= 0 {
			if text[start..i] in names {
				return true
			}
			start = -1
		}
	}
	return false
}
