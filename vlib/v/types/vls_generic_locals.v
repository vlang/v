module types

import v.flat

// The check leaves the body of a generic function to its instances (see
// check_generic_fn_body): it keeps no type for the nodes there, and a question
// about a local had only its written form to go by, which says nothing of
// `x := sep + '!'`. A question in such a body checks it once more, in a fork,
// with its type parameters open, as the check of a constrained body starts:
// what does not depend on them gets the type that every instance gives it, and
// those types answer the questions of the process. What depends on them gets
// the type that the fork gives it, with the type parameters kept, `[]A` for
// `[a, b]`, when that type tells it (see vls_open_type_holds); a call of a
// generic function takes the types that its arguments bind instead (see
// vls_generic_call_type), and the hover writes the rest from the declarations
// (see vls_generic_value_text).

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
	// typed as the one that was checked: its twin, or the one that starts where
	// it starts, as the checked tree may name it otherwise, with its module.
	fn_id = tc.vls_twins[int(fn_id)] or { fn_id }
	if int(fn_id) >= tc.vls_added_start {
		added := tc.a.node(fn_id)
		for idx in tc.a.user_code_start .. tc.vls_added_start {
			node := tc.a.nodes[idx]
			if node.kind == .fn_decl && node.pos.id == added.pos.id
				&& node.pos.offset == added.pos.offset {
				fn_id = flat.NodeId(idx)
				break
			}
		}
	}
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
	w := tc.checked_generic_fn_body(node, int(fn_id), map[string]string{}, true)
	tc.fn_context = saved_context
	tc.cur_fn_ret_type = saved_return
	tc.cur_scope = saved_scope
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	// With the type parameters open, the fork types what depends on them as best
	// it can, `[]void` for `xs.map(it.name.len)` of `xs []T`, and a generic call
	// as the callee declares it, the `T` of `identity` for `identity(a)`, which
	// every node around it takes. A node depends on them when it names one of
	// them, or a value that does, or when a node below it does (see
	// subtree_depends_on_type_params): the nodes come in the order of a walk from
	// the top, and are decided from the last one up.
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
	mut around_generic_call := map[int]bool{}
	for i := order.len - 1; i >= 0; i-- {
		current := order[i]
		n := tc.a.node(current)
		mut depending := (n.kind == .ident && n.value in dependent)
			|| type_text_names_any(n.typ, params)
			|| (n.kind !in [.string_literal, .char_literal, .int_literal, .float_literal]
				&& type_text_names_any(n.value, params))
		mut generic_call := n.kind == .call && tc.vls_calls_generic(current, *n)
		for j in 0 .. n.children_count {
			child := int(tc.a.child(n, j))
			depending = depending || depends[child]
			generic_call = generic_call || around_generic_call[child]
		}
		depends[int(current)] = depending
		around_generic_call[int(current)] = generic_call
		if depending {
			if !generic_call {
				typ := w.expr_type(current) or { w.placeholder_types[int(current)] or { continue } }
				if vls_open_type_holds(typ, params) {
					tc.vls_open_types[int(current)] = typ
				}
			}
			continue
		}
		if typ := w.expr_type(current) {
			if !type_contains_unknown(typ) && typ !is Void {
				tc.vls_body_types[int(current)] = typ
			}
		}
	}
}

// vls_calls_generic reports whether `node`, the call `id`, calls a generic
// function or a method of a generic type: what the check of a generic body with
// its type parameters open gives it can name the callee's own type parameters.
fn (tc &TypeChecker) vls_calls_generic(id flat.NodeId, node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	target := tc.vls_call_target(id, tc.a.child(&node, 0)) or { return false }
	if (tc.fn_generic_params[target] or { []string{} }).len > 0 {
		return true
	}
	_, _, receiver_is_generic := generic_type_application_parts(target.all_before_last('.'))
	return receiver_is_generic
}

// vls_open_type_holds reports whether `typ`, the type that the check of a
// generic body with its type parameters open gave a node, tells what the node
// is: each type parameter it names is one of `params`, the body's own, and
// nothing in it is void or a type the check could not tell.
fn vls_open_type_holds(typ Type, params map[string]bool) bool {
	match typ {
		Unknown {
			name := generic_placeholder_from_unknown(typ) or { return false }
			return name in params
		}
		Void {
			return false
		}
		Array {
			return vls_open_type_holds(typ.elem_type, params)
		}
		ArrayFixed {
			return vls_open_type_holds(typ.elem_type, params)
		}
		Map {
			return vls_open_type_holds(typ.key_type, params)
				&& vls_open_type_holds(typ.value_type, params)
		}
		Pointer {
			return vls_open_type_holds(typ.base_type, params)
		}
		OptionType {
			return vls_open_type_holds(typ.base_type, params)
		}
		ResultType {
			return vls_open_type_holds(typ.base_type, params)
		}
		FnType {
			for param in typ.params {
				if !vls_open_type_holds(param, params) {
					return false
				}
			}
			return typ.return_type is Void || vls_open_type_holds(typ.return_type, params)
		}
		MultiReturn {
			for part in typ.types {
				if !vls_open_type_holds(part, params) {
					return false
				}
			}
			return true
		}
		Struct, Interface, SumType {
			return vls_type_text_params_hold(typ.name, params)
		}
		else {
			return true
		}
	}
}

// vls_type_text_params_hold reports whether every type parameter that the type
// `text` names, `T` in `Box[T]`, is one of `params`.
fn vls_type_text_params_hold(text string, params map[string]bool) bool {
	mut i := 0
	for i < text.len {
		if !(text[i].is_letter() || text[i] == `_`) {
			i++
			continue
		}
		start := i
		for i < text.len && (text[i].is_alnum() || text[i] == `_`) {
			i++
		}
		name := text[start..i]
		if (start == 0 || text[start - 1] != `.`) && is_bare_generic_param(name)
			&& name !in params {
			return false
		}
	}
	return true
}

// vls_generic_call_type is the type of `node`, the call `id` of a generic
// function or of a method of a generic type, in a body the checker did not
// type: what the callee declares it returns, with its type parameters bound
// from the call, as constraint_walk_call_bindings binds them: from its type
// arguments, its receiver and its arguments. none while one stays unbound.
fn (tc &TypeChecker) vls_generic_call_type(id flat.NodeId, node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	callee_id := tc.a.child(&node, 0)
	target := tc.vls_call_target(id, callee_id)?
	fn_params := tc.fn_generic_params[target] or { []string{} }
	mut names := fn_params.clone()
	_, receiver_params, receiver_is_generic := generic_type_application_parts(target.all_before_last('.'))
	if receiver_is_generic {
		for param in receiver_params {
			name := param.trim_space()
			if is_bare_generic_param(name) && name !in names {
				names << name
			}
		}
	}
	if names.len == 0 {
		return none
	}
	callee := tc.a.node(callee_id)
	mut bound := map[string]Type{}
	// `first_of[A](values)`: its type arguments, a type parameter of the body
	// kept as one.
	if callee.kind == .index && tc.call_has_explicit_generic_type_args(node) {
		for k, text in tc.generic_call_type_arg_names(*callee) {
			if k < fn_params.len {
				bound[fn_params[k]] = if is_bare_generic_param(text.trim_space()) {
					unknown_type('generic placeholder `${text.trim_space()}`')
				} else {
					tc.parse_type(text)
				}
			}
		}
	}
	method := if callee.kind == .index && callee.children_count > 0 {
		*tc.a.child_node(callee, 0)
	} else {
		*callee
	}
	param_texts := tc.fn_param_type_texts[target] or { []string{} }
	mut first := 0
	if method.kind == .selector && method.children_count > 0 {
		if receiver := tc.vls_unconstrained_type(tc.a.child(&method, 0)) {
			if param_texts.len > 0 {
				tc.vls_bind_type_params(param_texts[0], unwrap_pointer(receiver), names, mut
					bound)
			}
			first = 1
		}
	}
	for param_idx in first .. param_texts.len {
		arg_idx := param_idx - first + 1
		if arg_idx >= node.children_count {
			break
		}
		arg := tc.vls_unconstrained_type(tc.call_arg_value(tc.a.child(&node, arg_idx))) or {
			continue
		}
		tc.vls_bind_type_params(param_texts[param_idx], arg, names, mut bound)
	}
	declared := tc.fn_ret_types[target] or { return none }
	mut args := []Type{cap: names.len}
	for name in names {
		args << bound[name] or { return none }
	}
	return tc.substitute_generic_type_values(declared, args, names)
}

// vls_bind_type_params binds in `bound` the type parameters `names` that
// `text`, the type of a parameter of a generic declaration, spells, from
// `actual`, the type of what a call passes there: `[]T` against `[]A` binds `T`
// to `A`, `Box[T]` against `Box[A]` too.
fn (tc &TypeChecker) vls_bind_type_params(text string, actual Type, names []string, mut bound map[string]Type) {
	clean := text.trim_space()
	if clean in names {
		if clean !in bound {
			bound[clean] = actual
		}
		return
	}
	for prefix in ['mut ', 'shared ', '&'] {
		if clean.starts_with(prefix) {
			tc.vls_bind_type_params(clean[prefix.len..], unwrap_pointer(actual), names, mut
				bound)
			return
		}
	}
	held := unalias_type(actual)
	if clean.starts_with('...') {
		elem := if held is Array { held.elem_type } else { actual }
		tc.vls_bind_type_params(clean[3..], elem, names, mut bound)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		base := match held {
			OptionType { held.base_type }
			ResultType { held.base_type }
			else { held }
		}
		tc.vls_bind_type_params(clean[1..], base, names, mut bound)
		return
	}
	if clean.starts_with('[]') {
		if held is Array {
			tc.vls_bind_type_params(clean[2..], held.elem_type, names, mut bound)
		}
		return
	}
	if clean.starts_with('[') {
		end := find_matching_bracket(clean, 0)
		if held is ArrayFixed && end > 0 && end + 1 < clean.len {
			tc.vls_bind_type_params(clean[end + 1..], held.elem_type, names, mut bound)
		}
		return
	}
	if clean.starts_with('map[') {
		end := find_matching_bracket(clean, 3)
		if held is Map && end > 0 && end + 1 < clean.len {
			tc.vls_bind_type_params(clean[4..end], held.key_type, names, mut bound)
			tc.vls_bind_type_params(clean[end + 1..], held.value_type, names, mut bound)
		}
		return
	}
	// `Box[T]` against `Box[A]`: each type argument against the actual's.
	base, args, is_generic := generic_type_application_parts(clean)
	if !is_generic {
		return
	}
	actual_base, actual_args, actual_is_generic := generic_type_application_parts(tc.vls_type_name(held))
	if !actual_is_generic || actual_args.len != args.len
		|| actual_base.all_after_last('.') != base.all_after_last('.') {
		return
	}
	for i, arg in args {
		arg_text := actual_args[i].trim_space()
		arg_type := if is_bare_generic_param(arg_text) {
			unknown_type('generic placeholder `${arg_text}`')
		} else {
			tc.parse_type(arg_text)
		}
		tc.vls_bind_type_params(arg, arg_type, names, mut bound)
	}
}

// vls_type_name is the name of `typ` with its type arguments, `Box[A]`, for a
// struct, an interface or a sum type.
fn (tc &TypeChecker) vls_type_name(typ Type) string {
	return match typ {
		Struct { typ.name }
		Interface { typ.name }
		SumType { typ.name }
		else { typ.name() }
	}
}
