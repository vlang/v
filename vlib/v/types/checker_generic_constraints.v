module types

import v.flat
import v.token

// A type parameter can name the interface it must satisfy, its constraint:
// `fn longest[T Named](a T, b T) T`. A call is checked against the interface,
// where V checks a value passed to an interface parameter, and the body can
// use on a value of type `T` only the fields and methods the interface
// declares. The parser keeps the constraints next to the generic params (see
// flat.Node.generic_constraints).

// generic_constraint_type resolves the constraint `text` of a type parameter
// as written where `decl` declares it: in that file and that module.
fn (tc &TypeChecker) generic_constraint_type(decl flat.Node, text string) Type {
	file := tc.a.source_files[int(decl.pos.id)] or { return tc.parse_type(text) }
	decl_module := tc.file_modules[file.name] or { tc.cur_module }
	mut scoped := tc.fork_type_parse_view(file.name, decl_module)
	return scoped.parse_resolution_type(text)
}

// generic_constraint_interface is the interface that the constraint `text` of a
// type parameter of `decl` names, or none when it names no interface.
fn (tc &TypeChecker) generic_constraint_interface(decl flat.Node, text string) ?Interface {
	typ := tc.generic_constraint_type(decl, text)
	if typ is Interface {
		return typ
	}
	return none
}

// generic_constraint_interfaces maps each type parameter of the function `decl`
// that names an interface as its constraint to that interface: its own type
// parameters, and those of a generic receiver, `fn (b Box[T])`, which the
// struct `Box[T Named]` constrains.
fn (tc &TypeChecker) generic_constraint_interfaces(decl flat.Node) map[string]Interface {
	mut interfaces := map[string]Interface{}
	names := decl.generic_params()
	constraints := decl.generic_constraints()
	if constraints.len == names.len {
		for i, name in names {
			if constraints[i].len == 0 {
				continue
			}
			if iface := tc.generic_constraint_interface(decl, constraints[i]) {
				interfaces[name] = iface
			}
		}
	}
	if decl.children_count == 0 || !decl.value.contains('.') {
		return interfaces
	}
	receiver := tc.a.child_node(&decl, 0)
	if receiver.kind != .param {
		return interfaces
	}
	receiver_type := receiver.typ.trim_left('&').all_after('mut ').trim_space()
	if !receiver_type.contains('[') || !receiver_type.ends_with(']') {
		return interfaces
	}
	struct_decl := tc.generic_struct_decl(receiver_type.all_before('[')) or { return interfaces }
	struct_constraints := struct_decl.generic_constraints()
	struct_names := struct_decl.generic_params()
	receiver_names := split_params(receiver_type.all_after('[').all_before_last(']'))
	if struct_constraints.len != struct_names.len || receiver_names.len != struct_names.len {
		return interfaces
	}
	for i, name in receiver_names {
		if struct_constraints[i].len == 0 || name.trim_space() in interfaces {
			continue
		}
		if iface := tc.generic_constraint_interface(struct_decl, struct_constraints[i]) {
			interfaces[name.trim_space()] = iface
		}
	}
	return interfaces
}

// generic_struct_decl is the declaration of the generic struct `name`.
fn (tc &TypeChecker) generic_struct_decl(name string) ?flat.Node {
	for candidate in [tc.qualify_decl_name(name), name] {
		if idx := tc.first_type_declaration_ids[candidate] {
			node := tc.a.nodes[idx]
			if node.kind == .struct_decl {
				return node
			}
		}
	}
	return none
}

// check_generic_constraint_decls reports a constraint that names no interface,
// as `[T User]`, where it is written.
fn (mut tc TypeChecker) check_generic_constraint_decls(node_id flat.NodeId, node flat.Node) {
	names := node.generic_params()
	constraints := node.generic_constraints()
	if constraints.len != names.len {
		return
	}
	for i, text in constraints {
		if text.len == 0 {
			continue
		}
		typ := tc.generic_constraint_type(node, text)
		if typ is Interface {
			continue
		}
		pos := tc.generic_constraint_pos(node, names[i], text)
		if typ is Unknown {
			tc.record_error_at(.unknown_type, 'unknown type `${text}`', node_id, pos)
			continue
		}
		tc.record_error_at(.unknown_type, 'the constraint of `${names[i]}` must be an interface or a constraint, not `${text}`',
			node_id, pos)
	}
}

// generic_constraint_pos is where the constraint `text` of the type parameter
// `name` of `decl` is written, or the position of `decl` when it cannot be
// found there.
fn (tc &TypeChecker) generic_constraint_pos(decl flat.Node, name string, text string) token.Pos {
	source := tc.vls_source(int(decl.pos.id))
	start := int(decl.pos.offset)
	if start < 0 || start >= source.len {
		return decl.pos
	}
	open := source.index_after('[', start) or { return decl.pos }
	close := source.index_after(']', open) or { return decl.pos }
	mut at := open + 1
	for at < close {
		found := source.index_after(name, at) or { break }
		if found >= close {
			break
		}
		mut after := found + name.len
		for after < source.len && (source[after] == ` ` || source[after] == `\t`) {
			after++
		}
		if after > found + name.len && vls_holds_at(source, after, text) {
			return token.new_span(decl.pos.id, after, after + text.len)
		}
		at = found + name.len
	}
	return decl.pos
}

// check_generic_call_constraints reports a type argument of a generic call that
// does not implement the interface its type parameter names as its
// constraint, at the argument that decided it, as V reports a value passed to
// an interface parameter.
fn (mut tc TypeChecker) check_generic_call_constraints(call_id flat.NodeId, call flat.Node, info CallInfo) {
	instantiation := tc.generic_compile_error_instantiation(call, info) or { return }
	decl := tc.a.node(instantiation.decl_id)
	names := decl.generic_params()
	constraints := decl.generic_constraints()
	if constraints.len != names.len {
		return
	}
	for i, name in names {
		if constraints[i].len == 0 {
			continue
		}
		iface := tc.generic_constraint_interface(decl, constraints[i]) or { continue }
		k := instantiation.generic_params.index(name)
		if k < 0 || k >= instantiation.concrete_args.len {
			continue
		}
		actual := tc.parse_type(instantiation.concrete_args[k])
		if tc.type_implements_interface(actual, iface) {
			continue
		}
		site_id, site_pos := tc.generic_param_binding_site(call_id, call, info, decl, name, k)
		tc.record_interface_implementation_error(.call_arg_mismatch, actual, iface, site_id,
			site_pos)
	}
}

// generic_param_binding_site is where a call decides the type parameter `name`
// of `decl`: its type argument, `f[int](...)`, or the first argument passed to
// a parameter of that type; the call itself otherwise.
fn (tc &TypeChecker) generic_param_binding_site(call_id flat.NodeId, call flat.Node, info CallInfo, decl flat.Node, name string, k int) (flat.NodeId, token.Pos) {
	callee := tc.a.child_node(&call, 0)
	if callee.kind == .index && k + 1 < callee.children_count {
		type_arg := tc.a.child(&callee, k + 1)
		return type_arg, tc.a.node(type_arg).pos
	}
	mut arg_index := 1 + info.arg_offset
	mut source_param_index := 0
	for i in 0 .. decl.children_count {
		param := tc.a.child_node(&decl, i)
		if param.kind != .param {
			continue
		}
		if info.has_receiver && source_param_index == 0 {
			source_param_index++
			continue
		}
		if arg_index >= call.children_count {
			break
		}
		arg_id := tc.call_arg_value(tc.a.child(&call, arg_index))
		if param.typ.trim_left('&').all_after('mut ').trim_space() == name {
			return arg_id, tc.call_argument_diagnostic_pos(arg_id)
		}
		arg_index++
		source_param_index++
	}
	return call_id, call.pos
}

// check_generic_struct_constraints reports a type argument of a generic struct,
// `Box[int]{...}`, that does not implement the interface its type parameter
// names as its constraint.
fn (mut tc TypeChecker) check_generic_struct_constraints(id flat.NodeId, node flat.Node, base string, params []string, args []string) {
	decl := tc.generic_struct_decl(base) or { return }
	constraints := decl.generic_constraints()
	if constraints.len != params.len || args.len != params.len {
		return
	}
	for i, text in constraints {
		if text.len == 0 {
			continue
		}
		iface := tc.generic_constraint_interface(decl, text) or { continue }
		actual := tc.parse_type(args[i])
		if tc.type_implements_interface(actual, iface) {
			continue
		}
		tc.record_interface_implementation_error(.call_arg_mismatch, actual, iface, id, node.pos)
	}
}

struct ConstraintWalkItem {
	id       flat.NodeId
	narrowed []string // names whose type an `is` or a `match` decided here
}

// check_generic_fn_constraint_members reports, in the body of a generic
// function, a field or a method used on a value of a constrained type parameter
// that its constraint does not declare. The body is not checked otherwise
// while its type parameters are open (see check_fn_decl_semantics), and this
// check runs for bodies only, whether a call instantiates them or not.
fn (mut tc TypeChecker) check_generic_fn_constraint_members(fn_node flat.Node) {
	interfaces := tc.generic_constraint_interfaces(fn_node)
	if interfaces.len == 0 {
		return
	}
	mut stack := []ConstraintWalkItem{}
	for i := fn_node.children_count - 1; i >= 0; i-- {
		child := tc.a.child(&fn_node, i)
		if tc.valid_node_id(child) && tc.a.node(child).kind != .param {
			stack << ConstraintWalkItem{
				id: child
			}
		}
	}
	mut callees := map[int]bool{}
	for stack.len > 0 {
		item := stack.pop()
		if !tc.valid_node_id(item.id) {
			continue
		}
		node := tc.a.node(item.id)
		match node.kind {
			// Compile-time branches and closures decide their own types.
			.comptime_if, .comptime_for, .fn_literal, .lambda_expr {
				continue
			}
			.call {
				if node.children_count > 0 {
					callee_id := tc.a.child(node, 0)
					callee := tc.a.node(callee_id)
					if callee.kind == .selector {
						callees[int(callee_id)] = true
						tc.check_constraint_member(item.id, *node, callee_id, callee, true,
							interfaces, item.narrowed)
					}
				}
			}
			.selector {
				if !callees[int(item.id)] {
					tc.check_constraint_member(item.id, *node, item.id, *node, false, interfaces,
						item.narrowed)
				}
			}
			else {}
		}
		narrowed := tc.constraint_walk_narrowed(*node, item.narrowed)
		for i := node.children_count - 1; i >= 0; i-- {
			stack << ConstraintWalkItem{
				id:       tc.a.child(node, i)
				narrowed: narrowed
			}
		}
	}
}

// constraint_walk_narrowed adds to `narrowed` the name that `node`, an `if x is
// Y` or a `match x`, decides the type of in its branches.
fn (tc &TypeChecker) constraint_walk_narrowed(node flat.Node, narrowed []string) []string {
	if node.children_count == 0 || node.kind !in [.if_expr, .match_expr] {
		return narrowed
	}
	subject := tc.a.child_node(&node, 0)
	mut name := ''
	if node.kind == .match_expr && subject.kind == .ident {
		name = subject.value
	} else if subject.kind == .is_expr && subject.children_count > 0 {
		left := tc.a.child_node(subject, 0)
		if left.kind == .ident {
			name = left.value
		}
	}
	if name.len == 0 || name in narrowed {
		return narrowed
	}
	mut result := narrowed.clone()
	result << name
	return result
}

// check_constraint_member checks the member that the selector `sel` names, a
// field, or with `is_call` a method, when its base is a value of a type
// parameter that `interfaces` constrains.
fn (mut tc TypeChecker) check_constraint_member(node_id flat.NodeId, node flat.Node, sel_id flat.NodeId, sel flat.Node, is_call bool, interfaces map[string]Interface, narrowed []string) {
	if sel.children_count == 0 || sel.value.len == 0 || sel.value.starts_with('$') {
		return
	}
	base_id := tc.a.child(&sel, 0)
	base := tc.a.node(base_id)
	// `T.name` is about the type parameter itself; `x` after `x is User` is a User.
	if base.kind == .ident && (base.value in interfaces || base.value in narrowed) {
		return
	}
	base_type := unwrap_pointer(tc.resolve_type(base_id))
	if base_type !is Unknown {
		return
	}
	param := generic_placeholder_from_unknown(base_type as Unknown) or { return }
	iface := interfaces[param] or { return }
	iface_name := tc.interface_metadata_name(iface.name)
	display := iface_name.all_after_last('.')
	member := sel.value
	if member in tc.interface_abstract_method_names(iface_name) {
		return
	}
	if !is_call && tc.interface_field_list(iface_name).any(it.name == member) {
		return
	}
	if is_call {
		tc.record_error_at(.unknown_fn, 'type `${param}` has no method `${member}`: its constraint `${display}` does not declare it',
			node_id, tc.method_call_name_pos(node, sel))
	} else {
		tc.record_error_at(.unknown_field, 'type `${param}` has no field named `${member}`: its constraint `${display}` does not declare it',
			sel_id, tc.node_value_diagnostic_pos(sel_id))
	}
}
