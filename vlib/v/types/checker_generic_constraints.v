module types

import v.flat
import v.token

// A type parameter can name what it must satisfy, its constraint: an interface,
// `fn longest[T Named](a T, b T) T`, or a set of types declared with
// `constraint Number = int | i64 | f64`. A call is checked against it, where V
// checks a value passed to an interface parameter, and the body can use on a
// value of type `T` only what the constraint provides: what the interface
// declares, or what every type of the set has. The parser keeps the constraints
// next to the generic params (see flat.Node.generic_constraints).

// GenericConstraint is what the constraint of a type parameter names.
struct GenericConstraint {
	name         string // as written, for the messages
	is_interface bool
	iface        Interface
	types        []Type // the types of a set, when it names no interface
}

// generic_constraint_type resolves the constraint `text` of a type parameter
// as a type, as written where `decl` declares it: in that file and that module.
fn (tc &TypeChecker) generic_constraint_type(decl flat.Node, text string) Type {
	file := tc.a.source_files[int(decl.pos.id)] or { return tc.parse_type(text) }
	decl_module := tc.file_modules[file.name] or { tc.cur_module }
	mut scoped := tc.fork_type_parse_view(file.name, decl_module)
	return scoped.parse_resolution_type(text)
}

// generic_constraint_set is the `constraint` declaration that the constraint
// `text` of a type parameter of `decl` names, as written there.
fn (tc &TypeChecker) generic_constraint_set(decl flat.Node, text string) ?flat.NodeId {
	file := tc.a.source_files[int(decl.pos.id)] or { return none }
	decl_module := tc.file_modules[file.name] or { tc.cur_module }
	mut scoped := tc.fork_type_parse_view(file.name, decl_module)
	return tc.constraint_sets[scoped.qualify_decl_name(text)] or { return none }
}

// generic_constraint resolves the constraint `text` of a type parameter of
// `decl`: an interface, or a set of types; none when it names neither.
fn (tc &TypeChecker) generic_constraint(decl flat.Node, text string) ?GenericConstraint {
	if set_id := tc.generic_constraint_set(decl, text) {
		return GenericConstraint{
			name:  text
			types: tc.constraint_set_types(set_id)
		}
	}
	typ := tc.generic_constraint_type(decl, text)
	if typ is Interface {
		return GenericConstraint{
			name:         text
			is_interface: true
			iface:        typ
		}
	}
	return none
}

// constraint_set_types resolves the types of the `constraint` declaration
// `set_id` where it is declared.
fn (tc &TypeChecker) constraint_set_types(set_id flat.NodeId) []Type {
	set := tc.a.node(set_id)
	mut types := []Type{cap: set.children_count}
	file := tc.a.source_files[int(set.pos.id)] or { return types }
	set_module := tc.file_modules[file.name] or { tc.cur_module }
	mut scoped := tc.fork_type_parse_view(file.name, set_module)
	for i in 0 .. set.children_count {
		types << scoped.parse_resolution_type(tc.a.child_node(set, i).value)
	}
	return types
}

// generic_constraint_accepts reports whether `actual` satisfies `constraint`.
fn (tc &TypeChecker) generic_constraint_accepts(constraint GenericConstraint, actual Type) bool {
	if constraint.is_interface {
		return tc.type_implements_interface(actual, constraint.iface)
	}
	if actual is Unknown {
		return true
	}
	return constraint.types.any(it.name() == actual.name())
}

// record_generic_constraint_error reports that `actual`, the type argument of
// the type parameter `param`, does not satisfy its constraint.
fn (mut tc TypeChecker) record_generic_constraint_error(constraint GenericConstraint, param string, actual Type, id flat.NodeId, pos token.Pos) {
	if constraint.is_interface {
		tc.record_interface_implementation_error(.call_arg_mismatch, actual, constraint.iface,
			id, pos)
		return
	}
	tc.record_error_at(.call_arg_mismatch, 'cannot use `${actual.name().all_after_last('.')}` as `${param}`: it is not in its constraint `${constraint.name}`',
		id, pos)
}

// generic_constraints_of maps each type parameter of the function `decl` that
// names a constraint to it: its own type parameters, and those of a generic
// receiver, `fn (b Box[T])`, which the struct `Box[T Named]` constrains.
fn (tc &TypeChecker) generic_constraints_of(decl flat.Node) map[string]GenericConstraint {
	mut interfaces := map[string]GenericConstraint{}
	names := decl.generic_params()
	constraints := decl.generic_constraints()
	if constraints.len == names.len {
		for i, name in names {
			if constraints[i].len == 0 {
				continue
			}
			if constraint := tc.generic_constraint(decl, constraints[i]) {
				interfaces[name] = constraint
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
		if constraint := tc.generic_constraint(struct_decl, struct_constraints[i]) {
			interfaces[name.trim_space()] = constraint
		}
	}
	return interfaces
}

// generic_struct_decl is the declaration of the generic struct `name`.
fn (tc &TypeChecker) generic_struct_decl(name string) ?flat.Node {
	for candidate in [tc.qualify_decl_name(name), name, name.trim_string_left('main.')] {
		if idx := tc.first_type_declaration_ids[candidate] {
			node := tc.a.nodes[idx]
			if node.kind == .struct_decl {
				return node
			}
		}
	}
	return none
}

// check_generic_constraint_decls reports a constraint that names neither an
// interface nor a set of types, as `[T User]`, where it is written.
fn (mut tc TypeChecker) check_generic_constraint_decls(node_id flat.NodeId, node flat.Node) {
	names := node.generic_params()
	constraints := node.generic_constraints()
	if constraints.len != names.len {
		return
	}
	for i, text in constraints {
		if text.len == 0 || tc.generic_constraint_set(node, text) != none {
			continue
		}
		typ := tc.generic_constraint_type(node, text)
		if typ is Interface {
			continue
		}
		pos := tc.generic_constraint_pos(node, names[i], text)
		file := tc.a.source_files[int(node.pos.id)] or { continue }
		if !tc.type_name_known_in_scope(text.all_before('['), file.name, tc.file_modules[file.name] or {
			tc.cur_module
		})
		{
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
		constraint := tc.generic_constraint(decl, constraints[i]) or { continue }
		k := instantiation.generic_params.index(name)
		if k < 0 || k >= instantiation.concrete_args.len {
			continue
		}
		actual := tc.parse_type(instantiation.concrete_args[k])
		if tc.generic_constraint_accepts(constraint, actual) {
			continue
		}
		site_id, site_pos := tc.generic_param_binding_site(call_id, call, info, decl, name, k)
		tc.record_generic_constraint_error(constraint, name, actual, site_id, site_pos)
	}
}

// generic_param_binding_site is where a call decides the type parameter `name`
// of `decl`: its type argument, `f[int](...)`, or the first argument passed to
// a parameter of that type; the call itself otherwise.
fn (tc &TypeChecker) generic_param_binding_site(call_id flat.NodeId, call flat.Node, info CallInfo, decl flat.Node, name string, k int) (flat.NodeId, token.Pos) {
	callee := tc.a.child_node(&call, 0)
	if callee.kind == .index && k + 1 < callee.children_count {
		// Reported on the call: a type argument has no position of its own.
		return call_id, tc.explicit_type_arg_pos(call, k) or { call.pos }
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

// explicit_type_arg_pos is where the `k`th type argument of the explicit
// generic call `call` is written, `int` in `f[int](...)`.
fn (tc &TypeChecker) explicit_type_arg_pos(call flat.Node, k int) ?token.Pos {
	source := tc.vls_source(int(call.pos.id))
	start := int(call.pos.offset)
	if start < 0 || start >= source.len {
		return none
	}
	open := source.index_after('[', start) or { return none }
	mut depth := 0
	mut arg := 0
	mut arg_start := open + 1
	for i := open + 1; i < source.len; i++ {
		c := source[i]
		if c == `[` {
			depth++
			continue
		}
		at_end := c == `]` && depth == 0
		if c == `]` && depth > 0 {
			depth--
			continue
		}
		if at_end || (c == `,` && depth == 0) {
			if arg == k {
				mut first := arg_start
				for first < i && (source[first] == ` ` || source[first] == `\t`) {
					first++
				}
				last := vls_blanks_start(source, i)
				return token.new_span(call.pos.id, first, last)
			}
			if at_end {
				return none
			}
			arg++
			arg_start = i + 1
		}
	}
	return none
}

// check_generic_struct_constraints reports a type argument of a generic struct,
// `Box[int]`, that does not satisfy the constraint of its type parameter:
// written in a struct literal, inferred from one, or written in a
// declaration. `pos` is where the type is written.
fn (mut tc TypeChecker) check_generic_struct_constraints(id flat.NodeId, pos token.Pos, base string, args []string) {
	decl := tc.generic_struct_decl(base) or { return }
	params := decl.generic_params()
	constraints := decl.generic_constraints()
	if constraints.len != params.len || args.len != params.len {
		return
	}
	for i, text in constraints {
		if text.len == 0 {
			continue
		}
		constraint := tc.generic_constraint(decl, text) or { continue }
		actual := tc.parse_type(args[i])
		if tc.generic_constraint_accepts(constraint, actual) {
			continue
		}
		tc.record_generic_constraint_error(constraint, params[i], actual, id, pos)
	}
}

// check_generic_fn_value_constraints reports a type argument of a generic
// function taken as a value, `f := longest[int]`, that does not satisfy the
// constraint of its type parameter.
fn (mut tc TypeChecker) check_generic_fn_value_constraints(id flat.NodeId, node flat.Node) {
	name := tc.generic_call_base_name(tc.a.child_node(&node, 0)) or { return }
	type_args := tc.generic_call_type_arg_names(node)
	decl_module := tc.fn_type_modules[name] or { tc.cur_module }
	found := tc.visible_mutation_fn_decl(name, decl_module) or { return }
	decl := tc.a.node(flat.NodeId(found.idx))
	params := decl.generic_params()
	constraints := decl.generic_constraints()
	if constraints.len != params.len || type_args.len != params.len {
		return
	}
	for i, text in constraints {
		if text.len == 0 {
			continue
		}
		constraint := tc.generic_constraint(decl, text) or { continue }
		actual := tc.parse_type(tc.explicit_generic_concrete_arg_text(type_args[i]))
		if tc.generic_constraint_accepts(constraint, actual) {
			continue
		}
		tc.record_generic_constraint_error(constraint, params[i], actual, id, tc.explicit_type_arg_pos(node,
			i) or { node.pos })
	}
}

// check_constraint_decl reports a type of a `constraint` declaration that does
// not exist.
fn (mut tc TypeChecker) check_constraint_decl(node_id flat.NodeId, node flat.Node) {
	file := tc.a.source_files[int(node.pos.id)] or { return }
	decl_module := tc.file_modules[file.name] or { tc.cur_module }
	for i in 0 .. node.children_count {
		type_id := tc.a.child(&node, i)
		type_node := tc.a.node(type_id)
		if !tc.type_name_known_in_scope(type_node.value, file.name, decl_module) {
			tc.record_error_at(.unknown_type, 'unknown type `${type_node.value}`', type_id,
				type_node.pos)
		}
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
	interfaces := tc.generic_constraints_of(fn_node)
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
fn (mut tc TypeChecker) check_constraint_member(node_id flat.NodeId, node flat.Node, sel_id flat.NodeId, sel flat.Node, is_call bool, interfaces map[string]GenericConstraint, narrowed []string) {
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
	constraint := interfaces[param] or { return }
	member := sel.value
	if !constraint.is_interface {
		// A set of types: every one of them must have the member.
		for typ in constraint.types {
			if tc.type_has_member(typ, member, is_call) {
				continue
			}
			what := if is_call { 'method `${member}`' } else { 'field named `${member}`' }
			message := 'type `${param}` has no ${what}: `${typ.name().all_after_last('.')}`, in its constraint `${constraint.name}`, does not have it'
			if is_call {
				tc.record_error_at(.unknown_fn, message, node_id, tc.method_call_name_pos(node, sel))
			} else {
				tc.record_error_at(.unknown_field, message, sel_id, tc.node_value_diagnostic_pos(sel_id))
			}
			return
		}
		return
	}
	iface_name := tc.interface_metadata_name(constraint.iface.name)
	display := iface_name.all_after_last('.')
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

// type_has_member reports whether the type `typ` has the method `member`, or
// without `is_call` the field or the method `member`.
fn (tc &TypeChecker) type_has_member(typ Type, member string, is_call bool) bool {
	name := method_type_name(unwrap_pointer(typ))
	if tc.concrete_method_signature_key(name, member) != none {
		return true
	}
	if member == 'str' && tc.type_has_implicit_str_method(name) {
		return true
	}
	return !is_call && tc.struct_field_type(name, member) != none
}
