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
	// The body is not checked, so its locals have no bindings: the walk binds
	// each one to the type of its value where it is declared, in a scope of its
	// own. V has no shadowing, so one scope in source order gives every use the
	// declaration it sees, and `x := a` makes `x` a `T` when `a` is one.
	mut guards := map[int]bool{}
	// Operators written as statements, `items << x`, an append.
	mut statements := map[int]bool{}
	tc.push_scope()
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
			.decl_assign {
				tc.bind_constraint_walk_decl(*node, guards[int(item.id)])
			}
			.for_in_stmt {
				tc.bind_constraint_walk_loop(*node)
			}
			.if_expr {
				// `if x := opt {`: its guard binds what the option holds.
				if node.children_count > 0 {
					cond_id := tc.a.child(node, 0)
					if tc.valid_node_id(cond_id) && tc.a.node(cond_id).kind == .decl_assign {
						guards[int(cond_id)] = true
					}
				}
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
			.expr_stmt {
				if node.children_count > 0 {
					statements[int(tc.a.child(node, 0))] = true
				}
			}
			.infix, .prefix, .postfix, .assign, .selector_assign, .index_assign, .index {
				tc.check_constraint_operator(item.id, *node, interfaces, item.narrowed,
					statements[int(item.id)])
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
	tc.pop_scope()
}

// bind_constraint_walk_decl binds the names that the declaration `node`
// introduces to the types of their values; the guard of an `if x := opt {`
// binds what the option holds.
fn (mut tc TypeChecker) bind_constraint_walk_decl(node flat.Node, is_guard bool) {
	if node.children_count < 2 {
		return
	}
	if is_guard {
		lhs_ids := tc.if_guard_lhs_ids(node)
		if lhs_ids.len == 1 {
			rhs_type := unalias_type(tc.constraint_walk_value_type(tc.a.child(&node, 1)))
			held := match rhs_type {
				OptionType { rhs_type.base_type }
				ResultType { rhs_type.base_type }
				else { rhs_type }
			}
			tc.bind_constraint_walk_local(lhs_ids[0], held)
		}
		return
	}
	lhs_ids := tc.multi_assign_lhs_ids(node)
	rhs_count := tc.multi_assign_rhs_count(node)
	if lhs_ids.len == rhs_count {
		for k, lhs_id in lhs_ids {
			tc.bind_constraint_walk_local(lhs_id, tc.constraint_walk_value_type(tc.multi_assign_rhs_id(node,
				k)))
		}
	} else if rhs_count == 1 {
		// `x, y := f()`: the values of a call that returns several.
		rhs_type := unalias_type(tc.constraint_walk_value_type(tc.multi_assign_rhs_id(node, 0)))
		if rhs_type is MultiReturn && rhs_type.types.len == lhs_ids.len {
			for k, lhs_id in lhs_ids {
				tc.bind_constraint_walk_local(lhs_id, rhs_type.types[k])
			}
		}
	}
}

// bind_constraint_walk_loop binds the variables of a `for ... in` loop to what
// its container holds: `for x in items` makes `x` a `T` when `items` is a `[]T`.
fn (mut tc TypeChecker) bind_constraint_walk_loop(node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		return
	}
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	if header == 4 || tc.a.node(container_id).kind == .range {
		tc.bind_constraint_walk_local(key_id, Type(int_))
		return
	}
	container := tc.constraint_walk_iterable_type(container_id)
	mut key := Type(int_)
	mut value := Type(u8_)
	match container {
		Array {
			value = container.elem_type
		}
		ArrayFixed {
			value = container.elem_type
		}
		Map {
			key = container.key_type
			value = container.value_type
		}
		String {}
		else {
			return
		}
	}
	if int(val_id) >= 0 {
		tc.bind_constraint_walk_local(key_id, key)
		tc.bind_constraint_walk_local(val_id, value)
	} else {
		tc.bind_constraint_walk_local(key_id, if container is Map { key } else { value })
	}
}

// constraint_walk_value_type is the type of the value `id` in the body of the
// walk. A generic call takes the types of its own arguments there: `find(items)`
// in the body of `fn f[U Named](items []U)` is a `?U`, not the `?T` that `find`
// declares, which would name a type parameter of the caller by chance.
fn (mut tc TypeChecker) constraint_walk_value_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return tc.resolve_type(id)
	}
	node := tc.a.node(id)
	match node.kind {
		.call {
			if typ := tc.constraint_walk_call_type(id, *node) {
				return typ
			}
		}
		.paren {
			if node.children_count > 0 {
				return tc.constraint_walk_value_type(tc.a.child(node, 0))
			}
		}
		.or_expr {
			// `f() or { ... }`: what the option holds.
			if node.children_count > 0 {
				held := unalias_type(tc.constraint_walk_value_type(tc.a.child(node, 0)))
				return match held {
					OptionType { held.base_type }
					ResultType { held.base_type }
					else { held }
				}
			}
		}
		else {}
	}
	return tc.resolve_type(id)
}

// constraint_walk_call_type is the type of the call `node` in the body of the
// walk when what it calls is generic: its declared return type, with the type
// parameters of its declaration bound from the call, from its explicit type
// arguments, its receiver and its arguments; none when it is not generic. A type
// parameter that stays unbound gives an unknown type rather than one that names
// the declaration's own `T`, which a type parameter of the caller can share by
// chance.
fn (mut tc TypeChecker) constraint_walk_call_type(id flat.NodeId, node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	info := tc.resolve_call_info(id, node) or { return none }
	fn_params := tc.fn_generic_params[info.name] or { []string{} }
	param_texts := tc.fn_param_type_texts[info.name] or { []string{} }
	mut names := fn_params.clone()
	callee := tc.a.child_node(&node, 0)
	mut receiver_id := flat.NodeId(-1)
	if info.has_receiver {
		_, receiver_params, receiver_is_generic := generic_type_application_parts(info.name.all_before_last('.'))
		if receiver_is_generic {
			for param in receiver_params {
				if param !in names {
					names << param
				}
			}
		}
		mut method := *callee
		if method.kind == .index && method.children_count > 0 {
			method = *tc.a.child_node(&method, 0)
		}
		if method.kind == .selector && method.children_count > 0 {
			receiver_id = tc.a.child(&method, 0)
		}
	}
	if names.len == 0 {
		return none
	}
	mut inferred := map[string]Type{}
	if tc.call_has_explicit_generic_type_args(node) {
		for k, name in tc.generic_call_type_arg_names(*callee) {
			if k < fn_params.len {
				inferred[fn_params[k]] = tc.constraint_walk_named_type(name)
			}
		}
	}
	mut first := 0
	if info.has_receiver && param_texts.len > 0 {
		if tc.valid_node_id(receiver_id) {
			receiver := unwrap_pointer(tc.constraint_walk_value_type(receiver_id))
			tc.constraint_walk_infer(param_texts[0], receiver, names, mut inferred)
		}
		first = 1
	}
	for param_idx in first .. param_texts.len {
		arg_idx := param_idx - first + 1 + info.arg_offset
		if arg_idx >= node.children_count {
			break
		}
		arg := tc.constraint_walk_value_type(tc.call_arg_value(tc.a.child(&node, arg_idx)))
		tc.constraint_walk_infer(param_texts[param_idx], arg, names, mut inferred)
	}
	mut args := []Type{cap: names.len}
	for name in names {
		args << inferred[name] or { return unknown_type('unbound type parameter') }
	}
	declared := tc.fn_ret_types[info.name] or { info.return_type }
	return tc.substitute_generic_type_values(declared, args, names)
}

// constraint_walk_infer binds the type parameters `names` that `param_text`, the
// type of a parameter of a generic declaration, spells, from `actual`, the type
// of what a call passes there. A generic struct binds its type arguments,
// `Box[T]` against `Box[U]`; infer_generic_type_value_from_type does the rest.
fn (mut tc TypeChecker) constraint_walk_infer(param_text string, actual Type, names []string, mut inferred map[string]Type) {
	clean := trimmed_space(param_text)
	if clean.starts_with('&') {
		tc.constraint_walk_infer(clean[1..], unwrap_pointer(actual), names, mut inferred)
		return
	}
	if clean.starts_with('mut ') {
		tc.constraint_walk_infer(clean[4..], actual, names, mut inferred)
		return
	}
	if clean.starts_with('[]') {
		if array := array_type_from_receiver(actual) {
			tc.constraint_walk_infer(clean[2..], array.elem_type, names, mut inferred)
		}
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		held := unalias_type(actual)
		base := match held {
			OptionType { held.base_type }
			ResultType { held.base_type }
			else { held }
		}
		tc.constraint_walk_infer(clean[1..], base, names, mut inferred)
		return
	}
	if generic_type_application(clean) {
		param_base, param_args, _ := generic_type_application_parts(clean)
		actual_base, actual_args, actual_is_generic := generic_type_application_parts(tc.generic_infer_type_text(unwrap_pointer(actual)))
		if actual_is_generic && param_args.len == actual_args.len
			&& tc.generic_type_base_matches(tc.generic_param_type_text(param_base), actual_base) {
			for i in 0 .. param_args.len {
				tc.constraint_walk_infer(param_args[i], tc.constraint_walk_named_type(actual_args[i]),
					names, mut inferred)
			}
			return
		}
	}
	tc.infer_generic_type_value_from_type(clean, actual, names, mut inferred)
}

// constraint_walk_named_type is the type that `name`, an explicit type argument
// of a call in the body of the walk, names there: a type parameter of the caller,
// or a type.
fn (tc &TypeChecker) constraint_walk_named_type(name string) Type {
	if is_bare_generic_param(name)
		&& (name in tc.fn_context.generic_params || tc.active_generic_param(name)) {
		return unknown_type('generic placeholder `${name}`')
	}
	return tc.parse_type(name)
}

// constraint_walk_iterable_type is what a `for ... in` loop of the walk goes
// through: the container without its aliases, its pointer or its option, as
// for_in_iterable_type takes it.
fn (mut tc TypeChecker) constraint_walk_iterable_type(container_id flat.NodeId) Type {
	mut clean := unwrap_pointer(tc.constraint_walk_value_type(container_id))
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		if clean is OptionType {
			base := unalias_type(unwrap_pointer(clean.base_type))
			if base is Array || base is ArrayFixed {
				clean = base
				continue
			}
		}
		break
	}
	return clean
}

// bind_constraint_walk_local binds the local that `id` names, if it names one,
// to `typ` in the scope of the walk.
fn (mut tc TypeChecker) bind_constraint_walk_local(id flat.NodeId, typ Type) {
	if !tc.valid_node_id(id) {
		return
	}
	local := tc.a.node(id)
	if local.kind == .ident && local.value.len > 0 && local.value != '_' {
		tc.cur_scope.insert(local.value, typ)
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
	base_type := unwrap_pointer(tc.constraint_walk_value_type(base_id))
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
				tc.record_error_at(.unknown_field, message, sel_id, tc.constraint_member_pos(sel_id, sel))
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
			sel_id, tc.constraint_member_pos(sel_id, sel))
	}
}

// constraint_member_pos is where the member that the selector `sel` names is
// written: at the end of the selector, after its base, which can hold the same
// name before it, `x.age + pick(n).age`.
fn (tc &TypeChecker) constraint_member_pos(sel_id flat.NodeId, sel flat.Node) token.Pos {
	end := int(sel.pos.end)
	start := end - sel.value.len
	source := tc.vls_source(int(sel.pos.id))
	if start > int(sel.pos.offset) && end <= source.len && source[start..end] == sel.value {
		return token.new_span(sel.pos.id, start, end)
	}
	return tc.node_value_diagnostic_pos(sel_id)
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

// ConstraintOperand is the other operand of an operator on a value of a
// constrained type parameter, told apart as the rules of the operators tell it:
// another value of the same type parameter, a literal, or a value of a builtin
// type. Nothing is checked against an unknown one.
enum ConstraintOperand {
	unknown
	same
	int_lit
	float_lit
	str_lit
	bool_lit
	rune_lit
	int_val
	float_val
	str_val
	bool_val
	rune_val
}

fn (o ConstraintOperand) is_int_like() bool {
	return o in [.int_lit, .rune_lit, .int_val, .rune_val]
}

fn (o ConstraintOperand) is_int() bool {
	return o in [.int_lit, .int_val]
}

fn (o ConstraintOperand) is_rune() bool {
	return o in [.rune_lit, .rune_val]
}

fn (o ConstraintOperand) is_float() bool {
	return o in [.float_lit, .float_val]
}

fn (o ConstraintOperand) is_str() bool {
	return o in [.str_lit, .str_val]
}

fn (o ConstraintOperand) is_bool() bool {
	return o in [.bool_lit, .bool_val]
}

// ConstraintTypeKind groups the types of a constraint by the operators the
// checker takes on them.
enum ConstraintTypeKind {
	unchecked // pointers and what else the rules below do not cover
	integer
	isize // `isize` and `usize`: no `==` with a float
	rune
	float
	string
	boolean
	flag_enum
	other // `==` and `!=`, and the operators the type declares
}

const constraint_integer_ops = ['+', '-', '*', '/', '%', '**', '<', '>', '<=', '>=', '==', '!=',
	'&', '|', '^', '<<', '>>', '>>>']
const constraint_float_ops = ['+', '-', '*', '/', '**', '<', '>', '<=', '>=', '==', '!=']
const constraint_overloadable_ops = ['+', '-', '*', '/', '%', '**']
const constraint_order_ops = ['<', '>', '<=', '>=']

// constraint_type_kind is the group of `typ` for the rules of the operators.
fn (tc &TypeChecker) constraint_type_kind(typ Type) ConstraintTypeKind {
	clean := unalias_type(typ)
	return match clean {
		ISize, USize {
			.isize
		}
		Rune {
			.rune
		}
		Primitive {
			if clean.props.has(.boolean) {
				ConstraintTypeKind.boolean
			} else if clean.props.has(.float) {
				ConstraintTypeKind.float
			} else if clean.props.has(.integer) {
				ConstraintTypeKind.integer
			} else {
				ConstraintTypeKind.unchecked
			}
		}
		String {
			.string
		}
		Enum {
			if clean.is_flag { ConstraintTypeKind.flag_enum } else { ConstraintTypeKind.other }
		}
		Struct, Interface, SumType, Array, ArrayFixed, Map, OptionType, ResultType, FnType {
			.other
		}
		else {
			.unchecked
		}
	}
}

// constraint_type_declares_operator reports whether `typ`, or the type it is an
// alias of, declares the operator `op`: `fn (a Vec) + (b Vec) Vec`. `<` gives
// `>`, `<=` and `>=` too.
fn (tc &TypeChecker) constraint_type_declares_operator(typ Type, op string) bool {
	name := if op in constraint_order_ops { '<' } else { op }
	flat_op := match name {
		'+' { flat.Op.plus }
		'-' { flat.Op.minus }
		'*' { flat.Op.mul }
		'/' { flat.Op.div }
		'%' { flat.Op.mod }
		'**' { flat.Op.power }
		'<' { flat.Op.lt }
		else { return false }
	}
	return tc.infix_operator_signature(flat_op, typ) != none
}

// constraint_binary_allowed reports whether the operator `op` works on a value of
// the type `typ` and `other`, on its right, or on its left with `left`: what the
// checker takes for that type. The rules follow what it reports for every kind
// of operand, `<` and the others with #28854, which rejects an order between
// operands that have none.
fn (tc &TypeChecker) constraint_binary_allowed(typ Type, op string, other ConstraintOperand, left bool) bool {
	if other == .unknown {
		return true
	}
	kind := tc.constraint_type_kind(typ)
	match kind {
		.unchecked {
			return true
		}
		.integer, .isize, .rune {
			if other == .same || other.is_int_like() {
				return op in constraint_integer_ops
			}
			if other.is_float() {
				if kind == .integer {
					// An alias that declares the operator takes only its own type on its right.
					if !left && typ is Alias && tc.constraint_type_declares_operator(typ, op) {
						return false
					}
					return op in constraint_float_ops
				}
				return op in constraint_float_ops && op !in ['==', '!=']
			}
			return kind == .rune && other.is_str() && !left && op == '+'
		}
		.float {
			if other == .same || other.is_float() || other.is_int() {
				return op in constraint_float_ops
			}
			return other.is_rune() && op in constraint_float_ops && op !in ['==', '!=']
		}
		.string {
			if other == .same || other.is_str() {
				return op in ['+', '<', '>', '<=', '>=', '==', '!=']
			}
			return other.is_rune() && left && op == '+'
		}
		.boolean {
			return (other == .same || other.is_bool()) && op in ['==', '!=', '&&', '||']
		}
		.flag_enum {
			if other == .same {
				return op in ['==', '!=', '&', '|', '^']
			}
			return other.is_int() && op in ['==', '!=']
		}
		.other {
			if other != .same {
				return false
			}
			if op in ['==', '!='] {
				return true
			}
			if op in constraint_order_ops || op in constraint_overloadable_ops {
				return tc.constraint_type_declares_operator(typ, op)
			}
			return false
		}
	}
}

// constraint_unary_allowed reports whether the prefix operator `op`, `-`, `!` or
// `~`, works on a value of the type `typ`.
fn (tc &TypeChecker) constraint_unary_allowed(typ Type, op string) bool {
	return match tc.constraint_type_kind(typ) {
		.unchecked { true }
		.integer, .isize, .rune { op in ['-', '~'] }
		.float { op == '-' }
		.boolean { op == '!' }
		.flag_enum { op == '~' }
		else { false }
	}
}

// constraint_postfix_allowed reports whether `++` and `--` work on a value of
// the type `typ`.
fn (tc &TypeChecker) constraint_postfix_allowed(typ Type) bool {
	return tc.constraint_type_kind(typ) in [.unchecked, .integer, .isize, .rune, .float]
}

// constraint_assign_allowed reports whether the assignment operator `op=`, with
// `op` the operator it applies, works on a value of the type `typ` and `other`.
fn (tc &TypeChecker) constraint_assign_allowed(typ Type, op string, other ConstraintOperand) bool {
	if other == .unknown {
		return true
	}
	match tc.constraint_type_kind(typ) {
		.unchecked {
			return true
		}
		.integer, .isize, .rune {
			return (other == .same || other.is_int_like()) && op in constraint_integer_ops
				&& op !in ['<', '>', '<=', '>=', '==', '!=']
		}
		.float {
			return (other == .same || other.is_float() || other.is_int())
				&& op in constraint_overloadable_ops
		}
		.string {
			if other == .same && typ is Alias {
				// What the checker takes on an alias of `string` with its own type.
				return op in ['+', '-', '*', '/', '%']
			}
			return (other == .same || other.is_str() || other.is_rune()) && op == '+'
		}
		.boolean {
			return false
		}
		.flag_enum {
			return (other == .same || other.is_int()) && op in ['&', '|', '^', '<<', '>>', '>>>']
		}
		.other {
			return other == .same && op in constraint_overloadable_ops
				&& tc.constraint_type_declares_operator(typ, op)
		}
	}
}

// constraint_index_allowed reports whether a value of the type `typ` can be
// indexed with `index`: an array or a string with an integer, a map with its
// key, a type that declares `[]` with its parameter.
fn (tc &TypeChecker) constraint_index_allowed(typ Type, index ConstraintOperand) bool {
	if index == .unknown {
		return true
	}
	clean := unalias_type(typ)
	match clean {
		Array, ArrayFixed, String {
			return index.is_int()
		}
		Map {
			return tc.constraint_operand_matches(clean.key_type, index)
		}
		Struct, Alias {
			info := tc.index_overload_call_info(typ, false) or { return false }
			if info.params.len < 2 {
				return false
			}
			return tc.constraint_operand_matches(info.params[1], index)
		}
		else {
			return tc.constraint_type_kind(typ) == .unchecked
		}
	}
}

// constraint_operand_matches reports whether `operand` is of the builtin type
// `typ`, which an index takes: an integer for an integer, a string for a string.
fn (tc &TypeChecker) constraint_operand_matches(typ Type, operand ConstraintOperand) bool {
	return match tc.constraint_type_kind(typ) {
		.integer, .isize { operand.is_int() }
		.string { operand.is_str() }
		.unchecked { true }
		else { false }
	}
}

// constraint_operand is what the operand `id` is to an operator whose other
// operand is a value of the type parameter `param`.
fn (mut tc TypeChecker) constraint_operand(id flat.NodeId, param string) ConstraintOperand {
	if !tc.valid_node_id(id) {
		return .unknown
	}
	node := tc.a.node(id)
	match node.kind {
		.int_literal {
			return .int_lit
		}
		.float_literal {
			return .float_lit
		}
		.string_literal, .string_interp {
			return .str_lit
		}
		.bool_literal {
			return .bool_lit
		}
		.char_literal {
			return .rune_lit
		}
		.paren {
			if node.children_count > 0 {
				return tc.constraint_operand(tc.a.child(node, 0), param)
			}
		}
		else {}
	}
	typ := tc.constraint_walk_value_type(id)
	if typ is Unknown {
		if name := generic_placeholder_from_unknown(typ) {
			return if name == param { ConstraintOperand.same } else { ConstraintOperand.unknown }
		}
		return .unknown
	}
	clean := unalias_type(typ)
	return match clean {
		ISize, USize {
			.int_val
		}
		Rune {
			.rune_val
		}
		Primitive {
			if clean.props.has(.boolean) {
				ConstraintOperand.bool_val
			} else if clean.props.has(.float) {
				ConstraintOperand.float_val
			} else if clean.props.has(.integer) {
				ConstraintOperand.int_val
			} else {
				ConstraintOperand.unknown
			}
		}
		String {
			.str_val
		}
		else {
			.unknown
		}
	}
}

// constraint_operand_name is how an error names `operand`, the value of `id`.
fn (mut tc TypeChecker) constraint_operand_name(id flat.NodeId, operand ConstraintOperand) string {
	return match operand {
		.int_lit { 'int literal' }
		.float_lit { 'float literal' }
		.str_lit { 'string' }
		.bool_lit { 'bool' }
		.rune_lit { 'rune' }
		else { tc.constraint_walk_value_type(id).name().all_after_last('.') }
	}
}

// constraint_walk_param is the constrained type parameter whose value `id` is,
// if it is one that `interfaces` constrains and no `is` has decided its type.
fn (mut tc TypeChecker) constraint_walk_param(id flat.NodeId, interfaces map[string]GenericConstraint, narrowed []string) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident && node.value in narrowed {
		return none
	}
	typ := tc.constraint_walk_value_type(id)
	if typ !is Unknown {
		return none
	}
	param := generic_placeholder_from_unknown(typ as Unknown) or { return none }
	if param !in interfaces {
		return none
	}
	return param
}

// check_constraint_operator reports an operator of the body of a generic
// function, `a + b`, `-a`, `a++`, `a += b` or `a[i]`, on a value of a constrained
// type parameter, that some type of its constraint does not take: an interface
// takes `==` and `!=` only, as a value of the interface does, and a set of types
// takes what every one of them takes.
fn (mut tc TypeChecker) check_constraint_operator(id flat.NodeId, node flat.Node, interfaces map[string]GenericConstraint, narrowed []string, is_statement bool) {
	match node.kind {
		.infix {
			if node.children_count < 2 {
				return
			}
			op := infix_operator_name(node.op) or { return }
			lhs_id := tc.a.child(&node, 0)
			rhs_id := tc.a.child(&node, 1)
			if param := tc.constraint_walk_param(lhs_id, interfaces, narrowed) {
				other := tc.constraint_operand(rhs_id, param)
				tc.check_constraint_binary(id, node, op, param, interfaces[param], other, rhs_id,
					false, is_statement)
			} else if param := tc.constraint_walk_param(rhs_id, interfaces, narrowed) {
				other := tc.constraint_operand(lhs_id, param)
				tc.check_constraint_binary(id, node, op, param, interfaces[param], other, lhs_id,
					true, is_statement)
			}
		}
		.prefix {
			if node.children_count == 0 || node.op !in [.minus, .not, .bit_not] {
				return
			}
			param := tc.constraint_walk_param(tc.a.child(&node, 0), interfaces, narrowed) or {
				return
			}
			op := match node.op {
				.minus { '-' }
				.not { '!' }
				else { '~' }
			}
			constraint := interfaces[param]
			for typ in tc.constraint_operator_types(constraint) {
				if !tc.constraint_unary_allowed(typ, op) {
					tc.record_constraint_operator_error(id, tc.prefix_operator_pos(id, op), 'operator `${op}` is not defined on type `${param}`',
						constraint, typ)
					return
				}
			}
		}
		.postfix {
			if node.children_count == 0 || node.op !in [.inc, .dec] {
				return
			}
			param := tc.constraint_walk_param(tc.a.child(&node, 0), interfaces, narrowed) or {
				return
			}
			op := if node.op == .inc { '++' } else { '--' }
			constraint := interfaces[param]
			for typ in tc.constraint_operator_types(constraint) {
				if !tc.constraint_postfix_allowed(typ) {
					tc.record_constraint_operator_error(id, tc.prefix_operator_pos(id, op), 'operator `${op}` is not defined on type `${param}`',
						constraint, typ)
					return
				}
			}
		}
		.assign, .selector_assign, .index_assign {
			if node.children_count < 2 {
				return
			}
			// `+=` and the like; a plain `=` changes no type.
			assign_op := assignment_operator_text(node.op)
			if assign_op.len < 2 {
				return
			}
			op := assign_op[..assign_op.len - 1]
			lhs_id := tc.a.child(&node, 0)
			rhs_id := tc.a.child(&node, 1)
			param := tc.constraint_walk_param(lhs_id, interfaces, narrowed) or { return }
			other := tc.constraint_operand(rhs_id, param)
			constraint := interfaces[param]
			for typ in tc.constraint_operator_types(constraint) {
				if !tc.constraint_assign_allowed(typ, op, other) {
					what := if other == .same {
						'operator `${assign_op}` is not defined on type `${param}`'
					} else {
						'operator `${assign_op}` is not defined on type `${param}` and `${tc.constraint_operand_name(rhs_id, other)}`'
					}
					tc.record_constraint_operator_error(id, tc.constraint_assign_operator_pos(node, assign_op),
						what, constraint, typ)
					return
				}
			}
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return
			}
			base_id := tc.a.child(&node, 0)
			index_id := tc.a.child(&node, 1)
			if tc.valid_node_id(index_id) && tc.a.node(index_id).kind == .range {
				return
			}
			param := tc.constraint_walk_param(base_id, interfaces, narrowed) or { return }
			index := tc.constraint_operand(index_id, param)
			constraint := interfaces[param]
			for typ in tc.constraint_operator_types(constraint) {
				if !tc.constraint_index_allowed(typ, index) {
					base := tc.a.node(base_id)
					pos := tc.constraint_bracket_pos(node, base)
					tc.record_constraint_operator_error(id, pos, 'type `${param}` cannot be indexed with `${tc.constraint_operand_name(index_id, index)}`', constraint, typ)
					return
				}
			}
		}
		else {}
	}
}

// check_constraint_binary reports the binary operator `op` of `node` when a type
// of `constraint` does not take it with `other`, the value of `other_id`, on the
// right of the value of `param`, or on its left with `left`.
fn (mut tc TypeChecker) check_constraint_binary(id flat.NodeId, node flat.Node, op string, param string, constraint GenericConstraint, other ConstraintOperand, other_id flat.NodeId, left bool, is_statement bool) {
	if op in ['&&', '||'] && other == .unknown {
		return
	}
	for typ in tc.constraint_operator_types(constraint) {
		if is_statement && op == '<<' && unalias_type(typ) is Array {
			// An append, `items << x`: what the array holds decides it.
			continue
		}
		if tc.constraint_binary_allowed(typ, op, other, left) {
			continue
		}
		what := if other == .same {
			'operator `${op}` is not defined on type `${param}`'
		} else if left {
			'operator `${op}` is not defined on `${tc.constraint_operand_name(other_id, other)}` and type `${param}`'
		} else {
			'operator `${op}` is not defined on type `${param}` and `${tc.constraint_operand_name(other_id, other)}`'
		}
		tc.record_constraint_operator_error(id, tc.infix_operator_pos(node, op), what, constraint,
			typ)
		return
	}
}

// constraint_operator_types are the types an operator on a value of a type
// parameter with `constraint` must work for: the interface itself, or every
// type of the set.
fn (tc &TypeChecker) constraint_operator_types(constraint GenericConstraint) []Type {
	if constraint.is_interface {
		return [constraint.iface]
	}
	return constraint.types
}

// record_constraint_operator_error reports `what`, an operator that the type
// `typ` of `constraint` does not take, with the constraint that decides it.
fn (mut tc TypeChecker) record_constraint_operator_error(id flat.NodeId, pos token.Pos, what string, constraint GenericConstraint, typ Type) {
	message := if constraint.is_interface {
		display := tc.interface_metadata_name(constraint.iface.name).all_after_last('.')
		'${what}: its constraint `${display}` does not declare it'
	} else {
		'${what}: `${typ.name().all_after_last('.')}`, in its constraint `${constraint.name}`, does not have it'
	}
	tc.record_error_at(.assignment_mismatch, message, id, pos)
}

// constraint_assign_operator_pos is where the assignment operator `op` of
// `node`, `+=` in `c += b`, is written: between its two sides.
fn (tc &TypeChecker) constraint_assign_operator_pos(node flat.Node, op string) token.Pos {
	if node.children_count >= 2 {
		lhs := tc.a.child_node(&node, 0)
		rhs := tc.a.child_node(&node, 1)
		source := tc.vls_source(int(lhs.pos.id))
		start := int(lhs.pos.end)
		end := int_min(int(rhs.pos.offset), source.len)
		if start >= 0 && start < end {
			if relative := source[start..end].index(op) {
				return token.new_span(lhs.pos.id, start + relative, start + relative + op.len)
			}
		}
	}
	return node.pos
}

// constraint_bracket_pos is where the `[` of the index `node` is written, after
// its base.
fn (tc &TypeChecker) constraint_bracket_pos(node flat.Node, base flat.Node) token.Pos {
	source := tc.vls_source(int(node.pos.id))
	start := int_max(int(base.pos.end), int(node.pos.offset))
	if start >= 0 && start < source.len {
		if open := source.index_after('[', start) {
			if open < int(node.pos.end) {
				return token.new_span(node.pos.id, open, open + 1)
			}
		}
	}
	return node.pos
}
