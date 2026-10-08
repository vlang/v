module types

import v.flat

// vls_hover answers a hover request with the declaration of what the cursor is
// on, in the format of V1's mini-VLS protocol.
fn (mut tc TypeChecker) vls_hover(target VlsTarget) string {
	declaration := tc.vls_hover_declaration(target)
	if declaration == '' {
		return ''
	}
	return vls_hover_json(declaration, '')
}

// vls_hover_declaration is the text of the hover answer: `fn name(p t) r`,
// `name type`, `const name type`, `struct Name`, `Enum.value`, or '' for
// nothing to say.
fn (mut tc TypeChecker) vls_hover_declaration(target VlsTarget) string {
	id := target.id
	node := tc.a.nodes[int(id)]
	tc.vls_enter_file(target.file_id)
	// The name of a call stands for the function it calls; a value that holds
	// one, as a local or a parameter of a function type, is that value.
	if call_id := tc.vls_called_by(id) {
		if resolved := tc.vls_call_target(call_id, id) {
			return tc.vls_fn_signature(resolved) or { '' }
		}
	}
	match node.kind {
		.ident {
			return tc.vls_hover_ident(id, node)
		}
		.selector {
			return tc.vls_hover_selector(node)
		}
		.enum_val {
			typ := tc.expr_type(id) or { tc.vls_match_subject_type(id) or { return '' } }
			enum_type := vls_unwrap_type(typ)
			if enum_type is Enum {
				return tc.vls_enum_value_hover(enum_type.name, node.value)
			}
			return '${tc.vls_type_text(typ).all_after_last('.')}.${node.value}'
		}
		.enum_field {
			decl_id := tc.vls_parent_id(id)
			if !tc.valid_node_id(decl_id) || tc.a.node(decl_id).kind != .enum_decl {
				return ''
			}
			decl := tc.a.node(decl_id)
			value := tc.vls_enum_values(decl)[node.value] or {
				return '${decl.value}.${node.value}'
			}
			return '${decl.value}.${node.value} = ${value}'
		}
		.cast_expr, .struct_init, .is_expr, .as_expr {
			if text := tc.vls_type_param_hover(id, node.value) {
				return text
			}
			return tc.vls_type_declaration(node.value) or { '' }
		}
		.param {
			declared := tc.parse_type(node.typ)
			if type_contains_unknown(declared) {
				return tc.vls_generic_value_text(id, node.value, node.typ)
			}
			return '${node.value} ${tc.vls_type_text(declared)}'
		}
		.field_init {
			owner := tc.vls_field_init_owner(id) or { return '' }
			typ := tc.vls_field_type(owner, node.value) or { return '' }
			return '${node.value} ${tc.vls_type_text(typ)}'
		}
		else {
			return ''
		}
	}
}

// vls_field_init_owner is the struct `name: value` sets a field of: the one of
// a struct literal, or of a call's parameter, `f(name: value)`.
fn (tc &TypeChecker) vls_field_init_owner(id flat.NodeId) ?Type {
	parent_id := tc.vls_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return none
	}
	parent := tc.a.node(parent_id)
	if parent.kind == .struct_init {
		// The checker keeps no type for a struct literal: it is the one it names.
		return tc.expr_type(parent_id) or { tc.parse_type(parent.value.all_before('[')) }
	}
	if parent.kind != .call || parent.children_count < 2 {
		return none
	}
	resolved := tc.vls_call_target(parent_id, tc.a.child(parent, 0))?
	sig := tc.vls_signature(resolved)?
	// The named arguments fill the parameter after the positional ones.
	for i in 1 .. parent.children_count {
		if tc.a.child_node(parent, i).kind == .field_init {
			if i - 1 < sig.types.len && sig.types[i - 1] !is Void {
				return sig.types[i - 1]
			}
			break
		}
	}
	return none
}

// vls_match_subject_type is the type of the value a `match` compares, for a
// pattern of one of its branches: `.red` in `match c { .red {} }`.
fn (tc &TypeChecker) vls_match_subject_type(id flat.NodeId) ?Type {
	branch_id := tc.vls_parent_id(id)
	if !tc.valid_node_id(branch_id) || tc.a.node(branch_id).kind != .match_branch {
		return none
	}
	match_id := tc.vls_parent_id(branch_id)
	if !tc.valid_node_id(match_id) || tc.a.node(match_id).kind != .match_stmt {
		return none
	}
	match_node := tc.a.node(match_id)
	if match_node.children_count == 0 {
		return none
	}
	return tc.expr_type(tc.a.child(match_node, 0))
}

// vls_enter_file makes names resolve as they do in the file the query is about.
fn (mut tc TypeChecker) vls_enter_file(file_id int) {
	file := tc.a.source_files[file_id] or { return }
	tc.cur_file = file.name
	tc.cur_module = tc.file_modules[file.name] or { 'main' }
}

// vls_call_target is the function a call calls: the name the checker resolved
// it to, or where the checker resolved none, the method of the declared type of
// the receiver: a builtin method of an array, a string or a map, which the
// checker handles without resolving, or any method in the body of a generic
// function, which the checker does not type.
fn (tc &TypeChecker) vls_call_target(call_id flat.NodeId, callee_id flat.NodeId) ?string {
	if resolved := tc.resolved_call_name(call_id) {
		return resolved
	}
	callee := tc.a.node(callee_id)
	// `f[T](x)` calls `f`.
	name_id := if callee.kind == .index && callee.children_count > 0 {
		tc.a.child(callee, 0)
	} else {
		callee_id
	}
	// A function called by its name where the checker resolved no call: in the
	// body of a generic function, as it checks the body of each instance.
	name_node := tc.a.node(name_id)
	if name_node.kind == .ident && tc.vls_local_declaration(name_id) == none {
		name := tc.qualify_name(name_node.value)
		if name in tc.fn_type_files {
			return name
		}
	}
	// A method, `x.m(...)`, or `x.m[T](...)` with its type arguments.
	if name_node.kind != .selector || name_node.children_count == 0 {
		return none
	}
	return tc.vls_method_target(tc.a.child(name_node, 0), name_node.value)
}

// vls_method_target is the method `method` of the value `receiver_id`, called
// or named without a call: of its type, with its type parameters (`xs.map()` of
// `xs []T` calls the `map` of every array), of a struct that its struct embeds,
// or of the interface that the constraint of its type parameter names. A method
// of a generic struct is registered as its declaration writes its receiver,
// `Box[T].label`; one of an interface keeps the type arguments of the value,
// `Shelf[User].get`, which its signature takes.
fn (tc &TypeChecker) vls_method_target(receiver_id flat.NodeId, method string) ?string {
	mut receiver_type := tc.vls_value_type(receiver_id)?
	if constrained := tc.vls_constrained_type(receiver_id, receiver_type) {
		receiver_type = constrained
	}
	// A method of an alias comes before the ones of the type it names, as in a
	// call: `a.describe` with `fn (a Alias) describe()`.
	alias_receiver := unwrap_all_pointers(receiver_type)
	if alias_receiver is Alias {
		alias_method := '${alias_receiver.name}.${method}'
		if alias_method in tc.fn_type_files {
			return alias_method
		}
	}
	owner := tc.vls_member_owner(receiver_type)?
	name := '${owner}.${method}'
	if name in tc.fn_type_files || tc.vls_builtin_method_decl(name) != none {
		return name
	}
	if info := tc.resolve_generic_struct_method(owner, method) {
		return info.name
	}
	owners := tc.embedded_method_candidates(owner, method)
	if owners.len == 1 {
		return '${owners[0]}.${method}'
	}
	if _ := tc.vls_interface_member(owner, method) {
		return name
	}
	return none
}

// vls_interface_decl is the declaration of the interface `name`; of a generic
// interface, `Shelf[User]`, the one of `Shelf`.
fn (tc &TypeChecker) vls_interface_decl(name string) ?flat.Node {
	base, _, is_generic := generic_type_application_parts(name)
	mut keys := [name]
	if is_generic {
		keys << base
	}
	for key in keys {
		index := tc.first_type_declaration_ids[key] or { continue }
		decl := tc.a.nodes[index]
		if decl.kind == .interface_decl {
			return decl
		}
	}
	return none
}

// vls_interface_member is the member `method` that the interface `name`
// declares, generic or not.
fn (tc &TypeChecker) vls_interface_member(name string, method string) ?flat.Node {
	decl := tc.vls_interface_decl(name)?
	for i in 0 .. decl.children_count {
		member := tc.a.child_node(&decl, i)
		if member.kind == .interface_field && member.value == method {
			return *member
		}
	}
	return none
}

// vls_expr_type is the type the checker gave the expression `id`, or for a
// call it kept no type for, the return type of the function it calls.
fn (tc &TypeChecker) vls_expr_type(id flat.NodeId) ?Type {
	if typ := tc.expr_type(id) {
		return tc.vls_constrained_type(id, typ) or { typ }
	}
	// In the body of a generic function, what no type parameter decides.
	if typ := tc.vls_body_types[int(id)] {
		return typ
	}
	node := tc.a.node(id)
	if node.kind == .call {
		if resolved := tc.resolved_call_name(id) {
			return tc.fn_ret_types[resolved] or { return none }
		}
		// A call in a body the checker did not type, as that of a generic
		// function: an array method over a `[]T`, or what the function it calls
		// returns.
		if typ := tc.vls_array_method_call_type(node) {
			return typ
		}
		if node.children_count == 0 {
			return none
		}
		target := tc.vls_call_target(id, tc.a.child(node, 0)) or { return none }
		// Builtin declares the methods of arrays and maps over `voidptr`: the
		// checker types their calls itself.
		if target.starts_with('array.') || target.starts_with('map.') {
			return none
		}
		return tc.fn_ret_types[target] or { return none }
	}
	// A field the checker did not reach, as it stops typing a function after
	// some errors: the field's type, from the type of its receiver.
	if node.kind == .selector && node.children_count > 0 {
		// With the type parameters of the receiver: `b.item` of `b Box[T]` is a
		// `T`, which its constraint makes a value with members.
		receiver_type := tc.vls_value_type(tc.a.child(node, 0))?
		field_type := tc.vls_field_type(receiver_type, node.value)?
		return tc.vls_constrained_type(id, field_type) or { field_type }
	}
	// Literals the checker keeps no type for, as the receivers of the builtin
	// methods it handles itself: `[a, b].filter()`, `'abc'.to_upper()`.
	if node.kind == .array_literal && node.children_count > 0 {
		return Type(Array{
			elem_type: tc.vls_expr_type(tc.a.child(node, 0))?
		})
	}
	if node.kind in [.string_literal, .string_interp] {
		return Type(String{})
	}
	// The other literals, as the elements of `[1, 2].map()`: the type V gives
	// them by default.
	match node.kind {
		.int_literal { return tc.parse_type('int') }
		.float_literal { return tc.parse_type('f64') }
		.bool_literal { return tc.parse_type('bool') }
		.char_literal { return tc.parse_type('rune') }
		else {}
	}
	// The receiver of a builtin method, which the checker handles without
	// typing it: the type of the local it names, from its declaration.
	if node.kind == .ident {
		binding := tc.vls_local_binding(id) or { return none }
		// `it`, `a`, `b` or `err` in a body the checker did not type, as that
		// of a generic function.
		if binding.implicit {
			typ := tc.vls_implicit_type(binding) or { return none }
			if constrained := tc.vls_constrained_type(id, typ) {
				return constrained
			}
			return if type_contains_unknown(typ) { none } else { typ }
		}
		decl_id := binding.decl_id
		decl := tc.a.node(decl_id)
		if decl.kind == .param {
			declared := tc.parse_type(decl.typ)
			if constrained := tc.vls_constrained_type(id, declared) {
				return constrained
			}
			return if type_contains_unknown(declared) { none } else { declared }
		}
		if elem := tc.vls_lambda_param_type(decl_id) {
			if constrained := tc.vls_constrained_type(id, elem) {
				return constrained
			}
			return if type_contains_unknown(elem) { none } else { elem }
		}
		// A local that holds a `T` has what the constraint of `T` declares.
		typ := tc.vls_local_type(decl_id)?
		return tc.vls_constrained_type(id, typ) or { typ }
	}
	// An element of an array or a map the checker kept no type for: `xs[0]` of
	// `xs []T` is a `T`, which has what its constraint declares.
	if node.kind == .index {
		elem := tc.vls_element_type(node)?
		return tc.vls_constrained_type(id, elem) or { elem }
	}
	return none
}

// vls_array_method_call_type is the type of `call`, an array method that the
// checker types itself, where it did not: in the body of a generic function,
// over a `[]T`. `filter` and `sorted` give the array, `any` and `all` a bool,
// `count` an int, and `map` an array of what its argument gives an element.
fn (tc &TypeChecker) vls_array_method_call_type(call flat.Node) ?Type {
	name := tc.unresolved_array_dsl_call_name(call)
	if name == '' {
		return none
	}
	callee := tc.a.child_node(&call, 0)
	receiver := tc.vls_value_type(tc.a.child(callee, 0))?
	if unalias_type(unwrap_pointer(receiver)) !is Array {
		return none
	}
	match name {
		'array.filter', 'array.sorted' {
			return receiver
		}
		'array.any', 'array.all' {
			return tc.parse_type('bool')
		}
		'array.count' {
			return tc.parse_type('int')
		}
		'array.map' {
			if call.children_count < 2 {
				return none
			}
			arg_id := tc.a.child(&call, 1)
			arg := tc.a.node(arg_id)
			// A lambda gives what its body does, a function value what it returns,
			// and an expression of `it`, `it * 2`, its value.
			elem := if arg.kind == .lambda_expr && arg.children_count > 0 {
				body_id := tc.a.child(arg, int(arg.children_count) - 1)
				tc.vls_expr_type(body_id) or { tc.vls_unconstrained_type(body_id)? }
			} else {
				value := tc.vls_value_type(arg_id) or { tc.vls_unconstrained_type(arg_id)? }
				if value is FnType { value.return_type } else { value }
			}
			if elem is Void {
				return none
			}
			return Type(Array{
				elem_type: elem
			})
		}
		else {
			return none
		}
	}
}

// vls_implicit_type is the type of a variable the language declares, from the
// node that declares it (see VlsBinding): the elements of the array that an
// array method is called on for its `it`, `a` and `b`, and `IError` for `err`.
fn (tc &TypeChecker) vls_implicit_type(binding VlsBinding) ?Type {
	if !tc.valid_node_id(binding.site) {
		return none
	}
	site := tc.a.node(binding.site)
	if site.kind == .block {
		return tc.parse_type('IError')
	}
	return tc.vls_array_method_elem(*site)
}

// vls_lambda_param_type is the type of a parameter of a short lambda that is an
// argument of an array method, `|x| x.name` in `xs.map()`: its elements.
fn (tc &TypeChecker) vls_lambda_param_type(decl_id flat.NodeId) ?Type {
	lambda_id := tc.vls_parent_id(decl_id)
	if !tc.valid_node_id(lambda_id) || tc.a.node(lambda_id).kind != .lambda_expr {
		return none
	}
	call_id := tc.vls_parent_id(lambda_id)
	if !tc.valid_node_id(call_id) {
		return none
	}
	call := tc.a.node(call_id)
	if call.kind != .call || call.children_count == 0 || tc.a.child(call, 0) == lambda_id {
		return none
	}
	return tc.vls_array_method_elem(*call)
}

// vls_array_method_elem is the type of the elements of the array that `call`,
// an array method such as `xs.map(...)`, is called on: a `T` in the body of a
// generic function where `xs` is a `[]T`.
fn (tc &TypeChecker) vls_array_method_elem(call flat.Node) ?Type {
	if call.kind != .call || call.children_count == 0 || tc.unresolved_array_dsl_call_name(call) == '' {
		return none
	}
	callee := tc.a.child_node(&call, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	receiver := unalias_type(unwrap_pointer(tc.vls_value_type(tc.a.child(callee, 0))?))
	return if receiver is Array { receiver.elem_type } else { none }
}

// vls_value_type is the type of the value `id` with the type parameters it
// holds: `xs` of `fn f[T](xs []T)` is a `[]T`, which vls_expr_type leaves out.
fn (tc &TypeChecker) vls_value_type(id flat.NodeId) ?Type {
	if typ := tc.vls_expr_type(id) {
		return typ
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		decl_id := tc.vls_local_declaration(id)?
		decl := tc.a.node(decl_id)
		if decl.kind == .param && decl.typ.len > 0 {
			return tc.parse_type(decl.typ)
		}
	}
	// A struct literal is a value of the type it writes: `Host{}` of
	// `Host{}.first_of(values)` in a body the checker did not type.
	if node.kind == .struct_init && node.value.len > 0 {
		return tc.parse_type(node.value)
	}
	return none
}

// vls_value_type_text writes the type of a value for a hover, a type parameter
// by its name: `it T`.
fn (tc &TypeChecker) vls_value_type_text(typ Type) string {
	if typ is Unknown {
		if param := generic_placeholder_from_unknown(typ) {
			return param
		}
	}
	return tc.vls_type_text(typ)
}

// vls_constrained_type is the interface that stands for `typ` when it is a type
// parameter of the generic function around `id` that names that interface as
// its constraint: a value of it has the members of the interface.
fn (tc &TypeChecker) vls_constrained_type(id flat.NodeId, typ Type) ?Type {
	constraint := tc.vls_type_constraint(id, typ)?
	if constraint.is_interface {
		return Type(constraint.iface)
	}
	// `$if a is User {` leaves it one type: a value of it is one.
	if constraint.types.len == 1 {
		return constraint.types[0]
	}
	return none
}

// vls_type_constraint is the constraint of `typ` when it is a type parameter of
// the generic function around `id` that names one, where `id` is: in `$if T is
// f64 {` it is `f64`, as the check of the body takes it.
fn (tc &TypeChecker) vls_type_constraint(id flat.NodeId, typ Type) ?GenericConstraint {
	clean := unwrap_pointer(typ)
	if clean !is Unknown {
		return none
	}
	param := generic_placeholder_from_unknown(clean as Unknown)?
	decl, path := tc.vls_enclosing_decl(id)?
	if decl.kind != .fn_decl {
		return none
	}
	return tc.vls_narrowed_constraints(decl, path)[param] or { return none }
}

// vls_narrowed_constraints are the constraints of the type parameters of
// `fn_node` at the end of `path`, the nodes from there up to the body: the
// `$if`s on the way, together, leave each one the types with which it can go
// through all of them the way the path does (see comptime_ways_constraints).
fn (tc &TypeChecker) vls_narrowed_constraints(fn_node flat.Node, path []flat.NodeId) map[string]GenericConstraint {
	scope := tc.type_param_scope(fn_node)
	mut ways := []ComptimeWay{}
	for i := path.len - 1; i >= 1; i-- {
		node := tc.a.node(path[i])
		if node.kind != .comptime_if || node.children_count == 0 {
			continue
		}
		taken := tc.a.child(node, 0) == path[i - 1]
		if !taken && (node.children_count < 2 || tc.a.child(node, 1) != path[i - 1]) {
			continue
		}
		mut tested := tc.comptime_tested_params(node.value, fn_node, scope.names, false)
		// A local that holds a value of a type parameter, `y := x`: `$if y is
		// User {` tests that type parameter.
		for tested_name in comptime_condition_tested_names(node.value) {
			if tested_name in tested || tested_name in scope.names {
				continue
			}
			if param := tc.vls_local_type_param(fn_node, tested_name, scope.names) {
				tested[tested_name] = param
			}
		}
		ways << ComptimeWay{
			cond:  comptime_condition_on_type_params(node.value, tested)
			taken: taken
		}
	}
	return tc.comptime_ways_constraints(ways, scope.constraints, scope.names)
}

// vls_local_type_param is the type parameter, one of `names`, that the local
// `name` of the generic function `fn_node` holds a value of: `T` for `y` of
// `y := x` with `x T`.
fn (tc &TypeChecker) vls_local_type_param(fn_node flat.Node, name string, names []string) ?string {
	for id in tc.vls_idents_named(fn_node, name) {
		if typ := tc.vls_local_type(id) {
			clean := unwrap_pointer(typ)
			if clean is Unknown {
				if param := generic_placeholder_from_unknown(clean) {
					if param in names {
						return param
					}
				}
			}
		}
	}
	return none
}

// vls_local_named_type is the type of the local `name` of the function
// `fn_node` where it is declared, `y := x` or `for y in ys {`: what its value
// or its container gives it, not what an `is` makes of it somewhere.
fn (tc &TypeChecker) vls_local_named_type(fn_node flat.Node, name string) ?Type {
	for id in tc.vls_idents_named(fn_node, name) {
		parent_id := tc.vls_parent_id(id)
		if !tc.valid_node_id(parent_id) {
			continue
		}
		parent := tc.a.node(parent_id)
		if (parent.kind == .decl_assign && id in tc.multi_assign_lhs_ids(parent))
			|| parent.kind == .for_in_stmt {
			if typ := tc.vls_local_type(id) {
				return typ
			}
		}
	}
	return none
}

// vls_idents_named returns the idents `name` of the function `fn_node`. V has
// no shadowing: they all stand for one binding.
fn (tc &TypeChecker) vls_idents_named(fn_node flat.Node, name string) []flat.NodeId {
	mut found := []flat.NodeId{}
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		stack << tc.a.child(&fn_node, i)
	}
	for stack.len > 0 {
		id := stack.pop()
		if !tc.valid_node_id(id) {
			continue
		}
		node := tc.a.node(id)
		if node.kind == .ident && node.value == name {
			found << id
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return found
}

// vls_enclosing_decl returns the declaration around the node `id` that type
// parameters can belong to, a function or a type, and the path from `id` up to
// it: each node a child of the next, without the declaration.
fn (tc &TypeChecker) vls_enclosing_decl(id flat.NodeId) ?(flat.Node, []flat.NodeId) {
	first := tc.a.node(id)
	if first.kind in [.fn_decl, .struct_decl, .interface_decl, .type_decl] {
		return *first, []flat.NodeId{}
	}
	mut path := [id]
	mut cur := id
	for _ in 0 .. 4096 {
		cur = tc.vls_parent_id(cur)
		if !tc.valid_node_id(cur) {
			return none
		}
		node := tc.a.node(cur)
		if node.kind in [.fn_decl, .struct_decl, .interface_decl, .type_decl] {
			return *node, path
		}
		path << cur
	}
	return none
}

// vls_constraint_known reports whether `constraint`, of a type parameter of
// `decl`, names a type that exists: of `[T Nope]`, which the check reports,
// a hover says nothing.
fn (tc &TypeChecker) vls_constraint_known(decl flat.Node, constraint GenericConstraint) bool {
	// A type parameter without a constraint, which a `$if` decided (see
	// comptime_ways_constraints).
	if constraint.name == '' {
		return true
	}
	file := tc.a.source_files[int(decl.pos.id)] or { return false }
	decl_module := tc.file_modules[file.name] or { tc.cur_module }
	return tc.type_name_known_in_scope(constraint.name.all_before('['), file.name, decl_module)
}

// vls_constraint_single_type is the one type that `constraint` leaves, when it
// stands for that type alone: `f64` in the branch of `$if T is f64 {`, but not
// the struct of `[T User]`, which the structs that embed it stand with.
fn (tc &TypeChecker) vls_constraint_single_type(constraint GenericConstraint) ?Type {
	if constraint.is_interface || constraint.types.len != 1
		|| tc.is_constraint_family(constraint, constraint.types[0]) {
		return none
	}
	return constraint.types[0]
}

// vls_type_param_line says what the type parameter `param` can be with
// `constraint`, for a hover: `T: int | i64` for a set, `T: implements
// main.Named` for an interface.
fn (tc &TypeChecker) vls_type_param_line(param string, constraint GenericConstraint) ?string {
	if constraint.is_interface {
		return '${param}: implements ${tc.vls_type_text(Type(constraint.iface))}'
	}
	mut texts := []string{cap: constraint.types.len}
	for t in constraint.types {
		text := tc.vls_type_text(t)
		texts << if t is Interface {
			'implements ${text}'
		} else if tc.is_family_struct(constraint, t) {
			'${text} or a struct that embeds it'
		} else {
			text
		}
	}
	if texts.len == 0 {
		return none
	}
	return '${param}: ${texts.join(' | ')}'
}

// vls_generic_value_text writes, for a hover, the value `name` of the type
// `type_text`, which names type parameters of the declaration around `id`: one
// that is one type there, as in the branch of `$if T is f64 {`, is that type,
// `x f64`; any other stays, with what it can be there on a line of its own,
// `T: int | i64` or `T: implements main.Named`.
fn (tc &TypeChecker) vls_generic_value_text(id flat.NodeId, name string, type_text string) string {
	decl, path := tc.vls_enclosing_decl(id) or { return '${name} ${type_text}' }
	scope := tc.type_param_scope(decl)
	constraints := tc.vls_narrowed_constraints(decl, path)
	mut names := []string{}
	mut args := []string{}
	mut lines := []string{}
	for param in scope.names {
		mut one := map[string]bool{}
		one[param] = true
		if !type_text_names_any(type_text, one) {
			continue
		}
		constraint := constraints[param] or { continue }
		if !tc.vls_constraint_known(decl, constraint) {
			continue
		}
		if single := tc.vls_constraint_single_type(constraint) {
			names << param
			args << tc.vls_type_text(single)
		} else if line := tc.vls_type_param_line(param, constraint) {
			lines << line
		}
	}
	mut text := if names.len > 0 {
		'${name} ${subst_generic_text(type_text, args, names)}'
	} else {
		'${name} ${type_text}'
	}
	for line in lines {
		text += '\n${line}'
	}
	return text
}

// vls_value_hover is the hover of the value `name` of the type `typ` at `id`:
// `name type`, with what its type parameters are there (see
// vls_generic_value_text).
fn (tc &TypeChecker) vls_value_hover(id flat.NodeId, name string, typ Type) string {
	text := tc.vls_value_type_text(typ)
	// `[]T`, or a generic type with a type parameter as its argument, `Box[T]`.
	if type_contains_unknown(typ) || !vls_type_text_params_hold(text, map[string]bool{}) {
		return tc.vls_generic_value_text(id, name, text)
	}
	return '${name} ${text}'
}

// vls_type_param_hover is the hover of `word` where it names a type parameter
// of the declaration around the node `id`: the type parameter as declared,
// `[T Number]`, and what it can be there, `T: int | f64`, or `T: f64` in the
// branch of `$if T is f64 {`.
fn (tc &TypeChecker) vls_type_param_hover(id flat.NodeId, word string) ?string {
	decl, path := tc.vls_enclosing_decl(id)?
	scope := tc.type_param_scope(decl)
	if word !in scope.names {
		return none
	}
	declared := scope.constraints[word] or {
		// Without a constraint: what a `$if` makes of it there, if one does.
		narrowed := tc.vls_narrowed_constraints(decl, path)[word] or { return '[${word}]' }
		line := tc.vls_type_param_line(word, narrowed) or { return '[${word}]' }
		return '[${word}]\n${line}'
	}
	head := '[${word} ${declared.name}]'
	if !tc.vls_constraint_known(decl, declared) {
		return head
	}
	constraint := tc.vls_narrowed_constraints(decl, path)[word] or { return head }
	line := tc.vls_type_param_line(word, constraint) or { return head }
	return '${head}\n${line}'
}

// vls_smartcast_type is the type that the checker gave the use `id` of a
// parameter declared as `declared`, a sum type or an interface, where `is` or
// `match` makes it one of its types: `Circle` in the branch of
// `if s is Circle {`.
fn (tc &TypeChecker) vls_smartcast_type(id flat.NodeId, declared Type) ?Type {
	base := unalias_type(unwrap_pointer(declared))
	if base !is SumType && base !is Interface {
		return none
	}
	typ := tc.expr_type(id)?
	narrowed := unwrap_pointer(typ)
	if narrowed is Unknown || narrowed is Void || narrowed.name() == base.name()
		|| narrowed.name() == unwrap_pointer(declared).name() {
		return none
	}
	return typ
}

// vls_local_type is the type of the variable the ident `id` declares: the one
// the checker gave it, or for a variable of `if x := f() {`, which it keeps no
// type for, what `f` returns without its option or result.
fn (tc &TypeChecker) vls_local_type(id flat.NodeId) ?Type {
	if typ := tc.expr_type(id) {
		return typ
	}
	if typ := tc.vls_body_types[int(id)] {
		return typ
	}
	decl_id := tc.vls_parent_id(id)
	if !tc.valid_node_id(decl_id) {
		return none
	}
	decl := tc.a.node(decl_id)
	// A variable of a `for ... in` loop: what its container holds.
	if decl.kind == .for_in_stmt {
		return tc.vls_loop_variable_type(*decl, id)
	}
	if decl.kind != .decl_assign {
		return none
	}
	lhs_ids := tc.multi_assign_lhs_ids(decl)
	i := lhs_ids.index(id)
	if i < 0 {
		return none
	}
	rhs_count := tc.multi_assign_rhs_count(decl)
	if_id := tc.vls_parent_id(decl_id)
	if rhs_count == 1 && tc.valid_node_id(if_id) && tc.a.node(if_id).kind == .if_expr
		&& tc.a.child(tc.a.node(if_id), 0) == decl_id {
		rhs_id := tc.multi_assign_rhs_id(decl, 0)
		value := tc.vls_unconstrained_type(rhs_id) or { tc.vls_expr_type(rhs_id)? }
		base := match value {
			OptionType { value.base_type }
			ResultType { value.base_type }
			else { value }
		}
		if base is MultiReturn {
			return if i < base.types.len { base.types[i] } else { none }
		}
		return if lhs_ids.len == 1 { base } else { none }
	}
	// A local of a body the checker did not type, as the body of a generic
	// function: the type of its value, `[]int` for `xs.map(it.name.len)`, with a
	// type parameter kept as one, `T` for `x` of `x T`.
	if rhs_count == lhs_ids.len {
		value_id := tc.multi_assign_rhs_id(decl, i)
		value := tc.a.node(value_id)
		// A function literal: the type its signature writes, `fn (T) int`.
		if value.kind == .fn_literal {
			return tc.vls_fn_literal_type(value)
		}
		if typ := tc.vls_unconstrained_type(value_id) {
			return typ
		}
	} else if rhs_count == 1 {
		// `x, y := f()`: the values of a call that returns several.
		if values := tc.vls_unconstrained_type(tc.multi_assign_rhs_id(decl, 0)) {
			held := unalias_type(values)
			if held is MultiReturn && i < held.types.len {
				return held.types[i]
			}
		}
	}
	// What the check of a generic body with its type parameters open gave the
	// variable (see vls_type_generic_body).
	if typ := tc.vls_open_types[int(id)] {
		return typ
	}
	return none
}

// vls_loop_variable_type is the type of the variable `id` of the `for ... in`
// loop `loop`, which the checker did not type: an index or a key, or what the
// container holds, `T` for an element of a `[]T`.
fn (tc &TypeChecker) vls_loop_variable_type(loop flat.Node, id flat.NodeId) ?Type {
	header := loop.value.int()
	if header < 3 || loop.children_count < 3 {
		return none
	}
	key_id := tc.a.child(&loop, 0)
	val_id := tc.a.child(&loop, 1)
	container_id := tc.a.child(&loop, 2)
	if id != key_id && id != val_id {
		return none
	}
	// `for i in 0 .. n`
	if header == 4 || tc.a.node(container_id).kind == .range {
		return if id == key_id { Type(int_) } else { none }
	}
	container := unalias_type(unwrap_pointer(tc.vls_unconstrained_type(container_id)?))
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
			return none
		}
	}
	// `for x in c` names one variable, `for k, x in c` two: as the check of the
	// body binds them (see bind_constraint_walk_loop).
	if int(val_id) < 0 {
		return if container is Map { key } else { value }
	}
	return if id == key_id { key } else { value }
}

// vls_unconstrained_type is the type of the value `id` as the code writes it,
// with a type parameter kept as one: `T` for `x` of `x T`, which vls_expr_type
// gives as the constraint of `T`, whose members it has.
fn (tc &TypeChecker) vls_unconstrained_type(id flat.NodeId) ?Type {
	// What no type parameter decides has the type that every instance gives it.
	if typ := tc.vls_body_types[int(id)] {
		return typ
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		if binding := tc.vls_local_binding(id) {
			if binding.implicit {
				return tc.vls_implicit_type(binding)
			}
			decl := tc.a.node(binding.decl_id)
			if decl.kind == .param && decl.typ.len > 0 {
				return tc.parse_type(decl.typ)
			}
			if elem := tc.vls_lambda_param_type(binding.decl_id) {
				return elem
			}
			return tc.vls_local_type(binding.decl_id)
		}
	}
	if node.kind == .index {
		return tc.vls_element_type(*node)
	}
	// `T(0)`: a value of the type it casts to, a type parameter kept as one; a
	// literal that writes its type, `[]T{}` or `map[string]T{}`, of that type.
	if node.kind == .cast_expr && node.value.len > 0 {
		return tc.parse_type(node.value)
	}
	if node.kind in [.array_init, .map_init] && node.typ.len > 0 {
		return tc.parse_type(node.typ)
	}
	// A generic call returns what its arguments bind: `identity(a)` of `a A` is
	// an `A`, not the `T` that `identity` declares.
	if node.kind == .call {
		if typ := tc.vls_generic_call_type(id, *node) {
			return typ
		}
	}
	// `f() or { ... }`: what the option holds.
	if node.kind == .or_expr && node.children_count > 0 {
		held := unalias_type(tc.vls_unconstrained_type(tc.a.child(node, 0))?)
		return match held {
			OptionType { held.base_type }
			ResultType { held.base_type }
			else { held }
		}
	}
	// What the check of a generic body with its type parameters open gives it
	// (see vls_type_generic_body).
	if typ := tc.vls_open_types[int(id)] {
		return typ
	}
	if typ := tc.vls_value_type(id) {
		return typ
	}
	return tc.vls_operation_type(*node)
}

// vls_operation_type is the type of `node`, an operation that the checker kept
// no type for, as in a generic body: a comparison, a logical operation, `in`
// and `is` are a `bool`; `-x`, `(x)` and an operation on numbers have the type
// of their operands, the first one that is not a literal, and a shift the type
// of what it shifts.
fn (tc &TypeChecker) vls_operation_type(node flat.Node) ?Type {
	match node.kind {
		.in_expr, .is_expr {
			return Type(bool_)
		}
		.paren {
			if node.children_count == 1 {
				return tc.vls_unconstrained_type(tc.a.child(&node, 0))
			}
		}
		.prefix {
			if node.op == .not {
				return Type(bool_)
			}
			if node.op in [.minus, .plus, .bit_not] && node.children_count == 1 {
				return tc.vls_unconstrained_type(tc.a.child(&node, 0))
			}
		}
		.infix {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return Type(bool_)
			}
			if node.children_count != 2 {
				return none
			}
			left := tc.a.child(&node, 0)
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
				return tc.vls_unconstrained_type(left)
			}
			if node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .power] {
				if tc.a.node(left).kind !in [.int_literal, .float_literal] {
					if typ := tc.vls_unconstrained_type(left) {
						return typ
					}
				}
				return tc.vls_unconstrained_type(tc.a.child(&node, 1))
			}
		}
		else {}
	}
	return none
}

// vls_element_type is the type of `node`, an index, where the checker kept
// none: an element of an array or a map, a byte of a string, or for a slice,
// `xs[1..]`, the container itself.
fn (tc &TypeChecker) vls_element_type(node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	container := tc.vls_unconstrained_type(tc.a.child(&node, 0))?
	if node.value == 'range' {
		return container
	}
	base := unalias_type(unwrap_pointer(container))
	return match base {
		Array { base.elem_type }
		ArrayFixed { base.elem_type }
		Map { base.value_type }
		String { Type(u8_) }
		else { none }
	}
}

// vls_fn_literal_type is the type of the function literal `literal` as its
// signature writes it.
fn (tc &TypeChecker) vls_fn_literal_type(literal flat.Node) Type {
	mut params := []Type{}
	mut params_mut := []bool{}
	mut is_variadic := false
	for i in 0 .. literal.children_count {
		param := tc.a.child_node(&literal, i)
		if param.kind != .param {
			continue
		}
		params << tc.parse_type(param.typ)
		is_variadic = param.typ.trim_space().starts_with('...')
		params_mut << param.is_mut
	}
	return Type(FnType{
		params:      params
		params_mut:  params_mut
		is_variadic: is_variadic
		return_type: if literal.typ in ['', 'void'] {
			Type(void_)
		} else {
			tc.parse_type(literal.typ)
		}
	})
}

// vls_builtin_method_decl finds the declaration of a method of builtin's
// `array`, `string` or `map` by its name, `array.map`: the checker keeps no
// signature for the methods it handles itself.
fn (tc &TypeChecker) vls_builtin_method_decl(method string) ?flat.NodeId {
	// Only builtin declares methods of these types.
	for index in tc.top_level_idx {
		node := tc.a.nodes[index]
		// A method builtin declares without a body is a `c_fn_decl`.
		if node.kind in [.fn_decl, .c_fn_decl] && node.value == method {
			return flat.NodeId(index)
		}
	}
	return none
}

// vls_called_by returns the call whose callee is the node `id`: the name in
// `name(...)`, the member in `recv.name(...)`, or the name of a generic
// function called with its type arguments, `name[T](...)`.
fn (tc &TypeChecker) vls_called_by(id flat.NodeId) ?flat.NodeId {
	parent_id := tc.vls_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return none
	}
	parent := tc.a.node(parent_id)
	if parent.kind == .call && parent.children_count > 0 && tc.a.child(parent, 0) == id {
		return parent_id
	}
	if parent.kind == .index && parent.children_count > 0 && tc.a.child(parent, 0) == id {
		return tc.vls_called_by(parent_id)
	}
	return none
}

fn (mut tc TypeChecker) vls_hover_ident(id flat.NodeId, node flat.Node) string {
	name := node.value
	if module_name := tc.vls_import_symbol_module(id) {
		return tc.vls_module_member_declaration(module_name, name) or { '' }
	}
	if decl_id := tc.vls_local_declaration(id) {
		decl := tc.a.node(decl_id)
		// A parameter keeps the type it was declared with: `mut` makes a
		// receiver a reference.
		if decl.kind == .param && decl.typ.len > 0 {
			declared := tc.parse_type(decl.typ)
			if type_contains_unknown(declared) {
				// A generic parameter, `items []T`, and what `T` is there.
				return tc.vls_generic_value_text(id, name, decl.typ)
			}
			// `if s is Circle {` and a branch of `match s {` make it one of its
			// types there, as they make a local.
			if narrowed := tc.vls_smartcast_type(id, declared) {
				return '${name} ${tc.vls_type_text(narrowed)}'
			}
			return '${name} ${tc.vls_type_text(declared)}'
		}
		// A variable of no type the checker knows, as the value of a call of a
		// function that does not exist, has nothing to show.
		if typ := tc.expr_type(id) {
			return if vls_holds_a_value(typ) { tc.vls_value_hover(id, name, typ) } else { '' }
		}
		if typ := tc.vls_local_type(decl_id) {
			return if vls_holds_a_value(typ) { tc.vls_value_hover(id, name, typ) } else { '' }
		}
		// A parameter of a lambda that the checker did not type, in the body
		// of a generic function.
		if typ := tc.vls_lambda_param_type(decl_id) {
			return tc.vls_value_hover(id, name, typ)
		}
	}
	if typ := tc.vls_const_type(name) {
		return 'const ${name.all_after_last('.')} ${tc.vls_type_text(typ)}'
	}
	if tc.vls_is_global(name) {
		typ := tc.expr_type(id) or { return '' }
		return '__global ${name.all_after_last('.')} ${tc.vls_type_text(typ)}'
	}
	// A variable where it is declared, `if v := f() {` too.
	if typ := tc.vls_local_type(id) {
		return if vls_holds_a_value(typ) { tc.vls_value_hover(id, name, typ) } else { '' }
	}
	// `it`, `a`, `b` or `err` that the checker did not type, in the body of a
	// generic function.
	if binding := tc.vls_local_binding(id) {
		if binding.implicit {
			if typ := tc.vls_implicit_type(binding) {
				return tc.vls_value_hover(id, name, typ)
			}
		}
	}
	if declaration := tc.vls_type_declaration(name) {
		return declaration
	}
	// A module name before one of its members stands for that member, as
	// V1's `module.Type` was one node.
	if member := tc.vls_module_receiver_member(id) {
		return tc.vls_module_member_declaration(name, member) or { '' }
	}
	return ''
}

fn (mut tc TypeChecker) vls_hover_selector(node flat.Node) string {
	if node.children_count == 0 {
		return ''
	}
	receiver_id := tc.a.child(&node, 0)
	receiver := tc.a.node(receiver_id)
	// `Enum.value`, also `module.Enum.value`
	if enum_name := tc.vls_enum_receiver(receiver) {
		return tc.vls_enum_value_hover(enum_name, node.value)
	}
	// `module.member`: a const, a function or a type of that module.
	if receiver.kind == .ident && tc.expr_type(receiver_id) == none {
		if declaration := tc.vls_module_member_declaration(receiver.value, node.value) {
			return declaration
		}
	}
	receiver_type := tc.vls_expr_type(receiver_id) or { return '' }
	if field_type := tc.vls_field_type(receiver_type, node.value) {
		return '${node.value} ${tc.vls_type_text(field_type)}'
	}
	// A method named without a call, `label_of := b.label`: as a call of it.
	target := tc.vls_method_target(receiver_id, node.value) or { return '' }
	return tc.vls_fn_signature(target) or { '' }
}

// vls_enum_receiver returns the enum a selector's receiver names: `Color`, or
// `models.Role` written as `models.Role`.
fn (tc &TypeChecker) vls_enum_receiver(receiver &flat.Node) ?string {
	if receiver.kind == .ident && tc.vls_is_enum(receiver.value) {
		return receiver.value
	}
	if receiver.kind == .selector && receiver.children_count > 0 {
		module_node := tc.a.child_node(receiver, 0)
		if module_node.kind == .ident {
			qualified := '${module_node.value}.${receiver.value}'
			if qualified in tc.enum_names {
				return qualified
			}
		}
	}
	return none
}

// vls_module_member_declaration describes `module.member`: `const name type`,
// a function's signature, or a type's declaration.
fn (tc &TypeChecker) vls_module_member_declaration(module_name string, member string) ?string {
	qualified := tc.vls_member_name(module_name, member)
	if typ := tc.vls_const_type(qualified) {
		return 'const ${member} ${tc.vls_type_text(typ)}'
	}
	if signature := tc.vls_fn_signature(qualified) {
		return signature
	}
	return tc.vls_type_declaration(qualified)
}

// vls_member_name is `module.member` for a member of the module an import
// alias names: `s.x` stands for `v.tests.vls.sample_mod1.x`.
fn (tc &TypeChecker) vls_member_name(module_name string, member string) string {
	full := tc.imports[module_name] or { module_name }
	return '${full}.${member}'
}

// vls_import_symbol_module is the module an import takes the identifier `id`
// from, `mod` for `Name` in `import mod { Name }`.
fn (tc &TypeChecker) vls_import_symbol_module(id flat.NodeId) ?string {
	parent_id := tc.vls_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return none
	}
	parent := tc.a.node(parent_id)
	if parent.kind != .import_decl {
		return none
	}
	return parent.value
}

// vls_module_receiver_member returns the member after a module name that is
// the receiver of a selector: `Notifier` for `services` in `services.Notifier`.
fn (tc &TypeChecker) vls_module_receiver_member(id flat.NodeId) ?string {
	parent_id := tc.vls_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return none
	}
	parent := tc.a.node(parent_id)
	if parent.kind != .selector || parent.children_count == 0 || tc.a.child(parent, 0) != id {
		return none
	}
	return parent.value
}

// vls_field_type is the type of the field `name` of the struct or interface
// behind `typ`, through pointers and aliases.
fn (tc &TypeChecker) vls_field_type(typ Type, name string) ?Type {
	type_name := tc.vls_member_owner(typ) or { return none }
	if declared := tc.vls_declared_field_type(type_name, name) {
		return declared
	}
	// A generic type applied to type arguments, `Box[T]` or `Box[User]`: the
	// field of `Box`, with its type parameters bound to them. A type parameter
	// given stays one, for its constraint.
	base, args, is_generic := generic_type_application_parts(type_name)
	if !is_generic {
		return none
	}
	declared := tc.vls_declared_field_type(base, name)?
	decl := tc.generic_type_decl(base) or { return declared }
	params := decl.generic_params().map(it.trim_space())
	if params.len != args.len {
		return declared
	}
	mut values := []Type{cap: args.len}
	for arg in args {
		values << if is_bare_generic_param(arg) {
			unknown_type('generic placeholder `${arg}`')
		} else {
			tc.parse_type(arg)
		}
	}
	return tc.substitute_generic_type_values(declared, values, params)
}

// vls_declared_field_type is the type of the field `name` as the struct or the
// interface `type_name` declares it.
fn (tc &TypeChecker) vls_declared_field_type(type_name string, name string) ?Type {
	fields := tc.structs[type_name] or { tc.interface_fields[type_name] or { return none } }
	for field in fields {
		if field.name == name {
			return field.typ
		}
	}
	return none
}

// vls_member_owner is the struct or interface that declares the members of
// `typ`: its own, or builtin's `array`, `string` and `map` for those types.
fn (tc &TypeChecker) vls_member_owner(typ Type) ?string {
	base := vls_unwrap_type(typ)
	if base is Struct {
		return base.name
	}
	if base is Interface {
		return base.name
	}
	if base is Array || base is ArrayFixed {
		return 'array'
	}
	if base is String {
		return 'string'
	}
	if base is Map {
		return 'map'
	}
	return none
}

// vls_holds_a_value reports whether a variable of the type `typ` holds a
// value: not `void`, nor the empty list of values that the checker gives a call
// of a function it does not know.
fn vls_holds_a_value(typ Type) bool {
	if typ is Void {
		return false
	}
	if typ is MultiReturn {
		return typ.types.len > 0
	}
	return true
}

// vls_unwrap_type strips pointers and aliases, as a selector does.
fn vls_unwrap_type(typ Type) Type {
	mut t := typ
	for _ in 0 .. 16 {
		if t is Pointer {
			t = t.base_type
		} else if t is Alias {
			t = t.base_type
		} else {
			break
		}
	}
	return t
}

fn (tc &TypeChecker) vls_const_type(name string) ?Type {
	if name.contains('.') {
		return tc.const_types[name] or { return none }
	}
	qualified := tc.qualify_name(name)
	return tc.const_types[qualified] or { tc.const_types[name] or { return none } }
}

fn (tc &TypeChecker) vls_is_global(name string) bool {
	return name in tc.global_names || tc.qualify_name(name) in tc.global_names
}

fn (tc &TypeChecker) vls_is_enum(name string) bool {
	return name in tc.enum_names || tc.qualify_name(name) in tc.enum_names
}

// vls_enum_values is the value of each field of the enum `decl`, as its hover
// and its inlay hints write it: `5`, or for a `@[flag]` enum the bit of the
// field, `0b010 (2)`. A field whose value is no constant the checker can
// compute has none, and neither have the fields after it that follow it.
fn (tc &TypeChecker) vls_enum_values(decl flat.Node) map[string]string {
	mut texts := map[string]string{}
	file := tc.a.source_files[decl.pos.id] or { return texts }
	module_name := tc.file_modules[file.name] or { 'main' }
	mut field_names := []string{}
	mut field_exprs := map[string]flat.NodeId{}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .enum_field {
			continue
		}
		field_names << field.value
		if field.children_count > 0 {
			field_exprs[field.value] = tc.a.child(field, 0)
		}
	}
	if decl.typ == 'flag' {
		for i, name in field_names {
			// A u64: the 64th flag does not fit an i64.
			bit := u64(1) << i
			bits := '${bit:b}'
			padding := if field_names.len > bits.len {
				'0'.repeat(field_names.len - bits.len)
			} else {
				''
			}
			texts[name] = '0b${padding}${bits} (${bit})'
		}
		return texts
	}
	mut values := map[string]int{}
	mut next := i64(0)
	mut known := true
	for name in field_names {
		mut value := next
		if expr_id := field_exprs[name] {
			expr := tc.a.node(expr_id)
			mut resolving := map[string]bool{}
			if expr.kind == .int_literal {
				// Also a literal past the range of an `int`.
				value = expr.value.i64()
				known = true
			} else if computed := tc.comptime_static_enum_field_value(expr_id, module_name,
				decl.value, mut values, field_exprs, mut resolving)
			{
				value = computed
				known = true
			} else {
				known = false
			}
		}
		if known {
			texts[name] = value.str()
			values[name] = int(value)
			next = value + 1
		}
	}
	return texts
}

// vls_enum_value_hover describes the value `field` of the enum `enum_name`:
// `Color.green = 5`, `Perm.write = 0b010 (2)`, or `Color.green` alone when its
// value is not known.
fn (tc &TypeChecker) vls_enum_value_hover(enum_name string, field string) string {
	name := '${enum_name.all_after_last('.')}.${field}'
	index := tc.first_type_declaration_ids[enum_name] or {
		tc.first_type_declaration_ids[tc.qualify_name(enum_name)] or { return name }
	}
	decl := tc.a.nodes[index]
	if decl.kind != .enum_decl {
		return name
	}
	value := tc.vls_enum_values(decl)[field] or { return name }
	return '${name} = ${value}'
}

// vls_type_declaration is how V1 describes a type: `struct Name`, `enum Name`,
// `interface Name`, `type Name = Parent` or `type Name = A | B`.
fn (tc &TypeChecker) vls_type_declaration(name string) ?string {
	if name.len == 0 {
		return none
	}
	qualified := tc.qualify_name(name)
	short := name.all_after_last('.')
	for key in [qualified, name] {
		if key in tc.structs {
			return 'struct ${short}'
		}
		if key in tc.enum_names {
			return 'enum ${short}'
		}
		if key in tc.interface_names {
			return 'interface ${short}'
		}
		if variants := tc.sum_types[key] {
			texts := variants.map(tc.vls_named_type_text(it))
			return 'type ${short} = ${texts.join(' | ')}'
		}
		if parent := tc.type_aliases[key] {
			return 'type ${short} = ${tc.vls_type_text(tc.parse_type(parent))}'
		}
	}
	return none
}

// vls_fn_decl_id finds the declaration of the function a call resolved to.
fn (tc &TypeChecker) vls_fn_decl_id(resolved string) ?flat.NodeId {
	file := tc.fn_type_files[resolved] or { return none }
	module_name := tc.fn_type_modules[resolved] or { '' }
	local := if module_name !in ['', 'main', 'builtin'] && resolved.starts_with('${module_name}.') {
		resolved[module_name.len + 1..]
	} else {
		resolved
	}
	for index in tc.top_level_idx {
		node := tc.a.nodes[index]
		if node.kind != .fn_decl || node.value != local {
			continue
		}
		decl_file := tc.a.source_files[node.pos.id] or { continue }
		if decl_file.name == file {
			return flat.NodeId(index)
		}
	}
	return none
}

// VlsSignature is a function's signature in parts: its name, its parameters
// as `name type` and their names alone, and its return type with a leading
// space, or ''.
struct VlsSignature {
	name        string
	type_params string // `[T Named, U]`, or ''
	params      []string
	names       []string
	types       []Type // unknown ones are void
	ret         string
}

// vls_fn_signature is `fn name(param type, ...) return_type` for the function a
// call resolved to, without the receiver of a method, as V1 wrote it.
fn (tc &TypeChecker) vls_fn_signature(resolved string) ?string {
	sig := tc.vls_signature(resolved)?
	return 'fn ${sig.name}${sig.type_params}(${sig.params.join(', ')})${sig.ret}'
}

// vls_signature is the signature of the function a call resolved to.
fn (tc &TypeChecker) vls_signature(resolved string) ?VlsSignature {
	decl_id := tc.vls_fn_decl_id(resolved) or {
		tc.vls_builtin_method_decl(resolved) or {
			return tc.vls_interface_method_signature(resolved)
		}
	}
	decl := tc.a.nodes[int(decl_id)]
	short := decl.value.all_after_last('.').all_after_last('@')
	is_method := decl.value.contains('.') && !decl.value.contains('@static@')
	// A generic function is written as declared: `fn (T) bool`, `[]T`.
	is_generic := (tc.fn_generic_params[resolved] or { []string{} }).len > 0
	checked_types := tc.fn_param_types[resolved] or { []Type{} }
	param_types := if is_generic { []Type{} } else { checked_types }
	mut params := []string{}
	mut names := []string{}
	mut types := []Type{}
	mut param_idx := 0
	for i in 0 .. decl.children_count {
		child := tc.a.child_node(&decl, i)
		if child.kind != .param {
			continue
		}
		if is_method && param_idx == 0 {
			param_idx++
			continue
		}
		params << tc.vls_param_text(child, param_types, param_idx)
		names << child.value
		// The parameters of a generic function that are not generic have
		// their types too.
		types << if param_idx < checked_types.len
			&& !type_contains_unknown(checked_types[param_idx]) {
			checked_types[param_idx]
		} else {
			Type(void_)
		}
		param_idx++
	}
	ret_text := if ret := tc.fn_ret_types[resolved] {
		if ret is Void {
			''
		} else if is_generic || type_contains_unknown(ret) {
			' ${decl.typ}'
		} else {
			' ${tc.vls_type_text(ret)}'
		}
	} else if decl.typ.len == 0 || decl.typ == 'void' {
		''
	} else {
		' ${decl.typ}'
	}
	// Its own type parameters as declared, with their constraints.
	generic_names := decl.generic_params()
	generic_constraints := decl.generic_constraints()
	mut written := []string{cap: generic_names.len}
	for i, generic_name in generic_names {
		constraint := if i < generic_constraints.len { generic_constraints[i] } else { '' }
		written << if constraint.len > 0 {
			'${generic_name.trim_space()} ${constraint}'
		} else {
			generic_name.trim_space()
		}
	}
	return VlsSignature{
		name:        short
		type_params: if written.len > 0 { '[${written.join(', ')}]' } else { '' }
		params:      params
		names:       names
		types:       types
		ret:         ret_text
	}
}

// vls_param_text is `name type` for the parameter `param`, the one at `idx` of
// its function's checked `types`. A generic parameter is written as declared,
// `[]T`, and a variadic one with its dots, `...string`, where V1 dropped them.
fn (tc &TypeChecker) vls_param_text(param &flat.Node, types []Type, idx int) string {
	mut type_text := param.typ
	if idx < types.len && !type_contains_unknown(types[idx]) {
		typ := types[idx]
		type_text = if param.typ.starts_with('...') && typ is Array {
			'...${tc.vls_type_text(typ.elem_type)}'
		} else {
			tc.vls_type_text(typ)
		}
	}
	// V1 wrote a function type with a space: `fn (T) bool`.
	return '${param.value} ${type_text.replace('fn(', 'fn (')}'
}

// vls_interface_method_signature is the signature of a method an interface
// declares, `Named.greet` or `IError.msg`: its declaration is a member of the
// interface, not a function. A generic interface's is written with the type
// arguments of the value, `fn get() main.User` for `Shelf[User].get`.
fn (tc &TypeChecker) vls_interface_method_signature(resolved string) ?VlsSignature {
	interface_name := resolved.all_before_last('.')
	method := resolved.all_after_last('.')
	if interface_name == resolved {
		return none
	}
	decl := tc.vls_interface_decl(interface_name)?
	_, args, is_generic := generic_type_application_parts(interface_name)
	type_params := decl.generic_params().map(it.trim_space())
	if is_generic && type_params.len == args.len {
		member := tc.vls_interface_member(interface_name, method)?
		type_args := args.map(it.trim_space())
		mut params := []string{}
		mut names := []string{}
		mut types := []Type{}
		for j in 0 .. member.children_count {
			param := tc.a.child_node(&member, j)
			if param.kind != .param {
				continue
			}
			text := tc.vls_written_type_text(subst_generic_text(param.typ, type_args,
				type_params))
			params << '${param.value} ${text.replace('fn(', 'fn (')}'
			names << param.value
			types << Type(void_)
		}
		ret_text := if member.typ.len == 0 || member.typ == 'void' {
			''
		} else {
			' ${tc.vls_written_type_text(subst_generic_text(member.typ, type_args, type_params))}'
		}
		return VlsSignature{
			name:   method
			params: params
			names:  names
			types:  types
			ret:    ret_text
		}
	}
	for i in 0 .. decl.children_count {
		member := tc.a.child_node(&decl, i)
		if member.kind != .interface_field || member.value != method {
			continue
		}
		mut param_types := tc.fn_param_types[resolved] or { []Type{} }
		mut param_count := 0
		for j in 0 .. member.children_count {
			if tc.a.child_node(member, j).kind == .param {
				param_count++
			}
		}
		// The checked types start with the interface's own, as a receiver.
		if param_types.len == param_count + 1 {
			param_types = param_types[1..].clone()
		}
		mut params := []string{}
		mut names := []string{}
		mut types := []Type{}
		mut param_idx := 0
		for j in 0 .. member.children_count {
			param := tc.a.child_node(member, j)
			if param.kind != .param {
				continue
			}
			params << tc.vls_param_text(param, param_types, param_idx)
			names << param.value
			types << if param_idx < param_types.len { param_types[param_idx] } else { Type(void_) }
			param_idx++
		}
		ret_text := if member.typ.len == 0 || member.typ == 'void' {
			''
		} else if ret := tc.fn_ret_types[resolved] {
			if ret is Void { '' } else { ' ${tc.vls_type_text(ret)}' }
		} else {
			' ${member.typ}'
		}
		return VlsSignature{
			name:   method
			params: params
			names:  names
			types:  types
			ret:    ret_text
		}
	}
	return none
}

// vls_written_type_text writes the type `text` as vls_type_text does, or as it
// is written when it names a type parameter, `T` or `[]T`.
fn (tc &TypeChecker) vls_written_type_text(text string) string {
	typ := tc.parse_type(text)
	if type_contains_unknown(typ) {
		return text
	}
	return tc.vls_type_text(typ)
}

// vls_type_text writes a type as V1's hover did: the program's own types with
// their `main.` module, a library's types with theirs, builtin types bare.
fn (tc &TypeChecker) vls_type_text(t Type) string {
	match t {
		Struct {
			return tc.vls_named_type_text(t.name)
		}
		Interface {
			return tc.vls_named_type_text(t.name)
		}
		Enum {
			return tc.vls_named_type_text(t.name)
		}
		SumType {
			return tc.vls_named_type_text(t.name)
		}
		Alias {
			return tc.vls_named_type_text(t.name)
		}
		Pointer {
			if t.base_type is Void {
				return 'voidptr'
			}
			return '&${tc.vls_type_text(t.base_type)}'
		}
		Array {
			return '[]${tc.vls_type_text(t.elem_type)}'
		}
		ArrayFixed {
			len_text := if t.len_expr.len > 0 { t.len_expr } else { t.len.str() }
			return '[${len_text}]${tc.vls_type_text(t.elem_type)}'
		}
		Map {
			return 'map[${tc.vls_type_text(t.key_type)}]${tc.vls_type_text(t.value_type)}'
		}
		OptionType {
			// A function that returns only an error is written `!`, not `!void`.
			if t.base_type is Void {
				return '?'
			}
			return '?${tc.vls_type_text(t.base_type)}'
		}
		ResultType {
			if t.base_type is Void {
				return '!'
			}
			return '!${tc.vls_type_text(t.base_type)}'
		}
		Channel {
			return 'chan ${tc.vls_type_text(t.elem_type)}'
		}
		FnType {
			// V1 wrote a function type with a space: `fn (int) string`.
			if !type_contains_unknown(t) {
				return 'fn ${t.name().all_after('fn')}'
			}
			// One that takes or returns a type parameter, of a function literal
			// in a generic body, names it: `fn (T) int`.
			mut params := []string{cap: t.params.len}
			for i in 0 .. t.params.len {
				param := fn_type_param_type(t, i)
				if fn_type_param_is_mut(t, i) {
					base := if param is Pointer { param.base_type } else { param }
					params << 'mut ${tc.vls_type_text(base)}'
				} else {
					params << tc.vls_type_text(param)
				}
			}
			ret := if t.return_type is Void { '' } else { ' ${tc.vls_type_text(t.return_type)}' }
			return 'fn (${params.join(', ')})${ret}'
		}
		Unknown {
			// A type parameter, in a generic body: by its name, `T`.
			if param := generic_placeholder_from_unknown(t) {
				return param
			}
			return t.name()
		}
		else {
			return t.name()
		}
	}
}

// vls_named_type_text qualifies a type the program declares with `main.`,
// and a library's type with the last part of its module, as code names it
// after `import sync.pool`: `pool.PoolProcessor`.
fn (tc &TypeChecker) vls_named_type_text(name string) string {
	base := name.all_before('[')
	if base.contains('.') {
		module_path := base.all_before_last('.')
		return '${module_path.all_after_last('.')}.${name[module_path.len + 1..]}'
	}
	if name.contains('[') {
		return name
	}
	index := tc.first_type_declaration_ids[name] or { return name }
	node := tc.a.nodes[index]
	file := tc.a.source_files[node.pos.id] or { return name }
	// A file without a `module` line belongs to `main`.
	module_name := tc.file_modules[file.name] or { 'main' }
	return if module_name == 'main' { 'main.${name}' } else { name }
}
