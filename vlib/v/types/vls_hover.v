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
			return tc.vls_type_declaration(node.value) or { '' }
		}
		.param {
			declared := tc.parse_type(node.typ)
			type_text := if type_contains_unknown(declared) {
				node.typ
			} else {
				tc.vls_type_text(declared)
			}
			return '${node.value} ${type_text}'
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
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	// With the type parameters of the receiver: `xs.map()` of `xs []T` calls the
	// `map` of every array.
	receiver_type := tc.vls_value_type(tc.a.child(callee, 0)) or { return none }
	owner := tc.vls_member_owner(receiver_type) or { return none }
	method := '${owner}.${callee.value}'
	if method in tc.fn_type_files || tc.vls_builtin_method_decl(method) != none {
		return method
	}
	// A method of a generic struct, which is registered with its receiver as
	// its declaration writes it: `Box[T].label` for `b.label()` of a `Box[T]`.
	if info := tc.resolve_generic_struct_method(owner, callee.value) {
		return info.name
	}
	// A method of a struct that the receiver's struct embeds.
	owners := tc.embedded_method_candidates(owner, callee.value)
	if owners.len == 1 {
		return '${owners[0]}.${callee.value}'
	}
	return none
}

// vls_expr_type is the type the checker gave the expression `id`, or for a
// call it kept no type for, the return type of the function it calls.
fn (tc &TypeChecker) vls_expr_type(id flat.NodeId) ?Type {
	if typ := tc.expr_type(id) {
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
		receiver_type := tc.vls_expr_type(tc.a.child(node, 0))?
		return tc.vls_field_type(receiver_type, node.value)
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
		decl_id := tc.vls_local_declaration(id) or { return none }
		decl := tc.a.node(decl_id)
		if decl.kind == .param {
			declared := tc.parse_type(decl.typ)
			return if type_contains_unknown(declared) { none } else { declared }
		}
		// A local of a generic body that holds a type parameter, `first :=
		// xs[0]`: vls_value_type keeps it.
		typ := tc.vls_local_type(decl_id)?
		return if type_contains_unknown(typ) { none } else { typ }
	}
	// An element of an array or a map that the checker kept no type for.
	if node.kind == .index {
		elem := tc.vls_element_type(node)?
		return if type_contains_unknown(elem) { none } else { elem }
	}
	return none
}

// vls_value_type is the type of the value `id` with the type parameters it
// holds: `xs` of `fn f[T](xs []T)` is a `[]T`, and `xs[0]` a `T`, which
// vls_expr_type leaves out.
fn (tc &TypeChecker) vls_value_type(id flat.NodeId) ?Type {
	if typ := tc.vls_expr_type(id) {
		return typ
	}
	node := tc.a.node(id)
	if node.kind == .index {
		return tc.vls_element_type(*node)
	}
	if node.kind == .ident {
		decl_id := tc.vls_local_declaration(id)?
		decl := tc.a.node(decl_id)
		if decl.kind == .param && decl.typ.len > 0 {
			return tc.parse_type(decl.typ)
		}
		return tc.vls_local_type(decl_id)
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
			// A lambda gives what its body does, a function value what it returns.
			elem := if arg.kind == .lambda_expr && arg.children_count > 0 {
				tc.vls_value_type(tc.a.child(arg, int(arg.children_count) - 1))?
			} else {
				value := tc.vls_value_type(arg_id)?
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

// vls_local_type is the type of the variable the ident `id` declares: the one
// the checker gave it, or for a variable of `if x := f() {`, which it keeps no
// type for, what `f` returns without its option or result.
fn (tc &TypeChecker) vls_local_type(id flat.NodeId) ?Type {
	if typ := tc.expr_type(id) {
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
		value := tc.vls_expr_type(tc.multi_assign_rhs_id(decl, 0))?
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
	// function: the type of its value, with a type parameter kept as one, `T`
	// for `first := xs[0]` of `xs []T`.
	if rhs_count == lhs_ids.len {
		value_id := tc.multi_assign_rhs_id(decl, i)
		value := tc.a.node(value_id)
		// A function literal: the type its signature writes, `fn (T) int`.
		if value.kind == .fn_literal {
			return tc.vls_fn_literal_type(value)
		}
		return tc.vls_value_type(value_id)
	}
	// `x, y := f()`: the values of a call that returns several.
	if rhs_count == 1 {
		values := unalias_type(tc.vls_value_type(tc.multi_assign_rhs_id(decl, 0))?)
		if values is MultiReturn && i < values.types.len {
			return values.types[i]
		}
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
	container := unalias_type(unwrap_pointer(tc.vls_value_type(container_id)?))
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
	// `for x in c` names the value alone; V ranges a map with a key and a value.
	if int(val_id) < 0 {
		return if container is Map { none } else { value }
	}
	return if id == key_id { key } else { value }
}

// vls_element_type is the type of `node`, an index, where the checker kept
// none: an element of an array or a map, a byte of a string, or for a slice,
// `xs[1..]`, the container itself.
fn (tc &TypeChecker) vls_element_type(node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	container := tc.vls_value_type(tc.a.child(&node, 0))?
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
	for i in 0 .. literal.children_count {
		param := tc.a.child_node(&literal, i)
		if param.kind != .param {
			continue
		}
		params << tc.parse_type(param.typ)
		params_mut << param.is_mut
	}
	return Type(FnType{
		params:      params
		params_mut:  params_mut
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
				// A generic parameter: `items []T`.
				return '${name} ${decl.typ}'
			}
			return '${name} ${tc.vls_type_text(declared)}'
		}
		// A variable of no type the checker knows, as the value of a call of a
		// function that does not exist, has nothing to show.
		if typ := tc.expr_type(id) {
			return if vls_holds_a_value(typ) { '${name} ${tc.vls_type_text(typ)}' } else { '' }
		}
		if typ := tc.vls_local_type(decl_id) {
			return if vls_holds_a_value(typ) { '${name} ${tc.vls_type_text(typ)}' } else { '' }
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
		return if vls_holds_a_value(typ) { '${name} ${tc.vls_type_text(typ)}' } else { '' }
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
	return ''
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
	name   string
	params []string
	names  []string
	types  []Type // unknown ones are void
	ret    string
}

// vls_fn_signature is `fn name(param type, ...) return_type` for the function a
// call resolved to, without the receiver of a method, as V1 wrote it.
fn (tc &TypeChecker) vls_fn_signature(resolved string) ?string {
	sig := tc.vls_signature(resolved)?
	return 'fn ${sig.name}(${sig.params.join(', ')})${sig.ret}'
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
	return VlsSignature{
		name:   short
		params: params
		names:  names
		types:  types
		ret:    ret_text
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
// interface, not a function.
fn (tc &TypeChecker) vls_interface_method_signature(resolved string) ?VlsSignature {
	interface_name := resolved.all_before_last('.')
	method := resolved.all_after_last('.')
	if interface_name == resolved {
		return none
	}
	index := tc.first_type_declaration_ids[interface_name] or { return none }
	decl := tc.a.nodes[index]
	if decl.kind != .interface_decl {
		return none
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
