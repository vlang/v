module types

import v.flat

fn (tc &TypeChecker) ownership_is_scoped_callback(typ Type) bool {
	clean := unalias_type(typ)
	return clean is Struct && tc.ownership_thread_library_type(clean.name_id, 'sync/thread', [
		'sync.thread.ScopedMutCallback',
		'thread.ScopedMutCallback',
	])
}

// Only declarations in sync.thread can supply the worker and boxing guarantees.
fn (tc &TypeChecker) ownership_scoped_callback_function(name string, member string) bool {
	if name != 'sync.thread.${member}' && name != 'thread.${member}' {
		return false
	}
	module_name := tc.fn_type_modules[name] or { return false }
	decl := tc.visible_mutation_fn_decl(name, module_name) or { return false }
	return tc.ownership_thread_library_declaration(*tc.a.node(flat.NodeId(decl.idx)),
		'sync/thread')
}

fn (tc &TypeChecker) ownership_scoped_callback_call_is(id flat.NodeId, member string) bool {
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	mut callee := tc.a.child_node(call, 0)
	if callee.kind == .index && callee.children_count > 1 && callee.value != 'range' {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind == .ident && tc.name_has_local_binding(callee.value) {
		return false
	}
	name := tc.generic_call_base_name(*callee) or { return false }
	return tc.ownership_scoped_callback_function(name, member)
}

fn (tc &TypeChecker) ownership_scoped_callback_origin(id flat.NodeId, expected Type) ?flat.NodeId {
	value_id := tc.ownership_unwrap_expr(id)
	value := tc.a.node(value_id)
	if value.kind != .ident {
		return value_id
	}
	call := tc.ownership_scoped_thread_use_parent(value_id)
	spawn_id := tc.ownership_scoped_thread_use_parent(call)
	mut stmt_id := tc.ownership_scoped_thread_use_parent(spawn_id)
	parent_id := tc.direct_parent_id(stmt_id)
	if tc.a.node(parent_id).kind == .expr_stmt {
		stmt_id = parent_id
	}
	scope := tc.a.node(tc.direct_parent_id(stmt_id))
	for i in 1 .. scope.children_count {
		if tc.a.child(scope, i) != stmt_id {
			continue
		}
		for previous := i - 1; previous >= 0; previous-- {
			decl := tc.a.child_node(scope, previous)
			if decl.kind != .decl_assign || decl.children_count != 2 {
				continue
			}
			binding_id := tc.a.child(decl, 0)
			binding := tc.a.node(binding_id)
			if binding.kind != .ident || binding.value != value.value {
				continue
			}
			if previous + 1 < i {
				// The box may be used before spawning if those uses borrow
				// it without replacing it or exposing its payload.
				fn_id := tc.diagnostic_enclosing_fn(stmt_id)
				if !tc.valid_node_id(fn_id) || isnil(tc.ownership) {
					return none
				}
				setup := ScopedCallbackWorkerScope{
					decl:        fn_id
					name:        tc.ownership.cur_fn
					module_name: tc.cur_module
					callback:    expected
				}
				mut helpers := map[u64]bool{}
				for j in previous + 1 .. i {
					if !tc.ownership_scoped_callback_worker_uses(tc.a.child(scope, j), setup,
						ScopedCallbackWorkerBinding{ param: binding_id }, false, 0, mut helpers) {
						return none
					}
				}
			}
			return tc.ownership_unwrap_expr(tc.a.child(decl, 1))
		}
		return none
	}
	return none
}

// The referent is checked while the box is being built, before its local
// binding exists. The consuming invocation in the next statement checks the
// payload and replaces the box's loan with its own scoped reservation.
fn (tc &TypeChecker) ownership_scoped_callback_binding_reborrows(decl_id flat.NodeId) bool {
	decl := tc.a.node(decl_id)
	if decl.kind != .decl_assign || decl.children_count != 2 || decl.is_mut {
		return false
	}
	binding := tc.a.child_node(decl, 0)
	if binding.kind != .ident || binding.is_mut {
		return false
	}
	constructor_id := tc.ownership_unwrap_expr(tc.a.child(decl, 1))
	if !tc.ownership_scoped_callback_call_is(constructor_id, 'new_scoped_mut_callback') {
		return false
	}
	scope := tc.a.node(tc.direct_parent_id(decl_id))
	for i in 1 .. scope.children_count {
		if tc.a.child(scope, i - 1) != decl_id {
			continue
		}
		stmt := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(scope, i)))
		if stmt.children_count != 2
			|| (stmt.kind != .decl_assign && !(stmt.kind == .infix && stmt.op == .left_shift)) {
			return false
		}
		spawn_id := tc.ownership_unwrap_expr(tc.a.child(stmt, 1))
		spawn_node := tc.a.node(spawn_id)
		if spawn_node.kind != .spawn_expr || spawn_node.children_count != 1 {
			return false
		}
		runner_id := tc.ownership_unwrap_expr(tc.a.child(spawn_node, 0))
		runner := tc.a.node(runner_id)
		if runner.children_count != 3
			|| !tc.ownership_scoped_callback_call_is(runner_id, 'invoke_scoped_mut_callback') {
			return false
		}
		arg := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(runner, 1)))
		return arg.kind == .ident && arg.value == binding.value
			&& (tc.ownership_scoped_thread_join_site(spawn_id) != none
				|| tc.ownership_scoped_thread_group(spawn_id) != none)
	}
	return false
}

// Check both the worker that receives the box and the origin of its payload loans.
fn (tc &TypeChecker) ownership_scoped_callback_borrows(id flat.NodeId, expected Type, info CallInfo, index int) ?[]ScopedThreadBorrow {
	if !semantic_types_equal(unalias_type(tc.resolve_type(id)), unalias_type(expected)) {
		return none
	}
	if !(index == 0 && tc.ownership_scoped_callback_function(info.name, 'invoke_scoped_mut_callback'))
		&& !tc.ownership_scoped_callback_worker(info, index, expected) {
		return none
	}
	origin := tc.ownership_scoped_callback_origin(id, expected) or { return none }
	if !tc.ownership_scoped_callback_call_is(origin, 'new_scoped_mut_callback') {
		return tc.ownership_scoped_callback_factory_borrows(origin)
	}
	constructor := tc.a.node(origin)
	if constructor.children_count != 2 {
		return none
	}
	state_id := tc.call_arg_value(tc.a.child(constructor, 1))
	state_type := tc.resolve_type(state_id)
	// Alias methods can dispatch independently of the methods on the underlying struct.
	if state_type !is Struct {
		return none
	}
	borrows := tc.ownership_scoped_thread_argument_borrows(state_id, state_type, true) or { return none }
	return if tc.ownership_scoped_callback_methods(state_type, borrows, tc.cur_module) {
		borrows
	} else {
		none
	}
}

fn (tc &TypeChecker) ownership_scoped_callback_methods(state_type Struct, borrows []ScopedThreadBorrow, module_name string) bool {
	return tc.ownership_scoped_callback_method(state_type, 'call', borrows, module_name)
		&& (!tc.ownership_type_has_explicit_drop(state_type.name_id)
			|| tc.ownership_scoped_callback_method(state_type, 'drop', borrows, module_name))
}

fn (tc &TypeChecker) ownership_scoped_callback_method(state_type Type, member string, borrows []ScopedThreadBorrow, module_name string) bool {
	mut checked := false
	for name in receiver_method_name_candidates(state_type, member, module_name) {
		if name !in tc.fn_ret_types || !tc.method_can_be_called_on_receiver(state_type, member, name) {
			continue
		}
		method := tc.call_info(name, true)
		receiver := method.params[0] or { return false }
		if unalias_type(receiver) !is Pointer || !tc.call_param_is_mut(method, 0)
			|| !tc.ownership_scoped_thread_parameter_does_not_escape(method, 0, state_type, borrows) {
			return false
		}
		checked = true
	}
	return checked
}

fn (tc &TypeChecker) ownership_scoped_callback_move(id flat.NodeId, expected Type, info CallInfo, index int) bool {
	if !tc.ownership_is_scoped_callback(expected) {
		return false
	}
	call := tc.ownership_scoped_thread_use_parent(tc.ownership_unwrap_expr(id))
	spawn_id := tc.ownership_scoped_thread_use_parent(call)
	return (tc.ownership_scoped_thread_join_site(spawn_id) != none
		|| tc.ownership_scoped_thread_group(spawn_id) != none)
		&& tc.ownership_scoped_callback_borrows(id, expected, info, index) != none
}

// Verify a fresh box from a concrete method, then reserve its receiver until the
// join. The borrows come directly from reference fields of that receiver.
fn (tc &TypeChecker) ownership_scoped_callback_factory_borrows(id flat.NodeId) ?[]ScopedThreadBorrow {
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return none
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.children_count != 1 {
		return none
	}
	recv_id := tc.ownership_unwrap_expr(tc.a.child(callee, 0))
	receiver := tc.a.node(recv_id)
	if receiver.kind != .ident {
		return none
	}
	owner := tc.cur_scope.lookup_owner(receiver.value) or { return none }
	if tc.binding_owner_is_global(owner) {
		return none
	}
	receiver_type := unwrap_pointer(tc.resolve_type(recv_id))
	info := tc.resolve_call_info(id, *call) or { return none }
	allow_open_input := receiver_type is Interface
		&& tc.ownership_scoped_factory_bound_input(id, info)
	if int(call.children_count) != info.params.len
		|| !tc.ownership_scoped_factory_signature(info, allow_open_input) {
		return none
	}
	loan := ScopedThreadBorrow{ source: receiver.value, pos: recv_id }
	if receiver_type is Interface {
		if !tc.ownership_scoped_factory_interface_method(receiver_type.name_id, callee.value) {
			return none
		}
		// Conformance has already checked the concrete methods. The receiver
		// pins the referents without granting the worker access to the factory.
		return [ScopedThreadBorrow{ ...loan, access: .factory }]
	}
	if receiver_type !is Struct {
		return none
	}
	formal := unalias_type(info.params[0])
	if !semantic_types_equal(unwrap_pointer(formal), receiver_type) {
		return none
	}
	proof := tc.ownership_scoped_factory_method_proof(info, loan) or { return none }
	return proof.borrows
}

// Record proofs while the original declarations are available. Interface
// queries also run after lowering and after the source arena has been released.
fn (mut tc TypeChecker) ownership_collect_scoped_factory_contracts() {
	mut state := tc.ownership_state()
	for iface, methods in state.interface_lifetime_returns {
		for method, sources in methods {
			if sources.len != 1 || sources[0] != 0 {
				continue
			}
			key := tc.interface_method_signature_key(iface, method) or { continue }
			if tc.ownership_scoped_factory_interface_signature(iface, tc.call_info(key, true)) {
				state.scoped_factory_contracts[key] = true
			}
		}
	}
	if state.scoped_factory_contracts.len == 0 {
		return
	}
	previous_file := tc.cur_file
	previous_module := tc.cur_module
	defer {
		tc.cur_file = previous_file
		tc.cur_module = previous_module
	}
	for name, result in tc.fn_ret_types {
		if !tc.ownership_is_scoped_callback(result) {
			continue
		}
		file := tc.fn_type_files[name] or { continue }
		tc.cur_file = file
		tc.cur_module = tc.file_modules[file] or { tc.fn_type_modules[name] }
		if proof := tc.ownership_scoped_factory_method_proof(tc.call_info(name, true),
			ScopedThreadBorrow{}) {
			state.scoped_factory_implementations[name] = proof.pops
		}
	}
}

fn (tc &TypeChecker) ownership_scoped_factory_interface_method(iface u32, method string) bool {
	if isnil(tc.ownership) || tc.ownership.scoped_factory_contracts.len == 0 {
		return false
	}
	key := tc.interface_method_signature_key(iface, method) or { return false }
	return key in tc.ownership.scoped_factory_contracts
}

fn (tc &TypeChecker) ownership_scoped_factory_interface(iface u32) bool {
	if isnil(tc.ownership) || tc.ownership.scoped_factory_contracts.len == 0 {
		return false
	}
	for method in tc.interface_abstract_method_names(iface) {
		if tc.ownership_scoped_factory_interface_method(iface, method) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_scoped_factory_conforms(name string, contract string) bool {
	if isnil(tc.ownership) {
		return false
	}
	pops := tc.ownership.scoped_factory_implementations[name] or { return false }
	is_mut, is_shared := tc.method_receiver_flags(contract)
	return !pops || (is_mut && !is_shared)
}

fn (tc &TypeChecker) ownership_scoped_factory_interface_call(recv_id flat.NodeId, info CallInfo) bool {
	if isnil(tc.ownership) || tc.ownership.scoped_factory_contracts.len == 0
		|| !info.has_receiver || !tc.ownership_is_scoped_callback(info.return_type) {
		return false
	}
	receiver := unwrap_pointer(tc.resolve_type(recv_id))
	return receiver is Interface
		&& tc.ownership_scoped_factory_interface_method(receiver.name_id, info.name.all_after_last('.'))
}

fn (tc &TypeChecker) ownership_scoped_factory_receiver_use(id flat.NodeId) bool {
	selector_id := tc.ownership_scoped_thread_use_parent(id)
	selector := tc.a.node(selector_id)
	if selector.kind != .selector || selector.children_count != 1
		|| tc.ownership_unwrap_expr(tc.a.child(selector, 0)) != id {
		return false
	}
	call_id := tc.ownership_scoped_thread_use_parent(selector_id)
	call := tc.a.node(call_id)
	if call.kind != .call || call.children_count == 0 || tc.a.child(call, 0) != selector_id {
		return false
	}
	info := tc.resolve_call_info(call_id, *call) or { return false }
	return int(call.children_count) == info.params.len
		&& tc.ownership_scoped_factory_interface_call(id, info)
}

// A directly bound result inherits the verified receiver read. Other loans
// still conflict with the factory's reservation until the worker joins.
fn (tc &TypeChecker) ownership_scoped_factory_result_borrow(source string, pos flat.NodeId) bool {
	if isnil(tc.ownership) || tc.ownership.scoped_factory_contracts.len == 0 {
		return false
	}
	binding := tc.a.node(pos)
	if binding.kind !in [.decl_assign, .assign] || binding.children_count != 2 {
		return false
	}
	call_id := tc.ownership_unwrap_expr(tc.a.child(binding, 1))
	call := tc.a.node(call_id)
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.children_count != 1 {
		return false
	}
	recv_id := tc.ownership_unwrap_expr(tc.a.child(callee, 0))
	recv := tc.a.node(recv_id)
	return recv.kind == .ident && recv.value == source && tc.ownership_scoped_factory_receiver_use(recv_id)
}

struct ScopedFactoryProof {
	borrows []ScopedThreadBorrow
	pops    bool
}

struct ScopedFactoryInput {
	name string
	typ  Type
}

fn (tc &TypeChecker) ownership_scoped_factory_interface_signature(iface u32, info CallInfo) bool {
	// The contract can name a type parameter. Conformance still checks the
	// implementation and each scoped call must establish transferability.
	mut allow_open_input := false
	if info.params.len == 2 {
		input := info.params[1]
		if input is Unknown {
			placeholder := (input as Unknown).placeholder
			params := tc.interface_generic_params[tc.interface_metadata_name(iface)] or { []u32{} }
			allow_open_input = placeholder != 0 && placeholder in params
		}
	}
	return tc.ownership_scoped_factory_signature(info, allow_open_input)
}

fn (tc &TypeChecker) ownership_scoped_factory_bound_input(id flat.NodeId, info CallInfo) bool {
	if info.params.len != 2 {
		return false
	}
	input := info.params[1]
	if input !is Unknown {
		return false
	}
	placeholder := (input as Unknown).placeholder
	if placeholder == 0 {
		return false
	}
	decl := tc.diagnostic_enclosing_fn(id)
	if !tc.valid_node_id(decl) {
		return false
	}
	constraints := tc.generic_constraints_of(*tc.a.node(decl))
	constraint := constraints[placeholder] or { return false }
	return constraint.thread_capability in [.transfer, .share]
}

fn (tc &TypeChecker) ownership_scoped_factory_signature(info CallInfo, allow_open_input bool) bool {
	if !info.has_receiver || info.params.len !in [1, 2] || info.is_variadic || info.is_c_variadic
		|| !tc.ownership_is_scoped_callback(info.return_type) || call_param_is_shared(info, 0) {
		return false
	}
	if info.params.len == 1 {
		return true
	}
	input := info.params[1]
	if tc.call_param_is_mut(info, 1) || call_param_is_shared(info, 1) {
		return false
	}
	if input is Unknown {
		return allow_open_input && (input as Unknown).placeholder != 0
	}
	return !type_contains_unknown(input) && !fn_param_is_voidptr_type(input)
		&& tc.ownership_thread_transfer_problem(input, .transfer, 0, mut map[u64]bool{}) == none
}

fn (tc &TypeChecker) ownership_scoped_factory_method_proof(info CallInfo, loan ScopedThreadBorrow) ?ScopedFactoryProof {
	if !tc.ownership_scoped_factory_signature(info, false) {
		return none
	}
	formal := unalias_type(info.params[0])
	if formal !is Pointer {
		return none
	}
	receiver_type := unwrap_pointer(formal)
	if receiver_type !is Struct {
		return none
	}
	if tc.struct_embed_receiver_names(receiver_type.name_id).len != 0 {
		return none
	}

	file := tc.fn_type_files[info.name] or { return none }
	module_name := tc.fn_type_modules[info.name] or { return none }
	decl := tc.visible_mutation_fn_decl(info.name, module_name) or { return none }
	method := tc.a.node(flat.NodeId(decl.idx))
	if file.ends_with('.vh') || method.kind != .fn_decl
		|| int(method.children_count) != info.params.len + 1
		|| tc.ownership_fn_decl_file(*method) != file {
		return none
	}
	param := tc.a.child_node(method, 0)
	body := tc.a.child_node(method, info.params.len)
	if param.kind != .param || body.kind != .return_stmt || body.children_count != 1 {
		return none
	}
	mut input := ?ScopedFactoryInput(none)
	mut input_name := ''
	if info.params.len == 2 {
		input_param := tc.a.child_node(method, 1)
		if input_param.kind != .param || input_param.value == '' {
			return none
		}
		input_name = input_param.value
		input = ScopedFactoryInput{ name: input_name, typ: info.params[1] }
	}
	state_id, state_type := tc.ownership_scoped_callback_factory_state(tc.a.child(body, 0),
		param.value, input_name, file, module_name) or { return none }
	if tc.struct_embed_receiver_names(state_type.name_id).len != 0
		|| receiver_method_name_candidates(state_type, 'free', module_name).any(it in tc.fn_ret_types) {
		return none
	}
	proof := tc.ownership_scoped_callback_factory_fields(*tc.a.node(state_id), state_type,
		receiver_type, param.value, module_name, input, loan) or { return none }
	if proof.pops && !tc.call_param_is_mut(info, 0) {
		return none
	}
	return if tc.ownership_scoped_callback_methods(state_type, proof.borrows, module_name) {
		proof
	} else {
		none
	}
}

fn (tc &TypeChecker) ownership_scoped_callback_factory_state(id flat.NodeId, receiver string, input string, file string, module_name string) ?(flat.NodeId, Struct) {
	constructor := tc.a.node(tc.ownership_unwrap_expr(id))
	if constructor.kind != .call || constructor.children_count != 2 {
		return none
	}
	index := tc.a.child_node(constructor, 0)
	args := tc.generic_call_type_arg_names(*index)
	if args.len != 3 {
		return none
	}
	callee := tc.a.child_node(index, 0)
	if callee.kind != .selector || callee.value != 'new_scoped_mut_callback' || callee.children_count != 1 {
		return none
	}
	base := tc.a.child_node(callee, 0)
	if base.kind != .ident || base.value == receiver || base.value == input {
		return none
	}
	imports := tc.file_imports_by_file[file] or { return none }
	imported_module := imports.imports[base.value] or { return none }
	if !tc.ownership_scoped_callback_function('${imported_module}.${callee.value}', callee.value) {
		return none
	}
	state_id := tc.ownership_unwrap_expr(tc.call_arg_value(tc.a.child(constructor, 1)))
	state := tc.a.node(state_id)
	if state.kind != .struct_init || state.source_value_id() != args[2]
		|| flat.source_type_shape(args[2]).has(.dotted) || flat.source_type_syntax(args[2]).generic_app_args().len != 0 {
		return none
	}
	state_type := tc.parse_type(tc.qualify_type_name_at(args[2], state_id, module_name))
	if state_type is Struct {
		return state_id, state_type
	}
	return none
}

fn (tc &TypeChecker) ownership_scoped_callback_factory_fields(state flat.Node, state_type Struct, receiver_type Struct, param string, module_name string, input ?ScopedFactoryInput, loan ScopedThreadBorrow) ?ScopedFactoryProof {
	mut borrows := []ScopedThreadBorrow{}
	mut pool_elements := []Type{}
	mut initialized := 0
	mut moves_input := false
	for field in tc.struct_fields_for_type(state_type.name_id) {
		if type_contains_unknown(field.typ) || receiver_method_name_candidates(state_type, field.name, module_name).any(it in tc.fn_ret_types) {
			return none
		}
		field_type := unalias_type(field.typ)
		value_id := tc.ownership_scoped_thread_field_initializer(&state, field.name) or {
			if field_type is Pointer || field.has_default
				|| tc.ownership_thread_transfer_problem(field_type, .transfer, 0, mut map[u64]bool{}) != none {
				return none
			}
			continue
		}
		initialized++
		value := tc.a.node(tc.ownership_unwrap_expr(value_id))
		if field_type !is Pointer {
			if tc.ownership_thread_transfer_problem(field_type, .transfer, 0, mut map[u64]bool{}) != none {
				return none
			}
			if owned := input {
				if value.kind == .ident && value.value == owned.name {
					if moves_input || !semantic_types_equal(field.typ, owned.typ) {
						return none
					}
					moves_input = true
					continue
				}
			}
			if value.kind !in [.int_literal, .float_literal, .bool_literal, .char_literal,
				.string_literal] {
				if !tc.ownership_scoped_factory_pool_pop(value, field_type, receiver_type, param,
					module_name) {
					return none
				}
				pool_elements << field_type
			}
			continue
		}
		if !tc.ownership_scoped_factory_receiver_field(*value, receiver_type, param, module_name) {
			return none
		}
		source := tc.struct_field_type(receiver_type.name_id, value.value) or { return none }
		source_type := unalias_type(source)
		if source_type !is Pointer || !semantic_types_equal(unwrap_pointer(source_type), unwrap_pointer(field_type)) {
			return none
		}
		referent := unwrap_pointer(field_type)
		access := tc.ownership_scoped_thread_access(referent, true) or { return none }
		borrows << ScopedThreadBorrow{ ...loan, field: field.name, access: access }
	}
	// Reject positional fields, spreads, and any initializer omitted by the proof.
	pops := pool_elements.len > 0
	if initialized != state.children_count || borrows.len == 0
		|| (input != none && !moves_input)
		|| (pops && !tc.ownership_scoped_factory_pool_borrows_disjoint(pool_elements, state_type, borrows)) {
		return none
	}
	return ScopedFactoryProof{ borrows: borrows, pops: pops }
}
