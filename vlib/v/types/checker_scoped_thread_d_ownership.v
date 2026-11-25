module types

import v.flat

// A direct, immutable handle binding immediately followed by a lexical defer joins
// before that binding's scope can exit. The receiver identifier is the handle's
// only permitted use. A function-wide defer would allow the borrowed storage to
// leave scope first.
fn (tc &TypeChecker) ownership_scoped_thread_join_site(spawn_id flat.NodeId) ?flat.NodeId {
	spawn_node := tc.a.node(spawn_id)
	if spawn_node.value != 'spawn' {
		return none
	}
	mut binding_id := tc.direct_parent_id(spawn_id)
	for tc.valid_node_id(binding_id) && tc.a.node(binding_id).kind == .paren {
		binding_id = tc.direct_parent_id(binding_id)
	}
	binding := tc.a.node(binding_id)
	if binding.kind != .decl_assign || binding.children_count != 2 || binding.value !in ['', '1'] || binding.is_mut {
		return none
	}
	handle := tc.a.child_node(binding, 0)
	if handle.kind != .ident || handle.is_mut || handle.value == '_' {
		return none
	}
	scope := tc.a.node(tc.direct_parent_id(binding_id))
	if scope.kind !in [.block, .fn_decl, .fn_literal, .for_stmt, .for_in_stmt, .match_branch,
		.select_branch]
		|| scope.children_count < 2 {
		return none
	}
	for i in 0 .. scope.children_count - 1 {
		if tc.a.child(scope, i) == binding_id {
			return tc.ownership_scoped_thread_defer_receiver(tc.a.child(scope, i + 1), handle.value)
		}
	}
	return none
}

fn (tc &TypeChecker) ownership_scoped_thread_defer_receiver(id flat.NodeId, name string) ?flat.NodeId {
	stmt := tc.a.node(id)
	if stmt.kind != .defer_stmt || stmt.value == 'function' || stmt.children_count != 1 {
		return none
	}
	block := tc.a.child_node(stmt, 0)
	if block.kind != .block || block.children_count != 1 {
		return none
	}
	call := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(block, 0)))
	if call.kind != .call || call.children_count != 1 {
		return none
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.value != 'wait' || callee.children_count != 1 {
		return none
	}
	receiver_id := tc.a.child(callee, 0)
	receiver := tc.a.node(receiver_id)
	if receiver.kind == .ident && receiver.value == name {
		return receiver_id
	}
	return none
}

// Only a direct address of a local immutable scalar establishes a loan here.
// Compound values can contain aliases whose lifetime and synchronization must
// also be proved before their references can be passed to workers.
fn (tc &TypeChecker) ownership_scoped_thread_source(id flat.NodeId) ?string {
	id0 := tc.ownership_unwrap_expr(id)
	expr := tc.a.node(id0)
	if expr.kind != .prefix || expr.op != .amp || expr.children_count != 1 {
		return none
	}
	source_id := tc.ownership_unwrap_expr(tc.a.child(expr, 0))
	source := tc.a.node(source_id)
	if source.kind != .ident || tc.ident_is_mutable_lvalue(source.value) {
		return none
	}
	owner := tc.cur_scope.lookup_owner(source.value) or { return none }
	if tc.binding_owner_is_global(owner) {
		return none
	}
	typ := unalias_type(tc.resolve_type(source_id))
	return if ownership_scoped_thread_is_scalar(typ) { source.value } else { none }
}

struct ScopedThreadBorrow {
	source string
	field  string
	pos    flat.NodeId
}

// Collect the loans supplied by a scoped spawn argument. A record's borrowed
// fields must come directly from the caller, so the initializer provides the
// complete set of scalar storage the worker can read.
fn (tc &TypeChecker) ownership_scoped_thread_argument_borrows(id flat.NodeId, expected Type) ?[]ScopedThreadBorrow {
	actual := unalias_type(tc.resolve_type(id))
	formal := unalias_type(expected)
	if actual is Pointer && formal is Pointer {
		if !semantic_types_equal(unalias_type(*actual.base_type), unalias_type(*formal.base_type)) {
			return none
		}
		source := tc.ownership_scoped_thread_source(id) or { return none }
		return [ScopedThreadBorrow{ source: source, pos: id }]
	}
	if actual !is Struct || !semantic_types_equal(actual, formal) {
		return none
	}
	name_id := (actual as Struct).name_id
	literal := tc.a.node(tc.ownership_unwrap_expr(id))
	if literal.kind != .struct_init || tc.struct_embed_receiver_names(name_id).len != 0
		|| tc.ownership_type_has_explicit_drop(name_id)
		|| receiver_method_name_candidates(actual, 'free', tc.cur_module).any(it in tc.fn_ret_types) {
		return none
	}
	mut result := []ScopedThreadBorrow{}
	for field in tc.struct_fields_for_type(name_id) {
		if type_contains_unknown(field.typ) || receiver_method_name_candidates(actual, field.name, tc.cur_module).any(it in tc.fn_ret_types) {
			return none
		}
		field_type := unalias_type(field.typ)
		if field_type is Pointer {
			field_id := tc.ownership_scoped_thread_field_initializer(literal, field.name) or { return none }
			actual_field := unalias_type(tc.resolve_type(field_id))
			if actual_field is Pointer {
				if !semantic_types_equal(unalias_type(*actual_field.base_type), unalias_type(*field_type.base_type)) {
					return none
				}
			} else {
				return none
			}
			source := tc.ownership_scoped_thread_source(field_id) or { return none }
			result << ScopedThreadBorrow{ source: source, field: field.name, pos: field_id }
			continue
		}
		if _ := tc.ownership_thread_transfer_problem(field_type, .transfer, 0, mut map[u64]bool{}) {
			return none
		}
	}
	return if result.len > 0 { result } else { none }
}

fn (tc &TypeChecker) ownership_scoped_thread_field_initializer(literal &flat.Node, name string) ?flat.NodeId {
	for i in 0 .. literal.children_count {
		field := tc.a.child_node(literal, i)
		if field.kind == .field_init && field.value == name && field.children_count == 1 {
			return tc.a.child(field, 0)
		}
	}
	return none
}

// The join bounds the worker lifetime. Check the resolved V body to prove
// that the only uses of the borrowed parameter read the scalar's value.
fn (tc &TypeChecker) ownership_scoped_thread_reads_parameter(info CallInfo, index int, expected Type, borrows []ScopedThreadBorrow) bool {
	module_name := tc.fn_type_modules[info.name] or { return false }
	file := tc.fn_type_files[info.name] or { return false }
	if file.ends_with('.vh') {
		return false
	}
	decl := tc.visible_mutation_fn_decl(info.name, module_name) or { return false }
	fn_node := tc.a.node(flat.NodeId(decl.idx))
	if fn_node.kind != .fn_decl || tc.ownership_fn_decl_file(*fn_node) != file
		|| index < 0 || index >= int(fn_node.children_count) {
		return false
	}
	parameter := tc.a.child_node(fn_node, index)
	if parameter.kind != .param || parameter.value == '' {
		return false
	}
	mut fields := map[string]bool{}
	formal := unalias_type(expected)
	if formal is Struct {
		for field in tc.struct_fields_for_type(formal.name_id) {
			fields[field.name] = borrows.any(it.field == field.name)
		}
	}
	for i in 0 .. fn_node.children_count {
		child := tc.a.child(fn_node, i)
		if tc.a.node(child).kind != .param
			&& !tc.ownership_scoped_thread_only_reads(child, parameter.value, fields, false, 0) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) ownership_scoped_thread_only_reads(id flat.NodeId, name string, fields map[string]bool, inside_callable bool, depth int) bool {
	if depth > 128 {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .asm_stmt {
		return false
	}
	if node.kind in [.ident, .param] && node.value == name {
		if inside_callable {
			return false
		}
		if fields.len == 0 {
			return tc.ownership_scoped_thread_scalar_read(id)
		}
		field_id := tc.direct_parent_id(id)
		field := tc.a.node(field_id)
		if field.kind != .selector {
			return false
		}
		borrowed := fields[field.value] or { return false }
		parent := tc.a.node(tc.direct_parent_id(field_id))
		if parent.kind == .call && tc.a.child(parent, 0) == field_id {
			return false
		}
		return !borrowed || tc.ownership_scoped_thread_scalar_read(field_id)
	}
	nested := inside_callable || node.kind in [.fn_literal, .lambda_expr]
	for i in 0 .. node.children_count {
		if !tc.ownership_scoped_thread_only_reads(tc.a.child(node, i), name, fields, nested, depth + 1) {
			return false
		}
	}
	return true
}

// Allow only dereferences used to compute a scalar value. A bare reference
// or a dereference passed by reference could outlive the worker's join.
fn (tc &TypeChecker) ownership_scoped_thread_scalar_read(id flat.NodeId) bool {
	deref_id := tc.ownership_scoped_thread_use_parent(id)
	deref := tc.a.node(deref_id)
	if deref.kind != .prefix || deref.op != .mul {
		return false
	}
	parent := tc.a.node(tc.ownership_scoped_thread_use_parent(deref_id))
	return match parent.kind {
		.assert_stmt, .decl_assign, .return_stmt, .string_interp { true }
		.infix {
			parent.op in [.plus, .minus, .mul, .div, .mod, .power, .eq, .ne, .lt, .gt, .le, .ge,
				.amp, .pipe, .xor, .right_shift, .right_shift_unsigned, .logical_and, .logical_or,
				.arrow]
		}
		.prefix { parent.op in [.plus, .minus, .not, .bit_not] }
		.cast_expr { ownership_scoped_thread_is_scalar(unalias_type(*tc.parse_type(parent.typ))) }
		else { false }
	}
}

fn ownership_scoped_thread_is_scalar(typ Type) bool {
	return typ is Primitive || typ is Enum || typ is Rune || typ is Char || typ is ISize || typ is USize
}

fn (mut tc TypeChecker) ownership_record_scoped_thread_borrows(handle string, rhs_id flat.NodeId) {
	spawn_id := tc.ownership_unwrap_expr(rhs_id)
	spawn_node := tc.a.node(spawn_id)
	if spawn_node.kind != .spawn_expr || spawn_node.children_count != 1
		|| !tc.ownership_checks_thread_boundary() {
		return
	}
	join_site := tc.ownership_scoped_thread_join_site(spawn_id) or { return }
	call_id := tc.a.child(spawn_node, 0)
	call := tc.a.node(call_id)
	if call.kind != .call {
		return
	}
	declared := tc.resolve_call_info(call_id, *call) or { return }
	info := tc.specialized_plain_generic_call_info(*call, declared)
	tc.ownership_record_scoped_thread_call(*call, info, handle, join_site)
}

fn (mut tc TypeChecker) ownership_record_scoped_thread_call(call flat.Node, info CallInfo, handle string, join_site flat.NodeId) {
	if recv := tc.ownership_thread_receiver(tc.a.child(&call, 0), info) {
		expected := info.params[0] or { tc.resolve_type(recv) }
		tc.ownership_record_scoped_thread_borrow(recv, info, 0, expected, handle, join_site)
	}
	for i in 1 .. call.children_count {
		value_id := tc.call_arg_value(tc.a.child(&call, i))
		arg := tc.spread_arg_value(value_id) or { value_id }
		param_idx := tc.ownership_call_arg_decl_param_idx(info, i) +
			tc.ownership_call_arg_shift(call, info)
		elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, param_idx, value_id)
		expected := tc.ownership_call_arg_expected_type(info, param_idx, elem_idx)
		tc.ownership_record_scoped_thread_borrow(arg, info, param_idx, expected, handle, join_site)
	}
}

fn (mut tc TypeChecker) ownership_record_scoped_thread_borrow(arg flat.NodeId, info CallInfo, param_idx int, expected Type, handle string, join_site flat.NodeId) {
	if tc.call_param_is_mut(info, param_idx) {
		return
	}
	borrows := tc.ownership_scoped_thread_argument_borrows(arg, expected) or { return }
	for borrow in borrows {
		mut state := tc.ownership_state()
		if active := state.borrowed_vars[borrow.source] {
			if active.any(it.thread_join_site == join_site && it.borrower == handle) {
				continue
			}
		}
		state.scoped_thread_loans_seen = true
		tc.ownership_add_borrow_info(borrow.source, BorrowInfo{
			borrower:         handle
			pos:              borrow.pos
			// Reserve the referent for the worker until its defer joins.
			is_mut:           true
			thread_join_site: join_site
		})
	}
}

fn (tc &TypeChecker) ownership_scoped_thread_use_parent(id flat.NodeId) flat.NodeId {
	mut parent := tc.direct_parent_id(id)
	for tc.a.node(parent).kind == .paren {
		parent = tc.direct_parent_id(parent)
	}
	return parent
}

// A further scoped worker can borrow the same scalar as a direct argument or
// through an inline initializer. The spawn check certifies its use of the
// borrow. The arguments of an ordinary helper call remain outside this rule.
fn (tc &TypeChecker) ownership_scoped_thread_reborrows(id flat.NodeId) bool {
	addr_id := tc.ownership_scoped_thread_use_parent(id)
	addr := tc.a.node(addr_id)
	if addr.kind != .prefix || addr.op != .amp {
		return false
	}
	mut call_id := tc.ownership_scoped_thread_use_parent(addr_id)
	if tc.a.node(call_id).kind == .field_init {
		literal_id := tc.ownership_scoped_thread_use_parent(call_id)
		if tc.a.node(literal_id).kind != .struct_init {
			return false
		}
		call_id = tc.ownership_scoped_thread_use_parent(literal_id)
	}
	if tc.a.node(call_id).kind != .call {
		return false
	}
	spawn_id := tc.ownership_scoped_thread_use_parent(call_id)
	return tc.a.node(spawn_id).kind == .spawn_expr
		&& (tc.ownership_scoped_thread_join_site(spawn_id) != none || tc.ownership_scoped_thread_group(spawn_id) != none)
}

fn (mut tc TypeChecker) ownership_reject_scoped_thread_access(name string, id flat.NodeId) bool {
	state := tc.ownership_state()
	if !state.scoped_thread_loans_seen {
		return false
	}
	for source, borrows in state.borrowed_vars {
		for borrow in borrows {
			if !tc.valid_node_id(borrow.thread_join_site) {
				continue
			}
			if source == name {
				if tc.ownership_scoped_thread_reborrows(id) {
					continue
				}
				tc.record_error(.thread_transfer,
					'cannot use `${name}` until scoped thread `${borrow.borrower}` has joined', id)
				return true
			}
			if (borrow.borrower == name || ownership_storage_key_is_descendant(name, borrow.borrower))
				&& id != borrow.thread_join_site && id != tc.direct_parent_id(borrow.thread_join_site)
				&& !tc.ownership_scoped_thread_group_append(id) {
				tc.record_error(.thread_transfer,
					'cannot use scoped thread handle `${borrow.borrower}` outside its deferred wait', id)
				return true
			}
		}
	}
	return false
}
