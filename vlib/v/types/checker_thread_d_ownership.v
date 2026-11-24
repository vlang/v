module types

import v.flat

enum ThreadCapability {
	transfer
	share
}

fn (tc &TypeChecker) ownership_checks_thread_boundary() bool {
	return !tc.autofree_mode && tc.unsafe_depth == 0 && !tc.ownership_effects_disabled()
}

fn (mut tc TypeChecker) ownership_check_spawn_transfer(call_id flat.NodeId, call flat.Node) {
	info := tc.resolve_call_info(call_id, call) or { return }
	callee_id := tc.a.child(&call, 0)
	mut callee := tc.a.node(callee_id)
	if info.has_receiver && callee.kind == .index && callee.children_count > 0 {
		// The index supplies explicit type arguments to the method selector.
		callee = tc.a.child_node(callee, 0)
	}
	if info.has_receiver && callee.kind == .selector && callee.children_count > 0 {
		receiver_id := tc.a.child(callee, 0)
		expected := info.params[0] or { tc.resolve_type(receiver_id) }
		tc.ownership_check_thread_argument(receiver_id, expected, info, 0, 'thread receiver')
	} else if tc.ownership_thread_uses_function_value(callee_id) {
		tc.record_error(.thread_transfer,
			'cannot start a thread with a function value whose captures are unknown', callee_id)
	}
	for i in 1 .. call.children_count {
		value_id := tc.call_arg_value(tc.a.child(&call, i))
		arg_id := tc.spread_arg_value(value_id) or { value_id }
		param_idx := tc.ownership_call_arg_decl_param_idx(info, i) +
			tc.ownership_call_arg_shift(call, info)
		elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, param_idx, value_id)
		expected := tc.ownership_call_arg_expected_type(info, param_idx, elem_idx)
		tc.ownership_check_thread_argument(arg_id, expected, info, param_idx, 'thread argument')
	}
	tc.ownership_check_thread_transfer(info.return_type, callee_id, 'thread result')
}

fn (tc &TypeChecker) ownership_thread_uses_function_value(id flat.NodeId) bool {
	callee := tc.a.node(tc.ownership_unwrap_expr(id))
	match callee.kind {
		.ident { return tc.name_has_local_binding(callee.value) }
		.fn_literal { return tc.fn_literal_has_captures(*callee) }
		.index {
			return callee.children_count > 0
				&& tc.ownership_thread_uses_function_value(tc.a.child(callee, 0))
		}
		.call, .cast_expr, .if_expr, .lambda_expr, .match_stmt, .or_expr { return true }
		.selector {
			if callee.children_count > 0 {
				base := tc.a.child_node(callee, 0)
				return base.kind != .ident || tc.name_has_local_binding(base.value)
			}
		}
		else {}
	}
	return false
}

fn (mut tc TypeChecker) ownership_check_thread_argument(id flat.NodeId, expected Type, info CallInfo, index int, role string) {
	if call_param_is_shared(info, index) && tc.expr_is_shared_arg(id) {
		tc.ownership_check_thread_transfer(unwrap_pointer(unalias_type(expected)), id, role)
		return
	}
	actual := tc.resolve_type(id)
	if tc.call_param_is_mut(info, index) {
		tc.record_error(.thread_transfer,
			'cannot transfer mutable borrow of `${actual.name()}` as ${role}', id)
		return
	}
	if unalias_type(expected) is Pointer {
		tc.ownership_check_thread_transfer(expected, id, role)
		return
	}
	// Check both types for implicit conversions, including interface boxing.
	if tc.ownership_check_thread_transfer(actual, id, role) && expected !is Void
		&& actual.source_type_id() != expected.source_type_id() {
		tc.ownership_check_thread_transfer(expected, id, role)
	}
}

fn (mut tc TypeChecker) ownership_check_channel_close_error(id flat.NodeId) {
	clean_id := tc.ownership_unwrap_expr(id)
	node := tc.a.node(clean_id)
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .ident && !tc.ident_resolves_to_value(callee.value)
			&& tc.ownership_call_is_builtin_error_ctor(clean_id, tc.cur_module) {
			return
		}
	}
	if (node.kind != .ident || !tc.ident_resolves_to_value(node.value))
		&& (tc.ownership_expr_reads_message_error_const(clean_id)
			|| tc.ownership_expr_reads_error_sentinel(clean_id)) {
		return
	}
	_ = tc.ownership_check_thread_transfer(tc.resolve_type(id), id, 'channel close error')
}

fn (mut tc TypeChecker) ownership_check_thread_transfer(typ Type, id flat.NodeId, role string) bool {
	if reason := tc.ownership_thread_transfer_problem(typ, .transfer, 0, mut map[u64]bool{}) {
		tc.record_error(.thread_transfer,
			'cannot transfer `${typ.name()}` as ${role}: ${reason}', id)
		return false
	}
	return true
}

fn (tc &TypeChecker) ownership_thread_transfer_problem(typ Type, capability ThreadCapability, depth int, mut seen map[u64]bool) ?string {
	if depth > 64 {
		return 'cannot prove thread safety of this recursive type'
	}
	match typ {
		Pointer {
			return 'references cannot be transferred between threads'
		}
		FnType {
			return 'the function type does not describe its captures'
		}
		Interface {
			return 'the interface type does not guarantee thread-safe implementations'
		}
		Alias {
			return tc.ownership_thread_transfer_problem(typ.base_type, capability, depth + 1, mut seen)
		}
		OptionType {
			return tc.ownership_thread_transfer_problem(typ.base_type, capability, depth + 1, mut seen)
		}
		ResultType {
			if reason := tc.ownership_thread_transfer_problem(typ.base_type, capability, depth + 1, mut seen) {
				return reason
			}
			return 'the result can contain an error whose type does not guarantee thread-safe implementations'
		}
		Array, ArrayFixed {
			return tc.ownership_thread_transfer_problem(typ.elem_type, capability, depth + 1, mut seen)
		}
		Channel {
			// The channel synchronizes access to transferred elements.
			return tc.ownership_thread_transfer_problem(typ.elem_type, .transfer, depth + 1, mut seen)
		}
		Map {
			if reason := tc.ownership_thread_transfer_problem(typ.key_type, capability, depth + 1, mut seen) {
				return reason
			}
			return tc.ownership_thread_transfer_problem(typ.value_type, capability, depth + 1, mut seen)
		}
		MultiReturn {
			for part in typ.types {
				if reason := tc.ownership_thread_transfer_problem(part, capability, depth + 1, mut seen) {
					return reason
				}
			}
		}
		Struct {
			name_id := typ.name_id
			visit_key := (u64(name_id) << 1) | u64(capability)
			if seen[visit_key] { return none }
			seen[visit_key] = true
			if flat.source_type_query(name_id).has(.c_prefix) {
				return 'the C type has no V ownership information'
			}
			if payload := tc.thread_wait_return_type(typ) {
				return tc.ownership_thread_transfer_problem(payload, .transfer, depth + 1, mut seen)
			}
			if payload := tc.ownership_thread_library_payload(name_id, 'sync/arc', [
				'sync.arc.Arc',
				'sync.arc.Weak',
				'arc.Arc',
				'arc.Weak',
			]) {
				// Sending an owner can share the payload with another owner. The
				// last owner can also destroy it on another thread.
				return tc.ownership_thread_transfer_problem(payload, .share, depth + 1, mut seen)
			}
			if payload := tc.ownership_thread_library_payload(name_id, 'sync/arc', [
				'sync.arc.Mutex',
				'arc.Mutex',
			]) {
				// The mutex gives each updated value one owner at a time.
				return tc.ownership_thread_transfer_problem(payload, .transfer, depth + 1, mut seen)
			}
			if capability == .share
				&& tc.ownership_thread_library_payload(name_id, 'cell', ['cell.RefCell']) != none {
				return 'cell.RefCell has unsynchronized borrow state'
			}
			for field in tc.struct_fields_for_type(name_id) {
				field_is_shared := tc.struct_field_is_shared(name_id, field.name)
				field_type := if field_is_shared {
					unwrap_pointer(unalias_type(field.typ))
				} else {
					field.typ
				}
				field_capability := if field_is_shared {
					ThreadCapability.transfer
				} else {
					capability
				}
				if reason := tc.ownership_thread_transfer_problem(field_type, field_capability, depth + 1, mut seen) {
					return 'field `${field.name}`: ${reason}'
				}
			}
		}
		SumType {
			name_id := typ.name_id
			visit_key := (u64(name_id) << 1) | u64(capability)
			if seen[visit_key] { return none }
			seen[visit_key] = true
			for variant in tc.sum_types.type_ids(tc.sum_base_name(name_id)) or { []u32{} } {
				concrete := tc.parse_type(tc.concrete_sum_variant_name(name_id, variant))
				if reason := tc.ownership_thread_transfer_problem(concrete, capability, depth + 1, mut seen) {
					return 'variant `${concrete.name()}`: ${reason}'
				}
			}
		}
		else {}
	}
	return none
}

// Check declaration provenance before relying on a standard-library type's
// synchronization or interior mutability.
fn (tc &TypeChecker) ownership_thread_library_payload(name_id u32, module_path string, supported_names []string) ?Type {
	syntax := flat.source_type_syntax(name_id)
	base := flat.source_type_name(syntax.generic_base())
	if base !in supported_names {
		return none
	}
	args := syntax.generic_app_args()
	if args.len != 1 || tc.compiler_vroot == '' {
		return none
	}
	decl := tc.source_struct_decl_for_name(syntax.generic_base()) or { return none }
	file := tc.ownership_fn_decl_file(decl)
	source := tc.a.cached_header_sources[file] or { file }
	file_path := tc.a.real_source_path(source).replace('\\', '/')
	root := tc.memo_real_path(tc.compiler_vroot).replace('\\', '/').trim_right('/')
	if !file_path.starts_with('${root}/vlib/${module_path}/') {
		return none
	}
	return *tc.parse_type(args[0])
}
