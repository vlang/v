				tc.check_module_name_conflict(param_id, param.value)
			}
		}
	}
	if !fast_valid_build && !node.value.contains('.') && tc.has_active_import(node.value) {
		tc.record_error_at(.duplicate_decl, 'duplicate of an import symbol `${node.value}`', flat.NodeId(fn_idx), tc.fn_declaration_diagnostic_pos(node))
	}
	for pi in 0 .. node.children_count {
		param_id := tc.a.child(&node, pi)
		if duplicate_parameter_ids[int(param_id)] {
			continue
		}
		p := tc.a.node(param_id)
		tc.insert_fn_param_binding(param_id, p)
	}
	tc.insert_implicit_veb_ctx(node)
	if !fast_valid_build {
		tc.check_veb_app_method_params(flat.NodeId(fn_idx), node)
	}
	// Full semantics for open generic declarations are checked when they are instantiated.
	// The source-level global-shadow rule does not depend on concrete types, so inspect
	// those bindings now before deferring expression and control-flow checks.
	generic_params := if is_specialized {
		map[string]bool{}
	} else {
		tc.infer_decl_generic_params(node)
	}
	// The parser marks bodyless .vh declarations with is_mut: their bodies
	// live in cached objects. Keep signature checks, but do not inspect a
	// nonexistent body for fallthrough, unused parameters, or noreturn behavior.
	has_body := !node.is_mut
	if has_body && generic_params.len > 0 {
		tc.check_generic_fn_body_global_shadowing(node)
	}
	signature_has_bare_generic_type := tc.fn_decl_has_bare_generic_signature_type(node)
	should_check_generic_body := generic_params.len == 0
	if has_body && should_check_generic_body && !signature_has_bare_generic_type {
		tc.check_fn_body(node)
		if !fast_valid_build {
			tc.check_recursive_str_calls(flat.NodeId(fn_idx), node)
		}
	} else if has_body && generic_params.len > 0 && node.value.contains('.') && !fast_valid_build
		&& tc.should_diagnose(flat.NodeId(fn_idx)) {
		tc.check_deferred_generic_receiver_comparisons(node)
	}
	if !fast_valid_build {
		if has_body {
			qname := checker_qualified_fn_name(module_name, node.value)
			tc.check_noreturn_fn_semantics(flat.NodeId(fn_idx), node, qname)
			tc.check_unreachable_after_noreturn_call(node)
		}
		if !is_specialized {
			if has_body && tc.should_diagnose(flat.NodeId(fn_idx)) {
				tc.record_unused_fn_vars(node)
				tc.record_unused_fn_params(node)
				tc.record_unused_fn_labels(node)
			}
			tc.check_fn_bare_generic_fntype_params(node)
		}
		is_disabled_stub := node.value in tc.a.disabled_fns
		// A terminal propagation whose payload still contains a generic placeholder
		// and return control flow guarded by a generic `$if` are lowered against the
		// concrete specialization. Keep those narrow deferrals without suppressing
		// ordinary generic fallthrough.
		has_deferred_generic_return := generic_params.len > 0
			&& tc.fn_has_deferred_generic_return(node, generic_params)
		if has_body && tc.fn_context.return_type !is Unknown
			&& !type_allows_implicit_return(tc.fn_context.return_type)
			&& !tc.fn_body_definitely_returns(node) && !is_disabled_stub
			&& !has_deferred_generic_return && tc.should_diagnose(flat.NodeId(fn_idx)) {
			message := 'missing return at end of function `${node.value.all_after_last('.')}`'
			tc.record_error_at(.return_mismatch, message, flat.NodeId(fn_idx), tc.fn_declaration_diagnostic_pos(node))
		}
	}
	tc.fn_context.node_id = -1
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
	tc.fn_context = saved_fn_context
}

fn (mut tc TypeChecker) check_fn_receiver_and_operator_return(node flat.Node, id flat.NodeId) {
	tc.check_fn_receiver_syntax(id, node)
	if node.children_count > 0 {
		receiver_id := tc.a.child(&node, 0)
		receiver := tc.a.node(receiver_id)
		if receiver.kind == .param && receiver.op == .dot
			&& unalias_type(tc.parse_type(receiver.typ)) is MultiReturn {
			tc.record_error_at(.call_arg_mismatch, 'cannot define method on multi-value', receiver_id, tc.type_diagnostic_pos(receiver_id, receiver.typ))
		}
	}
	raw_return_type := node.typ.trim_space()
	if raw_return_type.starts_with('!?') || raw_return_type.starts_with('?!') {
		tc.record_error_at(.return_mismatch, 'the type must be Option or Result', id, tc.nested_option_result_marker_pos(node))
	}
	if raw_return_type == '?void' {
		tc.record_error_at(.return_mismatch, 'use `?` instead of `?void`', id, tc.option_void_payload_diagnostic_pos(node))
	}
	if raw_return_type.ends_with('?') && !raw_return_type.starts_with('?') {
		tc.record_error_at(.return_mismatch, 'wrong syntax, it must be ?${raw_return_type.trim_right('?')}, not ${raw_return_type}', id, tc.suffix_option_return_type_diagnostic_pos(node))
	}
	signature_return_type := unalias_type(tc.parse_type(node.typ))
	if signature_return_type is MultiReturn {
		for typ in signature_return_type.types {
			if is_ierror_type(unalias_type(typ)) {
				tc.record_error_at(.return_mismatch, 'type `IError` cannot be used in multi-return, return an Option instead', id, tc.fn_return_type_diagnostic_pos(node))
				break
			}
		}
	}
	if node.children_count > 0 && node.value.contains('.') {
		receiver := tc.a.child_node(&node, 0)
		receiver_name := node.value.all_before_last('.').all_after_last('.')
		if receiver.kind == .param && receiver.op == .dot {
			receiver_type := unalias_type(tc.parse_type(receiver.typ))
			if receiver_type is Interface
