module transform

import v.flat
import v.types

// Preserve earlier call effects only in a frame where consuming a narrow Result
// can remove the ordinary returned-provenance path. Scan the whole frame first:
// a void/captured call can retain a source before the consuming reader appears.
fn (t &Transformer) has_consumable_scalar_result(id flat.NodeId) bool {
	node := t.a.node(id)
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if node.kind in [.or_expr, .cast_expr] && node.children_count > 0 {
		source := t.unwrap_parens(t.a.child(node, 0))
		if t.a.node(source).kind == .call {
			result := types.unalias_type(t.tc.resolve_type(source))
			if result is types.Unknown {
				return true
			}
			if result is types.ResultType {
				success := t.tc.resolve_type(id)
				if success is types.Unknown || (escape_call_result_is_scalar(success)
					&& !escape_scalar_result_can_encode_address(success)) {
					return true
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.has_consumable_scalar_result(t.a.child(node, i)) {
			return true
		}
	}
	return false
}

// These summaries rule out retaining a parameter's provenance through effects.
// An aggregate producer may return aliases: its caller still tracks that result.
// A narrow scalar Result can discard them only after handling its error locally.
// Storage-only borrow summaries cannot certify aliases held inside a value.
fn (mut t Transformer) prepare_call_provenance_borrow_params(summarised int) {
	if summarised > 0 {
		for decl, summary in t.tc.transform_call_provenance_borrow_params {
			t.call_provenance_borrow_params[decl] = summary
		}
	}
	mut seen := map[int]bool{}
	for name, decl in t.call_param_types_decl_index {
		if decl.idx < summarised || seen[decl.idx] {
			continue
		}
		seen[decl.idx] = true
		fn_node := t.a.node(flat.NodeId(decl.idx))
		ret := t.parse_decl_param_type(fn_node.typ, decl.module, decl.file)
		returns_aliases := !scalar_result_is_narrow(ret)
		params := t.call_param_types_from_decl(name) or { continue }
		mut summary := []bool{len: params.len}
		mut index := 0
		for i in 0 .. fn_node.children_count {
			param := t.a.child_node(fn_node, i)
			if param.kind != .param {
				continue
			}
			if index >= summary.len {
				break
			}
			t.begin_borrow_param_mentions(BorrowCandidate{ decl: decl.idx, index: index, nparams: params.len, name: param.value }, fn_node)
			mut has_body := false
			mut proven := true
			for j in 0 .. fn_node.children_count {
				body := t.a.child(fn_node, j)
				if t.a.node(body).kind == .param {
					continue
				}
				has_body = true
				if !t.scalar_result_param_consumed(body, param.value, false, returns_aliases) {
					proven = false
					break
				}
			}
			summary[index] = has_body && proven
			index++
		}
		t.call_provenance_borrow_params[decl.idx] = summary
	}
}

fn scalar_result_is_narrow(typ types.Type) bool {
	ret := types.unalias_type(typ)
	if ret is types.ResultType {
		return escape_call_result_is_scalar(ret.base_type)
			&& !escape_scalar_result_can_encode_address(ret.base_type)
	}
	return false
}

// This deliberately bounded proof does not follow aliases, calls or captures.
// A producer may return aliases through a pure expression; callers track their
// provenance. Other uses only consume narrow reads or builtin container lengths.
fn (t &Transformer) scalar_result_param_consumed(id flat.NodeId, param string, storage bool, returns_aliases bool) bool {
	node := t.a.node(id)
	if !t.fixed_array_expr_mentions_param(id, param) {
		return true
	}
	if returns_aliases && node.kind == .return_stmt {
		return t.scalar_result_return_is_pure(id, param)
	}
	if returns_aliases && node.kind == .postfix && node.op in [.inc, .dec]
		&& node.children_count == 1 {
		target := t.a.child(node, 0)
		typ := t.fixed_array_borrow_type(target) or { return false }
		return escape_type_is_scalar_value(typ)
			&& t.fixed_array_arg_hands_param_storage(target, param)
	}
	if node.kind in [.assign, .decl_assign, .selector_assign, .index_assign, .fn_literal, .lambda_expr,
		.spawn_expr, .for_stmt, .for_in_stmt, .comptime_for, .asm_stmt, .sql_expr, .lock_expr,
		.string_interp, .dump_expr, .in_expr] || (node.kind == .postfix && node.op in [
		.inc,
		.dec,
	]) {
		return false
	}
	if node.kind == .infix {
		typ := t.fixed_array_borrow_type(id) or { return false }
		if escape_call_result_is_scalar(typ) && !escape_scalar_result_can_encode_address(typ) {
			return t.scalar_result_return_is_pure(id, param)
		}
	}
	if node.kind == .ident {
		return node.value != param || storage
	}
	if node.kind == .call || node.kind == .as_expr || node.kind == .struct_init
		|| node.kind == .assoc || node.kind == .array_literal || node.kind == .map_init {
		return false
	}
	if node.kind == .prefix && node.op == .amp {
		return false
	}
	if node.kind in [.selector, .index] || (node.kind == .prefix && node.op == .mul) {
		if node.children_count == 0 || node.value == 'range'
			|| (node.kind == .index && t.expr_is_overloaded_index_result(id)) {
			return false
		}
		if !storage {
			typ := t.fixed_array_borrow_type(id) or { return false }
			mut length := false
			if node.kind == .selector && node.value == 'len' {
				base := t.fixed_array_borrow_type(t.a.child(node, 0)) or { return false }
				raw := types.unalias_type(types.unwrap_all_pointers(types.unalias_type(base)))
				length = raw is types.Array || raw is types.ArrayFixed || raw is types.String
					|| raw is types.Map
			}
			if !length && (!escape_type_is_scalar_value(typ)
				|| escape_scalar_result_can_encode_address(typ)) {
				return false
			}
		}
		if !t.scalar_result_param_consumed(t.a.child(node, 0), param, true, returns_aliases) {
			return false
		}
		for i in 1 .. node.children_count {
			if !t.scalar_result_param_consumed(t.a.child(node, i), param, false, returns_aliases) {
				return false
			}
		}
		return true
	}
	if node.kind == .cast_expr {
		typ := t.fixed_array_borrow_type(id) or { return false }
		if typ is types.Alias || !escape_type_is_scalar_value(typ) || escape_scalar_result_can_encode_address(typ) {
			return false
		}
	}
	for i in 0 .. node.children_count {
		if !t.scalar_result_param_consumed(t.a.child(node, i), param, false, returns_aliases) {
			return false
		}
	}
	return true
}

// Returning a value exports provenance through the result, not through an
// independent side effect. Reject calls, captures and overloaded operations.
fn (t &Transformer) scalar_result_return_is_pure(id flat.NodeId, param string) bool {
	if !t.fixed_array_expr_mentions_param(id, param) {
		return true
	}
	node := t.a.node(id)
	if node.kind in [.call, .fn_literal, .lambda_expr, .spawn_expr, .assign, .decl_assign,
		.selector_assign, .index_assign, .lock_expr, .sql_expr, .asm_stmt, .string_interp, .dump_expr,
		.in_expr] {
		return false
	}
	if node.kind == .index && t.expr_is_overloaded_index_result(id) {
		return false
	}
	if node.kind == .infix || node.kind == .cast_expr {
		for i in 0 .. node.children_count {
			typ := t.fixed_array_borrow_type(t.a.child(node, i)) or { return false }
			if typ is types.Alias || typ is types.Enum
				|| (node.kind == .infix && !escape_type_is_scalar_value(typ)) {
				return false
			}
		}
	}
	for i in 0 .. node.children_count {
		if !t.scalar_result_return_is_pure(t.a.child(node, i), param) {
			return false
		}
	}
	return true
}

// A resolved declaration is usable only for direct calls and concrete methods.
// Function fields, interfaces and local function values do not identify a body.
fn (t &Transformer) scalar_result_call_summary(id flat.NodeId) ?[]bool {
	call := t.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return none
	}
	name := t.call_name_for_node(id, *call)
	decl := t.fixed_array_call_param_ref(name, *call, 0) or { return none }
	params := t.call_param_types_decl_cache[decl.decl] or { return none }
	callee := t.a.child_node(call, 0)
	offset := t.call_param_offset(name, *call, params)
	if callee.kind == .selector {
		if callee.children_count == 0 {
			return none
		}
		base := t.a.child(callee, 0)
		if !t.callee_base_is_not_a_runtime_value(base) {
			base_type := types.unalias_type(types.unwrap_all_pointers(types.unalias_type(t.tc.resolve_type(base))))
			if offset != 1 || base_type !is types.Struct {
				return none
			}
		}
	} else if callee.kind != .ident {
		return none
	}
	if call.children_count - 1 + offset != params.len {
		return none
	}
	return t.call_provenance_borrow_params[decl.decl] or { return none }
}

fn (t &Transformer) scalar_result_literal_arg(id flat.NodeId) bool {
	node := t.a.node(t.unwrap_parens(id))
	return node.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.nil_literal, .none_expr, .enum_val]
}

fn (t &Transformer) scalar_result_call_borrows_arguments(id flat.NodeId) bool {
	call := t.a.node(id)
	summary := t.scalar_result_call_summary(id) or { return false }
	offset := summary.len - (call.children_count - 1)
	if offset == 1 && !summary[0] {
		return false
	}
	for i in 1 .. call.children_count {
		if !summary[i - 1 + offset] && !t.scalar_result_literal_arg(t.a.child(call, i)) {
			return false
		}
	}
	return true
}

fn (t &Transformer) call_provenance_known_borrow(id flat.NodeId, call flat.Node) bool {
	if t.escape_call_is_allocation_helper(id, call) {
		return true
	}
	if call.children_count != 6 {
		return false
	}
	callee := t.a.child_node(&call, 0)
	if callee.kind != .selector || callee.value != 'GC_get_heap_usage_safe'
		|| callee.children_count != 1 {
		return false
	}
	base := t.a.child_node(callee, 0)
	// libgc only fills these five numeric outputs; it never stores their pointers.
	// Keep gc_heap_usage's measurement itself free of heap promotion.
	return base.kind == .ident && base.value == 'C'
		&& t.call_name_for_node(id, call) == 'C.GC_get_heap_usage_safe'
}

// Effects happen even when a later pure scalar call discards the inner call's
// provenance. Preserve unproven argument sources when the call is evaluated.
fn (mut t Transformer) mark_call_provenance_argument_escapes(id flat.NodeId, call flat.Node, amp_sources map[string][]string, ptr_aliases map[string]string) {
	if !t.preserve_call_provenance {
		return
	}
	if t.call_provenance_known_borrow(id, call) {
		return
	}
	summary := t.scalar_result_call_summary(id) or { []bool{} }
	name := t.call_name_for_node(id, call)
	mut param_types := t.call_param_types_for_node(name, call)
	mut is_variadic := t.call_is_variadic_for_node(name, call)
	if concrete := t.concrete_generic_call_param_types(id, call) {
		param_types = concrete.clone()
		is_variadic = is_variadic || t.concrete_generic_call_is_variadic(id, call)
	}
	offset := if t.call_is_selector_form(call) && t.concrete_generic_call_is_method(id) {
		1
	} else {
		t.call_param_offset_for_node(name, call, param_types)
	}
	variadic := if is_variadic && param_types.len > 0 && param_types[param_types.len - 1] is types.Array {
		param_types.len - 1
	} else {
		-1
	}
	callee_id := t.a.child(&call, 0)
	callee := t.a.node(callee_id)
	if summary.len == 0 {
		// Invoking a function value can export its captures independently of its
		// explicit arguments. Read the current aliases before later reassignments.
		for source in t.escape_aggregate_address_sources(callee_id, amp_sources, ptr_aliases) {
			t.escaping_amp_sources[source] = true
		}
	}
	if callee.kind == .selector && callee.children_count > 0 {
		receiver := t.a.child(callee, 0)
		if !t.callee_base_is_not_a_runtime_value(receiver)
			&& (summary.len == 0 || offset != 1 || !summary[0]) {
			for source in t.escape_aggregate_address_sources(receiver, amp_sources, ptr_aliases) {
				t.escaping_amp_sources[source] = true
			}
			// Interface dispatch passes the object, never the header's address.
			if types.unalias_type(t.tc.resolve_type(receiver)) !is types.Interface
				&& t.method_receiver_is_reference(id, receiver, callee.value) {
				for source in t.escape_address_sources(receiver, amp_sources, ptr_aliases) {
					t.escaping_amp_sources[source] = true
				}
			}
		}
	}
	for i in 1 .. call.children_count {
		index := i - 1 + offset
		if index >= 0 && index < summary.len && summary[index] {
			continue
		}
		argument := t.a.child(&call, i)
		for source in t.escape_aggregate_address_sources(argument, amp_sources, ptr_aliases) {
			t.escaping_amp_sources[source] = true
		}
		actual := t.a.node(argument)
		param_type := call_argument_param_type(param_types, index, variadic) or { types.Type(types.Unknown{}) }
		implicit_reference := call_provenance_pointer_depth(param_type) >
			call_provenance_pointer_depth(t.tc.resolve_type(argument))
		if actual.is_mut || implicit_reference {
			for source in t.escape_address_sources(argument, amp_sources, ptr_aliases) {
				t.escaping_amp_sources[source] = true
			}
		}
	}
}

fn call_provenance_pointer_depth(typ types.Type) int {
	mut current := types.unalias_type(typ)
	mut depth := 0
	for current is types.Pointer {
		depth++
		current = types.unalias_type(current.base_type)
	}
	return depth
}
