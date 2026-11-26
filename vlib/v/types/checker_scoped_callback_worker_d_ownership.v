module types

import v.flat

struct ScopedCallbackWorkerScope {
	decl         flat.NodeId
	name         string
	module_name  string
	callback     Type
	helper_depth int
}

struct ScopedCallbackWorkerBinding {
	param     flat.NodeId
	local     flat.NodeId = flat.empty_node
	move_from flat.NodeId = flat.empty_node
}

// Inspect the declaring body before permitting an erased callback to move to
// a worker. The worker must own the box and keep it within the joined call.
fn (tc &TypeChecker) ownership_scoped_callback_worker(info CallInfo, index int, expected Type) bool {
	if expected !is Struct || info.is_variadic || tc.call_param_is_mut(info, index)
		|| call_param_is_shared(info, index) {
		return false
	}
	scope := tc.ownership_scoped_callback_worker_scope(info.name, expected, 0) or { return false }
	decl := tc.a.node(scope.decl)
	file := tc.ownership_fn_decl_file(*decl)
	if decl.skip_ownership_drops() || tc.declaration_has_attribute(scope.decl, 'manualfree')
		|| tc.ownership_file_uses_library_value_semantics(file) {
		return false
	}
	return tc.ownership_scoped_callback_worker_body(scope, index, true, mut map[u64]bool{})
}

fn (tc &TypeChecker) ownership_scoped_callback_worker_scope(name string, callback Type, helper_depth int) ?ScopedCallbackWorkerScope {
	module_name := tc.fn_type_modules[name] or { return none }
	file := tc.fn_type_files[name] or { return none }
	if file.ends_with('.vh') {
		return none
	}
	decl := tc.visible_mutation_fn_decl(name, module_name) or { return none }
	decl_id := flat.NodeId(decl.idx)
	node := tc.a.node(decl_id)
	if node.kind != .fn_decl || tc.ownership_fn_decl_file(*node) != file {
		return none
	}
	return ScopedCallbackWorkerScope{
		decl:         decl_id
		name:         name
		module_name:  module_name
		callback:     callback
		helper_depth: helper_depth
	}
}

fn (tc &TypeChecker) ownership_scoped_callback_worker_body(scope ScopedCallbackWorkerScope, index int, owns bool, mut helpers map[u64]bool) bool {
	decl := tc.a.node(scope.decl)
	if index < 0 || index >= decl.children_count {
		return false
	}
	param_id := tc.a.child(decl, index)
	param := tc.a.node(param_id)
	if param.kind != .param || param.value in ['', '_'] {
		return false
	}
	mut binding := ScopedCallbackWorkerBinding{ param: param_id }
	if owns {
		for i in 0 .. decl.children_count {
			stmt := tc.a.child_node(decl, i)
			if stmt.kind != .decl_assign || stmt.children_count != 2 {
				continue
			}
			from_id := tc.ownership_unwrap_expr(tc.a.child(stmt, 1))
			from := tc.a.node(from_id)
			if from.kind != .ident || from.value != param.value {
				continue
			}
			to_id := tc.a.child(stmt, 0)
			to := tc.a.node(to_id)
			if tc.valid_node_id(binding.local) || to.kind != .ident || to.value in ['_', param.value]
				|| (!stmt.is_mut && !to.is_mut) {
				return false
			}
			binding = ScopedCallbackWorkerBinding{ param: param_id, local: to_id, move_from: from_id }
		}
	}
	return tc.ownership_scoped_callback_worker_uses(scope.decl, scope, binding, false, 0, mut helpers)
}

fn (tc &TypeChecker) ownership_scoped_callback_worker_uses(id flat.NodeId, scope ScopedCallbackWorkerScope, binding ScopedCallbackWorkerBinding, blocked bool, depth int, mut helpers map[u64]bool) bool {
	if depth > 128 {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.asm_stmt, .goto_stmt] {
		return false
	}
	nested := blocked || node.kind in [.fn_literal, .lambda_expr, .spawn_expr, .defer_stmt]
		|| (node.kind == .block && node.value == 'unsafe')
	if node.kind in [.ident, .param] {
		if id in [binding.param, binding.local, binding.move_from] {
			return !nested
		}
		param := tc.a.node(binding.param)
		owner := if tc.valid_node_id(binding.local) { tc.a.node(binding.local) } else { param }
		if node.value == owner.value {
			return !nested && tc.ownership_scoped_callback_worker_use(id, scope, mut helpers)
		}
		if node.value == param.value {
			return false
		}
	}
	for i in 0 .. node.children_count {
		if !tc.ownership_scoped_callback_worker_uses(tc.a.child(node, i), scope, binding, nested,
			depth + 1, mut helpers) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) ownership_scoped_callback_worker_use(id flat.NodeId, scope ScopedCallbackWorkerScope, mut helpers map[u64]bool) bool {
	parent_id := tc.ownership_scoped_thread_use_parent(id)
	parent := tc.a.node(parent_id)
	if parent.kind == .selector && parent.value == 'call' && parent.children_count == 1 {
		call := tc.a.node(tc.ownership_scoped_thread_use_parent(parent_id))
		return call.kind == .call && call.children_count == 2 && tc.a.child(call, 0) == parent_id
	}
	// A source helper may receive a mutable borrow. Checking its parameter
	// recursively keeps the entire chain inside the joined worker.
	if parent.kind != .call || parent.children_count < 2 || scope.helper_depth >= 16 {
		return false
	}
	callee := tc.a.child_node(parent, 0)
	info := tc.ownership_scoped_callback_helper_info(*callee, scope) or { return false }
	if info.is_variadic {
		return false
	}
	helper := tc.ownership_scoped_callback_worker_scope(info.name, scope.callback, scope.helper_depth + 1) or {
		return false
	}
	offset := if info.has_receiver { 0 } else { 1 }
	for i in 1 .. parent.children_count {
		arg := tc.call_arg_value(tc.a.child(parent, i))
		if tc.ownership_unwrap_expr(arg) != id {
			continue
		}
		index := i - offset
		param := info.params[index] or { return false }
		if !tc.a.node(arg).is_mut || !tc.call_param_is_mut(info, index) || call_param_is_shared(info, index)
			|| param !is Pointer || !semantic_types_equal(unwrap_pointer(param), scope.callback) {
			return false
		}
		key := (u64(int(helper.decl)) << 32) | u64(index)
		if verified := helpers[key] {
			return verified
		}
		// An in-progress entry rejects cycles and already-checked helpers
		// are shared by all call sites in this proof.
		helpers[key] = false
		verified := tc.ownership_scoped_callback_worker_body(helper, index, false, mut helpers)
		helpers[key] = verified
		return verified
	}
	return false
}

fn (tc &TypeChecker) ownership_scoped_callback_helper_info(callee flat.Node, scope ScopedCallbackWorkerScope) ?CallInfo {
	if callee.kind == .ident {
		if !tc.ownership_scoped_callback_unshadowed(scope.decl, callee.value, flat.empty_node, 0) {
			return none
		}
		name := checker_qualified_fn_name(scope.module_name, callee.value)
		module_name := tc.fn_type_modules[name] or { return none }
		if module_name != scope.module_name && !(module_name in ['', 'main']
			&& scope.module_name in ['', 'main']) {
			return none
		}
		return tc.call_info(name, false)
	}
	if callee.kind != .selector || callee.children_count != 1 {
		return none
	}
	base := tc.a.child_node(&callee, 0)
	if base.kind != .ident {
		return none
	}
	decl := tc.a.node(scope.decl)
	params := tc.fn_param_types[scope.name] or { return none }
	for i, param in params {
		if i >= decl.children_count {
			return none
		}
		binding := tc.a.child(decl, i)
		node := tc.a.node(binding)
		if node.kind != .param || node.value != base.value {
			continue
		}
		if !tc.ownership_scoped_callback_unshadowed(scope.decl, base.value, binding, 0) {
			return none
		}
		receiver := unwrap_pointer(param)
		if receiver is Struct {
			name := tc.concrete_method_signature_key(receiver.name_id, callee.value) or { return none }
			info := tc.call_info(name, true)
			formal := info.params[0] or { return none }
			if semantic_types_equal(unwrap_pointer(formal), receiver) {
				return info
			}
		}
		return none
	}
	return none
}

// A helper name must be a bare call target or a selector on a declared
// parameter. Reject rebinding before trusting the statically selected body.
fn (tc &TypeChecker) ownership_scoped_callback_unshadowed(id flat.NodeId, name string, binding flat.NodeId, depth int) bool {
	if depth > 128 {
		return false
	}
	node := tc.a.node(id)
	if node.value == name && node.kind in [.ident, .param] {
		if id == binding {
			return true
		}
		parent := tc.a.node(tc.direct_parent_id(id))
		kind := if tc.valid_node_id(binding) { flat.NodeKind.selector } else { flat.NodeKind.call }
		return node.kind == .ident && parent.kind == kind && parent.children_count > 0
			&& tc.a.child(parent, 0) == id
	}
	for i in 0 .. node.children_count {
		if !tc.ownership_scoped_callback_unshadowed(tc.a.child(node, i), name, binding, depth + 1) {
			return false
		}
	}
	return true
}
