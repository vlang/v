module transform

import v.flat
import v.types

// Compute summaries before workers rewrite function bodies. Only scalar reads and
// writes through the parameter qualify; aliases, slices and address-taking remain
// conservative, as do calls that forward the parameter itself.
fn (mut t Transformer) prepare_fixed_array_borrow_params() {
	if isnil(t.tc) {
		return
	}
	t.ensure_call_param_types_decl_index()
	mut seen_decls := map[int]bool{}
	for name, decl in t.call_param_types_decl_index {
		if seen_decls[decl.idx] {
			continue
		}
		seen_decls[decl.idx] = true
		params := t.call_param_types_from_decl(name) or { continue }
		fn_node := t.a.node(flat.NodeId(decl.idx))
		mut summary := []bool{len: params.len}
		mut param_idx := 0
		for i in 0 .. fn_node.children_count {
			param := t.a.child_node(fn_node, i)
			if param.kind != .param {
				continue
			}
			index := param_idx
			param_idx++
			if index >= params.len || !escape_type_is_pointer(params[index]) {
				continue
			}
			mut seen_types := map[string]bool{}
			payload := types.unalias_type(types.unwrap_all_pointers(types.unalias_type(params[index])))
			if !t.escape_value_contains_fixed_array(payload, mut seen_types) {
				continue
			}
			mut safe := false
			for j in 0 .. fn_node.children_count {
				body_id := t.a.child(fn_node, j)
				if t.a.node(body_id).kind == .param {
					continue
				}
				safe = true
				if !t.fixed_array_param_use_borrows(body_id, param.value, false) {
					safe = false
					break
				}
			}
			summary[index] = safe
		}
		if summary.any(it) {
			t.fixed_array_borrow_params[decl.idx] = summary
		}
	}
}

fn (t &Transformer) fixed_array_call_param_borrows(name string, call flat.Node, index int) bool {
	callee_id := t.a.child(&call, 0)
	callee := t.a.node(callee_id)
	if callee.kind == .ident {
		if t.var_type(callee.value).len > 0 {
			return false
		}
		// Escape prescanning precedes local type registration. Resolve lexical
		// bindings before trusting a same-named function declaration's summary.
		if _ := t.local_binding_before(callee.value, callee_id) {
			return false
		}
	}
	decl := t.call_param_types_decl_index[name] or { return false }
	summary := t.fixed_array_borrow_params[decl.idx] or { return false }
	return index >= 0 && index < summary.len && summary[index]
}

fn (t &Transformer) fixed_array_expr_mentions_param(id flat.NodeId, param string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	if node.kind == .ident && node.value == param {
		return true
	}
	for i in 0 .. node.children_count {
		if t.fixed_array_expr_mentions_param(t.a.child(node, i), param) {
			return true
		}
	}
	return false
}

fn (t &Transformer) fixed_array_param_use_borrows(id flat.NodeId, param string, storage_use bool) bool {
	if !t.fixed_array_expr_mentions_param(id, param) {
		return true
	}
	node := t.a.node(id)
	if node.kind == .ident {
		return storage_use
	}
	if node.kind in [.fn_literal, .lambda_expr, .spawn_expr] {
		return false
	}
	if node.kind in [.selector, .index] {
		if node.value == 'range' || node.children_count == 0 {
			return false
		}
		if node.kind == .index {
			base_expr_type := t.tc.expr_type(t.a.child(node, 0)) or {
				return false
			}
			base_type := types.unalias_type(types.unwrap_all_pointers(types.unalias_type(base_expr_type)))
			if base_type !is types.Array && base_type !is types.ArrayFixed && base_type !is types.Map
				&& base_type !is types.String {
				return false
			}
		}
		if !storage_use {
			typ := t.tc.expr_type(id) or { return false }
			if !escape_type_is_scalar_value(typ) {
				return false
			}
		}
		if !t.fixed_array_param_use_borrows(t.a.child(node, 0), param, true) {
			return false
		}
		for i in 1 .. node.children_count {
			if !t.fixed_array_param_use_borrows(t.a.child(node, i), param, false) {
				return false
			}
		}
		return true
	}
	if node.kind == .prefix && node.op == .amp {
		return false
	}
	for i in 0 .. node.children_count {
		writes_storage := i == 0 && ((node.kind in [.assign, .selector_assign, .index_assign])
			|| (node.kind == .postfix && node.op in [.inc, .dec]))
		if !t.fixed_array_param_use_borrows(t.a.child(node, i), param, writes_storage) {
			return false
		}
	}
	return true
}
