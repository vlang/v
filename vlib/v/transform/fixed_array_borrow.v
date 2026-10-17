module transform

import v.flat
import v.types

// BorrowCandidate is one pointer parameter whose borrowing is being decided.
struct BorrowCandidate {
	decl    int
	index   int
	nparams int
	name    string
}

struct BorrowParamRef {
	decl  int
	index int
}

// Compute summaries before workers rewrite function bodies. A parameter borrows when no
// reference to its storage outlives the call: every use reads or writes scalars through it,
// or hands it — the parameter itself, or the address of storage inline in it — straight to a
// callee parameter that borrows in turn. Aliases, slices, stored or returned addresses and
// callbacks stay conservative.
//
// Callee summaries make this a fixed point. Every candidate starts as escaping and a pass can
// only turn one into borrowing, so the iteration ends; a recursive call through a parameter not
// yet known to borrow leaves it escaping. Plain pointer parameters (`data &u8`) are summarised
// as well as fixed-array ones, because that is what an element address `&a[i]` is handed to.
//
// Monomorphization starts from the summaries the transform stage computed before lowering and
// summarises only the declarations appended since; a call into an earlier declaration borrows
// as its summary says.
fn (mut t Transformer) prepare_fixed_array_borrow_params() {
	if isnil(t.tc) {
		return
	}
	t.ensure_call_param_types_decl_index()
	mut summarised := 0
	if t.borrow_params_after_transform && t.tc.transform_borrow_node_count <= t.a.nodes.len {
		summarised = t.tc.transform_borrow_node_count
		for decl, summary in t.tc.transform_borrow_params {
			t.fixed_array_borrow_params[decl] = summary
		}
	}
	t.fixed_array_borrow_node_count = t.a.nodes.len
	mut candidates := []BorrowCandidate{}
	mut seen_decls := map[int]bool{}
	for name, decl in t.call_param_types_decl_index {
		if decl.idx < summarised || seen_decls[decl.idx] {
			continue
		}
		seen_decls[decl.idx] = true
		params := t.call_param_types_from_decl(name) or { continue }
		fn_node := t.a.node(flat.NodeId(decl.idx))
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
			candidates << BorrowCandidate{
				decl:    decl.idx
				index:   index
				nparams: params.len
				name:    param.value
			}
		}
	}
	mut candidate_index := map[int][]int{}
	for i, c in candidates {
		mut indexes := candidate_index[c.decl] or { []int{len: c.nparams, init: -1} }
		indexes[c.index] = i
		candidate_index[c.decl] = indexes
	}
	// Inspect each body once, recording the callee parameters it requires to
	// borrow. Propagating proven summaries through reverse edges computes the
	// same least fixed point without revisiting escaping bodies on every round.
	mut dependents := [][]int{len: candidates.len}
	mut remaining := []int{len: candidates.len, init: -1}
	mut queue := []int{}
	mut qi := 0
	for i, c in candidates {
		mut dependencies := []BorrowParamRef{}
		if !t.fixed_array_param_body_borrows(c, mut dependencies) {
			continue
		}
		remaining[i] = 0
		for dependency in dependencies {
			if dependency.decl < summarised {
				if !t.fixed_array_borrow_summary(dependency) {
					remaining[i] = -1
					break
				}
				continue
			}
			indexes := candidate_index[dependency.decl] or { []int{} }
			if dependency.index < 0 || dependency.index >= indexes.len
				|| indexes[dependency.index] < 0 {
				remaining[i] = -1
				break
			}
			dependents[indexes[dependency.index]] << i
			remaining[i]++
		}
		if remaining[i] == 0 {
			queue << i
		}
		for qi < queue.len {
			proven_idx := queue[qi]
			qi++
			proven := candidates[proven_idx]
			mut summary := t.fixed_array_borrow_params[proven.decl] or { []bool{len: proven.nparams} }
			summary[proven.index] = true
			t.fixed_array_borrow_params[proven.decl] = summary
			for dependent in dependents[proven_idx] {
				if remaining[dependent] <= 0 {
					continue
				}
				remaining[dependent]--
				if remaining[dependent] == 0 {
					queue << dependent
				}
			}
		}
	}
}

fn (mut t Transformer) fixed_array_param_body_borrows(c BorrowCandidate, mut dependencies []BorrowParamRef) bool {
	fn_node := t.a.node(flat.NodeId(c.decl))
	mut has_body := false
	for j in 0 .. fn_node.children_count {
		body_id := t.a.child(fn_node, j)
		if t.a.node(body_id).kind == .param {
			continue
		}
		has_body = true
		if !t.fixed_array_param_use_borrows(body_id, c.name, false, mut dependencies) {
			return false
		}
	}
	return has_body
}

fn (t &Transformer) fixed_array_call_param_borrows(name string, call flat.Node, index int) bool {
	param := t.fixed_array_call_param_ref(name, call, index) or { return false }
	return t.fixed_array_borrow_summary(param)
}

fn (t &Transformer) fixed_array_borrow_summary(param BorrowParamRef) bool {
	summary := t.fixed_array_borrow_params[param.decl] or { return false }
	return param.index >= 0 && param.index < summary.len && summary[param.index]
}

fn (t &Transformer) fixed_array_call_param_ref(name string, call flat.Node, index int) ?BorrowParamRef {
	callee_id := t.a.child(&call, 0)
	callee := t.a.node(callee_id)
	if callee.kind == .ident {
		if flat.source_type_name(t.var_type(callee.value)).len > 0 {
			return none
		}
		// Escape prescanning precedes local type registration. Resolve lexical
		// bindings before trusting a same-named function declaration's summary.
		if _ := t.local_binding_before(callee.value, callee_id) {
			return none
		}
	}
	decl := t.call_param_types_decl_index[name] or { return none }
	return BorrowParamRef{ decl: decl.idx, index: index }
}

// fixed_array_borrow_type is an expression's type, resolved on demand when the checker has not
// cached it: monomorphization summarises callees whose bodies it has not reached yet, and a
// summary that gives up there moves a generic caller's local to the heap. Unknown stays unknown,
// which keeps the parameter escaping.
fn (t &Transformer) fixed_array_borrow_type(id flat.NodeId) ?types.Type {
	if typ := t.tc.expr_type(id) {
		return typ
	}
	typ := t.tc.resolve_type(id)
	if typ is types.Unknown {
		return none
	}
	return typ
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

// fixed_array_unwrap_arg looks through the wrappers an argument may carry: parentheses, an
// expression statement, and the one-expression block an `unsafe { ... }` argument becomes.
fn (t &Transformer) fixed_array_unwrap_arg(id flat.NodeId) flat.NodeId {
	mut cur := id
	for int(cur) >= 0 && int(cur) < t.a.nodes.len {
		node := t.a.node(cur)
		if node.kind in [.paren, .expr_stmt, .block] && node.children_count == 1 {
			cur = t.a.child(node, 0)
			continue
		}
		break
	}
	return cur
}

// fixed_array_arg_hands_param_storage reports whether a call argument is the parameter itself, or
// storage that belongs to it — `p[i]`, `p.buf[i]`, `p.field`, with or without `&`, as `mut p[i]` is
// — reached through fixed arrays and struct fields held by value, never through another pointer or
// a dynamic array. Index expressions along the way must not mention the parameter.
fn (t &Transformer) fixed_array_arg_hands_param_storage(arg_id flat.NodeId, param string) bool {
	arg := t.a.node(t.fixed_array_unwrap_arg(arg_id))
	if arg.kind == .ident {
		return arg.value == param
	}
	mut cur := t.fixed_array_unwrap_arg(arg_id)
	if arg.kind == .prefix && arg.op == .amp && arg.children_count == 1 {
		cur = t.fixed_array_unwrap_arg(t.a.child(arg, 0))
	}
	for {
		node := t.a.node(cur)
		if node.kind == .ident {
			return node.value == param
		}
		if node.kind !in [.index, .selector] || node.children_count == 0
			|| node.value == 'range' {
			return false
		}
		// an index through a `[]` method is a call that receives the base, not storage
		if node.kind == .index && t.expr_is_overloaded_index_result(cur) {
			return false
		}
		base_id := t.a.child(node, 0)
		base := t.a.node(base_id)
		// the parameter's own referent, or storage held by value inside it
		if !(base.kind == .ident && base.value == param) {
			base_type := types.unalias_type(t.fixed_array_borrow_type(base_id) or { return false })
			inline_base := if node.kind == .index {
				base_type is types.ArrayFixed
			} else {
				base_type is types.Struct
			}
			if !inline_base {
				return false
			}
		}
		for i in 1 .. node.children_count {
			if t.fixed_array_expr_mentions_param(t.a.child(node, i), param) {
				return false
			}
		}
		cur = base_id
	}
	return false
}

// fixed_array_call_uses_borrow: every use of `param` in a call borrows — the parameter or
// storage belonging to it handed straight to a callee parameter that borrows (the receiver of a
// method call included), or an ordinary scalar use inside an argument.
fn (mut t Transformer) fixed_array_call_uses_borrow(id flat.NodeId, node flat.Node, param string, mut dependencies []BorrowParamRef) bool {
	if node.children_count == 0 {
		return false
	}
	callee_id := t.a.child(&node, 0)
	callee := t.a.node(callee_id)
	name := t.call_name_for_node(id, node)
	params := t.call_param_types_from_decl(name) or { []types.Type{} }
	offset := if params.len > 0 { t.call_param_offset(name, node, params) } else { 0 }
	if t.fixed_array_expr_mentions_param(callee_id, param) {
		receiver_borrows := offset == 1 && callee.kind == .selector && callee.children_count > 0
			&& t.fixed_array_arg_hands_param_storage(t.a.child(callee, 0), param)
			&& t.fixed_array_storage_arg_borrows(name, node, params, 0, t.a.child(callee, 0), mut dependencies)
		if !receiver_borrows {
			if !t.fixed_array_param_use_borrows(callee_id, param, false, mut dependencies) {
				return false
			}
		}
		// A method callee has a function type, so it cannot provide the scalar-read
		// alternative used for ordinary arguments below. Keep its receiver dependency.
	}
	for i in 1 .. node.children_count {
		arg_id := t.a.child(&node, i)
		if !t.fixed_array_expr_mentions_param(arg_id, param) {
			continue
		}
		dependency_start := dependencies.len
		if t.fixed_array_arg_hands_param_storage(arg_id, param)
			&& t.fixed_array_storage_arg_borrows(name, node, params, i - 1 + offset, arg_id, mut dependencies) {
			// A scalar use can already borrow independently of the callee's
			// summary. Keep that alternative when the storage path is provisional.
			if dependencies.len > dependency_start {
				mut ordinary_dependencies := []BorrowParamRef{}
				if t.fixed_array_param_use_borrows(arg_id, param, false, mut ordinary_dependencies) {
					dependencies.trim(dependency_start)
					dependencies << ordinary_dependencies
				}
			}
			continue
		}
		if !t.fixed_array_param_use_borrows(arg_id, param, false, mut dependencies) {
			return false
		}
	}
	return true
}

// fixed_array_storage_arg_borrows: storage of the parameter, handed to callee parameter `index`,
// leaves no reference behind — passed BY VALUE into a fixed array, a struct or a scalar it is a copy
// (an explicit `&` never is, and a voidptr, pointer or interface parameter takes the address), and
// passed by reference the callee's parameter must borrow. A callee whose parameters are unknown
// borrows nothing.
fn (t &Transformer) fixed_array_storage_arg_borrows(name string, call flat.Node, params []types.Type, index int, arg_id flat.NodeId, mut dependencies []BorrowParamRef) bool {
	if index < 0 || index >= params.len {
		return false
	}
	arg := t.a.node(t.fixed_array_unwrap_arg(arg_id))
	explicit_address := arg.kind == .prefix && arg.op == .amp
	param_type := types.unalias_type(params[index])
	copied := param_type is types.ArrayFixed || param_type is types.Struct
		|| escape_type_is_scalar_value(param_type)
	if !explicit_address && !arg.is_mut && copied {
		return true
	}
	if t.fixed_array_call_param_borrows(name, call, index) {
		return true
	}
	dependency := t.fixed_array_call_param_ref(name, call, index) or { return false }
	dependencies << dependency
	return true
}

fn (mut t Transformer) fixed_array_param_use_borrows(id flat.NodeId, param string, storage_use bool, mut dependencies []BorrowParamRef) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return true
	}
	node := t.a.node(id)
	if node.kind == .ident {
		return node.value != param || storage_use
	}
	// Ordinary statements just combine their children's uses. Checking whether
	// every enclosing block mentions the parameter first repeats the same walk
	// at each nesting level. Only special uses need that guard.
	if node.kind in [.fn_literal, .lambda_expr, .spawn_expr, .call, .prefix, .selector, .index]
		&& !t.fixed_array_expr_mentions_param(id, param) {
		return true
	}
	if node.kind in [.fn_literal, .lambda_expr, .spawn_expr] {
		return false
	}
	if node.kind == .call {
		return t.fixed_array_call_uses_borrow(id, node, param, mut dependencies)
	}
	// `*p`: a scalar read, or a write, through a pointer parameter
	if node.kind == .prefix && node.op == .mul && node.children_count == 1 {
		target := t.a.node(t.a.child(node, 0))
		if target.kind == .ident && target.value == param {
			if storage_use {
				return true
			}
			typ := t.fixed_array_borrow_type(id) or { return false }
			return escape_type_is_scalar_value(typ)
		}
	}
	if node.kind in [.selector, .index] {
		if node.value == 'range' || node.children_count == 0 {
			return false
		}
		if node.kind == .index {
			if t.expr_is_overloaded_index_result(id) {
				return false
			}
			base_id := t.a.child(node, 0)
			base_expr_type := t.fixed_array_borrow_type(base_id) or { return false }
			raw_base := types.unalias_type(base_expr_type)
			base_type := types.unalias_type(types.unwrap_all_pointers(raw_base))
			// `p[i]` on a pointer parameter (unsafe indexing): a scalar element of its referent
			base := t.a.node(base_id)
			pointer_element := escape_type_is_pointer(raw_base) && base.kind == .ident
				&& base.value == param && escape_type_is_scalar_value(base_type)
			if !pointer_element && base_type !is types.Array && base_type !is types.ArrayFixed
				&& base_type !is types.Map && base_type !is types.String {
				return false
			}
		}
		if !storage_use {
			typ := t.fixed_array_borrow_type(id) or { return false }
			if !escape_type_is_scalar_value(typ) {
				return false
			}
		}
		if !t.fixed_array_param_use_borrows(t.a.child(node, 0), param, true, mut dependencies) {
			return false
		}
		for i in 1 .. node.children_count {
			if !t.fixed_array_param_use_borrows(t.a.child(node, i), param, false, mut dependencies) {
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
		if !t.fixed_array_param_use_borrows(t.a.child(node, i), param, writes_storage, mut dependencies) {
			return false
		}
	}
	return true
}
