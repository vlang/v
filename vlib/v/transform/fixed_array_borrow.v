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

// The AST stays immutable during summary preparation. One mask answers the
// mention queries for all parameters of a function, including repeated queries
// of nested calls and selectors. Exact node IDs make collisions harmless.
struct BorrowParamMentions {
mut:
	decl        int = -1
	params      []string
	param       string
	bit         u64
	generation  u32
	ids         [4096]int
	masks       [4096]u64
	generations [4096]u32
	// record_consults makes fixed_array_borrow_summary list every summary it reads
	// in `consulted` (prescan helpers only; see BorrowBodyResult).
	record_consults bool
	consulted       []BorrowParamRef
}

// BorrowBodyResult is one candidate's body analysis, made before the serial fixed
// point runs against no proven summaries. `consulted` lists the summaries it read,
// all of them unproven. While none of those is proven yet, the serial analysis
// takes the same path and reaches the same outcome.
struct BorrowBodyResult {
mut:
	ready        bool
	borrows      bool
	dependencies []BorrowParamRef
	consulted    []BorrowParamRef
}

// matches_proven_summaries reports whether no summary the prescan read has been
// proven since, so its outcome is the serial one.
fn (r &BorrowBodyResult) matches_proven_summaries(t &Transformer) bool {
	if !r.ready {
		return false
	}
	for param in r.consulted {
		if t.proven_fixed_array_borrow_summary(param) {
			return false
		}
	}
	return true
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
	t.borrow_param_mentions = &BorrowParamMentions{}
	defer { t.borrow_param_mentions = unsafe { nil } }
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
	// Bodies are analysed up front on helper threads against no proven summaries.
	// The fixed point below keeps its order and analyses a body again itself only
	// when a summary that analysis read has been proven by now.
	prescanned := t.prescan_fixed_array_borrow_bodies(candidates)
	for i, c in candidates {
		ready := i < prescanned.results.len && prescanned.results[i].matches_proven_summaries(t)
		mut scanned_dependencies := []BorrowParamRef{}
		borrows := if ready {
			prescanned.results[i].borrows
		} else {
			t.fixed_array_param_body_borrows(c, mut scanned_dependencies)
		}
		if !borrows {
			continue
		}
		dependencies := if ready {
			prescanned.results[i].dependencies
		} else {
			scanned_dependencies
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
	prescanned.release()
	t.prepare_call_provenance_borrow_params(summarised)
}

// BorrowPrescan holds the up-front body analyses and the arenas that own them.
struct BorrowPrescan {
mut:
	results []BorrowBodyResult
	scopes  []voidptr
}

// release frees the helper arenas that hold the prescanned dependency lists.
fn (p &BorrowPrescan) release() {
	for scope in p.scopes {
		transform_worker_scope_free(scope)
	}
}

// prescan_fixed_array_borrow_body analyses one candidate on a helper transformer
// that sees no proven summaries, and records which summaries the analysis read.
fn (mut t Transformer) prescan_fixed_array_borrow_body(c BorrowCandidate) BorrowBodyResult {
	mut memo := t.borrow_param_mentions
	memo.record_consults = true
	memo.consulted.clear()
	mut dependencies := []BorrowParamRef{}
	borrows := t.fixed_array_param_body_borrows(c, mut dependencies)
	return BorrowBodyResult{
		ready:        true
		borrows:      borrows
		dependencies: dependencies
		consulted:    memo.consulted.clone()
	}
}

fn (mut t Transformer) fixed_array_param_body_borrows(c BorrowCandidate, mut dependencies []BorrowParamRef) bool {
	fn_node := t.a.node(flat.NodeId(c.decl))
	t.begin_borrow_param_mentions(c, fn_node)
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

fn (mut t Transformer) begin_borrow_param_mentions(c BorrowCandidate, node &flat.Node) {
	if isnil(t.borrow_param_mentions) {
		return
	}
	mut memo := t.borrow_param_mentions
	if memo.decl != c.decl {
		memo.decl = c.decl
		memo.params.clear()
		memo.generation++
		if memo.generation == 0 {
			memo.generations = [4096]u32{}
			memo.generation = 1
		}
		for i in 0 .. node.children_count {
			param := t.a.child_node(node, i)
			if param.kind == .param {
				memo.params << param.value
			}
		}
	}
	memo.param = c.name
	memo.bit = 0
	if memo.params.len <= 64 {
		for i, name in memo.params {
			if name == c.name {
				memo.bit |= u64(1) << i
			}
		}
	}
}

fn (t &Transformer) fixed_array_mentioned_params(id flat.NodeId) u64 {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len {
		return 0
	}
	mut memo := t.borrow_param_mentions
	slot := idx & 4095
	if memo.generations[slot] == memo.generation && memo.ids[slot] == idx {
		return memo.masks[slot]
	}
	node := t.a.node(id)
	mut mask := u64(0)
	if node.kind == .ident {
		for i, param in memo.params {
			if node.value == param {
				mask |= u64(1) << i
			}
		}
	}
	for i in 0 .. node.children_count {
		mask |= t.fixed_array_mentioned_params(t.a.child(node, i))
	}
	memo.ids[slot] = idx
	memo.masks[slot] = mask
	memo.generations[slot] = memo.generation
	return mask
}

fn (t &Transformer) fixed_array_call_param_borrows(name string, call flat.Node, index int) bool {
	param := t.fixed_array_call_param_ref(name, call, index) or { return false }
	return t.fixed_array_borrow_summary(param)
}

fn (t &Transformer) fixed_array_borrow_summary(param BorrowParamRef) bool {
	if !isnil(t.borrow_param_mentions) && t.borrow_param_mentions.record_consults {
		mut memo := t.borrow_param_mentions
		memo.consulted << param
	}
	return t.proven_fixed_array_borrow_summary(param)
}

// proven_fixed_array_borrow_summary reports whether `param` is proven to borrow.
fn (t &Transformer) proven_fixed_array_borrow_summary(param BorrowParamRef) bool {
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
	if !isnil(t.borrow_param_mentions) && t.borrow_param_mentions.bit != 0
		&& t.borrow_param_mentions.param == param {
		return t.fixed_array_mentioned_params(id) & t.borrow_param_mentions.bit != 0
	}
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
