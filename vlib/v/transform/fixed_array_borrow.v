module transform

import time
import v.flat
import v.types

// BorrowCandidate is one pointer parameter whose borrowing is being decided.
struct BorrowCandidate {
	decl    int
	index   int
	nparams int
	name    string
}

// BorrowDeclItem is one declaration whose parameters are summarised. Its pointer
// parameters are candidates[cand_start..cand_end].
struct BorrowDeclItem {
	name       string
	decl       FnParamDeclRef
	nparams    int
	cand_start int
	cand_end   int
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
	// record_replaced makes fixed_array_call_uses_borrow list in `replaced` every
	// callee summary whose dependency it replaced by other dependencies (prescan
	// helpers only; see BorrowBodyResult).
	record_replaced bool
	replaced        []BorrowParamRef
}

// BorrowBodyResult is one candidate's body analysis, made before the serial fixed
// point runs, against no proven summaries. A summary's state never changes whether
// a body borrows, only its dependencies: handing storage to a proven callee
// parameter adds none, handing it to an unproven one adds that parameter. So the
// analysis made with a set of proven summaries has these dependencies less the
// proven ones. The one exception is an argument whose unproven dependency the
// analysis replaced by the dependencies of an ordinary use (see
// fixed_array_call_uses_borrow): with that summary proven it would have none.
// `replaced` lists those summaries.
struct BorrowBodyResult {
mut:
	ready        bool
	borrows      bool
	dependencies []BorrowParamRef
	replaced     []BorrowParamRef
}

// matches_proven_summaries reports whether dropping the proven dependencies gives
// the analysis the serial fixed point would make now.
fn (r &BorrowBodyResult) matches_proven_summaries(t &Transformer) bool {
	if !r.ready {
		return false
	}
	for param in r.replaced {
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
@[unsafe]
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
	t.fixed_array_borrow_prescanned = 0
	mut sw := time.new_stopwatch()
	mut candidates := []BorrowCandidate{}
	mut items := []BorrowDeclItem{}
	// The item of every declaration seen, -1 for one without known parameters.
	mut decl_items := map[int]int{}
	if summarised == 0 {
		decl_items.reserve(u32(t.call_param_types_decl_cache.len))
	}
	mut listed := summarised == 0 && t.borrow_decl_items_listed
	if listed {
		// A declaration the pre-scan could not resolve has no item. That is what
		// the walk below finds too, unless this transformer resolves it.
		for unresolved in t.unresolved_borrow_decls {
			if _ := t.call_param_types_from_decl(unresolved.name) {
				listed = false
				break
			}
		}
	}
	if listed {
		// The parameter pre-scan walked the same declarations in the same order
		// and listed them as it resolved their parameters.
		candidates = t.listed_borrow_candidates
		items = t.listed_borrow_decl_items
		for i, item in items {
			decl_items[item.decl.idx] = i
		}
	} else {
		for name, decl in t.call_param_types_decl_index {
			if decl.idx < summarised || decl.idx in decl_items {
				continue
			}
			// The parameter pre-scan has usually cached the declaration already.
			params := t.call_param_types_decl_cache[decl.idx] or {
				t.call_param_types_from_decl(name) or {
					decl_items[decl.idx] = -1
					continue
				}
			}
			decl_items[decl.idx] = items.len
			t.list_borrow_decl_item(name, decl, params, mut candidates, mut items)
		}
	}
	t.borrow_decl_items_listed = false
	t.listed_borrow_candidates = []BorrowCandidate{}
	t.listed_borrow_decl_items = []BorrowDeclItem{}
	t.unresolved_borrow_decls = []BorrowDeclItem{}
	// Inspect each body once, recording the callee parameters it requires to
	// borrow. Propagating proven summaries through reverse edges computes the
	// same least fixed point without revisiting escaping bodies on every round.
	mut dependents := [][]int{len: candidates.len}
	mut remaining := []int{len: candidates.len, init: -1}
	mut queue := []int{}
	mut qi := 0
	// Bodies are analysed up front on helper threads against no proven summaries.
	// The fixed point below keeps its order and analyses a body again itself only
	// when the summaries proven by now could change more than which of its
	// dependencies are still open (see BorrowBodyResult).
	t.prepare_times.candidates = f64(sw.elapsed().microseconds()) / 1000.0
	sw.restart()
	prescanned := t.prescan_fixed_array_borrow_bodies(candidates, items)
	t.prepare_times.prescan = f64(sw.elapsed().microseconds()) / 1000.0
	sw.restart()
	for i, c in candidates {
		ready := i < prescanned.results.len && prescanned.results[i].matches_proven_summaries(t)
		if ready {
			t.fixed_array_borrow_prescanned++
		}
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
			if ready && t.proven_fixed_array_borrow_summary(dependency) {
				// The analysis made now would not depend on a proven summary.
				continue
			}
			if dependency.decl < summarised {
				if !t.fixed_array_borrow_summary(dependency) {
					remaining[i] = -1
					break
				}
				continue
			}
			target := borrow_candidate_of_param(candidates, items, decl_items, dependency)
			if target < 0 {
				remaining[i] = -1
				break
			}
			dependents[target] << i
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
	t.prepare_times.fixed_point = f64(sw.elapsed().microseconds()) / 1000.0
	sw.restart()
	t.prepare_call_provenance_borrow_params(summarised, items, prescanned.provenance)
	prescanned.release()
	$if prealloc {
		// The work lists of a compiler build are megabytes that the transform
		// stage would otherwise hold in the preparation arena until it ends.
		unsafe {
			candidates.free()
			items.free()
			dependents.free()
			remaining.free()
			decl_items.free()
		}
	}
	t.prepare_times.provenance = f64(sw.elapsed().microseconds()) / 1000.0
}

// list_borrow_decl_item appends the item of one declaration with known parameter
// types, and a candidate for each of its pointer parameters.
fn (t &Transformer) list_borrow_decl_item(name string, decl FnParamDeclRef, params []types.Type, mut candidates []BorrowCandidate, mut items []BorrowDeclItem) {
	fn_node := t.a.node(flat.NodeId(decl.idx))
	cand_start := candidates.len
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
	items << BorrowDeclItem{
		name:       name
		decl:       decl
		nparams:    params.len
		cand_start: cand_start
		cand_end:   candidates.len
	}
}

// borrow_candidate_of_param is the candidate that decides `param`, or -1 when the
// parameter is not a candidate.
fn borrow_candidate_of_param(candidates []BorrowCandidate, items []BorrowDeclItem, decl_items map[int]int, param BorrowParamRef) int {
	item_idx := decl_items[param.decl] or { -1 }
	if item_idx < 0 {
		return -1
	}
	item := items[item_idx]
	for i in item.cand_start .. item.cand_end {
		if candidates[i].index == param.index {
			return i
		}
	}
	return -1
}

// BorrowPrescan holds the up-front body analyses, the call-provenance summaries
// made beside them (one per declaration item) and the arenas that own them.
struct BorrowPrescan {
mut:
	results    []BorrowBodyResult
	provenance [][]bool
	scopes     []voidptr
}

// release frees the helper arenas that hold the prescanned results.
fn (p &BorrowPrescan) release() {
	for scope in p.scopes {
		transform_worker_scope_free(scope)
	}
}

// prescan_fixed_array_borrow_body analyses one candidate on a helper transformer
// that sees no proven summaries, and records which dependencies it replaced.
fn (mut t Transformer) prescan_fixed_array_borrow_body(c BorrowCandidate) BorrowBodyResult {
	mut memo := t.borrow_param_mentions
	memo.record_replaced = true
	memo.replaced.clear()
	mut dependencies := []BorrowParamRef{}
	borrows := t.fixed_array_param_body_borrows(c, mut dependencies)
	return BorrowBodyResult{
		ready:        true
		borrows:      borrows
		dependencies: dependencies
		replaced:     if memo.replaced.len > 0 { memo.replaced.clone() } else { []BorrowParamRef{} }
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

@[direct_array_access]
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
		if t.var_type(callee.value) != 0 {
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
	// The receiver offset only matters for a callee or argument that mentions the
	// parameter; most calls mention none, so resolve it on first use.
	mut offset := -1
	if t.fixed_array_expr_mentions_param(callee_id, param) {
		offset = if params.len > 0 { t.call_param_offset(name, node, params) } else { 0 }
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
		if offset < 0 {
			offset = if params.len > 0 { t.call_param_offset(name, node, params) } else { 0 }
		}
		dependency_start := dependencies.len
		param_index := t.call_argument_param_index(node, i, offset, params.len, t.call_is_variadic_for_node(name, node))
		if t.fixed_array_arg_hands_param_storage(arg_id, param)
			&& t.fixed_array_storage_arg_borrows(name, node, params, param_index, arg_id, mut dependencies) {
			// A scalar use can already borrow independently of the callee's
			// summary. Keep that alternative when the storage path is provisional.
			if dependencies.len > dependency_start {
				mut ordinary_dependencies := []BorrowParamRef{}
				if t.fixed_array_param_use_borrows(arg_id, param, false, mut ordinary_dependencies) {
					if ordinary_dependencies.len > 0 && !isnil(t.borrow_param_mentions)
						&& t.borrow_param_mentions.record_replaced {
						// With the callee's summary proven the argument has no dependency
						// at all, so this analysis does not stand once it is.
						mut memo := t.borrow_param_mentions
						for replaced_idx in dependency_start .. dependencies.len {
							memo.replaced << dependencies[replaced_idx]
						}
					}
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
