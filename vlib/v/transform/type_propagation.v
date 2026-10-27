module transform

import v.flat
import v.types

const transform_source_type_thread = flat.source_type_id('thread')
const transform_source_type_chan = flat.source_type_id('chan')

@[heap]
struct NodeTypeMemo {
mut:
	active        bool
	lo            int
	hi            int
	values        []u32
	filled        []u32
	generation    u32
	module        string
	file          string
	expected_type u32
	context_flags u8
	type_tables   int
}

fn (mut t Transformer) begin_node_type_memo(lo int, hi int) {
	if lo < 0 || hi < lo {
		t.end_node_type_memo()
		return
	}
	if isnil(t.node_type_memo) {
		t.node_type_memo = &NodeTypeMemo{}
	}
	mut memo := t.node_type_memo
	n := hi - lo + 1
	if n > memo.values.cap {
		memo.values = []u32{len: n}
		memo.filled = []u32{len: n}
	} else {
		if n <= memo.values.len {
			memo.values.trim(n)
			memo.filled.trim(n)
		} else {
			old_len := memo.filled.len
			unsafe {
				memo.values.grow_len(n - memo.values.len)
				memo.filled.grow_len(n - memo.filled.len)
				vmemset(&memo.filled[old_len], 0, (n - old_len) * int(sizeof(u32)))
			}
		}
	}
	memo.lo = lo
	memo.hi = hi
	memo.active = true
	memo.module = t.cur_module
	memo.file = t.cur_file
	t.clear_node_type_memo()
}

fn (mut t Transformer) end_node_type_memo() {
	if !isnil(t.node_type_memo) {
		t.node_type_memo.active = false
	}
}

fn (t &Transformer) clear_node_type_memo() {
	if isnil(t.node_type_memo) {
		return
	}
	mut memo := t.node_type_memo
	if memo.active {
		memo.generation++
		if memo.generation == 0 {
			// Wrapped tags must not make an earlier context's entries visible.
			unsafe { vmemset(memo.filled.data, 0, memo.filled.len * int(sizeof(u32))) }
			memo.generation = 1
		}
	}
}

@[inline]
fn (t &Transformer) invalidate_node_type_memo(idx int) {
	_ = idx
	// A rewritten child can change any cached ancestor, including parents whose
	// header was not rewritten. Start a new epoch rather than retaining them.
	t.clear_node_type_memo()
}

// propagate_decl_type infers the declared variable type from a .decl_assign node
// and registers it in var_types so downstream transforms can resolve it.
fn (mut t Transformer) propagate_decl_type(node flat.Node) {
	if node.kind != .decl_assign || node.children_count < 2 {
		return
	}
	if node.children_count >= 4 && node.children_count % 2 == 0 {
		for i := 0; i < node.children_count; i += 2 {
			t.propagate_decl_pair_type(node, i, i + 1, u32(0))
		}
		return
	}
	t.propagate_decl_pair_type(node, 0, 1, node.typ)
}

fn (mut t Transformer) propagate_decl_pair_type(node flat.Node, lhs_idx int, rhs_idx int, fallback_type_id u32) {
	if lhs_idx >= node.children_count || rhs_idx >= node.children_count { return }
	lhs_id := t.a.child(&node, lhs_idx)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 { return }
	mut typ := u32(0)
	rhs_id := t.a.child(&node, rhs_idx)
	rhs := t.a.nodes[int(rhs_id)]
	authority := t.decl_rhs_type(rhs_id)
	if t.is_fn_pointer_type_name(authority) || t.decl_type_should_override_fallback(authority, fallback_type_id, rhs) {
		typ = authority
	} else if decl_type_is_usable(fallback_type_id) {
		typ = fallback_type_id
	} else if decl_type_is_usable(authority) {
		typ = authority
	} else if decl_type_is_usable(lhs.typ) {
		typ = lhs.typ
	}
	if typ != 0 { t.set_var_type_with_raw(lhs.value, t.normalize_type_alias(typ), typ) }
}

fn (t &Transformer) decl_type_should_override_fallback(authority_id u32, fallback_id u32, rhs flat.Node) bool {
	if !decl_type_is_usable(authority_id) { return false }
	if !decl_type_is_usable(fallback_id) { return true }
	if authority_id != fallback_id && rhs.kind in [.bool_literal, .char_literal, .float_literal,
		.int_literal, .string_literal, .string_interp] {
		return true
	}
	if t.local_struct_type_overrides_imported_alias(authority_id, fallback_id) { return true }
	if rhs.kind == .as_expr { return true }
	if rhs.kind == .ident && authority_id != fallback_id {
		if t.has_smartcast(rhs.value) || t.var_type(rhs.value) == authority_id { return true }
	}
	authority_shape := flat.source_type_shape(authority_id)
	fallback_shape := flat.source_type_shape(fallback_id)
	if rhs.kind == .call && authority_id != fallback_id && authority_shape.has(.dotted)
		&& flat.source_type_syntax(authority_id).unqualified == flat.source_type_syntax(fallback_id).unqualified {
		return true
	}
	if rhs.kind == .spawn_expr && fallback_id == transform_source_type_thread && authority_shape.has(.thread_type) {
		return true
	}
	if rhs.kind == .infix && rhs.op == .right_shift_unsigned { return true }
	// A mutable pointer parameter is represented by one extra C indirection. An
	// explicit source dereference is lowered through that slot, and its transformed
	// prefix is the reliable value type (`u64`), even if the original declaration
	// annotation still carries the parameter's pointer type (`&u64`).
	if t.decl_rhs_has_deref_tail(rhs) && authority_id != fallback_id && fallback_shape.has(.pointer) && !authority_shape.has(.pointer) {
		return true
	}
	if map_value_starts_with_fixed_array(authority_id) && fallback_shape.has(.map_type) {
		return true
	}
	return authority_shape.has(.pointer) && !fallback_shape.has(.pointer)
}

fn (t &Transformer) local_struct_type_overrides_imported_alias(authority_id u32, fallback_id u32) bool {
	mut local := t.trimmed_type_id(authority_id)
	mut imported := t.trimmed_type_id(fallback_id)
	for prefix in [flat.SourceTypeShape.array_type, .pointer, .option_type, .result_type] {
		for flat.source_type_shape(local).has(prefix) && flat.source_type_shape(imported).has(prefix) {
			local = flat.source_type_syntax(local).raw_base
			imported = flat.source_type_syntax(imported).raw_base
		}
	}
	return t.bare_struct_name_is_local_to_current_module(local)
		&& flat.source_type_shape(imported).has(.dotted)
		&& flat.source_type_syntax(imported).unqualified == local
}

fn map_value_starts_with_fixed_array(typ_id u32) bool {
	clean := transform_trimmed_type_id(typ_id)
	return flat.source_type_shape(clean).has(.map_type)
		&& flat.source_type_shape(flat.source_type_syntax(clean).raw_base).has(.leading_bracket)
}

fn decl_type_is_usable(typ_id u32) bool {
	return typ_id != 0 && !flat.source_type_shape(typ_id).has(.unusable_decl)
}

fn (t &Transformer) checker_expr_type_name(id flat.NodeId) ?u32 {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	mut name := u32(0)
	if typ := t.tc.expr_type_reference(id) {
		name = t.checker_recorded_type_name(id, typ)
	} else {
		typ := t.tc.resolve_type(id)
		name = t.checker_recorded_type_name(id, typ)
	}
	if t.tc.type_aliases.contains(flat.source_type_name(name)) {
		name = t.normalize_type_alias(name)
	}
	if flat.source_type_shape(name).has(.typeof_expr) {
		name = t.tc.type_name(t.tc.resolve_type(id))
		if t.tc.type_aliases.contains(flat.source_type_name(name)) {
			name = t.normalize_type_alias(name)
		}
	}
	if decl_type_is_usable(name) && name != flat.source_type_void {
		return name
	}
	return none
}

// Keep the indexed semantic identity through repeated reads of a checked
// composite expression. Calls retain their resolution/annotation precedence;
// literal and stored-name types already have a constant-time spelling.
fn (t &Transformer) checker_recorded_type_name(id flat.NodeId, typ &types.Type) u32 {
	composite := match typ {
		types.Array, types.ArrayFixed, types.Map, types.Pointer, types.FnType, types.OptionType,
		types.ResultType, types.Channel, types.MultiReturn {
			true
		}
		else { false }
	}
	if composite && int(id) >= 0 && int(id) < t.a.nodes.len
		&& t.a.nodes[int(id)].kind != .call {
		if type_id := t.tc.checked_expr_type_id(id) {
			return t.tc.type_name_by_id(type_id)
		}
	}
	return t.tc.type_name(typ)
}

fn (t &Transformer) decl_rhs_type(id flat.NodeId) u32 {
	// An explicit cast is the declaration's type authority even when its operand is
	// a function value. Looking through `u64(voidptr(C.linker_symbol))` first makes
	// the inferred local a function pointer and later drops both casts.
	if int(id) >= 0 {
		node := t.a.nodes[int(id)]
		if deref_type_source_id := t.decl_rhs_deref_type(id) {
			return deref_type_source_id
		}
		if node.kind == .cast_expr && node.source_value_id() != 0 {
			target_source_id := t.normalize_type_alias(node.source_value_id())
			target_source_id_2 := target_source_id

			if decl_type_is_usable(target_source_id_2) {
				return target_source_id
			}
		}
		// Primitive casts enter the first propagation pass as ordinary calls and
		// are lowered to `cast_expr` later. Preserve the callee's explicit type
		// before the checker's function-valued operand annotation can win.
		if node.kind == .call && node.children_count == 2 {
			callee := t.a.child_node(&node, 0)
			if callee.kind == .ident && callee.value in primitive_cast_type_names {
				return callee.source_value_id()
			}
		}
		// Infix expressions cannot be function values in the language. Prefer their
		// operand-derived result type over a stale checker annotation inherited from
		// a linker-symbol cast in an earlier declaration.
		if node.kind == .infix {
			infix_type_source_id := t.resolve_expr_type(id)
			infix_type_source_id_2 := infix_type_source_id

			if decl_type_is_usable(infix_type_source_id_2) {
				return infix_type_source_id
			}
		}
	}
	if fn_type_source_id := t.fn_value_type_name(id) {
		return fn_type_source_id
	}
	if map_type_source_id := t.map_expr_decl_type(id) {
		return map_type_source_id
	}
	if int(id) >= 0 {
		node := t.a.nodes[int(id)]
		match node.kind {
			.bool_literal {
				return flat.source_type_bool
			}
			.char_literal {
				return if node.value.starts_with('c:') {
					flat.source_type_id('&u8')
				} else {
					flat.source_type_rune
				}
			}
			.float_literal {
				return flat.source_type_f64
			}
			.int_literal {
				return flat.source_type_int
			}
			.string_literal, .string_interp {
				return flat.source_type_string
			}
			else {}
		}
		if node.kind == .spawn_expr {
			if spawn_type_source_id := t.spawn_expr_decl_type(node) {
				return spawn_type_source_id
			}
		}
		if node.kind == .as_expr && node.source_value_id() != 0 {
			return t.normalize_type_alias(node.source_value_id())
		}
		if node.kind == .selector {
			selector_type_source_id := t.resolve_selector_type(node)
			selector_type_source_id_2 := selector_type_source_id

			if decl_type_is_usable(selector_type_source_id_2) {
				return selector_type_source_id
			}
		}
		if node.kind == .call {
			if ret_source_id := t.checker_resolved_non_builtin_return_type(id, node) {
				return ret_source_id
			}
		}
		if node.kind == .ident {
			local_type_source_id := t.resolve_expr_type(id)
			local_type_source_id_3 := local_type_source_id

			if decl_type_is_usable(local_type_source_id_3) {
				return local_type_source_id
			}
		}
		if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			child_type_source_id := t.lvalue_type(child_id)
			if child.kind == .struct_init && t.is_optional_type_name(child_type_source_id) {
				wrapper := if flat.source_type_shape(child_type_source_id).has(.option_type) {
					flat.SourceTypeComposition.option
				} else {
					flat.SourceTypeComposition.result
				}
				return flat.source_type_composition_one(wrapper, flat.source_type_composition_one(.pointer, t.optional_base_type(child_type_source_id)))
			}
			prefix_type_source_id := t.node_type(id)
			prefix_type_source_id_2 := prefix_type_source_id

			if decl_type_is_usable(prefix_type_source_id_2) {
				return prefix_type_source_id
			}
			if child_type_source_id != 0 {
				return flat.source_type_composition_one(.pointer, child_type_source_id)
			}
		}
	}
	return t.node_type(id)
}

fn (t &Transformer) decl_rhs_has_deref_tail(node flat.Node) bool {
	if node.kind == .prefix {
		return node.op == .mul
	}
	if node.kind in [.block, .expr_stmt, .paren] && node.children_count > 0 {
		return t.decl_rhs_has_deref_tail(t.a.child_node(&node, node.children_count - 1))
	}
	return false
}

// decl_rhs_deref_type derives the value type of a dereference chain from the
// transformer's active binding. Mutable pointer parameters carry an additional
// ABI indirection (`mut p &T` is tracked as `&&T`), while stale checker
// annotations on `*p` can still say `&T`.
fn (t &Transformer) decl_rhs_deref_type(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	mut current := id
	mut node := t.a.nodes[int(current)]
	for node.kind in [.block, .expr_stmt, .paren] && node.children_count > 0 {
		current = t.a.child(&node, node.children_count - 1)
		node = t.a.nodes[int(current)]
	}
	mut derefs := 0
	for node.kind == .prefix && node.op == .mul && node.children_count > 0 {
		derefs++
		current = t.a.child(&node, 0)
		node = t.a.nodes[int(current)]
	}
	if derefs == 0 {
		return none
	}
	mut typ := (if node.kind == .ident {
		t.var_type(node.value)
	} else {
		t.node_type(current)
	})
	if node.kind == .ident && t.pointer_value_rvalues[node.value] && flat.source_type_shape(typ).has(.pointer) {
		typ = flat.source_type_syntax(typ).raw_base
	}
	for _ in 0 .. derefs {
		if !flat.source_type_shape(typ).has(.pointer) {
			return none
		}
		typ = flat.source_type_syntax(typ).raw_base
	}
	if decl_type_is_usable(typ) {
		return typ
	}
	return none
}

fn (t &Transformer) spawn_expr_decl_type(node flat.Node) ?u32 {
	if node.children_count == 0 {
		return none
	}
	call_id := t.a.child(&node, 0)
	call := t.a.node(call_id)
	if call.kind != .call || call.children_count == 0 {
		return none
	}
	mut ret_type := t.node_type(call_id)
	if (ret_type == 0 || ret_type == flat.source_type_unknown) && !isnil(t.tc) {
		callee := t.a.child_node(call, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base := t.a.child_node(callee, 0)
			base_type := (if base.kind == .ident {
				t.var_type(base.value)
			} else {
				t.node_type(t.a.child(callee, 0))
			})
			if info := t.tc.resolve_generic_struct_method(base_type, callee.value) {
				ret_type = info.return_type.source_type_id()
			}
		}
	}
	if ret_type == 0 || ret_type == flat.source_type_unknown {
		return none
	}
	if ret_type == flat.source_type_void {
		return flat.source_type_id('thread')
	}
	return flat.source_type_composition_one(.thread, ret_type)
}

fn (t &Transformer) map_expr_decl_type(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .map_init {
		for candidate in [node.source_value_id(), node.typ, t.node_type(id)] {
			if flat.source_type_shape(candidate).has(.map_type) {
				return t.refine_map_init_fixed_array_value_type(node, candidate)
			}
		}
	}
	if node.kind == .call {
		map_type_source_id := t.new_map_call_type(node)

		if map_type_source_id != 0 {
			return map_type_source_id
		}
		if flat.source_type_shape(node.typ).has(.map_type) {
			return node.typ
		}
	}
	return none
}

fn (t &Transformer) fn_value_type_name(id flat.NodeId) ?u32 {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .fn_literal {
		return t.fn_literal_type_text(node)
	}
	// The ordinary monomorphization scan has no live local scope. Its var_types
	// can still describe the last transformed function; only a cloned generic
	// specialization has seeded authoritative parameter bindings here.
	if node.kind == .ident && (!t.in_monomorphize_scan || t.cloning_generic_fn_depth > 0) {
		local_type_source_id := t.var_type(node.value)

		if local_type_source_id != 0 {
			if t.is_fn_pointer_type_name(local_type_source_id) {
				return t.normalize_type_alias(local_type_source_id)
			}
			// Generic parameters keep their concrete type in the local scope. A
			// same-named function must not turn a value parameter into a callback.
			return none
		}
	}
	if typ := t.tc.expr_type(id) {
		if name_source_id := fn_value_type_name_from_type(typ) {
			name_source_id_3 := name_source_id

			return t.normalize_type_alias(name_source_id_3)
		}
	}
	if node.kind == .ident {
		if name_source_id := fn_value_type_name_from_type(t.tc.resolve_type(id)) {
			name_source_id_4 := name_source_id

			return t.normalize_type_alias(name_source_id_4)
		}
		fn_name := t.tc.resolved_fn_value_name(id) or { node.value }
		params := t.tc.fn_param_types[fn_name] or { return none }
		ret := t.tc.fn_ret_types[fn_name] or { return none }
		return fn_literal_value_type_text(params, ret.source_type_id())
	}
	if node.kind == .lambda_expr {
		typ := t.tc.resolve_type(id)
		if name_source_id := fn_value_type_name_from_type(typ) {
			name_source_id_5 := name_source_id

			return t.normalize_type_alias(name_source_id_5)
		}
	}
	return none
}

fn (t &Transformer) fn_literal_type_text(node flat.Node) u32 {
	mut params := []u32{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		raw_id := if child.typ != 0 { child.typ } else { child.source_value_id() }
		slot_type_id := explicit_mut_pointer_param_type_text(child, fn_literal_param_type_text(raw_id))
		params << mutable_fn_value_param_type_text(child, slot_type_id)
	}
	mut ret_type := node.typ
	if inferred_type_id(ret_type) {
		mut param_nodes := []flat.Node{}
		for i in 0 .. node.children_count {
			child := t.a.child_node(&node, i)
			if child.kind == .param {
				param_nodes << *child
			}
		}
		ret_type = t.resolve_fn_literal_inferred_return_type(ret_type, param_nodes)
	}
	ret := transform_trimmed_type_id(ret_type)
	if ret == 0 || ret == flat.source_type_void {
		return flat.source_type_composition(.void_function_type_spaced, params)
	}
	params << ret
	return flat.source_type_composition(.function_type_spaced, params)
}

fn fn_literal_param_type_text(raw_id u32) u32 {
	return flat.source_type_syntax(raw_id).legacy_parameter_type
}

fn fn_value_type_name_from_type(typ types.Type) ?u32 {
	if typ is types.FnType {
		return types.Type(typ).source_type_id()
	}
	if typ is types.Alias && typ.base_type is types.FnType && typ.name_id != 0 {
		return typ.name_id
	}
	return none
}

fn fn_call_value_type_name_from_type(typ types.Type) ?u32 {
	if typ is types.FnType {
		for parameter in typ.params {
			if parameter.is_mut && parameter.typ is types.Pointer {
				return fn_literal_value_type_text(typ.params.map(*it.typ), typ.return_type.source_type_id())
			}
		}
	}
	return none
}

fn (t &Transformer) checker_call_fn_value_return_type(id flat.NodeId) ?u32 {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	name := t.tc.resolved_call_name(id) or { return none }
	ret_type := t.tc.fn_ret_types[name] or { return none }
	fn_value_type_source_id := fn_call_value_type_name_from_type(ret_type) or { return none }

	if t.generic_arg_is_unresolved(fn_value_type_source_id) {
		return none
	}
	return t.normalize_type_alias(fn_value_type_source_id)
}

fn (t &Transformer) concrete_node_type_name(node flat.Node) u32 {
	if node.typ == 0 {
		return u32(0)
	}
	typ_source_id := t.normalize_type_alias(node.typ)
	typ_source_id_2 := typ_source_id
	if !decl_type_is_usable(typ_source_id_2) {
		return u32(0)
	}
	if type_text_has_unresolved_generic_placeholder(typ_source_id_2) {
		return u32(0)
	}
	return typ_source_id
}

fn type_text_has_unresolved_generic_placeholder(typ_id u32) bool {
	if !flat.source_type_query(typ_id).has(.generic_token) { return false }
	clean := transform_trimmed_type_id(typ_id)
	if clean == 0 { return false }
	if flat.source_type_query(clean).has(.generic_fn_name) { return true }
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return type_text_has_unresolved_generic_placeholder(syntax.raw_base)
	}
	if shape.has(.map_type) && syntax.bracket_closed {
		return type_text_has_unresolved_generic_placeholder(syntax.bracket_raw_inner)
			|| type_text_has_unresolved_generic_placeholder(syntax.raw_base)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed {
		return type_text_has_unresolved_generic_placeholder(syntax.raw_base)
	}
	if !flat.source_type_query(clean).has(.generic_token) { return false }
	if !shape.has(.bracketed) { return true }
	if syntax.generic_app_valid {
		for arg in syntax.generic_app_args() {
			if type_text_has_unresolved_generic_placeholder(arg) { return true }
		}
	}
	return false
}

// resolve_selector_type resolves the type of a .selector node (e.g. `obj.field`).
// Looks up the base expression type, then finds the field in the struct definition.
fn (t &Transformer) resolve_selector_type(node flat.Node) u32 {
	if node.kind != .selector || node.children_count == 0 || t.smartcast_stack.len > 0
		|| isnil(t.selector_type_cache) {
		return t.resolve_selector_type_uncached(node)
	}
	mut cache := t.selector_type_cache
	slot := int((u64(node.children_start) * 2654435761 ^ (u64(voidptr(node.value.str)) >> 4)) & 1023)
	if cache.generations[slot] == cache.generation && cache.keys[slot] == node.children_start
		&& cache.value_ptrs[slot] == voidptr(node.value.str)
		&& cache.value_lens[slot] == node.value.len {
		return cache.results[slot]
	}
	result_source_id := t.resolve_selector_type_uncached(node)

	cache.keys[slot] = node.children_start
	cache.value_ptrs[slot] = voidptr(node.value.str)
	cache.value_lens[slot] = node.value.len
	cache.generations[slot] = cache.generation
	cache.results[slot] = result_source_id
	return result_source_id
}

fn (t &Transformer) resolve_selector_type_uncached(node flat.Node) u32 {
	if node.kind != .selector || node.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.child(&node, 0)
	base_node := t.a.nodes[int(base_id)]
	if base_node.kind == .ident && base_node.value == 'C' {
		// `C.<name>` references a C symbol (e.g. the `C.stdout &C.FILE` global).
		// Resolve it via the recorded C global/const type and never fall through to
		// the V struct field-name heuristics below: an unrelated V struct may have a
		// field with the same name (e.g. `os.Pipe.stdout`), which would otherwise
		// mistype `C.stdout` and trigger a bogus auto-reference (`&stdout`).
		if is_c_int_selector(node.value) {
			return flat.source_type_int
		}
		cname := 'C.${node.value}'
		if gt_source_id := t.globals.type_id(cname) {
			if gt_source_id != 0 {
				return t.normalize_type_alias(gt_source_id)
			}
		}
		if typ_source_id := t.const_type_name(cname) {
			return typ_source_id
		}
		return u32(0)
	}
	if !isnil(t.tc) {
		if typ := t.tc.selector_const_type(node) {
			return t.normalize_type_alias(typ.source_type_id())
		}
	}
	field_name := node.value
	if field_name.len == 0 {
		return u32(0)
	}
	enum_may_match := t.enum_selector_may_match(field_name)
	if enum_may_match {
		base_selector_name := t.selector_expr_name(base_id)
		if base_selector_name.len > 0 {
			if enum_type_source_id := t.enum_type_name_from_selector_name(base_selector_name) {
				enum_type_source_id_2 := enum_type_source_id
				enum_type := flat.source_type_name(enum_type_source_id_2)

				if fields := t.enum_types[enum_type] {
					if field_name in fields {
						return enum_type_source_id_2
					}
				}
			}
			if type_name_source_id := t.qualified_enum_type_selector_name_from_selector_name(base_selector_name, field_name) {
				type_name_source_id_2 := type_name_source_id

				return type_name_source_id_2
			}
		}
	}
	if base_node.kind == .ident {
		if typ_source_id := t.const_type_name('${base_node.value}.${field_name}') {
			return typ_source_id
		}
		if enum_may_match {
			if fields := t.enum_types[base_node.value] {
				if field_name in fields {
					return base_node.source_value_id()
				}
			}
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qenum := '${t.cur_module}.${base_node.value}'
				if fields := t.enum_types[qenum] {
					if field_name in fields {
						return flat.source_type_id(qenum)
					}
				}
			}
		}
	}
	base_type_source_id := t.resolve_expr_type(base_id)
	if base_type_source_id == 0 {
		if ftyp_source_id := t.lookup_unique_field_type(field_name) {
			return ftyp_source_id
		}
		return u32(0)
	}
	if builtin_type_source_id := t.builtin_selector_type(base_type_source_id, field_name) {
		return builtin_type_source_id
	}
	if t.smartcast_stack.len > 0 {
		base_key := t.expr_key(base_id)
		if base_key.len > 0 {
			full_key := '${base_key}.${field_name}'
			if sc := t.find_smartcast(full_key) {
				return t.smartcast_target_type(sc)
			}
			if sc := t.find_smartcast(base_key) {
				variant_type_source_id := t.qualify_variant(sc.variant_name, sc.sum_type_name)
				if ftyp_source_id := t.lookup_struct_field_type(variant_type_source_id, field_name) {
					return ftyp_source_id
				}
				if ftyp_source_id := t.lookup_struct_field_type(sc.variant_name, field_name) {
					return ftyp_source_id
				}
			}
		}
	}
	// Strip pointer prefix if present (e.g. "&MyStruct" -> "MyStruct")
	lookup_type := if flat.source_type_shape(base_type_source_id).has(.pointer) {
		flat.source_type_syntax(base_type_source_id).raw_base
	} else {
		base_type_source_id
	}
	if ftyp_source_id := t.lookup_sum_variant_field_type(lookup_type, field_name) {
		return ftyp_source_id
	}
	if ftyp_source_id := t.lookup_struct_field_type(lookup_type, field_name) {
		return ftyp_source_id
	}
	if _ := t.lookup_struct_info(lookup_type) {
		// Follow the complete embedding path, including pointer embeddings.
		if path := t.struct_field_path_for_field(lookup_type, field_name) {
			owner_type := (if path.len > 0 {
				t.trim_pointer_type(path.last().typ)
			} else {
				lookup_type
			})
			if owner_info := t.lookup_struct_info(owner_type) {
				if ftyp_source_id := t.struct_field_type(owner_info, field_name) {
					ftyp_source_id_2 := ftyp_source_id

					return ftyp_source_id_2
				}
			}
		}
		// A selector on a known struct may be a bound method. Do not infer its
		// type from a same-named field on an unrelated struct; let the checker
		// supply the method's function type instead.
		return u32(0)
	}
	// An interface declares the types of its fields: do not guess them from the name.
	iface_name_source_id := t.resolve_interface_type_name(lookup_type)

	if iface_name_source_id != 0 {
		if ftyp_source_id := t.interface_field_type_name(iface_name_source_id, field_name) {
			return ftyp_source_id
		}
	}
	if ftyp_source_id := t.lookup_unique_field_type(field_name) {
		return ftyp_source_id
	}
	return u32(0)
}

fn (t &Transformer) builtin_selector_type(base_type_id u32, field_name string) ?u32 {
	clean_source_id := t.trim_pointer_type(t.normalize_type_alias(base_type_id))
	clean_shape := flat.source_type_shape(clean_source_id)
	if clean_source_id == transform_source_type_array || clean_shape.has(.array_type) {
		// Array header fields have builtin types. Inferring them from a same-named
		// field on an unrelated struct can turn a pointer value into an implicit
		// reference when passing `values.data` to a V function such as vmemcpy.
		return match field_name {
			'data' { flat.source_type_voidptr }
			'element_size', 'offset', 'len', 'cap' { flat.source_type_int }
			'flags' { flat.source_type_id('ArrayFlags') }
			else { none }
		}
	}
	if field_name == 'len' && (clean_source_id == flat.source_type_string || clean_shape.has(.map_type) || t.is_fixed_array_type(clean_source_id)
		|| clean_source_id == flat.source_type_id('chan') || clean_shape.has(.channel_type)) {
		return flat.source_type_int
	}
	if clean_source_id == flat.source_type_id('chan') || clean_shape.has(.channel_type) {
		if field_name == 'cap' {
			return flat.source_type_int
		}
		if field_name == 'closed' {
			return flat.source_type_bool
		}
	}
	return none
}

fn (t &Transformer) lookup_sum_variant_field_type(sum_type_id u32, field_name string) ?u32 {
	return t.lookup_sum_variant_field_type_seen(sum_type_id, field_name, []u32{})
}

fn (t &Transformer) lookup_sum_variant_field_type_seen(sum_type_id u32, field_name string, seen_ids []u32) ?u32 {
	resolved := t.resolve_sum_name(t.trim_pointer_type(t.normalize_type_alias(sum_type_id)))
	if resolved == 0 || resolved in seen_ids { return none }
	variants := t.sum_types.type_ids(flat.source_type_name(resolved)) or { return none }
	mut next_seen := seen_ids.clone()
	next_seen << resolved
	mut found := u32(0)
	for variant in variants {
		mut ftyp := u32(0)
		if vt := t.lookup_struct_field_type(variant, field_name) {
			ftyp = t.normalize_type_alias(vt)
		} else if vt := t.checker_struct_field_type_name(variant, field_name) {
			ftyp = t.normalize_type_alias(vt)
		} else if vt := t.lookup_sum_variant_field_type_seen(variant, field_name, next_seen) {
			ftyp = t.normalize_type_alias(vt)
		}
		if ftyp == 0 { continue }
		if found != 0 && found != ftyp { return none }
		found = ftyp
	}
	if found == 0 { return none }
	return found
}

// is_c_int_selector reports whether is c int selector applies in transform.
fn is_c_int_selector(name string) bool {
	return name in ['errno', 'EINTR', 'STDOUT_FILENO', 'STDERR_FILENO', 'EINVAL', 'SOMAXCONN']
}

// selector_expr_name supports selector expr name handling for Transformer.
fn (t &Transformer) selector_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.selector_expr_name(t.a.child(&node, 0))
		if base.len > 0 && node.value.len > 0 {
			return '${base}.${node.value}'
		}
	}
	return ''
}

// enum_type_name_from_expr converts enum type name from expr data for transform.
fn (t &Transformer) enum_type_name_from_expr(id flat.NodeId) ?u32 {
	name := t.selector_expr_name(id)
	return t.enum_type_name_from_selector_name(name)
}

fn (t &Transformer) enum_type_name_from_selector_name(name string) ?u32 {
	if name == '' {
		return none
	}
	if name in t.enum_types {
		return flat.source_type_id(name)
	}
	resolved_source_id_3 := t.normalize_type_in_module(flat.source_type_id(name), t.cur_module)
	resolved := flat.source_type_name(resolved_source_id_3)
	if resolved in t.enum_types {
		return resolved_source_id_3
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return flat.source_type_id(qname)
		}
	}
	return none
}

// qualified_enum_type_selector_name
// supports helper handling in transform.
fn (t &Transformer) qualified_enum_type_selector_name(base_id flat.NodeId, field_name string) ?u32 {
	base := t.selector_expr_name(base_id)
	return t.qualified_enum_type_selector_name_from_selector_name(base, field_name)
}

fn (t &Transformer) qualified_enum_type_selector_name_from_selector_name(base string, field_name string) ?u32 {
	if base == '' || field_name == '' {
		return none
	}
	name := '${base}.${field_name}'
	if name in t.enum_types {
		return flat.source_type_id(name)
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return flat.source_type_id(qname)
		}
	}
	return none
}

// lookup_struct_field_type resolves lookup struct field type information for transform.
fn (t &Transformer) lookup_struct_field_type(type_name_id u32, field_name string) ?u32 {
	if type_name_id == 0 || field_name == '' || isnil(t.struct_field_type_cache) {
		return t.lookup_struct_field_type_keyed(type_name_id, field_name)
	}
	mut cache := t.struct_field_type_cache
	mut recent := cache.recent_cache()
	state, cached := recent.get(t.cur_module, type_name_id, field_name)
	if state > 0 { return cached }
	if state < 0 { return none }
	if resolved := t.lookup_struct_field_type_keyed(type_name_id, field_name) {
		recent.put(t.cur_module, type_name_id, field_name, 1, resolved)
		return resolved
	}
	recent.put(t.cur_module, type_name_id, field_name, -1, 0)
	return none
}

fn (t &Transformer) lookup_struct_field_type_keyed(type_name_id u32, field_name string) ?u32 {
	if type_name_id == 0 || field_name == '' {
		return none
	}
	if isnil(t.struct_field_type_cache) {
		return t.lookup_struct_field_type_uncached(type_name_id, field_name)
	}
	// The answer depends on the current module, the owner type and the field
	// name. Lookups are grouped by module so the key stays numeric; the field
	// name is interned through the cache's own recent table.
	mut cache := t.struct_field_type_cache
	mut module_cache := cache.module_cache(t.cur_module)
	key := (u64(type_name_id) << 32) | u64(cache.contexts.intern(field_name))
	if cached := module_cache.entries[key] {
		if cached == field_type_cache_miss {
			return none
		}
		return cached
	}
	resolved_source_id := t.lookup_struct_field_type_uncached(type_name_id, field_name) or {
		module_cache.entries[key] = field_type_cache_miss
		return none
	}
	module_cache.entries[key] = resolved_source_id
	return resolved_source_id
}

fn (t &Transformer) lookup_struct_field_type_uncached(type_name_id u32, field_name string) ?u32 {
	lookup := t.lookup_struct_info_for_field(type_name_id, field_name) or { return none }
	return t.struct_field_type_in_cur_module(lookup, t.lookup_struct_field_type_spelling(lookup,
		field_name)?)
}

fn (t &Transformer) lookup_struct_field_type_spelling(lookup StructFieldLookup, field_name string) ?u32 {
	f := lookup.info.field(field_name) or { return none }
	if flat.source_type_shape(lookup.owner_type).has(.bracketed) {
		specialized_source_id := t.normalize_field_type(f.typ, lookup.owner_type)
		specialized_source_id_2 := specialized_source_id

		if decl_type_is_usable(specialized_source_id_2) && !t.generic_arg_is_unresolved(specialized_source_id) {
			return specialized_source_id
		}
	}
	if checker_typ_source_id := t.checker_struct_field_type_name(lookup.owner_type, field_name) {
		return checker_typ_source_id
	}
	if !flat.source_type_shape(lookup.owner_type).has(.bracketed) {
		return f.typ
	}
	return t.normalize_field_type(f.typ, lookup.owner_type)
}

// struct_field_type_in_cur_module spells a field type for use in the current module.
// Main-module types are stored bare, so a `[]Cell` field of a main struct read inside
// an imported generic specialization must stay `main.Cell` instead of rebinding to the
// current module's own `Cell` when the text is parsed again.
fn (t &Transformer) struct_field_type_in_cur_module(lookup StructFieldLookup, field_type_id u32) u32 {
	if t.cur_module in ['', 'main', 'builtin'] || lookup.info.module !in ['', 'main'] {
		return field_type_id
	}
	return t.lock_colliding_main_generic_type_text(field_type_id, t.cur_module)
}

// lookup_struct_field_raw_type resolves lookup struct field raw type information for transform.
fn (t &Transformer) lookup_struct_field_raw_type(type_name_id u32, field_name string) ?u32 {
	lookup := t.lookup_struct_info_for_field(type_name_id, field_name) or { return none }
	f := lookup.info.field(field_name) or { return none }
	if f.raw_typ != 0 {
		return f.raw_typ
	}
	return f.typ
}

fn (t &Transformer) lookup_struct_field_raw_type_with_owner(type_name_id u32, field_name string) ?(u32, u32) {
	lookup := t.lookup_struct_info_for_field(type_name_id, field_name) or { return none }
	f := lookup.info.field(field_name) or { return none }
	raw_id := if f.raw_typ != 0 { f.raw_typ } else { f.typ }
	owner_id := if flat.source_type_shape(lookup.owner_type).has(.dotted) || lookup.info.module.len == 0
		|| lookup.info.module == 'main' || lookup.info.module == 'builtin' {
		lookup.owner_type
	} else {
		flat.source_type_composition_two(.qualified, flat.source_type_id(lookup.info.module), lookup.owner_type)
	}
	return raw_id, owner_id
}

fn (t &Transformer) checker_struct_field_type_name(type_name_id u32, field_name string) ?u32 {
	if isnil(t.tc) {
		return none
	}
	mut lookup_type := if flat.source_type_shape(type_name_id).has(.pointer) {
		flat.source_type_syntax(type_name_id).raw_base
	} else {
		type_name_id
	}
	if checker_typ_source_id := t.tc.struct_field_type_name(lookup_type, field_name) {
		return checker_typ_source_id
	}
	lookup_syntax := flat.source_type_syntax(lookup_type)
	if lookup_syntax.qualified_name_chars {
		short := lookup_syntax.unqualified
		if checker_typ_source_id := t.tc.struct_field_type_name(short, field_name) {
			return checker_typ_source_id
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qtype := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), lookup_type)
		if checker_typ_source_id := t.tc.struct_field_type_name(qtype, field_name) {
			return checker_typ_source_id
		}
	}
	normalized_source_id := t.normalize_type_alias(lookup_type)
	if normalized_source_id != lookup_type {
		if checker_typ_source_id := t.tc.struct_field_type_name(normalized_source_id, field_name) {
			return checker_typ_source_id
		}
	}
	return none
}

// lookup_struct_info_for_field resolves lookup struct info for field information for transform.
fn (t &Transformer) lookup_struct_info_for_field(type_name_id u32, field_name string) ?StructFieldLookup {
	if type_name_id == u32(0) || field_name == '' {
		return none
	}
	mut lookup_type := if flat.source_type_shape(type_name_id).has(.pointer) {
		flat.source_type_syntax(type_name_id).raw_base
	} else {
		type_name_id
	}
	owner_type_id := lookup_type
	lookup_syntax := flat.source_type_syntax(lookup_type)
	if lookup_syntax.generic_app_valid {
		lookup_type = lookup_syntax.generic_app_base
	}
	// `main.Foo` locks a program-module type (see lookup_struct_info): its bare
	// table key must not be requalified into a current module with its own `Foo`.
	lookup_type_syntax := if lookup_syntax.generic_app_valid {
		flat.source_type_syntax(lookup_type)
	} else {
		lookup_syntax
	}
	if flat.source_type_query(lookup_type).has(.main_prefix) && !flat.source_type_shape(lookup_type_syntax.main_base).has(.dotted) {
		if info := t.structs[flat.source_type_name(lookup_type_syntax.main_base)] {
			if info.module.len == 0 || info.module == 'main' {
				return StructFieldLookup{
					info:       info
					owner_type: owner_type_id
				}
			}
		}
	}
	if flat.source_type_name(lookup_type) !in t.structs && lookup_type_syntax.qualified_name_chars {
		short_type := lookup_type_syntax.unqualified
		if flat.source_type_name(short_type) in t.structs {
			lookup_type = short_type
		}
	}
	if !flat.source_type_shape(lookup_type).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtype_id := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), lookup_type)
		qtype := flat.source_type_name(qtype_id)
		if qtype in t.structs {
			lookup_type = qtype_id
		} else if !isnil(t.tc) {
			if target_source_id := t.tc.type_aliases.type_id(qtype) {
				target_source_id_3 := target_source_id

				lookup_type = target_source_id_3
			}
		}
	}
	if flat.source_type_name(lookup_type) !in t.structs {
		normalized_source_id_2 := t.normalize_type_alias(lookup_type)
		if normalized_source_id_2 != lookup_type {
			lookup_type = normalized_source_id_2
		}
	}
	info := t.structs[flat.source_type_name(lookup_type)] or { return none }
	return StructFieldLookup{
		info:       info
		owner_type: owner_type_id
	}
}

// checked_global_type_name returns the type that the checker inferred for a global
// declared with an initializer.
fn (t &Transformer) checked_global_type_name(name string, mod string) ?u32 {
	if isnil(t.tc) || isnil(t.tc.file_scope) {
		return none
	}
	qname := if mod.len > 0 && mod != 'main' && mod != 'builtin' { '${mod}.${name}' } else { name }
	typ := t.tc.file_scope.lookup(qname) or { return none }
	if typ is types.Unknown || typ is types.Void {
		return none
	}
	text := typ.source_type_id()
	if text == 0 || type_text_has_unresolved_generic_placeholder(text) {
		return none
	}
	return text
}

// lookup_unique_field_type resolves lookup unique field type information for transform.
fn (t &Transformer) lookup_unique_field_type(field_name string) ?u32 {
	match field_name {
		'name' {
			return flat.source_type_string
		}
		'generic_types' {
			return flat.source_type_id('[]u32')
		}
		'obj' {
			return t.resolve_sum_name(flat.source_type_id('ScopeObject'))
		}
		else {}
	}

	if ftyp_source_id := t.unique_fields.type_id(field_name) {
		if ftyp_source_id != 0 {
			return ftyp_source_id
		}
	}
	return none
}

// normalize_field_type transforms normalize field type data for transform.
fn (t &Transformer) normalize_field_type(typ_id u32, owner_type_id u32) u32 {
	return t.normalize_field_type_with_owner_substitution(typ_id, owner_type_id, true)
}

fn (t &Transformer) normalize_field_type_with_owner_substitution(typ_id u32, owner_type_id u32, allow_owner_substitution bool) u32 {
	if typ_id == 0 { return typ_id }
	shape := flat.source_type_shape(typ_id)
	syntax := flat.source_type_syntax(typ_id)
	if shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.atomic_type) || shape.has(.pointer) || shape.has(.array_type) {
		kind := if shape.has(.mutable_type) {
			flat.SourceTypeComposition.mutable_parameter
		} else if shape.has(.shared_type) {
			flat.SourceTypeComposition.shared_type
		} else if shape.has(.atomic_type) {
			flat.SourceTypeComposition.atomic_type
		} else if shape.has(.pointer) {
			flat.SourceTypeComposition.pointer
		} else {
			flat.SourceTypeComposition.array
		}
		return flat.source_type_composition_one(kind, t.normalize_field_type_with_owner_substitution(syntax.raw_base, owner_type_id, allow_owner_substitution))
	}
	// `?T` of an option `T` is that option, as the checker substitutes it.
	if shape.has(.option_type) || shape.has(.result_type) {
		inner := t.normalize_field_type_with_owner_substitution(syntax.raw_base, owner_type_id, allow_owner_substitution)
		is_option := shape.has(.option_type)
		if (is_option && flat.source_type_shape(inner).has(.option_type)) || (!is_option && flat.source_type_shape(inner).has(.result_type)) {
			return inner
		}
		return flat.source_type_composition_one(if is_option {
			flat.SourceTypeComposition.option
		} else {
			flat.SourceTypeComposition.result
		}, inner)
	}
	if shape.has(.map_type) {
		if !syntax.first_bracket_closed { return t.normalize_type_alias(typ_id) }
		key_type := t.normalize_field_type_with_owner_substitution(syntax.first_bracket_raw_inner, owner_type_id, allow_owner_substitution)
		value_type := t.normalize_field_type_with_owner_substitution(syntax.first_bracket_base, owner_type_id, allow_owner_substitution)
		return flat.source_type_composition_two(.map, key_type, value_type)
	}
	owner := flat.source_type_syntax(owner_type_id)
	if shape.has(.leading_bracket) {
		if !syntax.first_bracket_closed { return t.normalize_type_alias(typ_id) }
		mut length := syntax.first_bracket_raw_inner
		if allow_owner_substitution && owner.generic_app_valid && owner.generic_app_base != 0 {
			params := t.generic_struct_param_names_for_base_id(owner.generic_app_base)
			// Array lengths are source expressions; their substitution edits expression text.
			length = flat.source_type_id(substitute_generic_expr_text_with_params(flat.source_type_name(length), owner.generic_app_args(), params))
		}
		return flat.source_type_composition_two(.fixed_array, length, t.normalize_field_type_with_owner_substitution(syntax.first_bracket_base, owner_type_id, allow_owner_substitution))
	}
	if allow_owner_substitution && owner.generic_app_valid && owner.generic_app_base != 0 {
		params := t.generic_struct_param_names_for_base_id(owner.generic_app_base)
		// Arguments belong to the instantiation site, rather than the module
		// declaring the generic field. Qualify them before owner substitution.
		mut qualified_args := []u32{cap: owner.generic_app_args().len}
		for arg in owner.generic_app_args() {
			qualified_args << t.qualify_generic_arg_for_decl_module(arg, t.cur_module)
		}
		substituted := if params.len > 0 {
			substitute_generic_type_text_with_params(typ_id, qualified_args, params)
		} else {
			substitute_generic_type_text(typ_id, qualified_args)
		}
		if substituted != typ_id {
			return t.normalize_field_type_with_owner_substitution(substituted, owner_type_id, false)
		}
	}
	if syntax.generic_app_valid {
		mut field_base := syntax.generic_app_base
		if !flat.source_type_shape(field_base).has(.dotted) && flat.source_type_shape(owner_type_id).has(.dotted) {
			qbase := flat.source_type_composition_two(.qualified, owner.field_owner_module, field_base)
			if t.type_authority_has(flat.source_type_name(qbase)) { field_base = qbase }
		}
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		parts << field_base
		for arg in syntax.generic_app_args() {
			parts << t.normalize_field_type_with_owner_substitution(arg, owner_type_id, allow_owner_substitution)
		}
		return t.normalize_type_alias(flat.source_type_composition(.generic, parts))
	}
	if shape.has(.dotted) || !flat.source_type_shape(owner_type_id).has(.dotted) {
		return t.normalize_type_alias(typ_id)
	}
	qtyp := flat.source_type_composition_two(.qualified, owner.field_owner_module, typ_id)
	if t.type_authority_has(flat.source_type_name(qtyp)) { return t.normalize_type_alias(qtyp) }
	if !isnil(t.tc) && t.tc.type_aliases.contains(flat.source_type_name(qtyp)) {
		return t.normalize_type_alias(qtyp)
	}
	return t.normalize_type_alias(typ_id)
}

fn field_owner_module(owner_type string) string {
	return flat.source_type_name(flat.source_type_syntax(flat.source_type_id(owner_type)).field_owner_module)
}

fn (t &Transformer) generic_struct_param_names_for_base(base string) []string {
	return t.generic_struct_param_names_for_base_id(flat.source_type_id(base))
}

fn (t &Transformer) generic_struct_param_names_for_base_id(base u32) []string {
	if isnil(t.tc) { return []string{} }
	if params := t.tc.struct_generic_params[flat.source_type_name(base)] { return params.clone() }
	short := flat.source_type_syntax(base).unqualified
	if params := t.tc.struct_generic_params[flat.source_type_name(short)] { return params.clone() }
	if !flat.source_type_shape(base).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
		'main',
		'builtin',
	] {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
		if params := t.tc.struct_generic_params[flat.source_type_name(qualified)] {
			return params.clone()
		}
	}
	return []string{}
}

fn (t &Transformer) type_authority_has(name string) bool {
	if name in t.structs || t.sum_types.contains(name) || name in t.enum_types {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return name in t.tc.structs || t.tc.sum_types.contains(name) || name in t.tc.enum_names
		|| name in t.tc.interface_names || t.tc.type_aliases.contains(name)
}

// normalize_type_alias resolves an interned spelling while retaining its numeric identity.
@[direct_array_access; inline]
fn (t &Transformer) normalize_type_alias(type_id u32) u32 {
	if type_id == 0 || isnil(t.tc) { return type_id }
	// Canonical entries are plain builtins or declared dotted names; neither
	// can contain an inferred witness. Reuse that proof before querying flags.
	if !isnil(t.alias_cache) && voidptr(t.alias_cache.texts) == voidptr(t.var_type_texts) {
		c := t.alias_cache
		recent_slot := int(type_id & 1023)
		if c.canonical_types[recent_slot] == type_id { return type_id }
		// Only witness-free IDs enter this cache. A matching recent entry can
		// therefore skip the witness query, retaining every context guard.
		if c.recent_generations[recent_slot] == c.recent_generation
			&& c.recent_types[recent_slot] == type_id
			&& same_transform_text(c.module, t.cur_module)
			&& same_transform_text(c.file, t.cur_file) {
			return c.recent_results[recent_slot]
		}
	}
	if inferred_type_id(type_id) {
		resolved := t.resolve_inferred_type_id(type_id)
		if resolved != type_id { return t.normalize_type_alias(resolved) }
		return type_id
	}
	return t.normalize_type_alias_cached(type_id)
}

fn (t &Transformer) normalize_type_alias_cached(type_id u32) u32 {
	if isnil(t.alias_cache) {
		return t.normalize_type_alias_uncached(type_id)
	}
	mut c := t.alias_cache
	if voidptr(c.texts) != voidptr(t.var_type_texts) {
		// Hand-built integrations may supply a cache from another table owner.
		// Cache entries are disposable; spelling identities remain process-wide.
		c.texts = t.var_type_texts
		c.entries.clear()
		c.canonical_types = [1024]u32{}
		c.clear_recent()
	}
	recent_slot := int(type_id & 1023)
	if c.canonical_types[recent_slot] == type_id { return type_id }
	if !same_transform_text(c.module, t.cur_module) || !same_transform_text(c.file, t.cur_file) {
		c.module = t.cur_module
		c.file = t.cur_file
		c.entries.clear()
		c.clear_recent()
	}
	if c.recent_generations[recent_slot] == c.recent_generation
		&& c.recent_types[recent_slot] == type_id {
		return c.recent_results[recent_slot]
	}
	if result_id := c.entries[type_id] {
		c.recent_types[recent_slot] = type_id
		c.recent_results[recent_slot] = result_id
		c.recent_generations[recent_slot] = c.recent_generation
		return result_id
	}
	// The normal entry point resolves witnesses before reaching this helper.
	// Its direct callers must not seed scope-dependent witness entries either.
	if inferred_type_id(type_id) {
		return t.normalize_type_alias_uncached(type_id)
	}
	// Only a miss crosses back to the source/alias spelling resolver.
	if t.normalize_dotted_canonical_fast(type_id) {
		result_id := type_id
		c.entries[type_id] = result_id
		c.recent_types[recent_slot] = type_id
		c.recent_results[recent_slot] = result_id
		c.recent_generations[recent_slot] = c.recent_generation
		c.canonical_types[recent_slot] = type_id
		return result_id
	}
	result_id := t.normalize_type_alias_uncached(type_id)
	c.entries[type_id] = result_id
	c.recent_types[recent_slot] = type_id
	c.recent_results[recent_slot] = result_id
	c.recent_generations[recent_slot] = c.recent_generation
	if result_id == type_id && is_plain_builtin_alias_type(type_id) {
		c.canonical_types[recent_slot] = type_id
	}
	return result_id
}

// normalize_dotted_canonical_fast answers the most common per-file cache miss
// without the full uncached walk: a plain `module.Type` spelling that is not an
// alias key and is declared in the authority tables is already canonical (the
// uncached walk's qualified-name branch returns it before consulting imports,
// so the answer is file-independent). Any other shape falls through.
@[direct_array_access; inline]
fn (t &Transformer) normalize_dotted_canonical_fast(type_id u32) bool {
	if !flat.source_type_shape(type_id).has(.dotted)
		|| !flat.source_type_syntax(type_id).canonical_dotted_name {
		return false
	}
	typ := flat.source_type_name(type_id)
	if t.tc.type_aliases.contains(typ) { return false }
	return t.type_authority_has(typ)
}

// normalize_type_alias_uncached resolves aliases while preserving source spelling identities.
fn (t &Transformer) normalize_type_alias_uncached(type_id u32) u32 {
	if flat.source_type_query(type_id).has(.generic_name) && !t.is_known_type_name(type_id) {
		return type_id
	}
	shape := flat.source_type_shape(type_id)
	if shape.has(.mutable_type) {
		return flat.source_type_composition_one(.pointer, t.normalize_type_alias(flat.source_type_syntax(type_id).raw_base))
	}
	if shape.has(.pointer) {
		syntax := flat.source_type_syntax(type_id)
		if syntax.void_pointer_postfix {
			return flat.source_type_composition_two(.concat, flat.source_type_voidptr, syntax.bracket_from)
		}
		return flat.source_type_composition_one(.pointer, t.normalize_type_alias(syntax.raw_base))
	}
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array, t.normalize_type_alias(flat.source_type_syntax(type_id).raw_base))
	}
	if shape.has(.channel_type) {
		return flat.source_type_composition_one(.channel, t.normalize_type_alias(flat.source_type_syntax(type_id).raw_base))
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(type_id)
		if !syntax.first_bracket_closed { return type_id }
		return flat.source_type_composition_two(.concat, syntax.first_bracket_prefix, t.normalize_type_alias(syntax.first_bracket_base))
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(type_id)
		if syntax.bracket_closed {
			key := t.normalize_type_alias(syntax.bracket_raw_inner)
			value := t.normalize_type_alias(syntax.raw_base)
			return flat.source_type_composition_two(.map, key, value)
		}
	}
	if shape.has(.option_type) {
		return flat.source_type_composition_one(.option, t.normalize_type_alias(flat.source_type_syntax(type_id).raw_base))
	}
	if shape.has(.result_type) {
		return flat.source_type_composition_one(.result, t.normalize_type_alias(flat.source_type_syntax(type_id).raw_base))
	}
	typ := flat.source_type_name(type_id)
	if expanded_source_id := t.expand_generic_type_alias(type_id) {
		return t.normalize_type_alias(expanded_source_id)
	}
	// A fully qualified type is already canonical. Resolve its exact alias before
	// consulting imports from the current file; otherwise an unrelated import with
	// the same trailing type name can steal the identity (for example
	// `v.ssa.TypeID` being resolved as `v.types.Type`).
	if shape.has(.dotted) {
		if target_source_id := t.tc.type_aliases.type_id(typ) {
			return t.normalize_type_alias(target_source_id)
		}
		if t.type_authority_has(typ) {
			return type_id
		}
	}
	// Resolve the importing file's alias before any short-name or suffix fallback.
	// Two packages can both expose `tast.Value`; `other_tast.Value` and
	// `tast.Value` must retain their exact canonical module identities.
	if imported_source_id := t.resolve_imported_type_name(type_id) {
		imported := flat.source_type_name(imported_source_id)
		if target_source_id := t.tc.type_aliases.type_id(imported) {
			return t.normalize_type_alias(target_source_id)
		}
		if t.type_authority_has(imported) {
			return imported_source_id
		}
	}
	if !shape.has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qualified_id := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), type_id)
		qtyp := flat.source_type_name(qualified_id)
		if target_source_id := t.tc.type_aliases.type_id(qtyp) {
			return target_source_id
		}
		if t.type_authority_has(qtyp) {
			return qualified_id
		}
	}
	// Imported aliases are indexed by their short spelling. A main-module struct
	// with the same name remains authoritative for an unqualified reference.
	if t.bare_struct_name_is_local_to_current_module(type_id) {
		return type_id
	}
	if target_source_id := t.tc.type_aliases.type_id(typ) {
		return target_source_id
	}
	if typ in t.structs || t.sum_types.contains(typ) || typ in t.enum_types {
		return type_id
	}
	if !shape.has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qualified_id := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), type_id)
		qtyp := flat.source_type_name(qualified_id)
		if target_source_id := t.tc.type_aliases.type_id(qtyp) {
			return target_source_id
		}
	}
	if !shape.has(.dotted) {
		if unique_target := t.type_alias_suffixes[typ] {
			if unique_target != 0 {
				return unique_target
			}
		}
	}
	return type_id
}

fn (mut t Transformer) build_type_alias_suffix_index() {
	t.type_alias_suffixes = map[string]u32{}
	mut by_short := map[string][]TypeAliasEntry{}
	for name, target_id in t.tc.type_aliases.ids {
		short := flat.source_type_name(flat.source_type_syntax(flat.source_type_id(name)).unqualified)
		if existing := t.type_alias_suffixes[short] {
			if existing != target_id {
				t.type_alias_suffixes[short] = 0
			}
		} else {
			t.type_alias_suffixes[short] = target_id
		}
		by_short[short] << TypeAliasEntry{
			name:   name
			target: target_id
		}
	}
	t.type_alias_short_index = &TypeAliasShortIndex{
		alias_count: t.tc.type_aliases.ids.len
		by_short:    by_short
	}
}

// TypeAliasEntry is one `name -> target` pair of the checker's type aliases.
struct TypeAliasEntry {
	name   string
	target u32
}

// TypeAliasShortIndex groups the checker's type aliases by short name (the text
// after the last `.`), keeping `tc.type_aliases` order within each group. The
// alias map is complete before transform starts, so the index is built once and
// shared read-only with worker forks.
struct TypeAliasShortIndex {
	alias_count int
	by_short    map[string][]TypeAliasEntry
}

// type_aliases_with_short_name returns the aliases whose short name is `short`,
// in `tc.type_aliases` order. It scans the alias map only when the index is
// missing or stale.
fn (t &Transformer) type_aliases_with_short_name(short string) []TypeAliasEntry {
	if !isnil(t.type_alias_short_index)
		&& t.type_alias_short_index.alias_count == t.tc.type_aliases.ids.len {
		return t.type_alias_short_index.by_short[short] or { []TypeAliasEntry{} }
	}
	mut entries := []TypeAliasEntry{}
	for name, target_id in t.tc.type_aliases.ids {
		if flat.source_type_syntax(flat.source_type_id(name)).unqualified == flat.source_type_id(short) {
			entries << TypeAliasEntry{
				name:   name
				target: target_id
			}
		}
	}
	return entries
}

fn (t &Transformer) expand_generic_type_alias(typ_id u32) ?u32 {
	if !flat.source_type_shape(typ_id).has(.bracketed) { return none }
	syntax := flat.source_type_syntax(typ_id)
	if !syntax.generic_app_valid || syntax.generic_app_args().len == 0 { return none }
	base := syntax.generic_app_base
	mut candidates := [base]
	if imported := t.resolve_imported_type_name(base) {
		if imported !in candidates { candidates << imported }
	}
	if !flat.source_type_shape(base).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
		'main',
		'builtin',
	] {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
		if qualified !in candidates { candidates << qualified }
	}
	for candidate in candidates {
		// Declaration tables retain their source name keys.
		name := flat.source_type_name(candidate)
		target := t.tc.type_aliases.type_id(name) or { continue }
		params := t.tc.type_alias_generic_params[name] or { continue }
		if params.len != syntax.generic_app_args().len { continue }
		expanded := substitute_generic_type_text_with_params(target, syntax.generic_app_args(), params)
		if flat.source_type_shape(candidate).has(.dotted) {
			// Alias targets are written in the alias declaration's module.
			// Qualify a bare target there before normalizing it at the use site.
			return t.normalize_type_in_module(expanded, flat.source_type_name(flat.source_type_syntax(candidate).qualifier))
		}
		return expanded
	}
	return none
}

fn (t &Transformer) is_known_type_name(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	if typ in t.structs || t.sum_types.contains(typ) || typ in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		if t.tc.type_aliases.contains(typ) || typ in t.tc.structs || t.tc.sum_types.contains(typ)
			|| typ in t.tc.enum_names || typ in t.tc.interface_names {
			return true
		}
	}
	if !flat.source_type_shape(typ_id).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := flat.source_type_name(flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), typ_id))
		if qtyp in t.structs || t.sum_types.contains(qtyp) || qtyp in t.enum_types {
			return true
		}
		if !isnil(t.tc) {
			return t.tc.type_aliases.contains(qtyp) || qtyp in t.tc.structs || t.tc.sum_types.contains(qtyp)
				|| qtyp in t.tc.enum_names || qtyp in t.tc.interface_names
		}
	}
	return false
}

// is_plain_builtin_alias_type reports whether is plain builtin alias type applies in transform.
@[inline]
fn is_plain_builtin_alias_type(typ_id u32) bool {
	return flat.source_type_shape(typ_id).has(.builtin_alias)
}

fn is_generic_placeholder_type_id(typ u32) bool {
	return flat.source_type_query(typ).has(.generic_name)
}

// normalize_type_in_module transforms normalize type in module data for transform.
fn (t &Transformer) normalize_type_in_module(type_id u32, mod string) u32 {
	if flat.source_type_shape(type_id).has(.builtin_alias) { return type_id }
	if isnil(t.module_type_cache) {
		return t.normalize_type_in_module_uncached(type_id, mod)
	}
	mut cache := t.module_type_cache
	if !same_transform_text(cache.file, t.cur_file)
		|| !same_transform_text(cache.source_module, t.cur_module) {
		cache.file = t.cur_file
		cache.source_module = t.cur_module
		cache.entries.clear()
		cache.generation++
	}
	recent_slot := (int(type_id & 1023) ^ (alias_cache_slot(mod) << 1)) & 1023
	if cache.recent_generations[recent_slot] == cache.generation
		&& same_transform_text(cache.recent_modules[recent_slot], mod)
		&& cache.recent_types[recent_slot] == type_id {
		return cache.recent_results[recent_slot]
	}
	if !same_transform_text(cache.module, mod) {
		cache.module = mod
		cache.entries.clear()
	}
	mut result := u32(0)
	if cached := cache.entries[type_id] {
		result = cached
	} else {
		result = t.normalize_type_in_module_uncached(type_id, mod)
		// Expanding an alias may recursively normalize in another owner module.
		if !same_transform_text(cache.module, mod) {
			cache.module = mod
			cache.entries.clear()
		}
		cache.entries[type_id] = result
	}
	cache.recent_types[recent_slot] = type_id
	cache.recent_modules[recent_slot] = mod
	cache.recent_results[recent_slot] = result
	cache.recent_generations[recent_slot] = cache.generation
	return result
}

fn (t &Transformer) normalize_type_in_module_uncached(type_id u32, mod string) u32 {
	clean_id := transform_trimmed_type_id(type_id)
	if clean_id == 0 {
		return clean_id
	}
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) {
		return flat.source_type_composition_one(.pointer, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.shared_type) {
		return flat.source_type_composition_one(.shared_type, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.atomic_type) {
		return flat.source_type_composition_one(.atomic_type, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.option_type) {
		return flat.source_type_composition_one(.option, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.result_type) {
		return flat.source_type_composition_one(.result, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.variadic) {
		return flat.source_type_composition_one(.variadic_parameter, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.channel_type) {
		return flat.source_type_composition_one(.channel, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.map_type) && syntax.bracket_closed {
		key_type := t.normalize_type_in_module(syntax.bracket_raw_inner, mod)
		value_type := t.normalize_type_in_module(syntax.raw_base, mod)
		return flat.source_type_composition_two(.map, key_type, value_type)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed && syntax.raw_base != 0 {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix, t.normalize_type_in_module(syntax.raw_base, mod))
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if !syntax.function_closed {
			return clean_id
		}
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param in syntax.function_params() {
			parts << t.normalize_type_in_module(flat.source_type_syntax(param).legacy_parameter_mut_type, mod)
		}
		normalized_ret := t.normalize_type_in_module(syntax.function_result, mod)
		if normalized_ret != 0 {
			parts << normalized_ret
			return flat.source_type_composition(.function_type_spaced, parts)
		}
		return flat.source_type_composition(.void_function_type_spaced, parts)
	}
	if syntax.generic_app_valid {
		// Expand the applied alias before its base: expanding only `Values` in
		// `Values[Item]` would leave the arguments attached to its `[]T` target.
		mod_id := flat.source_type_id(mod)
		qualified_base := flat.source_type_composition_two(.qualified, mod_id, syntax.generic_app_base)
		alias_type_id := if !flat.source_type_shape(syntax.generic_app_base).has(.dotted)
			&& mod.len > 0 && t.type_authority_has(flat.source_type_name(qualified_base)) {
			flat.source_type_composition_two(.qualified, mod_id, clean_id)
		} else {
			clean_id
		}
		if !isnil(t.tc) {
			if expanded_source_id := t.expand_generic_type_alias(alias_type_id) {
				return t.normalize_type_alias(expanded_source_id)
			}
		}
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		parts << t.normalize_type_in_module(syntax.generic_app_base, mod)
		for arg in syntax.generic_app_args() {
			parts << t.normalize_type_in_module(arg, mod)
		}
		return flat.source_type_composition(.generic, parts)
	}
	if shape.has(.dotted) || mod == '' || mod == 'main' || mod == 'builtin' {
		return t.normalize_type_alias(clean_id)
	}
	if !isnil(t.tc) && mod == t.cur_module && t.cur_file.len > 0 {
		clean := flat.source_type_name(clean_id)
		mut imported := ''
		for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, clean)] or {
			[]string{}
		} {
			if !t.type_authority_has(candidate) {
				continue
			}
			if imported != '' && imported != candidate {
				imported = ''
				break
			}
			imported = candidate
		}
		if imported.len > 0 {
			return t.normalize_type_alias(flat.source_type_id(imported))
		}
	}
	qualified_id := flat.source_type_composition_two(.qualified, flat.source_type_id(mod), clean_id)
	qtyp := flat.source_type_name(qualified_id)
	if t.type_authority_has(qtyp) {
		return t.normalize_type_alias(qualified_id)
	}
	if !isnil(t.tc) {
		if t.tc.type_aliases.contains(qtyp) {
			return t.normalize_type_alias(qualified_id)
		}
	}
	return t.normalize_type_alias(clean_id)
}

// resolve_index_elem_type determines the element type of an .index expression.
// For `[]T` arrays returns `T`, for `map[K]V` returns `V`, for `string` returns `u8`.
fn (t &Transformer) resolve_index_elem_type(node flat.Node) u32 {
	if node.kind != .index || node.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.resolve_expr_type(base_id)
	if base_type == 0 {
		base_node := t.a.nodes[int(base_id)]
		if base_node.kind == .selector {
			// Selector nodes can share their compact child slice with an earlier
			// speculative node. Re-resolve a cached miss from the actual slice
			// receiver before trusting the checker's index fallback.
			base_type = t.resolve_selector_type_uncached(base_node)
		}
	}
	if base_type == 0 {
		return u32(0)
	}
	base_type = t.normalize_type_alias(base_type)
	if flat.source_type_shape(base_type).has(.pointer) {
		ptr_elem_type_source_id := t.normalize_type_alias(flat.source_type_syntax(base_type).raw_base)
		ptr_elem_type_source_id_2 := ptr_elem_type_source_id
		ptr_elem_type_shape := flat.source_type_shape(ptr_elem_type_source_id_2)
		// A `mut map`/`mut []T` param is a pointer to the container; indexing it must
		// still yield the element/value type, so fall through to the container cases.
		if !ptr_elem_type_shape.has(.array_type) && !ptr_elem_type_shape.has(.map_type)
			&& !t.is_fixed_array_type(ptr_elem_type_source_id) {
			return ptr_elem_type_source_id
		}
		base_type = ptr_elem_type_source_id_2
	}
	is_slice := node.value == 'range'
		|| (node.children_count > 1 && t.a.child_node(&node, 1).kind == .range)
	if is_slice {
		if base_type == flat.source_type_string {
			return flat.source_type_string
		}
		if flat.source_type_shape(base_type).has(.array_type) {
			return base_type
		}
		// A range/slice of a fixed array (`arr[..]`, `arr[a..b]`) yields a dynamic
		// `[]T`, not the fixed array or a bogus `range` type.
		if t.is_fixed_array_type(base_type) {
			return flat.source_type_composition_one(.array, t.normalize_type_alias(t.for_in_fixed_array_elem_type(base_type)))
		}
	}
	if flat.source_type_shape(base_type).has(.array_type) {
		return t.normalize_type_alias(flat.source_type_syntax(base_type).raw_base)
	}
	if t.is_fixed_array_type(base_type) {
		return t.normalize_type_alias(t.for_in_fixed_array_elem_type(base_type))
	}
	if flat.source_type_shape(base_type).has(.map_type) {
		return t.normalize_type_alias(t.map_value_type(base_type))
	}
	if base_type == flat.source_type_string {
		return flat.source_type_u8
	}
	return u32(0)
}

fn (t &Transformer) index_expr_type(id flat.NodeId, node flat.Node) u32 {
	resolved_elem_type_source_id := t.resolve_index_elem_type(node)

	is_slice := node.value == 'range'
		|| (node.children_count > 1 && t.a.child_node(&node, 1).kind == .range)
	if resolved_elem_type_source_id == flat.source_type_u8 || (is_slice && resolved_elem_type_source_id != 0) {
		return resolved_elem_type_source_id
	}
	if t.is_fixed_array_type(resolved_elem_type_source_id)
		&& t.fixed_array_type_contains_map(resolved_elem_type_source_id) {
		return resolved_elem_type_source_id
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name_source_id := t.checker_recorded_type_name(id, typ)

			if name_source_id != 0 && name_source_id != flat.source_type_unknown {
				return t.normalize_type_alias(name_source_id)
			}
		}
	}
	return resolved_elem_type_source_id
}

@[inline]
fn (t &Transformer) normalized_node_type(id flat.NodeId) u32 {
	return t.normalize_type_alias(t.node_type(id))
}

// node_type returns the interned source type of an expression node, preferring the
// precise type recorded by the type checker (tc.expr_types, populated before
// transform) and falling back to the transformer's local heuristics. This is the
// bridge that makes the transformer type-aware, so type-dependent lowering lives
// here rather than in the backend.
@[direct_array_access; inline]
fn (t &Transformer) node_type(id flat.NodeId) u32 {
	idx := int(id)
	if idx < 0 {
		return u32(0)
	}
	// These expression types cannot depend on annotations, bindings or the
	// expected type. Resolve them before maintaining the contextual body memo.
	match t.a.nodes[idx].kind {
		.bool_literal { return flat.source_type_bool }
		.float_literal { return flat.source_type_f64 }
		.int_literal { return flat.source_type_int }
		.string_literal, .string_interp { return flat.source_type_string }
		.nil_literal { return flat.source_type_voidptr }
		.char_literal {
			return if t.a.nodes[idx].value.starts_with('c:') {
				flat.source_type_id('&u8')
			} else {
				flat.source_type_rune
			}
		}
		.sizeof_expr, .offsetof_expr { return flat.source_type_u32 }
		.prefix {
			if t.a.nodes[idx].children_count > 0 && t.a.nodes[idx].op == .not {
				return flat.source_type_bool
			}
		}
		.infix {
			if t.a.nodes[idx].children_count >= 2
				&& t.a.nodes[idx].op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return flat.source_type_bool
			}
		}
		else {}
	}
	if !isnil(t.node_type_memo) {
		mut memo := t.node_type_memo
		if memo.active && idx >= memo.lo && idx <= memo.hi {
			type_tables := if isnil(t.tc) {
				0
			} else {
				t.tc.structs.len + t.tc.type_aliases.ids.len + t.tc.sum_types.ids.len +
					t.tc.interface_names.len + t.tc.enum_names.len + t.tc.fn_ret_types.len
			}
			context_flags := u8(t.in_const_init) | (u8(t.validating_generic_spec) << 1) |
				(u8(t.fn_body_locals_in_scope) << 2) | (u8(t.in_monomorphize_scan) << 3)
			if memo.expected_type != t.expected_expr_type
				|| memo.context_flags != context_flags || memo.type_tables != type_tables
				|| !same_transform_text(memo.module, t.cur_module)
				|| !same_transform_text(memo.file, t.cur_file) {
				t.clear_node_type_memo()
				memo.expected_type = t.expected_expr_type
				memo.context_flags = context_flags
				memo.type_tables = type_tables
				memo.module = t.cur_module
				memo.file = t.cur_file
			}
			slot := idx - memo.lo
			if memo.filled[slot] == memo.generation {
				return memo.values[slot]
			}
			return t.node_type_memo_miss(id, slot)
		}
	}
	return t.node_type_uncached(id)
}

@[direct_array_access]
fn (t &Transformer) node_type_memo_miss(id flat.NodeId, slot int) u32 {
	mut memo := t.node_type_memo
	generation := memo.generation
	result_id := t.node_type_uncached(id)

	// Resolution can request a nested specialization and change the type
	// environment. Publish only results from an unchanged context, and let
	// provisional answers resolve again after declarations are registered.
	if memo.generation == generation && result_id != 0 && result_id != flat.source_type_unknown
		&& !t.generic_arg_is_unresolved(result_id) {
		memo.values[slot] = result_id
		memo.filled[slot] = generation
	}
	return result_id
}

@[direct_array_access]
fn (t &Transformer) node_type_uncached(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind == .fn_literal || node.kind == .lambda_expr {
		if fn_type_source_id := t.fn_value_type_name(id) {
			return fn_type_source_id
		}
	}
	if node.kind == .struct_init {
		// A specialized literal locks caller-owned types with `main.`. Its checker
		// semantic spelling drops that lock and can rebind an array element to a
		// homonym in the declaration module when parsed again during conversion.
		if t.validating_generic_spec && flat.source_type_syntax(node.typ).main_fragment
			&& !t.ident_is_import_alias('main') && decl_type_is_usable(node.typ) {
			return node.typ
		}
		if checker_type_source_id := t.checker_expr_type_name(id) {
			return checker_type_source_id
		}
	}
	// `offsetof` stores its member name in `node.typ_text()`; that payload is not the
	// expression type. Keep it from becoming a synthetic named type when dump
	// lowering declares a temporary for the expression.
	if node.kind == .offsetof_expr || node.kind == .sizeof_expr {
		return flat.source_type_u32
	}
	resolved_source_id := t.resolve_expr_type(id)
	if resolved_source_id != 0 {
		if node.kind == .array_literal && t.generic_arg_is_unresolved(resolved_source_id) {
			elem_type_source_id := t.array_literal_elem_type(node)
			elem_type_source_id_3 := elem_type_source_id
			if decl_type_is_usable(elem_type_source_id_3) && !t.generic_arg_is_unresolved(elem_type_source_id) {
				if t.is_fixed_array_type(resolved_source_id) {
					return flat.source_type_composition_two(.fixed_array, fixed_array_length_id(resolved_source_id), elem_type_source_id_3)
				}
				return flat.source_type_composition_one(.array, elem_type_source_id_3)
			}
		}
		// The checker override only applies to named struct types. Most expressions
		// are builtin scalars or containers; avoid parsing both types and deriving
		// their C names for those overwhelmingly common cases.
		resolved_shape := flat.source_type_shape(resolved_source_id)
		if !t.building_v && flat.source_type_query(resolved_source_id).has(.struct_candidate) {
			if checked_source_id := t.checker_type_over_struct_guess(id, resolved_source_id) {
				return checked_source_id
			}
		}
		if resolved_shape.has(.typeof_expr) && !isnil(t.tc) {
			if typ := t.tc.expr_type(id) {
				name_source_id := t.normalize_type_alias(t.tc.type_name(typ))
				name_source_id_2 := name_source_id

				if decl_type_is_usable(name_source_id_2) {
					return name_source_id
				}
			}
		}
		if t.generic_arg_is_unresolved(resolved_source_id) && node.typ != 0 {
			node_typ_source_id := t.normalize_type_alias(node.typ)

			if node_typ_source_id != 0 && !t.generic_arg_is_unresolved(node_typ_source_id) {
				return node_typ_source_id
			}
		}
		return resolved_source_id
	}
	if node.kind == .dump_expr && node.children_count > 0 {
		return t.node_type(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		if checker_type_source_id := t.checker_expr_type_name(id) {
			return checker_type_source_id
		}
		if !isnil(t.tc) {
			checker_resolved_source_id := t.tc.type_name(t.tc.resolve_type(id))
			checker_resolved_source_id_2 := checker_resolved_source_id

			if decl_type_is_usable(checker_resolved_source_id_2) && checker_resolved_source_id != flat.source_type_void {
				return t.normalize_type_alias(checker_resolved_source_id)
			}
		}
		child_type_source_id_2 := t.node_type(t.a.child(&node, 0))
		if child_type_source_id_2 != 0 {
			if t.is_optional_type_name(child_type_source_id_2) {
				wrapper := if flat.source_type_shape(child_type_source_id_2).has(.option_type) {
					flat.SourceTypeComposition.option
				} else {
					flat.SourceTypeComposition.result
				}
				return flat.source_type_composition_one(wrapper, flat.source_type_composition_one(.pointer, t.optional_base_type(child_type_source_id_2)))
			}
			return flat.source_type_composition_one(.pointer, child_type_source_id_2)
		}
	}
	mut deferred_call_typ := u32(0)
	if node.typ != 0 {
		node_typ_source_id_2 := t.normalize_type_alias(node.typ)
		if node.kind == .call && node_typ_source_id_2 in [flat.source_type_int,
			transform_source_type_array, transform_source_type_map, flat.source_type_unknown] {
			deferred_call_typ = node_typ_source_id_2
		} else {
			return node_typ_source_id_2
		}
	}
	if node.kind == .selector {
		sel_type_source_id := t.resolve_selector_type(node)

		if sel_type_source_id != 0 {
			return sel_type_source_id
		}
	}
	if node.kind == .index {
		elem_type_source_id_2 := t.index_expr_type(id, node)

		if elem_type_source_id_2 != 0 {
			return elem_type_source_id_2
		}
	}
	if node.kind == .call {
		if array_type_source_id := t.array_call_type_name(id, node) {
			return array_type_source_id
		}
	}
	// NOTE: infix is intentionally not handled here — resolve_expr_type() (called at the top
	// of node_type) already resolves infix types, including struct operator overloads.
	if node.kind == .struct_init && node.source_value_id() != 0 {
		for candidate in [node.typ, node.source_value_id()] {
			if candidate == 0 {
				continue
			}
			normalized_source_id := t.normalize_type_alias(candidate)
			if normalized_source_id != candidate || flat.source_type_shape(normalized_source_id).has(.bracketed) {
				return normalized_source_id
			}
		}
		if node.value_text() in t.structs {
			return node.source_value_id()
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value_text()}'
			if qname in t.structs {
				return flat.source_type_id(qname)
			}
		}
		return node.source_value_id()
	}
	if !isnil(t.tc) {
		mut name_id := u32(0)
		if typ := t.tc.expr_type(id) {
			name_id = t.tc.type_name(typ)
		}
		if flat.source_type_shape(name_id).has(.typeof_expr) {
			name_id = t.tc.type_name(t.tc.resolve_type(id))
		}
		if name_id == 0 || name_id == flat.source_type_unknown {
			name_id = t.tc.type_name(t.tc.resolve_type(id))
		}
		if name_id != 0 && name_id != flat.source_type_void && (name_id != flat.source_type_int
			|| node.kind in [.ident, .int_literal, .infix, .prefix, .postfix, .paren, .selector,
				.index, .call]) {
			return t.normalize_type_alias(name_id)
		}
	}
	if deferred_call_typ != 0 {
		return deferred_call_typ
	}
	return u32(0)
}

fn (t &Transformer) checker_type_over_struct_guess(id flat.NodeId, guessed_id u32) ?u32 {
	if isnil(t.tc) || int(id) < 0 || guessed_id == u32(0) {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		local_type_source_id_2 := t.normalized_var_type(node.value)
		if local_type_source_id_2 != 0 && local_type_source_id_2 == t.normalize_type_alias(guessed_id) {
			// Function-local bindings are authoritative after lowering. The checker
			// cache still describes the source node and can retain a mutable
			// parameter's pointer storage type after that node is reused for a value
			// local with the same expression shape.
			return none
		}
		if t.current_module_declares_const(node.value) {
			// The checker still binds this name to the foreign `__global`; its
			// cached type must not override the current module's const.
			return none
		}
	}
	mut guessed_c_type := ''
	if isnil(t.struct_guess_cache) {
		guessed_type := t.tc.parse_type(guessed_id)
		guessed_base := types.unwrap_all_pointers(guessed_type)
		if guessed_base !is types.Struct && !(guessed_base is types.Alias
			&& guessed_base.base_type is types.Struct) {
			return none
		}
		guessed_c_type = t.tc.c_type(guessed_type)
	} else {
		mut cache := t.struct_guess_cache
		if !same_transform_text(cache.module, t.cur_module)
			|| !same_transform_text(cache.file, t.cur_file) {
			cache.module = t.cur_module
			cache.file = t.cur_file
			cache.entries.clear()
		}
		if cached_id := cache.entries[guessed_id] {
			cached := flat.source_type_name(cached_id)
			if cached_id == 0 {
				return none
			}
			guessed_c_type = cached
		} else {
			guessed_type := t.tc.parse_type(guessed_id)
			guessed_base := types.unwrap_all_pointers(guessed_type)
			if guessed_base !is types.Struct && !(guessed_base is types.Alias
				&& guessed_base.base_type is types.Struct) {
				cache.entries[guessed_id] = u32(0)
				return none
			}
			guessed_c_type = t.tc.c_type(guessed_type)
			cache.entries[guessed_id] = flat.source_type_id(guessed_c_type)
		}
	}
	checked_type := t.tc.expr_type(id) or { return none }
	if checked_type is types.Unknown || checked_type is types.Void {
		return none
	}
	// Calls may obtain their type from a newly resolved signature instead of
	// checked metadata. Other recorded expressions can carry their identity
	// straight into the C layout cache, avoiding another semantic hash walk.
	mut checked_c_type := ''
	if node.kind != .call {
		if type_id := t.tc.checked_expr_type_id(id) {
			checked_c_type = t.tc.c_type_by_id(type_id) or { '' }
		}
	}
	if checked_c_type.len == 0 {
		checked_c_type = t.tc.c_type(checked_type)
	}
	if guessed_c_type == checked_c_type {
		return none
	}
	checked_source_id := t.normalize_type_alias(checked_type.source_type_id())
	checked_source_id_2 := checked_source_id

	if !decl_type_is_usable(checked_source_id_2) {
		return none
	}
	return checked_source_id
}

fn (t &Transformer) array_call_type_name(id flat.NodeId, node flat.Node) ?u32 {
	if map_type_source_id := t.array_map_call_type_name(id, node) {
		return map_type_source_id
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_type0_source_id := t.node_type(t.a.child(fn_node, 0))
	base_type := (if flat.source_type_shape(base_type0_source_id).has(.pointer) {
		flat.source_type_syntax(base_type0_source_id).raw_base
	} else {
		base_type0_source_id
	})
	if !flat.source_type_shape(base_type).has(.array_type) {
		return none
	}
	elem_type := flat.source_type_syntax(base_type).raw_base
	if elem_type == 0 {
		return none
	}
	match fn_node.value {
		'filter', 'clone', 'reverse', 'sorted', 'repeat', 'repeat_to_depth' {
			return base_type
		}
		'first', 'last', 'pop', 'pop_left' {
			return elem_type
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) array_map_call_type_name(id flat.NodeId, node flat.Node) ?u32 {
	if node.children_count < 2 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'map' || fn_node.children_count == 0 {
		return none
	}
	map_expr_id := t.a.child(&node, 1)
	if callback_ret_source_id := t.array_map_callback_return_type_name(map_expr_id) {
		return flat.source_type_composition_one(.array, callback_ret_source_id)
	}
	if checker_type_source_id := t.checker_expr_type_name(id) {
		checker_type_shape := flat.source_type_shape(checker_type_source_id)

		if checker_type_shape.has(.array_type) {
			return checker_type_source_id
		}
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type0 := t.node_type(base_id)
	if !flat.source_type_shape(t.trim_pointer_type(base_type0)).has(.array_type) {
		base_type0 = t.original_expr_type(base_id)
	}
	base_type := t.trim_pointer_type(base_type0)
	if !flat.source_type_shape(base_type).has(.array_type) {
		return none
	}
	mut elem_type_source_id := if callback_ret_source_id := t.array_map_callback_return_type_name(map_expr_id) {
		callback_ret_source_id
	} else if checker_type_source_id := t.checker_expr_type_name(map_expr_id) {
		checker_type_source_id
	} else {
		t.node_type(map_expr_id)
	}
	if elem_type_source_id == 0 || elem_type_source_id in [
		transform_source_type_array,
		transform_source_type_map,
		flat.source_type_unknown,
	] {
		elem_type_source_id = t.reliable_stringify_type(map_expr_id)
	}
	if elem_type_source_id == 0 || elem_type_source_id in [
		transform_source_type_array,
		transform_source_type_map,
		flat.source_type_unknown,
		flat.source_type_void,
	] {
		return none
	}
	return flat.source_type_composition_one(.array, elem_type_source_id)
}

fn (t &Transformer) array_map_callback_return_type_name(map_expr_id flat.NodeId) ?u32 {
	if int(map_expr_id) < 0 {
		return none
	}
	map_expr := t.a.nodes[int(map_expr_id)]
	if map_expr.kind == .ident {
		if fn_name := t.resolve_fn_value_ident(map_expr.value) {
			if ret_source_id := t.fn_ret_types.type_id(fn_name) {
				return t.normalize_type_alias(ret_source_id)
			}
			if !isnil(t.tc) {
				if ret_type := t.tc.fn_ret_types[fn_name] {
					return t.normalize_type_alias(ret_type.source_type_id())
				}
			}
		} else if ret_type_source_id := t.fn_value_return_type_name(map_expr_id) {
			return ret_type_source_id
		}
	} else if ret_type_source_id := t.fn_value_return_type_name(map_expr_id) {
		return ret_type_source_id
	}
	return none
}

// lvalue_type returns the source type ID for an assignable expression, handling
// idents/calls (via node_type), selectors (struct field types) and index
// expressions (array/map element types). Used by array-append lowering so that
// `obj.field << x` and `m[k] << x` are typed, not just plain `arr << x`.
fn (t &Transformer) lvalue_type(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector {
		sel_type_source_id := t.resolve_selector_type(node)
		sel_type_shape := flat.source_type_shape(sel_type_source_id)
		if !isnil(t.tc) && !sel_type_shape.has(.pointer) {
			if checker_type := t.tc.expr_type(id) {
				checker_name := t.tc.type_name(checker_type)
				if flat.source_type_shape(checker_name).has(.pointer) {
					return checker_name
				}
			}
		}
		if sel_type_source_id != 0 {
			return sel_type_source_id
		}
	}
	if node.kind == .index {
		if node.value == 'range'
			|| (node.children_count > 1 && t.a.child_node(&node, 1).kind == .range) {
			return t.index_expr_type(id, node)
		}
		elem_type_source_id := t.resolve_index_elem_type(node)

		if elem_type_source_id != 0 {
			return elem_type_source_id
		}
	}
	nt_source_id := t.node_type(id)

	if nt_source_id != 0 {
		return nt_source_id
	}
	return u32(0)
}

// is_string_type checks if an expression resolves to string type.
// Handles string literals, interpolations, and ident/call expressions typed as string.
fn (t &Transformer) is_string_type(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal || node.kind == .string_interp {
		return true
	}
	if node.kind == .ident {
		raw_var_type_source_id := t.raw_var_type(node.value)

		if raw_var_type_source_id != 0 {
			return t.normalize_type_alias(raw_var_type_source_id) == flat.source_type_string
		}
	}
	if node.kind == .ident && !isnil(t.tc) {
		if key := t.const_type_key_in_context(node.value, t.cur_module, t.cur_file) {
			if expr_id := t.tc.const_exprs[key] {
				const_expr := t.a.nodes[int(expr_id)]
				if const_expr.kind == .char_literal {
					return false
				}
				if const_expr.kind == .string_literal || const_expr.kind == .string_interp {
					return true
				}
			}
		}
	}
	if raw_const_type_source_id := t.raw_const_type_name_for_expr(id) {
		return t.normalize_type_alias(raw_const_type_source_id) == flat.source_type_string
	}
	mut typ := u32(0)
	if sc := t.find_smartcast_for_expr(id) {
		typ = t.smartcast_target_type(sc)
	}
	if typ == 0 {
		typ = t.node_type(id)
	}
	if typ == 0 {
		typ = t.raw_checker_node_type(id)
	}
	return t.normalize_type_alias(typ) == flat.source_type_string
}

// is_array_type checks if a type string represents an array type (starts with `[]`).
fn (t &Transformer) is_array_type(type_str_id u32) bool {
	type_str_shape := flat.source_type_shape(type_str_id)
	return type_str_shape.has(.array_type)
}

// is_map_type checks if a type string represents a map type (starts with `map[`).
fn (t &Transformer) is_map_type(type_str_id u32) bool {
	type_str_shape_2 := flat.source_type_shape(type_str_id)
	return type_str_shape_2.has(.map_type)
}

// array_elem_type extracts the element type from an array type string.
// `[]int` -> `int`, `[][]string` -> `[]string`.
fn (t &Transformer) array_elem_type(type_str_id u32) u32 {
	return if flat.source_type_shape(type_str_id).has(.array_type) {
		flat.source_type_syntax(type_str_id).raw_base
	} else {
		u32(0)
	}
}

// map_value_type extracts the value type from a map type string.
// `map[string]int` -> `int`, `map[string][]bool` -> `[]bool`.
fn (t &Transformer) map_value_type(type_str_id u32) u32 {
	if !flat.source_type_shape(type_str_id).has(.map_type) { return u32(0) }
	return flat.source_type_syntax(type_str_id).raw_base
}

// map_key_type extracts the key type from a map type string.
// `map[string]int` -> `string`, `map[int]bool` -> `int`.
fn (t &Transformer) map_key_type(type_str_id u32) u32 {
	if !flat.source_type_shape(type_str_id).has(.map_type) { return u32(0) }
	return flat.source_type_syntax(type_str_id).bracket_source_inner
}

// target_text resolves the stored source spelling for a text consumer.
fn (value &TypeAliasEntry) target_text() string {
	return flat.source_type_name(value.target)
}
