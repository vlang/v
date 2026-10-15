module transform

import v.flat
import v.types

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
			t.propagate_decl_pair_type(node, i, i + 1, '')
		}
		return
	}
	t.propagate_decl_pair_type(node, 0, 1, node.typ_text())
}

fn (mut t Transformer) propagate_decl_pair_type(node flat.Node, lhs_idx int, rhs_idx int, fallback_type string) {
	if lhs_idx >= node.children_count || rhs_idx >= node.children_count {
		return
	}
	lhs_id := t.a.child(&node, lhs_idx)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	mut typ := ''
	rhs_id := t.a.child(&node, rhs_idx)
	rhs := t.a.nodes[int(rhs_id)]
	rhs_authority_source_id := t.decl_rhs_type(rhs_id)
	rhs_authority := flat.source_type_name(rhs_authority_source_id)
	if t.is_fn_pointer_type_name(rhs_authority_source_id) {
		typ = rhs_authority
	} else if t.decl_type_should_override_fallback(rhs_authority, fallback_type, rhs) {
		typ = rhs_authority
	} else if decl_type_is_usable(fallback_type) {
		typ = fallback_type
	} else {
		if decl_type_is_usable(rhs_authority) {
			typ = rhs_authority
		} else if decl_type_is_usable(lhs.typ_text()) {
			typ = lhs.typ_text()
		}
	}
	if typ.len > 0 {
		t.set_var_type_with_raw(lhs.value, t.normalize_type_alias(flat.source_type_id(typ)), flat.source_type_id(typ))
	}
}

fn (t &Transformer) decl_type_should_override_fallback(authority string, fallback string, rhs flat.Node) bool {
	if !decl_type_is_usable(authority) {
		return false
	}
	if !decl_type_is_usable(fallback) {
		return true
	}
	if authority != fallback
		&& rhs.kind in [.bool_literal, .char_literal, .float_literal, .int_literal, .string_literal,
			.string_interp] {
		return true
	}
	if t.local_struct_type_overrides_imported_alias(authority, fallback) {
		return true
	}
	if rhs.kind == .as_expr {
		return true
	}
	if rhs.kind == .ident && authority != fallback {
		if t.has_smartcast(rhs.value) {
			return true
		}
		if flat.source_type_name(t.var_type(rhs.value)) == authority {
			return true
		}
	}
	if rhs.kind == .call && authority != fallback && authority.contains('.')
		&& authority.all_after_last('.') == fallback.all_after_last('.') {
		return true
	}
	if rhs.kind == .spawn_expr && fallback == 'thread' && authority.starts_with('thread ') {
		return true
	}
	if rhs.kind == .infix && rhs.op == .right_shift_unsigned {
		return true
	}
	// A mutable pointer parameter is represented by one extra C indirection. An
	// explicit source dereference is lowered through that slot, and its transformed
	// prefix is the reliable value type (`u64`), even if the original declaration
	// annotation still carries the parameter's pointer type (`&u64`).
	if t.decl_rhs_has_deref_tail(rhs) && authority != fallback
		&& fallback.starts_with('&') && !authority.starts_with('&') {
		return true
	}
	if map_value_starts_with_fixed_array(authority) && fallback.starts_with('map[') {
		return true
	}
	return authority.starts_with('&') && !fallback.starts_with('&')
}

fn (t &Transformer) local_struct_type_overrides_imported_alias(authority string, fallback string) bool {
	mut local := trimmed_transform_text(authority)
	mut imported := trimmed_transform_text(fallback)
	for prefix in ['[]', '&', '?', '!'] {
		for local.starts_with(prefix) && imported.starts_with(prefix) {
			local = local[prefix.len..]
			imported = imported[prefix.len..]
		}
	}
	if !t.bare_struct_name_is_local_to_current_module(local) || !imported.contains('.')
		|| imported.all_after_last('.') != local {
		return false
	}
	return true
}

fn map_value_starts_with_fixed_array(typ string) bool {
	clean := trimmed_transform_text(typ)
	if !clean.starts_with('map[') {
		return false
	}
	bracket_end := generic_matching_bracket(clean, 3)
	return bracket_end + 1 < clean.len && clean[bracket_end + 1] == `[`
}

fn decl_type_is_usable(typ string) bool {
	if typ == '' || typ in ['unknown', 'array', 'map', 'struct'] || typ.contains('unknown') {
		return false
	}
	if types.type_text_contains_typeof(typ) {
		return false
	}
	// `replace` copies the text even when it holds no space.
	clean := if typ.contains_u8(` `) { typ.replace(' ', '') } else { typ }
	return clean !in ['Option', '__v_option', 'Result'] && !clean.starts_with('Option_')
		&& !clean.starts_with('__v_option_') && !clean.starts_with('Result_')
}

fn (t &Transformer) checker_expr_type_name(id flat.NodeId) ?u32 {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	typ := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	mut name := flat.source_type_name(t.checker_recorded_type_name(id, typ))
	if t.tc.type_aliases.contains(name) {
		name = flat.source_type_name(t.normalize_type_alias(flat.source_type_id(name)))
	}
	if types.type_text_contains_typeof(name) {
		name = flat.source_type_name(t.tc.type_name(t.tc.resolve_type(id)))
		if t.tc.type_aliases.contains(name) {
			name = flat.source_type_name(t.normalize_type_alias(flat.source_type_id(name)))
		}
	}
	if decl_type_is_usable(name) && name != 'void' {
		return flat.source_type_id(name)
	}
	return none
}

// Keep the indexed semantic identity through repeated reads of a checked
// composite expression. Calls retain their resolution/annotation precedence;
// literal and stored-name types already have a constant-time spelling.
fn (t &Transformer) checker_recorded_type_name(id flat.NodeId, typ types.Type) u32 {
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
		if node.kind == .cast_expr && node.value.len > 0 {
			target_source_id := t.normalize_type_alias(flat.source_type_id(node.value))
			target := flat.source_type_name(target_source_id)
			if decl_type_is_usable(target) {
				return target_source_id
			}
		}
		// Primitive casts enter the first propagation pass as ordinary calls and
		// are lowered to `cast_expr` later. Preserve the callee's explicit type
		// before the checker's function-valued operand annotation can win.
		if node.kind == .call && node.children_count == 2 {
			callee := t.a.child_node(&node, 0)
			if callee.kind == .ident && callee.value in primitive_cast_type_names {
				return flat.source_type_id(callee.value)
			}
		}
		// Infix expressions cannot be function values in the language. Prefer their
		// operand-derived result type over a stale checker annotation inherited from
		// a linker-symbol cast in an earlier declaration.
		if node.kind == .infix {
			infix_type_source_id := t.resolve_expr_type(id)
			infix_type := flat.source_type_name(infix_type_source_id)
			if decl_type_is_usable(infix_type) {
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
				return flat.source_type_id(if node.value.starts_with('c:') { '&u8' } else { 'rune' })
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
		if node.kind == .as_expr && node.value.len > 0 {
			return t.normalize_type_alias(flat.source_type_id(node.value))
		}
		if node.kind == .selector {
			selector_type_source_id := t.resolve_selector_type(node)
			selector_type := flat.source_type_name(selector_type_source_id)
			if decl_type_is_usable(selector_type) {
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
			local_type := flat.source_type_name(local_type_source_id)
			if decl_type_is_usable(local_type) {
				return local_type_source_id
			}
		}
		if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			child_type_source_id := t.lvalue_type(child_id)
			child_type := flat.source_type_name(child_type_source_id)
			if child.kind == .struct_init && t.is_optional_type_name(child_type_source_id) {
				return flat.source_type_id('${child_type[..1]}&${flat.source_type_name(t.optional_base_type(child_type_source_id))}')
			}
			prefix_type_source_id := t.node_type(id)
			prefix_type := flat.source_type_name(prefix_type_source_id)
			if decl_type_is_usable(prefix_type) {
				return prefix_type_source_id
			}
			if child_type_source_id != 0 {
				return flat.source_type_id('&${child_type}')
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
	mut typ := if node.kind == .ident {
		flat.source_type_name(t.var_type(node.value))
	} else {
		flat.source_type_name(t.node_type(current))
	}
	if node.kind == .ident && t.pointer_value_rvalues[node.value] && typ.starts_with('&') {
		typ = typ[1..]
	}
	for _ in 0 .. derefs {
		if !typ.starts_with('&') {
			return none
		}
		typ = typ[1..]
	}
	if decl_type_is_usable(typ) {
		return flat.source_type_id(typ)
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
	mut ret_type := flat.source_type_name(t.node_type(call_id))
	if (ret_type.len == 0 || ret_type == 'unknown') && !isnil(t.tc) {
		callee := t.a.child_node(call, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base := t.a.child_node(callee, 0)
			base_type := if base.kind == .ident {
				flat.source_type_name(t.var_type(base.value))
			} else {
				flat.source_type_name(t.node_type(t.a.child(callee, 0)))
			}
			if info := t.tc.resolve_generic_struct_method(base_type, callee.value) {
				ret_type = info.return_type.name()
			}
		}
	}
	if ret_type.len == 0 || ret_type == 'unknown' {
		return none
	}
	if ret_type == 'void' {
		return flat.source_type_id('thread')
	}
	return flat.source_type_id('thread ${ret_type}')
}

fn (t &Transformer) map_expr_decl_type(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .map_init {
		for candidate in [node.value, node.typ_text(), flat.source_type_name(t.node_type(id))] {
			if candidate.starts_with('map[') {
				return t.refine_map_init_fixed_array_value_type(node, flat.source_type_id(candidate))
			}
		}
	}
	if node.kind == .call {
		map_type_source_id := t.new_map_call_type(node)

		if map_type_source_id != 0 {
			return map_type_source_id
		}
		if node.typ_text().starts_with('map[') {
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
		if name := fn_value_type_name_from_type(typ) {
			return t.normalize_type_alias(flat.source_type_id(name))
		}
	}
	if node.kind == .ident {
		if name := fn_value_type_name_from_type(t.tc.resolve_type(id)) {
			return t.normalize_type_alias(flat.source_type_id(name))
		}
		fn_name := t.tc.resolved_fn_value_name(id) or { node.value }
		params := t.tc.fn_param_types[fn_name] or { return none }
		ret := t.tc.fn_ret_types[fn_name] or { return none }
		return flat.source_type_id(fn_literal_value_type_text(params, ret.name()))
	}
	if node.kind == .lambda_expr {
		typ := t.tc.resolve_type(id)
		if name := fn_value_type_name_from_type(typ) {
			return t.normalize_type_alias(flat.source_type_id(name))
		}
	}
	return none
}

fn (t &Transformer) fn_literal_type_text(node flat.Node) u32 {
	mut params := []string{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		raw := if child.typ != 0 { child.typ_text() } else { child.value }
		slot_type := explicit_mut_pointer_param_type_text(child, fn_literal_param_type_text(raw))
		params << mutable_fn_value_param_type_text(child, slot_type)
	}
	ret := trimmed_transform_text(node.typ_text())
	if ret.len == 0 || ret == 'void' {
		return flat.source_type_id('fn (${params.join(', ')})')
	}
	return flat.source_type_id('fn (${params.join(', ')}) ${ret}')
}

fn fn_literal_param_type_text(raw string) string {
	mut text := trimmed_transform_text(raw)
	if text.starts_with('mut ') {
		text = trimmed_transform_text(text[4..])
	}
	space := generic_top_level_space_index(text)
	if space <= 0 {
		return text
	}
	head := trimmed_transform_text(text[..space])
	tail := trimmed_transform_text(text[space + 1..])
	if generic_fn_type_param_head_is_name(head, tail) {
		return tail
	}
	return text
}

fn fn_value_type_name_from_type(typ types.Type) ?string {
	name := typ.name()
	if name.len == 0 {
		return none
	}
	if typ is types.FnType {
		return name
	}
	if typ is types.Alias {
		if typ.base_type is types.FnType {
			return name
		}
	}
	return none
}

fn fn_call_value_type_name_from_type(typ types.Type) ?string {
	if typ is types.FnType {
		for i, param in typ.params {
			if i < typ.params_mut.len && typ.params_mut[i] && param is types.Pointer {
				return fn_literal_value_type_text(typ.params, typ.return_type.name())
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
	fn_value_type := fn_call_value_type_name_from_type(ret_type) or { return none }
	if t.generic_arg_is_unresolved(flat.source_type_id(fn_value_type)) {
		return none
	}
	return t.normalize_type_alias(flat.source_type_id(fn_value_type))
}

fn (t &Transformer) concrete_node_type_name(node flat.Node) u32 {
	if node.typ == 0 {
		return u32(0)
	}
	typ_source_id := t.normalize_type_alias(node.typ)
	typ := flat.source_type_name(typ_source_id)
	if !decl_type_is_usable(typ) {
		return u32(0)
	}
	if type_text_has_unresolved_generic_placeholder(typ) {
		return u32(0)
	}
	return typ_source_id
}

fn type_text_has_unresolved_generic_placeholder(typ string) bool {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return false
	}
	if is_generic_fn_placeholder_name(clean) {
		return true
	}
	if clean.starts_with('&') {
		return type_text_has_unresolved_generic_placeholder(clean[1..])
	}
	if clean.starts_with('mut ') {
		return type_text_has_unresolved_generic_placeholder(clean[4..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return type_text_has_unresolved_generic_placeholder(clean[1..])
	}
	if clean.starts_with('...') {
		return type_text_has_unresolved_generic_placeholder(clean[3..])
	}
	if clean.starts_with('[]') {
		return type_text_has_unresolved_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return type_text_has_unresolved_generic_placeholder(clean[4..bracket_end])
				|| type_text_has_unresolved_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return type_text_has_unresolved_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	if !type_text_has_generic_placeholder_token(clean) {
		return false
	}
	if !clean.contains('[') {
		return true
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if type_text_has_unresolved_generic_placeholder(arg) {
				return true
			}
		}
	}
	return false
}

fn type_text_has_generic_placeholder_token(text string) bool {
	for i := 0; i < text.len; i++ {
		ch := text[i]
		if ch < `A` || ch > `Z` {
			continue
		}
		prev_is_ident := i > 0 && type_text_ident_char(text[i - 1])
		next_is_ident := i + 1 < text.len && type_text_ident_char(text[i + 1])
		if !prev_is_ident && !next_is_ident {
			return true
		}
	}
	return false
}

fn type_text_ident_char(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch == `_`
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
	base_selector_name := t.selector_expr_name(base_id)
	if base_selector_name.len > 0 {
		if enum_type := t.enum_type_name_from_selector_name(base_selector_name) {
			if fields := t.enum_types[enum_type] {
				if field_name in fields {
					return flat.source_type_id(enum_type)
				}
			}
		}
		if type_name := t.qualified_enum_type_selector_name_from_selector_name(base_selector_name, field_name) {
			return flat.source_type_id(type_name)
		}
	}
	if base_node.kind == .ident {
		if typ_source_id := t.const_type_name('${base_node.value}.${field_name}') {
			return typ_source_id
		}
		if fields := t.enum_types[base_node.value] {
			if field_name in fields {
				return flat.source_type_id(base_node.value)
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
	base_type_source_id := t.resolve_expr_type(base_id)
	base_type := flat.source_type_name(base_type_source_id)
	if base_type_source_id == 0 {
		if ftyp_source_id := t.lookup_unique_field_type(field_name) {
			return ftyp_source_id
		}
		return u32(0)
	}
	if builtin_type_source_id := t.builtin_selector_type(base_type_source_id, field_name) {
		return builtin_type_source_id
	}
	base_key := t.expr_key(base_id)
	if base_key.len > 0 {
		full_key := '${base_key}.${field_name}'
		if sc := t.find_smartcast(full_key) {
			return t.smartcast_target_type(sc)
		}
	}
	if base_key.len > 0 {
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
	// Strip pointer prefix if present (e.g. "&MyStruct" -> "MyStruct")
	lookup_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if ftyp_source_id := t.lookup_sum_variant_field_type(flat.source_type_id(lookup_type), field_name) {
		return ftyp_source_id
	}
	if ftyp_source_id := t.lookup_struct_field_type(flat.source_type_id(lookup_type), field_name) {
		return ftyp_source_id
	}
	if _ := t.lookup_struct_info(lookup_type) {
		// Follow the complete embedding path, including pointer embeddings.
		if path := t.struct_field_path_for_field(lookup_type, field_name) {
			owner_type := if path.len > 0 {
				flat.source_type_name(t.trim_pointer_type(path.last().typ))
			} else {
				lookup_type
			}
			if owner_info := t.lookup_struct_info(owner_type) {
				if ftyp := t.struct_field_type(owner_info, field_name) {
					return flat.source_type_id(ftyp)
				}
			}
		}
		// A selector on a known struct may be a bound method. Do not infer its
		// type from a same-named field on an unrelated struct; let the checker
		// supply the method's function type instead.
		return u32(0)
	}
	// An interface declares the types of its fields: do not guess them from the name.
	iface_name_source_id := t.resolve_interface_type_name(flat.source_type_id(lookup_type))
	iface_name := flat.source_type_name(iface_name_source_id)
	if iface_name_source_id != 0 {
		if ftyp := t.interface_field_type_name(iface_name, field_name) {
			return flat.source_type_id(ftyp)
		}
	}
	if ftyp_source_id := t.lookup_unique_field_type(field_name) {
		return ftyp_source_id
	}
	return u32(0)
}

fn (t &Transformer) builtin_selector_type(base_type_id u32, field_name string) ?u32 {
	clean_source_id := t.trim_pointer_type(t.normalize_type_alias(base_type_id))
	clean := flat.source_type_name(clean_source_id)
	if clean == 'array' || clean.starts_with('[]') {
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
	if field_name == 'len' && (clean_source_id == flat.source_type_string || clean.starts_with('map[') || t.is_fixed_array_type(clean_source_id)
		|| clean == 'chan' || clean.starts_with('chan ')) {
		return flat.source_type_int
	}
	if clean == 'chan' || clean.starts_with('chan ') {
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
	return t.lookup_sum_variant_field_type_seen(sum_type_id, field_name, []string{})
}

fn (t &Transformer) lookup_sum_variant_field_type_seen(sum_type_id u32, field_name string, seen []string) ?u32 {
	resolved_source_id_2 := t.resolve_sum_name(t.trim_pointer_type(t.normalize_type_alias(sum_type_id)))
	resolved := flat.source_type_name(resolved_source_id_2)
	if resolved_source_id_2 == 0 || resolved in seen {
		return none
	}
	variants := t.sum_types.get(resolved) or { return none }
	mut next_seen := seen.clone()
	next_seen << resolved
	mut found := ''
	for variant in variants {
		mut ftyp := ''
		if vt_source_id := t.lookup_struct_field_type(flat.source_type_id(variant), field_name) {
			ftyp = flat.source_type_name(t.normalize_type_alias(vt_source_id))
		} else if vt_source_id := t.checker_struct_field_type_name(flat.source_type_id(variant), field_name) {
			ftyp = flat.source_type_name(t.normalize_type_alias(vt_source_id))
		} else if nested_source_id := t.lookup_sum_variant_field_type_seen(flat.source_type_id(variant), field_name, next_seen) {
			ftyp = flat.source_type_name(t.normalize_type_alias(nested_source_id))
		}
		if ftyp.len == 0 {
			continue
		}
		if found != '' && found != ftyp {
			return none
		}
		found = ftyp
	}
	if found.len == 0 {
		return none
	}
	return flat.source_type_id(found)
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
fn (t &Transformer) enum_type_name_from_expr(id flat.NodeId) ?string {
	name := t.selector_expr_name(id)
	return t.enum_type_name_from_selector_name(name)
}

fn (t &Transformer) enum_type_name_from_selector_name(name string) ?string {
	if name == '' {
		return none
	}
	if name in t.enum_types {
		return name
	}
	resolved := flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(name), t.cur_module))
	if resolved in t.enum_types {
		return resolved
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return qname
		}
	}
	return none
}

// qualified_enum_type_selector_name
// supports helper handling in transform.
fn (t &Transformer) qualified_enum_type_selector_name(base_id flat.NodeId, field_name string) ?string {
	base := t.selector_expr_name(base_id)
	return t.qualified_enum_type_selector_name_from_selector_name(base, field_name)
}

fn (t &Transformer) qualified_enum_type_selector_name_from_selector_name(base string, field_name string) ?string {
	if base == '' || field_name == '' {
		return none
	}
	name := '${base}.${field_name}'
	if name in t.enum_types {
		return name
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return qname
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
	key := if isnil(t.struct_field_type_cache) {
		u64(0)
	} else {
		t.struct_field_type_cache_key(type_name_id, field_name)
	}
	if !isnil(t.struct_field_type_cache) {
		mut cache := t.struct_field_type_cache
		if cached := cache.entries[key] {
			return cached
		}
		if cache.misses[key] {
			return none
		}
	}
	resolved_source_id := t.lookup_struct_field_type_uncached(type_name_id, field_name) or {
		if !isnil(t.struct_field_type_cache) {
			mut cache := t.struct_field_type_cache
			cache.misses[key] = true
		}
		return none
	}

	if !isnil(t.struct_field_type_cache) {
		mut cache := t.struct_field_type_cache
		cache.entries[key] = resolved_source_id
	}
	return resolved_source_id
}

fn (t &Transformer) struct_field_type_cache_key(type_name_id u32, field_name string) u64 {
	mut cache := t.struct_field_type_cache
	context_id := cache.contexts.intern('${t.cur_module}\n${field_name}')
	return (u64(type_name_id) << 32) | u64(context_id)
}

fn (t &Transformer) lookup_struct_field_type_uncached(type_name_id u32, field_name string) ?u32 {
	lookup := t.lookup_struct_info_for_field(type_name_id, field_name) or { return none }
	return t.struct_field_type_in_cur_module(lookup, t.lookup_struct_field_type_spelling(lookup,
		field_name)?)
}

fn (t &Transformer) lookup_struct_field_type_spelling(lookup StructFieldLookup, field_name string) ?u32 {
	f := lookup.info.field(field_name) or { return none }
	if lookup.owner_type_text().contains('[') {
		specialized_source_id := t.normalize_field_type(f.typ, lookup.owner_type)
		specialized := flat.source_type_name(specialized_source_id)
		if decl_type_is_usable(specialized) && !t.generic_arg_is_unresolved(specialized_source_id) {
			return specialized_source_id
		}
	}
	if checker_typ_source_id := t.checker_struct_field_type_name(lookup.owner_type, field_name) {
		return checker_typ_source_id
	}
	if !lookup.owner_type_text().contains('[') {
		return f.typ
	}
	return t.normalize_field_type(f.typ, lookup.owner_type)
}

// struct_field_type_in_cur_module spells a field type for use in the current module.
// Main-module types are stored bare, so a `[]Cell` field of a main struct read inside
// an imported generic specialization must stay `main.Cell` instead of rebinding to the
// current module's own `Cell` when the text is parsed again.
fn (t &Transformer) struct_field_type_in_cur_module(lookup StructFieldLookup, field_type_id u32) u32 {
	field_type := flat.source_type_name(field_type_id)
	if t.cur_module in ['', 'main', 'builtin'] || lookup.info.module !in ['', 'main'] {
		return field_type_id
	}
	return flat.source_type_id(t.lock_colliding_main_generic_type_text(field_type, t.cur_module))
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
		flat.source_type_id('${lookup.info.module}.${lookup.owner_type_text()}')
	}
	return raw_id, owner_id
}

fn (t &Transformer) checker_struct_field_type_name(type_name_id u32, field_name string) ?u32 {
	type_name := flat.source_type_name(type_name_id)
	if isnil(t.tc) {
		return none
	}
	mut lookup_type := if type_name.starts_with('&') { type_name[1..] } else { type_name }
	if checker_typ_source_id := t.tc.struct_field_type_name(flat.source_type_id(lookup_type), field_name) {
		return checker_typ_source_id
	}
	if is_qualified_type_name_text(lookup_type) {
		short := lookup_type.all_after_last('.')
		if checker_typ_source_id := t.tc.struct_field_type_name(flat.source_type_id(short), field_name) {
			return checker_typ_source_id
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${lookup_type}'
		if checker_typ_source_id := t.tc.struct_field_type_name(flat.source_type_id(qtype), field_name) {
			return checker_typ_source_id
		}
	}
	normalized_source_id := t.normalize_type_alias(flat.source_type_id(lookup_type))
	normalized := flat.source_type_name(normalized_source_id)
	if normalized != lookup_type {
		if checker_typ_source_id := t.tc.struct_field_type_name(normalized_source_id, field_name) {
			return checker_typ_source_id
		}
	}
	return none
}

// is_qualified_type_name_text reports whether `text` is a module-qualified type name
// (`mod.Type`, `a.b.Type`): only name characters and dots. A composite type such as
// `[]mod.Type` or `map[string]mod.Type` is not, so its last segment is not its name.
fn is_qualified_type_name_text(text string) bool {
	if !text.contains('.') {
		return false
	}
	for c in text {
		if !(c.is_letter() || c.is_digit() || c == `_` || c == `.`) {
			return false
		}
	}
	return true
}

// lookup_struct_info_for_field resolves lookup struct info for field information for transform.
fn (t &Transformer) lookup_struct_info_for_field(type_name_id u32, field_name string) ?StructFieldLookup {
	type_name := flat.source_type_name(type_name_id)
	if type_name_id == u32(0) || field_name == '' {
		return none
	}
	mut lookup_type := if type_name.starts_with('&') { type_name[1..] } else { type_name }
	owner_type_id := if type_name.starts_with('&') {
		flat.source_type_id(lookup_type)
	} else {
		type_name_id
	}
	base, _, is_generic_app := generic_app_parts(lookup_type)
	if is_generic_app {
		lookup_type = base
	}
	if lookup_type !in t.structs && is_qualified_type_name_text(lookup_type) {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	if !lookup_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${lookup_type}'
		if qtype in t.structs {
			lookup_type = qtype
		} else if !isnil(t.tc) {
			if target_source_id := t.tc.type_aliases.type_id(qtype) {
				target := flat.source_type_name(target_source_id)

				lookup_type = target
			}
		}
	}
	if lookup_type !in t.structs {
		normalized := flat.source_type_name(t.normalize_type_alias(flat.source_type_id(lookup_type)))
		if normalized != lookup_type {
			lookup_type = normalized
		}
	}
	info := t.structs[lookup_type] or { return none }
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
	text := typ.name()
	if text.len == 0 || type_text_has_unresolved_generic_placeholder(text) {
		return none
	}
	return flat.source_type_id(text)
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
	typ := flat.source_type_name(typ_id)
	owner_type := flat.source_type_name(owner_type_id)
	if typ_id == u32(0) {
		return typ_id
	}
	if typ.starts_with('mut ') {
		return flat.source_type_id('mut ' + flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[4..]), owner_type_id, allow_owner_substitution)))
	}
	if typ.starts_with('shared ') {
		return flat.source_type_id('shared ' + flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[7..]), owner_type_id, allow_owner_substitution)))
	}
	if typ.starts_with('atomic ') {
		return flat.source_type_id('atomic ' + flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[7..]), owner_type_id, allow_owner_substitution)))
	}
	if typ.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[1..]), owner_type_id, allow_owner_substitution)))
	}
	if typ.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[2..]), owner_type_id, allow_owner_substitution)))
	}
	// `?T` of an option `T` is that option, as the checker substitutes it.
	if typ.starts_with('?') {
		inner := flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[1..]), owner_type_id, allow_owner_substitution))
		return flat.source_type_id(if inner.starts_with('?') { inner } else { '?' + inner })
	}
	if typ.starts_with('!') {
		inner := flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[1..]), owner_type_id, allow_owner_substitution))
		return flat.source_type_id(if inner.starts_with('!') { inner } else { '!' + inner })
	}
	if typ.starts_with('map[') {
		bracket_end := typ.index(']') or { return t.normalize_type_alias(typ_id) }
		key_type := flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[4..bracket_end]), owner_type_id, allow_owner_substitution))
		value_type := flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[bracket_end + 1..]), owner_type_id, allow_owner_substitution))
		return flat.source_type_id('map[${key_type}]${value_type}')
	}
	if typ.starts_with('[') {
		bracket_end := typ.index(']') or { return t.normalize_type_alias(typ_id) }
		mut len_text := typ[1..bracket_end]
		if allow_owner_substitution {
			owner_base, owner_args, owner_is_generic_app := generic_app_parts(owner_type)
			if owner_is_generic_app && owner_base.len > 0 {
				params := t.generic_struct_param_names_for_base(owner_base)
				len_text = substitute_generic_expr_text_with_params(len_text, flat.TypeTextList.from_names(owner_args).ids, params)
			}
		}
		return flat.source_type_id('[${len_text}]' + flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(typ[bracket_end + 1..]), owner_type_id, allow_owner_substitution)))
	}
	owner_base, owner_args, owner_is_generic_app := generic_app_parts(owner_type)
	if allow_owner_substitution && owner_is_generic_app {
		if owner_base.len > 0 {
			params := t.generic_struct_param_names_for_base(owner_base)
			// Arguments belong to the instantiation site, rather than the module
			// declaring the generic field. Qualify them before owner substitution.
			mut qualified_args := []u32{cap: owner_args.len}
			for arg in owner_args {
				qualified_args << t.qualify_generic_arg_for_decl_module(flat.source_type_id(arg), t.cur_module)
			}
			substituted := if params.len > 0 {
				substitute_generic_type_text_with_params(typ, qualified_args, params)
			} else {
				substitute_generic_type_text(typ, qualified_args)
			}
			if substituted != typ {
				return t.normalize_field_type_with_owner_substitution(flat.source_type_id(substituted), owner_type_id, false)
			}
		}
	}
	base, args, type_is_generic_app := generic_app_parts(typ)
	if type_is_generic_app {
		mut field_base := base
		if !field_base.contains('.') && owner_type.contains('.') {
			owner_mod := field_owner_module(owner_type)
			qbase := '${owner_mod}.${field_base}'
			if t.type_authority_has(qbase) {
				field_base = qbase
			}
		}
		mut normalized_args := []string{cap: args.len}
		for arg in args {
			normalized_args << flat.source_type_name(t.normalize_field_type_with_owner_substitution(flat.source_type_id(arg), owner_type_id, allow_owner_substitution))
		}
		return t.normalize_type_alias(flat.source_type_id('${field_base}[${normalized_args.join(', ')}]'))
	}
	if typ.contains('.') || !owner_type.contains('.') {
		return t.normalize_type_alias(typ_id)
	}
	owner_mod := field_owner_module(owner_type)
	qtyp := '${owner_mod}.${typ}'
	if t.type_authority_has(qtyp) {
		return t.normalize_type_alias(flat.source_type_id(qtyp))
	}
	if !isnil(t.tc) {
		if t.tc.type_aliases.contains(qtyp) {
			return t.normalize_type_alias(flat.source_type_id(qtyp))
		}
	}
	return t.normalize_type_alias(typ_id)
}

fn field_owner_module(owner_type string) string {
	for foreign_prefix in ['.C.', '.JS.'] {
		if idx := owner_type.index(foreign_prefix) {
			return owner_type[..idx]
		}
	}
	return owner_type.all_before_last('.')
}

fn (t &Transformer) generic_struct_param_names_for_base(base string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	if params := t.tc.struct_generic_params[base] {
		return params.clone()
	}
	short := base.all_after_last('.')
	if params := t.tc.struct_generic_params[short] {
		return params.clone()
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		if params := t.tc.struct_generic_params['${t.cur_module}.${base}'] {
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
fn (t &Transformer) normalize_type_alias(type_id u32) u32 {
	if type_id == 0 || isnil(t.tc) { return type_id }
	mut texts := t.var_type_texts
	if isnil(t.alias_cache) {
		return texts.intern(t.normalize_type_alias_uncached(texts.text(type_id)))
	}
	mut c := t.alias_cache
	if c.texts != t.var_type_texts {
		// Hand-built integrations may supply a cache with another namespace.
		// Cache entries are disposable; binding identities always belong to t.
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
	// Only a miss crosses back to the source/alias spelling resolver.
	typ := texts.text(type_id)
	if fast := t.normalize_dotted_canonical_fast(typ) {
		result_id := texts.intern(fast)
		c.entries[type_id] = result_id
		c.recent_types[recent_slot] = type_id
		c.recent_results[recent_slot] = result_id
		c.recent_generations[recent_slot] = c.recent_generation
		c.canonical_types[recent_slot] = type_id
		return result_id
	}
	result := t.normalize_type_alias_uncached(typ)
	result_id := texts.intern(result)
	c.entries[type_id] = result_id
	c.recent_types[recent_slot] = type_id
	c.recent_results[recent_slot] = result_id
	c.recent_generations[recent_slot] = c.recent_generation
	if result_id == type_id && is_plain_builtin_alias_type(typ) {
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
fn (t &Transformer) normalize_dotted_canonical_fast(typ string) ?string {
	mut has_dot := false
	for i in 0 .. typ.len {
		c := typ[i]
		if c == `.` {
			has_dot = true
			continue
		}
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		return none
	}
	if !has_dot {
		return none
	}
	first := typ[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return none
	}
	if t.tc.type_aliases.contains(typ) {
		return none
	}
	if t.type_authority_has(typ) {
		return typ
	}
	return none
}

// normalize_type_alias_uncached converts normalize type alias uncached data for transform.
fn (t &Transformer) normalize_type_alias_uncached(typ string) string {
	if is_generic_placeholder_type_name(typ) && !t.is_known_type_name(typ) {
		return typ
	}
	if typ.starts_with('mut ') {
		return '&' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[4..])))
	}
	if typ.starts_with('&void[') && typ.ends_with(']') {
		return 'voidptr' + typ['&void'.len..]
	}
	if typ.starts_with('&') {
		return '&' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[1..])))
	}
	if typ.starts_with('[]') {
		return '[]' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[2..])))
	}
	if typ.starts_with('chan ') {
		return 'chan ' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[5..])))
	}
	if typ.starts_with('[') {
		bracket_end := typ.index(']') or { return typ }
		return typ[..bracket_end + 1] + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[bracket_end + 1..])))
	}
	if typ.starts_with('map[') {
		bracket_end := generic_matching_bracket(typ, 3)
		if bracket_end < typ.len {
			key := flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[4..bracket_end])))
			value := flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[bracket_end + 1..])))
			return 'map[${key}]${value}'
		}
	}
	if typ.starts_with('?') {
		return '?' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[1..])))
	}
	if typ.starts_with('!') {
		return '!' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(typ[1..])))
	}
	if expanded := t.expand_generic_type_alias(typ) {
		return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(expanded)))
	}
	// A fully qualified type is already canonical. Resolve its exact alias before
	// consulting imports from the current file; otherwise an unrelated import with
	// the same trailing type name can steal the identity (for example
	// `v.ssa.TypeID` being resolved as `v.types.Type`).
	if typ.contains('.') {
		if target_source_id := t.tc.type_aliases.type_id(typ) {
			return flat.source_type_name(t.normalize_type_alias(target_source_id))
		}
		if t.type_authority_has(typ) {
			return typ
		}
	}
	// Resolve the importing file's alias before any short-name or suffix fallback.
	// Two packages can both expose `tast.Value`; `other_tast.Value` and
	// `tast.Value` must retain their exact canonical module identities.
	if imported := t.resolve_imported_type_name(typ) {
		if target_source_id := t.tc.type_aliases.type_id(imported) {
			return flat.source_type_name(t.normalize_type_alias(target_source_id))
		}
		if t.type_authority_has(imported) {
			return imported
		}
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
		if target_source_id := t.tc.type_aliases.type_id(qtyp) {
			target := flat.source_type_name(target_source_id)

			return target
		}
		if t.type_authority_has(qtyp) {
			return qtyp
		}
	}
	// Imported aliases are indexed by their short spelling. A main-module struct
	// with the same name remains authoritative for an unqualified reference.
	if t.bare_struct_name_is_local_to_current_module(typ) {
		return typ
	}
	if target_source_id := t.tc.type_aliases.type_id(typ) {
		target := flat.source_type_name(target_source_id)

		return target
	}
	if typ in t.structs || t.sum_types.contains(typ) || typ in t.enum_types {
		return typ
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
		if target_source_id := t.tc.type_aliases.type_id(qtyp) {
			target := flat.source_type_name(target_source_id)

			return target
		}
	}
	if !typ.contains('.') {
		if unique_target := t.type_alias_suffixes[typ] {
			if unique_target != 0 {
				return flat.source_type_name(unique_target)
			}
		}
	}
	return typ
}

fn (mut t Transformer) build_type_alias_suffix_index() {
	t.type_alias_suffixes = map[string]u32{}
	mut by_short := map[string][]TypeAliasEntry{}
	for name, target_id in t.tc.type_aliases.ids {
		target := flat.source_type_name(target_id)
		short := if name.contains('.') { name.all_after_last('.') } else { name }
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
		target := flat.source_type_name(target_id)
		if short_name_view(name) == short {
			entries << TypeAliasEntry{
				name:   name
				target: target_id
			}
		}
	}
	return entries
}

fn (t &Transformer) expand_generic_type_alias(typ string) ?string {
	base, args, ok := generic_app_parts(typ)
	if !ok || args.len == 0 {
		return none
	}
	mut candidates := [base]
	if imported := t.resolve_imported_type_name(base) {
		if imported !in candidates {
			candidates << imported
		}
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qualified := '${t.cur_module}.${base}'
		if qualified !in candidates {
			candidates << qualified
		}
	}
	for candidate in candidates {
		target := flat.source_type_name(t.tc.type_aliases.type_id(candidate) or { continue })
		params := t.tc.type_alias_generic_params[candidate] or { continue }
		if params.len != args.len {
			continue
		}
		expanded := substitute_generic_type_text_with_params(target, flat.TypeTextList.from_names(args).ids, params)
		if candidate.contains('.') {
			// Alias targets are written in the alias declaration's module.
			// Qualify a bare target there before the caller recursively
			// normalizes it, otherwise `a.Box[T] = Inner[T]` can bind to a
			// colliding `main.Inner[T]` at the use site.
			return flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(expanded), candidate.all_before_last('.')))
		}
		return expanded
	}
	return none
}

fn strip_field_module_prefix_from_type(typ string, module_name string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || module_name == '' {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + strip_field_module_prefix_from_type(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + strip_field_module_prefix_from_type(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + strip_field_module_prefix_from_type(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + strip_field_module_prefix_from_type(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + strip_field_module_prefix_from_type(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := strip_field_module_prefix_from_type(clean[4..bracket_end], module_name)
			value := strip_field_module_prefix_from_type(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + strip_field_module_prefix_from_type(clean[bracket_end + 1..], module_name)
		}
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		stripped_base := strip_field_module_prefix_from_base(base, module_name)
		mut stripped_args := []string{cap: args.len}
		for arg in args {
			stripped_args << strip_field_module_prefix_from_type(arg, module_name)
		}
		return '${stripped_base}[${stripped_args.join(', ')}]'
	}
	return strip_field_module_prefix_from_base(clean, module_name)
}

fn strip_field_module_prefix_from_base(name string, module_name string) string {
	prefix := '${module_name}.'
	if !name.starts_with(prefix) {
		return name
	}
	short := name[prefix.len..]
	if short.starts_with('Array_') {
		return name
	}
	return short
}

fn (t &Transformer) is_known_type_name(typ string) bool {
	if typ in t.structs || t.sum_types.contains(typ) || typ in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		if t.tc.type_aliases.contains(typ) || typ in t.tc.structs || t.tc.sum_types.contains(typ)
			|| typ in t.tc.enum_names || typ in t.tc.interface_names {
			return true
		}
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
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
fn is_plain_builtin_alias_type(typ string) bool {
	// 'i8'..'voidptr': every plain builtin spelling is 2..7 bytes long.
	if typ.len < 2 || typ.len > 7 {
		return false
	}
	return match typ {
		'bool', 'string', 'void', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
		'f64', 'rune', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr' {
			true
		}
		else {
			false
		}
	}
}

// is_generic_placeholder_type_name reports is_generic_placeholder_type_name logic in transform.
fn is_generic_placeholder_type_name(typ string) bool {
	if typ.contains('.') {
		return is_generic_placeholder_type_name(typ.all_after_last('.'))
	}
	return typ in ['T', 'U', 'V', 'K']
}

// normalize_type_in_module transforms normalize type in module data for transform.
fn (t &Transformer) normalize_type_in_module(type_id u32, mod string) u32 {
	if flat.source_type_shape(type_id).has(.builtin_alias) { return type_id }
	if isnil(t.module_type_cache) {
		return flat.source_type_id(t.normalize_type_in_module_uncached(flat.source_type_name(type_id), mod))
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
		result = flat.source_type_id(t.normalize_type_in_module_uncached(flat.source_type_name(type_id), mod))
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

fn (t &Transformer) normalize_type_in_module_uncached(typ string, mod string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[1..]), mod))
	}
	if clean.starts_with('mut ') {
		return '&' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[4..]), mod))
	}
	if clean.starts_with('shared ') {
		return 'shared ' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[7..]), mod))
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[7..]), mod))
	}
	if clean.starts_with('?') {
		return '?' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[1..]), mod))
	}
	if clean.starts_with('!') {
		return '!' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[1..]), mod))
	}
	if clean.starts_with('...') {
		return '...' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[3..]), mod))
	}
	if clean.starts_with('[]') {
		return '[]' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[2..]), mod))
	}
	if clean.starts_with('chan ') {
		return 'chan ' + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[5..]), mod))
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end > 3 && bracket_end < clean.len {
			key_type := flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[4..bracket_end]), mod))
			value_type := flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[bracket_end + 1..]), mod))
			return 'map[${key_type}]${value_type}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 0 && bracket_end < clean.len - 1 {
			return clean[..bracket_end + 1] + flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean[bracket_end + 1..]), mod))
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		params, ret := fn_type_text_parts(clean) or { return clean }
		mut normalized_params := []string{cap: params.len}
		for param in params {
			raw_param := trimmed_transform_text(param)
			payload := generic_fn_type_param_payload(raw_param)
			param_type := if raw_param.starts_with('mut ') { 'mut ${payload}' } else { payload }
			normalized_params << flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(param_type), mod))
		}
		normalized_ret_source_id := t.normalize_type_in_module(flat.source_type_id(ret), mod)
		normalized_ret := flat.source_type_name(normalized_ret_source_id)
		return if normalized_ret_source_id != 0 {
			'fn (${normalized_params.join(', ')}) ${normalized_ret}'
		} else {
			'fn (${normalized_params.join(', ')})'
		}
	}
	base, args, is_generic_app := generic_app_parts(clean)
	if is_generic_app {
		// Expand the applied alias before its base: expanding only `Values` in
		// `Values[Item]` would leave the arguments attached to its `[]T` target.
		alias_type := if !base.contains('.') && mod.len > 0
			&& t.type_authority_has('${mod}.${base}') {
			'${mod}.${clean}'
		} else {
			clean
		}
		if !isnil(t.tc) {
			if expanded := t.expand_generic_type_alias(alias_type) {
				return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(expanded)))
			}
		}
		mut normalized_args := []string{cap: args.len}
		for arg in args {
			normalized_args << flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(arg), mod))
		}
		return '${flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(base), mod))}[${normalized_args.join(', ')}]'
	}
	if clean.contains('.') || mod == '' || mod == 'main' || mod == 'builtin' {
		return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(clean)))
	}
	if !isnil(t.tc) && mod == t.cur_module && t.cur_file.len > 0 {
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
			return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(imported)))
		}
	}
	qtyp := '${mod}.${clean}'
	if t.type_authority_has(qtyp) {
		return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(qtyp)))
	}
	if !isnil(t.tc) {
		if t.tc.type_aliases.contains(qtyp) {
			return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(qtyp)))
		}
	}
	return flat.source_type_name(t.normalize_type_alias(flat.source_type_id(clean)))
}

// resolve_index_elem_type determines the element type of an .index expression.
// For `[]T` arrays returns `T`, for `map[K]V` returns `V`, for `string` returns `u8`.
fn (t &Transformer) resolve_index_elem_type(node flat.Node) u32 {
	if node.kind != .index || node.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.child(&node, 0)
	mut base_type := flat.source_type_name(t.resolve_expr_type(base_id))
	if base_type.len == 0 {
		base_node := t.a.nodes[int(base_id)]
		if base_node.kind == .selector {
			// Selector nodes can share their compact child slice with an earlier
			// speculative node. Re-resolve a cached miss from the actual slice
			// receiver before trusting the checker's index fallback.
			base_type = flat.source_type_name(t.resolve_selector_type_uncached(base_node))
		}
	}
	if base_type.len == 0 {
		return u32(0)
	}
	base_type = flat.source_type_name(t.normalize_type_alias(flat.source_type_id(base_type)))
	if base_type.starts_with('&') {
		ptr_elem_type_source_id := t.normalize_type_alias(flat.source_type_id(base_type[1..]))
		ptr_elem_type := flat.source_type_name(ptr_elem_type_source_id)
		// A `mut map`/`mut []T` param is a pointer to the container; indexing it must
		// still yield the element/value type, so fall through to the container cases.
		if !ptr_elem_type.starts_with('[]') && !ptr_elem_type.starts_with('map[')
			&& !t.is_fixed_array_type(ptr_elem_type_source_id) {
			return ptr_elem_type_source_id
		}
		base_type = ptr_elem_type
	}
	is_slice := node.value == 'range'
		|| (node.children_count > 1 && t.a.child_node(&node, 1).kind == .range)
	if is_slice {
		if base_type == 'string' {
			return flat.source_type_string
		}
		if base_type.starts_with('[]') {
			return flat.source_type_id(base_type)
		}
		// A range/slice of a fixed array (`arr[..]`, `arr[a..b]`) yields a dynamic
		// `[]T`, not the fixed array or a bogus `range` type.
		if t.is_fixed_array_type(flat.source_type_id(base_type)) {
			return flat.source_type_id('[]${flat.source_type_name(t.normalize_type_alias(flat.source_type_id(for_in_fixed_array_elem_type(base_type))))}')
		}
	}
	if base_type.starts_with('[]') {
		return t.normalize_type_alias(flat.source_type_id(base_type[2..]))
	}
	if t.is_fixed_array_type(flat.source_type_id(base_type)) {
		return t.normalize_type_alias(flat.source_type_id(for_in_fixed_array_elem_type(base_type)))
	}
	if base_type.starts_with('map[') {
		bracket_end := base_type.index(']') or { return u32(0) }
		if bracket_end + 1 < base_type.len {
			return t.normalize_type_alias(flat.source_type_id(base_type[bracket_end + 1..]))
		}
		return u32(0)
	}
	if base_type == 'string' {
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
@[direct_array_access]
fn (t &Transformer) node_type(id flat.NodeId) u32 {
	idx := int(id)
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
	}
	return t.node_type_uncached(id)
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
		if t.validating_generic_spec && node.typ_text().contains('main.')
			&& !t.ident_is_import_alias('main') && decl_type_is_usable(node.typ_text()) {
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
	resolved := flat.source_type_name(resolved_source_id)
	if resolved_source_id != 0 {
		if node.kind == .array_literal && t.generic_arg_is_unresolved(resolved_source_id) {
			elem_type_source_id := t.array_literal_elem_type(node)
			elem_type := flat.source_type_name(elem_type_source_id)
			if decl_type_is_usable(elem_type) && !t.generic_arg_is_unresolved(elem_type_source_id) {
				if t.is_fixed_array_type(resolved_source_id) {
					return flat.source_type_id('[${fixed_array_len_text(resolved)}]${elem_type}')
				}
				return flat.source_type_id('[]${elem_type}')
			}
		}
		// The checker override only applies to named struct types. Most expressions
		// are builtin scalars or containers; avoid parsing both types and deriving
		// their C names for those overwhelmingly common cases.
		if !t.building_v && type_text_may_name_struct(resolved) {
			if checked_source_id := t.checker_type_over_struct_guess(id, resolved_source_id) {
				return checked_source_id
			}
		}
		if types.type_text_contains_typeof(resolved) && !isnil(t.tc) {
			if typ := t.tc.expr_type(id) {
				name_source_id := t.normalize_type_alias(t.tc.type_name(typ))
				name := flat.source_type_name(name_source_id)
				if decl_type_is_usable(name) {
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
			checker_resolved := flat.source_type_name(checker_resolved_source_id)
			if decl_type_is_usable(checker_resolved) && checker_resolved_source_id != flat.source_type_void {
				return t.normalize_type_alias(checker_resolved_source_id)
			}
		}
		child_type_source_id_2 := t.node_type(t.a.child(&node, 0))
		child_type := flat.source_type_name(child_type_source_id_2)
		if child_type_source_id_2 != 0 {
			if t.is_optional_type_name(child_type_source_id_2) {
				return flat.source_type_id('${child_type[..1]}&${flat.source_type_name(t.optional_base_type(child_type_source_id_2))}')
			}
			return flat.source_type_id('&${child_type}')
		}
	}
	mut deferred_call_typ := u32(0)
	if node.typ != 0 {
		node_typ_source_id_2 := t.normalize_type_alias(node.typ)
		node_typ := flat.source_type_name(node_typ_source_id_2)
		if node.kind == .call && node_typ in ['int', 'array', 'map', 'unknown'] {
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
	if node.kind == .struct_init && node.value.len > 0 {
		for candidate in [node.typ_text(), node.value] {
			if candidate.len == 0 {
				continue
			}
			normalized_source_id := t.normalize_type_alias(flat.source_type_id(candidate))
			normalized := flat.source_type_name(normalized_source_id)
			if normalized != candidate || normalized.contains('[') {
				return normalized_source_id
			}
		}
		if node.value in t.structs {
			return flat.source_type_id(node.value)
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value}'
			if qname in t.structs {
				return flat.source_type_id(qname)
			}
		}
		return flat.source_type_id(node.value)
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

@[inline]
fn type_text_may_name_struct(typ string) bool {
	mut start := 0
	for start < typ.len && typ[start] == `&` {
		start++
	}
	if start >= typ.len {
		return false
	}
	// Only read below, while `typ` owns the bytes.
	clean := unsafe { typ.substr_unsafe(start, typ.len) }
	if is_plain_builtin_alias_type(clean) {
		return false
	}
	return !clean.starts_with('[]') && !clean.starts_with('map[') && !clean.starts_with('[')
		&& !clean.starts_with('fn(') && !clean.starts_with('fn (') && !clean.starts_with('chan ')
		&& !clean.starts_with('?') && !clean.starts_with('!')
}

fn (t &Transformer) checker_type_over_struct_guess(id flat.NodeId, guessed_id u32) ?u32 {
	if isnil(t.tc) || int(id) < 0 || guessed_id == u32(0) {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		local_type_source_id_2 := t.normalized_var_type(node.value)
		local_type := flat.source_type_name(local_type_source_id_2)
		if local_type_source_id_2 != 0 && local_type == flat.source_type_name(t.normalize_type_alias(guessed_id)) {
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
	checked := flat.source_type_name(checked_source_id)
	if !decl_type_is_usable(checked) {
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
	base_type0 := flat.source_type_name(t.node_type(t.a.child(fn_node, 0)))
	base_type := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	if elem_type.len == 0 {
		return none
	}
	match fn_node.value {
		'filter', 'clone', 'reverse', 'sorted', 'repeat', 'repeat_to_depth' {
			return flat.source_type_id(base_type)
		}
		'first', 'last', 'pop', 'pop_left' {
			return flat.source_type_id(elem_type)
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
		callback_ret := flat.source_type_name(callback_ret_source_id)

		return flat.source_type_id('[]${callback_ret}')
	}
	if checker_type_source_id := t.checker_expr_type_name(id) {
		checker_type := flat.source_type_name(checker_type_source_id)

		if checker_type.starts_with('[]') {
			return checker_type_source_id
		}
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type0 := flat.source_type_name(t.node_type(base_id))
	if !base_type0.trim_string_left('&').starts_with('[]') {
		base_type0 = flat.source_type_name(t.original_expr_type(base_id))
	}
	base_type := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if !base_type.starts_with('[]') {
		return none
	}
	mut elem_type := if callback_ret_source_id := t.array_map_callback_return_type_name(map_expr_id) {
		callback_ret := flat.source_type_name(callback_ret_source_id)

		callback_ret
	} else if checker_type_source_id := t.checker_expr_type_name(map_expr_id) {
		checker_type := flat.source_type_name(checker_type_source_id)

		checker_type
	} else {
		flat.source_type_name(t.node_type(map_expr_id))
	}
	if elem_type.len == 0 || elem_type in ['array', 'map', 'unknown'] {
		elem_type = t.reliable_stringify_type(map_expr_id)
	}
	if elem_type.len == 0 || elem_type in ['array', 'map', 'unknown', 'void'] {
		return none
	}
	return flat.source_type_id('[]${elem_type}')
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
		sel_type := flat.source_type_name(sel_type_source_id)
		if !isnil(t.tc) {
			if checker_type := t.tc.expr_type(id) {
				checker_name := checker_type.name()
				if checker_name.starts_with('&') && !sel_type.starts_with('&') {
					return flat.source_type_id(checker_name)
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
	mut typ := ''
	if sc := t.find_smartcast(t.expr_key(id)) {
		typ = flat.source_type_name(t.smartcast_target_type(sc))
	}
	if typ.len == 0 {
		typ = flat.source_type_name(t.node_type(id))
	}
	if typ.len == 0 {
		typ = flat.source_type_name(t.raw_checker_node_type(id))
	}
	return t.normalize_type_alias(flat.source_type_id(typ)) == flat.source_type_string
}

// is_array_type checks if a type string represents an array type (starts with `[]`).
fn (t &Transformer) is_array_type(type_str string) bool {
	return type_str.starts_with('[]')
}

// is_map_type checks if a type string represents a map type (starts with `map[`).
fn (t &Transformer) is_map_type(type_str string) bool {
	return type_str.starts_with('map[')
}

// array_elem_type extracts the element type from an array type string.
// `[]int` -> `int`, `[][]string` -> `[]string`.
fn (t &Transformer) array_elem_type(type_str_id u32) u32 {
	type_str := flat.source_type_name(type_str_id)
	if type_str.starts_with('[]') {
		return flat.source_type_id(type_str[2..])
	}
	return u32(0)
}

// map_value_type extracts the value type from a map type string.
// `map[string]int` -> `int`, `map[string][]bool` -> `[]bool`.
fn (t &Transformer) map_value_type(type_str_id u32) u32 {
	type_str := flat.source_type_name(type_str_id)
	if !type_str.starts_with('map[') {
		return u32(0)
	}
	bracket_end := generic_matching_bracket(type_str, 3)
	if bracket_end + 1 < type_str.len {
		return flat.source_type_id(type_str[bracket_end + 1..])
	}
	return u32(0)
}

// map_key_type extracts the key type from a map type string.
// `map[string]int` -> `string`, `map[int]bool` -> `int`.
fn (t &Transformer) map_key_type(type_str_id u32) u32 {
	type_str := flat.source_type_name(type_str_id)
	if !type_str.starts_with('map[') {
		return u32(0)
	}
	bracket_end := generic_matching_bracket(type_str, 3)
	if bracket_end > 4 {
		return flat.source_type_id(type_str[4..bracket_end])
	}
	return u32(0)
}

// target_text resolves the stored source spelling for a text consumer.
fn (value &TypeAliasEntry) target_text() string {
	return flat.source_type_name(value.target)
}
