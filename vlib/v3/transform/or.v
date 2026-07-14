module transform

import v3.flat
import v3.types

// ArrayIndexInfo stores array index info metadata used by transform.
struct ArrayIndexInfo {
	base_id        flat.NodeId
	index_id       flat.NodeId
	base_type      string
	value_type     string
	optional_type  string
	base_optional  bool
	is_fixed_array bool
}

struct StringSliceInfo {
	base_id  flat.NodeId
	start_id flat.NodeId
	end_id   flat.NodeId
	has_end  bool
}

struct ChannelReceiveInfo {
	channel_id  flat.NodeId
	value_type  string
	needs_deref bool
}

struct MultiReturnTailValues {
	values      []flat.NodeId
	start_index int
}

// EnumFromStringInfo stores enum from string info metadata used by transform.
struct EnumFromStringInfo {
	enum_type string
	fields    []string
	arg_id    flat.NodeId
}

fn (t &Transformer) enum_from_string_members(info EnumFromStringInfo) []EnumValueMeta {
	metas := t.comptime_enum_members(info.enum_type)
	if metas.len > 0 {
		return metas
	}
	mut fallback := []EnumValueMeta{cap: info.fields.len}
	for idx, field in info.fields {
		fallback << EnumValueMeta{
			name:  field
			value: idx
		}
	}
	return fallback
}

fn (mut t Transformer) enum_from_string_member_value(info EnumFromStringInfo, member EnumValueMeta) flat.NodeId {
	if t.is_flag_enum_type(info.enum_type) && t.flag_enum_has_backing_type(info.enum_type) {
		return t.a.add_node(flat.Node{
			kind:  .enum_val
			value: '${info.enum_type}.${member.name}'
			typ:   info.enum_type
		})
	}
	return t.make_int_literal_typed(member.value.str(), info.enum_type)
}

// optional_base_type supports optional base type handling for Transformer.
fn (t &Transformer) optional_base_type(typ string) string {
	if typ.len > 1 && (typ[0] == `?` || typ[0] == `!`) {
		return typ[1..]
	}
	return typ
}

// array_index_info supports array index info handling for Transformer.
fn (mut t Transformer) array_index_info(index_id flat.NodeId) ?ArrayIndexInfo {
	if int(index_id) < 0 {
		return none
	}
	expr := t.a.nodes[int(index_id)]
	if expr.kind != .index || expr.children_count < 2 || expr.value == 'range' {
		return none
	}
	base_id := t.a.child(&expr, 0)
	index_expr_id := t.a.child(&expr, 1)
	mut base_type := t.resolve_expr_type(base_id)
	for base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	for base_type.starts_with('shared ') {
		base_type = base_type[7..].trim_space()
	}
	mut optional_type := ''
	mut base_optional := false
	if t.is_optional_type_name(base_type) {
		optional_type = t.qualify_optional_type(base_type)
		inner_type := t.optional_base_type(optional_type)
		if inner_type.starts_with('[]') {
			base_type = inner_type
			base_optional = true
		}
	}
	if t.normalize_type_alias(base_type) == 'string' {
		return ArrayIndexInfo{
			base_id:       base_id
			index_id:      index_expr_id
			base_type:     'string'
			value_type:    'u8'
			optional_type: optional_type
			base_optional: base_optional
		}
	}
	if t.is_fixed_array_type(base_type) {
		fixed_type := t.resolved_fixed_array_canonical_type(base_type)
		return ArrayIndexInfo{
			base_id:        base_id
			index_id:       index_expr_id
			base_type:      fixed_type
			value_type:     fixed_array_elem_type(fixed_type)
			optional_type:  optional_type
			base_optional:  base_optional
			is_fixed_array: true
		}
	}
	if !base_type.starts_with('[]') {
		return none
	}
	value_type := t.normalize_type_alias(base_type[2..])
	if value_type.len == 0 {
		return none
	}
	return ArrayIndexInfo{
		base_id:       base_id
		index_id:      index_expr_id
		base_type:     base_type
		value_type:    value_type
		optional_type: optional_type
		base_optional: base_optional
	}
}

fn (mut t Transformer) array_index_len_expr(info ArrayIndexInfo, base_expr flat.NodeId) flat.NodeId {
	if info.is_fixed_array {
		return t.make_fixed_array_len_expr(info.base_type)
	}
	return t.make_selector(base_expr, 'len', 'int')
}

// is_array_index_or_expr reports whether is array index or expr applies in transform.
fn (mut t Transformer) is_array_index_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	_ := t.array_index_info(t.a.child(&node, 0)) or { return false }
	return true
}

fn (mut t Transformer) string_slice_info(index_id flat.NodeId) ?StringSliceInfo {
	if int(index_id) < 0 {
		return none
	}
	expr := t.a.nodes[int(index_id)]
	if expr.kind != .index || expr.children_count < 2 || expr.value != 'range' {
		return none
	}
	base_id := t.a.child(&expr, 0)
	base_type := t.resolve_expr_type(base_id)
	if t.normalize_type_alias(base_type) != 'string' {
		return none
	}
	return StringSliceInfo{
		base_id:  base_id
		start_id: t.a.child(&expr, 1)
		end_id:   if expr.children_count > 2 { t.a.child(&expr, 2) } else { flat.empty_node }
		has_end:  expr.children_count > 2
	}
}

fn (mut t Transformer) is_string_slice_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	_ := t.string_slice_info(t.a.child(&node, 0)) or { return false }
	return true
}

fn (mut t Transformer) channel_receive_info(expr_id flat.NodeId) ?ChannelReceiveInfo {
	if int(expr_id) < 0 {
		return none
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind in [.paren, .expr_stmt] && expr.children_count > 0 {
		return t.channel_receive_info(t.a.child(&expr, 0))
	}
	if expr.kind != .prefix || expr.op != .arrow || expr.children_count == 0 {
		return none
	}
	channel_id := t.a.child(&expr, 0)
	source_needs_deref := t.channel_receive_source_needs_deref(channel_id)
	if !isnil(t.tc) {
		raw_type := t.tc.resolve_type(channel_id)
		clean_type := types.unwrap_pointer(raw_type)
		if clean_type is types.Channel {
			value_type := t.normalize_type_alias(clean_type.elem_type.name())
			if value_type.len > 0 {
				return ChannelReceiveInfo{
					channel_id:  channel_id
					value_type:  value_type
					needs_deref: raw_type is types.Pointer
				}
			}
		}
		if recv_type := t.tc.expr_type(expr_id) {
			value_type := t.normalize_type_alias(recv_type.name())
			if value_type.len > 0 && value_type !in ['void', 'unknown'] {
				return ChannelReceiveInfo{
					channel_id:  channel_id
					value_type:  value_type
					needs_deref: source_needs_deref
				}
			}
		}
	}
	if expr.typ.len > 0 {
		value_type := t.normalize_type_alias(expr.typ)
		if value_type.len > 0 && value_type !in ['void', 'unknown'] {
			return ChannelReceiveInfo{
				channel_id:  channel_id
				value_type:  value_type
				needs_deref: source_needs_deref
			}
		}
	}
	mut channel_type := t.resolve_expr_type(channel_id)
	if channel_type.len == 0 {
		channel_type = t.node_type(channel_id)
	}
	channel_type = t.normalize_type_alias(channel_type).trim_space()
	fallback_needs_deref := channel_type.starts_with('&')
	for channel_type.starts_with('&') {
		channel_type = channel_type[1..].trim_space()
	}
	if !channel_type.starts_with('chan ') {
		return none
	}
	value_type := t.normalize_type_alias(channel_type[5..].trim_space())
	if value_type.len == 0 {
		return none
	}
	return ChannelReceiveInfo{
		channel_id:  channel_id
		value_type:  value_type
		needs_deref: fallback_needs_deref
	}
}

fn (mut t Transformer) channel_receive_source_needs_deref(channel_id flat.NodeId) bool {
	if int(channel_id) < 0 {
		return false
	}
	if !isnil(t.tc) {
		raw_type := t.tc.resolve_type(channel_id)
		clean_type := types.unwrap_pointer(raw_type)
		if raw_type is types.Pointer && clean_type is types.Channel {
			return true
		}
	}
	mut source_type := t.resolve_expr_type(channel_id)
	if source_type.len == 0 {
		source_type = t.node_type(channel_id)
	}
	source_type = t.normalize_type_alias(source_type).trim_space()
	return source_type.starts_with('&')
}

fn (mut t Transformer) is_channel_receive_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	source_id := t.a.child(&node, 0)
	_ := t.channel_receive_info(source_id) or { return false }
	return true
}

fn (mut t Transformer) transform_channel_receive_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	info := t.channel_receive_info(expr_id) or { return id }
	val_name := t.new_temp('chan_val')
	ok_name := t.new_temp('chan_ok')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	channel_expr := t.transform_expr(info.channel_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.value_type),
		info.value_type)
	channel_name := t.new_temp('chan_src')
	mut channel_source := channel_expr
	if info.needs_deref {
		channel_source = t.make_prefix(.mul, channel_source)
	}
	channel_cast := t.make_cast('&sync.Channel', channel_source, '&sync.Channel')
	prelude << t.make_decl_assign_typed(channel_name, channel_cast, '&sync.Channel')
	pop_call := t.make_call_typed('sync__Channel__pop', arr2(t.make_ident(channel_name), t.make_prefix(.amp,
		t.make_ident(val_name))), 'bool')
	prelude << t.make_decl_assign_typed(ok_name, pop_call, 'bool')
	t.mark_fn_used('sync__Channel__closed_error')
	err_expr := t.make_call_typed('sync__Channel__closed_error', arr1(t.make_ident(channel_name)),
		'IError')
	else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
		info.value_type, node.value, err_expr))
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << t.make_if(t.make_prefix(.not, t.make_ident(ok_name)), else_block,
		t.make_empty())
	return t.make_ident(val_name)
}

fn (mut t Transformer) transform_string_slice_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	info := t.string_slice_info(expr_id) or { return id }
	base_expr := t.stable_expr_for_reuse(info.base_id)
	start_name := t.new_temp('str_start')
	end_name := t.new_temp('str_end')
	val_name := t.new_temp('str_slice')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	start_expr := if int(info.start_id) >= 0 && t.a.nodes[int(info.start_id)].kind != .empty {
		t.transform_expr(info.start_id)
	} else {
		t.make_int_literal(0)
	}
	end_expr := if info.has_end {
		t.transform_expr(info.end_id)
	} else {
		t.make_selector(base_expr, 'len', 'int')
	}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(start_name, start_expr, 'int')
	prelude << t.make_decl_assign_typed(end_name, end_expr, 'int')
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type('string'), 'string')
	start_ident := t.make_ident(start_name)
	lower_ok := t.make_infix(.ge, start_ident, t.make_int_literal(0))
	ordered := t.make_infix(.le, t.make_ident(start_name), t.make_ident(end_name))
	upper_ok := t.make_infix(.le, t.make_ident(end_name), t.make_selector(base_expr, 'len', 'int'))
	bounds_ok := t.make_infix(.logical_and, t.make_infix(.logical_and, lower_ok, ordered), upper_ok)
	slice_call := t.make_call_typed('string__substr', arr3(base_expr, t.make_ident(start_name),
		t.make_ident(end_name)), 'string')
	then_block := t.make_block(arr1(t.make_assign(t.make_ident(val_name), slice_call)))
	else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name, 'string',
		node.value, t.make_ierror_none()))
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << t.make_if(bounds_ok, then_block, else_block)
	return t.make_ident(val_name)
}

// transform_array_index_or_expr transforms transform array index or expr data for transform.
fn (mut t Transformer) transform_array_index_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	info := t.array_index_info(expr_id) or { return id }
	base_expr0 := t.stable_expr_for_reuse(info.base_id)
	mut base_expr := base_expr0
	index_name := t.new_temp('arr_idx')
	val_name := t.new_temp('arr_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut base_opt_name := ''
	if info.base_optional {
		base_opt_name = t.new_temp('arr_base_opt')
		base_expr = t.make_selector(t.make_ident(base_opt_name), 'value', info.base_type)
	}
	index_expr := t.transform_expr(info.index_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	mut result_type := info.value_type
	mut wrap_found_value := false
	source_is_optional := t.is_optional_type_name(info.value_type)
	mut source_value_type := info.value_type
	if source_is_optional {
		source_value_type = t.optional_base_type(t.qualify_optional_type(info.value_type))
		result_type = source_value_type
	}
	if t.is_optional_type_name(node.typ) {
		optional_target := t.qualify_optional_type(node.typ)
		body_type := t.stmt_value_type(body_id)
		body_keeps_optional := t.or_body_is_none(body_id) || t.is_optional_type_name(body_type)
		if (!source_is_optional || body_keeps_optional)
			&& t.normalize_type_alias(t.optional_base_type(optional_target)) == t.normalize_type_alias(source_value_type) {
			result_type = optional_target
			wrap_found_value = true
		}
	}
	if info.base_optional {
		// The gated negative-index wrap reads `base_opt.value.len`, so the
		// optional base must exist before that wrap is evaluated.
		prelude << t.make_decl_assign_typed(base_opt_name, base_expr0, info.optional_type)
	}
	prelude << t.make_decl_assign_typed(index_name, index_expr, 'int')
	if t.a.nodes[int(expr_id)].op == .gated_index {
		// `base#[i] or {}`: a negative index counts from the end before the
		// bounds check decides between the value and the or-branch.
		neg_cond := t.make_infix(.lt, t.make_ident(index_name), t.make_int_literal(0))
		wrap_assign := t.make_assign(t.make_ident(index_name), t.make_infix(.plus,
			t.make_ident(index_name), t.array_index_len_expr(info, base_expr)))
		prelude << t.make_if(neg_cond, t.make_block(arr1(wrap_assign)), t.make_empty())
	}
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(result_type), result_type)

	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name), t.array_index_len_expr(info, base_expr))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)
	else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
		result_type, node.value, t.make_ierror_none()))
	index_value0 := t.make_index(base_expr, t.make_ident(index_name), info.value_type)
	mut then_block := flat.empty_node
	if source_is_optional {
		opt_name := t.new_temp('arr_opt')
		opt_decl := t.make_decl_assign_typed(opt_name, index_value0, info.value_type)
		opt_value := t.make_selector(t.make_ident(opt_name), 'value', source_value_type)
		index_value := if wrap_found_value {
			t.make_optional_some(opt_value, result_type)
		} else {
			opt_value
		}
		assign_found := t.make_assign(t.make_ident(val_name), index_value)
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', 'bool')
		opt_err_expr := t.make_selector(t.make_ident(opt_name), 'err', 'IError')
		opt_else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
			result_type, node.value, opt_err_expr))
		then_block = t.make_block([opt_decl,
			t.make_if(ok_cond, t.make_block(arr1(assign_found)), opt_else_block)])
	} else {
		index_value := if wrap_found_value {
			t.make_optional_some(index_value0, result_type)
		} else {
			index_value0
		}
		then_block = t.make_block(arr1(t.make_assign(t.make_ident(val_name), index_value)))
	}
	mut opt_else := flat.empty_node
	if info.base_optional {
		opt_err := t.make_selector(t.make_ident(base_opt_name), 'err', 'IError')
		opt_else = t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
			result_type, node.value, opt_err))
	}
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	if info.base_optional {
		opt_ok := t.make_selector(t.make_ident(base_opt_name), 'ok', 'bool')
		t.pending_stmts << t.make_if(opt_ok, t.make_block(arr1(t.make_if(found_cond, then_block,
			else_block))), opt_else)
	} else {
		t.pending_stmts << t.make_if(found_cond, then_block, else_block)
	}
	return t.make_ident(val_name)
}

// or_body_is_nil supports or body is nil handling for Transformer.
fn (t &Transformer) or_body_is_nil(body_id flat.NodeId) bool {
	if int(body_id) < 0 {
		return false
	}
	body := t.a.nodes[int(body_id)]
	if body.kind == .nil_literal {
		return true
	}
	if body.kind != .block || body.children_count != 1 {
		return false
	}
	stmt_id := t.a.child(&body, 0)
	stmt := t.a.nodes[int(stmt_id)]
	if stmt.kind == .expr_stmt && stmt.children_count == 1 {
		return t.a.nodes[int(t.a.child(&stmt, 0))].kind == .nil_literal
	}
	return stmt.kind == .nil_literal
}

fn (t &Transformer) or_body_is_none(body_id flat.NodeId) bool {
	if int(body_id) < 0 {
		return false
	}
	body := t.a.nodes[int(body_id)]
	if body.kind == .none_expr {
		return true
	}
	if body.kind != .block || body.children_count != 1 {
		return false
	}
	stmt_id := t.a.child(&body, 0)
	stmt := t.a.nodes[int(stmt_id)]
	if stmt.kind == .expr_stmt && stmt.children_count == 1 {
		return t.a.nodes[int(t.a.child(&stmt, 0))].kind == .none_expr
	}
	return stmt.kind == .none_expr
}

// enum_from_string_info converts enum from string info data for transform.
fn (t &Transformer) enum_from_string_info(expr_id flat.NodeId) ?EnumFromStringInfo {
	if int(expr_id) < 0 {
		return none
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind != .call || expr.children_count < 2 {
		return none
	}
	fn_id := t.a.child(&expr, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'from_string' || fn_node.children_count == 0 {
		return none
	}
	if t.enum_from_string_call_uses_user_method(expr_id) {
		return none
	}
	enum_id := t.a.child(&fn_node, 0)
	enum_type := t.enum_type_from_node(enum_id) or { return none }
	fields := t.enum_types[enum_type] or { return none }
	if fields.len == 0 {
		return none
	}
	return EnumFromStringInfo{
		enum_type: enum_type
		fields:    fields.clone()
		arg_id:    t.a.child(&expr, 1)
	}
}

fn (t &Transformer) enum_from_string_call_uses_user_method(expr_id flat.NodeId) bool {
	if isnil(t.tc) {
		return false
	}
	resolved := t.tc.resolved_call_name(expr_id) or { return false }
	return resolved.len > 0 && t.is_known_fn_name(resolved)
}

// enum_type_from_node converts enum type from node data for transform.
fn (t &Transformer) enum_type_from_node(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value in t.enum_types {
			return node.value
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value}'
			if qname in t.enum_types {
				return qname
			}
		}
		for key, _ in t.enum_types {
			if key.all_after_last('.') == node.value {
				return key
			}
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident {
			qname := '${base.value}.${node.value}'
			if resolved := t.resolve_import_alias_pattern(qname) {
				if resolved in t.enum_types {
					return resolved
				}
			}
			if qname in t.enum_types {
				return qname
			}
		}
	}
	return none
}

// is_enum_from_string_or_expr reports whether is enum from string or expr applies in transform.
fn (mut t Transformer) is_enum_from_string_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	_ := t.enum_from_string_info(t.a.child(&node, 0)) or { return false }
	return true
}

// transform_enum_from_string_or_expr supports transform_enum_from_string_or_expr handling.
fn (mut t Transformer) transform_enum_from_string_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	info := t.enum_from_string_info(expr_id) or { return id }
	members := t.enum_from_string_members(info)
	if members.len == 0 {
		return id
	}
	str_name := t.new_temp('enum_str')
	val_name := t.new_temp('enum_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	arg_expr := t.transform_expr(info.arg_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(str_name, arg_expr, 'string')
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.enum_type),
		info.enum_type)

	mut else_block := t.make_block(t.lower_or_body_to_stmts(body_id, val_name, info.enum_type,
		node.value, ''))
	for i := members.len - 1; i >= 0; i-- {
		cond := t.make_call_typed('string__eq', arr2(t.make_ident(str_name),
			t.make_string_literal(members[i].name)), 'bool')
		assign := t.make_assign(t.make_ident(val_name), t.enum_from_string_member_value(info,
			members[i]))
		then_block := t.make_block(arr1(assign))
		else_block = t.make_if(cond, then_block, else_block)
	}
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << else_block
	return t.make_ident(val_name)
}

fn (mut t Transformer) try_lower_enum_from_string_call(call_id flat.NodeId, _node flat.Node) ?flat.NodeId {
	info := t.enum_from_string_info(call_id) or { return none }
	members := t.enum_from_string_members(info)
	if members.len == 0 {
		return none
	}
	str_name := t.new_temp('enum_str')
	val_name := t.new_temp('enum_val')
	ok_name := t.new_temp('enum_ok')
	optional_type := '?${info.enum_type}'
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	arg_expr := t.transform_expr(info.arg_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(str_name, arg_expr, 'string')
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.enum_type),
		info.enum_type)
	prelude << t.make_decl_assign_typed(ok_name, t.make_bool_literal(false), 'bool')

	mut else_block := t.make_block([]flat.NodeId{})
	for i := members.len - 1; i >= 0; i-- {
		cond := t.make_call_typed('string__eq', arr2(t.make_ident(str_name),
			t.make_string_literal(members[i].name)), 'bool')
		assign_val := t.make_assign(t.make_ident(val_name), t.enum_from_string_member_value(info,
			members[i]))
		assign_ok := t.make_assign(t.make_ident(ok_name), t.make_bool_literal(true))
		then_block := t.make_block(arr2(assign_val, assign_ok))
		else_block = t.make_if(cond, then_block, else_block)
	}
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << else_block

	ok_field := t.make_sum_literal_field('ok', t.make_ident(ok_name), 'bool')
	value_field := t.make_sum_literal_field('value', t.make_ident(val_name), info.enum_type)
	start := t.a.children.len
	t.a.children << ok_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          optional_type
		typ:            optional_type
	})
}

// or_expr_types supports or expr types handling for Transformer.
fn (mut t Transformer) or_expr_types(expr_id flat.NodeId, fallback_type string) (string, string) {
	expr_node := if int(expr_id) >= 0 && int(expr_id) < t.a.nodes.len {
		t.a.nodes[int(expr_id)]
	} else {
		flat.Node{}
	}
	if !isnil(t.tc) {
		if expr_node.kind == .call {
			if current_ret := t.current_generic_receiver_call_return_type(expr_node) {
				if t.is_optional_type_name(current_ret) {
					return t.canonical_or_expr_types(current_ret)
				}
			}
			if decode_ret := t.json_decode_or_expr_type(expr_id, expr_node) {
				return t.canonical_or_expr_types(decode_ret)
			}
			concrete_ret := t.concrete_generic_call_return_type(expr_id, expr_node)
			if t.is_optional_type_name(concrete_ret) {
				return t.canonical_or_expr_types(concrete_ret)
			}
			call_ret := t.get_call_return_type(expr_id, expr_node)
			if t.is_optional_type_name(call_ret) {
				return t.canonical_or_expr_types(call_ret)
			}
		}
		if typ := t.tc.expr_type(expr_id) {
			if typ is types.OptionType {
				base_name := t.value_type_name(typ.base_type)
				source_name := if typ.base_type is types.Void || base_name == 'Optional' {
					'?void'
				} else {
					'?${base_name}'
				}
				return t.canonical_or_expr_types(source_name)
			}
			if typ is types.ResultType {
				base_name := t.value_type_name(typ.base_type)
				source_name := if typ.base_type is types.Void || base_name == 'Optional' {
					'!void'
				} else {
					'!${base_name}'
				}
				return t.canonical_or_expr_types(source_name)
			}
		}
	}
	mut expr_type := t.node_type(expr_id)
	if expr_type == 'Optional' {
		return '?void', 'void'
	}
	if fallback_type == 'Optional' {
		return '?void', 'void'
	}
	if expr_type.len == 0 || expr_type == 'Optional' {
		if fallback_type.len > 0 && !t.is_optional_type_name(fallback_type) {
			expr_type = '?${fallback_type}'
		} else {
			expr_type = fallback_type
		}
	}
	if expr_type == '!' || expr_type == '?' {
		return '${expr_type}void', 'void'
	}
	base_type := t.optional_base_type(expr_type)
	if base_type == 'Optional' {
		return expr_type, 'void'
	}
	mut value_type := if base_type.len > 0 { base_type } else { fallback_type }
	if value_type.len == 0 || value_type == '!' || value_type == '?' {
		value_type = 'int'
	}
	canon_expr_type, canon_value_type := t.canonical_or_expr_types(expr_type)
	if t.is_optional_type_name(canon_expr_type) {
		return canon_expr_type, canon_value_type
	}
	return expr_type, value_type
}

fn (t &Transformer) canonical_or_expr_types(expr_type string) (string, string) {
	if !t.is_optional_type_name(expr_type) {
		return expr_type, t.optional_base_type(expr_type)
	}
	prefix := expr_type[..1]
	base := t.optional_base_type(expr_type)
	if base.len == 0 || base == 'void' || base == 'Optional' {
		return '${prefix}void', 'void'
	}
	return expr_type, base
}

fn (t &Transformer) json_decode_or_expr_type(expr_id flat.NodeId, expr_node flat.Node) ?string {
	if isnil(t.tc) || expr_node.kind != .call {
		return none
	}
	name := t.tc.resolved_call_name(expr_id) or { return none }
	if name !in ['json.decode', 'json2.decode', 'x.json2.decode'] {
		return none
	}
	if args := t.explicit_generic_call_args(expr_node, t.cur_module) {
		if args.len == 1 && args[0].len > 0 {
			return '!${args[0]}'
		}
		return none
	}
	if expr_node.children_count < 2 {
		return none
	}
	type_arg := t.generic_call_type_arg_name(t.a.child(&expr_node, 1))
	if type_arg.len == 0 {
		return none
	}
	return '!${type_arg}'
}

// value_type_name returns value type name data for Transformer.
fn (t &Transformer) value_type_name(typ types.Type) string {
	if typ is types.Void {
		return 'void'
	}
	return typ.name()
}

// is_optional_type_name reports whether is optional type name applies in transform.
fn (t &Transformer) is_optional_type_name(typ string) bool {
	return typ.len > 0 && (typ[0] == `?` || typ[0] == `!`)
}

// qualify_optional_type supports qualify optional type handling for Transformer.
fn (t &Transformer) qualify_optional_type(typ string) string {
	if typ.len < 2 || (typ[0] != `?` && typ[0] != `!`) {
		return typ
	}
	base := typ[1..]
	if base.contains('.') || base.len == 0 {
		return typ
	}
	qualified := t.qualify_type(base)
	if qualified != base {
		return typ[..1] + qualified
	}
	return typ
}

// qualify_type supports qualify type handling for Transformer.
fn (t &Transformer) qualify_type(name string) string {
	if name.contains('.') || name.len == 0 {
		return name
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.sum_types || qname in t.structs {
			return qname
		}
	}
	if qualified := t.qualified_types[name] {
		return qualified
	}
	return name
}

// make_decl_assign_typed builds make decl assign typed data for transform.
fn (mut t Transformer) make_decl_assign_typed(name string, rhs flat.NodeId, typ string) flat.NodeId {
	decl := t.make_decl_assign(name, rhs)
	if typ.len > 0 {
		t.set_node_typ(int(decl), typ)
		decl_node := t.a.nodes[int(decl)]
		if decl_node.children_count > 0 {
			lhs_id := t.a.child(&decl_node, 0)
			if int(lhs_id) >= 0 {
				t.set_node_typ(int(lhs_id), typ)
			}
		}
		t.set_var_type(name, typ)
	}
	return decl
}

// zero_value_for_type supports zero value for type handling for Transformer.
fn (mut t Transformer) zero_value_for_type(typ string) flat.NodeId {
	mut clean := typ
	if clean.starts_with('&') {
		return t.a.add(.nil_literal)
	}
	if clean.len > 1 && (clean[0] == `?` || clean[0] == `!`) {
		return t.make_optional_none(clean)
	}
	clean = t.normalize_type_alias(clean)
	if clean.starts_with('fn(') || clean.starts_with('fn (') || clean.starts_with('fn_ptr:') {
		return t.make_cast(clean, t.make_int_literal(0), clean)
	}
	if clean.starts_with('[]') {
		return t.make_array_init(clean[2..])
	}
	if clean.starts_with('map[') {
		return t.make_map_init(clean)
	}
	if t.is_fixed_array_type(clean) {
		fixed_type := fixed_array_canonical_type(clean)
		len_text := fixed_array_len_text(fixed_type)
		if is_decimal_text(len_text) {
			elem_type := fixed_array_elem_type(fixed_type)
			mut values := []flat.NodeId{cap: len_text.int()}
			for _ in 0 .. len_text.int() {
				values << t.zero_value_for_type(elem_type)
			}
			return t.make_array_literal_typed(values, fixed_type)
		}
	}
	if clean == 'string' {
		return t.make_string_literal('')
	}
	if clean == 'bool' {
		return t.make_bool_literal(false)
	}
	if clean in ['f32', 'f64'] {
		return t.make_float_literal('0.0')
	}
	if clean in ['void', ''] {
		return t.make_int_literal(0)
	}
	if clean in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte', 'u16', 'u32', 'u64', 'rune', 'char']
		|| clean in t.enum_types {
		return t.make_int_literal(0)
	}
	if default_sum := t.make_default_sum_value(clean) {
		return default_sum
	}
	return t.make_struct_init(clean)
}

// make_panic_stmt builds make panic stmt data for transform.
fn (mut t Transformer) make_panic_stmt(message string) flat.NodeId {
	call := t.make_call('panic', arr1(t.make_string_literal(message)))
	return t.make_expr_stmt(call)
}

// make_none_return_stmt builds make none return stmt data for transform.
fn (mut t Transformer) make_none_return_stmt() flat.NodeId {
	return t.make_return(t.a.add(.none_expr), t.cur_fn_ret_type)
}

// make_none_return_stmt_with_err builds make none return stmt with err data for transform.
fn (mut t Transformer) make_none_return_stmt_with_err(err_source string) flat.NodeId {
	if err_source.len == 0 {
		return t.make_none_return_stmt()
	}
	err_expr := t.make_selector(t.make_ident(err_source), 'err', 'IError')
	return t.make_none_return_stmt_with_err_expr(err_expr)
}

fn (mut t Transformer) make_none_return_stmt_with_err_expr(err_expr flat.NodeId) flat.NodeId {
	if int(err_expr) < 0 {
		return t.make_none_return_stmt()
	}
	return t.make_return(t.make_optional_none_with_err(t.cur_fn_ret_type, err_expr),
		t.cur_fn_ret_type)
}

// lower_or_expr_to_temp converts lower or expr to temp data for transform.
fn (mut t Transformer) lower_or_expr_to_temp(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	expr_node := if int(expr_id) >= 0 && int(expr_id) < t.a.nodes.len {
		t.a.nodes[int(expr_id)]
	} else {
		flat.Node{}
	}
	expr_type, value_type := t.or_expr_types(expr_id, node.typ)
	if !t.is_optional_type_name(expr_type) {
		return t.transform_expr(expr_id)
	}
	is_void := value_type.len == 0 || value_type == 'void'
		|| value_type == 'Optional' || value_type == '!' || value_type == '?'
		|| (t.is_optional_type_name(value_type) && t.optional_base_type(value_type) == 'void')
	opt_tmp := t.new_temp('or_opt')
	val_tmp := t.new_temp('or_val')

	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_expr := t.disabled_optional_call_or_none(expr_id, expr_node, expr_type) or {
		t.transform_expr(expr_id)
	}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	new_expr_type := t.node_type(new_expr)
	if !t.is_optional_type_name(new_expr_type) {
		t.pending_stmts = outer_pending
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		return new_expr
	}

	if is_void {
		prelude << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type)
		opt_ident := t.make_ident(opt_tmp)
		not_ok := t.make_prefix(.not, t.make_selector(opt_ident, 'ok', 'bool'))
		else_block := t.make_block(t.lower_or_body_to_stmts(body_id, '', '', node.value, opt_tmp))
		if_stmt := t.make_if(not_ok, else_block, t.make_empty())
		t.pending_stmts = outer_pending
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		t.pending_stmts << if_stmt
		return t.make_int_literal(0)
	}

	prelude << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type)
	prelude << t.make_decl_assign_typed(val_tmp, t.zero_value_for_type(value_type), value_type)

	opt_ident := t.make_ident(opt_tmp)
	ok_cond := t.make_selector(opt_ident, 'ok', 'bool')
	value_expr := t.make_selector(t.make_ident(opt_tmp), 'value', value_type)
	then_assign := t.make_assign(t.make_ident(val_tmp), value_expr)
	then_block := t.make_block(arr1(then_assign))
	else_stmts := if multi_types := t.multi_return_types_for_expr(expr_id, 0) {
		t.lower_or_body_to_multi_return_stmts(body_id, val_tmp, value_type, multi_types,
			node.value, opt_tmp)
	} else {
		t.lower_or_body_to_stmts(body_id, val_tmp, value_type, node.value, opt_tmp)
	}
	else_block := t.make_block(else_stmts)
	if_stmt := t.make_if(ok_cond, then_block, else_block)
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << if_stmt
	return t.make_ident(val_tmp)
}

fn (mut t Transformer) disabled_optional_call_or_none(expr_id flat.NodeId, expr_node flat.Node, expr_type string) ?flat.NodeId {
	if isnil(t.tc) || expr_node.kind != .call || !t.is_optional_type_name(expr_type) {
		return none
	}
	if t.is_cgen_magic_json_call(expr_id, expr_node) {
		return none
	}
	if _ := t.json_decode_or_expr_type(expr_id, expr_node) {
		if name := t.tc.resolved_call_name(expr_id) {
			if name in ['json.decode', 'json2.decode', 'x.json2.decode'] {
				return none
			}
		}
		return t.make_optional_none(expr_type)
	}
	if !t.is_disabled_fn_call(expr_id, expr_node) {
		return none
	}
	return t.make_optional_none(expr_type)
}

fn (mut t Transformer) lower_or_expr_to_stmt(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	expr_type, _ := t.or_expr_types(expr_id, node.typ)
	if !t.is_optional_type_name(expr_type) {
		t.pending_stmts << t.make_expr_stmt(t.transform_expr(expr_id))
		return
	}

	opt_tmp := t.new_temp('or_opt')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_expr := t.transform_expr(expr_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type)

	opt_ident := t.make_ident(opt_tmp)
	not_ok := t.make_prefix(.not, t.make_selector(opt_ident, 'ok', 'bool'))
	else_block := t.make_block(t.lower_or_body_to_stmts(body_id, '', '', node.value, opt_tmp))
	if_stmt := t.make_if(not_ok, else_block, t.make_empty())

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << if_stmt
}

// lower_or_body_to_stmts converts lower or body to stmts data for transform.
fn (mut t Transformer) lower_or_body_to_stmts(body_id flat.NodeId, target_name string, target_type string, mode string, err_source string) []flat.NodeId {
	err_expr := if err_source.len > 0 {
		t.make_selector(t.make_ident(err_source), 'err', 'IError')
	} else {
		flat.empty_node
	}
	return t.lower_or_body_to_stmts_with_err_expr(body_id, target_name, target_type, mode, err_expr)
}

fn (mut t Transformer) lower_or_body_to_multi_return_stmts(body_id flat.NodeId, target_name string, target_type string, field_types []types.Type, mode string, err_source string) []flat.NodeId {
	err_expr := if err_source.len > 0 {
		t.make_selector(t.make_ident(err_source), 'err', 'IError')
	} else {
		flat.empty_node
	}
	return t.lower_or_body_to_multi_return_stmts_with_err_expr(body_id, target_name, target_type,
		field_types, mode, err_expr)
}

fn (mut t Transformer) lower_or_body_to_multi_return_stmts_with_err_expr(body_id flat.NodeId, target_name string, target_type string, field_types []types.Type, mode string, err_expr flat.NodeId) []flat.NodeId {
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return arr1(t.make_none_return_stmt_with_err_expr(err_expr))
		}
		return arr1(t.make_panic_stmt('option/result propagation failed'))
	}
	if int(body_id) < 0 || target_name.len == 0 || field_types.len == 0 {
		return t.lower_or_body_to_stmts_with_err_expr(body_id, target_name, target_type, mode,
			err_expr)
	}
	body := t.a.nodes[int(body_id)]
	saved_var_types := t.var_types.clone()
	t.set_var_type('err', 'IError')
	err_value := if int(err_expr) >= 0 {
		err_expr
	} else {
		t.make_struct_init('IError')
	}
	mut result := []flat.NodeId{}
	result << t.make_decl_assign_typed('err', err_value, 'IError')
	if lowered := t.lower_or_multi_return_tail(body_id, target_name, target_type, field_types) {
		for stmt in lowered {
			result << stmt
		}
		t.restore_var_types(saved_var_types)
		return result
	}
	_ = body
	t.restore_var_types(saved_var_types)
	return t.lower_or_body_to_stmts_with_err_expr(body_id, target_name, target_type, mode, err_expr)
}

fn (t &Transformer) or_tail_can_supply_multi_return(id flat.NodeId, expected_count int) bool {
	if int(id) < 0 || expected_count <= 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.block {
			if node.children_count == 0 {
				return false
			}
			last_id := t.a.child(&node, node.children_count - 1)
			if t.or_tail_can_supply_multi_return(last_id, expected_count) {
				return true
			}
			if _ := t.block_trailing_multi_return_values(node, expected_count) {
				return true
			}
			return false
		}
		.expr_stmt {
			if node.children_count == expected_count {
				return true
			}
			if node.children_count == 1 {
				child_id := t.a.child(&node, 0)
				child := t.a.nodes[int(child_id)]
				if child.kind == .block {
					return t.or_tail_can_supply_multi_return(child_id, expected_count)
				}
				if _ := t.multi_return_types_for_expr(child_id, expected_count) {
					return true
				}
			}
			return false
		}
		else {
			if t.is_stmt_kind(node.kind) {
				return false
			}
			if _ := t.multi_return_types_for_expr(id, expected_count) {
				return true
			}
			return false
		}
	}
}

fn (t &Transformer) block_trailing_multi_return_values(node flat.Node, expected_count int) ?MultiReturnTailValues {
	if node.kind != .block || expected_count <= 0 || node.children_count == 0 {
		return none
	}
	mut values := []flat.NodeId{cap: expected_count}
	mut start_index := int(node.children_count)
	for i := int(node.children_count) - 1; i >= 0; i-- {
		stmt_id := t.a.child(&node, i)
		stmt := t.a.nodes[int(stmt_id)]
		if stmt.kind != .expr_stmt || stmt.children_count == 0 {
			break
		}
		for j := int(stmt.children_count) - 1; j >= 0; j-- {
			values.prepend(t.a.child(&stmt, j))
			if values.len == expected_count {
				start_index = i
				break
			}
		}
		if values.len == expected_count {
			break
		}
	}
	if values.len != expected_count {
		return none
	}
	return MultiReturnTailValues{
		values:      values
		start_index: start_index
	}
}

fn (mut t Transformer) lower_or_multi_return_tail(id flat.NodeId, target_name string, target_type string, field_types []types.Type) ?[]flat.NodeId {
	if int(id) < 0 || field_types.len == 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.block {
			if node.children_count == 0 {
				return none
			}
			last_id := t.a.child(&node, node.children_count - 1)
			if t.or_tail_can_supply_multi_return(last_id, field_types.len) {
				mut result := []flat.NodeId{}
				for i in 0 .. node.children_count - 1 {
					child_id := t.a.child(&node, i)
					expanded := t.transform_stmt(child_id)
					t.drain_pending(mut result)
					for stmt in expanded {
						result << stmt
					}
				}
				tail_stmts := t.lower_or_multi_return_tail(last_id, target_name, target_type,
					field_types) or { return none }
				for stmt in tail_stmts {
					result << stmt
				}
				return result
			}
			if tail := t.block_trailing_multi_return_values(node, field_types.len) {
				mut result := []flat.NodeId{}
				for i in 0 .. tail.start_index {
					child_id := t.a.child(&node, i)
					expanded := t.transform_stmt(child_id)
					t.drain_pending(mut result)
					for stmt in expanded {
						result << stmt
					}
				}
				for stmt in t.lower_or_multi_return_values(tail.values, target_name, field_types) {
					result << stmt
				}
				return result
			}
			return none
		}
		.expr_stmt {
			if node.children_count == field_types.len {
				mut values := []flat.NodeId{cap: int(node.children_count)}
				for i in 0 .. node.children_count {
					values << t.a.child(&node, i)
				}
				return t.lower_or_multi_return_values(values, target_name, field_types)
			}
			if node.children_count == 1 {
				child_id := t.a.child(&node, 0)
				child := t.a.nodes[int(child_id)]
				if child.kind == .block {
					return t.lower_or_multi_return_tail(child_id, target_name, target_type,
						field_types)
				}
				if _ := t.multi_return_types_for_expr(child_id, field_types.len) {
					return t.lower_or_multi_return_expr(child_id, target_name, target_type)
				}
			}
			return none
		}
		else {
			if t.is_stmt_kind(node.kind) {
				return none
			}
			if _ := t.multi_return_types_for_expr(id, field_types.len) {
				return t.lower_or_multi_return_expr(id, target_name, target_type)
			}
			return none
		}
	}
}

fn (mut t Transformer) lower_or_multi_return_values(values []flat.NodeId, target_name string, field_types []types.Type) []flat.NodeId {
	mut result := []flat.NodeId{}
	for i, value_id in values {
		if i >= field_types.len {
			break
		}
		field_type := field_types[i].name()
		value := if field_type in t.sum_types || t.resolve_sum_name(field_type) in t.sum_types {
			t.wrap_sum_value(value_id, field_type)
		} else {
			t.transform_expr_for_type(value_id, field_type)
		}
		t.drain_pending(mut result)
		field := t.make_selector(t.make_ident(target_name), 'arg${i}', field_type)
		result << t.make_assign(field, value)
	}
	return result
}

fn (mut t Transformer) lower_or_multi_return_expr(expr_id flat.NodeId, target_name string, target_type string) []flat.NodeId {
	value := t.transform_expr_for_type(expr_id, target_type)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_assign(t.make_ident(target_name), value)
	return result
}

fn (mut t Transformer) lower_or_body_to_stmts_with_err_expr(body_id flat.NodeId, target_name string, target_type string, mode string, err_expr flat.NodeId) []flat.NodeId {
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return arr1(t.make_none_return_stmt_with_err_expr(err_expr))
		}
		return arr1(t.make_panic_stmt('option/result propagation failed'))
	}
	if int(body_id) < 0 {
		return []flat.NodeId{}
	}
	body := t.a.nodes[int(body_id)]
	if body.kind != .block {
		if body.kind == .none_expr && t.is_optional_type_name(t.cur_fn_ret_type) {
			return arr1(t.make_none_return_stmt())
		}
		if target_name.len == 0 {
			value := t.transform_expr(body_id)
			mut result := []flat.NodeId{}
			t.drain_pending(mut result)
			if t.node_type(body_id) != 'void' {
				result << t.make_expr_stmt(value)
			}
			return result
		}
		return arr1(t.make_assign(t.make_ident(target_name), t.transform_expr(body_id)))
	}
	mut result := []flat.NodeId{}
	if body.children_count == 0 {
		return result
	}
	// The or-body is its own scope: `err` (and any locals it declares) are bound only
	// for its lowering and restored afterwards, so the previous binding (e.g. an outer
	// `err := 1`) survives and a subsequent `${err}` is not mis-typed as `IError`.
	// Mirrors transform_if_guard_else_block.
	saved_var_types := t.var_types.clone()
	t.set_var_type('err', 'IError')
	err_value := if int(err_expr) >= 0 {
		err_expr
	} else {
		t.make_struct_init('IError')
	}
	result << t.make_decl_assign_typed('err', err_value, 'IError')
	for i in 0 .. body.children_count {
		child_id := t.a.child(&body, i)
		child := t.a.nodes[int(child_id)]
		is_last := i == body.children_count - 1
		if is_last && child.kind == .expr_stmt && child.children_count > 0 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .call && t.is_noreturn_call(inner_id) {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else if inner.kind == .call && t.is_error_call(inner)
				&& t.is_optional_type_name(t.cur_fn_ret_type) {
				result << t.make_return(inner_id, t.cur_fn_ret_type)
			} else if inner.kind == .none_expr && t.is_optional_type_name(t.cur_fn_ret_type) {
				result << t.make_none_return_stmt()
			} else if t.node_type(inner_id) == 'void' && target_name.len == 0 {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else if target_name.len == 0 {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else {
				value := if target_type in t.sum_types
					|| t.resolve_sum_name(target_type) in t.sum_types {
					t.wrap_sum_value(inner_id, target_type)
				} else {
					t.transform_expr_for_type(inner_id, target_type)
				}
				t.drain_pending(mut result)
				result << t.make_assign(t.make_ident(target_name), value)
			}
		} else if is_last && target_name.len > 0 && child.kind in [.if_expr, .match_stmt] {
			value := if target_type in t.sum_types || t.resolve_sum_name(target_type) in t.sum_types {
				t.wrap_sum_value(child_id, target_type)
			} else {
				t.transform_expr_for_type(child_id, target_type)
			}
			t.drain_pending(mut result)
			result << t.make_assign(t.make_ident(target_name), value)
		} else if is_last && target_name.len > 0 && !t.is_stmt_kind(child.kind) {
			value := if target_type in t.sum_types || t.resolve_sum_name(target_type) in t.sum_types {
				t.wrap_sum_value(child_id, target_type)
			} else {
				t.transform_expr_for_type(child_id, target_type)
			}
			t.drain_pending(mut result)
			result << t.make_assign(t.make_ident(target_name), value)
		} else {
			expanded := t.transform_stmt(child_id)
			t.drain_pending(mut result)
			for eid in expanded {
				result << eid
			}
		}
	}
	_ = target_type
	t.restore_var_types(saved_var_types)
	return result
}

fn (t &Transformer) is_error_call(node flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.kind == .ident
		&& (fn_node.value == 'error' || fn_node.value == 'error_with_code')
}

// is_noreturn_call reports whether is noreturn call applies in transform.
fn (t &Transformer) is_noreturn_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	return t.tc.resolved_call_never_returns(id)
}
