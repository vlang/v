module transform

import v.flat
import v.types

// ArrayIndexInfo stores array index info metadata used by transform.
struct ArrayIndexInfo {
	base_id        flat.NodeId
	index_id       flat.NodeId
	base_type      u32
	value_type     u32
	optional_type  u32
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
	value_type  u32
	needs_deref bool
}

struct MultiReturnTailValues {
	values      []flat.NodeId
	start_index int
}

struct NestedOptionalPart {
	expr    flat.NodeId
	stmts   []flat.NodeId
	changed bool
}

// EnumFromStringInfo stores enum from string info metadata used by transform.
struct EnumFromStringInfo {
	enum_type    u32
	fields       []string
	arg_id       flat.NodeId
	accept_empty bool
	is_result    bool
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
			value: '${info.enum_type_text()}.${member.name}'
			typ:   info.enum_type
		})
	}
	return t.make_int_literal_typed(member.value.str(), info.enum_type)
}

// optional_base_type returns the payload type, including void for a bare ? or !.
fn (t &Transformer) optional_base_type(typ_id u32) u32 {
	if !flat.source_type_shape(typ_id).has(.optional) {
		return typ_id
	}
	if !isnil(t.alias_cache) {
		if base := t.alias_cache.optional_bases[typ_id] {
			return base
		}
	}
	typ := flat.source_type_name(typ_id)
	base := if typ.len == 1 { flat.source_type_void } else { flat.source_type_id(typ[1..]) }
	if !isnil(t.alias_cache) {
		mut cache := t.alias_cache
		cache.optional_bases[typ_id] = base
	}
	return base
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
	return t.array_index_info_from_parts(base_id, index_expr_id)
}

fn (mut t Transformer) array_get_call_info(call_id flat.NodeId) ?ArrayIndexInfo {
	if int(call_id) < 0 {
		return none
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 2 {
		return none
	}
	fn_node := t.a.child_node(&call, 0)
	if fn_node.kind != .selector || fn_node.value != 'get' || fn_node.children_count == 0 {
		return none
	}
	if _ := t.checker_selected_receiver_method_name(call_id, 'array.get') {
		return none
	}
	return t.array_index_info_from_parts(t.a.child(fn_node, 0), t.a.child(&call, 1))
}

fn (mut t Transformer) array_index_info_from_parts(base_id flat.NodeId, index_expr_id flat.NodeId) ?ArrayIndexInfo {
	mut base_type := t.resolve_expr_type(base_id)
	if base_type == 0 || base_type in [flat.source_type_void, flat.source_type_unknown] {
		base_type = t.array_literal_index_base_type(base_id)
	}
	literal_base_type_source_id := t.array_literal_index_base_type(base_id)
	literal_base_type_source_id_2 := literal_base_type_source_id

	if literal_base_type_source_id != 0 && !flat.source_type_shape(base_type).has(.array_type)
		&& t.normalize_type_alias(base_type) != flat.source_type_string && !t.is_fixed_array_type(base_type) {
		base_type = literal_base_type_source_id_2
	}
	base_type = t.trim_all_pointer_type(base_type)
	for flat.source_type_shape(base_type).has(.shared_type) {
		base_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(base_type)[7..]))
	}
	mut optional_type := u32(0)
	mut base_optional := false
	if t.is_optional_type_name(base_type) {
		optional_type = t.qualify_optional_type(base_type)
		inner_type_source_id := t.optional_base_type(optional_type)
		inner_type_shape := flat.source_type_shape(inner_type_source_id)
		if inner_type_shape.has(.array_type) {
			base_type = inner_type_source_id
			base_optional = true
		}
	}
	if t.normalize_type_alias(base_type) == flat.source_type_string {
		return ArrayIndexInfo{
			base_id:       base_id
			index_id:      index_expr_id
			base_type:     flat.source_type_string
			value_type:    flat.source_type_u8
			optional_type: optional_type
			base_optional: base_optional
		}
	}
	if t.is_fixed_array_type(base_type) {
		fixed_type_source_id := t.resolved_fixed_array_canonical_type(base_type)
		return ArrayIndexInfo{
			base_id:        base_id
			index_id:       index_expr_id
			base_type:      fixed_type_source_id
			value_type:     fixed_array_elem_type(fixed_type_source_id)
			optional_type:  optional_type
			base_optional:  base_optional
			is_fixed_array: true
		}
	}
	if !flat.source_type_shape(base_type).has(.array_type) {
		return none
	}
	value_type_source_id := t.normalize_type_alias(flat.source_type_id(flat.source_type_name(base_type)[2..]))
	if value_type_source_id == 0 {
		return none
	}
	return ArrayIndexInfo{
		base_id:       base_id
		index_id:      index_expr_id
		base_type:     base_type
		value_type:    value_type_source_id
		optional_type: optional_type
		base_optional: base_optional
	}
}

fn (mut t Transformer) array_literal_index_base_type(base_id flat.NodeId) u32 {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return u32(0)
	}
	base := t.a.nodes[int(base_id)]
	if base.kind != .array_literal {
		return u32(0)
	}
	elem_type := if base.children_count > 0 {
		mut child_type := t.node_type(t.a.child(&base, 0))
		if child_type == 0 || child_type in [flat.source_type_void, flat.source_type_unknown] {
			child_type = t.resolve_expr_type(t.a.child(&base, 0))
		}
		if child_type == 0 || child_type in [flat.source_type_void, flat.source_type_unknown] {
			'int'
		} else {
			flat.source_type_name(child_type)
		}
	} else {
		'int'
	}
	return flat.source_type_id('[]${elem_type}')
}

fn (mut t Transformer) array_index_len_expr(info ArrayIndexInfo, base_expr flat.NodeId) flat.NodeId {
	if info.is_fixed_array {
		return t.make_fixed_array_len_expr(info.base_type)
	}
	return t.make_selector(base_expr, 'len', flat.source_type_int)
}

// is_array_index_or_expr reports whether is array index or expr applies in transform.
fn (mut t Transformer) is_array_index_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	source_id := t.a.child(&node, 0)
	if _ := t.array_index_info(source_id) {
		return true
	}
	if _ := t.array_get_call_info(source_id) {
		return true
	}
	return false
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
	base_type_source_id := t.resolve_expr_type(base_id)

	if t.normalize_type_alias(base_type_source_id) != flat.source_type_string {
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
			value_type_source_id := t.normalize_type_alias(clean_type.elem_type.source_type_id())
			value_type_comparison_type_id_2 := value_type_source_id
			if value_type_source_id != 0 && value_type_comparison_type_id_2 !in [
				flat.source_type_void,
				flat.source_type_unknown,
			] {
				return ChannelReceiveInfo{
					channel_id:  channel_id
					value_type:  value_type_source_id
					needs_deref: raw_type is types.Pointer
				}
			}
		}
		if recv_type := t.tc.expr_type(expr_id) {
			value_type_source_id_2 := t.normalize_type_alias(recv_type.source_type_id())
			value_type_comparison_type_id_3 := value_type_source_id_2
			if value_type_source_id_2 != 0 && value_type_comparison_type_id_3 !in [
				flat.source_type_void,
				flat.source_type_unknown,
			] {
				return ChannelReceiveInfo{
					channel_id:  channel_id
					value_type:  value_type_source_id_2
					needs_deref: source_needs_deref
				}
			}
		}
	}
	if expr.typ != 0 {
		value_type_source_id_3 := t.normalize_type_alias(expr.typ)
		value_type_comparison_type_id_4 := value_type_source_id_3
		if value_type_source_id_3 != 0 && value_type_comparison_type_id_4 !in [
			flat.source_type_void,
			flat.source_type_unknown,
		] {
			return ChannelReceiveInfo{
				channel_id:  channel_id
				value_type:  value_type_source_id_3
				needs_deref: source_needs_deref
			}
		}
	}
	mut channel_type := t.resolve_expr_type(channel_id)
	if channel_type == 0 {
		channel_type = t.node_type(channel_id)
	}
	channel_type = t.trimmed_type_id(t.normalize_type_alias(channel_type))
	fallback_needs_deref := flat.source_type_shape(channel_type).has(.pointer)
	for flat.source_type_shape(channel_type).has(.pointer) {
		channel_type = t.trimmed_type_id(t.trim_pointer_type(channel_type))
	}
	if !flat.source_type_shape(channel_type).has(.channel_type) {
		if field_channel_type_source_id := t.channel_receive_selector_source_type(channel_id) {
			field_channel_type_source_id_2 := field_channel_type_source_id

			channel_type = field_channel_type_source_id_2
		}
	}
	if !flat.source_type_shape(channel_type).has(.channel_type) {
		return none
	}
	value_type_source_id_4 := t.normalize_type_alias(flat.source_type_id(trimmed_transform_text(flat.source_type_name(channel_type)[5..])))
	if value_type_source_id_4 == 0 {
		return none
	}
	return ChannelReceiveInfo{
		channel_id:  channel_id
		value_type:  value_type_source_id_4
		needs_deref: fallback_needs_deref
	}
}

fn (t &Transformer) channel_receive_selector_source_type(channel_id flat.NodeId) ?u32 {
	if int(channel_id) < 0 {
		return none
	}
	node := t.a.nodes[int(channel_id)]
	if node.kind != .selector {
		return none
	}
	for candidate in [flat.source_type_name(t.resolve_selector_type(node)),
		flat.source_type_name(t.selector_field_type(node))] {
		clean := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(flat.source_type_id(candidate))))
		if clean.starts_with('chan ') || clean.starts_with('&chan ') {
			return flat.source_type_id(clean.trim_left('&').trim_space())
		}
	}
	return none
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
	if source_type == 0 {
		source_type = t.node_type(channel_id)
	}
	source_type = t.trimmed_type_id(t.normalize_type_alias(source_type))
	return flat.source_type_shape(source_type).has(.pointer)
}

fn (mut t Transformer) is_channel_receive_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	_ := t.channel_receive_or_info(node) or { return false }
	return true
}

fn (mut t Transformer) channel_receive_or_info(node flat.Node) ?ChannelReceiveInfo {
	if node.kind != .or_expr || node.children_count < 1 {
		return none
	}
	source_id := t.a.child(&node, 0)
	if info := t.channel_receive_info(source_id) {
		return info
	}
	mut expr_id := source_id
	for int(expr_id) >= 0 {
		expr := t.a.nodes[int(expr_id)]
		if expr.kind in [.paren, .expr_stmt] && expr.children_count > 0 {
			expr_id = t.a.child(&expr, 0)
			continue
		}
		break
	}
	if int(expr_id) < 0 {
		return none
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind != .prefix || expr.op != .arrow || expr.children_count == 0 {
		return none
	}
	mut value_type := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(node.typ)))
	if (value_type.len == 0 || value_type in ['void', 'unknown'])
		&& t.is_optional_type_name(t.cur_fn_ret_type) {
		value_type = flat.source_type_name(t.optional_base_type(t.qualify_optional_type(t.cur_fn_ret_type)))
	}
	if value_type.len == 0 || value_type in ['void', 'unknown'] {
		return none
	}
	channel_id := t.a.child(&expr, 0)
	return ChannelReceiveInfo{
		channel_id:  channel_id
		value_type:  flat.source_type_id(value_type)
		needs_deref: t.channel_receive_source_needs_deref(channel_id)
	}
}

fn (mut t Transformer) transform_channel_receive_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	body_id := t.a.child(&node, 1)
	info := t.channel_receive_or_info(node) or { return id }
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
	channel_cast := t.make_cast(flat.source_type_id('&sync.Channel'), channel_source, flat.source_type_id('&sync.Channel'))
	prelude << t.make_decl_assign_typed(channel_name, channel_cast, flat.source_type_id('&sync.Channel'))
	pop_call := t.make_call_typed('sync__Channel__pop', [t.make_ident(channel_name),
		t.make_prefix(.amp, t.make_ident(val_name))], flat.source_type_bool)
	prelude << t.make_decl_assign_typed(ok_name, pop_call, flat.source_type_bool)
	t.mark_fn_used('sync__Channel__closed_error')
	err_expr := t.make_call_typed('sync__Channel__closed_error', [
		t.make_ident(channel_name),
	], flat.source_type_id('IError'))
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
		t.make_selector(base_expr, 'len', flat.source_type_int)
	}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(start_name, start_expr, flat.source_type_int)
	prelude << t.make_decl_assign_typed(end_name, end_expr, flat.source_type_int)
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(flat.source_type_string), flat.source_type_string)
	start_ident := t.make_ident(start_name)
	lower_ok := t.make_infix(.ge, start_ident, t.make_int_literal(0))
	ordered := t.make_infix(.le, t.make_ident(start_name), t.make_ident(end_name))
	upper_ok := t.make_infix(.le, t.make_ident(end_name), t.make_selector(base_expr, 'len', flat.source_type_int))
	bounds_ok := t.make_infix(.logical_and, t.make_infix(.logical_and, lower_ok, ordered), upper_ok)
	slice_call := t.make_call_typed('string__substr', [base_expr, t.make_ident(start_name),
		t.make_ident(end_name)], flat.source_type_string)
	then_block := t.make_block([t.make_assign(t.make_ident(val_name), slice_call)])
	else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name, flat.source_type_string,
		node.value, flat.empty_node))
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
	info := t.array_index_info(expr_id) or { t.array_get_call_info(expr_id) or { return id } }
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
	mut result_type := info.value_type_text()
	mut wrap_found_value := false
	source_is_optional := t.is_optional_type_name(info.value_type)
	mut source_value_type := info.value_type_text()
	if source_is_optional {
		source_value_type = flat.source_type_name(t.optional_base_type(t.qualify_optional_type(info.value_type)))
		result_type = source_value_type
	}
	if t.is_optional_type_name(node.typ) {
		optional_target_source_id := t.qualify_optional_type(node.typ)
		optional_target := flat.source_type_name(optional_target_source_id)
		body_type_source_id := t.stmt_value_type(body_id)

		body_keeps_optional := t.or_body_is_none(body_id) || t.is_optional_type_name(body_type_source_id)
		if (!source_is_optional || body_keeps_optional)
			&& t.normalize_type_alias(t.optional_base_type(optional_target_source_id)) == t.normalize_type_alias(flat.source_type_id(source_value_type)) {
			result_type = optional_target
			wrap_found_value = true
		}
	}
	if info.base_optional {
		// The gated negative-index wrap reads `base_opt.value.len`, so the
		// optional base must exist before that wrap is evaluated.
		prelude << t.make_decl_assign_typed(base_opt_name, base_expr0, info.optional_type)
	}
	prelude << t.make_decl_assign_typed(index_name, index_expr, flat.source_type_int)
	if t.a.nodes[int(expr_id)].op == .gated_index {
		// `base#[i] or {}`: a negative index counts from the end before the
		// bounds check decides between the value and the or-branch.
		neg_cond := t.make_infix(.lt, t.make_ident(index_name), t.make_int_literal(0))
		wrap_assign := t.make_assign(t.make_ident(index_name), t.make_infix(.plus,
			t.make_ident(index_name), t.array_index_len_expr(info, base_expr)))
		prelude << t.make_if(neg_cond, t.make_block([wrap_assign]), t.make_empty())
	}
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(flat.source_type_id(result_type)), flat.source_type_id(result_type))

	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name), t.array_index_len_expr(info, base_expr))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)
	else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
		flat.source_type_id(result_type), node.value, flat.empty_node))
	index_value0 := t.make_index(base_expr, t.make_ident(index_name), info.value_type)
	mut then_block := flat.empty_node
	if source_is_optional {
		opt_name := t.new_temp('arr_opt')
		opt_decl := t.make_decl_assign_typed(opt_name, index_value0, info.value_type)
		opt_value := t.make_selector(t.make_ident(opt_name), 'value', flat.source_type_id(source_value_type))
		index_value := if wrap_found_value {
			t.make_optional_some(opt_value, flat.source_type_id(result_type))
		} else {
			opt_value
		}
		assign_found := t.make_assign(t.make_ident(val_name), index_value)
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', flat.source_type_bool)
		opt_err_expr := t.result_error_expr(t.make_ident(opt_name))
		opt_else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
			flat.source_type_id(result_type), node.value, opt_err_expr))
		then_block = t.make_block([opt_decl,
			t.make_if(ok_cond, t.make_block([assign_found]), opt_else_block)])
	} else {
		index_value := if wrap_found_value {
			t.make_optional_some(index_value0, flat.source_type_id(result_type))
		} else {
			index_value0
		}
		then_block = t.make_block([t.make_assign(t.make_ident(val_name), index_value)])
	}
	mut opt_else := flat.empty_node
	if info.base_optional {
		opt_err := t.result_error_expr(t.make_ident(base_opt_name))
		opt_else = t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
			flat.source_type_id(result_type), node.value, opt_err))
	}
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	if info.base_optional {
		opt_ok := t.make_selector(t.make_ident(base_opt_name), 'ok', flat.source_type_bool)
		t.pending_stmts << t.make_if(opt_ok, t.make_block([
			t.make_if(found_cond, then_block, else_block),
		]), opt_else)
	} else {
		t.pending_stmts << t.make_if(found_cond, then_block, else_block)
	}
	result := t.make_ident(val_name)
	t.set_node_typ(int(result), flat.source_type_id(result_type))
	return result
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
fn (mut t Transformer) enum_from_string_info(expr_id flat.NodeId) ?EnumFromStringInfo {
	if int(expr_id) < 0 {
		return none
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind != .call || expr.children_count < 2 {
		return none
	}
	fn_id := t.a.child(&expr, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value !in ['from', 'from_string']
		|| fn_node.children_count == 0 {
		return none
	}
	if t.enum_from_string_call_uses_user_method(expr_id) {
		return none
	}
	enum_id := t.a.child(&fn_node, 0)
	enum_type_source_id := t.enum_type_from_node(enum_id) or { return none }
	enum_type := flat.source_type_name(enum_type_source_id)
	arg_id := t.a.child(&expr, 1)
	arg_node := t.a.nodes[int(arg_id)]
	mut arg_type := if arg_node.kind in [.string_literal, .string_interp] {
		'string'
	} else {
		flat.source_type_name(t.normalized_node_type(arg_id)).trim_left('&')
	}
	if t.validating_generic_spec && arg_type in ['', 'unknown'] {
		arg_type = flat.source_type_name(t.normalize_type_alias(t.specialized_expr_type_name(arg_id))).trim_left('&')
	}
	if fn_node.value == 'from' && arg_type != 'string' {
		return none
	}
	fields := t.enum_types[enum_type] or { return none }
	if fields.len == 0 {
		return none
	}
	return EnumFromStringInfo{
		enum_type:    enum_type_source_id
		fields:       fields.clone()
		arg_id:       arg_id
		accept_empty: fn_node.value == 'from' && t.is_flag_enum_type(enum_type_source_id)
		is_result:    fn_node.value == 'from'
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
fn (t &Transformer) enum_type_from_node(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		for i, param in t.active_generic_params {
			if node.value == param && i < t.active_specialization_args.ids.len {
				concrete_source_id := t.active_specialization_args.ids[i]

				if enum_type_source_id := t.enum_type_from_name(concrete_source_id) {
					return enum_type_source_id
				}
			}
		}
		if enum_type_source_id := t.enum_type_from_name(flat.source_type_id(node.value)) {
			return enum_type_source_id
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind == .ident {
			qname := '${base.value}.${node.value}'
			if resolved_source_id := t.resolve_import_alias_pattern(flat.source_type_id(qname)) {
				resolved_source_id_2 := resolved_source_id

				if enum_type_source_id := t.enum_type_from_name(resolved_source_id_2) {
					return enum_type_source_id
				}
			}
			if enum_type_source_id := t.enum_type_from_name(flat.source_type_id(qname)) {
				return enum_type_source_id
			}
		}
	}
	return none
}

fn (t &Transformer) enum_type_from_name(name_id u32) ?u32 {
	name := flat.source_type_name(name_id)
	if name in t.enum_types {
		return name_id
	}
	mut qname := name
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qname = '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return flat.source_type_id(qname)
		}
	}
	if !isnil(t.tc) {
		if !name.contains('.') && t.cur_file.len > 0 {
			for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
				[]string{}
			} {
				if candidate in t.enum_types {
					return flat.source_type_id(candidate)
				}
				if target_source_id := t.enum_alias_target(flat.source_type_id(candidate)) {
					return target_source_id
				}
			}
		}
		if target_source_id := t.enum_alias_target(name_id) {
			return target_source_id
		}
		if qname != name {
			if target_source_id := t.enum_alias_target(flat.source_type_id(qname)) {
				return target_source_id
			}
		}
	}
	mut found := ''
	for key, _ in t.enum_types {
		if key.all_after_last('.') != name {
			continue
		}
		if found != '' && found != key {
			return none
		}
		found = key
	}
	if found.len > 0 {
		return flat.source_type_id(found)
	}
	return none
}

fn (t &Transformer) enum_alias_target(name_id u32) ?u32 {
	name := flat.source_type_name(name_id)
	mut current := name
	for _ in 0 .. 16 {
		target_source_id := t.tc.type_aliases.type_id(current) or { return none }
		target := flat.source_type_name(target_source_id)
		if target == current {
			return none
		}
		if target in t.enum_types {
			return target_source_id
		}
		current = target
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
	prelude << t.make_decl_assign_typed(str_name, arg_expr, flat.source_type_string)
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.enum_type),
		info.enum_type)

	failure := if info.is_result {
		t.make_call_typed('error', [t.make_string_literal('invalid value')], flat.source_type_id('IError'))
	} else {
		flat.empty_node
	}
	mut else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, val_name,
		info.enum_type, node.value, failure))
	for i := members.len - 1; i >= 0; i-- {
		cond := t.make_call_typed('string__eq', [t.make_ident(str_name),
			t.make_string_literal(members[i].name)], flat.source_type_bool)
		assign := t.make_assign(t.make_ident(val_name), t.enum_from_string_member_value(info,
			members[i]))
		then_block := t.make_block([assign])
		else_block = t.make_if(cond, then_block, else_block)
	}
	if info.accept_empty {
		cond := t.make_call_typed('string__eq', [t.make_ident(str_name), t.make_string_literal('')], flat.source_type_bool)
		else_block = t.make_if(cond, t.make_block([]flat.NodeId{}), else_block)
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
	optional_type := (if info.is_result {
		flat.source_type_id('!${info.enum_type_text()}')
	} else {
		flat.source_type_id('?${info.enum_type_text()}')
	})
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	arg_expr := t.transform_expr(info.arg_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(str_name, arg_expr, flat.source_type_string)
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.enum_type),
		info.enum_type)
	prelude << t.make_decl_assign_typed(ok_name, t.make_bool_literal(false), flat.source_type_bool)

	mut else_block := t.make_block([]flat.NodeId{})
	for i := members.len - 1; i >= 0; i-- {
		cond := t.make_call_typed('string__eq', [t.make_ident(str_name),
			t.make_string_literal(members[i].name)], flat.source_type_bool)
		assign_val := t.make_assign(t.make_ident(val_name), t.enum_from_string_member_value(info,
			members[i]))
		assign_ok := t.make_assign(t.make_ident(ok_name), t.make_bool_literal(true))
		then_block := t.make_block([assign_val, assign_ok])
		else_block = t.make_if(cond, then_block, else_block)
	}
	if info.accept_empty {
		cond := t.make_call_typed('string__eq', [t.make_ident(str_name), t.make_string_literal('')], flat.source_type_bool)
		assign_ok := t.make_assign(t.make_ident(ok_name), t.make_bool_literal(true))
		else_block = t.make_if(cond, t.make_block([assign_ok]), else_block)
	}
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << else_block

	if info.is_result {
		result_name := t.new_temp('enum_result')
		t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_optional_none(optional_type),
			optional_type)
		success := t.make_optional_some(t.make_ident(val_name), optional_type)
		err_expr := t.make_call_typed('error', [t.make_string_literal('invalid value')], flat.source_type_id('IError'))
		failure := t.make_optional_none_with_err(optional_type, err_expr)
		t.pending_stmts << t.make_if(t.make_ident(ok_name),
			t.make_block([t.make_assign(t.make_ident(result_name), success)]),
			t.make_block([t.make_assign(t.make_ident(result_name), failure)]))
		result := t.make_ident(result_name)
		t.set_node_typ(int(result), optional_type)
		return result
	}
	ok_field := t.make_sum_literal_field('ok', t.make_ident(ok_name), flat.source_type_bool)
	value_field := t.make_sum_literal_field('value', t.make_ident(val_name), info.enum_type)
	start := t.a.children.len
	t.a.children << ok_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          flat.source_type_name(optional_type)
		typ:            optional_type
	})
}

// or_expr_types supports or expr types handling for Transformer.
fn (mut t Transformer) or_expr_types(expr_id flat.NodeId, fallback_type_id u32) (u32, u32) {
	fallback_type := flat.source_type_name(fallback_type_id)
	expr_node := if int(expr_id) >= 0 && int(expr_id) < t.a.nodes.len {
		t.a.nodes[int(expr_id)]
	} else {
		flat.Node{}
	}
	if expr_node.kind == .ident {
		raw_type_source_id := t.raw_var_type(expr_node.value)
		if t.is_optional_type_name(raw_type_source_id) && !t.generic_arg_is_unresolved(raw_type_source_id) {
			return t.canonical_or_expr_types(raw_type_source_id)
		}
		if flat.source_type_shape(raw_type_source_id).has(.pointer) {
			base_type := t.trim_pointer_type(raw_type_source_id)
			if t.is_optional_type_name(base_type) && !t.generic_arg_is_unresolved(base_type) {
				return t.canonical_or_expr_types(base_type)
			}
		}
	}
	if expr_node.kind == .selector {
		if raw_type_source_id := t.raw_selector_field_type(expr_id) {
			if t.is_optional_type_name(raw_type_source_id) && !t.generic_arg_is_unresolved(raw_type_source_id) {
				return t.canonical_or_expr_types(raw_type_source_id)
			}
			if flat.source_type_shape(raw_type_source_id).has(.pointer) {
				base_type := t.trim_pointer_type(raw_type_source_id)
				if t.is_optional_type_name(base_type) && !t.generic_arg_is_unresolved(base_type) {
					return t.canonical_or_expr_types(base_type)
				}
			}
		}
	}
	if expr_node.kind == .sql_expr {
		if sql_type_source_id := t.sql_expr_result_type(expr_node) {
			sql_type_source_id_2 := sql_type_source_id

			return t.canonical_or_expr_types(sql_type_source_id_2)
		}
	}
	if !isnil(t.tc) {
		if expr_node.kind == .call {
			// `json2.decode[T](text)` returns `!T`; take it from the type argument
			// before generic/declaration fallbacks.
			if decode_ret_source_id := t.json_decode_or_expr_type(expr_id, expr_node) {
				decode_ret_source_id_2 := decode_ret_source_id

				return t.canonical_or_expr_types(decode_ret_source_id_2)
			}
			if specialized_ret_source_id := t.specialized_interface_method_call_return_type(expr_id,
				expr_node)
			{
				if t.is_optional_type_name(specialized_ret_source_id)
					&& !t.generic_arg_is_unresolved(specialized_ret_source_id) {
					return t.canonical_or_expr_types(specialized_ret_source_id)
				}
			}
			if thread_ret_source_id := t.thread_wait_or_expr_type(expr_node) {
				thread_ret_source_id_2 := thread_ret_source_id

				return t.canonical_or_expr_types(thread_ret_source_id_2)
			}
			if t.is_optional_type_name(expr_node.typ)
				&& t.generic_type_text_contains_alias(expr_node.typ, t.cur_module)
				&& !t.generic_arg_is_unresolved(expr_node.typ) {
				return t.specialized_or_expr_types(expr_node.typ)
			}
			if current_ret_source_id := t.current_generic_receiver_call_return_type(expr_node) {
				if t.is_optional_type_name(current_ret_source_id) && !t.generic_arg_is_unresolved(current_ret_source_id) {
					return t.canonical_or_expr_types(current_ret_source_id)
				}
			}
			concrete_ret_source_id := t.concrete_generic_call_return_type(expr_id, expr_node)

			if t.is_optional_type_name(concrete_ret_source_id) && !t.generic_arg_is_unresolved(concrete_ret_source_id) {
				return t.specialized_or_expr_types(concrete_ret_source_id)
			}
			if declared_ret_source_id := t.call_declared_return_type_text(expr_id) {
				declared_ret_source_id_2 := declared_ret_source_id

				if t.is_optional_type_name(declared_ret_source_id_2)
					&& !t.generic_arg_is_unresolved(declared_ret_source_id_2) {
					return t.canonical_or_expr_types(declared_ret_source_id_2)
				}
			}
			if expr_node.children_count > 0 {
				callee := t.a.child_node(&expr_node, 0)
				if callee.kind == .ident && t.generic_callee_is_specialization(callee.value)
					&& t.is_optional_type_name(expr_node.typ) {
					return t.specialized_or_expr_types(expr_node.typ)
				}
			}
			if typ := t.tc.expr_type(expr_id) {
				mut prefix := ''
				if typ is types.OptionType {
					prefix = '?'
				} else if typ is types.ResultType {
					prefix = '!'
				}
				if prefix != '' {
					if multi_types := multi_return_types_from_type(typ, 0) {
						return t.canonical_or_expr_types(flat.source_type_id('${prefix}${flat.source_type_name(t.multi_return_type_name(multi_types))}'))
					}
				}
			}
			call_ret_source_id := t.get_call_return_type(expr_id, expr_node)

			if t.is_optional_type_name(call_ret_source_id) && !t.generic_arg_is_unresolved(call_ret_source_id) {
				return t.canonical_or_expr_types(call_ret_source_id)
			}
		}
		if typ := t.tc.expr_type(expr_id) {
			if typ is types.OptionType {
				base_name_source_id := t.value_type_name(typ.base_type)
				base_name := flat.source_type_name(base_name_source_id)
				if t.generic_arg_is_unresolved(base_name_source_id) && decl_type_is_usable(fallback_type_id)
					&& !t.generic_arg_is_unresolved(fallback_type_id) {
					return t.canonical_or_expr_types(flat.source_type_id('?${fallback_type}'))
				}
				source_name := if typ.base_type is types.Void || base_name == '__v_option' {
					'?void'
				} else {
					'?${base_name}'
				}
				return t.canonical_or_expr_types(flat.source_type_id(source_name))
			}
			if typ is types.ResultType {
				base_name_source_id_2 := t.value_type_name(typ.base_type)
				base_name := flat.source_type_name(base_name_source_id_2)
				if t.generic_arg_is_unresolved(base_name_source_id_2) && decl_type_is_usable(fallback_type_id)
					&& !t.generic_arg_is_unresolved(fallback_type_id) {
					return t.canonical_or_expr_types(flat.source_type_id('!${fallback_type}'))
				}
				source_name := if typ.base_type is types.Void || base_name == '__v_option' {
					'!void'
				} else {
					'!${base_name}'
				}
				return t.canonical_or_expr_types(flat.source_type_id(source_name))
			}
		}
	}
	mut expr_type := t.node_type(expr_id)
	if t.generic_arg_is_unresolved(expr_type) && decl_type_is_usable(fallback_type_id)
		&& !t.generic_arg_is_unresolved(fallback_type_id) {
		prefix := if flat.source_type_shape(expr_type).has(.result_type) { '!' } else { '?' }
		expr_type = flat.source_type_id('${prefix}${fallback_type}')
	}
	if flat.source_type_name(expr_type) == '__v_option' {
		return flat.source_type_id('?void'), flat.source_type_void
	}
	if fallback_type == '__v_option' {
		return flat.source_type_id('?void'), flat.source_type_void
	}
	if expr_type == u32(0) || flat.source_type_name(expr_type) == '__v_option' {
		if fallback_type_id != u32(0) && !t.is_optional_type_name(fallback_type_id) {
			expr_type = flat.source_type_id('?${fallback_type}')
		} else {
			expr_type = fallback_type_id
		}
	}
	if flat.source_type_name(expr_type) == '!' || flat.source_type_name(expr_type) == '?' {
		return flat.source_type_id('${flat.source_type_name(expr_type)}void'), flat.source_type_void
	}
	base_type_source_id_2 := t.optional_base_type(expr_type)
	base_type := flat.source_type_name(base_type_source_id_2)
	if base_type == '__v_option' {
		return expr_type, flat.source_type_void
	}
	mut value_type := if base_type_source_id_2 != 0 {
		base_type_source_id_2
	} else {
		fallback_type_id
	}
	if value_type == 0 || flat.source_type_name(value_type) == '!' || flat.source_type_name(value_type) == '?' {
		value_type = flat.source_type_int
	}
	canon_expr_type_source_id, canon_value_type_source_id := t.canonical_or_expr_types(expr_type)
	canon_expr_type_source_id_2 := canon_expr_type_source_id

	canon_value_type_source_id_2 := canon_value_type_source_id

	if t.is_optional_type_name(canon_expr_type_source_id_2) {
		return canon_expr_type_source_id_2, canon_value_type_source_id_2
	}
	return expr_type, value_type
}

fn (t &Transformer) specialized_or_expr_types(expr_type_id u32) (u32, u32) {
	expr_type := flat.source_type_name(expr_type_id)
	clean := trimmed_transform_text(expr_type)
	if clean in ['!', '?'] {
		return flat.source_type_id('${clean}void'), flat.source_type_void
	}
	if clean.len < 2 || (clean[0] != `!` && clean[0] != `?`) {
		clean_id := if clean == expr_type { expr_type_id } else { flat.source_type_id(clean) }
		return clean_id, clean_id
	}
	base := clean[1..]
	if base.len == 0 || base in ['void', '__v_option'] {
		return flat.source_type_id('${clean[..1]}void'), flat.source_type_void
	}
	base_id := t.qualify_specialized_or_expr_value_type(flat.source_type_id(base))
	// The specialized signature is the ABI authority. In particular, resolving
	// `StructKeyDecodeResult[sapp.Event]` again here would unalias the argument to
	// `C.sapp_event`, while the specialized callee still returns the former C
	// generic struct type.
	return flat.source_type_id('${clean[..1]}${flat.source_type_name(base_id)}'), base_id
}

fn (t &Transformer) qualify_specialized_or_expr_value_type(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	qualified := t.qualify_specialized_or_expr_value_type_bytes(typ)
	return if qualified == typ { typ_id } else { flat.source_type_id(qualified) }
}

// qualify_specialized_or_expr_value_type_bytes qualifies spelling bytes inside the numeric query boundary.
fn (t &Transformer) qualify_specialized_or_expr_value_type_bytes(typ string) string {
	clean := trimmed_transform_text(typ)
	if clean.starts_with('(') && clean.ends_with(')') {
		mut qualified_items := []string{}
		for item in split_generic_args(clean[1..clean.len - 1]) {
			qualified_items << t.qualify_specialized_or_expr_value_type_bytes(item)
		}
		return '(${qualified_items.join(', ')})'
	}
	if clean.starts_with('&') {
		return '&' + t.qualify_specialized_or_expr_value_type_bytes(clean[1..])
	}
	if clean.starts_with('[]') {
		return '[]' + t.qualify_specialized_or_expr_value_type_bytes(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := t.qualify_specialized_or_expr_value_type_bytes(clean[4..bracket_end])
			value := t.qualify_specialized_or_expr_value_type_bytes(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 0 && bracket_end < clean.len - 1 {
			return clean[..bracket_end + 1] +
				t.qualify_specialized_or_expr_value_type_bytes(clean[bracket_end + 1..])
		}
	}
	base, args, is_generic := generic_app_parts(clean)
	if !is_generic {
		return clean
	}
	return '${t.qualify_or_expr_generic_base(base)}[${args.join(', ')}]'
}

fn (mut t Transformer) thread_wait_or_expr_type(call flat.Node) ?u32 {
	if call.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&call, 0)
	if callee.kind != .selector || callee.value != 'wait' || callee.children_count == 0 {
		return none
	}
	base_id := t.a.child(callee, 0)
	base := t.a.nodes[int(base_id)]
	mut base_type := if base.kind == .ident {
		flat.source_type_name(t.raw_var_type(base.value))
	} else {
		''
	}
	if base_type.len == 0 {
		base_type = flat.source_type_name(t.resolve_expr_type(base_id))
	}
	clean := base_type.trim_space().trim_left('&').trim_space()
	if t.is_fixed_array_type(flat.source_type_id(clean)) {
		return thread_array_wait_return_type(fixed_array_elem_type(flat.source_type_id(clean)))
	}
	if clean.starts_with('[]') {
		return thread_array_wait_return_type(flat.source_type_id(trimmed_transform_text(clean[2..])))
	}
	if !clean.starts_with('thread ') {
		return none
	}
	payload := trimmed_transform_text(clean[7..])
	if payload == '?' || payload == '!' {
		return flat.source_type_id('${payload}void')
	}
	if payload.starts_with('?') || payload.starts_with('!') {
		return flat.source_type_id(payload)
	}
	return none
}

fn (t &Transformer) canonical_or_expr_types(expr_type_id u32) (u32, u32) {
	expr_type := flat.source_type_name(expr_type_id)
	// A bare `!` or `?` (a result/option of `void`, e.g. `fn f() !`) carries no payload.
	// Canonicalize the wrapper spelling before normalizing its payload.
	if expr_type == '!' || expr_type == '?' {
		return flat.source_type_id('${expr_type}void'), flat.source_type_void
	}
	mut clean_type_id := expr_type_id
	if !t.is_optional_type_name(clean_type_id) {
		normalized := t.normalize_type_alias(clean_type_id)
		if normalized != clean_type_id { clean_type_id = normalized }
	}
	if !t.is_optional_type_name(clean_type_id) {
		return clean_type_id, t.optional_base_type(clean_type_id)
	}
	prefix := flat.source_type_name(clean_type_id)[..1]
	mut base_id := t.optional_base_type(clean_type_id)
	// A void Result/Option has no payload. Avoid normalizing its wrapper struct
	// as a value type and adding another wrapper around it.
	if base_id == 0 || base_id == flat.source_type_void
		|| flat.source_type_name(base_id) == '__v_option' || base_id == clean_type_id {
		return flat.source_type_id('${prefix}void'), flat.source_type_void
	}
	// Monomorphized collection arguments can reach an or-expression in their
	// flattened receiver spelling. Decode those names back to semantic V types so
	// option wrappers use the runtime Array/map ABI instead of imaginary structs.
	base_text := flat.source_type_name(base_id)
	if base_text.starts_with('Array_') || base_text.starts_with('Map_') {
		decoded := generic_type_arg_from_suffix_with_containers(base_text)
		if decoded != base_text { base_id = flat.source_type_id(decoded) }
	}
	shared_storage := t.shared_alias_storage_type(base_id)
	if shared_storage != base_id {
		return flat.source_type_id('${prefix}${flat.source_type_name(shared_storage)}'), shared_storage
	}
	base_id = t.normalize_or_expr_value_type(base_id)
	// Resolve import aliases and nested generic arguments in the payload. The
	// optional wrapper and its lowered value temporary must use one canonical
	// spelling (`json2.Any`, not the source alias `json.Any`).
	if !isnil(t.tc) {
		resolved := t.tc.parse_resolution_type(base_id)
		if resolved !is types.Unknown && resolved !is types.Void {
			base_id = resolved.source_type_id()
		}
	}
	return flat.source_type_id('${prefix}${flat.source_type_name(base_id)}'), base_id
}

fn (t &Transformer) call_declared_return_type_text(id flat.NodeId) ?u32 {
	if isnil(t.tc) {
		return none
	}
	name := t.tc.resolved_call_name(id) or { return none }
	if ret_source_id := t.tc.fn_ret_type_texts.type_id(name) {
		resolved := t.tc.fn_signature_type(name, ret_source_id)
		if resolved !is types.Unknown && resolved !is types.Void {
			return resolved.source_type_id()
		}
		return ret_source_id
	}
	short_name := name.all_after_last('.')
	if ret_source_id := t.tc.fn_ret_type_texts.type_id(short_name) {
		return ret_source_id
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		if ret_source_id := t.tc.fn_ret_type_texts.type_id('${t.cur_module}.${short_name}') {
			return ret_source_id
		}
	}
	return none
}

fn (t &Transformer) normalize_or_expr_value_type(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	// Container types must be decomposed before `generic_app_parts`, which mistakes
	// a `map[K]V` for a `map[K]` generic application and drops the value type (V).
	if typ.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.normalize_or_expr_value_type(flat.source_type_id(typ[1..]))))
	}
	if typ.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.normalize_or_expr_value_type(flat.source_type_id(typ[2..]))))
	}
	if typ.starts_with('map[') {
		bracket_end := generic_matching_bracket(typ, 3)
		if bracket_end > 3 && bracket_end < typ.len {
			key := typ[4..bracket_end]
			val := typ[bracket_end + 1..]
			return flat.source_type_id('map[${flat.source_type_name(t.normalize_or_expr_value_type(flat.source_type_id(key)))}]${flat.source_type_name(t.normalize_or_expr_value_type(flat.source_type_id(val)))}')
		}
	}
	if typ.starts_with('(') && typ.ends_with(')') {
		mut normalized_items := []string{}
		for item in split_generic_args(typ[1..typ.len - 1]) {
			normalized_items << flat.source_type_name(t.normalize_or_expr_value_type(flat.source_type_id(item)))
		}
		return flat.source_type_id('(${normalized_items.join(', ')})')
	}
	if typ.starts_with('map[') {
		key_type_source_id, value_type_source_id_5 := t.map_type_parts(typ_id)
		if key_type_source_id != 0 && value_type_source_id_5 != 0 {
			return flat.source_type_id('map[${flat.source_type_name(t.normalize_or_expr_value_type(key_type_source_id))}]${flat.source_type_name(t.normalize_or_expr_value_type(value_type_source_id_5))}')
		}
	}
	base, args, is_generic := generic_app_parts(typ)
	if !is_generic {
		return t.normalize_type_alias(typ_id)
	}
	mut normalized_base := t.normalize_type_alias(flat.source_type_id(base))
	// Imported generic structs have both a qualified declaration entry and a
	// short convenience entry. The latter is sufficient for field lookup but is
	// not a valid C type outside its declaring module (`QueryBuilder_User` versus
	// `orm__QueryBuilder_User`), so retain the qualified owner in option/result
	// payload temporaries.
	normalized_base = flat.source_type_id(t.qualify_or_expr_generic_base(flat.source_type_name(normalized_base)))
	mut normalized_args := []string{cap: args.len}
	for arg in args {
		normalized_args << flat.source_type_name(t.normalize_or_expr_value_type(flat.source_type_id(arg)))
	}
	return flat.source_type_id('${flat.source_type_name(normalized_base)}[${normalized_args.join(', ')}]')
}

fn (t &Transformer) qualify_or_expr_generic_base(base string) string {
	if base.contains('.') {
		return base
	}
	local_qualified := if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		'${t.cur_module}.${base}'
	} else {
		''
	}
	if local_qualified.len > 0 && t.type_authority_has(local_qualified) {
		return local_qualified
	}
	if !t.bare_struct_name_is_local_to_current_module(flat.source_type_id(base)) {
		if qualified_source_id := t.qualified_types.type_id(base) {
			qualified := flat.source_type_name(qualified_source_id)

			return qualified
		}
	}
	return base
}

// json_decode_or_expr_type returns `!T` for a `json2.decode[T](text)` call.
fn (t &Transformer) json_decode_or_expr_type(expr_id flat.NodeId, expr_node flat.Node) ?u32 {
	if isnil(t.tc) || expr_node.kind != .call {
		return none
	}
	mut is_decode := t.call_name_for_node(expr_id, expr_node) in ['json2.decode', 'x.json2.decode']
	if name := t.tc.resolved_call_name(expr_id) {
		is_decode = ['json2.decode', 'x.json2.decode'].any(name == it
			|| name.starts_with('${it}[') || name.starts_with('${it}__'))
	}
	if !is_decode && expr_node.children_count > 0 {
		callee := t.a.child_node(&expr_node, 0)
		if callee.kind == .selector && callee.value == 'decode' && callee.children_count > 0 {
			base := t.a.child_node(callee, 0)
			is_decode = (base.kind == .selector && base.value == 'json2')
				|| (base.kind == .ident && (base.value == 'json2'
					|| (t.file_import_module(t.cur_file, base.value) or { '' }) in [
						'json2',
						'x.json2',
					]))
		}
	}
	if !is_decode {
		return none
	}
	args := t.explicit_generic_call_args(expr_node, t.cur_module) or { return none }
	if args.len == 1 && args[0] != 0 {
		return t.specialized_json_decode_result_type(args[0])
	}
	return none
}

fn (t &Transformer) specialized_json_decode_result_type(type_arg_id u32) u32 {
	type_arg := flat.source_type_name(type_arg_id)
	if t.active_specialization_args.ids.len == 0 {
		return flat.source_type_id('!${type_arg}')
	}
	concrete := flat.source_type_name(t.subst_type(type_arg_id, t.active_specialization_args.ids))
	return flat.source_type_id('!${concrete}')
}

// value_type_name returns value type name data for Transformer.
fn (t &Transformer) value_type_name(typ types.Type) u32 {
	if typ is types.Void {
		return flat.source_type_void
	}
	return typ.source_type_id()
}

// is_optional_type_name reports whether is optional type name applies in transform.
fn (t &Transformer) is_optional_type_name(typ_id u32) bool {
	return flat.source_type_shape(typ_id).has(.optional)
}

// qualify_optional_type supports qualify optional type handling for Transformer.
fn (t &Transformer) qualify_optional_type(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	if typ.len < 2 || (typ[0] != `?` && typ[0] != `!`) {
		return typ_id
	}
	base := typ[1..]
	if base.contains('.') || base.len == 0 {
		return typ_id
	}
	qualified := flat.source_type_name(t.qualify_type(flat.source_type_id(base)))
	if qualified != base {
		return flat.source_type_id(typ[..1] + qualified)
	}
	return typ_id
}

// qualify_type supports qualify type handling for Transformer.
fn (t &Transformer) qualify_type(name_id u32) u32 {
	name := flat.source_type_name(name_id)
	if name.contains('.') || name_id == u32(0) {
		return name_id
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if t.sum_types.contains(qname) || qname in t.structs || qname in t.enum_types
			|| (!isnil(t.tc) && (t.tc.type_aliases.contains(qname) || qname in t.tc.interface_names)) {
			return flat.source_type_id(qname)
		}
	}
	// A bare type declared by main wins over an imported homonym. The global
	// short-name index is intentionally lossy and may point at the import.
	if name in t.structs || t.sum_types.contains(name) || name in t.enum_types {
		return name_id
	}
	if !isnil(t.tc) && (t.tc.type_aliases.contains(name) || name in t.tc.interface_names) {
		return name_id
	}
	if qualified_source_id := t.qualified_types.type_id(name) {
		return qualified_source_id
	}
	return name_id
}

// make_decl_assign_typed builds make decl assign typed data for transform.
fn (mut t Transformer) make_decl_assign_typed(name string, rhs flat.NodeId, typ_id u32) flat.NodeId {
	decl := t.make_decl_assign(name, rhs)
	if typ_id != u32(0) {
		t.set_node_typ(int(decl), typ_id)
		decl_node := t.a.nodes[int(decl)]
		if decl_node.children_count > 0 {
			lhs_id := t.a.child(&decl_node, 0)
			if int(lhs_id) >= 0 {
				t.set_node_typ(int(lhs_id), typ_id)
			}
		}
		t.set_var_type(name, typ_id)
	}
	return decl
}

// make_stack_value_decl_assign_typed creates an internal value staging local that must not
// itself inherit `@[heap]`. The source binding that consumes it performs the single heap copy.
fn (mut t Transformer) make_stack_value_decl_assign_typed(name string, rhs flat.NodeId, typ_id u32) flat.NodeId {
	decl := t.make_decl_assign_typed(name, rhs, typ_id)
	t.a.nodes[int(decl)].value = stack_value_decl_marker
	return decl
}

// make_staging_value_decl reserves a temporary assigned by every continuing branch.
// Preserve its typed initializer for other backends; C needs only zeroed storage.
fn (mut t Transformer) make_staging_value_decl(name string, typ_id u32) flat.NodeId {
	decl := t.make_stack_value_decl_assign_typed(name, t.zero_value_for_type(typ_id), typ_id)
	t.a.nodes[int(decl)].value = zeroed_stack_value_decl_marker
	return decl
}

// zero_value_for_type supports zero value for type handling for Transformer.
fn (mut t Transformer) zero_value_for_type(typ_id u32) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	mut clean := typ
	if clean.starts_with('&') {
		return t.a.add(.nil_literal)
	}
	if clean.len > 1 && (clean[0] == `?` || clean[0] == `!`) {
		return t.make_optional_none(flat.source_type_id(clean))
	}
	_, _, is_generic_app := generic_app_parts(clean)
	if expanded_source_id := t.expand_generic_type_alias(flat.source_type_id(clean)) {
		clean = flat.source_type_name(t.normalize_type_alias(expanded_source_id))
	} else if !is_generic_app {
		clean = flat.source_type_name(t.normalize_type_alias(flat.source_type_id(clean)))
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') || clean.starts_with('fn_ptr:') {
		return t.make_cast(flat.source_type_id(clean), t.make_int_literal(0), flat.source_type_id(clean))
	}
	if clean.starts_with('[]') {
		return t.make_array_init(flat.source_type_id(clean[2..]))
	}
	if clean.starts_with('map[') {
		return t.make_map_init(flat.source_type_id(clean))
	}
	if t.is_fixed_array_type(flat.source_type_id(clean)) {
		fixed_type_source_id_2 := t.resolved_fixed_array_canonical_type(flat.source_type_id(clean))
		elem_type := fixed_array_elem_type(fixed_type_source_id_2)
		if !t.fixed_array_empty_init_needs_values(elem_type) {
			return t.make_fixed_array_init(fixed_type_source_id_2)
		}
		if len := t.fixed_array_len_value(fixed_type_source_id_2) {
			return t.make_fixed_array_empty_literal(fixed_type_source_id_2, len, elem_type)
		}
		return t.make_fixed_array_init(fixed_type_source_id_2)
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
	if clean in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'u16', 'u32', 'u64',
		'rune', 'char'] {
		return t.make_int_literal(0)
	}
	if clean in t.enum_types {
		// An enum defaults to its first member, for example for a missing map key.
		if default_value := t.enum_nonzero_default_value(flat.source_type_id(clean)) {
			return default_value
		}
		return t.make_int_literal(0)
	}
	if default_sum := t.make_default_sum_value(flat.source_type_id(clean)) {
		return default_sum
	}
	return t.make_struct_init(flat.source_type_id(clean))
}

// make_panic_stmt builds make panic stmt data for transform.
fn (mut t Transformer) make_panic_stmt(message string) flat.NodeId {
	call := t.make_call('panic', [t.make_string_literal(message)])
	return t.make_expr_stmt(call)
}

fn (mut t Transformer) make_panic_expr_stmt_at(message flat.NodeId, source_id flat.NodeId) flat.NodeId {
	call := t.make_call('panic', [message])
	if int(source_id) >= 0 && int(source_id) < t.a.nodes.len {
		t.a.nodes[int(call)].pos = t.a.nodes[int(source_id)].pos
	}
	return t.make_expr_stmt(call)
}

fn (mut t Transformer) make_propagation_panic_stmts(mode string, err_expr flat.NodeId, source_id flat.NodeId) []flat.NodeId {
	if int(err_expr) < 0 || int(err_expr) >= t.a.nodes.len {
		kind := if mode == '?' { 'option' } else { 'result' }
		message := t.make_string_literal('${kind} not set ()')
		return [t.make_panic_expr_stmt_at(message, source_id)]
	}
	err_addr := t.make_prefix(.amp, err_expr)
	err_message := t.make_call_typed('IError__msg', [err_addr], flat.source_type_string)
	if mode != '?' {
		return [t.make_panic_expr_stmt_at(err_message, source_id)]
	}
	message_name := t.new_temp('propagation_message')
	message := t.make_ident(message_name)
	empty := t.make_infix(.eq, t.make_selector(message, 'len', flat.source_type_int), t.make_int_literal(0))
	missing := t.make_panic_expr_stmt_at(t.make_string_literal('option not set ()'), source_id)
	reported := t.make_panic_expr_stmt_at(message, source_id)
	return [
		t.make_decl_assign_typed(message_name, err_message, flat.source_type_string),
		t.make_if(empty, t.make_block([missing]), t.make_block([reported])),
	]
}

// make_none_return_stmt builds make none return stmt data for transform.
fn (mut t Transformer) make_none_return_stmt() flat.NodeId {
	return t.make_return(t.a.add(.none_expr), t.cur_fn_ret_type)
}

fn (mut t Transformer) make_none_return_stmt_with_err_expr(err_expr flat.NodeId) flat.NodeId {
	if !flat.source_type_shape(t.normalize_type_alias(t.cur_fn_ret_type)).has(.result_type) {
		return t.make_none_return_stmt()
	}
	failure := if int(err_expr) >= 0 { err_expr } else { t.make_ierror_none() }
	return t.make_return(t.make_optional_none_with_err(t.cur_fn_ret_type, failure),
		t.cur_fn_ret_type)
}

fn (mut t Transformer) make_or_else_block(mode string, stmts []flat.NodeId) flat.NodeId {
	// `!` and `?` lower to a branch the source never wrote, so it has no checker scope.
	if mode == '!' || mode == '?' {
		return t.make_block_skip_scope_drops(stmts)
	}
	return t.make_block(stmts)
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
	fallback_type := (if decl_type_is_usable(node.typ) && !t.generic_arg_is_unresolved(node.typ) {
		node.typ
	} else {
		t.stmt_value_type(body_id)
	})
	expr_type_source_id, value_type_source_id_7 := t.or_expr_types(expr_id, fallback_type)
	expr_type_source_id_4 := expr_type_source_id

	value_type_source_id_9 := value_type_source_id_7
	value_type := flat.source_type_name(value_type_source_id_9)
	if !t.is_optional_type_name(expr_type_source_id_4) {
		return t.transform_expr(expr_id)
	}
	is_void := value_type_source_id_9 == 0 || value_type_source_id_9 == flat.source_type_void
		|| value_type == '__v_option' || value_type == '!' || value_type == '?'
		|| (t.is_optional_type_name(value_type_source_id_9) && t.optional_base_type(value_type_source_id_9) == flat.source_type_void)
	opt_tmp := t.new_temp('or_opt')
	val_tmp := t.new_temp('or_val')

	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut new_expr := t.disabled_optional_call_or_none(expr_id, expr_node, expr_type_source_id_4) or {
		t.transform_expr(expr_id)
	}
	new_expr = t.optional_source_value_expr(expr_id, new_expr, expr_type_source_id_4)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	mut new_expr_type := t.node_type(new_expr)
	// A smartcast unwrap access (`x.value`) is typed with its payload. Resolving it
	// as a selector would find a payload struct field that is also named `value`.
	if int(new_expr) >= 0 && int(new_expr) < t.a.nodes.len {
		new_expr_node := t.a.nodes[int(new_expr)]
		if new_expr_node.kind == .selector && new_expr_node.typ != 0
			&& transformed_option_unwrap_access_marker in new_expr_node.generic_params() {
			new_expr_type = new_expr_node.typ
		}
	}
	// A preceding guard can prove an optional selector successful and smartcast
	// the source directly to its payload (for example `if !x.ok { return }`
	// followed by `x or { ... }`). In that case the fallback is unreachable and
	// wrapping the payload back as the optional source would emit `x.value` with
	// an optional C type.
	if !t.is_optional_type_name(new_expr_type)
		&& t.normalize_type_alias(new_expr_type) == t.normalize_type_alias(value_type_source_id_9) {
		t.pending_stmts = outer_pending
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		return new_expr
	}
	t.mark_optional_source_expr_type(new_expr, expr_type_source_id_4)
	new_expr_type = t.node_type(new_expr)
	if !t.is_optional_type_name(new_expr_type) {
		if t.is_optional_type_name(expr_type_source_id_4) {
			t.set_node_typ(int(new_expr), expr_type_source_id_4)
		} else {
			t.pending_stmts = outer_pending
			for stmt in prelude {
				t.pending_stmts << stmt
			}
			return new_expr
		}
	}

	if is_void {
		prelude << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type_source_id_4)
		opt_ident := t.make_ident(opt_tmp)
		not_ok := t.make_prefix(.not, t.make_selector(opt_ident, 'ok', flat.source_type_bool))
		else_block := t.make_or_else_block(node.value, t.lower_or_body_to_stmts(body_id, '', u32(0),
			node.value, opt_tmp))
		if_stmt := t.make_if(not_ok, else_block, t.make_empty())
		t.pending_stmts = outer_pending
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		t.pending_stmts << if_stmt
		return t.make_int_literal(0)
	}

	// `x?` on a plain option variable must not go through a staging copy: the
	// unwrap is an lvalue, so `x?.pop()` has to mutate `x`'s own payload rather
	// than a temp that is thrown away right after. A propagation unwrap always
	// diverges on the `none` branch, so there is no value slot to fill and the
	// payload can simply be read in place once `ok` has been checked.
	mut inplace_name := ''
	if node.value in ['?', '!'] && prelude.len == 0 {
		new_expr_node := t.a.nodes[int(new_expr)]
		if new_expr_node.kind == .ident && new_expr_node.value.len > 0
			&& t.is_optional_type_name(t.var_type(new_expr_node.value)) {
			inplace_name = new_expr_node.value
		}
	}
	if inplace_name.len == 0 {
		prelude << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type_source_id_4)
	}
	storage_value_type0 := (if t.is_fixed_array_type(value_type_source_id_9) {
		t.resolved_fixed_array_canonical_type(value_type_source_id_9)
	} else {
		t.shared_alias_storage_type(value_type_source_id_9)
	})
	qualified_storage_type_source_id := t.qualify_or_storage_type(storage_value_type0)
	storage_normalized := t.normalize_type_in_module(qualified_storage_type_source_id, t.cur_module)
	mut storage_value_type := qualified_storage_type_source_id
	if t.is_optional_type_name(t.cur_fn_ret_type) {
		return_value_type_source_id := t.optional_base_type(t.qualify_optional_type(t.cur_fn_ret_type))
		if return_value_type_source_id != storage_value_type0
			&& t.normalize_type_in_module(return_value_type_source_id, t.cur_module) == storage_normalized {
			storage_value_type = return_value_type_source_id
		}
	}
	for active_arg in t.active_specialization_args.ids {
		if active_arg != storage_value_type0
			&& t.normalize_type_in_module(active_arg, t.cur_module) == storage_normalized {
			storage_value_type = active_arg
			break
		}
	}
	if inplace_name.len > 0 {
		not_ok := t.make_prefix(.not, t.make_selector(t.make_ident(inplace_name), 'ok',
			flat.source_type_bool))
		inplace_else := t.make_or_else_block(node.value, t.lower_or_body_to_stmts(body_id,
			'', u32(0), node.value, inplace_name))
		t.pending_stmts = outer_pending
		t.pending_stmts << t.make_if(not_ok, inplace_else, t.make_empty())
		return t.make_selector(t.make_ident(inplace_name), 'value', storage_value_type)
	}
	prelude << t.make_staging_value_decl(val_tmp, storage_value_type)

	opt_ident := t.make_ident(opt_tmp)
	ok_cond := t.make_selector(opt_ident, 'ok', flat.source_type_bool)
	value_expr := t.make_selector(t.make_ident(opt_tmp), 'value', storage_value_type)
	then_value := t.clone_borrowed_storage_projection(expr_id, value_expr, storage_value_type)
	mut then_stmts := []flat.NodeId{}
	t.drain_pending(mut then_stmts)
	then_assign := t.make_assign(t.make_ident(val_tmp), then_value)
	then_stmts << then_assign
	then_block := t.make_block_skip_scope_drops(then_stmts)
	else_stmts := if multi_types := t.multi_return_types_for_expr(expr_id, 0) {
		t.lower_or_body_to_multi_return_stmts(body_id, val_tmp, storage_value_type, multi_types,
			node.value, opt_tmp)
	} else {
		t.lower_or_body_to_stmts(body_id, val_tmp, storage_value_type, node.value, opt_tmp)
	}
	else_block := t.make_or_else_block(node.value, else_stmts)
	if_stmt := t.make_if(ok_cond, then_block, else_block)
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << if_stmt
	return t.make_ident(val_tmp)
}

fn (t &Transformer) qualify_or_storage_type(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	qualified := t.qualify_or_storage_type_bytes(typ)
	return if qualified == typ { typ_id } else { flat.source_type_id(qualified) }
}

// qualify_or_storage_type_bytes qualifies spelling bytes inside the numeric query boundary.
fn (t &Transformer) qualify_or_storage_type_bytes(typ string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return clean
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
		if clean.starts_with(prefix) {
			return prefix + t.qualify_or_storage_type_bytes(clean[prefix.len..])
		}
	}
	if clean.starts_with('map[') {
		key_type_source_id_2, value_type_source_id_6 := t.map_type_parts(flat.source_type_id(clean))
		key_type := flat.source_type_name(key_type_source_id_2)
		value_type := flat.source_type_name(value_type_source_id_6)
		if key_type_source_id_2 != 0 && value_type_source_id_6 != 0 {
			return 'map[${t.qualify_or_storage_type_bytes(key_type)}]${t.qualify_or_storage_type_bytes(value_type)}'
		}
	}
	if clean.ends_with(']') {
		base, args, is_generic_app := generic_app_parts(clean)
		if is_generic_app {
			mut qualified_args := []string{cap: args.len}
			for arg_type in args {
				qualified_args << t.qualify_or_storage_type_bytes(arg_type)
			}
			mut qualified_base := flat.source_type_name(t.qualify_type(flat.source_type_id(base)))
			if qualified_base == base && !t.bare_struct_name_is_local_to_current_module(flat.source_type_id(base)) {
				if imported_base_source_id := t.unique_qualified_struct_for_short(flat.source_type_id(base)) {
					imported_base := flat.source_type_name(imported_base_source_id)

					qualified_base = imported_base
				}
			}
			return '${qualified_base}[${qualified_args.join(', ')}]'
		}
	}
	return clean
}

fn (mut t Transformer) preserve_or_expr_for_codegen(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_expr := t.transform_expr(expr_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}

	err_scope := t.enter_implicit_err_scope(flat.source_type_shape(t.node_type(new_expr)).has(.result_type))
	new_body := t.transform_or_body_for_codegen(body_id)
	t.leave_implicit_err_scope(err_scope)

	start := t.a.children.len
	t.a.children << new_expr
	t.a.children << new_body
	return t.a.add_node(flat.Node{
		kind:           .or_expr
		op:             node.op
		children_start: start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_or_body_for_codegen(body_id flat.NodeId) flat.NodeId {
	heaped_state := t.save_heaped_local_state()
	defer { t.restore_heaped_local_state(heaped_state) }
	if int(body_id) < 0 || int(body_id) >= t.a.nodes.len {
		return body_id
	}
	body := t.a.nodes[int(body_id)]
	if body.kind == .block {
		return t.make_block(t.transform_scope_stmts(t.a.children_of(&body)))
	}
	if t.is_stmt_kind_id(int(body.kind)) {
		return t.make_block(t.transform_stmt(body_id))
	}
	new_expr := t.transform_expr(body_id)
	return t.make_block([t.make_expr_stmt(new_expr)])
}

fn (mut t Transformer) transform_nested_optional_or_expr(expr_id flat.NodeId, or_node flat.Node) ?flat.NodeId {
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len || or_node.children_count < 2 {
		return none
	}
	result_type_source_id := t.nested_optional_value_type(expr_id, or_node.typ)

	if result_type_source_id == 0 || result_type_source_id == flat.source_type_void {
		return none
	}
	result_name := t.new_temp('or_val')
	ok_name := t.new_temp('or_ok')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	part := t.lower_nested_optional_expr_part(expr_id, or_node, result_name, result_type_source_id, ok_name)
	if !part.changed {
		t.pending_stmts = outer_pending
		return none
	}
	mut stmts := []flat.NodeId{}
	stmts << t.make_decl_assign_typed(result_name, t.zero_value_for_type(result_type_source_id), result_type_source_id)
	stmts << t.make_decl_assign_typed(ok_name, t.make_bool_literal(true), flat.source_type_bool)
	for stmt in part.stmts {
		stmts << stmt
	}
	value := t.transform_expr_for_type(part.expr, result_type_source_id)
	mut final_stmts := []flat.NodeId{}
	t.drain_pending(mut final_stmts)
	final_stmts << t.make_assign(t.make_ident(result_name), value)
	stmts << t.make_guarded_nested_optional_step(ok_name, final_stmts)
	t.pending_stmts = outer_pending
	for stmt in stmts {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (mut t Transformer) lower_nested_optional_expr_part(expr_id flat.NodeId, or_node flat.Node, result_name string, result_type_id u32, ok_name string) NestedOptionalPart {
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
		return NestedOptionalPart{
			expr: expr_id
		}
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .infix && expr.children_count >= 2 {
		lhs_id := t.a.child(&expr, 0)
		rhs_id := t.a.child(&expr, 1)
		lhs_part0 := t.lower_nested_optional_expr_part(lhs_id, or_node, result_name, result_type_id,
			ok_name)
		rhs_part := t.lower_nested_optional_expr_part(rhs_id, or_node, result_name, result_type_id,
			ok_name)
		if !lhs_part0.changed && !rhs_part.changed {
			return NestedOptionalPart{
				expr: expr_id
			}
		}
		if expr.op in [.logical_and, .logical_or] && rhs_part.changed {
			return t.lower_nested_optional_logical_infix_part(expr, expr_id, lhs_id, lhs_part0,
				rhs_part, ok_name)
		}
		mut lhs_part := lhs_part0
		mut stmts := []flat.NodeId{}
		for stmt in lhs_part.stmts {
			stmts << stmt
		}
		if rhs_part.changed {
			lhs_part = t.stabilize_nested_optional_part(lhs_part.expr,
				t.nested_optional_value_type(lhs_id, u32(0)), ok_name)
			for stmt in lhs_part.stmts {
				stmts << stmt
			}
		}
		for stmt in rhs_part.stmts {
			stmts << stmt
		}
		start := t.a.children.len
		t.a.children << lhs_part.expr
		t.a.children << rhs_part.expr
		return NestedOptionalPart{
			expr:    t.a.add_node(flat.Node{
				kind:           .infix
				op:             expr.op
				pos:            expr.pos
				children_start: start
				children_count: 2
				value:          expr.value
				typ:            t.nested_optional_value_type(expr_id, expr.typ)
			})
			stmts:   stmts
			changed: true
		}
	}
	if expr.kind == .paren && expr.children_count > 0 {
		inner := t.lower_nested_optional_expr_part(t.a.child(&expr, 0), or_node, result_name,
			result_type_id, ok_name)
		if !inner.changed {
			return NestedOptionalPart{
				expr: expr_id
			}
		}
		paren := t.make_paren(inner.expr)
		t.set_node_typ(int(paren), t.nested_optional_value_type(expr_id, expr.typ))
		return NestedOptionalPart{
			expr:    paren
			stmts:   inner.stmts
			changed: true
		}
	}
	if lowered := t.lower_optional_leaf_with_outer_or(expr_id, or_node, result_name, result_type_id,
		ok_name)
	{
		return lowered
	}
	return NestedOptionalPart{
		expr: expr_id
	}
}

fn (mut t Transformer) lower_nested_optional_logical_infix_part(expr flat.Node, expr_id flat.NodeId, lhs_id flat.NodeId, lhs_part0 NestedOptionalPart, rhs_part NestedOptionalPart, ok_name string) NestedOptionalPart {
	logic_type0_source_id := t.nested_optional_value_type(expr_id, expr.typ)
	logic_type0_source_id_2 := logic_type0_source_id

	logic_type := (if logic_type0_source_id != 0 {
		logic_type0_source_id_2
	} else {
		flat.source_type_bool
	})
	logic_name := t.new_temp('or_logic')
	mut stmts := []flat.NodeId{}
	stmts << t.make_decl_assign_typed(logic_name, t.zero_value_for_type(logic_type), logic_type)
	mut lhs_part := lhs_part0
	for stmt in lhs_part.stmts {
		stmts << stmt
	}
	lhs_part = t.stabilize_nested_optional_part(lhs_part.expr,
		t.nested_optional_value_type(lhs_id, u32(0)), ok_name)
	for stmt in lhs_part.stmts {
		stmts << stmt
	}
	mut rhs_stmts := []flat.NodeId{}
	for stmt in rhs_part.stmts {
		rhs_stmts << stmt
	}
	rhs_stmts << t.make_guarded_nested_optional_step(ok_name, t.nested_optional_assign_stmts(logic_name,
		rhs_part.expr, logic_type))
	cond := if expr.op == .logical_or {
		t.make_prefix(.not, lhs_part.expr)
	} else {
		lhs_part.expr
	}
	lhs_assign := t.make_assign(t.make_ident(logic_name), lhs_part.expr)
	logical_if := t.make_if(cond, t.make_block(rhs_stmts), t.make_block([lhs_assign]))
	stmts << t.make_guarded_nested_optional_step(ok_name, [logical_if])
	return NestedOptionalPart{
		expr:    t.make_ident(logic_name)
		stmts:   stmts
		changed: true
	}
}

fn (mut t Transformer) lower_optional_leaf_with_outer_or(expr_id flat.NodeId, or_node flat.Node, result_name string, result_type_id u32, ok_name string) ?NestedOptionalPart {
	if or_node.children_count < 2 {
		return none
	}
	source_id := t.nested_optional_leaf_source_id(expr_id)
	expr_type_source_id_2, value_type_source_id_8 := t.or_expr_types(source_id, u32(0))
	expr_type_source_id_5 := expr_type_source_id_2

	value_type_source_id_10 := value_type_source_id_8

	if !t.is_optional_type_name(expr_type_source_id_5) {
		return none
	}
	body_id := t.a.child(&or_node, 1)
	opt_tmp := t.new_temp('or_opt')
	val_tmp := t.new_temp('or_leaf')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	expr_node := t.a.nodes[int(source_id)]
	mut new_expr := t.disabled_optional_call_or_none(source_id, expr_node, expr_type_source_id_5) or {
		t.transform_expr(source_id)
	}
	new_expr = t.optional_source_value_expr(source_id, new_expr, expr_type_source_id_5)
	t.mark_optional_source_expr_type(new_expr, expr_type_source_id_5)
	mut eval_stmts := []flat.NodeId{}
	t.drain_pending(mut eval_stmts)
	t.pending_stmts = outer_pending
	eval_stmts << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type_source_id_5)
	opt_ident := t.make_ident(opt_tmp)
	ok_cond := t.make_selector(opt_ident, 'ok', flat.source_type_bool)
	value_expr := t.make_selector(t.make_ident(opt_tmp), 'value', value_type_source_id_10)
	then_block := t.make_block([t.make_assign(t.make_ident(val_tmp), value_expr)])
	mut else_stmts := t.lower_or_body_to_stmts(body_id, result_name, result_type_id, or_node.value,
		opt_tmp)
	else_stmts << t.make_assign(t.make_ident(ok_name), t.make_bool_literal(false))
	eval_stmts << t.make_if(ok_cond, then_block, t.make_block(else_stmts))
	return NestedOptionalPart{
		expr:    t.make_ident(val_tmp)
		stmts:   [
			t.make_decl_assign_typed(val_tmp, t.zero_value_for_type(value_type_source_id_10), value_type_source_id_10),
			t.make_guarded_nested_optional_step(ok_name, eval_stmts),
		]
		changed: true
	}
}

fn (t &Transformer) nested_optional_leaf_source_id(expr_id flat.NodeId) flat.NodeId {
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
		return expr_id
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .or_expr && expr.value in ['?', '!'] && expr.children_count > 0 {
		return t.a.child(&expr, 0)
	}
	return expr_id
}

fn (mut t Transformer) stabilize_nested_optional_part(expr_id flat.NodeId, typ_id u32, ok_name string) NestedOptionalPart {
	value_type := (if typ_id != u32(0) {
		typ_id
	} else {
		t.node_type(expr_id)
	})
	tmp := t.new_temp('or_part')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := if value_type != 0 {
		t.transform_expr_for_type(expr_id, value_type)
	} else {
		t.transform_expr(expr_id)
	}
	mut stmts := []flat.NodeId{}
	t.drain_pending(mut stmts)
	t.pending_stmts = outer_pending
	stmts << t.make_assign(t.make_ident(tmp), value)
	return NestedOptionalPart{
		expr:  t.make_ident(tmp)
		stmts: [
			t.make_decl_assign_typed(tmp, t.zero_value_for_type(value_type), value_type),
			t.make_guarded_nested_optional_step(ok_name, stmts),
		]
	}
}

fn (mut t Transformer) nested_optional_assign_stmts(target_name string, expr_id flat.NodeId, target_type_id u32) []flat.NodeId {
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := if target_type_id != u32(0) {
		t.transform_expr_for_type(expr_id, target_type_id)
	} else {
		t.transform_expr(expr_id)
	}
	mut stmts := []flat.NodeId{}
	t.drain_pending(mut stmts)
	t.pending_stmts = outer_pending
	stmts << t.make_assign(t.make_ident(target_name), value)
	return stmts
}

fn (mut t Transformer) make_guarded_nested_optional_step(ok_name string, stmts []flat.NodeId) flat.NodeId {
	return t.make_if(t.make_ident(ok_name), t.make_block(stmts), t.make_empty())
}

fn (mut t Transformer) nested_optional_value_type(expr_id flat.NodeId, fallback_id u32) u32 {
	mut typ := t.node_type(expr_id)
	if typ == 0 || flat.source_type_name(typ).contains('unknown') {
		typ = fallback_id
	}
	if flat.source_type_name(typ).all_after_last('.') in ['__v_option', 'Result'] {
		return flat.source_type_void
	}
	if t.is_optional_type_name(typ) {
		typ = t.optional_base_type(typ)
	}
	if flat.source_type_name(typ).all_after_last('.') in ['__v_option', 'Result'] {
		return flat.source_type_void
	}
	return typ
}

fn (t &Transformer) shared_alias_storage_type(typ_id u32) u32 {
	if isnil(t.tc) {
		return typ_id
	}
	clean := t.trimmed_type_id(typ_id)
	if clean == 0 || flat.source_type_shape(clean).has(.pointer) {
		return typ_id
	}
	for candidate in [clean, t.qualify_type(clean)] {
		if target := t.tc.type_aliases.type_id(flat.source_type_name(candidate)) {
			inner := t.shared_alias_inner_type(target) or { continue }
			return flat.source_type_id('&${flat.source_type_name(inner)}')
		}
	}
	if !flat.source_type_shape(clean).has(.dotted) {
		mut found := u32(0)
		// A bare spelling can match an alias declaration's short name.
		for alias in t.type_aliases_with_short_name(flat.source_type_name(clean)) {
			inner := t.shared_alias_inner_type(alias.target) or { continue }
			if found != 0 && found != inner {
				return typ_id
			}
			found = inner
		}
		if found != 0 {
			return flat.source_type_id('&${flat.source_type_name(found)}')
		}
	}
	return typ_id
}

fn (t &Transformer) shared_alias_inner_type(raw_id u32) ?u32 {
	clean := t.trimmed_type_id(raw_id)
	if flat.source_type_shape(clean).has(.shared_type) {
		return t.unshared_type_id(clean)
	}
	return none
}

fn (mut t Transformer) disabled_optional_call_or_none(expr_id flat.NodeId, expr_node flat.Node, expr_type_id u32) ?flat.NodeId {
	if isnil(t.tc) || expr_node.kind != .call || !t.is_optional_type_name(expr_type_id) {
		return none
	}
	if _ := t.json_decode_or_expr_type(expr_id, expr_node) {
		if name := t.tc.resolved_call_name(expr_id) {
			if name in ['json2.decode', 'x.json2.decode'] {
				return none
			}
		}
		return t.make_optional_none(expr_type_id)
	}
	if !t.is_disabled_fn_call(expr_id, expr_node) {
		return none
	}
	return t.make_optional_none(expr_type_id)
}

fn (mut t Transformer) lower_or_expr_to_stmt(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	expr_type_source_id_3, _ := t.or_expr_types(expr_id, node.typ)
	expr_type_source_id_6 := expr_type_source_id_3

	if !t.is_optional_type_name(expr_type_source_id_6) {
		t.pending_stmts << t.make_expr_stmt(t.transform_expr(expr_id))
		return
	}

	opt_tmp := t.new_temp('or_opt')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut new_expr := t.transform_expr(expr_id)
	new_expr = t.optional_source_value_expr(expr_id, new_expr, expr_type_source_id_6)
	t.mark_optional_source_expr_type(new_expr, expr_type_source_id_6)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(opt_tmp, new_expr, expr_type_source_id_6)

	opt_ident := t.make_ident(opt_tmp)
	not_ok := t.make_prefix(.not, t.make_selector(opt_ident, 'ok', flat.source_type_bool))
	else_block := t.make_or_else_block(node.value, t.lower_or_body_to_stmts(body_id, '', u32(0),
		node.value, opt_tmp))
	if_stmt := t.make_if(not_ok, else_block, t.make_empty())

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << if_stmt
}

fn (mut t Transformer) make_or_lowered_if(mode string, cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	if mode in ['!', '?'] {
		// Propagation has no source-level fallback scope. The ownership checker
		// records its early-return cleanup separately, so this synthetic branch
		// must not consume the next lexical scope's destructor slot.
		return t.make_if_with_skip_ownership_drops(cond, then_block, else_block)
	}
	return t.make_if(cond, then_block, else_block)
}

fn (mut t Transformer) mark_optional_source_expr_type(expr flat.NodeId, typ_id u32) {
	if !t.is_optional_type_name(typ_id) || int(expr) < 0 || int(expr) >= t.a.nodes.len {
		return
	}
	t.set_node_typ(int(expr), typ_id)
}

fn (mut t Transformer) optional_source_value_expr(source_id flat.NodeId, expr flat.NodeId, typ_id u32) flat.NodeId {
	if int(source_id) < 0 || int(source_id) >= t.a.nodes.len || !t.is_optional_type_name(typ_id) {
		return expr
	}
	source := t.a.nodes[int(source_id)]
	if source.kind == .ident && t.pointer_value_rvalues[source.value] {
		// Ordinary reads of pointer-backed bindings are already dereferenced by
		// transform_ident_expr; do not add a second optional-storage load.
		return expr
	}
	raw_type := match source.kind {
		.ident {
			t.raw_var_type(source.value)
		}
		.selector {
			t.raw_selector_field_type(source_id) or { return expr }
		}
		else {
			return expr
		}
	}
	if !flat.source_type_shape(raw_type).has(.pointer) || !t.is_optional_type_name(t.trim_pointer_type(raw_type)) {
		return expr
	}
	value := t.make_prefix(.mul, expr)
	t.set_node_typ(int(value), typ_id)
	return value
}

fn (mut t Transformer) result_error_expr(source flat.NodeId) flat.NodeId {
	if !flat.source_type_shape(t.normalized_node_type(source)).has(.result_type) {
		return flat.empty_node
	}
	return t.make_selector(source, 'err', flat.source_type_id('IError'))
}

fn (mut t Transformer) append_implicit_err_decl(mut stmts []flat.NodeId, err_expr flat.NodeId) {
	if int(err_expr) < 0 || !t.has_ierror_interface() {
		return
	}
	stmts << t.make_guard_value_decls('err', err_expr, flat.source_type_id('IError'))
	i := t.var_type_index('err')
	t.clear_node_type_memo()
	t.var_types[i] = VarTypeBinding{
		...t.var_types[i]
		is_implicit_err: true
	}
}

// lower_or_body_to_stmts converts lower or body to stmts data for transform.
fn (mut t Transformer) lower_or_body_to_stmts(body_id flat.NodeId, target_name string, target_type_id u32, mode string, err_source string) []flat.NodeId {
	err_expr := if err_source != '' {
		t.result_error_expr(t.make_ident(err_source))
	} else {
		flat.empty_node
	}
	return t.lower_or_body_to_stmts_with_err_expr(body_id, target_name, target_type_id, mode, err_expr)
}

fn (mut t Transformer) lower_or_body_to_multi_return_stmts(body_id flat.NodeId, target_name string, target_type_id u32, field_types []types.Type, mode string, err_source string) []flat.NodeId {
	err_expr := if err_source != '' {
		t.result_error_expr(t.make_ident(err_source))
	} else {
		flat.empty_node
	}
	return t.lower_or_body_to_multi_return_stmts_with_err_expr(body_id, target_name, target_type_id,
		field_types, mode, err_expr)
}

fn (mut t Transformer) lower_or_body_to_multi_return_stmts_with_err_expr(body_id flat.NodeId, target_name string, target_type_id u32, field_types []types.Type, mode string, err_expr flat.NodeId) []flat.NodeId {
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return [t.make_none_return_stmt_with_err_expr(err_expr)]
		}
		return t.make_propagation_panic_stmts(mode, err_expr, body_id)
	}
	if int(body_id) < 0 || target_name == '' || field_types.len == 0 {
		return t.lower_or_body_to_stmts_with_err_expr(body_id, target_name, target_type_id, mode,
			err_expr)
	}
	body := t.a.nodes[int(body_id)]
	err_scope := t.enter_implicit_err_scope(int(err_expr) >= 0)
	mut result := []flat.NodeId{}
	t.append_implicit_err_decl(mut result, err_expr)
	if t.stmt_tail_exits(body_id) {
		t.leave_implicit_err_scope(err_scope)
		return t.lower_or_body_to_stmts_with_err_expr(body_id, '', u32(0), mode, err_expr)
	}
	if lowered := t.lower_or_multi_return_tail(body_id, target_name, target_type_id, field_types) {
		for stmt in lowered {
			result << stmt
		}
		t.leave_implicit_err_scope(err_scope)
		return result
	}
	_ = body
	t.leave_implicit_err_scope(err_scope)
	return t.lower_or_body_to_stmts_with_err_expr(body_id, target_name, target_type_id, mode, err_expr)
}

fn (t &Transformer) or_tail_can_supply_multi_return(id flat.NodeId, expected_count int) bool {
	if int(id) < 0 || expected_count <= 0 {
		return false
	}
	if t.stmt_tail_exits(id) {
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
		if t.stmt_tail_exits(stmt_id) {
			return none
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

fn (mut t Transformer) lower_or_multi_return_tail(id flat.NodeId, target_name string, target_type_id u32, field_types []types.Type) ?[]flat.NodeId {
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
				tail_stmts := t.lower_or_multi_return_tail(last_id, target_name, target_type_id,
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
					return t.lower_or_multi_return_tail(child_id, target_name, target_type_id,
						field_types)
				}
				if _ := t.multi_return_types_for_expr(child_id, field_types.len) {
					return t.lower_or_multi_return_expr(child_id, target_name, target_type_id)
				}
			}
			return none
		}
		else {
			if t.is_stmt_kind(node.kind) {
				return none
			}
			if _ := t.multi_return_types_for_expr(id, field_types.len) {
				return t.lower_or_multi_return_expr(id, target_name, target_type_id)
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
		value := t.transform_if_branch_value(value_id, flat.source_type_id(field_type))
		t.drain_pending(mut result)
		field := t.make_selector(t.make_ident(target_name), 'arg${i}', flat.source_type_id(field_type))
		result << t.make_assign(field, value)
	}
	return result
}

fn (mut t Transformer) lower_or_multi_return_expr(expr_id flat.NodeId, target_name string, target_type_id u32) []flat.NodeId {
	value := t.transform_if_branch_value(expr_id, target_type_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_assign(t.make_ident(target_name), value)
	return result
}

fn (mut t Transformer) lower_or_body_to_stmts_with_err_expr(body_id flat.NodeId, target_name string, target_type_id u32, mode string, err_expr flat.NodeId) []flat.NodeId {
	target_type := flat.source_type_name(target_type_id)
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return [t.make_none_return_stmt_with_err_expr(err_expr)]
		}
		return t.make_propagation_panic_stmts(mode, err_expr, body_id)
	}
	if int(body_id) < 0 {
		return []flat.NodeId{}
	}
	body := t.a.nodes[int(body_id)]
	if body.kind != .block {
		if body.kind == .comptime_if && target_name != '' {
			return [t.lower_or_comptime_if_value(body, target_name, target_type_id)]
		}
		if body.kind == .none_expr && t.is_optional_type_name(t.cur_fn_ret_type) {
			return [t.make_none_return_stmt()]
		}
		if body.kind == .call && t.is_noreturn_call(body_id) {
			value := t.transform_expr(body_id)
			mut result := []flat.NodeId{}
			t.drain_pending(mut result)
			result << t.make_expr_stmt(value)
			return result
		}
		if target_name == '' {
			value := t.transform_expr(body_id)
			mut result := []flat.NodeId{}
			t.drain_pending(mut result)
			if t.node_type(body_id) != flat.source_type_void {
				result << t.make_expr_stmt(value)
			}
			return result
		}
		value := t.transform_if_branch_value(body_id, target_type_id)
		mut result := []flat.NodeId{}
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return result
	}
	mut result := []flat.NodeId{}
	if body.children_count == 0 {
		return result
	}
	// The or-body is its own scope: `err` (and any locals it declares) are bound only
	// for its lowering and restored afterwards, so the previous binding (e.g. an outer
	// `err := 1`) survives and a subsequent `${err}` is not mis-typed as `IError`.
	// Mirrors transform_if_guard_else_block.
	err_scope := t.enter_implicit_err_scope(int(err_expr) >= 0)
	t.append_implicit_err_decl(mut result, err_expr)
	mut i := 0
	for i < int(body.children_count) {
		child_id := t.a.child(&body, i)
		child := t.a.nodes[int(child_id)]
		// Keep the label paired with its loop, as in transform_stmts, so the
		// lowering emits the matching labelled break and continue targets.
		if child.kind == .label_stmt && i + 1 < int(body.children_count) {
			loop_id := t.a.child(&body, i + 1)
			loop_node := t.a.nodes[int(loop_id)]
			if loop_node.kind in [.for_stmt, .for_in_stmt] {
				expanded := t.transform_labeled_loop(child.value, loop_id, loop_node)
				t.drain_pending(mut result)
				result << expanded
				i += 2
				continue
			}
			if t.is_multi_init_for_block(loop_node) {
				expanded := t.transform_labeled_multi_init_loop(child.value, loop_id, loop_node)
				t.drain_pending(mut result)
				result << expanded
				i += 2
				continue
			}
		}
		// Or blocks lower their statements directly instead of through
		// `transform_stmts`. Capture narrowing facts before an assert/guard is
		// rewritten to a runtime check, then expose them to later statements.
		post_if_smartcasts := t.post_if_exit_smartcasts(child_id)
		post_assert_smartcasts := t.post_assert_smartcasts(child_id)
		is_last := i == body.children_count - 1
		if is_last && child.kind == .expr_stmt && child.children_count > 0 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .comptime_if && target_name != '' {
				result << t.lower_or_comptime_if_value(inner, target_name, target_type_id)
			} else if t.stmt_tail_exits(child_id) {
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
			} else if t.is_optional_type_name(t.cur_fn_ret_type)
				&& t.return_expr_is_propagated_err(inner_id, target_type_id) {
				result << t.make_none_return_stmt_with_err_expr(t.transform_expr(inner_id))
			} else if inner.kind == .block && target_name != '' {
				result << t.if_value_branch_block(inner_id, target_name, target_type_id)
			} else if t.node_type(inner_id) == flat.source_type_void && target_name == '' {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else if target_name == '' {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else {
				value := t.transform_if_branch_value(inner_id, target_type_id)
				t.drain_pending(mut result)
				result << t.make_assign(t.make_ident(target_name), value)
			}
		} else if is_last && target_name != '' && child.kind == .block {
			result << t.if_value_branch_block(child_id, target_name, target_type_id)
		} else if is_last && target_name != '' && child.kind == .comptime_if {
			result << t.lower_or_comptime_if_value(child, target_name, target_type_id)
		} else if is_last && target_name != '' && child.kind in [.if_expr, .match_stmt] {
			value := t.transform_if_branch_value(child_id, target_type_id)
			t.drain_pending(mut result)
			result << t.make_assign(t.make_ident(target_name), value)
		} else if is_last && target_name != '' && !t.is_stmt_kind(child.kind) {
			value := t.transform_if_branch_value(child_id, target_type_id)
			t.drain_pending(mut result)
			result << t.make_assign(t.make_ident(target_name), value)
		} else {
			expanded := t.transform_stmt(child_id)
			t.drain_pending(mut result)
			for eid in expanded {
				result << eid
			}
		}
		for info in post_if_smartcasts {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		for info in post_assert_smartcasts {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		i++
	}
	_ = target_type
	t.leave_implicit_err_scope(err_scope)
	return result
}

fn (mut t Transformer) lower_or_comptime_if_value(node flat.Node, target_name string, target_type_id u32) flat.NodeId {
	if take_then := t.comptime_type_condition_value(node.value) {
		branch_index := if take_then { 0 } else { 1 }
		for i in 0 .. node.children_count {
			if i != branch_index {
				t.ignore_comptime_for_subtree(t.a.child(&node, i))
			}
		}
		if branch_index >= node.children_count {
			return t.make_block([]flat.NodeId{})
		}
		return t.if_value_branch_block(t.a.child(&node, branch_index), target_name,
			target_type_id)
	}
	mut branches := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		branches << t.if_value_branch_block(t.a.child(&node, i), target_name, target_type_id)
	}
	return t.make_comptime_if(node.value, branches)
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
	if resolved := t.tc.resolved_call_name(id) {
		if t.tc.resolved_call_never_returns(id) || resolved in t.a.noreturn_fns {
			return true
		}
		resolved_module := resolved.all_before_last('.')
		for candidate, _ in t.a.noreturn_fns {
			if (candidate.contains('.') && resolved.ends_with('.${candidate}'))
				|| (!candidate.contains('.') && resolved_module in ['main', 'builtin']
					&& resolved == '${resolved_module}.${candidate}') {
				return true
			}
		}
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .ident && callee.value in ['panic', 'exit', 'v_panic'] {
		return (t.var_type(callee.value)) == 0
			&& !t.tc.module_declares_fn(t.cur_module, callee.value)
	}
	name := t.resolve_call_name(node)
	return name in ['panic', 'exit', 'os.exit', 'C.exit', 'builtin.panic']
		|| name in t.a.noreturn_fns
}

// base_type_text resolves the indexed source type spelling.
@[inline]
fn (value &ArrayIndexInfo) base_type_text() string {
	return flat.source_type_name(value.base_type)
}

// optional_type_text resolves the indexed source type spelling.
@[inline]
fn (value &ArrayIndexInfo) optional_type_text() string {
	return flat.source_type_name(value.optional_type)
}

// value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &ArrayIndexInfo) value_type_text() string {
	return flat.source_type_name(value.value_type)
}

// value_type_text resolves the indexed source type spelling.
@[inline]
fn (value &ChannelReceiveInfo) value_type_text() string {
	return flat.source_type_name(value.value_type)
}

// enum_type_text resolves the indexed source type spelling.
@[inline]
fn (value &EnumFromStringInfo) enum_type_text() string {
	return flat.source_type_name(value.enum_type)
}
