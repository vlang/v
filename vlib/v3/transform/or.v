module transform

import v3.flat
import v3.types

// ArrayIndexInfo stores array index info metadata used by transform.
struct ArrayIndexInfo {
	base_id    flat.NodeId
	index_id   flat.NodeId
	base_type  string
	value_type string
}

struct StringSliceInfo {
	base_id  flat.NodeId
	start_id flat.NodeId
	end_id   flat.NodeId
	has_end  bool
}

// EnumFromStringInfo stores enum from string info metadata used by transform.
struct EnumFromStringInfo {
	enum_type string
	fields    []string
	arg_id    flat.NodeId
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
	base_type := t.resolve_expr_type(base_id)
	if !base_type.starts_with('[]') {
		return none
	}
	value_type := t.normalize_type_alias(base_type[2..])
	if value_type.len == 0 {
		return none
	}
	if t.is_optional_type_name(value_type) {
		return none
	}
	return ArrayIndexInfo{
		base_id:    base_id
		index_id:   index_expr_id
		base_type:  base_type
		value_type: value_type
	}
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
	else_block :=
		t.make_block(t.lower_or_body_to_stmts(body_id, val_name, 'string', node.value, ''))
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
	base_expr := t.stable_expr_for_reuse(info.base_id)
	index_name := t.new_temp('arr_idx')
	val_name := t.new_temp('arr_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	index_expr := t.transform_expr(info.index_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(index_name, index_expr, 'int')
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.value_type),
		info.value_type)

	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name),
		t.make_selector(base_expr, 'len', 'int'))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)
	index_value := t.make_index(base_expr, t.make_ident(index_name), info.value_type)
	then_block := t.make_block(arr1(t.make_assign(t.make_ident(val_name), index_value)))
	else_block := t.make_block(t.lower_or_body_to_stmts(body_id, val_name, info.value_type,
		node.value, ''))
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << t.make_if(found_cond, then_block, else_block)
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
	for i := info.fields.len - 1; i >= 0; i-- {
		cond := t.make_call_typed('string__eq', arr2(t.make_ident(str_name),
			t.make_string_literal(info.fields[i])), 'bool')
		assign := t.make_assign(t.make_ident(val_name), t.make_int_literal(i))
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

// or_expr_types supports or expr types handling for Transformer.
fn (mut t Transformer) or_expr_types(expr_id flat.NodeId, fallback_type string) (string, string) {
	expr_node := if int(expr_id) >= 0 && int(expr_id) < t.a.nodes.len {
		t.a.nodes[int(expr_id)]
	} else {
		flat.Node{}
	}
	if !isnil(t.tc) {
		if expr_node.kind == .call {
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
	base_type := t.optional_base_type(expr_type)
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
		t.a.nodes[int(decl)].typ = typ
		decl_node := t.a.nodes[int(decl)]
		lhs_id := t.a.child(&decl_node, 0)
		if int(lhs_id) >= 0 {
			t.a.nodes[int(lhs_id)].typ = typ
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
	expr_type, value_type := t.or_expr_types(expr_id, node.typ)
	if !t.is_optional_type_name(expr_type) {
		return t.transform_expr(expr_id)
	}
	is_void := value_type.len == 0 || value_type == 'void'

	opt_tmp := t.new_temp('or_opt')
	val_tmp := t.new_temp('or_val')

	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_expr := t.transform_expr(expr_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)

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
	else_block := t.make_block(t.lower_or_body_to_stmts(body_id, val_tmp, value_type, node.value,
		opt_tmp))
	if_stmt := t.make_if(ok_cond, then_block, else_block)
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << if_stmt
	return t.make_ident(val_tmp)
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
			} else if t.node_type(inner_id) == 'void' {
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
