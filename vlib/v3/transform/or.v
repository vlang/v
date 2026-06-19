module transform

import v3.flat
import v3.types

fn (t &Transformer) optional_base_type(typ string) string {
	if typ.len > 1 && (typ[0] == `?` || typ[0] == `!`) {
		return typ[1..]
	}
	return typ
}

fn (t &Transformer) or_expr_types(expr_id flat.NodeId, fallback_type string) (string, string) {
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(expr_id) {
			if typ is types.OptionType {
				base_name := t.value_type_name(typ.base_type)
				source_name := if base_name == 'int' && typ.base_type is types.Void {
					'?void'
				} else {
					'?${base_name}'
				}
				return source_name, base_name
			}
			if typ is types.ResultType {
				base_name := t.value_type_name(typ.base_type)
				source_name := if base_name == 'int' && typ.base_type is types.Void {
					'!void'
				} else {
					'!${base_name}'
				}
				return source_name, base_name
			}
		}
	}
	mut expr_type := t.node_type(expr_id)
	if expr_type.len == 0 || expr_type == 'Optional' {
		expr_type = fallback_type
	}
	base_type := t.optional_base_type(expr_type)
	mut value_type := if base_type.len > 0 { base_type } else { fallback_type }
	if value_type.len == 0 || value_type == 'void' || value_type == '!' || value_type == '?' {
		value_type = 'int'
	}
	return expr_type, value_type
}

fn (t &Transformer) value_type_name(typ types.Type) string {
	if typ is types.Void {
		return 'int'
	}
	return typ.name()
}

fn (t &Transformer) is_optional_type_name(typ string) bool {
	return typ.len > 0 && (typ[0] == `?` || typ[0] == `!`)
}

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

fn (t &Transformer) qualify_type(name string) string {
	if name.contains('.') || name.len == 0 {
		return name
	}
	if !isnil(t.tc) {
		for key, _ in t.tc.sum_types {
			if key.ends_with('.${name}') {
				return key
			}
		}
		for key, _ in t.tc.structs {
			if key.ends_with('.${name}') {
				return key
			}
		}
	}
	return name
}

fn (mut t Transformer) make_decl_assign_typed(name string, rhs flat.NodeId, typ string) flat.NodeId {
	decl := t.make_decl_assign(name, rhs)
	if typ.len > 0 {
		t.a.nodes[int(decl)].typ = typ
		t.set_var_type(name, typ)
	}
	return decl
}

fn (mut t Transformer) zero_value_for_type(typ string) flat.NodeId {
	mut clean := typ
	if clean.starts_with('&') {
		return t.a.add(.nil_literal)
	}
	if clean.len > 1 && (clean[0] == `?` || clean[0] == `!`) {
		clean = clean[1..]
	}
	if clean.starts_with('[]') {
		return t.make_array_init(clean[2..])
	}
	if clean.starts_with('map[') {
		return t.make_map_init(clean)
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

fn (mut t Transformer) make_panic_stmt(message string) flat.NodeId {
	call := t.make_call('panic', arr1(t.make_string_literal(message)))
	return t.make_expr_stmt(call)
}

fn (mut t Transformer) make_none_return_stmt() flat.NodeId {
	return t.make_return(t.a.add(.none_expr), t.cur_fn_ret_type)
}

fn (mut t Transformer) lower_or_expr_to_temp(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	expr_type, value_type := t.or_expr_types(expr_id, node.typ)
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
		else_block := t.make_block(t.lower_or_body_to_stmts(body_id, '', '', node.value))
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
	else_block := t.make_block(t.lower_or_body_to_stmts(body_id, val_tmp, value_type, node.value))
	if_stmt := t.make_if(ok_cond, then_block, else_block)
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << if_stmt
	return t.make_ident(val_tmp)
}

fn (mut t Transformer) lower_or_body_to_stmts(body_id flat.NodeId, target_name string, target_type string, mode string) []flat.NodeId {
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return arr1(t.make_none_return_stmt())
		}
		return arr1(t.make_panic_stmt('option/result propagation failed'))
	}
	if int(body_id) < 0 {
		return []flat.NodeId{}
	}
	body := t.a.nodes[int(body_id)]
	if body.kind != .block {
		return arr1(t.make_assign(t.make_ident(target_name), t.transform_expr(body_id)))
	}
	mut result := []flat.NodeId{}
	if body.children_count == 0 {
		return result
	}
	t.set_var_type('err', 'IError')
	result << t.make_decl_assign_typed('err', t.make_struct_init('IError'), 'IError')
	for i in 0 .. body.children_count {
		child_id := t.a.child(&body, i)
		child := t.a.nodes[int(child_id)]
		is_last := i == body.children_count - 1
		if is_last && child.kind == .expr_stmt && child.children_count > 0 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .call && t.is_noreturn_call(inner) {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else if t.node_type(inner_id) == 'void' {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else {
				value := t.transform_expr(inner_id)
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
	return result
}

fn (t &Transformer) is_noreturn_call(node flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.value in ['panic', 'exit']
}
