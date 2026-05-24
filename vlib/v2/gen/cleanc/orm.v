// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast

fn (mut g Gen) try_gen_orm_create_call(c_name string, call_args []ast.Expr) bool {
	if call_args.len != 3 || !c_name.ends_with('__create') {
		return false
	}
	if !orm_table_can_emit(call_args[1]) || !orm_table_fields_array_can_emit(call_args[2]) {
		return false
	}
	g.sb.write_string('${c_name}(')
	g.gen_call_arg(c_name, 0, call_args[0])
	g.sb.write_string(', ')
	g.write_orm_table_c_expr(call_args[1])
	g.sb.write_string(', ')
	g.write_orm_table_fields_array_c_expr(call_args[2])
	g.sb.write_string(')')
	if c_name != '' {
		g.called_fn_names[c_name] = true
	}
	return true
}

fn (mut g Gen) try_gen_orm_create_call_expr(node ast.CallExpr) bool {
	if !orm_create_call_can_emit(node) {
		return false
	}
	lhs := node.lhs as ast.SelectorExpr
	name := g.orm_create_call_name(node) or { return false }
	call_args := [
		lhs.lhs,
		node.args[0],
		node.args[1],
	]
	return g.try_gen_orm_create_call(name, call_args)
}

fn orm_create_call_can_emit(node ast.CallExpr) bool {
	if node.args.len != 2 || node.lhs !is ast.SelectorExpr {
		return false
	}
	lhs := node.lhs as ast.SelectorExpr
	return lhs.rhs.name == 'create' && orm_table_can_emit(node.args[0])
		&& orm_table_fields_array_can_emit(node.args[1])
}

fn (mut g Gen) orm_create_call_name(node ast.CallExpr) ?string {
	if node.lhs !is ast.SelectorExpr {
		return none
	}
	lhs := node.lhs as ast.SelectorExpr
	method_name := sanitize_fn_ident(lhs.rhs.name)
	mut name := g.resolve_call_name(node.lhs, node.args.len)
	if name != '' {
		return name
	}
	mut base_type := g.method_receiver_base_type(lhs.lhs)
	if base_type == '' {
		base_type = g.get_expr_type(lhs.lhs).trim_space().trim_right('*')
	}
	if base_type == '' {
		base_type = g.receiver_local_base_type(lhs.lhs)
	}
	if name == '' && base_type != '' {
		if embedded := g.resolve_method_on_embedded_receiver(base_type, method_name) {
			name = embedded.method_c_name
		}
	}
	if name == '' && base_type != '' {
		name = '${base_type}__${method_name}'
	}
	if name == '' {
		return none
	}
	return name
}

fn (mut g Gen) orm_create_call_result_type(node ast.CallExpr) ?string {
	if !orm_create_call_can_emit(node) {
		return none
	}
	name := g.orm_create_call_name(node) or { return none }
	if ret := g.fn_return_types[name] {
		return ret
	}
	return none
}

fn orm_table_can_emit(expr ast.Expr) bool {
	init := orm_init_expr(expr) or { return false }
	name_expr := orm_init_field_value(init, 'name') or { return false }
	attrs_expr := orm_init_field_value(init, 'attrs') or { return false }
	return orm_string_value(name_expr) != none && orm_vattribute_array_can_emit(attrs_expr)
}

fn orm_table_fields_array_can_emit(expr ast.Expr) bool {
	arr := orm_array_init_expr(expr) or { return false }
	for field_expr in arr.exprs {
		if !orm_table_field_can_emit(field_expr) {
			return false
		}
	}
	return true
}

fn orm_table_field_can_emit(expr ast.Expr) bool {
	init := orm_init_expr(expr) or { return false }
	name_expr := orm_init_field_value(init, 'name') or { return false }
	typ_expr := orm_init_field_value(init, 'typ') or { return false }
	nullable_expr := orm_init_field_value(init, 'nullable') or { return false }
	default_val_expr := orm_init_field_value(init, 'default_val') or { return false }
	attrs_expr := orm_init_field_value(init, 'attrs') or { return false }
	is_arr_expr := orm_init_field_value(init, 'is_arr') or { return false }
	return orm_string_value(name_expr) != none && orm_number_c_expr(typ_expr) != none
		&& orm_bool_c_expr(nullable_expr) != none && orm_string_value(default_val_expr) != none
		&& orm_vattribute_array_can_emit(attrs_expr) && orm_bool_c_expr(is_arr_expr) != none
}

fn orm_vattribute_array_can_emit(expr ast.Expr) bool {
	arr := orm_array_init_expr(expr) or { return false }
	for attr_expr in arr.exprs {
		if !orm_vattribute_can_emit(attr_expr) {
			return false
		}
	}
	return true
}

fn orm_vattribute_can_emit(expr ast.Expr) bool {
	init := orm_init_expr(expr) or { return false }
	name_expr := orm_init_field_value(init, 'name') or { return false }
	has_arg_expr := orm_init_field_value(init, 'has_arg') or { return false }
	arg_expr := orm_init_field_value(init, 'arg') or { return false }
	kind_expr := orm_init_field_value(init, 'kind') or { return false }
	return orm_string_value(name_expr) != none && orm_bool_c_expr(has_arg_expr) != none
		&& orm_string_value(arg_expr) != none && orm_ident_c_expr(kind_expr) != none
}

fn (mut g Gen) write_orm_table_c_expr(expr ast.Expr) {
	init := orm_init_expr(expr) or { return }
	name_expr := orm_init_field_value(init, 'name') or { return }
	attrs_expr := orm_init_field_value(init, 'attrs') or { return }
	g.sb.write_string('((orm__Table){.name = ')
	g.write_orm_v_string_c_expr(name_expr)
	g.sb.write_string(',.attrs = ')
	g.write_orm_vattribute_array_c_expr(attrs_expr)
	g.sb.write_string('})')
}

fn (mut g Gen) write_orm_table_fields_array_c_expr(expr ast.Expr) {
	arr := orm_array_init_expr(expr) or { return }
	g.sb.write_string('new_array_from_c_array(${arr.exprs.len}, ${arr.exprs.len}, sizeof(orm__TableField), ')
	if arr.exprs.len == 0 {
		g.sb.write_string('&(array){0})')
		return
	}
	g.sb.write_string('&(orm__TableField[${arr.exprs.len}]){')
	for i, field_expr in arr.exprs {
		if i > 0 {
			g.sb.write_string(', ')
		}
		g.write_orm_table_field_c_expr(field_expr)
	}
	g.sb.write_string('})')
}

fn (mut g Gen) write_orm_table_field_c_expr(expr ast.Expr) {
	init := orm_init_expr(expr) or { return }
	name_expr := orm_init_field_value(init, 'name') or { return }
	typ_expr := orm_init_field_value(init, 'typ') or { return }
	nullable_expr := orm_init_field_value(init, 'nullable') or { return }
	default_val_expr := orm_init_field_value(init, 'default_val') or { return }
	attrs_expr := orm_init_field_value(init, 'attrs') or { return }
	is_arr_expr := orm_init_field_value(init, 'is_arr') or { return }
	g.sb.write_string('((orm__TableField){.name = ')
	g.write_orm_v_string_c_expr(name_expr)
	g.sb.write_string(',.typ = ')
	g.sb.write_string(orm_number_c_expr(typ_expr) or { return })
	g.sb.write_string(',.nullable = ')
	g.sb.write_string(orm_bool_c_expr(nullable_expr) or { return })
	g.sb.write_string(',.default_val = ')
	g.write_orm_v_string_c_expr(default_val_expr)
	g.sb.write_string(',.attrs = ')
	g.write_orm_vattribute_array_c_expr(attrs_expr)
	g.sb.write_string(',.is_arr = ')
	g.sb.write_string(orm_bool_c_expr(is_arr_expr) or { return })
	g.sb.write_string('})')
}

fn (mut g Gen) write_orm_vattribute_array_c_expr(expr ast.Expr) {
	arr := orm_array_init_expr(expr) or { return }
	if arr.exprs.len == 0 {
		g.sb.write_string('new_array_from_c_array(0, 0, sizeof(VAttribute), &(array){0})')
		return
	}
	g.sb.write_string('new_array_from_c_array(${arr.exprs.len}, ${arr.exprs.len}, sizeof(VAttribute), &(VAttribute[${arr.exprs.len}]){')
	for i, attr_expr in arr.exprs {
		if i > 0 {
			g.sb.write_string(', ')
		}
		g.write_orm_vattribute_c_expr(attr_expr)
	}
	g.sb.write_string('})')
}

fn (mut g Gen) write_orm_vattribute_c_expr(expr ast.Expr) {
	init := orm_init_expr(expr) or { return }
	name_expr := orm_init_field_value(init, 'name') or { return }
	has_arg_expr := orm_init_field_value(init, 'has_arg') or { return }
	arg_expr := orm_init_field_value(init, 'arg') or { return }
	kind_expr := orm_init_field_value(init, 'kind') or { return }
	g.sb.write_string('((VAttribute){.name = ')
	g.write_orm_v_string_c_expr(name_expr)
	g.sb.write_string(',.has_arg = ')
	g.sb.write_string(orm_bool_c_expr(has_arg_expr) or { return })
	g.sb.write_string(',.arg = ')
	g.write_orm_v_string_c_expr(arg_expr)
	g.sb.write_string(',.kind = ')
	g.sb.write_string(orm_ident_c_expr(kind_expr) or { return })
	g.sb.write_string('})')
}

fn orm_init_expr(expr ast.Expr) ?ast.InitExpr {
	match expr {
		ast.InitExpr {
			return expr
		}
		ast.ParenExpr {
			return orm_init_expr(expr.expr)
		}
		else {}
	}

	return none
}

fn orm_array_init_expr(expr ast.Expr) ?ast.ArrayInitExpr {
	match expr {
		ast.ArrayInitExpr {
			return expr
		}
		ast.CallExpr {
			name := orm_call_name(expr.lhs)
			if name in ['builtin__new_array_from_c_array_noscan', 'builtin__new_array_from_c_array', 'new_array_from_c_array']
				&& expr.args.len >= 4 {
				return orm_array_init_expr(expr.args[3])
			}
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return orm_array_init_expr(expr.expr)
			}
		}
		ast.ParenExpr {
			return orm_array_init_expr(expr.expr)
		}
		else {}
	}

	return none
}

fn orm_call_name(lhs ast.Expr) string {
	match lhs {
		ast.Ident {
			return lhs.name
		}
		ast.SelectorExpr {
			if lhs.lhs is ast.Ident {
				left := lhs.lhs as ast.Ident
				return '${left.name}__${lhs.rhs.name}'
			}
		}
		else {}
	}

	return ''
}

fn orm_init_field_value(init ast.InitExpr, name string) ?ast.Expr {
	for field in init.fields {
		if field.name == name {
			return field.value
		}
	}
	return none
}

fn (mut g Gen) write_orm_v_string_c_expr(expr ast.Expr) {
	raw := orm_string_value(expr) or { return }
	g.sb.write_string(c_static_v_string_expr_from_c_literal(c_string_literal_content_to_c(raw)))
}

fn orm_string_value(expr ast.Expr) ?string {
	match expr {
		ast.StringLiteral {
			mut val := strip_literal_quotes(expr.value)
			if expr.kind == .raw {
				val = val.replace('\\', '\\\\')
			} else {
				val = process_line_continuations(val)
			}
			return val
		}
		ast.BasicLiteral {
			if expr.kind == .string {
				return process_line_continuations(strip_literal_quotes(expr.value))
			}
		}
		else {}
	}

	return none
}

fn orm_bool_c_expr(expr ast.Expr) ?string {
	match expr {
		ast.BasicLiteral {
			if expr.kind == .key_true || expr.value == 'true' {
				return 'true'
			}
			if expr.kind == .key_false || expr.value == 'false' {
				return 'false'
			}
		}
		ast.Ident {
			if expr.name == 'true' || expr.name == 'false' {
				return expr.name
			}
		}
		else {}
	}

	return none
}

fn orm_number_c_expr(expr ast.Expr) ?string {
	match expr {
		ast.BasicLiteral {
			return sanitize_c_number_literal(expr.value)
		}
		ast.PrefixExpr {
			if expr.op == .minus && expr.expr is ast.BasicLiteral {
				inner := expr.expr as ast.BasicLiteral
				return '-' + sanitize_c_number_literal(inner.value)
			}
		}
		else {}
	}

	return none
}

fn orm_ident_c_expr(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				left := expr.lhs as ast.Ident
				return '${left.name}__${expr.rhs.name}'
			}
		}
		else {}
	}

	return none
}
