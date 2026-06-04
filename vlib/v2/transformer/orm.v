// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.token
import v2.types

fn (mut t Transformer) transform_sql_expr(expr ast.SqlExpr) ast.Expr {
	if !expr.is_create {
		return ast.Expr(ast.SqlExpr{
			expr:       t.transform_expr(expr.expr)
			table_name: expr.table_name
			is_count:   expr.is_count
			is_create:  expr.is_create
			pos:        expr.pos
		})
	}
	if lowered := t.lower_sql_create_expr(expr) {
		return lowered
	}
	return ast.Expr(ast.SqlExpr{
		expr:       t.transform_expr(expr.expr)
		table_name: expr.table_name
		is_count:   expr.is_count
		is_create:  expr.is_create
		pos:        expr.pos
	})
}

fn (mut t Transformer) lower_sql_create_expr(expr ast.SqlExpr) ?ast.Expr {
	table := t.lookup_sql_table_struct(expr.table_name) or { return none }
	call_pos := t.next_synth_pos()
	call := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.SelectorExpr{
			lhs: expr.expr
			rhs: ast.Ident{
				name: 'create'
				pos:  call_pos
			}
			pos: call_pos
		})
		args: [
			t.sql_orm_table_expr(table.name, expr.pos),
			t.sql_orm_table_fields_expr(table.fields, expr.pos),
		]
		pos:  call_pos
	})
	t.register_synth_type(call_pos, types.Type(types.ResultType{
		base_type: types.Type(types.void_)
	}))
	return t.transform_expr(call)
}

fn (t &Transformer) lookup_sql_table_struct(name string) ?types.Struct {
	if typ := t.lookup_type(name) {
		base := t.sql_unwrap_alias_pointer_type(typ)
		if base is types.Struct {
			return base
		}
	}
	return t.lookup_struct_type_any_module(name)
}

fn (t &Transformer) sql_unwrap_alias_pointer_type(typ types.Type) types.Type {
	mut cur := typ
	for {
		match cur {
			types.Alias {
				cur = cur.base_type
				continue
			}
			types.Pointer {
				cur = cur.base_type
				continue
			}
			else {}
		}

		break
	}
	return cur
}

fn (mut t Transformer) sql_orm_table_expr(type_name string, pos token.Pos) ast.Expr {
	init_pos := t.next_synth_pos()
	return ast.Expr(ast.InitExpr{
		typ:    t.sql_orm_type_expr('Table', pos)
		fields: [
			ast.FieldInit{
				name:  'name'
				value: t.sql_string_expr(type_name.to_lower(), pos)
			},
			ast.FieldInit{
				name:  'attrs'
				value: t.sql_vattribute_array_expr([], pos)
			},
		]
		pos:    init_pos
	})
}

fn (mut t Transformer) sql_orm_table_fields_expr(fields []types.Field, pos token.Pos) ast.Expr {
	mut exprs := []ast.Expr{cap: fields.len}
	for field in fields {
		if field_expr := t.sql_orm_table_field_expr(field, pos) {
			exprs << field_expr
		}
	}
	array_pos := t.next_synth_pos()
	return ast.Expr(ast.ArrayInitExpr{
		typ:   ast.Expr(ast.Type(ast.ArrayType{
			elem_type: t.sql_orm_type_expr('TableField', pos)
		}))
		exprs: exprs
		pos:   array_pos
	})
}

fn (mut t Transformer) sql_orm_table_field_expr(field types.Field, pos token.Pos) ?ast.Expr {
	if sql_field_is_skipped(field.attributes) {
		return none
	}
	field_type, nullable, is_arr := t.sql_field_storage_type(field.typ)
	init_pos := t.next_synth_pos()
	return ast.Expr(ast.InitExpr{
		typ:    t.sql_orm_type_expr('TableField', pos)
		fields: [
			ast.FieldInit{
				name:  'name'
				value: t.sql_string_expr(field.name, pos)
			},
			ast.FieldInit{
				name:  'typ'
				value: t.sql_orm_type_idx_expr(field_type, pos)
			},
			ast.FieldInit{
				name:  'nullable'
				value: t.sql_bool_expr(nullable, pos)
			},
			ast.FieldInit{
				name:  'default_val'
				value: t.sql_string_expr('', pos)
			},
			ast.FieldInit{
				name:  'attrs'
				value: t.sql_vattribute_array_expr(field.attributes, pos)
			},
			ast.FieldInit{
				name:  'is_arr'
				value: t.sql_bool_expr(is_arr, pos)
			},
		]
		pos:    init_pos
	})
}

fn (t &Transformer) sql_field_storage_type(typ types.Type) (types.Type, bool, bool) {
	mut cur := typ
	mut nullable := false
	mut is_arr := false
	for {
		match cur {
			types.OptionType {
				nullable = true
				cur = cur.base_type
				continue
			}
			types.Alias {
				cur = cur.base_type
				continue
			}
			types.Pointer {
				cur = cur.base_type
				continue
			}
			types.Array {
				is_arr = true
				cur = cur.elem_type
				continue
			}
			else {}
		}

		break
	}
	return cur, nullable, is_arr
}

fn (mut t Transformer) sql_orm_type_idx_expr(typ types.Type, pos token.Pos) ast.Expr {
	type_name := t.type_to_c_name(typ)
	if type_name == 'time__Time' || type_name == 'time.Time' {
		return t.sql_int_expr(-2, pos)
	}
	match typ {
		types.Enum {
			return t.sql_int_expr(-3, pos)
		}
		types.String {
			return t.sql_int_expr(typeof_type_idx('string'), pos)
		}
		types.Primitive {
			return t.sql_int_expr(typeof_type_idx(type_name), pos)
		}
		else {}
	}

	if type_name == 'string' {
		return t.sql_int_expr(typeof_type_idx('string'), pos)
	}
	return t.sql_int_expr(typeof_type_idx(type_name), pos)
}

fn sql_field_is_skipped(attrs []ast.Attribute) bool {
	if attrs.has('skip') {
		return true
	}
	for attr in attrs {
		if attr.name == 'sql' && sql_attribute_value_string(attr.value) == '-' {
			return true
		}
	}
	return false
}

fn (mut t Transformer) sql_vattribute_array_expr(attrs []ast.Attribute, pos token.Pos) ast.Expr {
	mut exprs := []ast.Expr{cap: attrs.len}
	for attr in attrs {
		if attr_expr := t.sql_vattribute_expr(attr, pos) {
			exprs << attr_expr
		}
	}
	array_pos := t.next_synth_pos()
	elem_pos := t.next_synth_pos()
	return ast.Expr(ast.ArrayInitExpr{
		typ:   ast.Expr(ast.Type(ast.ArrayType{
			elem_type: ast.Expr(ast.Ident{
				name: 'VAttribute'
				pos:  elem_pos
			})
		}))
		exprs: exprs
		pos:   array_pos
	})
}

fn (mut t Transformer) sql_vattribute_expr(attr ast.Attribute, pos token.Pos) ?ast.Expr {
	mut name := attr.name
	mut has_arg := attr.name != ''
	mut arg := ''
	mut kind := 'AttributeKind__plain'
	if name == '' {
		name = sql_attribute_value_string(attr.value)
		has_arg = false
		_, kind = sql_attribute_arg_and_kind(attr.value)
		if kind != 'AttributeKind__string' {
			kind = 'AttributeKind__plain'
		}
	} else {
		arg, kind = sql_attribute_arg_and_kind(attr.value)
	}
	if name == '' {
		return none
	}
	init_pos := t.next_synth_pos()
	typ_pos := t.next_synth_pos()
	kind_pos := t.next_synth_pos()
	return ast.Expr(ast.InitExpr{
		typ:    ast.Expr(ast.Ident{
			name: 'VAttribute'
			pos:  typ_pos
		})
		fields: [
			ast.FieldInit{
				name:  'name'
				value: t.sql_string_expr(name, pos)
			},
			ast.FieldInit{
				name:  'has_arg'
				value: t.sql_bool_expr(has_arg, pos)
			},
			ast.FieldInit{
				name:  'arg'
				value: t.sql_string_expr(arg, pos)
			},
			ast.FieldInit{
				name:  'kind'
				value: ast.Expr(ast.Ident{
					name: kind
					pos:  kind_pos
				})
			},
		]
		pos:    init_pos
	})
}

fn sql_attribute_arg_and_kind(expr ast.Expr) (string, string) {
	match expr {
		ast.StringLiteral {
			return sql_unquote_literal(expr.value), 'AttributeKind__string'
		}
		ast.BasicLiteral {
			if expr.kind in [.key_true, .key_false] {
				return expr.value, 'AttributeKind__bool'
			}
			return expr.value, 'AttributeKind__number'
		}
		ast.Ident {
			kind := if expr.name in sql_plain_attribute_args {
				'AttributeKind__plain'
			} else {
				'AttributeKind__string'
			}
			return expr.name, kind
		}
		ast.PrefixExpr {
			if expr.op == .minus && expr.expr is ast.BasicLiteral {
				inner := expr.expr as ast.BasicLiteral
				return '-' + inner.value, 'AttributeKind__number'
			}
		}
		else {}
	}

	return expr.name(), 'AttributeKind__plain'
}

fn sql_attribute_value_string(expr ast.Expr) string {
	match expr {
		ast.StringLiteral {
			return sql_unquote_literal(expr.value)
		}
		ast.BasicLiteral {
			return expr.value
		}
		ast.Ident {
			return expr.name
		}
		ast.PrefixExpr {
			if expr.op == .minus && expr.expr is ast.BasicLiteral {
				inner := expr.expr as ast.BasicLiteral
				return '-' + inner.value
			}
		}
		else {}
	}

	return expr.name()
}

fn sql_unquote_literal(raw string) string {
	if raw.len < 2 {
		return raw
	}
	first := raw[0]
	last := raw[raw.len - 1]
	if first == last && first in [`'`, `"`] {
		return raw[1..raw.len - 1]
	}
	return raw
}

const sql_plain_attribute_args = ['serial', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64',
	'f32', 'f64', 'bool', 'string']

fn (mut t Transformer) sql_orm_type_expr(name string, pos token.Pos) ast.Expr {
	_ = pos
	expr_pos := t.next_synth_pos()
	lhs_pos := t.next_synth_pos()
	rhs_pos := t.next_synth_pos()
	return ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'orm'
			pos:  lhs_pos
		})
		rhs: ast.Ident{
			name: name
			pos:  rhs_pos
		}
		pos: expr_pos
	})
}

fn (mut t Transformer) sql_orm_const_expr(name string, pos token.Pos) ast.Expr {
	_ = pos
	expr_pos := t.next_synth_pos()
	lhs_pos := t.next_synth_pos()
	rhs_pos := t.next_synth_pos()
	return ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'orm'
			pos:  lhs_pos
		})
		rhs: ast.Ident{
			name: name
			pos:  rhs_pos
		}
		pos: expr_pos
	})
}

fn (mut t Transformer) sql_string_expr(value string, pos token.Pos) ast.Expr {
	_ = pos
	lit_pos := t.next_synth_pos()
	return ast.Expr(ast.StringLiteral{
		kind:  .v
		value: value
		pos:   lit_pos
	})
}

fn (mut t Transformer) sql_int_expr(value int, pos token.Pos) ast.Expr {
	_ = pos
	lit_pos := t.next_synth_pos()
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value.str()
		pos:   lit_pos
	})
}

fn (mut t Transformer) sql_bool_expr(value bool, pos token.Pos) ast.Expr {
	_ = pos
	lit_pos := t.next_synth_pos()
	return ast.Expr(ast.BasicLiteral{
		kind:  if value { token.Token.key_true } else { token.Token.key_false }
		value: if value { 'true' } else { 'false' }
		pos:   lit_pos
	})
}
