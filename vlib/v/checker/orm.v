// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token
import v.util

const (
	fkey_attr_name = 'fkey'
	v_orm_prefix   = 'V ORM'
)

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	sym := c.table.sym(node.table_expr.typ)
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return ast.void_type }
	old_ts := c.cur_orm_ts
	c.cur_orm_ts = *sym
	defer {
		c.cur_orm_ts = old_ts
	}
	if sym.info !is ast.Struct {
		c.orm_error('the table symbol `${sym.name}` has to be a struct', node.table_expr.pos)
		return ast.void_type
	}
	info := sym.info as ast.Struct
	mut fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, sym.name)
	mut sub_structs := map[int]ast.SqlExpr{}
	for f in fields.filter((c.table.type_symbols[int(it.typ)].kind == .struct_
		|| (c.table.sym(it.typ).kind == .array
		&& c.table.sym(c.table.sym(it.typ).array_info().elem_type).kind == .struct_))
		&& c.table.get_type_name(it.typ) != 'time.Time') {
		typ := if c.table.sym(f.typ).kind == .struct_ {
			f.typ
		} else if c.table.sym(f.typ).kind == .array {
			c.table.sym(f.typ).array_info().elem_type
		} else {
			ast.Type(0)
		}
		mut n := ast.SqlExpr{
			pos: node.pos
			has_where: true
			typ: typ
			db_expr: node.db_expr
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: typ
			}
		}
		tmp_inside_sql := c.inside_sql
		c.sql_expr(mut n)
		c.inside_sql = tmp_inside_sql
		n.where_expr = ast.InfixExpr{
			op: .eq
			pos: n.pos
			left: ast.Ident{
				language: .v
				tok_kind: .eq
				scope: c.fn_scope
				obj: ast.Var{}
				mod: 'main'
				name: 'id'
				is_mut: false
				kind: .unresolved
				info: ast.IdentVar{}
			}
			right: ast.Ident{
				language: .c
				mod: 'main'
				tok_kind: .eq
				obj: ast.Var{}
				is_mut: false
				scope: c.fn_scope
				info: ast.IdentVar{
					typ: ast.int_type
				}
			}
			left_type: ast.int_type
			right_type: ast.int_type
			auto_locked: ''
			or_block: ast.OrExpr{}
		}

		sub_structs[int(typ)] = n
	}
	if node.is_count {
		fields = [
			ast.StructField{
				typ: ast.int_type
			},
		]
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
	if node.has_where {
		c.expr(node.where_expr)
	}

	if node.has_order {
		if mut node.order_expr is ast.Ident {
			order_ident_name := node.order_expr.name

			sym.find_field(order_ident_name) or {
				field_names := fields.map(it.name)

				c.orm_error(util.new_suggestion(order_ident_name, field_names).say('`${sym.name}` structure has no field with name `${order_ident_name}`'),
					node.order_expr.pos)
				return ast.void_type
			}
		} else {
			c.orm_error("expected `${sym.name}` structure's field", node.order_expr.pos())
			return ast.void_type
		}

		c.expr(node.order_expr)
	}

	if node.has_limit {
		c.expr(node.limit_expr)
		c.check_sql_value_expr_is_natural_number(mut node.limit_expr, 'limit')
	}

	if node.has_offset {
		c.expr(node.offset_expr)
		c.check_sql_value_expr_is_natural_number(mut node.offset_expr, 'offset')
	}
	c.expr(node.db_expr)

	if node.or_expr.kind == .block {
		if node.or_expr.stmts.len == 0 {
			c.orm_error('or block needs to return a default value', node.or_expr.pos)
		}
		if node.or_expr.stmts.len > 0 && node.or_expr.stmts.last() is ast.ExprStmt {
			c.expected_or_type = node.typ
		}
		c.stmts_ending_with_expression(node.or_expr.stmts)
		c.check_expr_opt_call(node, node.typ)
		c.expected_or_type = ast.void_type
	}
	return node.typ
}

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) ast.Type {
	node.db_expr_type = c.table.unaliased_type(c.expr(node.db_expr))
	mut typ := ast.void_type
	for mut line in node.lines {
		a := c.sql_stmt_line(mut line)
		if a != ast.void_type {
			typ = a
		}
	}
	if node.or_expr.kind == .block {
		c.stmts_ending_with_expression(node.or_expr.stmts)
	}
	return typ
}

fn (mut c Checker) sql_stmt_line(mut node ast.SqlStmtLine) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return ast.void_type }
	table_sym := c.table.sym(node.table_expr.typ)
	old_ts := c.cur_orm_ts
	c.cur_orm_ts = *table_sym
	defer {
		c.cur_orm_ts = old_ts
	}
	if table_sym.info !is ast.Struct {
		c.orm_error('unknown type `${table_sym.name}`', node.pos)
		return ast.void_type
	}
	info := table_sym.info as ast.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name)
	mut sub_structs := map[int]ast.SqlStmtLine{}
	for f in fields.filter(((c.table.type_symbols[int(it.typ)].kind == .struct_)
		|| (c.table.sym(it.typ).kind == .array
		&& c.table.sym(c.table.sym(it.typ).array_info().elem_type).kind == .struct_))
		&& c.table.get_type_name(it.typ) != 'time.Time') {
		c.check_orm_struct_field_attributes(f)

		typ := if c.table.sym(f.typ).kind == .struct_ {
			f.typ
		} else if c.table.sym(f.typ).kind == .array {
			c.table.sym(f.typ).array_info().elem_type
		} else {
			ast.Type(0)
		}

		mut object_var_name := '${node.object_var_name}.${f.name}'
		if typ != f.typ {
			object_var_name = node.object_var_name
		}
		mut n := ast.SqlStmtLine{
			pos: node.pos
			kind: node.kind
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: typ
			}
			object_var_name: object_var_name
		}
		tmp_inside_sql := c.inside_sql
		c.sql_stmt_line(mut n)
		c.inside_sql = tmp_inside_sql
		sub_structs[typ] = n
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
	for i, column in node.updated_columns {
		x := node.fields.filter(it.name == column)
		if x.len == 0 {
			c.orm_error('type `${table_sym.name}` has no field named `${column}`', node.pos)
			continue
		}
		field := x[0]
		node.updated_columns[i] = c.fetch_field_name(field)
	}
	if node.kind == .update {
		for expr in node.update_exprs {
			c.expr(expr)
		}
	}
	if node.where_expr !is ast.EmptyExpr {
		c.expr(node.where_expr)
	}

	return ast.void_type
}

fn (mut c Checker) check_orm_struct_field_attributes(field ast.StructField) {
	field_type := c.table.sym(field.typ)
	mut has_fkey_attr := false

	for attr in field.attrs {
		if attr.name == checker.fkey_attr_name {
			if field_type.kind != .array && field_type.kind != .struct_ {
				c.orm_error('the `${checker.fkey_attr_name}` attribute must be used only with arrays and structures',
					attr.pos)
				return
			}

			if !attr.has_arg {
				c.orm_error('the `${checker.fkey_attr_name}` attribute must have an argument',
					attr.pos)
				return
			}

			if attr.kind != .string {
				c.orm_error('`${checker.fkey_attr_name}` attribute must be string. Try [${checker.fkey_attr_name}: \'${attr.arg}\'] instead of [${checker.fkey_attr_name}: ${attr.arg}]',
					attr.pos)
				return
			}

			field_struct_type := if field_type.info is ast.Array {
				c.table.sym(field_type.info.elem_type)
			} else {
				field_type
			}

			field_struct_type.find_field(attr.arg) or {
				c.error('`${field_struct_type.name}` struct has no field with name `${attr.arg}`',
					attr.pos)
				return
			}

			has_fkey_attr = true
		}
	}

	if field_type.kind == .array && !has_fkey_attr {
		c.orm_error('a field that holds an array must be defined with the `${checker.fkey_attr_name}` attribute',
			field.pos)
	}
}

fn (mut c Checker) fetch_and_verify_orm_fields(info ast.Struct, pos token.Pos, table_name string) []ast.StructField {
	fields := info.fields.filter(
		(it.typ in [ast.string_type, ast.bool_type] || int(it.typ) in ast.number_type_idxs
		|| c.table.type_symbols[int(it.typ)].kind == .struct_
		|| (c.table.sym(it.typ).kind == .array
		&& c.table.sym(c.table.sym(it.typ).array_info().elem_type).kind == .struct_))
		&& !it.attrs.contains('skip'))
	if fields.len == 0 {
		c.orm_error('select: empty fields in `${table_name}`', pos)
		return []ast.StructField{}
	}
	if fields[0].name != 'id' {
		c.orm_error('`id int` must be the first field in `${table_name}`', pos)
	}
	return fields
}

fn (mut c Checker) check_sql_value_expr_is_natural_number(mut expr ast.Expr, sql_keyword string) {
	// cgen doesn't support call expressions in ORM
	if expr is ast.CallExpr {
		c.orm_error('call expressions are not supported yet', expr.pos())
		return
	}

	comptime_number := c.get_comptime_number_value(mut expr) or {
		c.check_sql_expr_type_is_uint(expr, sql_keyword)
		return
	}

	if comptime_number < 0 {
		c.orm_error('`${sql_keyword}` must be greater than or equal to zero', expr.pos())
	}
}

fn (mut c Checker) check_sql_expr_type_is_uint(expr &ast.Expr, sql_keyword string) {
	if expr is ast.Ident {
		if expr.obj.typ.is_unsigned() {
			return
		}
	}

	c.orm_error('the type of `${sql_keyword}` must be an unsigned integer type', expr.pos())
}

fn (mut c Checker) orm_error(message string, pos token.Pos) {
	c.error('${checker.v_orm_prefix}: ${message}', pos)
}
