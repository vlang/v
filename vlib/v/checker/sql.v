// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.table
import v.token

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) table.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	sym := c.table.get_type_symbol(node.table_expr.typ)
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return table.void_type }
	c.cur_orm_ts = sym
	info := sym.info as table.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, sym.name)
	mut sub_structs := map[int]ast.SqlExpr{}
	for f in fields.filter(c.table.type_symbols[int(it.typ)].kind == .struct_) {
		mut n := ast.SqlExpr{
			pos: node.pos
			has_where: true
			typ: f.typ
			db_expr: node.db_expr
			table_expr: ast.Type{
				pos: node.table_expr.pos
				typ: f.typ
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
					typ: table.int_type
				}
			}
			left_type: table.int_type
			right_type: table.int_type
			auto_locked: ''
			or_block: ast.OrExpr{}
		}

		sub_structs[int(f.typ)] = n
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
	if node.has_where {
		c.expr(node.where_expr)
	}
	if node.has_offset {
		c.expr(node.offset_expr)
	}
	if node.has_limit {
		c.expr(node.limit_expr)
	}
	if node.has_order {
		c.expr(node.order_expr)
	}
	c.expr(node.db_expr)
	return node.typ
}

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) table.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return table.void_type }
	table_sym := c.table.get_type_symbol(node.table_expr.typ)
	c.cur_orm_ts = table_sym
	info := table_sym.info as table.Struct
	fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name)
	mut sub_structs := map[int]ast.SqlStmt{}
	for f in fields.filter(c.table.type_symbols[int(it.typ)].kind == .struct_) {
		mut n := ast.SqlStmt{
			pos: node.pos
			db_expr: node.db_expr
			kind: node.kind
			table_expr: ast.Type{
				pos: node.table_expr.pos
				typ: f.typ
			}
			object_var_name: '${node.object_var_name}.$f.name'
		}
		tmp_inside_sql := c.inside_sql
		c.sql_stmt(mut n)
		c.inside_sql = tmp_inside_sql
		sub_structs[int(f.typ)] = n
	}
	node.fields = fields
	node.sub_structs = sub_structs.move()
	c.expr(node.db_expr)
	if node.kind == .update {
		for expr in node.update_exprs {
			c.expr(expr)
		}
	}
	c.expr(node.where_expr)

	return table.void_type
}

fn (mut c Checker) fetch_and_verify_orm_fields(info table.Struct, pos token.Position, table_name string) []table.Field {
	fields := info.fields.filter((it.typ in [table.string_type, table.int_type, table.bool_type]
		|| c.table.type_symbols[int(it.typ)].kind == .struct_) && !it.attrs.contains('skip'))
	if fields.len == 0 {
		c.error('V orm: select: empty fields in `$table_name`', pos)
		return []table.Field{}
	}
	if fields[0].name != 'id' {
		c.error('V orm: `id int` must be the first field in `$table_name`', pos)
	}
	return fields
}
