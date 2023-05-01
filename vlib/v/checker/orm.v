// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token
import v.util

const (
	fkey_attr_name            = 'fkey'
	v_orm_prefix              = 'V ORM'
	connection_interface_name = 'orm.Connection'
)

type ORMExpr = ast.SqlExpr | ast.SqlStmt

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}

	if !c.check_db_expr(node.db_expr) {
		return ast.void_type
	}
	c.ensure_type_exists(node.table_expr.typ, node.pos) or { return ast.void_type }
	sym := c.table.sym(node.table_expr.typ)

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
			where_expr: ast.None{}
			typ: typ.set_flag(.result)
			db_expr: node.db_expr
			table_expr: ast.TypeNode{
				pos: node.table_expr.pos
				typ: typ
			}
			is_generated: true
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
	field_names := fields.map(it.name)

	if node.has_where {
		c.expr(node.where_expr)
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(&node.where_expr)
		c.check_where_expr_has_no_pointless_exprs(sym, field_names, &node.where_expr)
	}

	if node.has_order {
		if mut node.order_expr is ast.Ident {
			order_ident_name := node.order_expr.name

			if !sym.has_field(order_ident_name) {
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
		c.check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut node.limit_expr,
			'limit')
	}

	if node.has_offset {
		c.expr(node.offset_expr)
		c.check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut node.offset_expr,
			'offset')
	}
	c.expr(node.db_expr)

	c.check_orm_or_expr(node)

	return node.typ.clear_flag(.result)
}

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) ast.Type {
	if !c.check_db_expr(node.db_expr) {
		return ast.void_type
	}
	node.db_expr_type = c.table.unaliased_type(c.expr(node.db_expr))

	for mut line in node.lines {
		c.sql_stmt_line(mut line)
	}

	c.check_orm_or_expr(node)

	return ast.void_type
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
	inserting_object_name := node.object_var_name

	if node.kind == .insert && !node.is_generated {
		inserting_object := node.scope.find(inserting_object_name) or {
			c.error('undefined ident: `${inserting_object_name}`', node.pos)
			return ast.void_type
		}
		mut inserting_object_type := inserting_object.typ

		if inserting_object_type.is_ptr() {
			inserting_object_type = inserting_object.typ.deref()
		}

		if inserting_object_type != node.table_expr.typ {
			table_name := table_sym.name
			inserting_type_name := c.table.sym(inserting_object_type).name

			c.error('cannot use `${inserting_type_name}` as `${table_name}`', node.pos)
			return ast.void_type
		}
	}

	if table_sym.info !is ast.Struct {
		c.error('unknown type `${table_sym.name}`', node.pos)
		return ast.void_type
	}

	info := table_sym.info as ast.Struct
	mut fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name)
	mut sub_structs := map[int]ast.SqlStmtLine{}
	for f in fields.filter((c.table.type_symbols[int(it.typ)].kind == .struct_
		|| (c.table.sym(it.typ).kind == .array
		&& c.table.sym(c.table.sym(it.typ).array_info().elem_type).kind == .struct_))
		&& c.table.get_type_name(it.typ) != 'time.Time') {
		// Delete an uninitialized struct from fields and skip adding the current field
		// to sub structs to skip inserting an empty struct in the related table.
		if c.check_field_of_inserting_structure_is_uninitialized(node, f.name) {
			fields.delete(fields.index(f))
			continue
		}

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
			is_generated: true
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
				c.orm_error("`${checker.fkey_attr_name}` attribute must be string. Try [${checker.fkey_attr_name}: '${attr.arg}'] instead of [${checker.fkey_attr_name}: ${attr.arg}]",
					attr.pos)
				return
			}

			field_struct_type := if field_type.info is ast.Array {
				c.table.sym(field_type.info.elem_type)
			} else {
				field_type
			}

			field_struct_type.find_field(attr.arg) or {
				c.orm_error('`${field_struct_type.name}` struct has no field with name `${attr.arg}`',
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

// check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type checks that an expression is compile-time
// and contains an integer greater than or equal to zero or it is a runtime expression with an integer type.
fn (mut c Checker) check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut expr ast.Expr, sql_keyword string) {
	comptime_number := c.get_comptime_number_value(mut expr) or {
		c.check_sql_expr_type_is_int(expr, sql_keyword)
		return
	}

	if comptime_number < 0 {
		c.orm_error('`${sql_keyword}` must be greater than or equal to zero', expr.pos())
	}
}

fn (mut c Checker) check_sql_expr_type_is_int(expr &ast.Expr, sql_keyword string) {
	if expr is ast.Ident {
		if expr.obj.typ.is_int() {
			return
		}
	} else if expr is ast.SelectorExpr {
		if expr.typ.is_int() {
			return
		}
	} else if expr is ast.CallExpr {
		if expr.return_type == 0 {
			return
		}

		type_symbol := c.table.sym(expr.return_type)
		is_error_type := expr.return_type.has_flag(.result) || expr.return_type.has_flag(.option)
		is_acceptable_type := type_symbol.is_int() && !is_error_type

		if !is_acceptable_type {
			error_type_symbol := c.fn_return_type_flag_to_string(expr.return_type)
			c.orm_error('function calls in `${sql_keyword}` must return only an integer type, but `${expr.name}` returns `${error_type_symbol}${type_symbol.name}`',
				expr.pos)
		}

		return
	} else if expr is ast.ParExpr {
		c.check_sql_expr_type_is_int(expr.expr, sql_keyword)
		return
	}

	c.orm_error('the type of `${sql_keyword}` must be an integer type', expr.pos())
}

fn (mut c Checker) orm_error(message string, pos token.Pos) {
	c.error('${checker.v_orm_prefix}: ${message}', pos)
}

// check_expr_has_no_fn_calls_with_non_orm_return_type checks that an expression has no function calls
// that return complex types which can't be transformed into SQL.
fn (mut c Checker) check_expr_has_no_fn_calls_with_non_orm_return_type(expr &ast.Expr) {
	if expr is ast.CallExpr {
		// `expr.return_type` may be empty. For example, a user call function incorrectly without passing all required arguments.
		// This error will be handled in another place. Otherwise, `c.table.sym` below does panic.
		//
		// fn test(flag bool) {}
		// test()
		//      ~~~~~~ expected 1 arguments, but got 0
		if expr.return_type == 0 {
			return
		}

		type_symbol := c.table.sym(expr.return_type)
		is_time := type_symbol.cname == 'time__Time'
		is_not_pointer := !type_symbol.is_pointer()
		is_error_type := expr.return_type.has_flag(.result) || expr.return_type.has_flag(.option)
		is_acceptable_type := (type_symbol.is_primitive() || is_time) && is_not_pointer
			&& !is_error_type

		if !is_acceptable_type {
			error_type_symbol := c.fn_return_type_flag_to_string(expr.return_type)
			c.orm_error('function calls must return only primitive types and time.Time, but `${expr.name}` returns `${error_type_symbol}${type_symbol.name}`',
				expr.pos)
		}
	} else if expr is ast.ParExpr {
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(expr.expr)
	} else if expr is ast.InfixExpr {
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(expr.left)
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(expr.right)
	}
}

// check_where_expr_has_no_pointless_exprs checks that an expression has no pointless expressions
// which don't affect the result. For example, `where 3` is pointless.
// Also, it checks that the left side of the infix expression is always the structure field.
fn (mut c Checker) check_where_expr_has_no_pointless_exprs(table_type_symbol &ast.TypeSymbol, field_names []string, expr &ast.Expr) {
	// Skip type checking for generated subqueries
	// that are not linked to scope and vars but only created for cgen.
	if expr is ast.None {
		return
	}

	if expr is ast.InfixExpr {
		has_no_field_error := "left side of the `${expr.op}` expression must be one of the `${table_type_symbol.name}`'s fields"

		if expr.left is ast.Ident {
			left_ident_name := expr.left.name

			if !table_type_symbol.has_field(left_ident_name) {
				c.orm_error(util.new_suggestion(left_ident_name, field_names).say(has_no_field_error),
					expr.left.pos)
			}
		} else if expr.left is ast.InfixExpr || expr.left is ast.ParExpr
			|| expr.left is ast.PrefixExpr {
			c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names,
				expr.left)
		} else {
			c.orm_error(has_no_field_error, expr.left.pos())
		}

		if expr.right is ast.InfixExpr || expr.right is ast.ParExpr || expr.right is ast.PrefixExpr {
			c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names,
				expr.right)
		}
	} else if expr is ast.ParExpr {
		c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names, expr.expr)
	} else if expr is ast.PrefixExpr {
		c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names, expr.right)
	} else {
		c.orm_error('`where` expression must have at least one comparison for filtering rows',
			expr.pos())
	}
}

fn (_ &Checker) fn_return_type_flag_to_string(typ ast.Type) string {
	is_result_type := typ.has_flag(.result)
	is_option_type := typ.has_flag(.option)

	return if is_result_type {
		'!'
	} else if is_option_type {
		'?'
	} else {
		''
	}
}

fn (mut c Checker) check_orm_or_expr(expr ORMExpr) {
	if expr is ast.SqlExpr {
		if expr.is_generated {
			return
		}
	}

	return_type := if expr is ast.SqlExpr {
		expr.typ
	} else {
		ast.void_type.set_flag(.result)
	}

	if expr.or_expr.kind == .absent {
		if c.inside_defer {
			c.error('V ORM returns a result, so it should have an `or {}` block at the end',
				expr.pos)
		} else {
			c.error('V ORM returns a result, so it should have either an `or {}` block, or `!` at the end',
				expr.pos)
		}
	} else {
		c.check_or_expr(expr.or_expr, return_type.clear_flag(.result), return_type, if expr is ast.SqlExpr {
			expr
		} else {
			ast.empty_expr
		})
	}

	if expr.or_expr.kind == .block {
		c.expected_or_type = return_type.clear_flag(.result)
		c.stmts_ending_with_expression(expr.or_expr.stmts)
		c.expected_or_type = ast.void_type
	}
}

fn (mut c Checker) check_db_expr(db_expr &ast.Expr) bool {
	connection_type_index := c.table.find_type_idx(checker.connection_interface_name)
	connection_typ := ast.Type(connection_type_index)
	db_expr_type := c.expr(db_expr)

	// If we didn't find `orm.Connection`, we don't have any imported modules
	// that depend on `orm` and implement the `orm.Connection` interface.
	if connection_type_index == 0 {
		c.error('expected a type that implements the `${checker.connection_interface_name}` interface',
			db_expr.pos())
		return false
	}

	is_implemented := c.type_implements(db_expr_type, connection_typ, db_expr.pos())
	is_option := db_expr_type.has_flag(.option)

	if is_implemented && is_option {
		c.error(c.expected_msg(db_expr_type, db_expr_type.clear_flag(.option)), db_expr.pos())
		return false
	}

	return true
}

fn (_ &Checker) check_field_of_inserting_structure_is_uninitialized(node &ast.SqlStmtLine, field_name string) bool {
	struct_scope := node.scope.find_var(node.object_var_name) or { return false }

	if struct_scope.expr is ast.StructInit {
		return struct_scope.expr.fields.filter(it.name == field_name).len == 0
	}

	return false
}
