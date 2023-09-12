// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token
import v.util

const (
	v_orm_prefix              = 'V ORM'
	fkey_attr_name            = 'fkey'
	connection_interface_name = 'orm.Connection'
)

type ORMExpr = ast.SqlExpr | ast.SqlStmt

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}

	if !c.check_db_expr(mut node.db_expr) {
		return ast.void_type
	}

	// To avoid panics while working with `table_expr`,
	// it is necessary to check if its type exists.
	if !c.ensure_type_exists(node.table_expr.typ, node.pos) {
		return ast.void_type
	}
	table_sym := c.table.sym(node.table_expr.typ)

	if !c.check_orm_table_expr_type(node.table_expr) {
		return ast.void_type
	}

	old_ts := c.cur_orm_ts
	c.cur_orm_ts = *table_sym
	defer {
		c.cur_orm_ts = old_ts
	}

	info := table_sym.info as ast.Struct
	mut fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name,
		node)
	non_primitive_fields := c.get_orm_non_primitive_fields(fields)
	mut sub_structs := map[int]ast.SqlExpr{}

	for field in non_primitive_fields {
		typ := c.get_type_of_field_with_related_table(field)

		mut subquery_expr := ast.SqlExpr{
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
		c.sql_expr(mut subquery_expr)
		c.inside_sql = tmp_inside_sql

		subquery_expr.where_expr = ast.InfixExpr{
			op: .eq
			pos: subquery_expr.pos
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

		sub_structs[int(typ)] = subquery_expr
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
		c.expr(mut node.where_expr)
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(&node.where_expr)
		c.check_where_expr_has_no_pointless_exprs(table_sym, field_names, &node.where_expr)
	}

	if node.has_order {
		if mut node.order_expr is ast.Ident {
			order_ident_name := node.order_expr.name

			if !table_sym.has_field(order_ident_name) {
				c.orm_error(util.new_suggestion(order_ident_name, field_names).say('`${table_sym.name}` structure has no field with name `${order_ident_name}`'),
					node.order_expr.pos)
				return ast.void_type
			}
		} else {
			c.orm_error("expected `${table_sym.name}` structure's field", node.order_expr.pos())
			return ast.void_type
		}

		c.expr(mut node.order_expr)
	}

	if node.has_limit {
		c.expr(mut node.limit_expr)
		c.check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut node.limit_expr,
			'limit')
	}

	if node.has_offset {
		c.expr(mut node.offset_expr)
		c.check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut node.offset_expr,
			'offset')
	}
	c.expr(mut node.db_expr)

	c.check_orm_or_expr(mut node)

	return node.typ.clear_flag(.result)
}

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) ast.Type {
	if !c.check_db_expr(mut node.db_expr) {
		return ast.void_type
	}
	node.db_expr_type = c.table.unaliased_type(c.expr(mut node.db_expr))

	for mut line in node.lines {
		c.sql_stmt_line(mut line)
	}

	c.check_orm_or_expr(mut node)

	return ast.void_type
}

fn (mut c Checker) sql_stmt_line(mut node ast.SqlStmtLine) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}

	// To avoid panics while working with `table_expr`,
	// it is necessary to check if its type exists.
	if !c.ensure_type_exists(node.table_expr.typ, node.pos) {
		return ast.void_type
	}
	table_sym := c.table.sym(node.table_expr.typ)

	if !c.check_orm_table_expr_type(node.table_expr) {
		return ast.void_type
	}

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
	mut fields := c.fetch_and_verify_orm_fields(info, node.table_expr.pos, table_sym.name,
		ast.SqlExpr{})

	for field in fields {
		c.check_orm_struct_field_attrs(node, field)
	}

	mut sub_structs := map[int]ast.SqlStmtLine{}
	non_primitive_fields := c.get_orm_non_primitive_fields(fields)

	for field in non_primitive_fields {
		// Delete an uninitialized struct from fields and skip adding the current field
		// to sub structs to skip inserting an empty struct in the related table.
		if c.check_field_of_inserting_struct_is_uninitialized(node, field.name) {
			fields.delete(fields.index(field))
			continue
		}

		c.check_orm_non_primitive_struct_field_attrs(field)

		typ := c.get_type_of_field_with_related_table(field)

		mut object_var_name := '${node.object_var_name}.${field.name}'
		if typ != field.typ {
			object_var_name = node.object_var_name
		} else if typ.has_flag(.option) {
			sym := c.table.sym(field.typ)
			object_var_name = '(*(${sym.cname}*)&${object_var_name}.data)'
		}

		mut subquery_expr := ast.SqlStmtLine{
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
		c.sql_stmt_line(mut subquery_expr)
		c.inside_sql = tmp_inside_sql
		sub_structs[typ] = subquery_expr
	}

	node.fields = fields
	node.sub_structs = sub_structs.move()

	for i, column in node.updated_columns {
		updated_fields := node.fields.filter(it.name == column)

		if updated_fields.len == 0 {
			c.orm_error('type `${table_sym.name}` has no field named `${column}`', node.pos)
			continue
		}

		field := updated_fields.first()
		node.updated_columns[i] = c.fetch_field_name(field)
	}

	if node.kind == .update {
		for mut expr in node.update_exprs {
			c.expr(mut expr)
		}
	}

	if node.where_expr !is ast.EmptyExpr {
		c.expr(mut node.where_expr)
	}

	return ast.void_type
}

fn (mut c Checker) check_orm_struct_field_attrs(node ast.SqlStmtLine, field ast.StructField) {
	for attr in field.attrs {
		if attr.name == 'nonull' {
			c.warn('[nonull] attributes are deprecated (all regular fields are "not null"), please use option fields to represent nullable columns',
				node.pos)
		}
	}
}

fn (mut c Checker) check_orm_non_primitive_struct_field_attrs(field ast.StructField) {
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

fn (mut c Checker) fetch_and_verify_orm_fields(info ast.Struct, pos token.Pos, table_name string, sql_expr ast.SqlExpr) []ast.StructField {
	field_pos := c.orm_get_field_pos(sql_expr.where_expr)
	mut fields := []ast.StructField{}
	for field in info.fields {
		is_primitive := field.typ.is_string() || field.typ.is_bool() || field.typ.is_number()
		fsym := c.table.sym(field.typ)
		is_struct := fsym.kind == .struct_
		is_array := fsym.kind == .array
		elem_sym := if is_array {
			c.table.sym(fsym.array_info().elem_type)
		} else {
			ast.invalid_type_symbol
		}
		is_array_with_struct_elements := is_array && elem_sym.kind == .struct_
		has_skip_attr := field.attrs.contains('skip') || field.attrs.contains_arg('sql', '-')
		if has_skip_attr {
			continue
		}
		if is_primitive || is_struct || is_array_with_struct_elements {
			fields << field
		}
		if is_array && elem_sym.is_primitive() {
			c.add_error_detail('')
			c.add_error_detail(' field name: `${field.name}`')
			c.add_error_detail(' data type: `${c.table.type_to_str(field.typ)}`')
			c.orm_error('does not support array of primitive types', field_pos)
			return []ast.StructField{}
		}
	}
	if fields.len == 0 {
		c.orm_error('select: empty fields in `${table_name}`', pos)
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

fn (mut c Checker) check_orm_or_expr(mut expr ORMExpr) {
	if mut expr is ast.SqlExpr {
		if expr.is_generated {
			return
		}
	}

	return_type := if mut expr is ast.SqlExpr {
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
		c.check_or_expr(expr.or_expr, return_type.clear_flag(.result), return_type, if mut expr is ast.SqlExpr {
			expr
		} else {
			ast.empty_expr
		})
	}

	if expr.or_expr.kind == .block {
		c.expected_or_type = return_type.clear_flag(.result)
		c.stmts_ending_with_expression(mut expr.or_expr.stmts)
		c.expected_or_type = ast.void_type
	}
}

// check_db_expr checks the `db_expr` implements `orm.Connection` and has no `option` flag.
fn (mut c Checker) check_db_expr(mut db_expr ast.Expr) bool {
	connection_type_index := c.table.find_type_idx(checker.connection_interface_name)
	connection_typ := ast.Type(connection_type_index)
	db_expr_type := c.expr(mut db_expr)

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

fn (mut c Checker) check_orm_table_expr_type(type_node &ast.TypeNode) bool {
	table_sym := c.table.sym(type_node.typ)

	if table_sym.info !is ast.Struct {
		c.orm_error('the table symbol `${table_sym.name}` has to be a struct', type_node.pos)

		return false
	}

	return true
}

// get_type_of_field_with_related_table gets the type of table in which
// the primary key is used as the foreign key in the current table.
// For example, if you are using `[]Child`, the related table type would be `Child`.
fn (c &Checker) get_type_of_field_with_related_table(table_field &ast.StructField) ast.Type {
	if c.table.sym(table_field.typ).kind == .struct_ {
		return table_field.typ
	} else if c.table.sym(table_field.typ).kind == .array {
		return c.table.sym(table_field.typ).array_info().elem_type
	} else {
		return ast.Type(0)
	}
}

// get_orm_non_primitive_fields filters the table fields by selecting only
// non-primitive fields such as arrays and structs.
fn (c &Checker) get_orm_non_primitive_fields(fields []ast.StructField) []ast.StructField {
	return fields.filter(fn [c] (field ast.StructField) bool {
		type_with_no_option_flag := field.typ.clear_flag(.option)
		is_struct := c.table.type_symbols[int(type_with_no_option_flag)].kind == .struct_
		is_array := c.table.sym(type_with_no_option_flag).kind == .array
		is_array_with_struct_elements := is_array
			&& c.table.sym(c.table.sym(type_with_no_option_flag).array_info().elem_type).kind == .struct_
		is_time := c.table.get_type_name(type_with_no_option_flag) == 'time.Time'

		return (is_struct || is_array_with_struct_elements) && !is_time
	})
}

// walkingdevel: Now I don't think it's a good solution
// because it only checks structure initialization,
// but structure fields may be updated later before inserting.
// For example,
// ```v
// mut package := Package{
// 	name: 'xml'
// }
//
// package.author = User{
// 	username: 'walkingdevel'
// }
// ```
// TODO: rewrite it, move to runtime.
fn (_ &Checker) check_field_of_inserting_struct_is_uninitialized(node &ast.SqlStmtLine, field_name string) bool {
	struct_scope := node.scope.find_var(node.object_var_name) or { return false }

	if struct_scope.expr is ast.StructInit {
		return struct_scope.expr.init_fields.filter(it.name == field_name).len == 0
	}

	return false
}

fn (c &Checker) orm_get_field_pos(expr &ast.Expr) token.Pos {
	mut pos := token.Pos{}
	if expr is ast.InfixExpr {
		if expr.left is ast.Ident {
			pos = expr.left.pos
		} else if expr.left is ast.InfixExpr || expr.left is ast.ParExpr
			|| expr.left is ast.PrefixExpr {
			pos = c.orm_get_field_pos(expr.left)
		} else {
			pos = expr.left.pos()
		}
	} else if expr is ast.ParExpr {
		pos = c.orm_get_field_pos(expr.expr)
	} else if expr is ast.PrefixExpr {
		pos = c.orm_get_field_pos(expr.right)
	} else {
		pos = expr.pos()
	}
	return pos
}
