// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

// orm_field_access_name converts an ORM field name to proper C struct member access.
// For embedded struct fields like "Payload.some_field", this returns "Payload.some_field"
// (keeping the dot for C member access). For regular fields, it uses c_name().
fn orm_field_access_name(field_name string) string {
	if field_name.contains('.') {
		// Embedded struct field - keep the dots for proper C struct member access
		// e.g., "Payload.some_field" -> "Payload.some_field"
		parts := field_name.split('.')
		mut result := []string{}
		for part in parts {
			result << c_name(part)
		}
		return result.join('.')
	}
	return c_name(field_name)
}

fn (g &Gen) orm_inserting_object_type(obj ast.ScopeObject) ast.Type {
	return match obj {
		ast.Var {
			if obj.smartcasts.len > 0 && obj.orig_type != 0
				&& g.table.final_sym(g.table.final_type(obj.orig_type)).kind == .sum_type {
				obj.orig_type
			} else {
				obj.typ
			}
		}
		else {
			ast.void_type
		}
	}
}

fn (g &Gen) orm_primitive_field_name(typ ast.Type) string {
	final_typ := g.table.final_type(typ.clear_flag(.option))
	sym := g.table.sym(final_typ)
	if sym.name == 'time.Time' {
		return 'time'
	}
	if sym.kind == .enum {
		return 'i64'
	}
	if sym.kind == .array {
		return vint2int(sym.cname.to_lower())
	}
	return vint2int(sym.cname)
}

fn (g &Gen) orm_non_array_fields(fields []ast.StructField) []ast.StructField {
	mut non_array_fields := []ast.StructField{cap: fields.len}
	for field in fields {
		final_typ := g.table.final_type(field.typ.clear_flag(.option))
		if g.table.sym(final_typ).kind != .array {
			non_array_fields << field
		}
	}
	return non_array_fields
}

fn (g &Gen) orm_primitive_variant_field_name(typ ast.Type) string {
	final_typ := g.table.final_type(typ.clear_flag(.option))
	sym := g.table.sym(final_typ)
	if sym.kind == .enum {
		return 'i64'
	}
	return vint2int(sym.cname)
}

enum SqlExprSide {
	left
	right
}

fn sql_query_data_field_name(expr ast.Expr) string {
	return match expr {
		ast.Ident { expr.name }
		ast.SelectorExpr { '${sql_query_data_field_name(expr.expr)}.${expr.field_name}' }
		ast.ParExpr { sql_query_data_field_name(expr.expr) }
		else { '' }
	}
}

fn sql_query_data_op_kind(expr ast.InfixExpr) string {
	is_nil_comparison := expr.right is ast.Nil && expr.op in [.eq, .ne]
	return match expr.op {
		.ne {
			if is_nil_comparison {
				'orm__OperationKind__is_not_null'
			} else {
				'orm__OperationKind__neq'
			}
		}
		.eq {
			if is_nil_comparison {
				'orm__OperationKind__is_null'
			} else {
				'orm__OperationKind__eq'
			}
		}
		.lt {
			'orm__OperationKind__lt'
		}
		.gt {
			'orm__OperationKind__gt'
		}
		.ge {
			'orm__OperationKind__ge'
		}
		.le {
			'orm__OperationKind__le'
		}
		.key_like {
			'orm__OperationKind__orm_like'
		}
		.key_ilike {
			'orm__OperationKind__orm_ilike'
		}
		.key_is {
			'orm__OperationKind__is_null'
		}
		.not_is {
			'orm__OperationKind__is_not_null'
		}
		.key_in {
			'orm__OperationKind__in'
		}
		.not_in {
			'orm__OperationKind__not_in'
		}
		else {
			'orm__OperationKind__eq'
		}
	}
}

fn (g &Gen) resolve_sql_query_data_expr(expr ast.Expr) ?ast.SqlQueryDataExpr {
	mut current := expr
	for {
		current = current.remove_par()
		mut next_expr := current
		mut has_next_expr := false
		match current {
			ast.SqlQueryDataExpr {
				return current as ast.SqlQueryDataExpr
			}
			ast.Ident {
				obj := current.obj
				match obj {
					ast.Var {
						if obj.is_mut {
							return none
						}
						next_expr = obj.expr
						has_next_expr = true
					}
					ast.ConstField {
						next_expr = obj.expr
						has_next_expr = true
					}
					else {}
				}
			}
			else {
				return none
			}
		}

		if has_next_expr {
			current = next_expr
			continue
		}
		return none
	}
	return none
}

fn (mut g Gen) emit_sql_query_data(node ast.SqlQueryDataExpr, resolve_columns bool) string {
	query_var := g.new_tmp_var()
	g.writeln('orm__QueryData ${query_var} = (orm__QueryData){')
	g.indent++
	g.writeln('.fields = builtin____new_array_with_default_noscan(0, 0, sizeof(string), 0),')
	g.writeln('.data = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0),')
	g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.parentheses = builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0),')
	g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.auto_fields = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.indent--
	g.writeln('};')
	g.emit_sql_query_data_items(query_var, node.items, resolve_columns)
	return query_var
}

fn (mut g Gen) emit_dynamic_sql_query_data(expr ast.Expr) string {
	node := g.resolve_sql_query_data_expr(expr) or {
		verror('ORM: expected a dynamic query-data block or immutable alias')
	}
	return g.emit_sql_query_data(node, true)
}

fn (mut g Gen) emit_sql_query_data_items(query_var string, items []ast.SqlQueryDataItem, resolve_columns bool) {
	for item in items {
		match item {
			ast.SqlQueryDataLeaf {
				g.emit_sql_query_data_leaf(query_var, item, resolve_columns)
			}
			ast.SqlQueryDataIf {
				for idx, branch in item.branches {
					is_else := branch.cond is ast.EmptyExpr
					if idx == 0 {
						if is_else {
							g.writeln('else {')
						} else {
							g.write('if (')
							g.expr(branch.cond)
							g.writeln(') {')
						}
					} else if is_else {
						g.writeln('else {')
					} else {
						g.write('else if (')
						g.expr(branch.cond)
						g.writeln(') {')
					}
					g.indent++
					g.emit_sql_query_data_items(query_var, branch.items, resolve_columns)
					g.indent--
					g.writeln('}')
				}
			}
		}
	}
}

fn (mut g Gen) emit_sql_query_data_leaf(query_var string, node ast.SqlQueryDataLeaf, resolve_columns bool) {
	mut expr := node.expr
	expr_ := expr.remove_par()
	if expr_ is ast.InfixExpr {
		mut field_name := sql_query_data_field_name(expr_.left)
		if resolve_columns {
			field := g.get_orm_current_table_field(field_name) or {
				verror('field "${field_name}" does not exist on "${g.sql_table_name}"')
			}
			field_name = g.get_orm_column_name_from_struct_field(field)
		}
		is_nil_comparison := expr_.right is ast.Nil && expr_.op in [.eq, .ne]
		ignore_rhs := expr_.op in [.key_is, .not_is] || is_nil_comparison
		g.writeln('if (${query_var}.fields.len > 0) {')
		g.indent++
		g.writeln('builtin__array_push(&${query_var}.is_and, _MOV((bool[1]){ true }));')
		g.indent--
		g.writeln('}')
		g.writeln('builtin__array_push(&${query_var}.fields, _MOV((string[1]){ _S("${field_name}") }));')
		g.writeln('builtin__array_push(&${query_var}.kinds, _MOV((orm__OperationKind[1]){ ${sql_query_data_op_kind(expr_)} }));')
		if !ignore_rhs {
			g.write('builtin__array_push(&${query_var}.data, _MOV((orm__Primitive[1]){')
			g.write_orm_expr_to_primitive(expr_.right)
			g.writeln('}));')
		}
	} else {
		verror('ORM: dynamic query-data leaf must be an infix expression')
	}
}

// All databases that are supported by ORM
// must implement the `Connection` interface from the file `vlib/orm/orm.v`
// The main function of the cgen for ORM is to identify the current database type,
// generate the necessary parameters for invoking its methods,
// and correctly process the results, saving them to a variable that the user can access.
// Essentially, it involves a desugaring of the ORM syntax.
// For example, if you write:
// ```v
// sql db {
// 	select from User
// }
// ```
// cgen will write calling the function `select` of the needed database.
// If you use sqlite, it calls `select` from `vlib/db/sqlite/orm.v`

// sql_select_expr writes C code that calls ORM functions for selecting objects
// from the database, which is used by the `select` query.
fn (mut g Gen) sql_select_expr(node ast.SqlExpr) {
	// users :=
	left := g.go_before_last_stmt()
	connection_var_name := g.new_tmp_var()

	g.writeln('')
	g.write_orm_connection_init(connection_var_name, &node.db_expr)
	result_var := g.new_tmp_var()
	result_c_typ := g.styp(node.typ)
	g.writeln('${result_c_typ} ${result_var};')
	g.write_orm_select(node, connection_var_name, result_var)
	unwrapped_c_typ := g.styp(node.typ.clear_flag(.result))
	g.write('${left} *(${unwrapped_c_typ}*)${result_var}.data')
}

fn (mut g Gen) sql_insert_expr(node ast.SqlExpr) {
	left := g.go_before_last_stmt()
	g.writeln('')
	connection_var_name := g.new_tmp_var()
	g.write_orm_connection_init(connection_var_name, &node.db_expr)
	table_name := g.get_table_name_by_struct_type(node.table_expr.typ)
	table_attrs := g.get_table_attrs_by_struct_type(node.table_expr.typ)
	result_var_name := g.new_tmp_var()
	g.sql_table_name = g.table.sym(node.table_expr.typ).name
	g.sql_table_typ = node.table_expr.typ

	// orm_insert needs an SqlStmtLine, build it from SqlExpr (most nodes are the same)
	hack_stmt_line := g.build_sql_stmt_line_from_sql_expr(node)
	g.write_orm_insert(hack_stmt_line, table_name, connection_var_name, result_var_name,
		node.or_expr, table_attrs)

	g.write2(left,
		'orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object)')
}

fn (mut g Gen) build_sql_stmt_line_from_sql_expr(node ast.SqlExpr) ast.SqlStmtLine {
	mut sub_structs := map[string]ast.SqlStmtLine{}
	for key, sub in node.sub_structs {
		sub_structs[key] = g.build_sql_stmt_line_from_sql_expr(sub)
	}
	return ast.SqlStmtLine{
		object_var:  node.inserted_var
		fields:      node.fields
		table_expr:  node.table_expr
		sub_structs: sub_structs
		scope:       node.scope
	}
}

// sql_stmt writes C code that calls ORM functions for
// performing various database operations such as creating and dropping tables,
// as well as inserting and updating objects.
// Can contain several queries. For example:
// ```v
// sql db {
// 	create table User
// 	insert user into User
// }!
// ```
// NOTE: Currently, in ORM only the `select` query is an expression.
// The others are statements.
fn (mut g Gen) sql_stmt(node ast.SqlStmt) {
	connection_var_name := g.new_tmp_var()

	g.write_orm_connection_init(connection_var_name, &node.db_expr)

	for line in node.lines {
		g.sql_stmt_line(line, connection_var_name, node.or_expr)
	}
}

// sql_stmt_line writes C code that calls ORM functions for
// performing various database operations such as creating and dropping tables,
// as well as inserting and updating objects.
// It is part of a multi-line query. For example, `create table User`
fn (mut g Gen) sql_stmt_line(stmt_line ast.SqlStmtLine, connection_var_name string, or_expr ast.OrExpr) {
	g.sql_last_stmt_out_len = g.out.len
	mut node := stmt_line
	table_name := g.get_table_name_by_struct_type(node.table_expr.typ)
	table_attrs := g.get_table_attrs_by_struct_type(node.table_expr.typ)
	result_var_name := g.new_tmp_var()
	g.sql_table_name = g.table.sym(node.table_expr.typ).name
	g.sql_table_typ = node.table_expr.typ

	if node.kind != .create {
		node.fields = g.filter_struct_fields_by_orm_attrs(node.fields)
	}

	if node.kind == .create {
		g.write_orm_create_table(node, table_name, connection_var_name, result_var_name,
			table_attrs)
	} else if node.kind == .drop {
		g.write_orm_drop_table(node, table_name, connection_var_name, result_var_name, table_attrs)
	} else if node.kind == .insert {
		g.write_orm_insert(node, table_name, connection_var_name, result_var_name, or_expr,
			table_attrs)
	} else if node.kind == .upsert {
		g.write_orm_upsert(node, table_name, connection_var_name, result_var_name, table_attrs)
	} else if node.kind == .update {
		g.write_orm_update(node, table_name, connection_var_name, result_var_name, table_attrs)
	} else if node.kind == .delete {
		g.write_orm_delete(node, table_name, connection_var_name, result_var_name, table_attrs)
	}

	g.or_block(result_var_name, or_expr, ast.int_type.set_flag(.result))
}

// write_orm_connection_init writes C code that saves the database connection
// into a variable for later use in ORM queries.
fn (mut g Gen) write_orm_connection_init(connection_var_name string, db_expr &ast.Expr) {
	db_expr_type := g.get_db_expr_type(db_expr) or { verror('ORM: unknown db type for ${db_expr}') }

	mut db_ctype_name := g.styp(db_expr_type.clear_flag(.shared_f))
	is_pointer := db_ctype_name.ends_with('*')
	reference_sign := if is_pointer { '' } else { '&' }
	db_ctype_name = db_ctype_name.trim_right('*')

	g.writeln('// ORM')
	g.write('orm__Connection ${connection_var_name} = ')

	if db_ctype_name == 'orm__Connection' {
		g.expr(db_expr)
		g.writeln(';')
	} else {
		g.write('(orm__Connection){._${db_ctype_name} = ${reference_sign}')
		if db_expr_type.has_flag(.shared_f) {
			g.write('&')
		}
		g.expr(db_expr)
		if db_expr_type.has_flag(.shared_f) {
			g.write('->val')
		}
		g.writeln(', ._typ = _orm__Connection_${db_ctype_name}_index};')
	}
}

// write_orm_table_struct writes C code for the orm.Table struct
fn (mut g Gen) write_orm_table_struct(typ ast.Type) {
	table_name := g.get_table_name_by_struct_type(typ)
	table_attrs := g.get_table_attrs_by_struct_type(typ)

	g.writeln('((orm__Table){')
	g.indent++
	g.writeln('.name = _S("${table_name}"),')
	g.writeln('.attrs = builtin__new_array_from_c_array(${table_attrs.len}, ${table_attrs.len}, sizeof(VAttribute),')
	g.indent++

	if table_attrs.len > 0 {
		g.write('_MOV((VAttribute[${table_attrs.len}]){')
		g.indent++
		for attr in table_attrs {
			g.write('(VAttribute){')
			g.indent++
			name1 := util.smart_quote(attr.name, false)
			name := cescape_nonascii(name1)
			g.write(' .name = _S("${name}"),')
			g.write(' .has_arg = ${attr.has_arg},')
			arg1 := util.smart_quote(attr.arg, false)
			arg := cescape_nonascii(arg1)
			g.write(' .arg = _S("${arg}"),')
			g.write(' .kind = ${int(attr.kind)},')
			g.indent--
			g.write('},')
		}
		g.indent--
		g.writeln('})')
	} else {
		g.writeln('NULL // No attrs')
	}
	g.indent--
	g.writeln(')')
	g.indent--
	g.write('})')
}

// write_orm_create_table writes C code that calls ORM functions for creating tables.
fn (mut g Gen) write_orm_create_table(node ast.SqlStmtLine, table_name string, connection_var_name string,
	result_var_name string, _ []ast.Attr) {
	g.writeln('// sql { create table `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_create(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('builtin__new_array_from_c_array(${node.fields.len}, ${node.fields.len}, sizeof(orm__TableField),')
	g.indent++

	if node.fields.len > 0 {
		g.writeln('_MOV((orm__TableField[${node.fields.len}]){')
		g.indent++

		for field in node.fields {
			g.writeln('// `${table_name}`.`${field.name}`')
			// Safety check: ensure field type is valid (not 0 and not still generic)
			if field.typ == 0 || field.typ.has_flag(.generic) {
				verror('ORM: field `${field.name}` in table `${table_name}` has unresolved type - this may be due to using a generic struct that was not properly instantiated')
			}
			final_field_typ := g.table.final_type(field.typ)
			sym := g.table.sym(final_field_typ)
			typ := match true {
				sym.name == 'time.Time' { '_const_orm__time_' }
				sym.kind == .enum { '_const_orm__enum_' }
				else { final_field_typ.idx().str() }
			}

			g.writeln('(orm__TableField){')
			g.indent++
			g.writeln('.name = _S("${field.name}"),')
			g.writeln('.typ = ${typ}, // `${sym.name}`')
			g.writeln('.is_arr = ${sym.kind == .array}, ')
			g.writeln('.nullable = ${final_field_typ.has_flag(.option)},')
			g.writeln('.default_val = (string){ .str = (byteptr) "${field.default_val}", .is_lit = 1 },')
			g.writeln('.attrs = builtin__new_array_from_c_array(${field.attrs.len}, ${field.attrs.len}, sizeof(VAttribute),')
			g.indent++

			if field.attrs.len > 0 {
				g.write('_MOV((VAttribute[${field.attrs.len}]){')
				g.indent++
				for attr in field.attrs {
					g.write('(VAttribute){')
					g.indent++
					name1 := util.smart_quote(attr.name, false)
					name := cescape_nonascii(name1)
					g.write(' .name = _S("${name}"),')
					g.write(' .has_arg = ${attr.has_arg},')
					arg1 := util.smart_quote(attr.arg, false)
					arg := cescape_nonascii(arg1)
					g.write(' .arg = _S("${arg}"),')
					g.write(' .kind = ${int(attr.kind)},')
					g.indent--
					g.write('},')
				}
				g.indent--
				g.writeln('})')
			} else {
				g.writeln('NULL // No attrs')
			}
			g.indent--
			g.writeln(')')
			g.indent--
			g.writeln('},')
		}

		g.indent--
		g.writeln('})')
	} else {
		g.writeln('NULL')
	}

	g.indent--
	g.writeln(')')
	g.indent--
	g.writeln(');')
}

// write_orm_drop_table writes C code that calls ORM functions for dropping tables.
fn (mut g Gen) write_orm_drop_table(node ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, _ []ast.Attr) {
	g.writeln('// sql { drop table `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_drop(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.indent--
	g.writeln(');')
}

// write_orm_insert writes C code that calls ORM functions for inserting structs into a table.
fn (mut g Gen) write_orm_insert(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string,
	or_expr &ast.OrExpr, _ []ast.Attr) {
	last_ids_variable_name := g.new_tmp_var()

	g.writeln('Array_orm__Primitive ${last_ids_variable_name} = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0);')
	g.write_orm_insert_with_last_ids(node, connection_var_name, table_name, last_ids_variable_name,
		result_var_name, '', '', or_expr)
}

fn (mut g Gen) write_orm_upsert(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string,
	table_attrs []ast.Attr) {
	g.writeln('// sql { upsert into `${table_name}` }')
	fields := node.fields
	auto_fields := get_auto_field_idxs(fields)
	mut inserting_object_type := ast.void_type
	mut member_access_type := '.'
	if node.scope != unsafe { nil } {
		if inserting_object := node.scope.find(node.object_var) {
			if inserting_object.typ.is_ptr() {
				member_access_type = '->'
			}
			inserting_object_type = g.orm_inserting_object_type(inserting_object)
		} else {
			inserting_object_type = node.table_expr.typ
		}
	}
	inserting_object_sym := g.table.sym(inserting_object_type)
	data_var_name := g.new_tmp_var()
	g.writeln('orm__QueryData ${data_var_name} = (orm__QueryData){')
	g.indent++
	if fields.len > 0 {
		g.writeln('.fields = builtin__new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
		g.indent++
		g.writeln('_MOV((string[${fields.len}]){')
		g.indent++
		for field in fields {
			g.writeln('_S("${g.get_orm_column_name_from_struct_field(field)}"),')
		}
		g.indent--
		g.writeln('})')
		g.indent--
		g.writeln('),')
		g.writeln('.data = builtin__new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(orm__Primitive),')
		g.indent++
		g.writeln('_MOV((orm__Primitive[${fields.len}]){')
		g.indent++
		for field in fields {
			final_field_typ := g.table.final_type(field.typ)
			sym := g.table.sym(final_field_typ)
			mut typ := g.orm_primitive_field_name(field.typ)
			mut ctyp := sym.cname
			typ = vint2int(typ)
			var := '${node.object_var}${member_access_type}${orm_field_access_name(field.name)}'
			if final_field_typ.has_flag(.option) {
				g.writeln('${var}.state == 2 ? _const_orm__null_primitive : orm__${typ}_to_primitive(*(${ctyp}*)(${var}.data)),')
			} else if inserting_object_sym.kind == .sum_type {
				table_sym := g.table.sym(node.table_expr.typ)
				sum_type_var := '(*${node.object_var}._${table_sym.cname})${member_access_type}${orm_field_access_name(field.name)}'
				g.writeln('orm__${typ}_to_primitive(${sum_type_var}),')
			} else {
				g.writeln('orm__${typ}_to_primitive(${var}),')
			}
		}
		g.indent--
		g.writeln('})')
		g.indent--
		g.writeln('),')
	} else {
		g.writeln('.fields = builtin____new_array_with_default_noscan(0, 0, sizeof(string), 0),')
		g.writeln('.data = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0),')
	}
	g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	if auto_fields.len > 0 {
		g.writeln('.auto_fields = builtin__new_array_from_c_array(${auto_fields.len}, ${auto_fields.len}, sizeof(${ast.int_type_name}),')
		g.indent++
		g.write('_MOV((${ast.int_type_name}[${auto_fields.len}]){')
		for i in auto_fields {
			g.write(' ${i},')
		}
		g.writeln(' })),')
		g.indent--
	} else {
		g.writeln('.auto_fields = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	}
	g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.writeln('.parentheses = builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0),')
	g.indent--
	g.writeln('};')
	conflict_groups := g.get_orm_upsert_conflict_groups(fields, table_attrs)
	conflict_groups_var_name := g.new_tmp_var()
	g.write('Array_Array_string ${conflict_groups_var_name} = ')
	g.write_orm_upsert_conflict_groups(conflict_groups)
	g.writeln(';')
	prepared_var_name := g.new_tmp_var()
	g.writeln('orm__UpsertData ${prepared_var_name} = orm__prepare_upsert(${data_var_name}, ${conflict_groups_var_name});')
	g.writeln('${result_name}_void ${result_var_name};')
	g.writeln('if (!${prepared_var_name}.valid) {')
	g.indent++
	g.write('${result_var_name} = orm__upsert_missing_conflict_error(')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(');')
	g.indent--
	g.writeln('} else {')
	g.indent++
	select_result_var_name := g.new_tmp_var()
	g.writeln('${result_name}_Array_Array_orm__Primitive ${select_result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_select(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('(orm__SelectConfig){')
	g.indent++
	g.write('.table = ')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('.aggregate_kind = orm__AggregateKind__count,')
	g.writeln('.aggregate_field = _S(""),')
	g.writeln('.has_where = true,')
	g.writeln('.has_order = false,')
	g.writeln('.order = _S(""),')
	g.writeln('.order_type = orm__OrderType__asc,')
	g.writeln('.has_limit = false,')
	g.writeln('.primary = _S("id"),')
	g.writeln('.has_offset = false,')
	g.writeln('.has_distinct = false,')
	g.writeln('.fields = builtin____new_array_with_default_noscan(0, 0, sizeof(string), 0),')
	g.writeln('.select_exprs = builtin____new_array_with_default_noscan(0, 0, sizeof(string), 0),')
	g.writeln('.types = builtin__new_array_from_c_array(1, 1, sizeof(${ast.int_type_name}), _MOV((${ast.int_type_name}[1]){ ${ast.int_type.idx()}, })),')
	g.writeln('.joins = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__JoinConfig), 0),')
	g.indent--
	g.writeln('},')
	g.writeln('(orm__QueryData){')
	g.indent++
	g.writeln('.fields = builtin____new_array_with_default_noscan(0, 0, sizeof(string), 0),')
	g.writeln('.data = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0),')
	g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.auto_fields = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.writeln('.parentheses = builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0),')
	g.indent--
	g.writeln('},')
	g.writeln('${prepared_var_name}.where')
	g.indent--
	g.writeln(');')
	g.writeln('${result_var_name}.is_error = ${select_result_var_name}.is_error;')
	g.writeln('${result_var_name}.err = ${select_result_var_name}.err;')
	g.writeln('if (!${result_var_name}.is_error) {')
	g.indent++
	count_rows_var_name := g.new_tmp_var()
	count_var_name := g.new_tmp_var()
	g.writeln('Array_Array_orm__Primitive ${count_rows_var_name} = (*(Array_Array_orm__Primitive*)${select_result_var_name}.data);')
	g.writeln('${ast.int_type_name} ${count_var_name} = orm__upsert_count(${count_rows_var_name});')
	g.writeln('if (${count_var_name} == 0) {')
	g.indent++
	g.writeln('${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_insert(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('${data_var_name}')
	g.indent--
	g.writeln(');')
	g.indent--
	g.writeln('} else if (${count_var_name} == 1) {')
	g.indent++
	g.writeln('if (${prepared_var_name}.insert_data.fields.len == 0) {')
	g.indent++
	g.writeln('${result_var_name} = (${result_name}_void){0};')
	g.indent--
	g.writeln('} else {')
	g.indent++
	g.writeln('${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_update(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('${prepared_var_name}.insert_data,')
	g.writeln('${prepared_var_name}.where')
	g.indent--
	g.writeln(');')
	g.indent--
	g.writeln('}')
	g.indent--
	g.writeln('} else {')
	g.indent++
	g.write('${result_var_name} = orm__upsert_ambiguous_error(')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(');')
	g.indent--
	g.writeln('}')
	g.indent--
	g.writeln('}')
	g.indent--
	g.writeln('}')
}

// write_orm_update writes C code that calls ORM functions for updating rows.
fn (mut g Gen) write_orm_update(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, _ []ast.Attr) {
	mut dynamic_update_data_var := ''
	if node.is_dynamic {
		dynamic_update_data_var = g.emit_dynamic_sql_query_data(node.update_data_expr)
	}
	g.writeln('// sql { update `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_update(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	if node.is_dynamic {
		g.writeln('${dynamic_update_data_var},')
	} else {
		g.writeln('(orm__QueryData){')
		g.indent++
		g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
		g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
		g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
		g.writeln('.parentheses = builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0),')

		if node.updated_columns.len > 0 {
			g.writeln('.fields = builtin__new_array_from_c_array(${node.updated_columns.len}, ${node.updated_columns.len}, sizeof(string),')
			g.indent++
			g.writeln('_MOV((string[${node.updated_columns.len}]){')
			g.indent++
			for field in node.updated_columns {
				g.writeln('_S("${field}"),')
			}
			g.indent--
			g.writeln('})')
			g.indent--
		} else {
			g.writeln('.fields = builtin____new_array_with_default_noscan(${node.updated_columns.len}, ${node.updated_columns.len}, sizeof(string), 0')
		}

		g.writeln2('),',
			'.data = builtin__new_array_from_c_array(${node.update_exprs.len}, ${node.update_exprs.len}, sizeof(orm__Primitive),')

		if node.update_exprs.len > 0 {
			g.indent++
			g.writeln('_MOV((orm__Primitive[${node.update_exprs.len}]){')
			g.indent++
			for i, e in node.update_exprs {
				column := node.updated_columns[i]
				if field := g.get_orm_field_by_column_name(node.fields, column) {
					final_field_typ := g.table.final_type(field.typ.clear_flag(.option))
					sym := g.table.sym(final_field_typ)
					if sym.kind == .struct && sym.name != 'time.Time' {
						g.write_orm_struct_field_expr_to_primitive(field, e)
						continue
					}
				}
				g.write_orm_expr_to_primitive(e)
			}
			g.indent--
			g.writeln('})')
			g.indent--
		}

		g.writeln('),')
		g.indent--
		g.writeln('},')
	}
	g.write_orm_where(node.where_expr)
	g.indent--
	g.writeln(');')
}

// write_orm_delete writes C code that calls ORM functions for deleting rows.
fn (mut g Gen) write_orm_delete(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, _ []ast.Attr) {
	g.writeln('// sql { delete from `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method__v_delete(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.write_orm_where(node.where_expr)
	g.indent--
	g.writeln(');')
}

// write_orm_insert_with_last_ids writes C code that calls ORM functions for
// inserting a struct into a table, saving inserted `id` into a passed variable.
fn (mut g Gen) write_orm_insert_with_last_ids(node ast.SqlStmtLine, connection_var_name string, table_name string,
	last_ids_arr string, res string, pid string, fkey string, or_expr ast.OrExpr) {
	mut subs := []ast.SqlStmtLine{}

	mut subs_unwrapped_c_typ := []string{}
	mut arrs := []ast.SqlStmtLine{}
	mut fkeys := []string{}
	mut field_names := []string{}
	mut opt_fields := []int{}

	for field in node.fields {
		final_field_typ := g.table.final_type(field.typ)
		sym := g.table.sym(final_field_typ)
		if sym.kind == .struct && sym.name != 'time.Time' {
			if field.name in node.sub_structs {
				subs << unsafe { node.sub_structs[field.name] }

				unwrapped_c_typ := g.styp(final_field_typ.clear_flag(.option))
				subs_unwrapped_c_typ << if final_field_typ.has_flag(.option) {
					unwrapped_c_typ
				} else {
					''
				}
			}
		} else if sym.kind == .array {
			// Handle foreign keys
			if attr := field.attrs.find_first('fkey') {
				fkeys << attr.arg
			} else {
				verror('missing fkey attribute')
			}
			if final_field_typ.has_flag(.option) {
				opt_fields << arrs.len
			}
			if field.name in node.sub_structs {
				arrs << unsafe { node.sub_structs[field.name] }
			}
			field_names << field.name
		}
	}

	fields := g.orm_non_array_fields(node.fields)
	auto_fields := get_auto_field_idxs(fields)

	primary_field := g.get_orm_struct_primary_field(fields) or { ast.StructField{} }

	is_serial := (primary_field.attrs.contains_arg('sql', 'serial')
		|| primary_field.attrs.contains('serial')) && primary_field.typ == ast.int_type

	mut inserting_object_type := ast.void_type
	mut member_access_type := '.'
	if node.scope != unsafe { nil } {
		if inserting_object := node.scope.find(node.object_var) {
			if inserting_object.typ.is_ptr() {
				member_access_type = '->'
			}
			inserting_object_type = g.orm_inserting_object_type(inserting_object)
		} else {
			inserting_object_type = node.table_expr.typ
		}
	}

	inserting_object_sym := g.table.sym(inserting_object_type)
	for i, mut sub in subs {
		if subs_unwrapped_c_typ[i].len > 0 {
			var := '${node.object_var}${member_access_type}${sub.object_var}'
			g.writeln('if(${var}.state == 0) {')
			g.indent++
			sub.object_var = '(*(${subs_unwrapped_c_typ[i]}*)${node.object_var}${member_access_type}${sub.object_var}.data)'
		} else {
			sub.object_var = '${node.object_var}${member_access_type}${sub.object_var}'
		}
		g.sql_stmt_line(sub, connection_var_name, or_expr)
		g.writeln('builtin__array_push(&${last_ids_arr}, _MOV((orm__Primitive[1]){')
		g.writeln('\torm__int_to_primitive(orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object))}));')
		if subs_unwrapped_c_typ[i].len > 0 {
			g.indent--
			g.writeln('} else {')
			g.writeln('\tbuiltin__array_push(&${last_ids_arr}, _MOV((orm__Primitive[1]){ _const_orm__null_primitive }));')
			g.writeln('}')
		}
	}

	g.writeln('// sql { insert into `${table_name}` }')
	g.writeln('${result_name}_void ${res} = orm__Connection_name_table[${connection_var_name}._typ]._method_insert(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('(orm__QueryData){')
	g.indent++
	g.writeln('.fields = builtin__new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
	g.indent++

	if fields.len > 0 {
		g.writeln('_MOV((string[${fields.len}]){ ')
		g.indent++
		for f in fields {
			g.writeln('_S("${g.get_orm_column_name_from_struct_field(f)}"),')
		}
		g.indent--
		g.writeln('})')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln('),')

	g.writeln('.data = builtin__new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(orm__Primitive),')
	g.indent++

	if fields.len > 0 {
		g.writeln('_MOV((orm__Primitive[${fields.len}]){')
		g.indent++
		mut structs := 0
		for field in fields {
			if field.name == fkey {
				g.writeln('${pid}, ')
				continue
			}
			final_field_typ := g.table.final_type(field.typ)
			mut sym := g.table.sym(final_field_typ)
			mut typ := g.orm_primitive_field_name(field.typ)
			mut ctyp := sym.cname
			if sym.kind == .struct && ctyp != 'time__Time' {
				g.writeln('(*(orm__Primitive*) builtin__array_get(${last_ids_arr}, ${structs})),')
				structs++
				continue
			}
			// fields processed hereafter can be NULL...
			typ = vint2int(typ)
			var := '${node.object_var}${member_access_type}${orm_field_access_name(field.name)}'
			if final_field_typ.has_flag(.option) {
				g.writeln('${var}.state == 2? _const_orm__null_primitive : orm__${typ}_to_primitive(*(${ctyp}*)(${var}.data)),')
			} else if inserting_object_sym.kind == .sum_type {
				table_sym := g.table.sym(node.table_expr.typ)
				sum_type_var := '(*${node.object_var}._${table_sym.cname})${member_access_type}${orm_field_access_name(field.name)}'
				g.writeln('orm__${typ}_to_primitive(${sum_type_var}),')
			} else {
				g.writeln('orm__${typ}_to_primitive(${var}),')
			}
		}
		g.indent--
		g.writeln('})')
	} else {
		g.write('NULL')
	}
	g.indent--
	g.writeln('),')
	g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	if auto_fields.len > 0 {
		g.writeln('.auto_fields = builtin__new_array_from_c_array(${auto_fields.len}, ${auto_fields.len}, sizeof(${ast.int_type_name}),')
		g.indent++
		g.write('_MOV((${ast.int_type_name}[${auto_fields.len}]){')
		for i in auto_fields {
			g.write(' ${i},')
		}
		g.writeln(' })),')
		g.indent--
	} else {
		g.writeln('.auto_fields = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	}
	g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.indent--
	g.writeln('}')
	g.indent--
	g.writeln(');')
	// Validate main insertion success otherwise, handled and propagated error.
	g.or_block(res, or_expr, ast.int_type.set_flag(.result))

	if arrs.len > 0 {
		mut id_name := g.new_tmp_var()
		if is_serial {
			// use last_insert_id if current struct has `int [primary; sql: serial]`
			g.writeln('orm__Primitive ${id_name} = orm__int_to_primitive(orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object));')
		} else {
			// else use the primary key value
			mut typ := g.orm_primitive_field_name(primary_field.typ)
			typ = vint2int(typ)
			g.writeln('orm__Primitive ${id_name} = orm__${typ}_to_primitive(${node.object_var}${member_access_type}${orm_field_access_name(primary_field.name)});')
		}
		for i, mut arr in arrs {
			idx := g.new_tmp_var()
			ctyp := g.styp(arr.table_expr.typ)
			is_option := opt_fields.contains(i)
			if is_option {
				g.writeln('for (${ast.int_type_name} ${idx} = 0; ${node.object_var}${member_access_type}${arr.object_var}.state != 2 && ${idx} < (*(Array_${ctyp}*)${node.object_var}${member_access_type}${arr.object_var}.data).len; ${idx}++) {')
			} else {
				g.writeln('for (${ast.int_type_name} ${idx} = 0; ${idx} < ${node.object_var}${member_access_type}${arr.object_var}.len; ${idx}++) {')
			}
			g.indent++
			last_ids := g.new_tmp_var()
			res_ := g.new_tmp_var()
			tmp_var := g.new_tmp_var()
			g.writeln('Array_orm__Primitive ${last_ids} = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0);')
			if is_option {
				g.writeln('${ctyp} ${tmp_var} = (*(${ctyp}*)builtin__array_get(*(Array_${ctyp}*)${node.object_var}${member_access_type}${arr.object_var}.data, ${idx}));')
			} else {
				g.writeln('${ctyp} ${tmp_var} = (*(${ctyp}*)builtin__array_get(${node.object_var}${member_access_type}${arr.object_var}, ${idx}));')
			}
			arr.object_var = tmp_var
			mut fff := []ast.StructField{}
			for f in arr.fields {
				mut skip := false
				for attr in f.attrs {
					if attr.name == 'skip' {
						skip = true
					}
					if attr.name == 'sql' && attr.arg == '-' {
						skip = true
					}
				}
				if !skip {
					fff << f
				}
			}
			arr.fields = fff.clone()
			unsafe { fff.free() }
			g.write_orm_insert_with_last_ids(arr, connection_var_name,
				g.get_table_name_by_struct_type(arr.table_expr.typ), last_ids, res_, id_name,
				fkeys[i], or_expr)
			g.indent--
			g.writeln('}')
		}
	}
}

// write_orm_expr_to_primitive writes C code for casting expressions into a primitive type
// by checking support expressions and their types.
fn (mut g Gen) write_orm_expr_to_primitive(expr ast.Expr) {
	match expr {
		ast.InfixExpr {
			g.write_orm_primitive(g.table.find_type('orm.InfixType'), expr)
		}
		ast.StringLiteral {
			g.write_orm_primitive(ast.string_type, expr)
		}
		ast.StringInterLiteral {
			g.write_orm_primitive(ast.string_type, expr)
		}
		ast.IntegerLiteral {
			g.write_orm_primitive(ast.int_type, expr)
		}
		ast.BoolLiteral {
			g.write_orm_primitive(ast.bool_type, expr)
		}
		ast.EnumVal {
			g.write_orm_primitive(ast.i64_type, expr)
		}
		ast.Ident {
			info := expr.info as ast.IdentVar
			g.write_orm_primitive(orm_expr_effective_type(expr, info.typ), expr)
		}
		ast.SelectorExpr {
			g.write_orm_primitive(orm_expr_effective_type(expr, expr.typ), expr)
		}
		ast.CallExpr {
			g.write_orm_primitive(orm_expr_effective_type(expr, expr.return_type), expr)
		}
		ast.Nil {
			g.write_orm_primitive(ast.none_type, expr)
		}
		ast.IfExpr {
			g.write_orm_primitive(expr.typ, expr)
		}
		ast.None {
			g.write_orm_primitive(ast.none_type, expr)
		}
		else {
			verror('ORM: ${expr.type_name()} is not supported')
		}
	}
}

fn (mut g Gen) write_orm_struct_field_expr_to_primitive(field ast.StructField, expr ast.Expr) {
	if expr is ast.None || expr is ast.Nil {
		g.writeln('_const_orm__null_primitive,')
		return
	}
	final_field_typ := g.table.final_type(field.typ.clear_flag(.option))
	foreign_sym := g.table.sym(final_field_typ)
	foreign_info := foreign_sym.info as ast.Struct
	primary_field := g.get_orm_struct_primary_field(foreign_info.fields) or {
		verror('ORM: struct field `${field.name}` of type `${foreign_sym.name}` has no primary field')
	}
	g.write_orm_primitive(primary_field.typ, ast.SelectorExpr{
		pos:        expr.pos()
		field_name: primary_field.name
		expr:       expr
		expr_type:  orm_expr_effective_type(expr, field.typ).clear_flag(.option).clear_flag(.result)
		typ:        primary_field.typ
		scope:      unsafe { nil }
	})
}

fn orm_expr_effective_type(expr ast.Expr, typ ast.Type) ast.Type {
	return match expr {
		ast.CallExpr {
			if expr.or_block.kind != .absent {
				typ.clear_option_and_result()
			} else {
				typ
			}
		}
		ast.Ident {
			if expr.or_expr.kind != .absent {
				typ.clear_option_and_result()
			} else {
				typ
			}
		}
		ast.SelectorExpr {
			if expr.or_block.kind != .absent {
				typ.clear_option_and_result()
			} else {
				typ
			}
		}
		else {
			typ
		}
	}
}

// write_orm_primitive writes C code for casting expressions into a primitive type,
// which will be used in low-level database libs.
fn (mut g Gen) write_orm_primitive(t ast.Type, expr ast.Expr) {
	if t == 0 {
		verror('${g.file.path}:${expr.pos().line_nr + 1}: ORM: unknown type t == 0\nexpr: ${expr}\nlast SQL stmt:\n${g.out.after(g.sql_last_stmt_out_len)}')
	}
	final_field_typ := g.table.final_type(t)
	mut sym := g.table.sym(final_field_typ)
	mut typ := sym.cname
	if typ == 'orm__Primitive' {
		g.expr(expr)
		g.writeln(',')
		return
	}
	if typ == 'none' {
		g.writeln('_const_orm__null_primitive,')
		return
	}

	if expr is ast.InfixExpr {
		g.writeln('orm__infix_to_primitive((orm__InfixType){')
		g.indent++
		g.write('.name = _S("${expr.left}"),')
		mut kind := match expr.op {
			.plus {
				'orm__MathOperationKind__add'
			}
			.minus {
				'orm__MathOperationKind__sub'
			}
			.div {
				'orm__MathOperationKind__div'
			}
			.mul {
				'orm__MathOperationKind__mul'
			}
			else {
				''
			}
		}

		g.writeln(' .operator = ${kind},')
		g.write(' .right = ')
		g.write_orm_expr_to_primitive(expr.right)
		g.indent--
		g.writeln(' }),')
	} else {
		if typ == 'time__Time' {
			typ = 'time'
		}

		if t.has_flag(.option) {
			typ = 'option_${typ}'
		} else if g.table.final_sym(t.clear_flag(.option)).kind == .enum {
			typ = 'i64'
		} else if g.table.final_sym(t).kind == .array {
			typ = g.table.sym(g.table.final_type(t)).cname.to_lower()
		}
		typ = vint2int(typ)
		g.write('orm__${typ}_to_primitive(')
		if expr is ast.CallExpr {
			g.call_expr(expr)
		} else {
			g.left_is_opt = true
			g.expr(expr)
		}
		g.writeln('),')
	}
}

// write_orm_where writes C code that generates
// the `QueryData` structure for passing it into ORM methods.
fn (mut g Gen) write_orm_where(where_expr ast.Expr) {
	mut fields := []string{}
	mut kinds := []string{}
	mut parentheses := [][]int{}
	mut data := []ast.Expr{}
	mut is_ands := []bool{}

	g.writeln('// ORM where')

	g.writeln('(orm__QueryData){')
	g.indent++
	g.write_orm_where_expr(where_expr, mut fields, mut parentheses, mut kinds, mut data, mut
		is_ands)
	g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	if fields.len > 0 {
		g.writeln('.fields = builtin__new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
		g.indent++
		g.writeln('_MOV((string[${fields.len}]){')
		g.indent++
		for field in fields {
			g.writeln('_S("${field}"),')
		}
		g.indent--
		g.writeln('})')
		g.indent--
	} else {
		g.writeln('.fields = builtin____new_array_with_default_noscan(${fields.len}, ${fields.len}, sizeof(string), 0')
	}
	g.writeln('),')

	g.writeln('.data = builtin__new_array_from_c_array(${data.len}, ${data.len}, sizeof(orm__Primitive),')
	g.indent++
	if data.len > 0 {
		g.writeln('_MOV((orm__Primitive[${data.len}]){')
		g.indent++
		for e in data {
			g.write_orm_expr_to_primitive(e)
		}
		g.indent--
		g.writeln('})')
	} else {
		g.writeln('0')
	}
	g.indent--
	g.writeln('),')

	g.write('.parentheses = ')
	if parentheses.len > 0 {
		g.write('builtin__new_array_from_c_array(${parentheses.len}, ${parentheses.len}, sizeof(Array_${ast.int_type_name}), _MOV((Array_${ast.int_type_name}[${parentheses.len}]){')
		for par in parentheses {
			if par.len > 0 {
				g.write('builtin__new_array_from_c_array(${par.len}, ${par.len}, sizeof(${ast.int_type_name}), _MOV((${ast.int_type_name}[${par.len}]){')
				for val in par {
					g.write('${val},')
				}
				g.write('})),')
			} else {
				g.write('builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
			}
		}
		g.write('}))')
	} else {
		g.write('builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0)')
	}
	g.writeln(',')

	if kinds.len > 0 {
		g.writeln('.kinds = builtin__new_array_from_c_array(${kinds.len}, ${kinds.len}, sizeof(orm__OperationKind),')
		g.indent++
		g.writeln('_MOV((orm__OperationKind[${kinds.len}]){')
		g.indent++
		for k in kinds {
			g.writeln('${k},')
		}
		g.indent--
		g.writeln('})')
		g.indent--
	} else {
		g.write('.kinds = builtin____new_array_with_default_noscan(${kinds.len}, ${kinds.len}, sizeof(orm__OperationKind), 0')
	}
	g.writeln('),')

	if is_ands.len > 0 {
		g.write('.is_and = builtin__new_array_from_c_array(${is_ands.len}, ${is_ands.len}, sizeof(bool),')
		g.indent++
		g.writeln('_MOV((bool[${is_ands.len}]){')
		g.indent++
		for is_and in is_ands {
			g.writeln('${is_and},')
		}
		g.indent--
		g.write('})')
		g.indent--
	} else {
		g.write('.is_and = builtin____new_array_with_default_noscan(${is_ands.len}, ${is_ands.len}, sizeof(bool), 0')
	}
	g.indent--
	g.writeln2('),', '}')
}

// write_orm_where_expr writes C code that generates expression which is used in the `QueryData`.
fn (mut g Gen) write_orm_where_expr(expr ast.Expr, mut fields []string, mut parentheses [][]int, mut kinds []string,
	mut data []ast.Expr, mut is_and []bool) {
	match expr {
		ast.InfixExpr {
			g.sql_side = .left
			g.write_orm_where_expr(expr.left, mut fields, mut parentheses, mut kinds, mut data, mut
				is_and)
			is_nil_comparison := expr.right is ast.Nil && expr.op in [.eq, .ne]
			mut ignore_rhs := expr.op in [.key_is, .not_is] || is_nil_comparison
			mut kind := match expr.op {
				.ne {
					if is_nil_comparison {
						'orm__OperationKind__is_not_null'
					} else {
						'orm__OperationKind__neq'
					}
				}
				.eq {
					if is_nil_comparison {
						'orm__OperationKind__is_null'
					} else {
						'orm__OperationKind__eq'
					}
				}
				.lt {
					'orm__OperationKind__lt'
				}
				.gt {
					'orm__OperationKind__gt'
				}
				.ge {
					'orm__OperationKind__ge'
				}
				.le {
					'orm__OperationKind__le'
				}
				.key_like {
					'orm__OperationKind__orm_like'
				}
				.key_ilike {
					'orm__OperationKind__orm_ilike'
				}
				.key_is {
					'orm__OperationKind__is_null'
				}
				.not_is {
					'orm__OperationKind__is_not_null'
				}
				.key_in {
					'orm__OperationKind__in'
				}
				.not_in {
					'orm__OperationKind__not_in'
				}
				else {
					''
				}
			}

			if kind == '' {
				if expr.op == .logical_or {
					is_and << false
				} else if expr.op == .and {
					is_and << true
				} else {
					kind = 'orm__OperationKind__eq'
				}
			}
			if expr.left !is ast.InfixExpr && expr.right !is ast.InfixExpr && kind != '' {
				kinds << kind
			}
			if !ignore_rhs { // ignore rhs for unary ops and SQL NULL equality
				g.sql_side = .right
				g.write_orm_where_expr(expr.right, mut fields, mut parentheses, mut kinds, mut
					data, mut is_and)
			}
		}
		ast.ParExpr {
			mut par := [fields.len]
			g.write_orm_where_expr(expr.expr, mut fields, mut parentheses, mut kinds, mut data, mut
				is_and)
			par << fields.len - 1
			parentheses << par
		}
		ast.Ident {
			if g.sql_side == .left {
				field := g.get_orm_current_table_field(expr.name) or {
					verror('field "${expr.name}" does not exist on "${g.sql_table_name}"')
				}
				fields << g.get_orm_column_name_from_struct_field(field)
			} else {
				data << expr
			}
		}
		ast.StringLiteral {
			data << expr
		}
		ast.StringInterLiteral {
			data << expr
		}
		ast.IntegerLiteral {
			data << expr
		}
		ast.SelectorExpr {
			if g.comptime.is_comptime_selector_field_name(expr, 'name') {
				fields << g.comptime.comptime_for_field_value.name
			} else {
				data << expr
			}
		}
		ast.BoolLiteral {
			data << expr
		}
		ast.EnumVal {
			data << expr
		}
		ast.CallExpr {
			data << expr
		}
		ast.None {
			data << expr
		}
		else {}
	}
}

// write_orm_select writes C code that calls ORM functions for selecting rows,
// storing the result in the provided variable name
fn (mut g Gen) write_orm_select(node ast.SqlExpr, connection_var_name string, result_var string) {
	mut fields := []ast.StructField{}
	mut primary_field := g.get_orm_struct_primary_field(node.fields) or { ast.StructField{} }
	mut dynamic_where_var := ''

	for field in node.fields {
		mut skip := false
		for attr in field.attrs {
			if attr.name == 'skip' {
				skip = true
			}
			if attr.name == 'sql' && attr.arg == '-' {
				skip = true
			}
		}
		if !skip {
			fields << field
		}
	}

	select_result_var_name := g.new_tmp_var()
	table_name := g.get_table_name_by_struct_type(node.table_expr.typ)
	g.sql_table_name = g.table.sym(node.table_expr.typ).name
	g.sql_table_typ = node.table_expr.typ
	if node.has_where && node.is_dynamic {
		dynamic_where_var = g.emit_dynamic_sql_query_data(node.where_expr)
	}

	g.writeln('// sql { select from `${table_name}` }')
	g.writeln('${result_name}_Array_Array_orm__Primitive ${select_result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_select(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('(orm__SelectConfig){')
	g.indent++
	g.writeln('.table = ')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('.aggregate_kind = orm__AggregateKind__${node.aggregate_kind},')
	g.writeln('.aggregate_field = _S("${node.aggregate_field}"),')
	g.writeln('.has_where = ${node.has_where},')
	g.writeln('.has_order = ${node.has_order},')

	if node.has_order {
		g.write('.order = _S("')
		if node.order_expr is ast.Ident {
			field := g.get_orm_current_table_field(node.order_expr.name) or {
				verror('field "${node.order_expr.name}" does not exist on "${g.sql_table_name}"')
			}
			g.write(g.get_orm_column_name_from_struct_field(field))
		} else {
			g.expr(node.order_expr)
		}
		g.writeln('"),')
		if node.has_desc {
			g.writeln('.order_type = orm__OrderType__desc,')
		} else {
			g.writeln('.order_type = orm__OrderType__asc,')
		}
	}

	g.writeln('.has_limit = ${node.has_limit},')
	g.writeln('.has_offset = ${node.has_offset},')
	g.writeln('.has_distinct = ${node.has_distinct},')

	if primary_field.name != '' {
		g.writeln('.primary = _S("${primary_field.name}"),')
	}

	mut select_fields := g.orm_non_array_fields(fields)
	if node.aggregate_kind != .none {
		select_fields = fields.clone()
	}
	g.writeln('.fields = builtin__new_array_from_c_array(${select_fields.len}, ${select_fields.len}, sizeof(string),')
	g.indent++
	mut types := []string{}
	mut select_exprs := []string{}
	if select_fields.len > 0 {
		g.writeln('_MOV((string[${select_fields.len}]){')
		g.indent++
		for field in select_fields {
			column_name := g.get_orm_column_name_from_struct_field(field)
			g.writeln('_S("${column_name}"),')
			select_exprs << g.get_orm_select_expr_from_struct_field(field)
			mut final_field_typ := g.table.final_type(field.typ.clear_flag(.option))
			if node.aggregate_kind == .avg {
				final_field_typ = ast.f64_type
			} else if node.aggregate_kind == .count {
				final_field_typ = ast.int_type
			}
			sym := g.table.sym(final_field_typ)
			if sym.name == 'time.Time' {
				types << '_const_orm__time_'
				continue
			}
			if sym.kind == .struct {
				types << int(ast.int_type).str()
				continue
			} else if sym.kind == .enum {
				types << '_const_orm__enum_'
				continue
			}
			types << final_field_typ.idx().str()
		}
		g.indent--
		g.writeln('})')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln2('),',
		'.select_exprs = builtin__new_array_from_c_array(${select_exprs.len}, ${select_exprs.len}, sizeof(string),')
	g.indent++

	if select_exprs.len > 0 {
		g.writeln('_MOV((string[${select_exprs.len}]){')
		g.indent++
		for select_expr in select_exprs {
			expr1 := util.smart_quote(select_expr, false)
			expr := cescape_nonascii(expr1)
			g.writeln('_S("${expr}"),')
		}
		g.indent--
		g.writeln('})')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln2('),',
		'.types = builtin__new_array_from_c_array(${types.len}, ${types.len}, sizeof(${ast.int_type_name}),')
	g.indent++

	if types.len > 0 {
		g.write('_MOV((${ast.int_type_name}[${types.len}]){')
		for typ in types {
			g.write(' ${typ},')
		}
		g.writeln(' })')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln('),')
	// Generate JOIN clauses array
	g.write_orm_joins(node.joins)
	g.indent--
	g.writeln('},')

	mut exprs := []ast.Expr{}
	if node.has_limit {
		exprs << node.limit_expr
	}
	if node.has_offset {
		exprs << node.offset_expr
	}

	g.writeln('(orm__QueryData) {')
	g.indent++
	g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.writeln('.parentheses = builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0),')
	if exprs.len > 0 {
		g.write('.data = builtin__new_array_from_c_array(${exprs.len}, ${exprs.len}, sizeof(orm__Primitive),')
		g.write(' _MOV((orm__Primitive[${exprs.len}]){')
		for e in exprs {
			g.write_orm_expr_to_primitive(e)
		}
		g.writeln('})')
	} else {
		g.writeln('.data = builtin____new_array_with_default_noscan(${exprs.len}, ${exprs.len}, sizeof(orm__Primitive), 0')
	}
	g.indent--
	g.writeln(')},')

	if node.has_where {
		if node.is_dynamic {
			g.writeln('${dynamic_where_var}')
		} else {
			g.write_orm_where(node.where_expr)
		}
	} else {
		g.writeln('(orm__QueryData) {')
		g.indent++
		g.writeln('.types = builtin____new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
		g.writeln('.kinds = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
		g.writeln('.is_and = builtin____new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
		g.writeln('.parentheses = builtin____new_array_with_default_noscan(0, 0, sizeof(Array_${ast.int_type_name}), 0),')
		g.writeln('.data = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0)')
		g.indent--
		g.writeln('}')
	}
	g.indent--
	g.writeln(');')

	g.writeln('${result_var}.is_error = ${select_result_var_name}.is_error;')
	g.writeln('${result_var}.err = ${select_result_var_name}.err;')
	g.or_block(result_var, node.or_expr, node.typ)

	// or_block could have ended in return (longjump) or could have
	// yielded another value, so we must test for on non-error result
	g.writeln('if (!${result_var}.is_error) {')
	g.indent++

	unwrapped_c_typ := g.styp(node.typ.clear_flag(.result))
	select_unwrapped_result_var_name := g.new_tmp_var()

	g.writeln('Array_Array_orm__Primitive ${select_unwrapped_result_var_name} = (*(Array_Array_orm__Primitive*)${select_result_var_name}.data);')

	if node.aggregate_kind != .none {
		prim_var := g.new_tmp_var()
		aggregate_type := if node.aggregate_kind == .avg {
			ast.f64_type
		} else {
			node.aggregate_field_type
		}
		primitive_field_name := g.orm_primitive_variant_field_name(aggregate_type)
		aggregate_value_styp := g.styp(aggregate_type.clear_flag(.option))
		if node.aggregate_kind == .count {
			g.writeln('*(${unwrapped_c_typ}*) ${result_var}.data = 0;')
			g.writeln('if (${select_unwrapped_result_var_name}.len > 0 && (*(Array_orm__Primitive*) builtin__array_get(${select_unwrapped_result_var_name}, 0)).len > 0) {')
			g.indent++
			g.writeln('orm__Primitive *${prim_var} = &(*(orm__Primitive*) builtin__array_get((*(Array_orm__Primitive*) builtin__array_get(${select_unwrapped_result_var_name}, 0)), 0));')
			g.writeln('*(${unwrapped_c_typ}*) ${result_var}.data = *(${prim_var}->_${primitive_field_name});')
			g.indent--
			g.writeln('}')
		} else {
			aggregate_result_var := g.new_tmp_var()
			g.writeln('${unwrapped_c_typ} ${aggregate_result_var} = (${unwrapped_c_typ}){ .state = 2, .err = _const_none__, .data = {E_STRUCT} };')
			g.writeln('if (${select_unwrapped_result_var_name}.len > 0 && (*(Array_orm__Primitive*) builtin__array_get(${select_unwrapped_result_var_name}, 0)).len > 0) {')
			g.indent++
			g.writeln('orm__Primitive *${prim_var} = &(*(orm__Primitive*) builtin__array_get((*(Array_orm__Primitive*) builtin__array_get(${select_unwrapped_result_var_name}, 0)), 0));')
			g.writeln('if (${prim_var}->_typ != ${g.table.find_type_idx('orm.Null')}) {')
			g.indent++
			g.writeln('builtin___option_ok(${prim_var}->_${primitive_field_name}, (_option *)&${aggregate_result_var}, sizeof(${aggregate_value_styp}));')
			g.indent--
			g.writeln('}')
			g.indent--
			g.writeln('}')
			g.writeln('*(${unwrapped_c_typ}*) ${result_var}.data = ${aggregate_result_var};')
		}
	} else {
		tmp := g.new_tmp_var()
		idx := g.new_tmp_var()
		g.writeln('${ast.int_type_name} ${idx} = 0;')
		mut typ_str := ''
		if node.is_array {
			info := g.table.sym(node.typ).array_info()
			typ_str = g.styp(info.elem_type)
			base_typ := g.base_type(node.typ)
			if node.typ.has_flag(.option) {
				g.writeln('${unwrapped_c_typ} ${tmp}_array = { .state = 2, .err = _const_none__, .data = {E_STRUCT} };')
				g.writeln('builtin___option_ok(&(${base_typ}[]) { builtin____new_array(0, ${select_unwrapped_result_var_name}.len, sizeof(${typ_str})) }, (_option *)&${tmp}_array, sizeof(${base_typ}));')
			} else {
				g.writeln('${unwrapped_c_typ} ${tmp}_array = builtin____new_array(0, ${select_unwrapped_result_var_name}.len, sizeof(${typ_str}));')
			}
			g.writeln('for (; ${idx} < ${select_unwrapped_result_var_name}.len; ${idx}++) {')
			g.indent++
			g.write('${typ_str} ${tmp} = (${typ_str}) {')
			inf := g.table.sym(info.elem_type).struct_info()
			for i, field in inf.fields {
				g.zero_struct_field(field)
				if i != inf.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		} else {
			g.write('${unwrapped_c_typ} ${tmp} = (${unwrapped_c_typ}){')
			info := g.table.sym(node.typ).struct_info()
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		}

		g.writeln('if (${select_unwrapped_result_var_name}.len > 0) {')
		g.indent++

		mut fields_idx := 0
		for field in fields {
			array_get_call_code := '(*(orm__Primitive*) builtin__array_get((*(Array_orm__Primitive*) builtin__array_get(${select_unwrapped_result_var_name}, ${idx})), ${fields_idx}))'
			final_field_typ := g.table.final_type(field.typ)
			sym := g.table.sym(final_field_typ)
			field_var := '${tmp}.${orm_field_access_name(field.name)}'
			field_c_typ := g.styp(final_field_typ)
			if sym.kind == .struct && sym.name != 'time.Time' {
				mut sub := node.sub_structs[field.name] or { continue }
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident
				primitive_type_index := g.table.find_type('orm.Primitive')

				if primitive_type_index != 0 {
					if mut ident.info is ast.IdentVar {
						ident.info.typ = primitive_type_index
					}
				}

				ident.name = array_get_call_code
				ident.obj = ast.Var{
					name: array_get_call_code
				}
				where_expr.right = ident
				sub.where_expr = where_expr

				sub_result_var := g.new_tmp_var()
				sub_result_c_typ := g.styp(sub.typ)
				g.writeln('${sub_result_c_typ} ${sub_result_var};')
				g.write_orm_select(sub, connection_var_name, sub_result_var)
				if final_field_typ.has_flag(.option) {
					unwrapped_field_c_typ := g.styp(final_field_typ.clear_flag(.option))
					g.writeln('if (!${sub_result_var}.is_error)')
					g.writeln('\tbuiltin___option_ok(${sub_result_var}.data, (_option *)&${field_var}, sizeof(${unwrapped_field_c_typ}));')
					g.writeln('else')
					g.writeln('\t${field_var} = (${field_c_typ}){ .state = 2, .err = _const_none__, .data = {E_STRUCT} };')
				} else {
					g.writeln('if (!${sub_result_var}.is_error)')
					g.writeln('\t${field_var} = *(${field_c_typ}*)${sub_result_var}.data;')
				}
				fields_idx++
			} else if sym.kind == .array {
				mut fkey := ''
				if attr := field.attrs.find_first('fkey') {
					fkey = attr.arg
				} else {
					verror('missing fkey attribute')
				}
				sub := node.sub_structs[field.name] or { continue }
				if sub.has_where {
					mut where_expr := sub.where_expr as ast.InfixExpr
					mut left_where_expr := where_expr.left as ast.Ident
					mut right_where_expr := where_expr.right as ast.Ident
					left_where_expr.name = fkey
					right_where_expr.name = tmp
					right_where_expr.obj = ast.Var{
						name: tmp
					}
					where_expr.left = left_where_expr
					where_expr.right = ast.SelectorExpr{
						pos:        right_where_expr.pos
						field_name: primary_field.name
						is_mut:     false
						expr:       right_where_expr
						expr_type:  (right_where_expr.info as ast.IdentVar).typ
						typ:        (right_where_expr.info as ast.IdentVar).typ
						scope:      unsafe { nil }
					}

					mut sql_expr_select_array := ast.SqlExpr{
						typ:                  final_field_typ.set_flag(.result)
						aggregate_kind:       sub.aggregate_kind
						aggregate_field:      sub.aggregate_field
						db_expr:              sub.db_expr
						has_where:            sub.has_where
						has_offset:           sub.has_offset
						offset_expr:          sub.offset_expr
						has_order:            sub.has_order
						order_expr:           sub.order_expr
						has_desc:             sub.has_desc
						is_array:             true
						is_generated:         true
						pos:                  sub.pos
						has_limit:            sub.has_limit
						limit_expr:           sub.limit_expr
						table_expr:           sub.table_expr
						fields:               sub.fields
						sub_structs:          sub.sub_structs
						where_expr:           where_expr
						aggregate_field_type: sub.aggregate_field_type
					}

					sub_result_var := g.new_tmp_var()
					sub_result_c_typ := g.styp(sub.typ)
					g.writeln('${sub_result_c_typ} ${sub_result_var};')
					g.write_orm_select(sql_expr_select_array, connection_var_name, sub_result_var)
					g.writeln('if (!${sub_result_var}.is_error) {')
					if final_field_typ.has_flag(.option) {
						g.writeln('\t${field_var}.state = 0;')
						g.writeln('\t*(${g.base_type(final_field_typ)}*)${field_var}.data = *(${g.base_type(final_field_typ)}*)${sub_result_var}.data;')
					} else {
						g.writeln('\t${field_var} = *(${g.base_type(field.typ)}*)${sub_result_var}.data;')
					}
					g.writeln('}')
				}
			} else if final_field_typ.has_flag(.option) {
				prim_var := g.new_tmp_var()
				g.writeln('orm__Primitive *${prim_var} = &${array_get_call_code};')
				g.writeln('if (${prim_var}->_typ == ${g.table.find_type_idx('orm.Null')})')
				g.writeln('\t${field_var} = (${field_c_typ}){ .state = 2, .err = _const_none__, .data = {E_STRUCT} };')

				g.writeln('else')
				g.writeln('\tbuiltin___option_ok(${prim_var}->_${sym.cname}, (_option *)&${field_var}, sizeof(${sym.cname}));')
				fields_idx++
			} else if sym.kind == .enum {
				mut typ := sym.cname
				g.writeln('${tmp}.${orm_field_access_name(field.name)} = (${typ}) (*(${array_get_call_code}._i64));')
				fields_idx++
			} else {
				g.writeln('${field_var} = *(${array_get_call_code}._${sym.cname});')
				fields_idx++
			}
		}

		if node.is_array {
			if node.typ.has_flag(.option) {
				g.writeln('${tmp}_array.state = 0;')
				g.writeln('builtin__array_push((${g.base_type(node.typ)}*)&${tmp}_array.data, _MOV((${typ_str}[]){ ${tmp} }));')
			} else {
				g.writeln('builtin__array_push(&${tmp}_array, _MOV((${typ_str}[]){ ${tmp} }));')
			}
			g.indent--
			g.writeln('}')
		}

		g.indent--
		if !node.is_array {
			g.writeln('} else {')
			g.writeln('\t${result_var}.is_error = true;')
		}
		g.writeln('}')

		if node.is_array {
			if node.typ.has_flag(.option) {
				g.writeln('*(${g.base_type(node.typ)}*) ${result_var}.data = *(${g.base_type(node.typ)}*)${tmp}_array.data;')
			} else {
				g.writeln('*(${unwrapped_c_typ}*) ${result_var}.data = ${tmp}_array;')
			}
		} else {
			g.writeln('*(${unwrapped_c_typ}*) ${result_var}.data = ${tmp};')
		}
	}

	g.indent--
	g.writeln('}')

	if node.is_generated {
		g.writeln(';')
	}
}

// filter_struct_fields_by_orm_attrs filters struct fields taking into its attributes.
// Used by non-create queries for skipping fields.
fn (_ &Gen) filter_struct_fields_by_orm_attrs(fields []ast.StructField) []ast.StructField {
	mut ret := []ast.StructField{}

	for field in fields {
		if field.attrs.contains('skip') || field.attrs.contains_arg('sql', '-') {
			continue
		}
		ret << field
	}

	return ret
}

// get_db_expr_type returns the database type from the database expression.
fn (g &Gen) get_db_expr_type(expr ast.Expr) ?ast.Type {
	if expr is ast.Ident {
		if expr.info is ast.IdentVar {
			return g.table.unaliased_type(expr.info.typ)
		}
	} else if expr is ast.SelectorExpr {
		return g.table.unaliased_type(expr.typ)
	}

	return none
}

// get_table_attrs_by_struct_type returns the struct attrs.
fn (g &Gen) get_table_attrs_by_struct_type(typ ast.Type) []ast.Attr {
	sym := g.table.sym(typ)
	info := sym.struct_info()
	return info.attrs
}

// get_table_name_by_struct_type converts the struct type to a table name.
// For generic types, uses ngname (name without generic params) to get the base table name.
fn (g &Gen) get_table_name_by_struct_type(typ ast.Type) string {
	sym := g.table.sym(typ)
	info := sym.struct_info()
	// Use ngname for generic types to strip the generic parameters (e.g., Message[Payload] -> Message)
	// Fall back to stripping manually from name if ngname is empty
	base_name := if sym.ngname.len > 0 {
		sym.ngname
	} else {
		// Strip generic parameters manually (e.g., main.Message[main.Payload] -> main.Message)
		sym.name.all_before('[')
	}
	mut table_name := util.strip_mod_name(base_name)

	if attr := info.attrs.find_first('table') {
		table_name = attr.arg
	} else {
		// Keep default ORM table names aligned with unquoted SQL identifiers across DB drivers.
		table_name = table_name.to_lower()
	}
	escaped_table_name := cescape_nonascii(util.smart_quote(table_name, false))
	return escaped_table_name
}

// get_orm_current_table_field returns the current processing table's struct field by name.
fn (g &Gen) get_orm_current_table_field(name string) ?ast.StructField {
	// Use sql_table_typ directly for proper generic type support
	sym := g.table.sym(g.sql_table_typ)
	// For GenericInst types, get the struct info from the parent type
	info := if sym.info is ast.GenericInst {
		g.table.type_symbols[sym.info.parent_idx].struct_info()
	} else {
		sym.struct_info()
	}

	for field in info.fields {
		if field.name == name {
			return field
		}
	}

	return none
}

// get_orm_column_name_from_struct_field converts the struct field to a table column name.
fn (g &Gen) get_orm_column_name_from_struct_field(field ast.StructField) string {
	mut name := field.name

	if attr := field.attrs.find_first('sql') {
		if attr.arg !in ['serial', 'i8', 'i16', 'i32', 'int', 'i64', 'u8', 'u16', 'u32', 'u64',
			'f32', 'f64', 'bool', 'string'] {
			name = attr.arg
		}
	}

	final_field_typ := g.table.final_type(field.typ)
	sym := g.table.sym(final_field_typ)
	if sym.kind == .struct && sym.name != 'time.Time' {
		name = '${name}_id'
	}

	return name
}

fn (g &Gen) get_orm_field_by_column_name(fields []ast.StructField, column string) ?ast.StructField {
	for field in fields {
		if g.get_orm_column_name_from_struct_field(field) == column {
			return field
		}
	}
	return none
}

// get_orm_select_expr_from_struct_field returns the SQL expression used in a SELECT list.
fn (g &Gen) get_orm_select_expr_from_struct_field(field ast.StructField) string {
	if attr := field.attrs.find_first('sql_select') {
		arg := attr.arg.trim_space()
		if arg.len >= 2 && ((arg.starts_with("'") && arg.ends_with("'"))
			|| (arg.starts_with('"') && arg.ends_with('"'))) {
			return arg[1..arg.len - 1].trim_space()
		}
		return arg
	}
	return g.get_orm_column_name_from_struct_field(field)
}

// get_orm_struct_primary_field returns the table's primary column field.
fn (_ &Gen) get_orm_struct_primary_field(fields []ast.StructField) ?ast.StructField {
	for field in fields {
		if _ := field.attrs.find_first('primary') {
			return field
		}
	}
	return none
}

fn (g &Gen) get_orm_upsert_conflict_groups(fields []ast.StructField, table_attrs []ast.Attr) [][]string {
	mut groups := [][]string{}
	mut named_unique_groups := map[string][]string{}
	mut named_unique_group_order := []string{}
	mut seen := map[string]bool{}
	for field in fields {
		column_name := g.get_orm_column_name_from_struct_field(field)
		for attr in field.attrs {
			match attr.name {
				'primary' {
					key := column_name
					if key !in seen {
						groups << [column_name]
						seen[key] = true
					}
				}
				'unique' {
					if attr.arg != '' && attr.kind == .string {
						if attr.arg !in named_unique_groups {
							named_unique_groups[attr.arg] = []string{}
							named_unique_group_order << attr.arg
						}
						named_unique_groups[attr.arg] << column_name
					} else {
						key := column_name
						if key !in seen {
							groups << [column_name]
							seen[key] = true
						}
					}
				}
				else {}
			}
		}
	}
	for group_name in named_unique_group_order {
		group := named_unique_groups[group_name]
		key := group.join(',')
		if key !in seen {
			groups << group
			seen[key] = true
		}
	}
	for attr in table_attrs {
		if attr.name != 'unique_key' || attr.arg == '' || attr.kind != .string {
			continue
		}
		mut group := []string{}
		for raw_field_name in attr.arg.split(',') {
			field_name := raw_field_name.trim_space()
			if field_name != '' {
				group << field_name
			}
		}
		key := group.join(',')
		if group.len > 0 && key !in seen {
			groups << group
			seen[key] = true
		}
	}
	return groups
}

fn (mut g Gen) write_orm_upsert_conflict_groups(groups [][]string) {
	if groups.len == 0 {
		g.write('builtin____new_array_with_default_noscan(0, 0, sizeof(Array_string), 0)')
		return
	}
	g.write('builtin__new_array_from_c_array(${groups.len}, ${groups.len}, sizeof(Array_string), _MOV((Array_string[${groups.len}]){')
	for group in groups {
		if group.len == 0 {
			g.write('builtin____new_array_with_default_noscan(0, 0, sizeof(string), 0),')
			continue
		}
		g.write('builtin__new_array_from_c_array(${group.len}, ${group.len}, sizeof(string), _MOV((string[${group.len}]){')
		for field_name in group {
			g.write('_S("${field_name}"),')
		}
		g.write('})),')
	}
	g.write('}))') // closing: `}` compound literal init, `)` _MOV, `)` new_array_from_c_array
}

// return indexes of any auto-increment fields or fields with default values
fn get_auto_field_idxs(fields []ast.StructField) []int {
	mut ret := []int{}
	for i, field in fields {
		for attr in field.attrs {
			if attr.name == 'default' {
				ret << i
			} else if attr.name == 'sql' && attr.arg == 'serial' {
				ret << i
			} else if attr.name == 'serial' && attr.kind == .plain && !attr.has_arg {
				ret << i
			}
		}
	}
	return ret
}

// write_orm_joins writes C code for the joins array in SelectConfig
fn (mut g Gen) write_orm_joins(joins []ast.JoinClause) {
	if joins.len == 0 {
		g.writeln('.joins = builtin____new_array_with_default_noscan(0, 0, sizeof(orm__JoinConfig), 0),')
		return
	}

	g.writeln('.joins = builtin__new_array_from_c_array(${joins.len}, ${joins.len}, sizeof(orm__JoinConfig),')
	g.indent++
	g.writeln('_MOV((orm__JoinConfig[${joins.len}]){')
	g.indent++

	for join in joins {
		g.writeln('(orm__JoinConfig){')
		g.indent++

		// Write join kind
		kind_str := match join.kind {
			.inner { 'orm__JoinType__inner' }
			.left { 'orm__JoinType__left' }
			.right { 'orm__JoinType__right' }
			.full_outer { 'orm__JoinType__full_outer' }
		}

		g.writeln('.kind = ${kind_str},')

		// Write joined table info
		g.write('.table = ')
		g.write_orm_table_struct(join.table_expr.typ)
		g.writeln(',')

		// Extract column names from the ON expression (should be an InfixExpr)
		left_col, right_col := g.extract_join_columns(join.on_expr)
		g.writeln('.on_left_col = _S("${left_col}"),')
		g.writeln('.on_right_col = _S("${right_col}"),')

		g.indent--
		g.writeln('},')
	}

	g.indent--
	g.writeln('})')
	g.indent--
	g.writeln('),')
}

// extract_join_columns extracts the left and right column names from a JOIN ON expression.
// The ON expression is expected to be an InfixExpr like: User.dept_id == Department.id
// Returns (left_col, right_col) where left_col is from the main table, right_col is from the joined table.
fn (g &Gen) extract_join_columns(on_expr ast.Expr) (string, string) {
	if on_expr is ast.InfixExpr {
		left_col := g.extract_join_field_name(on_expr.left)
		right_col := g.extract_join_field_name(on_expr.right)
		return left_col, right_col
	}

	// Fallback: return empty strings if the expression is not the expected format
	return '', ''
}

// extract_join_field_name extracts a field name from a JOIN ON expression operand.
// Handles both SelectorExpr (Table.field) and EnumVal (when parser interprets Type.field as enum).
fn (g &Gen) extract_join_field_name(expr ast.Expr) string {
	if expr is ast.SelectorExpr {
		return expr.field_name
	}
	if expr is ast.EnumVal {
		// EnumVal.val contains the field name (e.g., "department_id" from "User.department_id")
		return expr.val
	}
	return ''
}
