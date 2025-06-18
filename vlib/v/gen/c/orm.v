// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

enum SqlExprSide {
	left
	right
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

	// orm_insert needs an SqlStmtLine, build it from SqlExpr (most nodes are the same)
	hack_stmt_line := ast.SqlStmtLine{
		object_var: node.inserted_var
		fields:     node.fields
		table_expr: node.table_expr
		// sub_structs: node.sub_structs
	}
	g.write_orm_insert(hack_stmt_line, table_name, connection_var_name, result_var_name,
		node.or_expr, table_attrs)

	g.write2(left, 'orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object)')
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

	if node.kind != .create {
		node.fields = g.filter_struct_fields_by_orm_attrs(node.fields)
	}

	if node.kind == .create {
		g.write_orm_create_table(node, table_name, connection_var_name, result_var_name,
			table_attrs)
	} else if node.kind == .drop {
		g.write_orm_drop_table(node, table_name, connection_var_name, result_var_name,
			table_attrs)
	} else if node.kind == .insert {
		g.write_orm_insert(node, table_name, connection_var_name, result_var_name, or_expr,
			table_attrs)
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

	mut db_ctype_name := g.styp(db_expr_type)
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
		g.expr(db_expr)
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
	g.writeln('.attrs = new_array_from_c_array(${table_attrs.len}, ${table_attrs.len}, sizeof(VAttribute),')
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
	result_var_name string, table_attrs []ast.Attr) {
	g.writeln('// sql { create table `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_create(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('new_array_from_c_array(${node.fields.len}, ${node.fields.len}, sizeof(orm__TableField),')
	g.indent++

	if node.fields.len > 0 {
		g.writeln('_MOV((orm__TableField[${node.fields.len}]){')
		g.indent++

		for field in node.fields {
			g.writeln('// `${table_name}`.`${field.name}`')
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
			g.writeln('.attrs = new_array_from_c_array(${field.attrs.len}, ${field.attrs.len}, sizeof(VAttribute),')
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
fn (mut g Gen) write_orm_drop_table(node ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, table_attrs []ast.Attr) {
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
	or_expr &ast.OrExpr, table_attrs []ast.Attr) {
	last_ids_variable_name := g.new_tmp_var()

	g.writeln('Array_orm__Primitive ${last_ids_variable_name} = __new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0);')
	g.write_orm_insert_with_last_ids(node, connection_var_name, table_name, last_ids_variable_name,
		result_var_name, '', '', or_expr)
}

// write_orm_update writes C code that calls ORM functions for updating rows.
fn (mut g Gen) write_orm_update(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, table_attrs []ast.Attr) {
	g.writeln('// sql { update `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_update(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('(orm__QueryData){')
	g.indent++
	g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.parentheses = __new_array_with_default_noscan(0, 0, sizeof(Array_int), 0),')

	if node.updated_columns.len > 0 {
		g.writeln('.fields = new_array_from_c_array(${node.updated_columns.len}, ${node.updated_columns.len}, sizeof(string),')
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
		g.writeln('.fields = __new_array_with_default_noscan(${node.updated_columns.len}, ${node.updated_columns.len}, sizeof(string), 0')
	}

	g.writeln2('),', '.data = new_array_from_c_array(${node.update_exprs.len}, ${node.update_exprs.len}, sizeof(orm__Primitive),')

	if node.update_exprs.len > 0 {
		g.indent++
		g.writeln('_MOV((orm__Primitive[${node.update_exprs.len}]){')
		g.indent++
		for e in node.update_exprs {
			g.write_orm_expr_to_primitive(e)
		}
		g.indent--
		g.writeln('})')
		g.indent--
	}

	g.writeln('),')
	g.indent--
	g.writeln('},')
	g.write_orm_where(node.where_expr)
	g.indent--
	g.writeln(');')
}

// write_orm_delete writes C code that calls ORM functions for deleting rows.
fn (mut g Gen) write_orm_delete(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, table_attrs []ast.Attr) {
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
			subs << unsafe { node.sub_structs[int(final_field_typ)] }
			unwrapped_c_typ := g.styp(final_field_typ.clear_flag(.option))
			subs_unwrapped_c_typ << if final_field_typ.has_flag(.option) {
				unwrapped_c_typ
			} else {
				''
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
			if node.sub_structs.len > 0 {
				arrs << unsafe { node.sub_structs[int(final_field_typ)] }
			}
			field_names << field.name
		}
	}

	fields := node.fields.filter(g.table.sym(it.typ).kind != .array)
	auto_fields := get_auto_field_idxs(fields)

	primary_field := g.get_orm_struct_primary_field(fields) or { ast.StructField{} }

	is_serial := (primary_field.attrs.contains_arg('sql', 'serial')
		|| primary_field.attrs.contains('serial')) && primary_field.typ == ast.int_type

	mut inserting_object_type := ast.void_type
	mut member_access_type := '.'
	if node.scope != unsafe { nil } {
		inserting_object := node.scope.find(node.object_var) or {
			verror('`${node.object_var}` is not found in scope')
		}
		if inserting_object.typ.is_ptr() {
			member_access_type = '->'
		}
		inserting_object_type = inserting_object.typ
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
		g.writeln('array_push(&${last_ids_arr}, _MOV((orm__Primitive[1]){')
		g.writeln('\torm__int_to_primitive(orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object))}));')
		if subs_unwrapped_c_typ[i].len > 0 {
			g.indent--
			g.writeln('} else {')
			g.writeln('\tarray_push(&${last_ids_arr}, _MOV((orm__Primitive[1]){ _const_orm__null_primitive }));')
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
	g.writeln('.fields = new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
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

	g.writeln('.data = new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(orm__Primitive),')
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
			mut typ := sym.cname
			mut ctyp := sym.cname
			if sym.kind == .struct && typ != 'time__Time' {
				g.writeln('(*(orm__Primitive*) array_get(${last_ids_arr}, ${structs})),')
				structs++
				continue
			}
			// fields processed hereafter can be NULL...
			if typ == 'time__Time' {
				ctyp = 'time__Time'
				typ = 'time'
			} else if sym.kind == .enum {
				typ = g.table.sym(final_field_typ).cname
			}
			var := '${node.object_var}${member_access_type}${c_name(field.name)}'
			if final_field_typ.has_flag(.option) {
				g.writeln('${var}.state == 2? _const_orm__null_primitive : orm__${typ}_to_primitive(*(${ctyp}*)(${var}.data)),')
			} else if inserting_object_sym.kind == .sum_type {
				table_sym := g.table.sym(node.table_expr.typ)
				sum_type_var := '(*${node.object_var}._${table_sym.cname})${member_access_type}${c_name(field.name)}'
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
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	if auto_fields.len > 0 {
		g.writeln('.auto_fields = new_array_from_c_array(${auto_fields.len}, ${auto_fields.len}, sizeof(${ast.int_type_name}),')
		g.indent++
		g.write('_MOV((int[${auto_fields.len}]){')
		for i in auto_fields {
			g.write(' ${i},')
		}
		g.writeln(' })),')
		g.indent--
	} else {
		g.writeln('.auto_fields = __new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	}
	g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
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
			mut sym := g.table.sym(primary_field.typ)
			mut typ := sym.cname
			if typ == 'time__Time' {
				typ = 'time'
			}
			g.writeln('orm__Primitive ${id_name} = orm__${typ}_to_primitive(${node.object_var}${member_access_type}${c_name(primary_field.name)});')
		}
		for i, mut arr in arrs {
			idx := g.new_tmp_var()
			ctyp := g.styp(arr.table_expr.typ)
			is_option := opt_fields.contains(i)
			if is_option {
				g.writeln('for (int ${idx} = 0; ${node.object_var}${member_access_type}${arr.object_var}.state != 2 && ${idx} < (*(Array_${ctyp}*)${node.object_var}${member_access_type}${arr.object_var}.data).len; ${idx}++) {')
			} else {
				g.writeln('for (int ${idx} = 0; ${idx} < ${node.object_var}${member_access_type}${arr.object_var}.len; ${idx}++) {')
			}
			g.indent++
			last_ids := g.new_tmp_var()
			res_ := g.new_tmp_var()
			tmp_var := g.new_tmp_var()
			if is_option {
				g.writeln('${ctyp} ${tmp_var} = (*(${ctyp}*)array_get(*(Array_${ctyp}*)${node.object_var}${member_access_type}${arr.object_var}.data, ${idx}));')
			} else {
				g.writeln('${ctyp} ${tmp_var} = (*(${ctyp}*)array_get(${node.object_var}${member_access_type}${arr.object_var}, ${idx}));')
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
			g.write_orm_insert_with_last_ids(arr, connection_var_name, g.get_table_name_by_struct_type(arr.table_expr.typ),
				last_ids, res_, id_name, fkeys[i], or_expr)
			// Validates sub insertion success otherwise, handled and propagated error.
			g.or_block(res_, or_expr, ast.int_type.set_flag(.result))
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
			g.write_orm_primitive(info.typ, expr)
		}
		ast.SelectorExpr {
			g.write_orm_primitive(expr.typ, expr)
		}
		ast.CallExpr {
			g.write_orm_primitive(expr.return_type, expr)
		}
		ast.None {
			g.write_orm_primitive(ast.none_type, expr)
		}
		else {
			eprintln(expr)
			verror('ORM: ${expr.type_name()} is not supported')
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
		} else if g.table.final_sym(t).kind == .enum {
			typ = g.table.sym(g.table.final_type(t)).cname
		} else if g.table.final_sym(t).kind == .array {
			typ = g.table.sym(g.table.final_type(t)).cname.to_lower()
		}
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
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	if fields.len > 0 {
		g.writeln('.fields = new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
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
		g.writeln('.fields = __new_array_with_default_noscan(${fields.len}, ${fields.len}, sizeof(string), 0')
	}
	g.writeln('),')

	g.writeln('.data = new_array_from_c_array(${data.len}, ${data.len}, sizeof(orm__Primitive),')
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
		g.write('new_array_from_c_array(${parentheses.len}, ${parentheses.len}, sizeof(Array_int), _MOV((Array_int[${parentheses.len}]){')
		for par in parentheses {
			if par.len > 0 {
				g.write('new_array_from_c_array(${par.len}, ${par.len}, sizeof(${ast.int_type_name}), _MOV((int[${par.len}]){')
				for val in par {
					g.write('${val},')
				}
				g.write('})),')
			} else {
				g.write('__new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
			}
		}
		g.write('}))')
	} else {
		g.write('__new_array_with_default_noscan(0, 0, sizeof(Array_int), 0)')
	}
	g.writeln(',')

	if kinds.len > 0 {
		g.writeln('.kinds = new_array_from_c_array(${kinds.len}, ${kinds.len}, sizeof(orm__OperationKind),')
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
		g.write('.kinds = __new_array_with_default_noscan(${kinds.len}, ${kinds.len}, sizeof(orm__OperationKind), 0')
	}
	g.writeln('),')

	if is_ands.len > 0 {
		g.write('.is_and = new_array_from_c_array(${is_ands.len}, ${is_ands.len}, sizeof(bool),')
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
		g.write('.is_and = __new_array_with_default_noscan(${is_ands.len}, ${is_ands.len}, sizeof(bool), 0')
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
			g.write_orm_where_expr(expr.left, mut fields, mut parentheses, mut kinds, mut
				data, mut is_and)
			mut kind := match expr.op {
				.ne {
					'orm__OperationKind__neq'
				}
				.eq {
					'orm__OperationKind__eq'
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
			if expr.op !in [.key_is, .not_is] { // ignore rhs for unary ops
				g.sql_side = .right
				g.write_orm_where_expr(expr.right, mut fields, mut parentheses, mut kinds, mut
					data, mut is_and)
			}
		}
		ast.ParExpr {
			mut par := [fields.len]
			g.write_orm_where_expr(expr.expr, mut fields, mut parentheses, mut kinds, mut
				data, mut is_and)
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

	g.writeln('// sql { select from `${table_name}` }')
	g.writeln('${result_name}_Array_Array_orm__Primitive ${select_result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_select(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('(orm__SelectConfig){')
	g.indent++
	g.writeln('.table = ')
	g.write_orm_table_struct(node.table_expr.typ)
	g.writeln(',')
	g.writeln('.is_count = ${node.is_count},')
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

	if primary_field.name != '' {
		g.writeln('.primary = _S("${primary_field.name}"),')
	}

	select_fields := fields.filter(g.table.sym(it.typ).kind != .array)
	g.writeln('.fields = new_array_from_c_array(${select_fields.len}, ${select_fields.len}, sizeof(string),')
	g.indent++
	mut types := []string{}
	if select_fields.len > 0 {
		g.writeln('_MOV((string[${select_fields.len}]){')
		g.indent++
		for field in select_fields {
			g.writeln('_S("${g.get_orm_column_name_from_struct_field(field)}"),')
			final_field_typ := g.table.final_type(field.typ)
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
	g.writeln2('),', '.types = new_array_from_c_array(${types.len}, ${types.len}, sizeof(${ast.int_type_name}),')
	g.indent++

	if types.len > 0 {
		g.write('_MOV((int[${types.len}]){')
		for typ in types {
			g.write(' ${typ},')
		}
		g.writeln(' })')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln('),')
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
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
	g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.writeln('.parentheses = __new_array_with_default_noscan(0, 0, sizeof(Array_int), 0),')
	if exprs.len > 0 {
		g.write('.data = new_array_from_c_array(${exprs.len}, ${exprs.len}, sizeof(orm__Primitive),')
		g.write(' _MOV((orm__Primitive[${exprs.len}]){')
		for e in exprs {
			g.write_orm_expr_to_primitive(e)
		}
		g.writeln('})')
	} else {
		g.writeln('.data = __new_array_with_default_noscan(${exprs.len}, ${exprs.len}, sizeof(orm__Primitive), 0')
	}
	g.indent--
	g.writeln(')},')

	if node.has_where {
		g.write_orm_where(node.where_expr)
	} else {
		g.writeln('(orm__QueryData) {')
		g.indent++
		g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(${ast.int_type_name}), 0),')
		g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
		g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
		g.writeln('.parentheses = __new_array_with_default_noscan(0, 0, sizeof(Array_int), 0),')
		g.writeln('.data = __new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0)')
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

	if node.is_count {
		g.writeln('*(${unwrapped_c_typ}*) ${result_var}.data = *((*(orm__Primitive*) array_get((*(Array_orm__Primitive*)array_get(${select_unwrapped_result_var_name}, 0)), 0))._int);')
	} else {
		tmp := g.new_tmp_var()
		idx := g.new_tmp_var()
		g.writeln('int ${idx} = 0;')
		mut typ_str := ''
		if node.is_array {
			info := g.table.sym(node.typ).array_info()
			typ_str = g.styp(info.elem_type)
			base_typ := g.base_type(node.typ)
			if node.typ.has_flag(.option) {
				g.writeln('${unwrapped_c_typ} ${tmp}_array = { .state = 2, .err = _const_none__, .data = {E_STRUCT} };')
				g.writeln('_option_ok(&(${base_typ}[]) { __new_array(0, ${select_unwrapped_result_var_name}.len, sizeof(${typ_str})) }, (_option *)&${tmp}_array, sizeof(${base_typ}));')
			} else {
				g.writeln('${unwrapped_c_typ} ${tmp}_array = __new_array(0, ${select_unwrapped_result_var_name}.len, sizeof(${typ_str}));')
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
			array_get_call_code := '(*(orm__Primitive*) array_get((*(Array_orm__Primitive*) array_get(${select_unwrapped_result_var_name}, ${idx})), ${fields_idx}))'
			final_field_typ := g.table.final_type(field.typ)
			sym := g.table.sym(final_field_typ)
			field_var := '${tmp}.${c_name(field.name)}'
			field_c_typ := g.styp(final_field_typ)
			if sym.kind == .struct && sym.name != 'time.Time' {
				mut sub := node.sub_structs[int(final_field_typ)]
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident
				primitive_type_index := g.table.find_type('orm.Primitive')

				if primitive_type_index != 0 {
					if mut ident.info is ast.IdentVar {
						ident.info.typ = primitive_type_index
					}
				}

				ident.name = array_get_call_code
				where_expr.right = ident
				sub.where_expr = where_expr

				sub_result_var := g.new_tmp_var()
				sub_result_c_typ := g.styp(sub.typ)
				g.writeln('${sub_result_c_typ} ${sub_result_var};')
				g.write_orm_select(sub, connection_var_name, sub_result_var)
				if final_field_typ.has_flag(.option) {
					unwrapped_field_c_typ := g.styp(final_field_typ.clear_flag(.option))
					g.writeln('if (!${sub_result_var}.is_error)')
					g.writeln('\t_option_ok(${sub_result_var}.data, (_option *)&${field_var}, sizeof(${unwrapped_field_c_typ}));')
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
				sub := node.sub_structs[final_field_typ]
				if sub.has_where {
					mut where_expr := sub.where_expr as ast.InfixExpr
					mut left_where_expr := where_expr.left as ast.Ident
					mut right_where_expr := where_expr.right as ast.Ident
					left_where_expr.name = fkey
					right_where_expr.name = tmp
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
						typ:          final_field_typ.set_flag(.result)
						is_count:     sub.is_count
						db_expr:      sub.db_expr
						has_where:    sub.has_where
						has_offset:   sub.has_offset
						offset_expr:  sub.offset_expr
						has_order:    sub.has_order
						order_expr:   sub.order_expr
						has_desc:     sub.has_desc
						is_array:     true
						is_generated: true
						pos:          sub.pos
						has_limit:    sub.has_limit
						limit_expr:   sub.limit_expr
						table_expr:   sub.table_expr
						fields:       sub.fields
						where_expr:   where_expr
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
				g.writeln('\t_option_ok(${prim_var}->_${sym.cname}, (_option *)&${field_var}, sizeof(${sym.cname}));')
				fields_idx++
			} else if sym.kind == .enum {
				mut typ := sym.cname
				g.writeln('${tmp}.${c_name(field.name)} = (${typ}) (*(${array_get_call_code}._i64));')
				fields_idx++
			} else {
				g.writeln('${field_var} = *(${array_get_call_code}._${sym.cname});')
				fields_idx++
			}
		}

		if node.is_array {
			if node.typ.has_flag(.option) {
				g.writeln('${tmp}_array.state = 0;')
				g.writeln('array_push((${g.base_type(node.typ)}*)&${tmp}_array.data, _MOV((${typ_str}[]){ ${tmp} }));')
			} else {
				g.writeln('array_push(&${tmp}_array, _MOV((${typ_str}[]){ ${tmp} }));')
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
fn (g &Gen) get_table_name_by_struct_type(typ ast.Type) string {
	sym := g.table.sym(typ)
	info := sym.struct_info()
	mut table_name := util.strip_mod_name(sym.name)

	if attr := info.attrs.find_first('table') {
		table_name = attr.arg
	}
	escaped_table_name := cescape_nonascii(util.smart_quote(table_name, false))
	return escaped_table_name
}

// get_orm_current_table_field returns the current processing table's struct field by name.
fn (g &Gen) get_orm_current_table_field(name string) ?ast.StructField {
	info := g.table.sym(ast.idx_to_type(g.table.type_idxs[g.sql_table_name])).struct_info()

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
		if attr.arg !in ['serial', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
			'f64', 'bool', 'string'] {
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

// get_orm_struct_primary_field returns the table's primary column field.
fn (_ &Gen) get_orm_struct_primary_field(fields []ast.StructField) ?ast.StructField {
	for field in fields {
		if _ := field.attrs.find_first('primary') {
			return field
		}
	}
	return none
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
