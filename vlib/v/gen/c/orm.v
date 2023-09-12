// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
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
// cgen will write calling the function `@select` of the needed database.
// If you use sqlite, it calls `@select` from `vlib/db/sqlite/orm.v`

// sql_select_expr writes C code that calls ORM functions for selecting objects
// from the database, which is used by the `select` query.
fn (mut g Gen) sql_select_expr(node ast.SqlExpr) {
	// users :=
	left := g.go_before_stmt(0)
	connection_var_name := g.new_tmp_var()

	g.writeln('')
	g.write_orm_connection_init(connection_var_name, &node.db_expr)
	g.write_orm_select(node, connection_var_name, left, node.or_expr)
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
	mut node := stmt_line
	table_name := g.get_table_name_by_struct_type(node.table_expr.typ)
	result_var_name := g.new_tmp_var()
	g.sql_table_name = g.table.sym(node.table_expr.typ).name

	if node.kind != .create {
		node.fields = g.filter_struct_fields_by_orm_attrs(node.fields)
	}

	if node.kind == .create {
		g.write_orm_create_table(node, table_name, connection_var_name, result_var_name)
	} else if node.kind == .drop {
		g.write_orm_drop_table(table_name, connection_var_name, result_var_name)
	} else if node.kind == .insert {
		g.write_orm_insert(node, table_name, connection_var_name, result_var_name, or_expr)
	} else if node.kind == .update {
		g.write_orm_update(node, table_name, connection_var_name, result_var_name)
	} else if node.kind == .delete {
		g.write_orm_delete(node, table_name, connection_var_name, result_var_name)
	}

	g.or_block(result_var_name, or_expr, ast.int_type.set_flag(.result))
}

// write_orm_connection_init writes C code that saves the database connection
// into a variable for later use in ORM queries.
fn (mut g Gen) write_orm_connection_init(connection_var_name string, db_expr &ast.Expr) {
	db_expr_type := g.get_db_expr_type(db_expr) or {
		verror('V ORM: unknown db type for ${db_expr}')
	}

	mut db_ctype_name := g.typ(db_expr_type)
	is_pointer := db_ctype_name.ends_with('*')
	reference_sign := if is_pointer { '' } else { '&' }
	db_ctype_name = db_ctype_name.trim_right('*')

	g.writeln('// V ORM')
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

// write_orm_create_table writes C code that calls ORM functions for creating tables.
fn (mut g Gen) write_orm_create_table(node ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string) {
	g.writeln('// sql { create table `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_create(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('_SLIT("${table_name}"),')
	g.writeln('new_array_from_c_array(${node.fields.len}, ${node.fields.len}, sizeof(orm__TableField),')
	g.indent++

	if node.fields.len > 0 {
		g.writeln('_MOV((orm__TableField[${node.fields.len}]){')
		g.indent++

		for field in node.fields {
			g.writeln('// `${table_name}`.`${field.name}`')
			sym := g.table.sym(field.typ)
			typ := if sym.name == 'time.Time' { '_const_orm__time' } else { field.typ.idx().str() }
			g.writeln('(orm__TableField){')
			g.indent++
			g.writeln('.name = _SLIT("${field.name}"),')
			g.writeln('.typ = ${typ}, // `${sym.name}`')
			g.writeln('.is_arr = ${sym.kind == .array}, ')
			g.writeln('.nullable = ${if field.typ.has_flag(.option) { 'true' } else { 'false' }},')
			g.writeln('.default_val = (string){ .str = (byteptr) "${field.default_val}", .is_lit = 1 },')
			g.writeln('.attrs = new_array_from_c_array(${field.attrs.len}, ${field.attrs.len}, sizeof(StructAttribute),')
			g.indent++

			if field.attrs.len > 0 {
				g.write('_MOV((StructAttribute[${field.attrs.len}]){')
				g.indent++
				for attr in field.attrs {
					g.write('(StructAttribute){')
					g.indent++
					g.write(' .name = _SLIT("${attr.name}"),')
					g.write(' .has_arg = ${attr.has_arg},')
					g.write(' .arg = _SLIT("${attr.arg}"),')
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
fn (mut g Gen) write_orm_drop_table(table_name string, connection_var_name string, result_var_name string) {
	g.writeln('// sql { drop table `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_drop(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('_SLIT("${table_name}")')
	g.indent--
	g.writeln(');')
}

// write_orm_insert writes C code that calls ORM functions for inserting structs into a table.
fn (mut g Gen) write_orm_insert(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string, or_expr &ast.OrExpr) {
	last_ids_variable_name := g.new_tmp_var()

	g.writeln('Array_orm__Primitive ${last_ids_variable_name} = __new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0);')
	g.write_orm_insert_with_last_ids(node, connection_var_name, table_name, last_ids_variable_name,
		result_var_name, '', '', or_expr)
}

// write_orm_update writes C code that calls ORM functions for updating rows.
fn (mut g Gen) write_orm_update(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string) {
	g.writeln('// sql { update `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method_update(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('_SLIT("${table_name}"),')
	g.writeln('(orm__QueryData){')
	g.indent++
	g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(int), 0),')
	g.writeln('.parentheses = __new_array_with_default_noscan(0, 0, sizeof(Array_int), 0),')

	if node.updated_columns.len > 0 {
		g.writeln('.fields = new_array_from_c_array(${node.updated_columns.len}, ${node.updated_columns.len}, sizeof(string),')
		g.indent++
		g.writeln('_MOV((string[${node.updated_columns.len}]){')
		g.indent++
		for field in node.updated_columns {
			g.writeln('_SLIT("${field}"),')
		}
		g.indent--
		g.writeln('})')
		g.indent--
	} else {
		g.writeln('.fields = __new_array_with_default_noscan(${node.updated_columns.len}, ${node.updated_columns.len}, sizeof(string), 0')
	}

	g.writeln('),')
	g.writeln('.data = new_array_from_c_array(${node.update_exprs.len}, ${node.update_exprs.len}, sizeof(orm__Primitive),')

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
fn (mut g Gen) write_orm_delete(node &ast.SqlStmtLine, table_name string, connection_var_name string, result_var_name string) {
	g.writeln('// sql { delete from `${table_name}` }')
	g.writeln('${result_name}_void ${result_var_name} = orm__Connection_name_table[${connection_var_name}._typ]._method__v_delete(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('_SLIT("${table_name}"),')
	g.write_orm_where(node.where_expr)
	g.indent--
	g.writeln(');')
}

// write_orm_insert_with_last_ids writes C code that calls ORM functions for
// inserting a struct into a table, saving inserted `id` into a passed variable.
fn (mut g Gen) write_orm_insert_with_last_ids(node ast.SqlStmtLine, connection_var_name string, table_name string, last_ids_arr string, res string, pid string, fkey string, or_expr ast.OrExpr) {
	mut subs := []ast.SqlStmtLine{}
	mut arrs := []ast.SqlStmtLine{}
	mut fkeys := []string{}
	mut field_names := []string{}

	for field in node.fields {
		sym := g.table.sym(field.typ)
		if sym.kind == .struct_ && sym.name != 'time.Time' {
			subs << node.sub_structs[int(field.typ)]
		} else if sym.kind == .array {
			mut f_key := ''
			for attr in field.attrs {
				if attr.name == 'fkey' && attr.has_arg {
					if attr.kind == .string {
						f_key = attr.arg
					} else {
						verror("`fkey` attribute need be string. Try [fkey: '${attr.arg}'] instead of [fkey: ${attr.arg}]")
					}
				}
			}
			if f_key == '' {
				verror('a field which holds an array, needs a `fkey` defined ("${sym.name}")')
			}
			fkeys << f_key
			info := sym.array_info()
			if info.nr_dims == 1 {
				arrs << node.sub_structs[int(info.elem_type)]
				field_names << field.name
			} else {
				verror('V ORM only supports 1 dimensional arrays')
			}
		}
	}

	fields := node.fields.filter(g.table.sym(it.typ).kind != .array)
	auto_fields := get_auto_fields(fields)

	for sub in subs {
		g.sql_stmt_line(sub, connection_var_name, or_expr)
		g.writeln('array_push(&${last_ids_arr}, _MOV((orm__Primitive[]){orm__int_to_primitive(orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object))}));')
	}

	g.writeln('// sql { insert into `${table_name}` }')
	g.writeln('${result_name}_void ${res} = orm__Connection_name_table[${connection_var_name}._typ]._method_insert(')
	g.indent++
	g.writeln('${connection_var_name}._object, // Connection object')
	g.writeln('_SLIT("${table_name}"),')
	g.writeln('(orm__QueryData){')
	g.indent++
	g.writeln('.fields = new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
	g.indent++

	if fields.len > 0 {
		g.writeln('_MOV((string[${fields.len}]){ ')
		for f in fields {
			g.writeln('_SLIT("${g.get_orm_column_name_from_struct_field(f)}"),')
		}
		g.writeln('})')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln('),')

	mut member_access_type := '.'

	if node.scope != unsafe { nil } {
		inserting_object := node.scope.find(node.object_var_name) or {
			verror('`${node.object_var_name}` is not found in scope')
		}

		if inserting_object.typ.is_ptr() {
			member_access_type = '->'
		}
	}

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
			mut sym := g.table.sym(field.typ)
			mut typ := sym.cname
			if sym.kind == .struct_ && typ != 'time__Time' {
				g.writeln('(*(orm__Primitive*) array_get(${last_ids_arr}, ${structs})),')
				structs++
				continue
			}
			if typ == 'time__Time' {
				typ = 'time'
			}
			var := '${node.object_var_name}${member_access_type}${c_name(field.name)}'
			if field.typ.has_flag(.option) {
				null := '(orm__Primitive){ ._typ = ${g.table.find_type_idx('orm.Null')}, ._orm__Null = &_const_orm__null_instance }'
				g.writeln('${var}.state == 2? ${null} : orm__${typ}_to_primitive(*(${typ}*)(${var}.data)),')
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
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(int), 0),')
	if auto_fields.len > 0 {
		g.writeln('.auto_fields = new_array_from_c_array(${auto_fields.len}, ${auto_fields.len}, sizeof(int),')
		g.indent++
		g.write('_MOV((int[${auto_fields.len}]){')
		for i in auto_fields {
			g.write(' ${i},')
		}
		g.writeln(' })),')
		g.indent--
	} else {
		g.writeln('.auto_fields = __new_array_with_default_noscan(0, 0, sizeof(int), 0),')
	}
	g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
	g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
	g.indent--
	g.writeln('}')
	g.indent--
	g.writeln(');')

	if arrs.len > 0 {
		mut id_name := g.new_tmp_var()
		g.writeln('orm__Primitive ${id_name} = orm__int_to_primitive(orm__Connection_name_table[${connection_var_name}._typ]._method_last_id(${connection_var_name}._object));')
		for i, mut arr in arrs {
			c_field_name := c_name(field_names[i])
			idx := g.new_tmp_var()
			g.writeln('for (int ${idx} = 0; ${idx} < ${arr.object_var_name}${member_access_type}${c_field_name}.len; ${idx}++) {')
			last_ids := g.new_tmp_var()
			res_ := g.new_tmp_var()
			tmp_var := g.new_tmp_var()
			ctyp := g.typ(arr.table_expr.typ)
			g.writeln('${ctyp} ${tmp_var} = (*(${ctyp}*)array_get(${arr.object_var_name}${member_access_type}${c_field_name}, ${idx}));')
			arr.object_var_name = tmp_var
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
			g.writeln('}')
		}
	}
}

// write_orm_expr_to_primitive writes C code for casting expressions into a primitive type
// by checking support expressions and their types.
fn (mut g Gen) write_orm_expr_to_primitive(expr ast.Expr) {
	match expr {
		ast.InfixExpr {
			g.write_orm_primitive(g.table.find_type_idx('orm.InfixType'), expr)
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
			verror('V ORM: ${expr.type_name()} is not supported')
		}
	}
}

// write_orm_primitive writes C code for casting expressions into a primitive type,
// which will be used in low-level database libs.
fn (mut g Gen) write_orm_primitive(t ast.Type, expr ast.Expr) {
	mut sym := g.table.sym(t)
	mut typ := sym.cname
	if typ == 'orm__Primitive' {
		g.expr(expr)
		g.writeln(',')
		return
	}
	if typ == 'none' {
		g.writeln('(orm__Primitive){ ._typ = ${g.table.find_type_idx('orm.Null')}, ._orm__Null = &_const_orm__null_instance },')
		return
	}
	if typ == 'time__Time' {
		typ = 'time'
	}
	if typ == 'orm__InfixType' {
		typ = 'infix'
	}

	g.write('orm__${typ}_to_primitive(')
	if expr is ast.InfixExpr {
		g.write('(orm__InfixType){')
		g.write(' .name = _SLIT("${expr.left}"),')
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
		g.write(' .operator = ${kind},')
		g.write(' .right = ')
		g.write_orm_expr_to_primitive(expr.right)
		g.write(' }')
	} else if expr is ast.CallExpr {
		g.call_expr(expr)
	} else {
		g.expr(expr)
	}
	g.writeln('),')
}

// write_orm_where writes C code that generates
// the `QueryData` structure for passing it into ORM methods.
fn (mut g Gen) write_orm_where(where_expr ast.Expr) {
	mut fields := []string{}
	mut kinds := []string{}
	mut parentheses := [][]int{}
	mut data := []ast.Expr{}
	mut is_ands := []bool{}

	g.writeln('// V ORM where')
	g.writeln('(orm__QueryData){')
	g.indent++
	g.write_orm_where_expr(where_expr, mut fields, mut parentheses, mut kinds, mut data, mut
		is_ands)
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(int), 0),')
	if fields.len > 0 {
		g.writeln('.fields = new_array_from_c_array(${fields.len}, ${fields.len}, sizeof(string),')
		g.indent++
		g.writeln('_MOV((string[${fields.len}]){')
		g.indent++
		for field in fields {
			g.writeln('_SLIT("${field}"),')
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
				g.write('new_array_from_c_array(${par.len}, ${par.len}, sizeof(int), _MOV((int[${par.len}]){')
				for val in par {
					g.write('${val},')
				}
				g.write('})),')
			} else {
				g.write('__new_array_with_default_noscan(0, 0, sizeof(int), 0),')
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
	g.writeln('),')
	g.writeln('}')
}

// write_orm_where_expr writes C code that generates expression which is used in the `QueryData`.
fn (mut g Gen) write_orm_where_expr(expr ast.Expr, mut fields []string, mut parentheses [][]int, mut kinds []string, mut data []ast.Expr, mut is_and []bool) {
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
				.key_is {
					'orm__OperationKind__is'
				}
				.not_is {
					'orm__OperationKind__is_not'
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
			g.sql_side = .right
			g.write_orm_where_expr(expr.right, mut fields, mut parentheses, mut kinds, mut
				data, mut is_and)
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
				fields << g.get_orm_column_name_from_struct_field(g.get_orm_current_table_field(expr.name))
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
			data << expr
		}
		ast.BoolLiteral {
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

// write_orm_select writes C code that calls ORM functions for selecting rows.
fn (mut g Gen) write_orm_select(node ast.SqlExpr, connection_var_name string, left_expr_string string, or_expr ast.OrExpr) {
	mut fields := []ast.StructField{}
	mut primary_field_name := g.get_orm_struct_primary_field_name(node.fields) or { '' }

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
	g.writeln('.table = _SLIT("${table_name}"),')
	g.writeln('.is_count = ${node.is_count},')
	g.writeln('.has_where = ${node.has_where},')
	g.writeln('.has_order = ${node.has_order},')

	if node.has_order {
		g.write('.order = _SLIT("')
		g.expr(node.order_expr)
		g.writeln('"),')
		if node.has_desc {
			g.writeln('.order_type = orm__OrderType__desc,')
		} else {
			g.writeln('.order_type = orm__OrderType__asc,')
		}
	}

	g.writeln('.has_limit = ${node.has_limit},')
	g.writeln('.has_offset = ${node.has_offset},')

	if primary_field_name != '' {
		g.writeln('.primary = _SLIT("${primary_field_name}"),')
	}

	select_fields := fields.filter(g.table.sym(it.typ).kind != .array)
	g.writeln('.fields = new_array_from_c_array(${select_fields.len}, ${select_fields.len}, sizeof(string),')
	g.indent++
	mut types := []string{}
	if select_fields.len > 0 {
		g.writeln('_MOV((string[${select_fields.len}]){')
		g.indent++
		for field in select_fields {
			g.writeln('_SLIT("${g.get_orm_column_name_from_struct_field(field)}"),')
			sym := g.table.sym(field.typ)
			if sym.name == 'time.Time' {
				types << '_const_orm__time'
				continue
			}
			if sym.kind == .struct_ {
				types << int(ast.int_type).str()
				continue
			}
			types << field.typ.idx().str()
		}
		g.indent--
		g.writeln('})')
	} else {
		g.writeln('NULL')
	}
	g.indent--
	g.writeln('),')
	g.writeln('.types = new_array_from_c_array(${types.len}, ${types.len}, sizeof(int),')
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
	g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(int), 0),')
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
		g.writeln('.types = __new_array_with_default_noscan(0, 0, sizeof(int), 0),')
		g.writeln('.kinds = __new_array_with_default_noscan(0, 0, sizeof(orm__OperationKind), 0),')
		g.writeln('.is_and = __new_array_with_default_noscan(0, 0, sizeof(bool), 0),')
		g.writeln('.parentheses = __new_array_with_default_noscan(0, 0, sizeof(Array_int), 0),')
		g.writeln('.data = __new_array_with_default_noscan(0, 0, sizeof(orm__Primitive), 0)')
		g.indent--
		g.writeln('}')
	}
	g.indent--
	g.writeln(');')

	unwrapped_typ := node.typ.clear_flag(.result)
	unwrapped_c_typ := g.typ(unwrapped_typ)
	c_typ := g.typ(node.typ)

	mut non_orm_result_var_name := g.new_tmp_var()
	g.writeln('${c_typ} ${non_orm_result_var_name};')
	g.writeln('${non_orm_result_var_name}.is_error = ${select_result_var_name}.is_error;')
	g.writeln('${non_orm_result_var_name}.err = ${select_result_var_name}.err;')
	g.or_block(non_orm_result_var_name, node.or_expr, node.typ)

	select_unwrapped_result_var_name := g.new_tmp_var()

	g.writeln('Array_Array_orm__Primitive ${select_unwrapped_result_var_name} = (*(Array_Array_orm__Primitive*)${select_result_var_name}.data);')

	if node.is_count {
		g.writeln('*(${unwrapped_c_typ}*) ${non_orm_result_var_name}.data = *((*(orm__Primitive*) array_get((*(Array_orm__Primitive*)array_get(${select_unwrapped_result_var_name}, 0)), 0))._int);')
	} else {
		tmp := g.new_tmp_var()
		idx := g.new_tmp_var()
		g.writeln('int ${idx} = 0;')
		mut typ_str := ''
		if node.is_array {
			info := g.table.sym(node.typ).array_info()
			typ_str = g.typ(info.elem_type)
			g.writeln('${unwrapped_c_typ} ${tmp}_array = __new_array(0, ${select_unwrapped_result_var_name}.len, sizeof(${typ_str}));')
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
		for i, field in fields {
			array_get_call_code := '(*(orm__Primitive*) array_get((*(Array_orm__Primitive*) array_get(${select_unwrapped_result_var_name}, ${idx})), ${i}))'
			sym := g.table.sym(field.typ)
			if sym.kind == .struct_ && sym.name != 'time.Time' {
				mut sub := node.sub_structs[int(field.typ)]
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident
				primitive_type_index := g.table.find_type_idx('orm.Primitive')

				if primitive_type_index != 0 {
					if mut ident.info is ast.IdentVar {
						ident.info.typ = primitive_type_index
					}
				}

				ident.name = array_get_call_code
				where_expr.right = ident
				sub.where_expr = where_expr

				g.write_orm_select(sub, connection_var_name, '${tmp}.${c_name(field.name)} = ',
					or_expr)
			} else if sym.kind == .array {
				mut fkey := ''
				// TODO: move to the ORM checker
				for attr in field.attrs {
					if attr.name == 'fkey' && attr.has_arg {
						if attr.kind == .string {
							fkey = attr.arg
						} else {
							verror("`fkey` attribute need be string. Try [fkey: '${attr.arg}'] instead of [fkey: ${attr.arg}]")
						}
					}
				}
				// TODO: move to the ORM checker
				if fkey == '' {
					verror('a field which holds an array, needs a `fkey` defined ("${sym.name}")')
				}
				info := sym.array_info()
				arr_typ := info.elem_type
				sub := node.sub_structs[int(arr_typ)]
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut left_where_expr := where_expr.left as ast.Ident
				mut right_where_expr := where_expr.right as ast.Ident
				left_where_expr.name = fkey
				right_where_expr.name = tmp
				where_expr.left = left_where_expr
				where_expr.right = ast.SelectorExpr{
					pos: right_where_expr.pos
					field_name: primary_field_name
					is_mut: false
					expr: right_where_expr
					expr_type: (right_where_expr.info as ast.IdentVar).typ
					typ: ast.int_type
					scope: 0
				}
				mut sql_expr_select_array := ast.SqlExpr{
					typ: field.typ.set_flag(.result)
					is_count: sub.is_count
					db_expr: sub.db_expr
					has_where: sub.has_where
					has_offset: sub.has_offset
					offset_expr: sub.offset_expr
					has_order: sub.has_order
					order_expr: sub.order_expr
					has_desc: sub.has_desc
					is_array: true
					is_generated: true
					pos: sub.pos
					has_limit: sub.has_limit
					limit_expr: sub.limit_expr
					table_expr: sub.table_expr
					fields: sub.fields
					where_expr: where_expr
				}

				g.write_orm_select(sql_expr_select_array, connection_var_name, '${tmp}.${c_name(field.name)} = ',
					or_expr)
			} else if field.typ.has_flag(.option) {
				mut typ := sym.cname
				styp := g.typ(field.typ)

				// TIMEDIT
				prim := g.new_tmp_var()
				sfield := c_name(field.name)
				g.writeln('orm__Primitive *${prim} = &${array_get_call_code};')
				g.writeln('if (${prim}->_typ == ${g.table.find_type_idx('orm.Null')})')
				g.indent++
				g.writeln('${tmp}.${sfield} = (${styp}){ .state = 2, .err = _const_none__, .data = {EMPTY_STRUCT_INITIALIZATION} };')

				g.indent--
				g.writeln('else')
				g.indent++
				g.writeln('_option_ok(${prim}->_${typ}, (_option *)&${tmp}.${sfield}, sizeof(${typ}));')
				g.indent--
			} else {
				mut typ := sym.cname
				g.writeln('${tmp}.${c_name(field.name)} = *(${array_get_call_code}._${typ});')
			}
		}

		if node.is_array {
			g.writeln('array_push(&${tmp}_array, _MOV((${typ_str}[]){ ${tmp} }));')
			g.indent--
			g.writeln('}')
		}

		g.write('*(${unwrapped_c_typ}*) ${non_orm_result_var_name}.data = ${tmp}')
		if node.is_array {
			g.write('_array')
		}
		g.writeln(';')

		g.indent--
		g.writeln('}')
	}
	g.write('${left_expr_string.trim_space()} *(${unwrapped_c_typ}*) ${non_orm_result_var_name}.data')

	if node.is_generated {
		g.write(';')
	}
}

// filter_struct_fields_by_orm_attrs filters struct fields taking into its attributes.
// Used by non-create queries for skipping fields.
fn (_ &Gen) filter_struct_fields_by_orm_attrs(fields []ast.StructField) []ast.StructField {
	mut result := []ast.StructField{}

	for field in fields {
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
			result << field
		}
	}

	return result
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

// get_table_name_by_struct_type converts the struct type to a table name.
fn (g &Gen) get_table_name_by_struct_type(typ ast.Type) string {
	info := g.table.sym(typ).struct_info()
	mut table_name := util.strip_mod_name(g.table.sym(typ).name)

	for attr in info.attrs {
		if attr.kind == .string && attr.name == 'table' && attr.arg != '' {
			return attr.arg
		}
	}

	return table_name
}

// get_orm_current_table_field returns the current processing table's struct field by name.
fn (g &Gen) get_orm_current_table_field(name string) ast.StructField {
	info := g.table.sym(g.table.type_idxs[g.sql_table_name]).struct_info()

	for field in info.fields {
		if field.name == name {
			return field
		}
	}

	return ast.StructField{}
}

// get_orm_column_name_from_struct_field converts the struct field to a table column name.
fn (g &Gen) get_orm_column_name_from_struct_field(field ast.StructField) string {
	mut name := field.name

	for attr in field.attrs {
		if attr.kind == .string && attr.name == 'sql' && attr.arg != '' {
			name = attr.arg
			break
		}
	}

	sym := g.table.sym(field.typ)
	if sym.kind == .struct_ && sym.name != 'time.Time' {
		name = '${name}_id'
	}

	return name
}

// get_orm_struct_primary_field_name returns the table's primary column name.
fn (_ &Gen) get_orm_struct_primary_field_name(fields []ast.StructField) ?string {
	for field in fields {
		for attr in field.attrs {
			if attr.name == 'primary' {
				return field.name
			}
		}
	}
	return none
}

// return indexes of any auto-increment fields or fields with default values
fn get_auto_fields(fields []ast.StructField) []int {
	mut ret := []int{}
	for i, field in fields {
		for attr in field.attrs {
			if attr.name == 'sql' && attr.arg.to_lower() == 'serial' {
				ret << i
			} else if attr.name == 'default' {
				ret << i
			}
		}
	}
	return ret
}
