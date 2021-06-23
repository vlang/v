// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import strings
import v.util

// pg,mysql etc
const (
	dbtype                 = 'sqlite'
	default_unique_str_len = 256
)

enum SqlExprSide {
	left
	right
}

enum SqlType {
	sqlite3
	mysql
	psql
	mssql
	unknown
}

fn (mut g Gen) sql_stmt(node ast.SqlStmt) {
	for line in node.lines {
		g.sql_stmt_line(line, node.db_expr)
	}
}

fn (mut g Gen) sql_stmt_line(node ast.SqlStmtLine, expr ast.Expr) {
	if node.kind == .create {
		g.sql_create_table(node, expr)
		return
	} else if node.kind == .drop {
		g.sql_drop_table(node, expr)
		return
	}
	g.sql_table_name = g.table.get_type_symbol(node.table_expr.typ).name
	typ := g.parse_db_type(expr)
	match typ {
		.sqlite3 {
			g.sqlite3_stmt(node, typ, expr)
		}
		.mysql {
			g.mysql_stmt(node, typ, expr)
		}
		.psql {
			g.psql_stmt(node, typ, expr)
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}
}

fn (mut g Gen) sql_create_table(node ast.SqlStmtLine, expr ast.Expr) {
	typ := g.parse_db_type(expr)
	match typ {
		.sqlite3 {
			g.sqlite3_create_table(node, typ, expr)
		}
		.mysql {
			g.mysql_create_table(node, typ, expr)
		}
		.psql {
			g.psql_create_table(node, typ, expr)
		}
		.mssql {
			g.mssql_create_table(node, typ, expr)
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}
}

fn (mut g Gen) sql_drop_table(node ast.SqlStmtLine, expr ast.Expr) {
	typ := g.parse_db_type(expr)
	match typ {
		.sqlite3 {
			g.sqlite3_drop_table(node, typ, expr)
		}
		.mysql {
			g.mysql_drop_table(node, typ, expr)
		}
		.psql {
			g.psql_drop_table(node, typ, expr)
		}
		.mssql {
			g.mssql_drop_table(node, typ, expr)
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}
}

fn (mut g Gen) sql_select_expr(node ast.SqlExpr, sub bool, line string) {
	g.sql_table_name = g.table.get_type_symbol(node.table_expr.typ).name
	typ := g.parse_db_type(node.db_expr)
	match typ {
		.sqlite3 {
			g.sqlite3_select_expr(node, sub, line, typ)
		}
		.mysql {
			g.mysql_select_expr(node, sub, line, typ)
		}
		.psql {
			g.psql_select_expr(node, sub, line, typ)
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}
}

fn (mut g Gen) sql_bind(val string, len string, real_type ast.Type, typ SqlType) {
	match typ {
		.sqlite3 {
			g.sqlite3_bind(val, len, real_type)
		}
		.mysql {
			g.mysql_bind(val, real_type)
		}
		.psql {
			g.psql_bind(val, real_type)
		}
		else {}
	}
}

fn (mut g Gen) sql_type_from_v(typ SqlType, v_typ ast.Type) string {
	match typ {
		.sqlite3 {
			return g.sqlite3_type_from_v(v_typ)
		}
		.mysql {
			return g.mysql_get_table_type(v_typ)
		}
		.psql {
			return g.psql_get_table_type(v_typ)
		}
		.mssql {
			return g.mssql_get_table_type(v_typ)
		}
		else {
			// add error
		}
	}
	return ''
}

// sqlite3

fn (mut g Gen) sqlite3_stmt(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.sql_i = 0
	g.writeln('\n\t// sql insert')
	db_name := g.new_tmp_var()
	g.sql_stmt_name = g.new_tmp_var()
	g.write('${c.dbtype}__DB $db_name = ')
	g.expr(db_expr)
	g.writeln(';')
	g.write('sqlite3_stmt* $g.sql_stmt_name = ${c.dbtype}__DB_init_stmt($db_name, _SLIT("')
	g.sql_defaults(node, typ)
	g.writeln(');')
	mut arr_stmt := []ast.SqlStmtLine{}
	mut arr_fkeys := []string{}
	mut arr_field_name := []string{}
	if node.kind == .insert {
		// build the object now (`x.name = ... x.id == ...`)
		for i, field in node.fields {
			if g.get_sql_field_type(field) == ast.Type(-1) {
				continue
			}
			if field.name == g.sql_fkey && g.sql_fkey != '' {
				g.writeln('sqlite3_bind_int($g.sql_stmt_name, ${i + 0} , $g.sql_parent_id); // parent id')
				continue
			}
			x := '${node.object_var_name}.$field.name'
			if field.typ == ast.string_type {
				g.writeln('sqlite3_bind_text($g.sql_stmt_name, ${i + 0}, (char*)${x}.str, ${x}.len, 0);')
			} else if g.table.get_type_symbol(field.typ).kind == .struct_ {
				// insert again
				expr := node.sub_structs[int(field.typ)]
				tmp_sql_stmt_name := g.sql_stmt_name
				tmp_sql_table_name := g.sql_table_name
				g.sql_stmt_line(expr, db_expr)
				g.sql_stmt_name = tmp_sql_stmt_name
				g.sql_table_name = tmp_sql_table_name
				// get last inserted id
				res := g.new_tmp_var()
				g.writeln('Array_sqlite__Row $res = sqlite__DB_exec($db_name, _SLIT("SELECT last_insert_rowid()")).arg0;')
				id_name := g.new_tmp_var()
				g.writeln('int $id_name = string_int((*(string*)array_get((*(sqlite__Row*)array_get($res, 0)).vals, 0)));')
				g.writeln('sqlite3_bind_int($g.sql_stmt_name, ${i + 0} , $id_name); // id')
			} else if g.table.get_type_symbol(field.typ).kind == .array {
				t := g.table.get_type_symbol(field.typ).array_info().elem_type
				if g.table.get_type_symbol(t).kind == .struct_ {
					mut fkey := ''
					for attr in field.attrs {
						if attr.name == 'fkey' && attr.arg != '' && attr.kind == .string {
							fkey = attr.arg
							break
						}
					}
					if fkey == '' {
						verror('fkey attribute has to be set for arrays in orm')
						continue
					}

					arr_stmt << node.sub_structs[int(t)]
					arr_fkeys << fkey
					arr_field_name << field.name
				}
			} else {
				g.writeln('sqlite3_bind_int($g.sql_stmt_name, ${i + 0} , $x); // stmt')
			}
		}
	}
	// Dump all sql parameters generated by our custom expr handler
	binds := g.sql_buf.str()
	g.sql_buf = strings.new_builder(100)
	g.writeln(binds)
	step_res := g.new_tmp_var()
	g.writeln('\tint $step_res = sqlite3_step($g.sql_stmt_name);')
	g.writeln('\tif( ($step_res != SQLITE_OK) && ($step_res != SQLITE_DONE)){ puts(sqlite3_errmsg(${db_name}.conn)); }')
	g.writeln('\tsqlite3_finalize($g.sql_stmt_name);')

	if arr_stmt.len > 0 {
		res := g.new_tmp_var()
		g.writeln('Array_sqlite__Row $res = sqlite__DB_exec($db_name, _SLIT("SELECT last_insert_rowid()")).arg0;')
		id_name := g.new_tmp_var()
		g.writeln('int $id_name = string_int((*(string*)array_get((*(sqlite__Row*)array_get($res, 0)).vals, 0)));')

		g.sql_arr_stmt(arr_stmt, arr_fkeys, arr_field_name, id_name, db_expr)
	}
}

fn (mut g Gen) sqlite3_select_expr(node ast.SqlExpr, sub bool, line string, sql_typ SqlType) {
	g.sql_i = 0
	/*
	`nr_users := sql db { ... }` =>
	```
		sql_init_stmt()
		sqlite3_bind_int()
		sqlite3_bind_string()
		...
		int nr_users = get_int(stmt)
	```
	*/
	mut cur_line := line
	if !sub {
		cur_line = g.go_before_stmt(0)
	}
	// g.write('${dbtype}__DB_q_int(*(${dbtype}__DB*)${node.db_var_name}.data, _SLIT("$sql_query')
	g.sql_stmt_name = g.new_tmp_var()
	db_name := g.new_tmp_var()
	g.writeln('\n\t// sql select')
	// g.write('${dbtype}__DB $db_name = *(${dbtype}__DB*)${node.db_var_name}.data;')
	g.write('${c.dbtype}__DB $db_name = ') // $node.db_var_name;')
	g.expr(node.db_expr)
	g.writeln(';')
	stmt_name := g.new_tmp_var()
	g.write('string $stmt_name = _SLIT("')
	g.write(g.get_base_sql_select_query(node, sql_typ))
	g.sql_expr_defaults(node, sql_typ)
	g.writeln('");')
	// g.write('sqlite3_stmt* $g.sql_stmt_name = ${dbtype}__DB_init_stmt(*(${dbtype}__DB*)${node.db_var_name}.data, _SLIT("$sql_query')
	g.write('sqlite3_stmt* $g.sql_stmt_name = ${c.dbtype}__DB_init_stmt($db_name, $stmt_name);')
	// Dump all sql parameters generated by our custom expr handler
	binds := g.sql_buf.str()
	g.sql_buf = strings.new_builder(100)
	g.writeln(binds)
	binding_res := g.new_tmp_var()
	g.writeln('int $binding_res = sqlite3_extended_errcode(${db_name}.conn);')
	g.writeln('if ($binding_res != SQLITE_OK) { puts(sqlite3_errmsg(${db_name}.conn)); }')
	//
	if node.is_count {
		g.writeln('$cur_line ${c.dbtype}__get_int_from_stmt($g.sql_stmt_name);')
	} else {
		// `user := sql db { select from User where id = 1 }`
		tmp := g.new_tmp_var()
		styp := g.typ(node.typ)
		mut elem_type_str := ''
		if node.is_array {
			// array_User array_tmp;
			// for { User tmp; ... array_tmp << tmp; }
			array_sym := g.table.get_type_symbol(node.typ)
			array_info := array_sym.info as ast.Array
			elem_type_str = g.typ(array_info.elem_type)
			g.writeln('$styp ${tmp}_array = __new_array(0, 10, sizeof($elem_type_str));')
			g.writeln('while (1) {')
			g.writeln('\t$elem_type_str $tmp = ($elem_type_str) {')
			//
			sym := g.table.get_type_symbol(array_info.elem_type)
			info := sym.info as ast.Struct
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		} else {
			// `User tmp;`
			g.writeln('$styp $tmp = ($styp){')
			// Zero fields, (only the [skip] ones?)
			// If we don't, string values are going to be nil etc for fields that are not returned
			// by the db engine.
			sym := g.table.get_type_symbol(node.typ)
			info := sym.info as ast.Struct
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		}
		//
		g.writeln('int _step_res$tmp = sqlite3_step($g.sql_stmt_name);')
		if node.is_array {
			// g.writeln('\tprintf("step res=%d\\n", _step_res$tmp);')
			g.writeln('\tif (_step_res$tmp == SQLITE_DONE) break;')
			g.writeln('\tif (_step_res$tmp == SQLITE_ROW) ;') // another row
			g.writeln('\telse if (_step_res$tmp != SQLITE_OK) break;')
		} else {
			// g.writeln('printf("RES: %d\\n", _step_res$tmp) ;')
			g.writeln('\tif (_step_res$tmp == SQLITE_OK || _step_res$tmp == SQLITE_ROW) {')
		}
		mut primary := ''
		for i, field in node.fields {
			for attr in field.attrs {
				if attr.name == 'primary' {
					primary = '${tmp}.$field.name'
					break
				}
			}
			mut func := 'sqlite3_column_int'
			if field.typ == ast.string_type {
				func = 'sqlite3_column_text'
				string_data := g.new_tmp_var()
				g.writeln('byte* $string_data = (byte*)${func}($g.sql_stmt_name, $i);')
				g.writeln('if ($string_data != NULL) {')
				g.writeln('\t${tmp}.$field.name = tos_clone($string_data);')
				g.writeln('}')
			} else if g.table.get_type_symbol(field.typ).kind == .struct_ {
				id_name := g.new_tmp_var()
				g.writeln('//parse struct start')
				g.writeln('int $id_name = ${func}($g.sql_stmt_name, $i);')
				mut expr := node.sub_structs[int(field.typ)]
				mut where_expr := expr.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident
				ident.name = id_name
				where_expr.right = ident
				expr.where_expr = where_expr

				tmp_sql_i := g.sql_i
				tmp_sql_stmt_name := g.sql_stmt_name
				tmp_sql_buf := g.sql_buf
				tmp_sql_table_name := g.sql_table_name

				g.sql_select_expr(expr, true, '\t${tmp}.$field.name =')
				g.writeln('//parse struct end')

				g.sql_stmt_name = tmp_sql_stmt_name
				g.sql_buf = tmp_sql_buf
				g.sql_i = tmp_sql_i
				g.sql_table_name = tmp_sql_table_name
			} else if g.table.get_type_symbol(field.typ).kind == .array {
				g.sql_select_arr(field, node, primary, tmp)
			} else {
				g.writeln('${tmp}.$field.name = ${func}($g.sql_stmt_name, $i);')
			}
		}
		if node.is_array {
			g.writeln('\t array_push((array*)&${tmp}_array, _MOV(($elem_type_str[]){ $tmp }));\n')
		}
		g.writeln('}')
		g.writeln('sqlite3_finalize($g.sql_stmt_name);')
		if node.is_array {
			g.writeln('$cur_line ${tmp}_array; ') // `array_User users = tmp_array;`
		} else {
			g.writeln('$cur_line $tmp; ') // `User user = tmp;`
		}
	}
}

fn (mut g Gen) sqlite3_create_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.writeln('// sqlite3 table creator')
	create_string := g.table_gen(node, typ, db_expr)
	g.write('sqlite__DB_exec(')
	g.expr(db_expr)
	g.writeln(', _SLIT("$create_string"));')
}

fn (mut g Gen) sqlite3_drop_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	table_name := g.get_table_name(node.table_expr)
	g.writeln('// sqlite3 table drop')
	drop_string := 'DROP TABLE `$table_name`;'
	g.write('sqlite__DB_exec(')
	g.expr(db_expr)
	g.writeln(', _SLIT("$drop_string"));')
}

fn (mut g Gen) sqlite3_bind(val string, len string, typ ast.Type) {
	match g.sqlite3_type_from_v(typ) {
		'INTEGER' {
			g.sqlite3_bind_int(val)
		}
		'TEXT' {
			g.sqlite3_bind_string(val, len)
		}
		else {
			verror('bad sql type=$typ ident_name=$val')
		}
	}
}

fn (mut g Gen) sqlite3_bind_int(val string) {
	g.sql_buf.writeln('sqlite3_bind_int($g.sql_stmt_name, $g.sql_i, $val);')
}

fn (mut g Gen) sqlite3_bind_string(val string, len string) {
	g.sql_buf.writeln('sqlite3_bind_text($g.sql_stmt_name, $g.sql_i, (char*)$val, $len, 0);')
}

fn (mut g Gen) sqlite3_type_from_v(v_typ ast.Type) string {
	if v_typ.is_number() || v_typ == ast.bool_type || v_typ == -1 {
		return 'INTEGER'
	}
	if v_typ.is_string() {
		return 'TEXT'
	}
	return ''
}

// mysql

fn (mut g Gen) mysql_stmt(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.sql_i = 0
	g.writeln('\n\t//mysql insert')
	db_name := g.new_tmp_var()
	g.sql_stmt_name = g.new_tmp_var()
	g.write('mysql__Connection $db_name = ')
	g.expr(db_expr)
	g.writeln(';')
	stmt_name := g.new_tmp_var()
	g.write('string $stmt_name = _SLIT("')
	g.sql_defaults(node, typ)
	g.writeln(';')
	g.writeln('MYSQL_STMT* $g.sql_stmt_name = mysql_stmt_init(${db_name}.conn);')
	g.writeln('mysql_stmt_prepare($g.sql_stmt_name, ${stmt_name}.str, ${stmt_name}.len);')

	bind := g.new_tmp_var()
	g.writeln('MYSQL_BIND $bind[$g.sql_i];')
	g.writeln('memset($bind, 0, sizeof(MYSQL_BIND)*$g.sql_i);')
	mut arr_stmt := []ast.SqlStmtLine{}
	mut arr_fkeys := []string{}
	mut arr_field_name := []string{}
	if node.kind == .insert {
		for i, field in node.fields {
			if g.get_sql_field_type(field) == ast.Type(-1) {
				continue
			}
			if field.name == g.sql_fkey && g.sql_fkey != '' {
				t, sym := g.mysql_buffer_typ_from_field(field)
				g.writeln('$bind[${i - 1}].buffer_type = $t;')
				if sym == 'char' {
					g.writeln('$bind[${i - 1}].buffer = ($sym*) ${g.sql_parent_id}.str;')
				} else {
					g.writeln('$bind[${i - 1}].buffer = ($sym*) &$g.sql_parent_id;')
				}
				if sym == 'char' {
					g.writeln('$bind[${i - 1}].buffer_length = ${g.sql_parent_id}.len;')
				}
				g.writeln('$bind[${i - 1}].is_null = 0;')
				g.writeln('$bind[${i - 1}].length = 0;')
				continue
			}
			g.writeln('//$field.name ($field.typ)')
			x := '${node.object_var_name}.$field.name'
			if g.table.get_type_symbol(field.typ).kind == .struct_ {
				// insert again
				expr := node.sub_structs[int(field.typ)]
				tmp_sql_stmt_name := g.sql_stmt_name
				tmp_sql_table_name := g.sql_table_name
				g.sql_stmt_line(expr, db_expr)
				g.sql_stmt_name = tmp_sql_stmt_name
				g.sql_table_name = tmp_sql_table_name

				res := g.new_tmp_var()
				g.writeln('int ${res}_err = mysql_real_query(${db_name}.conn, "SELECT LAST_INSERT_ID();", 24);')
				g.writeln('if (${res}_err != 0) { puts(mysql_error(${db_name}.conn)); }')
				g.writeln('MYSQL_RES* $res = mysql_store_result(${db_name}.conn);')
				g.writeln('if (mysql_num_rows($res) != 1) { puts("Something went wrong"); }')
				g.writeln('MYSQL_ROW ${res}_row = mysql_fetch_row($res);')
				g.writeln('${x}.id = string_int(tos_clone(${res}_row[0]));')
				g.writeln('mysql_free_result($res);')

				g.writeln('$bind[${i - 1}].buffer_type = MYSQL_TYPE_LONG;')
				g.writeln('$bind[${i - 1}].buffer = &${x}.id;')
				g.writeln('$bind[${i - 1}].is_null = 0;')
				g.writeln('$bind[${i - 1}].length = 0;')
			} else if g.table.get_type_symbol(field.typ).kind == .array {
				t := g.table.get_type_symbol(field.typ).array_info().elem_type
				if g.table.get_type_symbol(t).kind == .struct_ {
					mut fkey := ''
					for attr in field.attrs {
						if attr.name == 'fkey' && attr.arg != '' && attr.kind == .string {
							fkey = attr.arg
							break
						}
					}
					if fkey == '' {
						verror('fkey attribute has to be set for arrays in orm')
						continue
					}

					arr_stmt << node.sub_structs[int(t)]
					arr_fkeys << fkey
					arr_field_name << field.name
				}
			} else {
				t, sym := g.mysql_buffer_typ_from_field(field)
				g.writeln('$bind[${i - 1}].buffer_type = $t;')
				if sym == 'char' {
					g.writeln('$bind[${i - 1}].buffer = ($sym*) ${x}.str;')
				} else {
					g.writeln('$bind[${i - 1}].buffer = ($sym*) &$x;')
				}
				if sym == 'char' {
					g.writeln('$bind[${i - 1}].buffer_length = ${x}.len;')
				}
				g.writeln('$bind[${i - 1}].is_null = 0;')
				g.writeln('$bind[${i - 1}].length = 0;')
			}
		}
	}
	binds := g.sql_buf.str()
	g.sql_buf = strings.new_builder(100)
	g.writeln(binds)
	res := g.new_tmp_var()
	g.writeln('int $res = mysql_stmt_bind_param($g.sql_stmt_name, $bind);')
	g.writeln('if ($res != 0) { puts(mysql_error(${db_name}.conn)); }')
	g.writeln('$res = mysql_stmt_execute($g.sql_stmt_name);')
	g.writeln('if ($res != 0) { puts(mysql_error(${db_name}.conn)); puts(mysql_stmt_error($g.sql_stmt_name)); }')
	g.writeln('mysql_stmt_close($g.sql_stmt_name);')
	g.writeln('mysql_stmt_free_result($g.sql_stmt_name);')

	if arr_stmt.len > 0 {
		rs := g.new_tmp_var()
		g.writeln('int ${rs}_err = mysql_real_query(${db_name}.conn, "SELECT LAST_INSERT_ID();", 24);')
		g.writeln('if (${rs}_err != 0) { puts(mysql_error(${db_name}.conn)); }')
		g.writeln('MYSQL_RES* $rs = mysql_store_result(${db_name}.conn);')
		g.writeln('if (mysql_num_rows($rs) != 1) { puts("Something went wrong"); }')
		g.writeln('MYSQL_ROW ${rs}_row = mysql_fetch_row($rs);')
		id_name := g.new_tmp_var()
		g.writeln('int $id_name = string_int(tos_clone(${rs}_row[0]));')
		g.writeln('mysql_free_result($rs);')

		g.sql_arr_stmt(arr_stmt, arr_fkeys, arr_field_name, id_name, db_expr)
	}
}

fn (mut g Gen) mysql_select_expr(node ast.SqlExpr, sub bool, line string, typ SqlType) {
	g.sql_i = 0
	mut cur_line := line
	if !sub {
		cur_line = g.go_before_stmt(0)
	}
	g.sql_stmt_name = g.new_tmp_var()
	g.sql_bind_name = g.new_tmp_var()
	db_name := g.new_tmp_var()
	g.writeln('\n\t// sql select')
	g.write('mysql__Connection $db_name = ')
	g.expr(node.db_expr)
	g.writeln(';')

	g.write('string $g.sql_stmt_name = _SLIT("')
	g.write(g.get_base_sql_select_query(node, typ))
	g.sql_expr_defaults(node, typ)
	g.writeln('");')

	rplc := g.sql_buf.str()
	g.sql_buf = strings.new_builder(100)
	g.writeln(rplc)

	query := g.new_tmp_var()
	res := g.new_tmp_var()
	fields := g.new_tmp_var()

	g.writeln('int $query = mysql_real_query(${db_name}.conn, ${g.sql_stmt_name}.str, ${g.sql_stmt_name}.len);')
	g.writeln('if ($query != 0) { puts(mysql_error(${db_name}.conn)); }')
	g.writeln('MYSQL_RES* $res = mysql_store_result(${db_name}.conn);')
	g.writeln('MYSQL_ROW $fields = mysql_fetch_row($res);')
	if node.is_count {
		g.writeln('$cur_line string_int(tos_clone($fields[0]));')
	} else {
		tmp := g.new_tmp_var()
		styp := g.typ(node.typ)
		tmp_i := g.new_tmp_var()
		mut elem_type_str := ''
		g.writeln('int $tmp_i = 0;')
		if node.is_array {
			array_sym := g.table.get_type_symbol(node.typ)
			array_info := array_sym.info as ast.Array
			elem_type_str = g.typ(array_info.elem_type)
			g.writeln('$styp ${tmp}_array = __new_array(0, 10, sizeof($elem_type_str));')
			g.writeln('for ($tmp_i = 0; $tmp_i < mysql_num_rows($res); $tmp_i++) {')
			g.writeln('\t$elem_type_str $tmp = ($elem_type_str) {')
			//
			sym := g.table.get_type_symbol(array_info.elem_type)
			info := sym.info as ast.Struct
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		} else {
			g.writeln('$styp $tmp = ($styp){')
			// Zero fields, (only the [skip] ones?)
			// If we don't, string values are going to be nil etc for fields that are not returned
			// by the db engine.
			sym := g.table.get_type_symbol(node.typ)
			info := sym.info as ast.Struct
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		}

		char_ptr := g.new_tmp_var()
		g.writeln('char* $char_ptr = "";')
		mut primary := ''
		for i, field in node.fields {
			for attr in field.attrs {
				if attr.name == 'primary' {
					primary = '${tmp}.$field.name'
					break
				}
			}
			g.writeln('$char_ptr = $fields[$i];')
			g.writeln('if ($char_ptr == NULL) { $char_ptr = ""; }')
			name := g.table.get_type_symbol(field.typ).cname
			if g.table.get_type_symbol(field.typ).kind == .struct_ {
				g.writeln('//parse struct start')

				mut expr := node.sub_structs[int(field.typ)]
				mut where_expr := expr.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident

				ident.name = '$char_ptr[$i]'
				where_expr.right = ident
				expr.where_expr = where_expr

				tmp_sql_i := g.sql_i
				tmp_sql_stmt_name := g.sql_stmt_name
				tmp_sql_buf := g.sql_buf
				tmp_sql_table_name := g.sql_table_name

				g.sql_select_expr(expr, true, '\t${tmp}.$field.name =')
				g.writeln('//parse struct end')

				g.sql_stmt_name = tmp_sql_stmt_name
				g.sql_buf = tmp_sql_buf
				g.sql_i = tmp_sql_i
				g.sql_table_name = tmp_sql_table_name
			} else if g.table.get_type_symbol(field.typ).kind == .array {
				g.sql_select_arr(field, node, primary, tmp)
			} else if field.typ == ast.string_type {
				g.writeln('${tmp}.$field.name = tos_clone($char_ptr);')
			} else if field.typ == ast.byte_type {
				g.writeln('${tmp}.$field.name = (byte) string_${name}(tos_clone($char_ptr));')
			} else if field.typ == ast.i8_type {
				g.writeln('${tmp}.$field.name = (i8) string_${name}(tos_clone($char_ptr));')
			} else {
				g.writeln('${tmp}.$field.name = string_${name}(tos_clone($char_ptr));')
			}
		}
		if node.is_array {
			g.writeln('\t array_push((array*)&${tmp}_array, _MOV(($elem_type_str[]) { $tmp }));\n')
			g.writeln('\t $fields = mysql_fetch_row($res);')
			g.writeln('}')
		}
		g.writeln('string_free(&$g.sql_stmt_name);')
		g.writeln('mysql_free_result($res);')
		if node.is_array {
			g.writeln('$cur_line ${tmp}_array; ')
		} else {
			g.writeln('$cur_line $tmp; ')
		}
	}
}

fn (mut g Gen) mysql_create_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.writeln('// mysql table creator')
	create_string := g.table_gen(node, typ, db_expr)
	tmp := g.new_tmp_var()
	g.write('Option_mysql__Result $tmp = mysql__Connection_query(&')
	g.expr(db_expr)
	g.writeln(', _SLIT("$create_string"));')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${tmp}.state != 0) { IError err = ${tmp}.err; eprintln($tmp_str); }')
}

fn (mut g Gen) mysql_drop_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	table_name := g.get_table_name(node.table_expr)
	g.writeln('// mysql table drop')
	drop_string := 'DROP TABLE `$table_name`;'
	tmp := g.new_tmp_var()
	g.write('Option_mysql__Result $tmp = mysql__Connection_query(&')
	g.expr(db_expr)
	g.writeln(', _SLIT("$drop_string"));')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${tmp}.state != 0) { IError err = ${tmp}.err; eprintln($tmp_str); }')
}

fn (mut g Gen) mysql_bind(val string, typ ast.Type) {
	g.write('$g.sql_i')
	mut sym := g.table.get_type_symbol(typ).cname
	g.sql_buf.write_string('$g.sql_stmt_name = string_replace($g.sql_stmt_name, _SLIT("?$g.sql_i"), ')
	if sym != 'string' {
		mut num := false
		if sym != 'bool' {
			num = true
			g.sql_buf.write_string('${sym}_str(')
		}
		g.sql_buf.write_string('(($sym) $val)')

		if sym == 'bool' {
			g.sql_buf.write_string('? _SLIT("1") : _SLIT("0")')
		}

		if num {
			g.sql_buf.write_string(')')
		}
	} else {
		g.sql_buf.write_string('string__plus(_SLIT("\'"), string__plus(((string) $val), _SLIT("\'")))')
	}
	g.sql_buf.writeln(');')
}

fn (mut g Gen) mysql_get_table_type(typ ast.Type) string {
	mut table_typ := ''
	match typ {
		ast.i8_type, ast.byte_type, ast.bool_type {
			table_typ = 'TINYINT'
		}
		ast.i16_type, ast.u16_type {
			table_typ = 'SMALLINT'
		}
		ast.int_type, ast.u32_type {
			table_typ = 'INT'
		}
		ast.i64_type, ast.u64_type {
			table_typ = 'BIGINT'
		}
		ast.f32_type {
			table_typ = 'FLOAT'
		}
		ast.f64_type {
			table_typ = 'DOUBLE'
		}
		ast.string_type {
			table_typ = 'TEXT'
		}
		-1 {
			table_typ = 'SERIAL'
		}
		else {}
	}
	return table_typ
}

fn (mut g Gen) mysql_buffer_typ_from_typ(typ ast.Type) string {
	mut buf_typ := ''
	match typ {
		ast.i8_type, ast.byte_type, ast.bool_type {
			buf_typ = 'MYSQL_TYPE_TINY'
		}
		ast.i16_type, ast.u16_type {
			buf_typ = 'MYSQL_TYPE_SHORT'
		}
		ast.int_type, ast.u32_type {
			buf_typ = 'MYSQL_TYPE_LONG'
		}
		ast.i64_type, ast.u64_type {
			buf_typ = 'MYSQL_TYPE_LONGLONG'
		}
		ast.f32_type {
			buf_typ = 'MYSQL_TYPE_FLOAT'
		}
		ast.f64_type {
			buf_typ = 'MYSQL_TYPE_DOUBLE'
		}
		ast.string_type {
			buf_typ = 'MYSQL_TYPE_STRING'
		}
		else {
			buf_typ = 'MYSQL_TYPE_NULL'
		}
	}
	return buf_typ
}

fn (mut g Gen) mysql_buffer_typ_from_field(field ast.StructField) (string, string) {
	mut typ := g.get_sql_field_type(field)
	mut sym := g.table.get_type_symbol(typ).cname
	buf_typ := g.mysql_buffer_typ_from_typ(typ)

	if typ == ast.string_type {
		sym = 'char'
	}

	return buf_typ, sym
}

// psql

fn (mut g Gen) psql_stmt(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.sql_i = 0
	g.sql_idents = []string{}
	g.writeln('\n\t//psql insert')
	db_name := g.new_tmp_var()
	g.sql_stmt_name = g.new_tmp_var()
	g.write('pg__DB $db_name = ')
	g.expr(db_expr)
	g.writeln(';')
	g.write('string $g.sql_stmt_name = _SLIT("')
	g.sql_defaults(node, typ)
	g.writeln(';')

	mut arr_stmt := []ast.SqlStmtLine{}
	mut arr_fkeys := []string{}
	mut arr_field_name := []string{}
	if node.kind == .insert {
		for i, field in node.fields {
			if g.get_sql_field_type(field) == ast.Type(-1) {
				continue
			}
			g.sql_i = i
			field_type := g.get_sql_field_type(field)
			if field.name == g.sql_fkey && g.sql_fkey != '' {
				g.sql_buf = strings.new_builder(100)
				g.sql_bind(g.sql_parent_id, '', field_type, typ)
				g.writeln(g.sql_buf.str())
				continue
			}
			g.writeln('//$field.name ($field.typ)')
			x := '${node.object_var_name}.$field.name'
			if g.table.get_type_symbol(field.typ).kind == .struct_ {
				// insert again
				expr := node.sub_structs[int(field.typ)]
				tmp_sql_stmt_name := g.sql_stmt_name
				tmp_sql_table_name := g.sql_table_name
				g.sql_stmt_line(expr, db_expr)
				g.sql_stmt_name = tmp_sql_stmt_name
				g.sql_table_name = tmp_sql_table_name

				res := g.new_tmp_var()
				g.writeln('Option_pg__Row $res = pg__DB_exec_one($db_name, _SLIT("SELECT LASTVAL();"));')

				tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, $si_s_code ,{.d_s=IError_str(err)}}}))'
				g.writeln('if (${res}.state != 0) { IError err = ${res}.err; eprintln($tmp_str); }')

				g.sql_buf = strings.new_builder(100)
				g.sql_bind('string_int((*(string*)array_get((*(pg__Row*)${res}.data).vals, 0)))',
					'', ast.int_type, typ)
				g.writeln(g.sql_buf.str())
			} else if g.table.get_type_symbol(field.typ).kind == .array {
				t := g.table.get_type_symbol(field.typ).array_info().elem_type
				if g.table.get_type_symbol(t).kind == .struct_ {
					mut fkey := ''
					for attr in field.attrs {
						if attr.name == 'fkey' && attr.arg != '' && attr.kind == .string {
							fkey = attr.arg
							break
						}
					}
					if fkey == '' {
						verror('fkey attribute has to be set for arrays in orm')
						continue
					}

					arr_stmt << node.sub_structs[int(t)]
					arr_fkeys << fkey
					arr_field_name << field.name
				}
			} else {
				g.sql_buf = strings.new_builder(100)
				g.sql_bind(x, '', field_type, typ)
				g.writeln(g.sql_buf.str())
			}
		}
	}
	binds := g.sql_buf.str()
	g.sql_buf = strings.new_builder(100)
	g.writeln(binds)

	g.writeln('pg__DB_exec($db_name, $g.sql_stmt_name);')

	if arr_stmt.len > 0 {
		res := g.new_tmp_var()
		g.writeln('Option_pg__Row $res = pg__DB_exec_one($db_name, _SLIT("SELECT LASTVAL();"));')

		tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, $si_s_code ,{.d_s=IError_str(err)}}}))'
		g.writeln('if (${res}.state != 0) { IError err = ${res}.err; eprintln($tmp_str); }')

		id_name := g.new_tmp_var()
		g.writeln('int $id_name = string_int((*(string*)array_get((*(pg__Row*)${res}.data).vals, 0)));')
		g.sql_arr_stmt(arr_stmt, arr_fkeys, arr_field_name, id_name, db_expr)
	}
}

fn (mut g Gen) psql_select_expr(node ast.SqlExpr, sub bool, line string, typ SqlType) {
	g.sql_i = 0
	mut cur_line := line
	if !sub {
		cur_line = g.go_before_stmt(0)
	}
	g.sql_stmt_name = g.new_tmp_var()
	db_name := g.new_tmp_var()
	g.writeln('\n\t// psql select')
	g.write('pg__DB $db_name = ')
	g.expr(node.db_expr)
	g.writeln(';')

	g.write('string $g.sql_stmt_name = _SLIT("')
	g.write(g.get_base_sql_select_query(node, typ))
	g.sql_expr_defaults(node, typ)
	g.writeln('");')

	buf := g.sql_buf.str()
	g.sql_buf = strings.new_builder(100)
	g.writeln(buf)

	res := g.new_tmp_var()
	g.writeln('Option_Array_pg__Row $res = pg__DB_exec($db_name, $g.sql_stmt_name);')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${res}.state != 0) { IError err = ${res}.err; eprintln($tmp_str); }')

	rows := g.new_tmp_var()

	g.writeln('Array_pg__Row $rows = *(Array_pg__Row*) ${res}.data;')

	if node.is_count {
		g.writeln('$cur_line string_int((*(string*)array_get(array_get($rows, 0).vals), 0)));')
	} else {
		tmp := g.new_tmp_var()
		styp := g.typ(node.typ)
		tmp_i := g.new_tmp_var()
		mut elem_type_str := ''
		g.writeln('int $tmp_i = 0;')
		if node.is_array {
			array_sym := g.table.get_type_symbol(node.typ)
			array_info := array_sym.info as ast.Array
			elem_type_str = g.typ(array_info.elem_type)
			g.writeln('$styp ${tmp}_array = __new_array(0, 10, sizeof($elem_type_str));')
			g.writeln('for ($tmp_i = 0; $tmp_i < ${rows}.len; $tmp_i++) {')
			g.writeln('\t$elem_type_str $tmp = ($elem_type_str) {')
			//
			sym := g.table.get_type_symbol(array_info.elem_type)
			info := sym.info as ast.Struct
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		} else {
			g.writeln('$styp $tmp = ($styp){')
			sym := g.table.get_type_symbol(node.typ)
			info := sym.info as ast.Struct
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		}
		fields := g.new_tmp_var()
		g.writeln('if (${rows}.len > 0) {')
		g.writeln('Array_string $fields = (*(pg__Row*) array_get($rows, $tmp_i)).vals;')
		fld := g.new_tmp_var()
		g.writeln('string $fld;')
		mut primary := ''
		for i, field in node.fields {
			for attr in field.attrs {
				if attr.name == 'primary' {
					primary = '${tmp}.$field.name'
					break
				}
			}
			if g.table.get_type_symbol(field.typ).kind == .array {
				g.sql_select_arr(field, node, primary, tmp)
				continue
			}
			g.writeln('$fld = (*(string*)array_get($fields, $i));')
			name := g.table.get_type_symbol(field.typ).cname

			if g.table.get_type_symbol(field.typ).kind == .struct_ {
				g.writeln('//parse struct start')

				mut expr := node.sub_structs[int(field.typ)]
				mut where_expr := expr.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident

				ident.name = '$fld'
				where_expr.right = ident
				expr.where_expr = where_expr

				tmp_sql_i := g.sql_i
				tmp_sql_stmt_name := g.sql_stmt_name
				tmp_sql_buf := g.sql_buf
				tmp_sql_table_name := g.sql_table_name

				g.sql_select_expr(expr, true, '\t${tmp}.$field.name =')
				g.writeln('//parse struct end')
				g.sql_stmt_name = tmp_sql_stmt_name
				g.sql_buf = tmp_sql_buf
				g.sql_i = tmp_sql_i
				g.sql_table_name = tmp_sql_table_name
			} else if field.typ == ast.string_type {
				g.writeln('${tmp}.$field.name = $fld;')
			} else if field.typ == ast.byte_type {
				g.writeln('${tmp}.$field.name = (byte) string_${name}($fld);')
			} else if field.typ == ast.i8_type {
				g.writeln('${tmp}.$field.name = (i8) string_${name}($fld);')
			} else if field.typ == ast.bool_type {
				g.writeln('${tmp}.$field.name = string__eq($fld, _SLIT("0")) ? false : true;')
			} else {
				g.writeln('${tmp}.$field.name = string_${name}($fld);')
			}
		}
		if node.is_array {
			g.writeln('\t array_push((array*)&${tmp}_array, _MOV(($elem_type_str[]) { $tmp }));\n')
			g.writeln('}')
		}
		g.writeln('}')
		g.writeln('string_free(&$g.sql_stmt_name);')
		if node.is_array {
			g.writeln('$cur_line ${tmp}_array; ')
		} else {
			g.writeln('$cur_line $tmp; ')
		}
	}
}

fn (mut g Gen) psql_create_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.writeln('// psql table creator')
	create_string := g.table_gen(node, typ, db_expr)
	tmp := g.new_tmp_var()
	g.write('Option_Array_pg__Row $tmp = pg__DB_exec(')
	g.expr(db_expr)
	g.writeln(', _SLIT("$create_string"));')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${tmp}.state != 0) { IError err = ${tmp}.err; eprintln($tmp_str); }')
}

fn (mut g Gen) psql_drop_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	table_name := g.get_table_name(node.table_expr)
	g.writeln('// psql table drop')
	lit := '\\"'
	drop_string := 'DROP TABLE $lit$table_name$lit;'
	tmp := g.new_tmp_var()
	g.write('Option_Array_pg__Row $tmp = pg__DB_exec(')
	g.expr(db_expr)
	g.writeln(', _SLIT("$drop_string"));')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${tmp}.state != 0) { IError err = ${tmp}.err; eprintln($tmp_str); }')
}

fn (mut g Gen) psql_get_table_type(typ ast.Type) string {
	mut table_typ := ''
	match typ {
		ast.i8_type, ast.byte_type, ast.bool_type {
			table_typ = 'CHAR(1)'
		}
		ast.i16_type, ast.u16_type {
			table_typ = 'SMALLINT'
		}
		ast.int_type, ast.u32_type {
			table_typ = 'INT'
		}
		ast.i64_type, ast.u64_type {
			table_typ = 'BIGINT'
		}
		ast.f32_type {
			table_typ = 'FLOAT4'
		}
		ast.f64_type {
			table_typ = 'FLOAT8'
		}
		ast.string_type {
			table_typ = 'TEXT'
		}
		-1 {
			table_typ = 'SERIAL'
		}
		else {}
	}
	return table_typ
}

fn (mut g Gen) psql_bind(val string, typ ast.Type) {
	mut sym := g.table.get_type_symbol(typ).cname
	g.sql_buf.write_string('$g.sql_stmt_name = string_replace($g.sql_stmt_name, _SLIT("\$$g.sql_i"), ')
	if sym != 'string' {
		mut num := false
		if sym != 'bool' {
			num = true
			g.sql_buf.write_string('${sym}_str(')
		}
		g.sql_buf.write_string('(($sym) $val)')

		if sym == 'bool' {
			g.sql_buf.write_string('? _SLIT("1") : _SLIT("0")')
		}

		if num {
			g.sql_buf.write_string(')')
		}
	} else {
		g.sql_buf.write_string('string__plus(_SLIT("\'"), string__plus(((string) $val), _SLIT("\'")))')
	}
	g.sql_buf.writeln(');')
}

// mssql

fn (mut g Gen) mssql_create_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	g.writeln('// mssql table creator')
	create_string := g.table_gen(node, typ, db_expr)
	tmp := g.new_tmp_var()
	g.write('Option_mssql__Result $tmp = mssql__Connection_query(&')
	g.expr(db_expr)
	g.writeln(', _SLIT("$create_string"));')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${tmp}.state != 0) { IError err = ${tmp}.err; eprintln($tmp_str); }')
}

fn (mut g Gen) mssql_drop_table(node ast.SqlStmtLine, typ SqlType, db_expr ast.Expr) {
	table_name := g.get_table_name(node.table_expr)
	g.writeln('// mssql table drop')
	lit := '\\"'
	drop_string := 'DROP TABLE $lit$table_name$lit;'
	tmp := g.new_tmp_var()
	g.write('Option_mssql__Result $tmp = mssql__Connection_query(&')
	g.expr(db_expr)
	g.writeln(', _SLIT("$drop_string"));')

	tmp_str := 'str_intp(1, _MOV((StrIntpData[]){{_SLIT("Something went wrong: "), $si_s_code ,{.d_s=IError_str(err)}}}))'
	g.writeln('if (${tmp}.state != 0) { IError err = ${tmp}.err; eprintln($tmp_str); }')
}

fn (mut g Gen) mssql_get_table_type(typ ast.Type) string {
	mut table_typ := ''
	match typ {
		ast.i8_type, ast.byte_type, ast.bool_type {
			table_typ = 'TINYINT'
		}
		ast.i16_type, ast.u16_type {
			table_typ = 'SMALLINT'
		}
		ast.int_type, ast.u32_type {
			table_typ = 'INT'
		}
		ast.i64_type, ast.u64_type {
			table_typ = 'BIGINT'
		}
		ast.f32_type {
			table_typ = 'FLOAT(24)'
		}
		ast.f64_type {
			table_typ = 'FLOAT(53)'
		}
		ast.string_type {
			table_typ = 'TEXT'
		}
		-1 {
			table_typ = 'INT IDENTITY'
		}
		else {}
	}
	return table_typ
}

// utils

fn (mut g Gen) sql_select_arr(field ast.StructField, node ast.SqlExpr, primary string, tmp string) {
	t := g.table.get_type_symbol(field.typ).array_info().elem_type
	if g.table.get_type_symbol(t).kind == .struct_ {
		mut fkey := ''
		for attr in field.attrs {
			if attr.name == 'fkey' && attr.arg != '' && attr.kind == .string {
				fkey = attr.arg
				break
			}
		}
		if fkey == '' {
			verror('fkey attribute has to be set for arrays in orm')
			return
		}
		g.writeln('//parse array start')

		e := node.sub_structs[int(t)]
		mut where_expr := e.where_expr as ast.InfixExpr
		mut lidt := where_expr.left as ast.Ident
		mut ridt := where_expr.right as ast.Ident
		ridt.name = primary
		lidt.name = fkey
		where_expr.right = ridt
		where_expr.left = lidt
		expr := ast.SqlExpr{
			typ: field.typ
			has_where: e.has_where
			db_expr: e.db_expr
			is_array: true
			pos: e.pos
			where_expr: where_expr
			table_expr: e.table_expr
			fields: e.fields
			sub_structs: e.sub_structs
		}
		tmp_sql_i := g.sql_i
		tmp_sql_stmt_name := g.sql_stmt_name
		tmp_sql_buf := g.sql_buf
		tmp_sql_table_name := g.sql_table_name

		g.sql_select_expr(expr, true, '\t${tmp}.$field.name =')
		g.writeln('//parse array end')

		g.sql_stmt_name = tmp_sql_stmt_name
		g.sql_buf = tmp_sql_buf
		g.sql_i = tmp_sql_i
		g.sql_table_name = tmp_sql_table_name
	}
}

fn (mut g Gen) sql_arr_stmt(arr_stmt []ast.SqlStmtLine, arr_fkeys []string, arr_field_name []string, id_name string, db_expr ast.Expr) {
	for i, s in arr_stmt {
		cnt := g.new_tmp_var()
		g.writeln('for (int $cnt = 0; $cnt < ${s.object_var_name}.${arr_field_name[i]}.len; $cnt++) {')
		name := g.table.get_type_symbol(s.table_expr.typ).cname
		tmp_var := g.new_tmp_var()
		g.writeln('\t$name $tmp_var = (*($name*)array_get(${s.object_var_name}.${arr_field_name[i]}, $cnt));')

		mut sub_structs := map[int]ast.SqlStmtLine{}

		for key, sub in s.sub_structs {
			sub_structs[key] = ast.SqlStmtLine{
				pos: sub.pos
				kind: sub.kind
				table_expr: sub.table_expr
				object_var_name: tmp_var
				fields: sub.fields
				sub_structs: sub.sub_structs
			}
		}
		stmt := ast.SqlStmtLine{
			pos: s.pos
			kind: s.kind
			table_expr: s.table_expr
			object_var_name: tmp_var
			fields: s.fields
			sub_structs: sub_structs
		}
		tmp_fkey := g.sql_fkey
		tmp_parent_id := g.sql_parent_id
		g.sql_fkey = arr_fkeys[i]
		g.sql_parent_id = id_name

		g.sql_stmt_line(stmt, db_expr)

		g.sql_fkey = tmp_fkey
		g.sql_parent_id = tmp_parent_id

		g.writeln('}')
	}
}

fn (mut g Gen) sql_expr_defaults(node ast.SqlExpr, sql_typ SqlType) {
	if node.has_where && node.where_expr is ast.InfixExpr {
		g.expr_to_sql(node.where_expr, sql_typ)
	}
	if node.has_order {
		g.write(' ORDER BY ')
		g.sql_side = .left
		g.expr_to_sql(node.order_expr, sql_typ)
		if node.has_desc {
			g.write(' DESC ')
		}
	} else {
		g.write(' ORDER BY id ')
	}
	if node.has_limit {
		g.write(' LIMIT ')
		g.sql_side = .right
		g.expr_to_sql(node.limit_expr, sql_typ)
	}
	if node.has_offset {
		g.write(' OFFSET ')
		g.sql_side = .right
		g.expr_to_sql(node.offset_expr, sql_typ)
	}
}

fn (mut g Gen) get_base_sql_select_query(node ast.SqlExpr, typ SqlType) string {
	mut lit := '`'
	if typ == .psql {
		lit = '\\"'
	}
	mut sql_query := 'SELECT '
	table_name := g.get_table_name(node.table_expr)
	if node.is_count {
		// `select count(*) from User`
		sql_query += 'COUNT(*) FROM $lit$table_name$lit '
	} else {
		// `select id, name, country from User`
		fields := node.fields.filter(g.table.get_type_symbol(it.typ).kind != .array)
		for i, field in fields {
			sql_query += '$lit${g.get_field_name(field)}$lit'
			if i < fields.len - 1 {
				sql_query += ', '
			}
		}
		sql_query += ' FROM $lit$table_name$lit'
	}
	if node.has_where {
		sql_query += ' WHERE '
	}
	return sql_query
}

fn (mut g Gen) sql_defaults(node ast.SqlStmtLine, typ SqlType) {
	table_name := g.get_table_name(node.table_expr)
	mut lit := '`'
	if typ == .psql {
		lit = '\\"'
	}
	if node.kind == .insert {
		g.write('INSERT INTO $lit$table_name$lit (')
	} else if node.kind == .update {
		g.write('UPDATE $lit$table_name$lit SET ')
	} else if node.kind == .delete {
		g.write('DELETE FROM $lit$table_name$lit ')
	}
	if node.kind == .insert {
		fields := node.fields.filter(g.table.get_type_symbol(it.typ).kind != .array)
		for i, field in fields {
			if g.get_sql_field_type(field) == ast.Type(-1) {
				continue
			}
			g.write('$lit${g.get_field_name(field)}$lit')
			if i < fields.len - 1 {
				g.write(', ')
			}
		}
		g.write(') VALUES (')
		for i, field in fields {
			if g.get_sql_field_type(field) == ast.Type(-1) {
				continue
			}
			g.inc_sql_i(typ)
			if i < fields.len - 1 {
				g.write(', ')
			}
		}
		g.write(')')
	} else if node.kind == .update {
		for i, col in node.updated_columns {
			g.write(' ${g.get_field_name(g.get_struct_field(col))} = ')
			g.expr_to_sql(node.update_exprs[i], typ)
			if i < node.updated_columns.len - 1 {
				g.write(', ')
			}
		}
		g.write(' WHERE ')
	} else if node.kind == .delete {
		g.write(' WHERE ')
	}
	if node.kind == .update || node.kind == .delete {
		g.expr_to_sql(node.where_expr, typ)
	}
	g.write(';")')
}

fn (mut g Gen) table_gen(node ast.SqlStmtLine, typ SqlType, expr ast.Expr) string {
	typ_sym := g.table.get_type_symbol(node.table_expr.typ)
	struct_data := typ_sym.struct_info()
	table_name := g.get_table_name(node.table_expr)
	mut lit := '`'
	if typ == .psql || typ == .mssql {
		lit = '\\"'
	}

	mut create_string := 'CREATE TABLE IF NOT EXISTS $lit$table_name$lit ('
	if typ == .mssql {
		// mssql detecting create if not exist is awkward
		create_string = 'IF NOT EXISTS (SELECT * FROM sysobjects WHERE name=\'$table_name\' and xtype=\'U\') CREATE TABLE $lit$table_name$lit ('
	}

	mut fields := []string{}
	mut unique_fields := []string{}

	mut primary := '' // for mysql
	mut unique := map[string][]string{}

	for field in struct_data.fields {
		name := g.get_field_name(field)
		mut is_primary := false
		mut no_null := false
		mut is_unique := false
		mut is_skip := false
		mut unique_len := 0
		mut fkey := ''
		for attr in field.attrs {
			match attr.name {
				'primary' {
					is_primary = true
					primary = name
				}
				'unique' {
					if attr.arg != '' {
						if attr.kind == .string {
							unique[attr.arg] << name
							continue
						} else if attr.kind == .number {
							unique_len = attr.arg.int()
							is_unique = true
							continue
						}
					}
					is_unique = true
				}
				'nonull' {
					no_null = true
				}
				'skip' {
					is_skip = true
				}
				'fkey' {
					if attr.arg != '' {
						if attr.kind == .string {
							fkey = attr.arg
							continue
						}
					}
				}
				else {}
			}
		}
		if is_skip {
			continue
		}
		mut stmt := ''
		mut converted_typ := g.sql_type_from_v(typ, g.get_sql_field_type(field))
		if converted_typ == '' {
			if g.table.get_type_symbol(field.typ).kind == .struct_ {
				converted_typ = g.sql_type_from_v(typ, ast.int_type)
				g.sql_create_table(ast.SqlStmtLine{
					kind: node.kind
					pos: node.pos
					table_expr: ast.TypeNode{
						typ: field.typ
						pos: node.table_expr.pos
					}
				}, expr)
			} else if g.table.get_type_symbol(field.typ).kind == .array {
				arr_info := g.table.get_type_symbol(field.typ).array_info()
				if arr_info.nr_dims > 1 {
					verror('array with one dim are supported in orm')
					continue
				}
				atyp := arr_info.elem_type
				if g.table.get_type_symbol(atyp).kind == .struct_ {
					if fkey == '' {
						verror('array field ($field.name) needs a fkey')
						continue
					}
					g.sql_create_table(ast.SqlStmtLine{
						kind: node.kind
						pos: node.pos
						table_expr: ast.TypeNode{
							typ: atyp
							pos: node.table_expr.pos
						}
					}, expr)
				} else {
					verror('unknown type ($field.typ) for field $field.name in struct $table_name')
				}
				continue
			} else {
				verror('unknown type ($field.typ) for field $field.name in struct $table_name')
				continue
			}
		}
		stmt = '$lit$name$lit $converted_typ'

		if field.has_default_expr && typ != .mysql {
			stmt += ' DEFAULT '
			stmt += field.default_expr.str()
		}
		if no_null {
			stmt += ' NOT NULL'
		}
		if is_unique {
			if typ == .mysql {
				mut f := 'UNIQUE KEY($lit$name$lit'
				if converted_typ == 'TEXT' {
					if unique_len > 0 {
						f += '($unique_len)'
					} else {
						f += '($c.default_unique_str_len)'
					}
				}
				f += ')'
				unique_fields << f
			} else {
				stmt += ' UNIQUE'
			}
		}
		if is_primary && typ == .sqlite3 {
			stmt += ' PRIMARY KEY'
		}
		fields << stmt
	}
	if unique.len > 0 {
		for k, v in unique {
			mut tmp := []string{}
			for f in v {
				tmp << '$lit$f$lit'
			}
			fields << '/* $k */UNIQUE(${tmp.join(', ')})'
		}
	}
	if typ == .mysql || typ == .psql {
		fields << 'PRIMARY KEY($lit$primary$lit)'
	}
	fields << unique_fields
	create_string += fields.join(', ')
	create_string += ');'
	return create_string
}

fn (mut g Gen) expr_to_sql(expr ast.Expr, typ SqlType) {
	// Custom handling for infix exprs (since we need e.g. `and` instead of `&&` in SQL queries),
	// strings. Everything else (like numbers, a.b) is handled by g.expr()
	//
	// TODO `where id = some_column + 1` needs literal generation of `some_column` as a string,
	// not a V variable. Need to distinguish column names from V variables.
	match expr {
		ast.InfixExpr {
			g.sql_side = .left
			g.expr_to_sql(expr.left, typ)
			match expr.op {
				.ne { g.write(' != ') }
				.eq { g.write(' = ') }
				.gt { g.write(' > ') }
				.lt { g.write(' < ') }
				.ge { g.write(' >= ') }
				.le { g.write(' <= ') }
				.and { g.write(' and ') }
				.logical_or { g.write(' or ') }
				.plus { g.write(' + ') }
				.minus { g.write(' - ') }
				.mul { g.write(' * ') }
				.div { g.write(' / ') }
				else {}
			}
			g.sql_side = .right
			g.expr_to_sql(expr.right, typ)
		}
		ast.StringLiteral {
			// g.write("'$it.val'")
			g.inc_sql_i(typ)
			g.sql_bind('"$expr.val"', expr.val.len.str(), g.sql_get_real_type(ast.string_type),
				typ)
		}
		ast.IntegerLiteral {
			g.inc_sql_i(typ)
			g.sql_bind(expr.val, '', g.sql_get_real_type(ast.int_type), typ)
		}
		ast.BoolLiteral {
			// true/false literals were added to Sqlite 3.23 (2018-04-02)
			// but lots of apps/distros use older sqlite (e.g. Ubuntu 18.04 LTS )
			g.inc_sql_i(typ)
			eval := if expr.val { '1' } else { '0' }
			g.sql_bind(eval, '', g.sql_get_real_type(ast.byte_type), typ)
		}
		ast.Ident {
			// `name == user_name` => `name == ?1`
			// for left sides just add a string, for right sides, generate the bindings
			if g.sql_side == .left {
				// println("sql gen left $expr.name")
				g.sql_left_type = g.get_struct_field_typ(expr.name)
				g.write(g.get_field_name(g.get_struct_field(expr.name)))
			} else {
				g.inc_sql_i(typ)
				info := expr.info as ast.IdentVar
				ityp := info.typ
				if typ == .sqlite3 {
					if ityp == ast.string_type {
						g.sql_bind('${expr.name}.str', '${expr.name}.len', g.sql_get_real_type(ityp),
							typ)
					} else {
						g.sql_bind(expr.name, '', g.sql_get_real_type(ityp), typ)
					}
				} else {
					g.sql_bind('$g.sql_i.str()', '', g.sql_get_real_type(ityp), typ)
					g.sql_idents << expr.name
					g.sql_idents_types << g.sql_get_real_type(ityp)
				}
			}
		}
		ast.SelectorExpr {
			g.inc_sql_i(typ)
			if expr.expr !is ast.Ident {
				verror('orm selector not ident')
			}
			ident := expr.expr as ast.Ident
			g.sql_bind(ident.name + '.' + expr.field_name, '', g.sql_get_real_type(expr.typ),
				typ)
		}
		else {
			g.expr(expr)
		}
	}
	/*
	ast.Ident {
			g.write('$it.name')
		}
		else {}
	*/
}

fn (mut g Gen) get_struct_field_typ(f string) ast.Type {
	sym := g.table.get_type_symbol(g.table.type_idxs[g.sql_table_name])

	mut typ := ast.Type(-1)

	if sym.kind != .struct_ {
		str := sym.info as ast.Struct
		for field in str.fields {
			if field.name != f {
				continue
			}
			typ = g.get_sql_field_type(field)
			break
		}
	}

	return typ
}

fn (mut g Gen) sql_get_real_type(typ ast.Type) ast.Type {
	if typ != g.sql_left_type && g.sql_left_type >= 0 {
		return g.sql_left_type
	}
	return typ
}

fn (mut g Gen) inc_sql_i(typ SqlType) {
	g.sql_i++
	if typ == .psql {
		g.write('$')
	} else {
		g.write('?') // used in sqlite `?i` and mysql `?`
	}
	if typ != .mysql {
		g.write('$g.sql_i')
	}
}

fn (mut g Gen) parse_db_type(expr ast.Expr) SqlType {
	match expr {
		ast.Ident {
			if expr.info is ast.IdentVar {
				return g.parse_db_from_type_string(g.table.get_type_name(expr.info.typ))
			}
		}
		ast.SelectorExpr {
			return g.parse_db_from_type_string(g.table.get_type_name(expr.typ))
		}
		else {
			return .unknown
		}
	}
	return .unknown
}

fn (mut g Gen) parse_db_from_type_string(name string) SqlType {
	match name {
		'sqlite.DB' {
			return .sqlite3
		}
		'mysql.Connection' {
			return .mysql
		}
		'pg.DB' {
			return .psql
		}
		'mssql.Connection' {
			return .mssql
		}
		else {
			return .unknown
		}
	}
}

fn (mut g Gen) get_sql_field_type(field ast.StructField) ast.Type {
	mut typ := field.typ
	for attr in field.attrs {
		if attr.kind == .plain && attr.name == 'sql' && attr.arg != '' {
			if attr.arg.to_lower() == 'serial' {
				typ = ast.Type(-1)
				break
			}
			typ = g.table.type_idxs[attr.arg]
		}
	}
	return typ
}

fn (mut g Gen) get_table_name(table_expr ast.TypeNode) string {
	info := g.table.get_type_symbol(table_expr.typ).struct_info()
	mut tablename := util.strip_mod_name(g.table.get_type_symbol(table_expr.typ).name)
	for attr in info.attrs {
		if attr.kind == .string && attr.name == 'table' && attr.arg != '' {
			tablename = attr.arg
			break
		}
	}
	return tablename
}

fn (mut g Gen) get_struct_field(name string) ast.StructField {
	info := g.table.get_type_symbol(g.table.type_idxs[g.sql_table_name]).struct_info()
	mut f := ast.StructField{}
	for field in info.fields {
		if field.name == name {
			f = field
		}
	}
	return f
}

fn (mut g Gen) get_field_name(field ast.StructField) string {
	mut name := field.name
	for attr in field.attrs {
		if attr.kind == .string && attr.name == 'sql' && attr.arg != '' {
			name = attr.arg
			break
		}
	}
	return name
}
