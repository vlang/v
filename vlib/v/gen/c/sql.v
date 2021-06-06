// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import strings
import v.util

enum SqlExprSide {
	left
	right
}

enum SqlType {
	sqlite3
	mysql
	psql
	unknown
}

fn (mut g Gen) sql_stmt(node ast.SqlStmt) {
	conn := g.new_tmp_var()
	g.writeln('')
	g.writeln('// orm')
	g.write('orm__OrmConnection $conn = (orm__OrmConnection){._')
	mut fn_prefix := ''
	typ := g.parse_db_type(node.db_expr)
	match typ {
		.sqlite3 {
			fn_prefix = 'sqlite__DB'
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}
	g.write('$fn_prefix = &')
	g.expr(node.db_expr)
	g.writeln(', ._typ = _orm__OrmConnection_${fn_prefix}_index};')
	for line in node.lines {
		g.sql_stmt_line(line, conn)
	}
}

fn (mut g Gen) sql_stmt_line(nd ast.SqlStmtLine, expr string) {
	mut node := nd
	table_name := g.get_table_name(node.table_expr)
	g.sql_table_name = g.table.get_type_symbol(node.table_expr.typ).name
	res := g.new_tmp_var()
	mut subs := false
	mut dcheck := false

	if node.kind != .create {
		mut fields := []ast.StructField{}
		for f in node.fields {
			mut skip := false
			mut primary := false
			for attr in f.attrs {
				if attr.name == 'primary' {
					primary = true
				}
				if attr.name == 'skip' {
					skip = true
				}
			}
			if !skip && !primary {
				fields << f
			}
		}
		node.fields = fields.clone()
		unsafe { fields.free() }
	}

	if node.kind == .create {
		g.write('Option_void $res = orm__OrmConnection_name_table[${expr}._typ]._method_')
		g.sql_create_table(node, expr, table_name)
		subs = true
	} else if node.kind == .drop {
		g.write('Option_void $res = orm__OrmConnection_name_table[${expr}._typ]._method_')
		g.writeln('drop(${expr}._object, _SLIT("$table_name"));')
		subs = true
	} else if node.kind == .insert {
		arr := g.new_tmp_var()
		g.writeln('Array_orm__Primitive $arr = new_array_from_c_array(0, 0, sizeof(orm__Primitive), _MOV((orm__Primitive[0]){}));')
		g.sql_insert(node, expr, table_name, arr, res, '', false, '')
		dcheck = true
	} else if node.kind == .update {
		g.write('Option_void $res = orm__OrmConnection_name_table[${expr}._typ]._method_')
		g.sql_update(node, expr, table_name)
	} else if node.kind == .delete {
		g.write('Option_void $res = orm__OrmConnection_name_table[${expr}._typ]._method_')
		g.sql_delete(node, expr, table_name)
	}
	if !dcheck {
		g.writeln('if (${res}.state != 0 && ${res}.err._typ != _IError_None___index) { v_panic(IError_str(${res}.err)); }')
	}
	if subs {
		for _, sub in node.sub_structs {
			g.sql_stmt_line(sub, expr)
		}
	}
}

fn (mut g Gen) sql_create_table(node ast.SqlStmtLine, expr string, table_name string) {
	g.write('create(${expr}._object, _SLIT("$table_name"), new_array_from_c_array($node.fields.len, $node.fields.len, sizeof(orm__OrmTableField), _MOV((orm__OrmTableField[$node.fields.len]){')
	for field in node.fields {
		sym := g.table.get_type_symbol(field.typ)
		g.write('(orm__OrmTableField){')
		g.write('.name = _SLIT("$field.name"),')
		g.write('.typ = ${int(field.typ)},')
		g.write('.is_arr = ${sym.kind == .array}, ')
		g.write('.is_time = ${int(g.table.get_type_name(field.typ) == 'time__Time')},')
		g.write('.default_val = (string){.str = (byteptr) "$field.default_val", .is_lit = 1},')
		g.write('.attrs = new_array_from_c_array($field.attrs.len, $field.attrs.len, sizeof(StructAttribute), _MOV((StructAttribute[$field.attrs.len]){')
		for attr in field.attrs {
			g.write('(StructAttribute){')
			g.write('.name = _SLIT("$attr.name"),')
			g.write('.has_arg = ${int(attr.has_arg)},')
			g.write('.arg = _SLIT("$attr.arg"),')
			g.write('.kind = ${int(attr.kind)},')
			g.write('},')
		}
		g.write('}))')
		g.write('},')
	}
	g.writeln('})));')
}

fn (mut g Gen) sql_insert(node ast.SqlStmtLine, expr string, table_name string, last_ids_arr string, res string, pid string, is_array bool, fkey string) {
	mut subs := []ast.SqlStmtLine{}
	mut arrs := []ast.SqlStmtLine{}
	mut fkeys := []string{}
	mut field_names := []string{}

	for f in node.fields {
		sym := g.table.get_type_symbol(f.typ)
		if sym.kind == .struct_ {
			subs << node.sub_structs[int(f.typ)]
		} else if sym.kind == .array {
			mut f_key := ''
			for attr in f.attrs {
				if attr.name == 'fkey' && attr.has_arg && attr.kind == .string {
					f_key = attr.arg
				}
			}
			if f_key == '' {
				verror('An field which holds an array, needs a fkey defined')
			}
			fkeys << f_key
			info := sym.array_info()
			if info.nr_dims == 1 {
				arrs << node.sub_structs[int(info.elem_type)]
				field_names << f.name
			} else {
				verror('V ORM only supports 1 dimensional arrays')
			}
		}
	}

	fields := node.fields.filter(g.table.get_type_symbol(it.typ).kind != .array)

	for sub in subs {
		g.sql_stmt_line(sub, expr)
		g.writeln('array_push(&$last_ids_arr, _MOV((orm__Primitive[]){orm__OrmConnection_name_table[${expr}._typ]._method_last_id(${expr}._object)}));')
	}

	g.write('Option_void $res = orm__OrmConnection_name_table[${expr}._typ]._method_')
	g.write('insert(${expr}._object, _SLIT("$table_name"), (orm__OrmQueryData){')

	g.write('.fields = new_array_from_c_array($fields.len, $fields.len, sizeof(string), _MOV((string[$fields.len]){')
	for f in fields {
		g.write('_SLIT("${g.get_field_name(f)}"),')
	}
	g.write('})),')

	g.write('.data = new_array_from_c_array($fields.len, $fields.len, sizeof(orm__Primitive), _MOV((orm__Primitive[$fields.len]){')
	mut structs := 0
	for f in fields {
		if f.name == fkey {
			g.write('$pid, ')
			continue
		}
		mut sym := g.table.get_type_symbol(f.typ)
		if sym.kind == .struct_ {
			g.write('(*(orm__Primitive*) array_get($last_ids_arr, $structs)),')
			structs++
			continue
		}
		mut typ := sym.cname
		if typ == 'time__Time' {
			typ = 'time'
		}
		g.write('orm__${typ}_to_primitive(${node.object_var_name}.$f.name),')
	}
	g.write('})),')
	g.write('.types = new_array_from_c_array(0, 0, sizeof(int), _MOV((int[0]){})),')
	g.write('.kinds = new_array_from_c_array(0, 0, sizeof(orm__OperationKind), _MOV((orm__OperationKind[0]){})),')
	g.writeln('});')

	g.writeln('if (${res}.state != 0 && ${res}.err._typ != _IError_None___index) { v_panic(IError_str(${res}.err)); }')
	if arrs.len > 0 {
		mut id_name := g.new_tmp_var()
		g.writeln('orm__Primitive $id_name = orm__OrmConnection_name_table[${expr}._typ]._method_last_id(${expr}._object);')
		for i, mut arr in arrs {
			idx := g.new_tmp_var()
			g.writeln('for (int $idx = 0; $idx < ${arr.object_var_name}.${field_names[i]}.len; $idx++) {')
			last_ids := g.new_tmp_var()
			res_ := g.new_tmp_var()
			tmp_var := g.new_tmp_var()
			ctyp := g.typ(arr.table_expr.typ)
			g.writeln('$ctyp $tmp_var = (*($ctyp*)array_get(${arr.object_var_name}.${field_names[i]}, $idx));')
			arr.object_var_name = tmp_var
			mut fff := []ast.StructField{}
			for f in arr.fields {
				mut skip := false
				mut primary := false
				for attr in f.attrs {
					if attr.name == 'primary' {
						primary = true
					}
					if attr.name == 'skip' {
						skip = true
					}
				}
				if !skip && !primary {
					fff << f
				}
			}
			arr.fields = fff.clone()
			unsafe { fff.free() }
			g.sql_insert(arr, expr, g.get_table_name(arr.table_expr), last_ids, res_,
				id_name, true, fkeys[i])
			g.writeln('}')
		}
	}
}

fn (mut g Gen) sql_update(node ast.SqlStmtLine, expr string, table_name string) {
	g.write('update(${expr}._object, _SLIT("$table_name"), (orm__OrmQueryData){')
	g.write('.fields = new_array_from_c_array($node.updated_columns.len, $node.updated_columns.len, sizeof(string), _MOV((string[$node.updated_columns.len]){')
	for field in node.updated_columns {
		g.write('_SLIT("$field"),')
	}
	g.write('})),')
	g.write('.data = new_array_from_c_array($node.update_exprs.len, $node.update_exprs.len, sizeof(orm__Primitive), _MOV((orm__Primitive[$node.update_exprs.len]){')
	for e in node.update_exprs {
		g.sql_expr_to_orm_primitive(e)
	}
	g.write('})),},')
	g.sql_gen_where_data(node.where_expr)
	g.writeln(');')
}

fn (mut g Gen) sql_delete(node ast.SqlStmtLine, expr string, table_name string) {
	g.write('v_delete(${expr}._object, _SLIT("$table_name"),')
	g.sql_gen_where_data(node.where_expr)
	g.writeln(');')
}

fn (mut g Gen) sql_expr_to_orm_primitive(expr ast.Expr) {
	match expr {
		ast.InfixExpr {
			g.sql_write_orm_primitive(expr.left_type, expr)
		}
		ast.StringLiteral {
			g.sql_write_orm_primitive(ast.string_type, expr)
		}
		ast.IntegerLiteral {
			g.sql_write_orm_primitive(ast.int_type, expr)
		}
		ast.BoolLiteral {
			g.sql_write_orm_primitive(ast.bool_type, expr)
		}
		ast.Ident {
			info := expr.info as ast.IdentVar
			g.sql_write_orm_primitive(info.typ, expr)
		}
		ast.SelectorExpr {
			g.sql_write_orm_primitive(expr.typ, expr)
		}
		else {
			eprintln(expr)
			verror('Unknown expr')
		}
	}
}

fn (mut g Gen) sql_write_orm_primitive(t ast.Type, expr ast.Expr) {
	mut sym := g.table.get_type_symbol(t)
	mut typ := sym.cname
	if typ == 'orm__Primitive' {
		g.expr(expr)
		g.write(',')
		return
	}
	if typ == 'time__Time' {
		typ = 'time'
	}
	g.write('orm__${typ}_to_primitive(')
	g.expr(expr)
	g.write('),')
}

fn (mut g Gen) sql_where_data(expr ast.Expr, mut fields []string, mut kinds []string, mut data []ast.Expr, mut is_and []bool) {
	match expr {
		ast.InfixExpr {
			g.sql_side = .left
			g.sql_where_data(expr.left, mut fields, mut kinds, mut data, mut is_and)
			mut kind := match expr.op {
				.ne {
					'orm__OperationKind_neq'
				}
				.eq {
					'orm__OperationKind_eq'
				}
				.lt {
					'orm__OperationKind_lt'
				}
				.gt {
					'orm__OperationKind_gt'
				}
				.ge {
					'orm__OperationKind_ge'
				}
				.le {
					'orm__OperationKind_le'
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
					kind = 'orm__OperationKind_eq'
				}
			}
			if expr.left !is ast.InfixExpr && expr.right !is ast.InfixExpr {
				kinds << kind
			}
			g.sql_side = .right
			g.sql_where_data(expr.right, mut fields, mut kinds, mut data, mut is_and)
		}
		ast.Ident {
			if g.sql_side == .left {
				fields << g.get_field_name(g.get_struct_field(expr.name))
			} else {
				data << expr
			}
		}
		ast.StringLiteral {
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
		else {}
	}
}

fn (mut g Gen) sql_gen_where_data(where_expr ast.Expr) {
	g.write('(orm__OrmQueryData){')
	mut fields := []string{}
	mut kinds := []string{}
	mut data := []ast.Expr{}
	mut is_and := []bool{}
	g.sql_where_data(where_expr, mut fields, mut kinds, mut data, mut is_and)

	g.write('.fields = new_array_from_c_array($fields.len, $fields.len, sizeof(string), _MOV((string[$fields.len]){')
	for field in fields {
		g.write('_SLIT("$field"),')
	}
	g.write('})),')

	g.write('.data = new_array_from_c_array($data.len, $data.len, sizeof(orm__Primitive), _MOV((orm__Primitive[$data.len]){')
	for e in data {
		g.sql_expr_to_orm_primitive(e)
	}
	g.write('})),')

	g.write('.kinds = new_array_from_c_array($kinds.len, $kinds.len, sizeof(orm__OperationKind), _MOV((orm__OperationKind[$kinds.len]){')
	for k in kinds {
		g.write('$k,')
	}
	g.write('})),')

	g.write('.is_and = new_array_from_c_array($is_and.len, $is_and.len, sizeof(bool), _MOV((bool[$kinds.len]){')
	for b in is_and {
		g.write('$b, ')
	}
	g.write('})),}')
}

fn (mut g Gen) sql_select_expr(node ast.SqlExpr) {
	left := g.go_before_stmt(0)
	conn := g.new_tmp_var()
	g.writeln('')
	g.writeln('// orm')
	g.write('orm__OrmConnection $conn = (orm__OrmConnection){._')
	mut fn_prefix := ''
	typ := g.parse_db_type(node.db_expr)
	match typ {
		.sqlite3 {
			fn_prefix = 'sqlite__DB'
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}

	g.write('$fn_prefix = &')
	g.expr(node.db_expr)
	g.writeln(', ._typ = _orm__OrmConnection_${fn_prefix}_index};')
	g.sql_select(node, conn, left)
}

fn (mut g Gen) sql_select(node ast.SqlExpr, expr string, left string) {
	mut fields := []ast.StructField{}
	mut prim := ''
	for f in node.fields {
		mut skip := false
		for attr in f.attrs {
			if attr.name == 'primary' {
				prim = f.name
			}
			if attr.name == 'skip' {
				skip = true
			}
		}
		if !skip {
			fields << f
		}
	}

	res := g.new_tmp_var()
	table_name := g.get_table_name(node.table_expr)
	g.sql_table_name = g.table.get_type_symbol(node.table_expr.typ).name
	g.write('Option_Array_Array_orm__Primitive _o$res = orm__OrmConnection_name_table[${expr}._typ]._method_select(${expr}._object, ')
	g.write('(orm__OrmSelectConfig){')
	g.write('.table = _SLIT("$table_name"),')
	g.write('.is_count = $node.is_count,')
	g.write('.has_where = $node.has_where,')
	g.write('.has_order = $node.has_order,')
	if node.has_order {
		g.write('.order = _SLIT("')
		g.expr(node.order_expr)
		g.write('"),')
		if node.has_desc {
			g.write('.order_type = orm__OrmOrderType_desc,')
		} else {
			g.write('.order_type = orm__OrmOrderType_asc,')
		}
	}
	g.write('.has_limit = $node.has_limit,')
	g.write('.has_offset = $node.has_offset,')
	if prim != '' {
		g.write('.primary = _SLIT("$prim"),')
	}
	select_fields := fields.filter(g.table.get_type_symbol(it.typ).kind != .array)
	g.write('.fields = new_array_from_c_array($select_fields.len, $select_fields.len, sizeof(string), _MOV((string[$select_fields.len]){')
	mut types := []int{}
	for field in select_fields {
		g.write('_SLIT("${g.get_field_name(field)}"),')
		sym := g.table.get_type_symbol(field.typ)
		if sym.kind == .struct_ {
			types << int(ast.int_type)
			continue
		}
		types << int(field.typ)
	}
	g.write('})),')
	g.write('.types = new_array_from_c_array($types.len, $types.len, sizeof(int), _MOV((int[$types.len]){')
	for typ in types {
		g.write('$typ,')
	}
	g.write('})),},')

	mut exprs := []ast.Expr{}
	if node.has_limit {
		exprs << node.limit_expr
	}
	if node.has_offset {
		exprs << node.offset_expr
	}
	g.write('(orm__OrmQueryData) {')
	g.write('.data = new_array_from_c_array($exprs.len, $exprs.len, sizeof(orm__Primitive), _MOV((orm__Primitive[$exprs.len]){')
	for e in exprs {
		g.sql_expr_to_orm_primitive(e)
	}
	g.write('}))},')

	if node.has_where {
		g.sql_gen_where_data(node.where_expr)
	} else {
		g.write('(orm__OrmQueryData) {}')
	}
	g.writeln(');')
	g.writeln('if (_o${res}.state != 0 && _o${res}.err._typ != _IError_None___index) { v_panic(IError_str(_o${res}.err)); }')
	g.writeln('Array_Array_orm__Primitive $res = (*(Array_Array_orm__Primitive*)_o${res}.data);')

	if node.is_count {
		g.writeln('$left *((*(orm__Primitive*) array_get((*(Array_orm__Primitive*)array_get($res, 0)), 0))._int);')
	} else {
		tmp := g.new_tmp_var()
		styp := g.typ(node.typ)
		idx := g.new_tmp_var()
		g.writeln('int $idx = 0;')
		mut typ_str := ''
		if node.is_array {
			info := g.table.get_type_symbol(node.typ).array_info()
			typ_str = g.typ(info.elem_type)
			g.writeln('$styp ${tmp}_array = __new_array(0, ${res}.len, sizeof($typ_str));')
			g.writeln('for (; $idx < ${res}.len; $idx++) {')
			g.write('\t$typ_str $tmp = ($typ_str) {')
			inf := g.table.get_type_symbol(info.elem_type).struct_info()
			for i, field in inf.fields {
				g.zero_struct_field(field)
				if i != inf.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		} else {
			g.write('$styp $tmp = ($styp){')
			info := g.table.get_type_symbol(node.typ).struct_info()
			for i, field in info.fields {
				g.zero_struct_field(field)
				if i != info.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		}

		g.writeln('if (${res}.len > 0) {')
		for i, field in fields {
			sel := '(*(orm__Primitive*) array_get((*(Array_orm__Primitive*) array_get($res, $idx)), $i))'
			sym := g.table.get_type_symbol(field.typ)
			if sym.kind == .struct_ {
				mut sub := node.sub_structs[int(field.typ)]
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident
				name := sel
				s := g.table.find_type_idx('orm.Primitive')
				if s != 0 {
					if ident.info is ast.IdentVar {
						mut info := ident.info as ast.IdentVar
						info.typ = s
						ident.info = info
					}
				}
				ident.name = name
				where_expr.right = ident
				sub.where_expr = where_expr

				g.sql_select(sub, expr, '${tmp}.$field.name = ')
			} else if sym.kind == .array {
				mut fkey := ''
				for attr in field.attrs {
					if attr.name == 'fkey' && attr.has_arg && attr.kind == .string {
						fkey = attr.arg
					}
				}
				if fkey == '' {
					verror('An field which holds an array, needs a fkey defined')
				}
				info := sym.array_info()
				arr_typ := info.elem_type
				sub := node.sub_structs[int(arr_typ)]
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut l := where_expr.left as ast.Ident
				mut r := where_expr.right as ast.Ident
				l.name = fkey
				r.name = tmp
				where_expr.left = l
				where_expr.right = ast.SelectorExpr{
					pos: r.pos
					field_name: prim
					is_mut: false
					expr: r
					expr_type: (r.info as ast.IdentVar).typ
					typ: ast.int_type
					scope: 0
				}
				mut arr := ast.SqlExpr{
					typ: field.typ
					is_count: sub.is_count
					db_expr: sub.db_expr
					has_where: sub.has_where
					has_offset: sub.has_offset
					offset_expr: sub.offset_expr
					has_order: sub.has_order
					order_expr: sub.order_expr
					has_desc: sub.has_desc
					is_array: true
					pos: sub.pos
					has_limit: sub.has_limit
					limit_expr: sub.limit_expr
					table_expr: sub.table_expr
					fields: sub.fields
					where_expr: where_expr
				}

				g.sql_select(arr, expr, '${tmp}.$field.name = ')
			} else {
				mut typ := sym.cname
				g.writeln('${tmp}.$field.name = *(${sel}._$typ);')
			}
		}
		g.writeln('}')

		if node.is_array {
			g.writeln('array_push(&${tmp}_array, _MOV(($typ_str[]){ $tmp }));')
			g.writeln('}')
		}

		g.write('$left $tmp')
		if node.is_array {
			g.write('_array')
		}
		g.writeln(';')
	}
}

/*
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
	if typ == .psql {
		lit = '\\"'
	}
	mut create_string := 'CREATE TABLE IF NOT EXISTS $lit$table_name$lit ('

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
*/

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
		else {
			return .unknown
		}
	}
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
	sym := g.table.get_type_symbol(field.typ)
	if sym.kind == .struct_ {
		name = '${name}_id'
	}
	return name
}
