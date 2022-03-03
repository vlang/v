// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

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
	conn := g.new_tmp_var()
	g.writeln('')
	g.writeln('// orm')
	g.write('orm__Connection $conn = (orm__Connection){._')
	mut fn_prefix := ''
	typ := g.parse_db_type(node.db_expr)
	match typ {
		.sqlite3 {
			fn_prefix = 'sqlite__DB'
		}
		.mysql {
			fn_prefix = 'mysql__Connection'
		}
		.psql {
			fn_prefix = 'pg__DB'
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}
	g.write('$fn_prefix = &')
	g.expr(node.db_expr)
	g.writeln(', ._typ = _orm__Connection_${fn_prefix}_index};')
	for line in node.lines {
		g.sql_stmt_line(line, conn)
	}
}

fn (mut g Gen) sql_stmt_line(nd ast.SqlStmtLine, expr string) {
	mut node := nd
	table_name := g.get_table_name(node.table_expr)
	g.sql_table_name = g.table.sym(node.table_expr.typ).name
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
		g.write('Option_void $res = orm__Connection_name_table[${expr}._typ]._method_')
		g.sql_create_table(node, expr, table_name)
		subs = true
	} else if node.kind == .drop {
		g.write('Option_void $res = orm__Connection_name_table[${expr}._typ]._method_')
		g.writeln('drop(${expr}._object, _SLIT("$table_name"));')
		subs = true
	} else if node.kind == .insert {
		arr := g.new_tmp_var()
		g.writeln('Array_orm__Primitive $arr = new_array_from_c_array(0, 0, sizeof(orm__Primitive), NULL);')
		g.sql_insert(node, expr, table_name, arr, res, '', false, '')
		dcheck = true
	} else if node.kind == .update {
		g.write('Option_void $res = orm__Connection_name_table[${expr}._typ]._method_')
		g.sql_update(node, expr, table_name)
	} else if node.kind == .delete {
		g.write('Option_void $res = orm__Connection_name_table[${expr}._typ]._method_')
		g.sql_delete(node, expr, table_name)
	}
	if !dcheck {
		g.writeln('if (${res}.state != 0 && ${res}.err._typ != _IError_None___index) { _v_panic(IError_str(${res}.err)); }')
	}
	if subs {
		for _, sub in node.sub_structs {
			g.sql_stmt_line(sub, expr)
		}
	}
}

fn (mut g Gen) sql_create_table(node ast.SqlStmtLine, expr string, table_name string) {
	g.write('create(${expr}._object, _SLIT("$table_name"), new_array_from_c_array($node.fields.len, $node.fields.len, sizeof(orm__TableField),')
	if node.fields.len > 0 {
		g.write(' _MOV((orm__TableField[$node.fields.len]){')
		for field in node.fields {
			sym := g.table.sym(field.typ)
			g.write('(orm__TableField){')
			g.write('.name = _SLIT("$field.name"),')
			mut typ := int(field.typ)
			if sym.name == 'time.Time' {
				typ = -2
			}
			g.write('.typ = $typ,')
			g.write('.is_arr = ${sym.kind == .array}, ')
			g.write('.is_time = ${int(g.table.get_type_name(field.typ) == 'time__Time')},')
			g.write('.default_val = (string){.str = (byteptr) "$field.default_val", .is_lit = 1},')
			g.write('.attrs = new_array_from_c_array($field.attrs.len, $field.attrs.len, sizeof(StructAttribute),')
			if field.attrs.len > 0 {
				g.write(' _MOV((StructAttribute[$field.attrs.len]){')
				for attr in field.attrs {
					g.write('(StructAttribute){')
					g.write('.name = _SLIT("$attr.name"),')
					g.write('.has_arg = ${int(attr.has_arg)},')
					g.write('.arg = _SLIT("$attr.arg"),')
					g.write('.kind = ${int(attr.kind)},')
					g.write('},')
				}
				g.write('})')
			} else {
				g.write('NULL')
			}
			g.write(')')
			g.write('},')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.writeln('));')
}

fn (mut g Gen) sql_insert(node ast.SqlStmtLine, expr string, table_name string, last_ids_arr string, res string, pid string, is_array bool, fkey string) {
	mut subs := []ast.SqlStmtLine{}
	mut arrs := []ast.SqlStmtLine{}
	mut fkeys := []string{}
	mut field_names := []string{}

	for f in node.fields {
		sym := g.table.sym(f.typ)
		if sym.kind == .struct_ && sym.name != 'time.Time' {
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

	fields := node.fields.filter(g.table.sym(it.typ).kind != .array)

	for sub in subs {
		g.sql_stmt_line(sub, expr)
		g.writeln('array_push(&$last_ids_arr, _MOV((orm__Primitive[]){orm__Connection_name_table[${expr}._typ]._method_last_id(${expr}._object)}));')
	}

	g.write('Option_void $res = orm__Connection_name_table[${expr}._typ]._method_')
	g.write('insert(${expr}._object, _SLIT("$table_name"), (orm__QueryData){')

	g.write('.fields = new_array_from_c_array($fields.len, $fields.len, sizeof(string),')
	if fields.len > 0 {
		g.write('_MOV((string[$fields.len]){')
		for f in fields {
			g.write('_SLIT("${g.get_field_name(f)}"),')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),')

	g.write('.data = new_array_from_c_array($fields.len, $fields.len, sizeof(orm__Primitive),')
	if fields.len > 0 {
		g.write(' _MOV((orm__Primitive[$fields.len]){')
		mut structs := 0
		for f in fields {
			if f.name == fkey {
				g.write('$pid, ')
				continue
			}
			mut sym := g.table.sym(f.typ)
			mut typ := sym.cname
			if sym.kind == .struct_ && typ != 'time__Time' {
				g.write('(*(orm__Primitive*) array_get($last_ids_arr, $structs)),')
				structs++
				continue
			}
			if typ == 'time__Time' {
				typ = 'time'
			}
			g.write('orm__${typ}_to_primitive(${node.object_var_name}.$f.name),')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),')
	g.write('.types = new_array_from_c_array(0, 0, sizeof(int), NULL),')
	g.write('.kinds = new_array_from_c_array(0, 0, sizeof(orm__OperationKind), NULL),')
	g.write('.is_and = new_array_from_c_array(0, 0, sizeof(bool), NULL),')
	g.writeln('});')

	g.writeln('if (${res}.state != 0 && ${res}.err._typ != _IError_None___index) { _v_panic(IError_str(${res}.err)); }')
	if arrs.len > 0 {
		mut id_name := g.new_tmp_var()
		g.writeln('orm__Primitive $id_name = orm__Connection_name_table[${expr}._typ]._method_last_id(${expr}._object);')
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
	// println(table_name)
	// println(expr)
	// println(node)
	g.write('update(${expr}._object, _SLIT("$table_name"), (orm__QueryData){')
	g.write('.kinds = new_array_from_c_array(0, 0, sizeof(orm__OperationKind), NULL),')
	g.write('.is_and = new_array_from_c_array(0, 0, sizeof(bool), NULL),')
	g.write('.types = new_array_from_c_array(0, 0, sizeof(int), NULL),')
	g.write('.fields = new_array_from_c_array($node.updated_columns.len, $node.updated_columns.len, sizeof(string),')
	if node.updated_columns.len > 0 {
		g.write(' _MOV((string[$node.updated_columns.len]){')
		for field in node.updated_columns {
			g.write('_SLIT("$field"),')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),')
	g.write('.data = new_array_from_c_array($node.update_exprs.len, $node.update_exprs.len, sizeof(orm__Primitive),')
	if node.update_exprs.len > 0 {
		g.write(' _MOV((orm__Primitive[$node.update_exprs.len]){')
		for e in node.update_exprs {
			g.sql_expr_to_orm_primitive(e)
		}
		g.write('})')
	}
	g.write('),},')
	g.sql_gen_where_data(node.where_expr)
	g.writeln(');')
}

fn (mut g Gen) sql_delete(node ast.SqlStmtLine, expr string, table_name string) {
	g.write('_v_delete(${expr}._object, _SLIT("$table_name"),')
	g.sql_gen_where_data(node.where_expr)
	g.writeln(');')
}

fn (mut g Gen) sql_expr_to_orm_primitive(expr ast.Expr) {
	match expr {
		ast.InfixExpr {
			g.sql_write_orm_primitive(g.table.find_type_idx('orm.InfixType'), expr)
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
	mut sym := g.table.sym(t)
	mut typ := sym.cname
	if typ == 'orm__Primitive' {
		g.expr(expr)
		g.write(',')
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
		g.write('.name = _SLIT("$expr.left"),')
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
		g.write('.operator = $kind,')
		g.write('.right = ')
		g.sql_expr_to_orm_primitive(expr.right)
		g.write('}')
	} else {
		g.expr(expr)
	}
	g.write('),')
}

fn (mut g Gen) sql_where_data(expr ast.Expr, mut fields []string, mut kinds []string, mut data []ast.Expr, mut is_and []bool) {
	match expr {
		ast.InfixExpr {
			g.sql_side = .left
			g.sql_where_data(expr.left, mut fields, mut kinds, mut data, mut is_and)
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
	g.write('(orm__QueryData){')
	mut fields := []string{}
	mut kinds := []string{}
	mut data := []ast.Expr{}
	mut is_and := []bool{}
	g.sql_where_data(where_expr, mut fields, mut kinds, mut data, mut is_and)
	g.write('.types = new_array_from_c_array(0, 0, sizeof(int), NULL),')
	g.write('.fields = new_array_from_c_array($fields.len, $fields.len, sizeof(string),')
	if fields.len > 0 {
		g.write(' _MOV((string[$fields.len]){')
		for field in fields {
			g.write('_SLIT("$field"),')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),')

	g.write('.data = new_array_from_c_array($data.len, $data.len, sizeof(orm__Primitive),')
	if data.len > 0 {
		g.write(' _MOV((orm__Primitive[$data.len]){')
		for e in data {
			g.sql_expr_to_orm_primitive(e)
		}
		g.write('})')
	}
	g.write('),')

	g.write('.kinds = new_array_from_c_array($kinds.len, $kinds.len, sizeof(orm__OperationKind),')
	if kinds.len > 0 {
		g.write(' _MOV((orm__OperationKind[$kinds.len]){')
		for k in kinds {
			g.write('$k,')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),')

	g.write('.is_and = new_array_from_c_array($is_and.len, $is_and.len, sizeof(bool),')
	if is_and.len > 0 {
		g.write(' _MOV((bool[$is_and.len]){')
		for b in is_and {
			g.write('$b, ')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),}')
}

fn (mut g Gen) sql_select_expr(node ast.SqlExpr) {
	left := g.go_before_stmt(0)
	conn := g.new_tmp_var()
	g.writeln('')
	g.writeln('// orm')
	g.write('orm__Connection $conn = (orm__Connection){._')
	mut fn_prefix := ''
	typ := g.parse_db_type(node.db_expr)
	match typ {
		.sqlite3 {
			fn_prefix = 'sqlite__DB'
		}
		.mysql {
			fn_prefix = 'mysql__Connection'
		}
		.psql {
			fn_prefix = 'pg__DB'
		}
		else {
			verror('This database type `$typ` is not implemented yet in orm') // TODO add better error
		}
	}

	g.write('$fn_prefix = &')
	g.expr(node.db_expr)
	g.writeln(', ._typ = _orm__Connection_${fn_prefix}_index};')
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
	g.sql_table_name = g.table.sym(node.table_expr.typ).name
	g.write('Option_Array_Array_orm__Primitive _o$res = orm__Connection_name_table[${expr}._typ]._method_select(${expr}._object, ')
	g.write('(orm__SelectConfig){')
	g.write('.table = _SLIT("$table_name"),')
	g.write('.is_count = $node.is_count,')
	g.write('.has_where = $node.has_where,')
	g.write('.has_order = $node.has_order,')
	if node.has_order {
		g.write('.order = _SLIT("')
		g.expr(node.order_expr)
		g.write('"),')
		if node.has_desc {
			g.write('.order_type = orm__OrderType__desc,')
		} else {
			g.write('.order_type = orm__OrderType__asc,')
		}
	}
	g.write('.has_limit = $node.has_limit,')
	g.write('.has_offset = $node.has_offset,')
	if prim != '' {
		g.write('.primary = _SLIT("$prim"),')
	}
	select_fields := fields.filter(g.table.sym(it.typ).kind != .array)
	g.write('.fields = new_array_from_c_array($select_fields.len, $select_fields.len, sizeof(string),')
	mut types := []int{}
	if select_fields.len > 0 {
		g.write(' _MOV((string[$select_fields.len]){')
		for field in select_fields {
			g.write('_SLIT("${g.get_field_name(field)}"),')
			sym := g.table.sym(field.typ)
			if sym.name == 'time.Time' {
				types << -2
				continue
			}
			if sym.kind == .struct_ {
				types << int(ast.int_type)
				continue
			}
			types << int(field.typ)
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),')
	g.write('.types = new_array_from_c_array($types.len, $types.len, sizeof(int),')
	if types.len > 0 {
		g.write(' _MOV((int[$types.len]){')
		for typ in types {
			g.write('$typ,')
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write('),},')

	mut exprs := []ast.Expr{}
	if node.has_limit {
		exprs << node.limit_expr
	}
	if node.has_offset {
		exprs << node.offset_expr
	}
	g.write('(orm__QueryData) {')
	g.write('.types = new_array_from_c_array(0, 0, sizeof(int), NULL),')
	g.write('.kinds = new_array_from_c_array(0, 0, sizeof(orm__OperationKind), NULL),')
	g.write('.is_and = new_array_from_c_array(0, 0, sizeof(bool), NULL),')
	g.write('.data = new_array_from_c_array($exprs.len, $exprs.len, sizeof(orm__Primitive),')
	if exprs.len > 0 {
		g.write(' _MOV((orm__Primitive[$exprs.len]){')
		for e in exprs {
			g.sql_expr_to_orm_primitive(e)
		}
		g.write('})')
	} else {
		g.write('NULL')
	}
	g.write(')},')

	if node.has_where {
		g.sql_gen_where_data(node.where_expr)
	} else {
		g.write('(orm__QueryData) {}')
	}
	g.writeln(');')
	g.writeln('if (_o${res}.state != 0 && _o${res}.err._typ != _IError_None___index) { _v_panic(IError_str(_o${res}.err)); }')
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
			info := g.table.sym(node.typ).array_info()
			typ_str = g.typ(info.elem_type)
			g.writeln('$styp ${tmp}_array = __new_array(0, ${res}.len, sizeof($typ_str));')
			g.writeln('for (; $idx < ${res}.len; $idx++) {')
			g.write('\t$typ_str $tmp = ($typ_str) {')
			inf := g.table.sym(info.elem_type).struct_info()
			for i, field in inf.fields {
				g.zero_struct_field(field)
				if i != inf.fields.len - 1 {
					g.write(', ')
				}
			}
			g.writeln('};')
		} else {
			g.write('$styp $tmp = ($styp){')
			info := g.table.sym(node.typ).struct_info()
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
			sym := g.table.sym(field.typ)
			if sym.kind == .struct_ && sym.name != 'time.Time' {
				mut sub := node.sub_structs[int(field.typ)]
				mut where_expr := sub.where_expr as ast.InfixExpr
				mut ident := where_expr.right as ast.Ident
				name := sel
				s := g.table.find_type_idx('orm.Primitive')
				if s != 0 {
					if mut ident.info is ast.IdentVar {
						ident.info.typ = s
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
		if !g.inside_call {
			g.writeln(';')
		}
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

fn (mut g Gen) get_table_name(table_expr ast.TypeNode) string {
	info := g.table.sym(table_expr.typ).struct_info()
	mut tablename := util.strip_mod_name(g.table.sym(table_expr.typ).name)
	for attr in info.attrs {
		if attr.kind == .string && attr.name == 'table' && attr.arg != '' {
			tablename = attr.arg
			break
		}
	}
	return tablename
}

fn (mut g Gen) get_struct_field(name string) ast.StructField {
	info := g.table.sym(g.table.type_idxs[g.sql_table_name]).struct_info()
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
	sym := g.table.sym(field.typ)
	if sym.kind == .struct_ && sym.name != 'time.Time' {
		name = '${name}_id'
	}
	return name
}
