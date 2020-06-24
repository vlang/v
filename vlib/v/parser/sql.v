// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.table

fn (mut p Parser) sql_expr() ast.Expr {
	// `sql db {`
	p.check_name()
	db_expr := p.expr(0)
	p.check(.lcbr)
	// kind := ast.SqlExprKind.select_
	//
	/*
	if p.tok.kind == .name && p.tok.lit == 'insert' {
		return p.sql_insert_expr(db_var_name)
		// kind = .insert
	}
	*/
	p.check(.key_select)
	n := p.check_name()
	is_count := n == 'count'
	mut typ := table.void_type
	if is_count {
		p.check_name() // from
		typ = table.int_type
	}
	table_type := p.parse_type() // `User`
	sym := p.table.get_type_symbol(table_type)
	table_name := sym.name
	mut where_expr := ast.Expr{}
	has_where := p.tok.kind == .name && p.tok.lit == 'where'
	mut query_one := false // one object is returned, not an array
	if has_where {
		p.next()
		where_expr = p.expr(0)
		// `id == x` means that a single object is returned
		if !is_count && where_expr is ast.InfixExpr {
			e := where_expr as ast.InfixExpr
			if e.op == .eq && e.left is ast.Ident {
				ident := e.left as ast.Ident
				if ident.name == 'id' {
					query_one = true
				}
			}
		}
	}
	if p.tok.kind == .name && p.tok.lit == 'limit' {
		// `limit 1` means that a single object is returned
		p.check_name() // `limit`
		if p.tok.kind == .number && p.tok.lit == '1' {
			query_one = true
		}
		p.next()
	}
	if !query_one && !is_count {
		// return an array
		typ = table.new_type(p.table.find_or_register_array(table_type, 1, p.mod))
	} else if !is_count {
		// return a single object
		// TODO optional
		// typ = table_type.set_flag(.optional)
		typ = table_type
	}
	p.check(.rcbr)
	// /////////
	// Register this type's fields as variables so they can be used in `where`
	// expressions
	// fields := typ.fields.filter(typ == 'string' || typ == 'int')
	// fields := typ.fields
	// get only string and int fields
	// mut fields := []Var
	info := sym.info as table.Struct
	fields := info.fields.filter(it.typ in [table.string_type, table.int_type, table.bool_type] &&
		'skip' !in it.attrs)
	if fields.len == 0 {
		p.error('V orm: select: empty fields in `$table_name`')
	}
	if fields[0].name != 'id' {
		p.error('V orm: `id int` must be the first field in `$table_name`')
	}
	for field in fields {
		// println('registering sql field var $field.name')
		p.scope.register(field.name, ast.Var{
			name: field.name
			typ: field.typ
			is_mut: true
			is_used: true
			is_changed: true
		})
	}
	// ////////////
	return ast.SqlExpr{
		is_count: is_count
		typ: typ
		db_expr: db_expr
		table_name: table_name
		where_expr: where_expr
		has_where: has_where
		fields: fields
		is_array: !query_one
	}
}

// insert user into User
// update User set nr_oders=nr_orders+1 where id == user_id
fn (mut p Parser) sql_stmt() ast.SqlStmt {
	p.inside_match = true
	defer {
		p.inside_match = false
	}
	// `sql db {`
	p.check_name()
	db_expr := p.expr(0)
	// println(typeof(db_expr))
	p.check(.lcbr)
	// kind := ast.SqlExprKind.select_
	//
	mut n := p.check_name() // insert
	mut kind := ast.SqlStmtKind.insert
	if n == 'delete' {
		kind = .delete
	} else if n == 'update' {
		kind = .update
	}
	mut inserted_var_name := ''
	mut table_name := ''
	expr := p.expr(0)
	match expr {
		ast.Ident {
			if kind == .insert {
				inserted_var_name = expr.name
			} else if kind == .update {
				table_name = expr.name
			}
		}
		else {
			p.error('can only insert variables')
		}
	}
	n = p.check_name() // into
	mut updated_columns := []string{}
	if kind == .insert && n != 'into' {
		p.error('expecting `into`')
	} else if kind == .update {
		if n != 'set' {
			p.error('expecting `set`')
		}
		column := p.check_name()
		updated_columns << column
		p.check(.assign)
		p.expr(0)
	}
	mut table_type := table.Type(0)
	if kind == .insert {
		table_type = p.parse_type() // `User`
		sym := p.table.get_type_symbol(table_type)
		// info := sym.info as table.Struct
		// fields := info.fields.filter(it.typ in [table.string_type, table.int_type, table.bool_type])
		table_name = sym.name
	} else if kind == .update {
		idx := p.table.find_type_idx(table_name)
		table_type = table.new_type(idx)
		p.check_sql_keyword('where')
		p.expr(0)
	}
	p.check(.rcbr)
	return ast.SqlStmt{
		db_expr: db_expr
		table_name: table_name
		table_type: table_type
		object_var_name: inserted_var_name
	}
}

fn (mut p Parser) check_sql_keyword(name string) {
	if p.check_name() != name {
		p.error('orm: expecting `$name`')
	}
}
