// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

fn (mut p Parser) sql_expr() ast.Expr {
	tmp_inside_match := p.inside_match
	p.inside_match = true
	// `sql db {`
	pos := p.tok.pos()
	p.check_name()
	db_expr := p.check_expr(0) or {
		p.unexpected(prepend_msg: 'invalid expression:', expecting: 'database')
	}
	p.check(.lcbr)
	p.check(.key_select)
	n := p.check_name()
	is_count := n == 'count'
	mut typ := ast.void_type
	if is_count {
		p.check_name() // from
		typ = ast.int_type
	}
	table_pos := p.tok.pos()
	table_type := p.parse_type() // `User`
	mut where_expr := ast.empty_expr
	has_where := p.tok.kind == .name && p.tok.lit == 'where'
	mut query_one := false // one object is returned, not an array
	if has_where {
		p.next()
		where_expr = p.expr(0)
		// `id == x` means that a single object is returned
		if !is_count && mut where_expr is ast.InfixExpr {
			if where_expr.op == .eq && mut where_expr.left is ast.Ident {
				if where_expr.left.name == 'id' {
					query_one = true
				}
			}
			if mut where_expr.right is ast.Ident {
				if !p.scope.known_var(where_expr.right.name) {
					p.check_undefined_variables([where_expr.left], where_expr.right) or {
						return p.error_with_pos(err.msg(), where_expr.right.pos)
					}
				}
			}
		}
	}
	mut has_limit := false
	mut limit_expr := ast.empty_expr
	mut has_offset := false
	mut offset_expr := ast.empty_expr
	mut has_order := false
	mut order_expr := ast.empty_expr
	mut has_desc := false
	if p.tok.kind == .name && p.tok.lit == 'order' {
		p.check_name() // `order`
		order_pos := p.tok.pos()
		if p.tok.kind == .name && p.tok.lit == 'by' {
			p.check_name() // `by`
		} else {
			return p.error_with_pos('use `order by` in ORM queries', order_pos)
		}
		has_order = true
		order_expr = p.expr(0)
		if p.tok.kind == .name && p.tok.lit == 'desc' {
			p.check_name() // `desc`
			has_desc = true
		}
	}
	if p.tok.kind == .name && p.tok.lit == 'limit' {
		// `limit 1` means that a single object is returned
		p.check_name() // `limit`
		if p.tok.kind == .number && p.tok.lit == '1' {
			query_one = true
		}
		has_limit = true
		limit_expr = p.expr(0)
	}
	if p.tok.kind == .name && p.tok.lit == 'offset' {
		p.check_name() // `offset`
		has_offset = true
		offset_expr = p.expr(0)
	}
	if !query_one && !is_count {
		// return an array
		typ = ast.new_type(p.table.find_or_register_array(table_type))
	} else if !is_count {
		// return a single object
		// TODO optional
		// typ = table_type.set_flag(.optional)
		typ = table_type
	}
	p.check(.rcbr)
	p.inside_match = false
	or_expr := p.parse_sql_or_block()
	p.inside_match = tmp_inside_match
	return ast.SqlExpr{
		is_count: is_count
		typ: typ
		or_expr: or_expr
		db_expr: db_expr
		where_expr: where_expr
		has_where: has_where
		has_limit: has_limit
		limit_expr: limit_expr
		has_offset: has_offset
		offset_expr: offset_expr
		has_order: has_order
		order_expr: order_expr
		has_desc: has_desc
		is_array: !query_one
		pos: pos.extend(p.prev_tok.pos())
		table_expr: ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
	}
}

// insert user into User
// update User set nr_oders=nr_orders+1 where id == user_id
fn (mut p Parser) sql_stmt() ast.SqlStmt {
	mut pos := p.tok.pos()
	p.inside_match = true
	defer {
		p.inside_match = false
	}
	// `sql db {`
	p.check_name()
	db_expr := p.check_expr(0) or {
		p.unexpected(prepend_msg: 'invalid expression:', expecting: 'database')
	}
	// println(typeof(db_expr))
	p.check(.lcbr)

	mut lines := []ast.SqlStmtLine{}

	for p.tok.kind != .rcbr {
		lines << p.parse_sql_stmt_line()
	}

	p.next()

	mut or_expr := p.parse_sql_or_block()

	pos.last_line = p.prev_tok.line_nr
	return ast.SqlStmt{
		pos: pos.extend(p.prev_tok.pos())
		db_expr: db_expr
		lines: lines
		or_expr: or_expr
	}
}

fn (mut p Parser) parse_sql_or_block() ast.OrExpr {
	mut stmts := []ast.Stmt{}
	mut kind := ast.OrKind.absent
	mut pos := p.tok.pos()

	if p.tok.kind == .key_orelse {
		was_inside_or_expr := p.inside_or_expr
		p.inside_or_expr = true
		p.next()
		p.open_scope()
		p.scope.register(ast.Var{
			name: 'err'
			typ: ast.error_type
			pos: p.tok.pos()
			is_used: true
		})
		kind = .block
		stmts = p.parse_block_no_scope(false)
		pos = pos.extend(p.prev_tok.pos())
		p.close_scope()
		p.inside_or_expr = was_inside_or_expr
	}

	return ast.OrExpr{
		stmts: stmts
		kind: kind
		pos: pos
	}
}

fn (mut p Parser) parse_sql_stmt_line() ast.SqlStmtLine {
	mut n := p.check_name() // insert
	pos := p.tok.pos()
	mut kind := ast.SqlStmtKind.insert
	if n == 'delete' {
		kind = .delete
	} else if n == 'update' {
		kind = .update
	} else if n == 'create' {
		kind = .create
		table := p.check_name()
		if table != 'table' {
			p.error('expected `table` got `$table`')
			return ast.SqlStmtLine{}
		}
		typ := p.parse_type()
		typ_pos := p.tok.pos()
		return ast.SqlStmtLine{
			kind: kind
			pos: pos.extend(p.prev_tok.pos())
			table_expr: ast.TypeNode{
				typ: typ
				pos: typ_pos
			}
		}
	} else if n == 'drop' {
		kind = .drop
		table := p.check_name()
		if table != 'table' {
			p.error('expected `table` got `$table`')
			return ast.SqlStmtLine{}
		}
		typ := p.parse_type()
		typ_pos := p.tok.pos()
		return ast.SqlStmtLine{
			kind: kind
			pos: pos.extend(p.prev_tok.pos())
			table_expr: ast.TypeNode{
				typ: typ
				pos: typ_pos
			}
		}
	}
	mut inserted_var_name := ''
	mut table_type := ast.Type(0)
	if kind != .delete {
		if kind == .update {
			table_type = p.parse_type()
		} else if kind == .insert {
			expr := p.expr(0)
			if expr is ast.Ident {
				inserted_var_name = expr.name
			} else {
				p.error('can only insert variables')
				return ast.SqlStmtLine{}
			}
		}
	}
	n = p.check_name() // into
	mut updated_columns := []string{}
	mut update_exprs := []ast.Expr{cap: 5}
	if kind == .insert && n != 'into' {
		p.error('expecting `into`')
		return ast.SqlStmtLine{}
	} else if kind == .update {
		if n != 'set' {
			p.error('expecting `set`')
			return ast.SqlStmtLine{}
		}
		for {
			column := p.check_name()
			updated_columns << column
			p.check(.assign)
			update_exprs << p.expr(0)
			if p.tok.kind == .comma {
				p.check(.comma)
			} else {
				break
			}
		}
	} else if kind == .delete && n != 'from' {
		p.error('expecting `from`')
		return ast.SqlStmtLine{}
	}

	mut table_pos := p.tok.pos()
	mut where_expr := ast.empty_expr
	if kind == .insert {
		table_pos = p.tok.pos()
		table_type = p.parse_type()
	} else if kind == .update {
		p.check_sql_keyword('where') or { return ast.SqlStmtLine{} }
		where_expr = p.expr(0)
	} else if kind == .delete {
		table_pos = p.tok.pos()
		table_type = p.parse_type()
		p.check_sql_keyword('where') or { return ast.SqlStmtLine{} }
		where_expr = p.expr(0)
	}
	return ast.SqlStmtLine{
		table_expr: ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
		object_var_name: inserted_var_name
		pos: pos
		updated_columns: updated_columns
		update_exprs: update_exprs
		kind: kind
		where_expr: where_expr
	}
}

fn (mut p Parser) check_sql_keyword(name string) ?bool {
	if p.check_name() != name {
		p.error('orm: expecting `$name`')
		return none
	}
	return true
}
