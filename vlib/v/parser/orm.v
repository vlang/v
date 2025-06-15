// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast

// select from User
// insert user into User returning id
fn (mut p Parser) sql_expr() ast.Expr {
	tmp_inside_match := p.inside_match
	p.inside_orm = true
	p.inside_match = true
	// `sql db {`
	pos := p.tok.pos()
	p.check_name()
	db_expr := p.check_expr(0) or {
		p.unexpected(prepend_msg: 'invalid expression:', expecting: 'database')
	}
	p.check(.lcbr)
	// p.check(.key_select)
	is_select := p.tok.kind == .key_select
	is_insert := p.tok.lit == 'insert'
	if !is_select && !is_insert {
		p.error('expected "select" or "insert" in an ORM expression')
	}
	p.next()
	// kind := if is_select { ast.SqlExprKind.select_ } else { ast.SqlExprKind.insert }
	mut inserted_var := ''
	mut is_count := false
	if is_insert {
		inserted_var = p.check_name()
		p.scope.mark_var_as_used(inserted_var)
		into := p.check_name()
		if into != 'into' {
			p.error('expecting `into`')
		}
	} else if is_select {
		is_count = p.check_name() == 'count'
	}
	mut typ := ast.void_type

	if is_count {
		n := p.check_name() // from
		if n != 'from' {
			p.error('expecting "from" in a "select count" ORM statement')
		}
	}

	table_pos := p.tok.pos()
	table_type := p.parse_type() // `User`

	mut where_expr := ast.empty_expr
	has_where := p.tok.kind == .name && p.tok.lit == 'where'

	if has_where {
		p.next()
		where_expr = p.expr(0)

		where_check_result := p.check_sql_where_expr_has_no_undefined_variables(&where_expr,
			[])
		if where_check_result is ast.NodeError {
			return where_check_result
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
		p.check_name() // `limit`
		has_limit = true
		limit_expr = p.expr(0)
	}

	if p.tok.kind == .name && p.tok.lit == 'offset' {
		p.check_name() // `offset`
		has_offset = true
		offset_expr = p.expr(0)
	}

	if is_count {
		typ = ast.int_type
	} else if table_type.has_flag(.generic) {
		typ = ast.new_type(p.table.find_or_register_array(table_type)).set_flag(.generic)
	} else {
		typ = ast.new_type(p.table.find_or_register_array(table_type))
	}

	p.check(.rcbr)
	p.inside_match = false
	p.inside_orm = false
	or_expr := p.parse_sql_or_block()
	p.inside_match = tmp_inside_match

	return ast.SqlExpr{
		is_count:     is_count
		is_insert:    is_insert
		typ:          typ.set_flag(.result)
		or_expr:      or_expr
		db_expr:      db_expr
		where_expr:   where_expr
		has_where:    has_where
		has_limit:    has_limit
		limit_expr:   limit_expr
		has_offset:   has_offset
		offset_expr:  offset_expr
		has_order:    has_order
		order_expr:   order_expr
		has_desc:     has_desc
		is_array:     if is_count { false } else { true }
		is_generated: false
		inserted_var: inserted_var
		pos:          pos.extend(p.prev_tok.pos())
		table_expr:   ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
	}
}

// insert user into User
// update User set nr_oders=nr_orders+1 where id == user_id
// delete
fn (mut p Parser) sql_stmt() ast.SqlStmt {
	mut pos := p.tok.pos()
	p.inside_orm = true
	p.inside_match = true
	defer {
		p.inside_orm = false
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
		if p.tok.kind == .eof {
			p.unexpected_with_pos(pos, got: 'eof, while parsing an SQL statement')
			return ast.SqlStmt{}
		}
		lines << p.parse_sql_stmt_line()
	}

	p.next()

	mut or_expr := p.parse_sql_or_block()

	pos.last_line = p.prev_tok.line_nr
	return ast.SqlStmt{
		pos:     pos.extend(p.prev_tok.pos())
		db_expr: db_expr
		lines:   lines
		or_expr: or_expr
	}
}

fn (mut p Parser) parse_sql_or_block() ast.OrExpr {
	mut stmts := []ast.Stmt{}
	mut kind := ast.OrKind.absent
	mut pos := p.tok.pos()

	if p.tok.kind == .key_orelse {
		kind = .block
		stmts, pos = p.or_block(.with_err_var)
	} else if p.tok.kind == .not {
		kind = .propagate_result
		p.next()
	}

	return ast.OrExpr{
		stmts: stmts
		kind:  kind
		pos:   pos
	}
}

fn (mut p Parser) parse_sql_stmt_line() ast.SqlStmtLine {
	pre_comments := p.eat_comments()
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
			p.error('expected `table` got `${table}`')
			return ast.SqlStmtLine{}
		}
		typ := p.parse_type()
		typ_pos := p.tok.pos()
		end_comments := p.eat_comments()
		return ast.SqlStmtLine{
			kind:         kind
			pos:          pos.extend(p.prev_tok.pos())
			table_expr:   ast.TypeNode{
				typ: typ
				pos: typ_pos
			}
			scope:        p.scope
			is_generated: false
			pre_comments: pre_comments
			end_comments: end_comments
		}
	} else if n == 'drop' {
		kind = .drop
		table := p.check_name()
		if table != 'table' {
			p.error('expected `table` got `${table}`')
			return ast.SqlStmtLine{}
		}
		typ := p.parse_type()
		typ_pos := p.tok.pos()
		end_comments := p.eat_comments()
		return ast.SqlStmtLine{
			kind:         kind
			pos:          pos.extend(p.prev_tok.pos())
			table_expr:   ast.TypeNode{
				typ: typ
				pos: typ_pos
			}
			is_generated: false
			scope:        p.scope
			pre_comments: pre_comments
			end_comments: end_comments
		}
	}
	mut inserted_var := ''
	mut table_type := ast.no_type
	if kind != .delete {
		if kind == .update {
			table_type = p.parse_type()
		} else if kind == .insert {
			expr := p.expr(0)
			if expr is ast.Ident {
				inserted_var = expr.name
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

		where_expr_result := p.check_sql_where_expr_has_no_undefined_variables(&where_expr,
			[])
		if where_expr_result is ast.NodeError {
			return ast.SqlStmtLine{}
		}
	} else if kind == .delete {
		table_pos = p.tok.pos()
		table_type = p.parse_type()
		p.check_sql_keyword('where') or { return ast.SqlStmtLine{} }
		where_expr = p.expr(0)

		where_expr_result := p.check_sql_where_expr_has_no_undefined_variables(&where_expr,
			[])
		if where_expr_result is ast.NodeError {
			return ast.SqlStmtLine{}
		}
	}
	end_comments := p.eat_comments()
	return ast.SqlStmtLine{
		table_expr:      ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
		object_var:      inserted_var
		pos:             pos
		updated_columns: updated_columns
		update_exprs:    update_exprs
		kind:            kind
		where_expr:      where_expr
		is_generated:    false
		scope:           p.scope
		pre_comments:    pre_comments
		end_comments:    end_comments
	}
}

fn (mut p Parser) check_sql_keyword(name string) ?bool {
	if p.check_name() != name {
		p.error('ORM: expecting `${name}`')
		return none
	}
	return true
}

// check_sql_where_expr_has_no_undefined_variables recursively tries to find undefined variables in the right part of infix expressions.
fn (mut p Parser) check_sql_where_expr_has_no_undefined_variables(expr &ast.Expr, unacceptable_variable_names []string) ast.Expr {
	if expr is ast.Ident {
		if !p.scope.known_var(expr.name) {
			p.check_undefined_variables(unacceptable_variable_names, expr) or {
				return p.error_with_pos(err.msg(), expr.pos)
			}
		}
	} else if expr is ast.InfixExpr {
		if expr.left is ast.Ident {
			if expr.right is ast.Ident {
				return p.check_sql_where_expr_has_no_undefined_variables(expr.right, [
					expr.left.name,
				])
			}
		}

		left_check_result := p.check_sql_where_expr_has_no_undefined_variables(expr.left,
			[])

		if left_check_result is ast.NodeError {
			return left_check_result
		}

		variable_names := if expr.left is ast.Ident { [expr.left.str()] } else { []string{} }
		right_check_result := p.check_sql_where_expr_has_no_undefined_variables(expr.right,
			variable_names)

		if right_check_result is ast.NodeError {
			return right_check_result
		}
	} else if expr is ast.ParExpr {
		return p.check_sql_where_expr_has_no_undefined_variables(expr.expr, [])
	}

	return ast.empty_expr
}
