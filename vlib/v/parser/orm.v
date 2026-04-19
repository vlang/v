// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.token

struct SqlPrefix {
	pos              token.Pos
	db_expr          ast.Expr
	tmp_inside_match bool
	is_dynamic       bool
}

fn sql_aggregate_kind_from_name(name string) ast.SqlAggregateKind {
	return match name {
		'count' { .count }
		'sum' { .sum }
		'avg' { .avg }
		'min' { .min }
		'max' { .max }
		else { .none }
	}
}

// select from User
// insert user into User returning id
fn (mut p Parser) sql_expr() ast.Expr {
	prefix := p.sql_prefix()
	return p.sql_expr_after_prefix(prefix)
}

fn (mut p Parser) sql_stmt_or_expr() ast.Stmt {
	prefix := p.sql_prefix()
	if p.sql_stmt_can_be_value()
		&& (p.tok.kind == .key_select || (p.tok.kind == .name && p.tok.lit == 'insert')) {
		expr := p.sql_expr_after_prefix(prefix)
		return ast.ExprStmt{
			expr: expr
			pos:  expr.pos()
		}
	}
	return p.sql_stmt_after_prefix(prefix)
}

fn (p &Parser) sql_stmt_can_be_value() bool {
	return p.inside_if_expr || p.inside_match_body || p.inside_or_expr
}

fn (mut p Parser) sql_prefix() SqlPrefix {
	tmp_inside_match := p.inside_match
	p.inside_orm = true
	p.inside_match = true
	pos := p.tok.pos()
	p.check_name()
	db_expr := p.check_expr(0) or {
		p.unexpected(prepend_msg: 'invalid expression:', expecting: 'database')
	}
	p.check(.lcbr)
	mut is_dynamic := false
	if p.tok.kind == .name && p.tok.lit == 'dynamic' {
		is_dynamic = true
		p.next()
	}
	return SqlPrefix{
		pos:              pos
		db_expr:          db_expr
		tmp_inside_match: tmp_inside_match
		is_dynamic:       is_dynamic
	}
}

fn (mut p Parser) sql_expr_after_prefix(prefix SqlPrefix) ast.Expr {
	is_dynamic := prefix.is_dynamic
	is_select := p.tok.kind == .key_select
	is_insert := p.tok.lit == 'insert'
	if !is_select && !is_insert {
		p.error('expected "select" or "insert" in an ORM expression')
	}
	if is_dynamic && !is_select {
		p.error('expected "select" after `dynamic` in an ORM expression')
	}
	p.next()
	// kind := if is_select { ast.SqlExprKind.select_ } else { ast.SqlExprKind.insert }
	mut inserted_var := ''
	mut aggregate_kind := ast.SqlAggregateKind.none
	mut aggregate_field := ''
	mut has_distinct := false
	if is_insert {
		inserted_var = p.check_name()
		p.scope.mark_var_as_used(inserted_var)
		into := p.check_name()
		if into != 'into' {
			p.error('expecting `into`')
		}
	} else if is_select {
		n := p.check_name()
		if n == 'distinct' {
			has_distinct = true
			n2 := p.check_name()
			aggregate_kind = sql_aggregate_kind_from_name(n2)
		} else {
			aggregate_kind = sql_aggregate_kind_from_name(n)
		}
	}
	mut typ := ast.void_type

	if aggregate_kind == .count {
		n := p.check_name() // from
		if n != 'from' {
			p.error('expecting "from" in a "select count" ORM statement')
		}
	} else if aggregate_kind != .none {
		if p.tok.kind != .lpar {
			p.error('expecting `(` after aggregate function in ORM select')
		}
		p.next()
		if p.tok.kind != .name {
			p.error('ORM aggregate functions only support a single field name argument')
		}
		aggregate_field = p.check_name()
		if p.tok.kind != .rpar {
			p.error('ORM aggregate functions only support a single field name argument')
		}
		p.next()
		n := p.check_name() // from
		if n != 'from' {
			p.error('expecting `from` after ORM aggregate function')
		}
	}

	table_pos := p.tok.pos()
	table_type := p.parse_type() // `User`

	// Parse JOIN clauses (e.g., `join Department on User.dept_id == Department.id`)
	mut joins := []ast.JoinClause{}
	for p.tok.kind == .name && p.tok.lit in ['join', 'left', 'right', 'full', 'inner'] {
		join_clause := p.parse_sql_join_clause()
		if join_clause.table_expr.typ != ast.void_type {
			joins << join_clause
		}
	}

	mut where_expr := ast.empty_expr
	has_where := p.tok.kind == .name && p.tok.lit == 'where'

	if has_where {
		p.next()
		where_expr = p.expr(0)
		if !is_dynamic {
			where_check_result := p.check_sql_where_expr_has_no_undefined_variables(&where_expr, [])
			if where_check_result is ast.NodeError {
				return where_check_result
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
		p.check_name() // `limit`
		has_limit = true
		limit_expr = p.expr(0)
	}

	if p.tok.kind == .name && p.tok.lit == 'offset' {
		p.check_name() // `offset`
		has_offset = true
		offset_expr = p.expr(0)
	}

	if aggregate_kind == .count {
		typ = ast.int_type
	} else if aggregate_kind != .none {
		typ = ast.void_type
	} else if table_type.has_flag(.generic) {
		typ = ast.new_type(p.table.find_or_register_array(table_type)).set_flag(.generic)
	} else {
		typ = ast.new_type(p.table.find_or_register_array(table_type))
	}

	p.check(.rcbr)
	p.inside_match = false
	p.inside_orm = false
	or_expr := p.parse_sql_or_block()
	p.inside_match = prefix.tmp_inside_match

	return ast.SqlExpr{
		aggregate_kind:  aggregate_kind
		aggregate_field: aggregate_field
		is_insert:       is_insert
		typ:             typ.set_flag(.result)
		scope:           p.scope
		or_expr:         or_expr
		db_expr:         prefix.db_expr
		where_expr:      where_expr
		has_where:       has_where
		has_limit:       has_limit
		limit_expr:      limit_expr
		has_offset:      has_offset
		offset_expr:     offset_expr
		has_order:       has_order
		order_expr:      order_expr
		has_desc:        has_desc
		has_distinct:    has_distinct
		is_array:        aggregate_kind == .none
		is_generated:    false
		is_dynamic:      is_dynamic
		inserted_var:    inserted_var
		pos:             prefix.pos.extend(p.prev_tok.pos())
		table_expr:      ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
		joins:           joins
	}
}

// parse_sql_join_clause parses a JOIN clause like:
// `join Department on User.dept_id == Department.id`
// `left join Department on User.dept_id == Department.id`
// `inner join Department on User.dept_id == Department.id`
fn (mut p Parser) parse_sql_join_clause() ast.JoinClause {
	mut kind := ast.JoinKind.inner
	join_pos := p.tok.pos()

	// Check for join type prefix (left, right, full, inner)
	if p.tok.lit == 'left' {
		kind = .left
		p.next()
	} else if p.tok.lit == 'right' {
		kind = .right
		p.next()
	} else if p.tok.lit == 'full' {
		kind = .full_outer
		p.next()
		// Handle optional 'outer' keyword
		if p.tok.kind == .name && p.tok.lit == 'outer' {
			p.next()
		}
	} else if p.tok.lit == 'inner' {
		// 'inner' is optional, just skip it
		p.next()
	}

	// Now expect 'join'
	if p.tok.kind != .name || p.tok.lit != 'join' {
		p.error('expected `join` keyword after join type')
		return ast.JoinClause{}
	}
	p.next() // consume 'join'

	// Parse the joined table type
	table_pos := p.tok.pos()
	table_type := p.parse_type()

	// Expect 'on' keyword
	if p.tok.kind != .name || p.tok.lit != 'on' {
		p.error('expected `on` keyword after table name in JOIN clause')
		return ast.JoinClause{}
	}
	p.next() // consume 'on'

	// Parse the ON condition expression
	on_expr := p.expr(0)

	return ast.JoinClause{
		kind:       kind
		pos:        join_pos
		table_expr: ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
		on_expr:    on_expr
	}
}

// insert user into User
// update User set nr_oders=nr_orders+1 where id == user_id
// delete
fn (mut p Parser) sql_stmt() ast.SqlStmt {
	prefix := p.sql_prefix()
	return p.sql_stmt_after_prefix(prefix)
}

fn (mut p Parser) sql_stmt_after_prefix(prefix SqlPrefix) ast.SqlStmt {
	mut pos := prefix.pos

	mut lines := []ast.SqlStmtLine{}

	for p.tok.kind != .rcbr {
		if p.tok.kind == .eof {
			p.unexpected_with_pos(pos, got: 'eof, while parsing an SQL statement')
			return ast.SqlStmt{}
		}
		lines << p.parse_sql_stmt_line(prefix.is_dynamic)
	}

	p.next()

	p.inside_orm = false
	p.inside_match = false
	mut or_expr := p.parse_sql_or_block()
	p.inside_match = prefix.tmp_inside_match

	pos.last_line = p.prev_tok.line_nr
	return ast.SqlStmt{
		pos:     pos.extend(p.prev_tok.pos())
		db_expr: prefix.db_expr
		lines:   lines
		or_expr: or_expr
	}
}

fn (mut p Parser) parse_sql_or_block() ast.OrExpr {
	mut stmts := []ast.Stmt{}
	mut kind := ast.OrKind.absent
	mut pos := p.tok.pos()
	mut or_scope := ast.empty_scope

	if p.tok.kind == .key_orelse {
		kind = .block
		stmts, pos, or_scope = p.or_block(.with_err_var)
	} else if p.tok.kind == .not {
		kind = .propagate_result
		or_scope = p.scope
		p.next()
	}

	return ast.OrExpr{
		stmts: stmts
		kind:  kind
		pos:   pos
		scope: or_scope
	}
}

fn (mut p Parser) parse_sql_stmt_line(is_dynamic bool) ast.SqlStmtLine {
	pre_comments := p.eat_comments()
	mut n := p.check_name() // insert
	pos := p.tok.pos()
	mut kind := ast.SqlStmtKind.insert
	if n == 'delete' {
		kind = .delete
	} else if n == 'upsert' {
		kind = .upsert
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
			is_dynamic:   is_dynamic
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
			is_dynamic:   is_dynamic
			scope:        p.scope
			pre_comments: pre_comments
			end_comments: end_comments
		}
	}
	if is_dynamic && kind != .update {
		p.error('`dynamic` is only supported for ORM `select` and `update` queries')
		return ast.SqlStmtLine{}
	}
	mut inserted_var := ''
	mut table_type := ast.no_type
	if kind != .delete {
		if kind == .update {
			table_type = p.parse_type()
		} else if kind in [.insert, .upsert] {
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
	mut update_data_expr := ast.empty_expr
	if kind in [.insert, .upsert] && n != 'into' {
		p.error('expecting `into`')
		return ast.SqlStmtLine{}
	} else if kind == .update {
		if n != 'set' {
			p.error('expecting `set`')
			return ast.SqlStmtLine{}
		}
		if is_dynamic {
			update_data_expr = p.expr(0)
		} else {
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
		}
	} else if kind == .delete && n != 'from' {
		p.error('expecting `from`')
		return ast.SqlStmtLine{}
	}

	mut table_pos := p.tok.pos()
	mut where_expr := ast.empty_expr
	if kind in [.insert, .upsert] {
		table_pos = p.tok.pos()
		table_type = p.parse_type()
	} else if kind == .update {
		p.check_sql_keyword('where') or { return ast.SqlStmtLine{} }
		where_expr = p.expr(0)

		where_expr_result := p.check_sql_where_expr_has_no_undefined_variables(&where_expr, [])
		if where_expr_result is ast.NodeError {
			return ast.SqlStmtLine{}
		}
	} else if kind == .delete {
		table_pos = p.tok.pos()
		table_type = p.parse_type()
		p.check_sql_keyword('where') or { return ast.SqlStmtLine{} }
		where_expr = p.expr(0)

		where_expr_result := p.check_sql_where_expr_has_no_undefined_variables(&where_expr, [])
		if where_expr_result is ast.NodeError {
			return ast.SqlStmtLine{}
		}
	}
	end_comments := p.eat_comments()
	return ast.SqlStmtLine{
		table_expr:       ast.TypeNode{
			typ: table_type
			pos: table_pos
		}
		object_var:       inserted_var
		pos:              pos
		updated_columns:  updated_columns
		update_exprs:     update_exprs
		update_data_expr: update_data_expr
		kind:             kind
		where_expr:       where_expr
		is_generated:     false
		is_dynamic:       is_dynamic
		scope:            p.scope
		pre_comments:     pre_comments
		end_comments:     end_comments
	}
}

fn (p &Parser) is_sql_query_data_expr() bool {
	if p.tok.kind != .lcbr {
		return false
	}
	mut idx := 1
	for p.peek_token(idx).kind == .comment {
		idx++
	}
	first := p.peek_token(idx)
	if first.kind == .key_if {
		return true
	}
	if first.kind != .name {
		return false
	}
	idx++
	for p.peek_token(idx).kind == .dot && p.peek_token(idx + 1).kind == .name {
		idx += 2
	}
	return is_sql_query_data_operator(p.peek_token(idx).kind)
}

fn is_sql_query_data_operator(kind token.Kind) bool {
	return kind in [.eq, .ne, .gt, .lt, .ge, .le, .key_like, .key_ilike, .key_in, .not_in, .key_is,
		.not_is]
}

fn (mut p Parser) sql_query_data_expr() ast.Expr {
	start_pos := p.tok.pos()
	items := p.parse_sql_query_data_items_block()
	return ast.SqlQueryDataExpr{
		pos:   start_pos.extend(p.prev_tok.pos())
		items: items
	}
}

fn (mut p Parser) parse_sql_query_data_items_block() []ast.SqlQueryDataItem {
	p.check(.lcbr)
	mut items := []ast.SqlQueryDataItem{}
	for {
		p.eat_comments()
		if p.tok.kind == .rcbr {
			break
		}
		if p.tok.kind == .eof {
			p.unexpected(got: 'eof, while parsing a dynamic ORM expression')
			break
		}
		items << p.parse_sql_query_data_item()
		p.eat_comments()
		if p.tok.kind == .comma {
			p.next()
			continue
		}
		if p.tok.kind != .rcbr {
			p.error('expected `,` or `}` in a dynamic ORM expression')
			break
		}
	}
	p.check(.rcbr)
	return items
}

fn (mut p Parser) parse_sql_query_data_item() ast.SqlQueryDataItem {
	if p.tok.kind == .key_if {
		return p.parse_sql_query_data_if_item()
	}
	expr := p.expr(0)
	return ast.SqlQueryDataLeaf{
		pos:  expr.pos()
		expr: expr
	}
}

fn (mut p Parser) parse_sql_query_data_if_item() ast.SqlQueryDataItem {
	start_pos := p.tok.pos()
	mut branches := []ast.SqlQueryDataBranch{}
	mut has_else := false
	for {
		branch_pos := p.tok.pos()
		p.check(.key_if)
		cond := p.expr(0)
		items := p.parse_sql_query_data_items_block()
		branches << ast.SqlQueryDataBranch{
			pos:   branch_pos.extend(p.prev_tok.pos())
			cond:  cond
			items: items
		}
		p.eat_comments()
		if p.tok.kind != .key_else {
			break
		}
		has_else = true
		else_pos := p.tok.pos()
		p.next()
		if p.tok.kind == .key_if {
			continue
		}
		else_items := p.parse_sql_query_data_items_block()
		branches << ast.SqlQueryDataBranch{
			pos:   else_pos.extend(p.prev_tok.pos())
			cond:  ast.empty_expr
			items: else_items
		}
		break
	}
	return ast.SqlQueryDataIf{
		pos:      start_pos.extend(p.prev_tok.pos())
		branches: branches
		has_else: has_else
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

		left_check_result := p.check_sql_where_expr_has_no_undefined_variables(expr.left, [])

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
