module parser

import v.token
import v.ast

// parse `x` or `x.y.z` - no index, no struct literals (`{` starts lock block)
fn (mut p Parser) lockable() ast.Expr {
	mut names := []string{}
	mut positions := []token.Position{}
	mut pos := p.tok.position()
	for {
		if p.tok.kind != .name {
			p.error_with_pos('unexpected `$p.tok.lit` (field/variable name expected)',
				p.tok.position())
		}
		names << p.tok.lit
		positions << pos
		p.next()
		if p.tok.kind != .dot {
			break
		}
		p.next()
		pos.extend(p.tok.position())
	}
	mut expr := ast.Expr(ast.Ident{
		language: ast.Language.v
		pos: positions[0]
		mod: p.mod
		name: names[0]
		is_mut: true
		info: ast.IdentVar{}
		scope: p.scope
	})
	for i := 1; i < names.len; i++ {
		expr = ast.SelectorExpr{
			expr: expr
			field_name: names[i]
			next_token: if i < names.len - 1 { token.Kind.dot } else { p.tok.kind }
			is_mut: true
			pos: positions[i]
			scope: p.scope
		}
	}
	return expr
}

// like `expr_list()` but only lockables are allowed, `{` starts lock block (not struct literal)
fn (mut p Parser) lockable_list() ([]ast.Expr, []ast.Comment) {
	mut exprs := []ast.Expr{}
	mut comments := []ast.Comment{}
	for {
		expr := p.lockable()
		if expr is ast.Comment {
			comments << expr
		} else {
			exprs << expr
			if p.tok.kind != .comma {
				break
			}
			p.next()
		}
	}
	return exprs, comments
}

fn (mut p Parser) lock_expr() ast.LockExpr {
	// TODO Handle aliasing sync
	p.register_auto_import('sync')
	p.open_scope()
	mut pos := p.tok.position()
	mut lockeds := []ast.Expr{}
	mut comments := []ast.Comment{}
	mut is_rlocked := []bool{}
	for {
		is_rlock := p.tok.kind == .key_rlock
		if !is_rlock && p.tok.kind != .key_lock {
			p.error_with_pos('unexpected `$p.tok`, expected `lock` or `rlock`', p.tok.position())
		}
		p.next()
		if p.tok.kind == .lcbr {
			break
		}
		if p.tok.kind == .name {
			exprs, comms := p.lockable_list()
			for e in exprs {
				if !e.is_lockable() {
					p.error_with_pos('`$e` cannot be locked - only `x` or `x.y` are supported',
						e.position())
				}
				lockeds << e
				is_rlocked << is_rlock
			}
			comments << comms
		}
		if p.tok.kind == .lcbr {
			break
		}
		if p.tok.kind == .semicolon {
			p.next()
		}
	}
	stmts := p.parse_block_no_scope(false)
	scope := p.scope
	p.close_scope()
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.LockExpr{
		lockeds: lockeds
		stmts: stmts
		is_rlock: is_rlocked
		pos: pos
		scope: scope
	}
}
