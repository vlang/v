module parser

import v.ast

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
			p.error_with_pos('unexpected $p.tok, expected `lock` or `rlock`', p.tok.position())
		}
		p.next()
		if p.tok.kind == .lcbr {
			break
		}
		if p.tok.kind == .name {
			exprs, comms := p.expr_list()
			for e in exprs {
				if !e.is_lockable() {
					p.error_with_pos('`$e` cannot be locked - only `x` or `x.y` are supported', e.position())
				}
				lockeds << e
				is_rlocked << is_rlock
			}
			comments << comms
		}
		if p.tok.kind == .semicolon {
			p.next()
			continue
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
