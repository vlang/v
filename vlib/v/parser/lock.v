module parser

import v.ast
import v.table

fn (mut p Parser) lock_expr() ast.LockExpr {
	// TODO Handle aliasing sync
	p.register_auto_import('sync')
	mut pos := p.tok.position()
	mut lockeds := []ast.Ident{}
	mut is_rlocked := []bool{}
	outer: for {
		if p.tok.kind == .lcbr {
			break
		}
		is_rlock := p.tok.kind == .key_rlock
		if !is_rlock && p.tok.kind != .key_lock {
			p.error_with_pos('unexpected $p.tok, expected `lock` or `rlock`', p.tok.position())
		}
		p.next()
		for p.tok.kind == .name {
			lockeds << ast.Ident{
				language: table.Language.v
				pos: p.tok.position()
				mod: p.mod
				name: p.tok.lit
				is_mut: true
				info: ast.IdentVar{}
				scope: p.scope
			}
			is_rlocked << is_rlock
			p.next()
			if p.tok.kind == .lcbr {
				break outer
			}
			if p.tok.kind == .semicolon {
				p.next()
				break
			}
			p.check(.comma)
		}
	}
	stmts := p.parse_block()
	pos.update_last_line(p.prev_tok.line_nr)
	return ast.LockExpr{
		lockeds: lockeds
		stmts: stmts
		is_rlock: is_rlocked
		pos: pos
	}
}
