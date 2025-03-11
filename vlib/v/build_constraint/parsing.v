module build_constraint

// parsing:
struct BParser {
	tokens []Token
mut:
	pos int
}

fn (mut p BParser) peek(n int) Token {
	if p.pos + n >= p.tokens.len {
		return Token{
			kind: .teof
		}
	}
	t := p.tokens[p.pos + n]
	return t
}

fn (mut p BParser) next() {
	p.pos++
}

fn (mut p BParser) parse() !BExpr {
	return p.expr()
}

fn (mut p BParser) expr() !BExpr {
	return BExpr{
		expr: p.or_expr()!
	}
}

fn (mut p BParser) or_expr() !BOr {
	mut exprs := []BAnd{}
	exprs << p.and_expr()!
	for t := p.peek(0); t.kind == .tor; t = p.peek(0) {
		p.next()
		exprs << p.and_expr()!
	}
	return BOr{
		exprs: exprs
	}
}

fn (mut p BParser) and_expr() !BAnd {
	mut exprs := []BUnary{}
	exprs << p.unary_expr()!
	for t := p.peek(0); t.kind == .tand; t = p.peek(0) {
		p.next()
		exprs << p.unary_expr()!
	}
	return BAnd{
		exprs: exprs
	}
}

fn (mut p BParser) unary_expr() !BUnary {
	t := p.peek(0)
	match t.kind {
		.tfact {
			p.next()
			return BUnary(BFact(t.value))
		}
		.tdefine {
			p.next()
			return BUnary(BDefine(t.value))
		}
		.tnot {
			p.next()
			nt := p.peek(0)
			if nt.kind in [.tfact, .tdefine] {
				ident := p.unary_expr()!
				return BNot{
					expr: ident
				}
			}
			expr := p.expr()!
			return BNot{
				expr: expr
			}
		}
		.tparen_open {
			p.next()
			expr := p.expr()!
			if p.peek(0).kind != .tparen_close {
				return error('expected closing )')
			}
			p.next()
			return BUnary(expr)
		}
		else {}
	}
	return error('unary failed, unexpected ${t}')
}
