struct Pos {
	x int
	y int
}

struct Ident {
	pos  Pos
	name string
}

struct EmptyExpr {
	dummy u8
}

type Expr = EmptyExpr | Ident

const empty_expr = Expr(EmptyExpr{})

fn (e Expr) get_pos() Pos {
	return match e {
		Ident { e.pos }
		EmptyExpr { Pos{} }
	}
}

fn parse_expr() Expr {
	return Expr(Ident{
		pos:  Pos{
			x: 42
			y: 99
		}
		name: 'macos'
	})
}

struct Parser {
mut:
	tok      int
	comptime bool
}

fn (mut p Parser) expr_or_type() Expr {
	return Expr(Ident{
		pos:  Pos{
			x: 77
			y: 88
		}
		name: 'linux'
	})
}

fn (mut p Parser) if_expr(is_comptime bool) {
	// Exact pattern from the real parser
	mut cond := if p.tok == 42 {
		empty_expr // global constant
	} else {
		if is_comptime { p.expr_or_type() } else { p.expr_or_type() }
	}
	// This line is what crashes in v3
	guard_pos := cond.get_pos()
	println(guard_pos.x.str())
	println(guard_pos.y.str())
}

fn main() {
	mut parser := Parser{
		tok:      99
		comptime: true
	}
	parser.if_expr(true) // should print 77, 88
}
