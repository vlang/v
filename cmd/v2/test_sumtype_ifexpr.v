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

fn test_if_expr(is_lcbr bool) {
	mut cond := if is_lcbr {
		Expr(EmptyExpr{})
	} else {
		parse_expr()
	}
	p := cond.get_pos()
	println(p.x.str())
	println(p.y.str())
}

fn main() {
	test_if_expr(false) // should print 42, 99
	test_if_expr(true) // should print 0, 0
}
