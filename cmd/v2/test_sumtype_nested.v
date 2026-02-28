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

fn parse_a() Expr {
	return Expr(Ident{
		pos:  Pos{
			x: 42
			y: 99
		}
		name: 'aaa'
	})
}

fn parse_b() Expr {
	return Expr(Ident{
		pos:  Pos{
			x: 10
			y: 20
		}
		name: 'bbb'
	})
}

fn test_nested(is_lcbr bool, is_comptime bool) {
	// This mirrors the parser pattern exactly
	mut cond := if is_lcbr {
		Expr(EmptyExpr{})
	} else {
		if is_comptime { parse_a() } else { parse_b() }
	}
	p := cond.get_pos()
	println(p.x.str())
	println(p.y.str())
}

fn main() {
	println('case 1: lcbr=false comptime=true')
	test_nested(false, true) // should print 42, 99
	println('case 2: lcbr=false comptime=false')
	test_nested(false, false) // should print 10, 20
	println('case 3: lcbr=true comptime=true')
	test_nested(true, true) // should print 0, 0
}
