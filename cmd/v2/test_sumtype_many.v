struct Pos {
	x int
	y int
}

struct V0 {
	pos Pos
	a   int
}

struct V1 {
	pos Pos
	b   int
}

struct V2 {
	pos Pos
	c   int
}

struct V3 {
	pos Pos
	d   int
}

struct V4 {
	pos Pos
	e   int
}

struct V5 {
	pos Pos
	f   int
}

struct V6 {
	pos Pos
	g   int
}

struct V7 {
	pos Pos
	h   int
}

struct V8 {
	dummy u8
} // EmptyExpr equivalent (tag 8)

struct V9 {
	pos Pos
	j   int
}

struct V10 {
	pos Pos
	k   int
}

struct V11 {
	pos Pos
	l   int
}

struct V12 {
	pos Pos
	m   int
}

struct V13 {
	pos  Pos
	name string
} // Ident equivalent (tag 13)

struct V14 {
	pos Pos
	n   int
}

type Expr = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | V11 | V12 | V13 | V14

fn (e Expr) get_pos() Pos {
	return match e {
		V0 { e.pos }
		V1 { e.pos }
		V2 { e.pos }
		V3 { e.pos }
		V4 { e.pos }
		V5 { e.pos }
		V6 { e.pos }
		V7 { e.pos }
		V8 { Pos{} }
		V9 { e.pos }
		V10 { e.pos }
		V11 { e.pos }
		V12 { e.pos }
		V13 { e.pos }
		V14 { e.pos }
	}
}

fn make_v13() Expr {
	return Expr(V13{
		pos:  Pos{
			x: 42
			y: 99
		}
		name: 'hello'
	})
}

fn test_it(is_lcbr bool, is_comptime bool) {
	mut cond := if is_lcbr {
		Expr(V8{})
	} else {
		if is_comptime { make_v13() } else { make_v13() }
	}
	p := cond.get_pos()
	println(p.x.str())
	println(p.y.str())
}

fn main() {
	println('test with 15 variants, accessing V13 (tag 13):')
	test_it(false, true)
}
