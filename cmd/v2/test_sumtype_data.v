struct Pos {
	x int
	y int
}

struct Ident {
	pos  Pos
	name string
}

struct Lit {
	pos Pos
	val int
}

type Expr = Ident | Lit

fn (e Expr) get_pos() Pos {
	return match e {
		Ident { e.pos }
		Lit { e.pos }
	}
}

fn make_ident() Expr {
	return Expr(Ident{
		pos:  Pos{
			x: 10
			y: 20
		}
		name: 'hello'
	})
}

fn main() {
	e := make_ident()
	p := e.get_pos()
	println(p.x.str())
	println(p.y.str())
}
