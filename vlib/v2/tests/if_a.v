module main

type Expr = Ident | SelectorExpr

struct Ident {
	name string
}

struct SelectorExpr {
	lhs Expr
	rhs Expr
	// op  int
}

fn main() {
	a := Ident{
		name: 'foo'
	}
	node := Expr(a)
	if a.name == 'bar' {
	} else if node !in [Ident, SelectorExpr] {
	}
}
