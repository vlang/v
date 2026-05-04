struct Ident {
	name string
	line int
	col  int
}

struct Literal {
	value string
}

type Expr = Ident | Literal

fn make_ident() Ident {
	return Ident{
		name: 'foo'
		line: 42
		col:  10
	}
}

fn wrap_expr() Expr {
	i := make_ident()
	return i
}

fn main() {
	e := wrap_expr()
	match e {
		Ident {
			println(e.name)
			println(e.line)
			println(e.col)
		}
		Literal {
			println(e.value)
		}
	}
}
