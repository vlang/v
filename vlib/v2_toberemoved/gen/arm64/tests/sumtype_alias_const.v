module main

type EmptyExpr = u8

struct Node {
	value int
}

type Expr = EmptyExpr | Node

const empty_expr = Expr(EmptyExpr(0))

fn main() {
	if empty_expr is EmptyExpr {
		println('ok')
		return
	}
	println('bad')
}
