enum Color {
	red
	blue
	green
}

fn enum_optional_helper(b bool) ?Color {
	if b {
		return .red
	}
	return error('failed')
}

fn test_enum_optional() {
	a := enum_optional_helper(true) or {
		assert false
		return
	}
	assert a == .red
}

fn test_enum() {
	assert Color.red == .red
	assert Color.blue == .blue
	assert Color.green == .green
	assert Color.red != .blue
	assert Color.red != .green
	assert Color.blue != .green
	mut color := Color.red
	assert color == .red
	color = .green
	assert color == .green
}

fn test_in() {
	color := Color.red
	num := 3	// used to be an expr bug before `in`
	assert color in [.red, .green]
	assert num == 3
	println(color)
	assert true
}

fn test_match() {
	color := Color.green
	num := 3
	match color {
		.red {
			assert false
		}
		.green {
			assert true
		}
		else {
			assert false
		}
	}
	println(color)
	assert num == 3
}

enum Foo {
	a = 1
	b = 2
	c = 3
	d = -10
}

fn test_nums() {
	foo := Foo.a
	assert foo == Foo(1)
	assert Foo.c == Foo(3)
	d := Foo.d
	assert d == Foo(-10)
}

/*
enum Expr {
	BoolExpr(bool)
	IntExpr(int)
	//FloatExpr(int)
}

fn get_expr() Expr {
	return Expr.IntExpr(10)

}

fn test_typed_enum() {
	i := Expr.IntExpr(10)
	expr := Expr.BoolExpr(true)
	//println(i)
	//if i == expr {

	//}
	println('done')
	// expr = i
	/*
	match expr {
		IntExpr(n)  { println('INT $n')  }
		BoolExpr(b) { println('BOOL $b') }
	}
	*/
}
*/
/*
fn test_typed_enum() {
	Expr i = { .obj = 10, .typ = IntExpr_type };
	Expr expr = { .obj = true, .typ = BoolExpr_type };
	//    val = expr;
	if (expr.typ == IntExpr_type) {
		int n = (int)expr.obj;
		println('INT $n');
	}
	else if (expr.typ == BoolExpr_type) {
		int b = (bool)expr.obj;
		println('BOOL $b');
	}
}
*/
