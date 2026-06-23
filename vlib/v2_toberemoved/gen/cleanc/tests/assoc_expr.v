module main

struct Foo {
	a int
	b int
}

fn main() {
	x := Foo{
		a: 1
		b: 2
	}

	// AssocExpr lowering: Foo{...x b: 3}
	y := Foo{
		...x
		b: 3
	}
	println(y.a)
	println(y.b)

	// &AssocExpr lowering: &Foo{...x a: 9}
	p := &Foo{
		...x
		a: 9
	}
	println(p.a)
	println(p.b)
}
