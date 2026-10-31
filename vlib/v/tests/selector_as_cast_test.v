struct Foo {
	expr &SumType
}

struct Bar {
	expr &SumType
}

type SumType = Foo | string | Bar
type SumType2 = SumType | int

struct Gen {}

fn (g Gen) t(arg SumType2) {
}

fn test_main() {
	gen := Gen{}
	text := SumType('foobar')
	foo := SumType(Foo{ expr: &text })
	s := Bar{ expr: &foo }
	gen.t((s.expr as Foo).expr)
	assert true
}
