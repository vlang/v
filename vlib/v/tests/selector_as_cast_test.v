struct Foo {
	expr SumType
}

struct Bar {
	expr SumType
}

type SumType = Foo | string | Bar
type SumType2 = SumType | int

struct Gen {}

fn (g Gen) t(arg SumType2) {
}

fn test_main() {
	gen := Gen{}
	s := Bar{
		expr: Foo{
			expr: 'foobar'
		}
	}
	gen.t((s.expr as Foo).expr)
	assert true
}
