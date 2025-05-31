interface IA {
	a int
	fa()
}

interface IB[T] {
	a int
	b T
	fa()
}

struct Foo[T] implements IA, IB[T] {
	a int
	b T
}

fn (foo Foo[T]) fa() {
	println(foo.b)
}

fn test_main() {
	foo := Foo[u8]{
		b: 16
	}
	foo.fa()
	assert true
}
