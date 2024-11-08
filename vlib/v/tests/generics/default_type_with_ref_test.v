struct Dec {}

type Foo = Dec

type Bar = Dec | int

type Baz = int

fn (d Dec) a[T]() T {
	return T{}
}

fn test_main() {
	dec := Dec{}
	dec.a[&int]()
	dec.a[&Dec]()
	dec.a[&Bar]()
	dec.a[&Baz]()
}
