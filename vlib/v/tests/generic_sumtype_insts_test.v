struct Foo[T] {
	x T
}

struct Bar[T] {
	x T
}

type MyType[T] = Bar[T] | Foo[T]

fn test_generic_sumtype_insts() {
	f := Foo[string]{'hi'}
	t := MyType[string](f)
	println(t.type_name())
	assert t.type_name() == 'Foo[string]'
}
