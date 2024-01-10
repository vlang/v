struct Bar {
	anon struct {
		foofoo Foo = Foo{'foofoo'}
	}
}

struct Foo {
	name string
}

fn test_anon_struct_with_default_expr() {
	bar := Bar{}
	println(bar.anon.foofoo.name)
	assert bar.anon.foofoo.name == 'foofoo'
}
