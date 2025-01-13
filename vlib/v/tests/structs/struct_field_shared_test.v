struct Foo {
	bar shared struct {
	mut:
		foo int
		bar int
	}
	baz shared int
}

fn test_main() {
	assert Foo{}.str() == 'Foo{
    bar: struct {
        foo: 0
        bar: 0
    }
    baz: 0
}'
	mut t := Foo{}
	lock t.bar {
		t.bar.foo = 1
		t.bar.bar = 2
	}
	rlock t.bar {
		assert t.bar.foo + t.bar.bar == 3
	}
}
