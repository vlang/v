struct Foo {
	bar struct {
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
}
