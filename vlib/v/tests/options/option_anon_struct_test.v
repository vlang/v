struct Foo {
	a ?struct {
		name string
	}
}

fn test_main() {
	t := Foo{}
	assert '${t}' == 'Foo{
    a: Option(none)
}'

	t2 := Foo{
		a: struct {
			name: 'foo'
		}
	}
	assert '${t2}' == "Foo{
    a: Option(struct {
        name: 'foo'
    })
}"
}
