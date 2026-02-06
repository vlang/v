fn t() int {
	return 123
}

fn r(a struct { name string age int }) {
	assert '${a}' == "struct {
    name: 'Foo'
    age: 123
}"
}

fn test_main() {
	mut a := struct {
		name: 'Foo'
		age:  t()
	}
	dump(a)
	r(a)
	r(struct { name: 'Foo', age: t() })
	a.age = 2
	assert '${a}' == "struct {
    name: 'Foo'
    age: 2
}"
}
