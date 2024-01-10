struct Foo1 {
	x int
}

struct Foo2 {
	Foo1
}

struct Foo3 {
	Foo2
}

fn (f Foo1) bar() string {
	println('Foo1.bar()')
	return 'Foo1.bar()'
}

fn test_nested_struct_embed_method_call() {
	f3 := Foo3{}
	ret := f3.bar()
	assert ret == 'Foo1.bar()'
}
