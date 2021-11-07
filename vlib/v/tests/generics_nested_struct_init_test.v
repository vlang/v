struct Foo<T> {
	foo T
}

struct Bar<T> {
mut:
	foos []Foo<T>
}

fn (mut b Bar<T>) add(v T) {
	b.foos << Foo<T>{
		foo: v
	}
}

fn test_nested_generics_struct_init() {
	mut bar := Bar<string>{}
	bar.add('bar')
	println(bar)

	result := '$bar'
	assert result.contains('Bar<string>{')
	assert result.contains('foos: [Foo<string>{')
	assert result.contains("foo: 'bar'")
}
