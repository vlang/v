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
	mut bar1 := Bar<string>{}
	bar1.add('bar')
	println(bar1)

	result1 := '${bar1}'
	assert result1.contains('Bar<string>{')
	assert result1.contains('foos: [Foo<string>{')
	assert result1.contains("foo: 'bar'")

	mut bar2 := Bar<int>{}
	bar2.add(22)
	println(bar2)

	result2 := '${bar2}'
	assert result2.contains('Bar<int>{')
	assert result2.contains('foos: [Foo<int>{')
	assert result2.contains('foo: 22')

	mut bar3 := Bar<f64>{}
	bar3.add(2.2)
	println(bar3)

	result3 := '${bar3}'
	assert result3.contains('Bar<f64>{')
	assert result3.contains('foos: [Foo<f64>{')
	assert result3.contains('foo: 2.2')

	mut bar4 := Bar<bool>{}
	bar4.add(true)
	println(bar4)

	result4 := '${bar4}'
	assert result4.contains('Bar<bool>{')
	assert result4.contains('foos: [Foo<bool>{')
	assert result4.contains('foo: true')
}
