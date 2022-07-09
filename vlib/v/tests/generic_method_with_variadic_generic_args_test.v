struct Foo<T> {
mut:
	arr []T
}

fn (mut foo Foo<T>) push<T>(items ...T) {
	for item in items {
		foo.arr << item
	}
}

fn test_generic_method_with_variadic_args() {
	mut f := Foo<int>{}
	f.push(1, 2, 3, 5, 8, 13, 21, 34, 55)
	println(f.arr)
	assert f.arr == [1, 2, 3, 5, 8, 13, 21, 34, 55]
}
