struct Animal<T> {
	metadata T
}

fn (a Animal<T>) get<T>() T {
	return a.metadata
}

fn extract<T>(x Animal<T>) T {
	return x.get()
}

fn test_generics_with_multi_generic_struct_types() {
	a := Animal<int>{123}
	b := Animal<string>{'456'}

	assert extract<int>(a) == 123
	assert extract<string>(b) == '456'
}
