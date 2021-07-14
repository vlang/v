interface Gettable<T> {
	get() T
}

struct Animal<T> {
	metadata T
}

fn (a Animal<T>) get<T>() T {
	return a.metadata
}

fn extract<T>(xs []Gettable<T>) []T {
	return xs.map(it.get())
}

fn extract_basic<T>(xs Gettable<T>) T {
	return xs.get()
}

fn test_extract() {
	a := Animal<int>{123}
	b := Animal<int>{456}

	arr := [Gettable<int>(a), Gettable<int>(b)]
	assert typeof(arr).name == '[]Gettable<int>'

	x := extract<int>(arr)
	assert x == [123, 456]
}

fn test_extract_basic() {
	a := Animal<int>{123}
	b := Animal<int>{456}

	aa := extract_basic(a)
	bb := extract_basic(b)
	assert '$aa | $bb' == '123 | 456'
}
