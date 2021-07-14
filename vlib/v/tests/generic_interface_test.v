interface Gettable<T> {
	get() T
}

struct Animal<T> {
	metadata T
}

fn (a Animal<T>) get<T>() T {
	return a.metadata
}

// different struct implementing the same interface:
struct Mineral<T> {
	value T
}

fn (m Mineral<T>) get<T>() T {
	return m.value
}

////

fn extract<T>(xs []Gettable<T>) []T {
	return xs.map(it.get())
}

fn extract_basic<T>(xs Gettable<T>) T {
	return xs.get()
}

fn test_extract() {
	a := Animal<int>{123}
	b := Animal<int>{456}
	c := Mineral<int>{789}

	arr := [Gettable<int>(a), Gettable<int>(b), Gettable<int>(c)]
	assert typeof(arr).name == '[]Gettable<int>'

	x := extract<int>(arr)
	assert x == [123, 456, 789]
}

// fn test_extract_multiple_instance_types() {
// 	a := Animal<string>{'123'}
// 	b := Animal<string>{'456'}
// 	c := Mineral<string>{'789'}

// 	arr := [Gettable<string>(a), Gettable<string>(b), Gettable<string>(c)]
// 	assert typeof(arr).name == '[]Gettable<string>'

// 	x := extract<string>(arr)
// 	assert x == ['123', '456', '789']
// }

fn test_extract_basic() {
	a := Animal<int>{123}
	b := Animal<int>{456}
	c := Mineral<int>{789}

	aa := extract_basic(a)
	bb := extract_basic(b)
	cc := extract_basic(c)
	assert '$aa | $bb | $cc' == '123 | 456 | 789'
}
