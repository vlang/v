import arrays

interface Foo {}

fn test_main() {
	mut arr := []Foo{}
	str := 'abc'
	i := 0
	arr = arrays.concat(arr, str, i)
	assert arr == [Foo('abc'), Foo(0)]
	arr = arrays.concat(arr, Foo(str), Foo(i))
	assert arr == [Foo('abc'), Foo(0), Foo('abc'), Foo(0)]
}
