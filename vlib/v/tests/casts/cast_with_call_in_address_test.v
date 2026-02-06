@[heap]
struct Foo {
mut:
	val int
}

@[heap]
struct Bar {
mut:
	val int
}

interface FooBar {
mut:
	val int
}

fn test_main() {
	mut fbs := []&FooBar{}
	fbs << &Foo{1}
	a := &(fbs[0] as Foo)
	println(a)
	b := &(fbs.last() as Foo)
	println(b)
	arr1 := [(fbs.last() as Foo)]
	arr2 := [&(fbs.last() as Foo)]
	arr3 := [&(get_foo_bar() as Foo)]
	println(arr1)
	println(arr2)
	println(arr3)
	println(&(fbs.last() as Foo))
	assert arr2[0] == arr3[0]
}

fn get_foo_bar() FooBar {
	return Foo{1}
}
