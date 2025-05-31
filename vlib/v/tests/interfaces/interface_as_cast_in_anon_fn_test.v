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

fn test_interface_as_cast_in_anon_fn() {
	mut fbs := []&FooBar{}
	fbs << &Foo{1}
	do_something := fn [mut fbs] () {
		_ := fbs.last() as Foo // this line works outside of anon fn
	}
	do_something()
	assert true
}
