#include "@VMODROOT/cstruct.h"

struct Foo {
	a int = 3
}

struct C.Bar {
	a int = 3
	b f64
}

struct FooBar {
	foo Foo
	bar C.Bar
}

fn test_main() {
	a := dump(Foo{})
	b := dump(C.Bar{})
	c := dump(FooBar{})

	assert a.a == b.a
	assert b.a == c.bar.a
	assert b.b == c.bar.b
}
