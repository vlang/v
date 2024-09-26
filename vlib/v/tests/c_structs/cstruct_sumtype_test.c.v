#include "@VMODROOT/cstruct.h"

const the_string = 'the string'

@[typedef]
struct C.Foo {
	a int
}

struct C.Bar {
	a int
}

type Baz = C.Bar | C.Foo

fn test_cstruct() {
	a := Baz(C.Foo{
		a: 1000
	})
	dump(a)
	assert (a as C.Foo).a == 1000
}
