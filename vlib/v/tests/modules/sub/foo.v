module sub

import sub.foo.c

[typedef]
struct C.sub_foo {
	a int
}

pub type Foo = C.sub_foo
