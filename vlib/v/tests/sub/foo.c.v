module sub

import sub.foo.c as _ // imported only for the C declarations

@[typedef]
pub struct C.sub_foo {
	a int
}

pub type Foo = C.sub_foo
