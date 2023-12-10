module sub

import sub.foo.c

@[typedef]
pub struct C.sub_foo {
	a int
}

pub type Foo = C.sub_foo

// avoiding compiler warnings: module 'c (sub.foo.c)' is imported but never used
fn bar() {
	_ = c.used_import
}
