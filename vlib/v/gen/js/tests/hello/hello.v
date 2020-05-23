module hello

// TODO: Fix const namespacing, uncomment once it works
/*
pub const (
	hello = 'Hello'
)
*/

pub struct A {
pub mut:
	foo string
}

pub fn (mut a A) update(s string) {
	a.foo = s
}

struct B {}

pub enum C {}

pub fn debugger() string {
	v := B{}
	return 'Hello'
}

pub fn excited() string {
	return debugger() + "!"
}