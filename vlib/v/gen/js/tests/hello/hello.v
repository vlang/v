module hello

import v.gen.js.tests.hello.hello1

pub const (
	hello = 'Hello'
)

pub struct Aaa {
pub mut:
	foo string
}

pub fn (mut a Aaa) update(s string) {
	a.foo = s
}

struct Bbb {}

pub enum Ccc {
	a
	b = 5
	c
}

pub fn debugger() string {
	v := Bbb{}
	return hello.hello
}

pub fn excited() string {
	return '$hello1.nested() $debugger()!'
}
