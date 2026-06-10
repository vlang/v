module common

import api

pub struct Type_a {
pub:
	name string
}

pub struct Type_b {
pub:
	age int
}

pub fn call_api() {
	a := Type_a{
		name: 'hello'
	}
	b := Type_b{
		age: 42
	}
	_ = api.make_struct(a)
	_ = api.make_struct(b)
}
