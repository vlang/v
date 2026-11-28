module main

import inner32402 as inner

// Regression test for https://github.com/vlang/v/issues/32402
//
// An alias that has the same short name as the struct it aliases (`type Ctx =
// inner.Ctx`) made the checker take the struct's generic method for the alias'
// own, so the receiver was cast to the alias and C got a temporary copy of it:
// `inner32402__Ctx_int__put(&((inner32402__Ctx[]){*((inner32402__Ctx*)(c))})[0], 2, 5)`.
// What the method wrote was lost.

type Ctx = inner.Ctx

type Other = inner.Ctx

fn put_through_param(mut c Ctx) {
	c.put(2, 'x')
}

fn test_generic_mut_method_on_a_local() {
	mut c := Ctx{}
	c.put(1, 5)
	assert c.n == 1
}

fn test_generic_mut_method_through_a_mut_param() {
	mut c := Ctx{}
	put_through_param(mut c)
	assert c.n == 2
}

fn test_non_generic_mut_method() {
	mut c := Ctx{}
	c.bump()
	assert c.n == 1
}

fn test_differently_named_alias() {
	mut c := Other{}
	c.put(3, 5)
	assert c.n == 3
}
