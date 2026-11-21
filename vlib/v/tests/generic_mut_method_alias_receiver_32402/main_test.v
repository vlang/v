module main

import inner32402

// Regression test for https://github.com/vlang/v/issues/32402
//
// A generic `mut` method called through a cross-module type alias used to
// receive the address of a temporary copy, so its writes were lost.
// `type Ctx = inner32402.Ctx` erases to the parent struct in C; the receiver
// must be passed by address (`&d`), not materialized (`&((T[]){d})[0]`).
type Ctx = inner32402.Ctx

struct SameModInner {
pub mut:
	n int
}

type SameModCtx = SameModInner

fn (mut c SameModInner) set[T](status int, v T) {
	c.n = status
}

fn test_generic_mut_method_through_cross_module_alias() {
	mut d := Ctx{}
	d.set(201, 5)
	assert d.n == 201
}

fn test_generic_mut_method_through_same_module_alias() {
	mut d := SameModCtx{}
	d.set(202, 5)
	assert d.n == 202
}

fn test_non_generic_mut_method_through_cross_module_alias() {
	mut d := Ctx{}
	d.set_plain(203)
	assert d.n == 203
}

fn test_generic_mut_method_direct() {
	mut d := inner32402.Ctx{}
	d.set(204, 5)
	assert d.n == 204
}
