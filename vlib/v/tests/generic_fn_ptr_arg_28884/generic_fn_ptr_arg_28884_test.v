@[has_globals]
module main

import generics28884 { Box, Foo }

struct Baz {
	n int
}

type Bar = fn (&Baz) int

__global alias_global generics28884.Foo[Bar]
__global inline_global generics28884.Foo[fn (&Baz) int]

struct Holder {
mut:
	f generics28884.Foo[fn (&Baz) int]
}

fn double(b &Baz) int {
	return b.n * 2
}

fn triple(b &Baz) int {
	return b.n * 3
}

fn make_baz() &Baz {
	return &Baz{9}
}

fn call_stored(mut f generics28884.Foo[fn (&Baz) int], n int) int {
	return f.value(&Baz{n})
}

// https://github.com/vlang/v/issues/28884
fn test_global_generic_container_of_fn_alias() {
	alias_global.set(double)
	alias_global.set(alias_global.value)
	assert alias_global.value(&Baz{21}) == 42
	mut local := generics28884.Foo[Bar]{}
	local.set(alias_global.value)
	assert local.value(&Baz{5}) == 10
}

fn test_global_generic_container_of_inline_fn_type() {
	inline_global.set(triple)
	inline_global.set(inline_global.value)
	assert inline_global.value(&Baz{4}) == 12
	stored := inline_global.value
	assert stored(&Baz{5}) == 15
}

fn test_local_generic_container_of_inline_fn_type() {
	mut local := generics28884.Foo[fn (&Baz) int]{}
	local.set(double)
	local.set(local.value)
	assert local.value(&Baz{6}) == 12
	mut holder := Holder{}
	holder.f.set(triple)
	assert holder.f.value(&Baz{7}) == 21
	assert call_stored(mut holder.f, 2) == 6
	mut named := Foo[fn (b &Baz) int]{}
	named.set(double)
	assert named.value(&Baz{8}) == 16
	mut returning := Foo[fn () &Baz]{}
	returning.set(make_baz)
	assert returning.value().n == 9
}

fn test_generic_args_with_nested_pointer_types() {
	b := &Baz{3}
	mut arr := Foo[[]&Baz]{}
	arr.set([b])
	assert arr.value[0].n == 3
	mut opt := Foo[?&Baz]{}
	opt.set(b)
	got := opt.value or { &Baz{0} }
	assert got.n == 3
	mut ptr_ptr := Foo[&&Baz]{}
	ptr_ptr.set(&b)
	assert (**ptr_ptr.value).n == 3
	mut boxed := Foo[Box[&Baz]]{}
	boxed.set(Box[&Baz]{b})
	assert boxed.value.v.n == 3
	mut boxed_fn := Foo[Box[fn (&Baz) int]]{}
	boxed_fn.set(Box[fn (&Baz) int]{double})
	assert boxed_fn.value.v(b) == 6
}
