@[has_globals]
module main

import v.tests.generics.generic_default_modules.domainmain

__global global_plain_box PlainBox[int]
__global global_described domainmain.Described[Local]

struct GenericBox[T] {
mut:
	x    int    = 5
	size int    = sizeof(T)
	ch   chan T = chan T{cap: 1}
}

struct PlainBox[T] {
mut:
	n int = 5
}

struct ArrayBucket[T] {
mut:
	items []T = []T{}
}

struct HoldsGeneric {
mut:
	inner GenericBox[string]
}

struct HoldsPlainGeneric {
mut:
	box PlainBox[int]
}

struct PromotedInner[T] {
mut:
	x    int
	y    int = 6
	size int = sizeof(T)
}

struct PromotedOuter[T] {
	PromotedInner[T]
}

struct PromotedFixedInner[T] {
mut:
	x   int
	arr [2]int = [int(sizeof(T)), 5]!
	y   int    = 6
}

struct PromotedFixedOuter[T] {
	PromotedFixedInner[T]
}

struct Pair[T] {
	v T
	k int = 4
}

struct WrappedPair[T] {
	pair Pair[T] = Pair[T]{
		v: T(3)
	}
	vals []T = [T(1), T(2)]
}

struct Local {
	x int = 11
	y i64
	z i64
}

fn test_generic_defaults_use_main_source_under_imported_collision() {
	imported := domainmain.GenericBox[int]{}
	assert imported.x == 7
	assert imported.size == sizeof(int)

	local := GenericBox[string]{}
	assert local.x == 5
	assert local.size == sizeof(string)

	holder := HoldsGeneric{}
	assert holder.inner.x == 5
	assert holder.inner.size == sizeof(string)
}

fn test_promoted_generic_defaults_keep_remaining_explicit_defaults() {
	o := PromotedOuter[int]{
		x: 1
	}
	assert o.x == 1
	assert o.y == 6
	assert o.size == sizeof(int)
}

fn test_promoted_generic_fixed_array_default_uses_concrete_argument() {
	o := PromotedFixedOuter[u64]{
		x: 1
	}
	assert o.x == 1
	assert o.arr == [8, 5]!
	assert o.y == 6
}

fn test_imported_generic_default_uses_caller_local_type() {
	mut m := map[string]domainmain.Described[Local]{}
	d := m['missing']
	assert d.size == sizeof(Local)
	assert d.name == 'Local'
	assert d.tname == 'Local'
	assert d.n == 3
	assert global_described.size == sizeof(Local)
	assert global_described.name == 'Local'
	assert global_described.n == 3
}

fn test_generic_channel_default_expr_uses_concrete_element_type() {
	mut local := GenericBox[string]{}
	local.ch <- 'abc'
	received := <-local.ch
	assert received == 'abc'
}

fn test_map_zero_value_uses_generic_source_defaults() {
	mut m := map[string]GenericBox[u16]{}
	mut b := m['missing']
	assert b.x == 5
	assert b.size == sizeof(u16)
	b.ch <- u16(65535)
	received := <-b.ch
	assert received == 65535
}

fn test_recovered_generic_default_struct_literal_uses_concrete_argument() {
	mut m := map[string]WrappedPair[f64]{}
	w := m['missing']
	assert w.pair.v == 3.0
	assert w.pair.k == 4
	assert w.vals == [1.0, 2.0]
}

fn test_generic_array_default_expr_uses_concrete_element_type() {
	mut bucket := ArrayBucket[string]{}
	assert bucket.items.element_size == sizeof(string)
	bucket.items << 'abc'
	assert bucket.items[0] == 'abc'
}

fn test_nested_plain_generic_default_uses_source_defaults() {
	holder := HoldsPlainGeneric{}
	assert holder.box.n == 5
}

fn test_global_plain_generic_default_uses_source_defaults() {
	assert global_plain_box.n == 5
}
