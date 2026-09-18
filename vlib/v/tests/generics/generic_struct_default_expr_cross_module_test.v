@[has_globals]
module main

import v.tests.generics.generic_default_modules.domainmain

__global global_plain_box PlainBox[int]

struct GenericBox[T] {
mut:
	x    int = 5
	size int = sizeof(T)
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

struct Local {
	x int = 11
}

fn test_flattened_generic_defaults_use_main_source_under_imported_collision() {
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

fn test_imported_generic_default_uses_caller_local_type() {
	box := domainmain.Box[Local]{}
	assert box.value.x == 11
}

fn test_generic_channel_default_expr_uses_concrete_element_type() {
	mut local := GenericBox[string]{}
	local.ch <- 'abc'
	received := <-local.ch
	assert received == 'abc'
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

fn test_recovered_generic_default_is_expr_uses_concrete_generic_argument() {
	assert domainmain.ShapeHolder[domainmain.Square]{}.matches
	assert !domainmain.ShapeHolder[domainmain.Circle]{}.matches
	assert domainmain.SumHolder[int]{}.matches
	assert !domainmain.SumHolder[string]{}.matches
}

fn test_recovered_generic_default_as_expr_uses_concrete_generic_argument() {
	assert domainmain.AsHolder[int]{}.value == 5
}
