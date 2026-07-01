module main

interface Getter {
	f() int
}

struct Impl1 {
	x int
}

struct Impl2 {
	x int
}

fn (i Impl1) f() int {
	return i.x + 100
}

fn (i Impl2) f() int {
	return i.x + 200
}

struct Container[T] {
	v T
}

fn (c Container[T]) get() (Getter, int) {
	return c.v, 42
}

fn test_generic_multi_return_interface_cast() {
	// Test with Impl1 — should call Impl1.f() which returns x+100
	c1 := Container[Impl1]{
		v: Impl1{10}
	}
	i1, extra1 := c1.get()
	assert i1.f() == 110
	assert extra1 == 42

	// Test with Impl2 — should call Impl2.f() which returns x+200
	c2 := Container[Impl2]{
		v: Impl2{20}
	}
	i2, extra2 := c2.get()
	assert i2.f() == 220
	assert extra2 == 42
}

// Regression test: generic multi-return with mut param and interface cast
// The resolved type from scope needs to be dereferenced when auto-deref is active.
@[heap]
struct HeapStruct {
	x int
}

fn (h HeapStruct) f() int {
	return h.x
}

fn get_mut[T](mut x T) (Getter, int) {
	return x, 42
}

fn test_generic_multi_return_mut_param_interface_cast() {
	mut h := HeapStruct{7}
	g, extra := get_mut[HeapStruct](mut h)
	assert g.f() == 7
	assert extra == 42
}
