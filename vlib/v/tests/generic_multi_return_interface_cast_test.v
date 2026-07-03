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

// Regression test: generic multi-return with mut param whose concrete type
// is itself a pointer (T = &Impl). The synthetic PrefixExpr should not
// trigger the extra deref for mut generic pointer params.
struct Impl {
	x int
}

fn (i Impl) f() int {
	return i.x
}

fn get_ptr_multi[T](mut x T) (Getter, int) {
	return x, 0
}

fn test_generic_multi_return_ptr_param_interface_cast() {
	mut i := &Impl{42}
	g, n := get_ptr_multi[&Impl](mut i)
	assert g.f() == 42
	assert n == 0
}

// Regression test: generic multi-return with `as T` cast then interface cast.
// Tests both value-interface and &Interface pointer codegen paths.
// Two concrete types with DIFFERENT struct layouts ensure the C compiler
// catches any stale-type codegen.

interface Gettable {
	get() int
}

struct TypeA {
	x int
	y int
}

fn (a TypeA) get() int {
	return a.x + a.y
}

struct TypeB {
	msg string
}

fn (b TypeB) get() int {
	return b.msg.len
}

struct Holder {
	ptr &Gettable
}

fn (h Holder) inner_ptr() &Gettable {
	return h.ptr
}

// Value interface path: Gettable stored directly, `as T` cast, multi-return
@[heap]
struct Pool[T] {
mut:
	raw Gettable
}

fn (mut p Pool[T]) acquire() (Gettable, int) {
	v := p.raw as T
	return v, 42
}

// &Interface pointer path: Holder stores &Gettable, extracted via inner_ptr()
struct Wrapper[T] {
	src Holder
}

fn (w Wrapper[T]) extract() (Gettable, &Gettable) {
	raw_conn := w.src.inner_ptr()
	raw := raw_conn as T
	return raw, raw_conn
}

fn test_generic_as_cast_multi_return_interface() {
	// Value interface path
	mut p1 := Pool[TypeA]{
		raw: TypeA{10, 20}
	}
	v1, _ := p1.acquire()
	assert v1.get() == 30

	mut p2 := Pool[TypeB]{
		raw: TypeB{'hi'}
	}
	v2, _ := p2.acquire()
	assert v2.get() == 2

	// &Interface pointer path
	w1 := Wrapper[TypeA]{
		src: Holder{&TypeA{10, 20}}
	}
	g1, _ := w1.extract()
	assert g1.get() == 30

	w2 := Wrapper[TypeB]{
		src: Holder{&TypeB{'hi'}}
	}
	g2, _ := w2.extract()
	assert g2.get() == 2
}
