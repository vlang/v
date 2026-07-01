module main

// Regression test: generic method with `&Interface → as T` cast and
// multi-return interface conversion. Two concrete types with DIFFERENT
// struct layouts are used to ensure the C compiler catches any stale-type
// codegen where the wrong type's interface conversion is emitted.

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

fn (h Holder) inner() &Gettable {
	return h.ptr
}

struct Container[T] {
	src Holder
}

fn (c Container[T]) extract() (Gettable, &Gettable) {
	raw_conn := c.src.inner() // &Gettable
	raw := raw_conn as T // cast &Gettable → concrete T
	return raw, raw_conn // T returned as Gettable
}

const _inst_a = Container[TypeA]{
	src: Holder{&TypeA{1, 2}}
}
const _inst_b = Container[TypeB]{
	src: Holder{&TypeB{'hello'}}
}

fn test_container_a() {
	c := Container[TypeA]{
		src: Holder{&TypeA{10, 20}}
	}
	v, _ := c.extract()
	assert v.get() == 30
}

fn test_container_b() {
	c := Container[TypeB]{
		src: Holder{&TypeB{'hi'}}
	}
	v, _ := c.extract()
	assert v.get() == 2
}
