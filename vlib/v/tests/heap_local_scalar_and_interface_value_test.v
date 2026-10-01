// Locals moved to the heap because their address escapes must still lower to valid C
// when the local is a generic scalar or an interface value.

fn fill_first_byte(ptr voidptr, size int) !int {
	unsafe {
		*(&u8(ptr)) = 65
	}
	return size
}

// The error of a Result call may keep `&t`, so `t` is moved to the heap here, as in
// `os.File.read_raw`; for a scalar `T`, `T{}` is a literal like `0`, which has no address.
fn read_value[T]() !T {
	size := int(sizeof(T))
	mut t := T{}
	nbytes := fill_first_byte(&t, size)!
	if nbytes != size {
		return error_with_code('incomplete read', nbytes)
	}
	return t
}

fn test_generic_scalar_local_moved_by_a_result_call() {
	assert read_value[u8]()! == 65
	assert read_value[u32]()! == 65
}

// A generic `T{}` lowers to a scalar literal for `T = u8`, which has no address.
fn escaping_zero[T]() &T {
	mut t := T{}
	p := &t
	return p
}

struct Point {
	x int
}

fn test_escaping_generic_scalar_zero_value() {
	b := escaping_zero[u8]()
	assert *b == 0
	unsafe {
		*b = 7
	}
	assert *b == 7
	pt := escaping_zero[Point]()
	assert pt.x == 0
}

interface Errer {
	err() IError
	name() string
}

struct Ok {}

fn (o Ok) err() IError {
	return none
}

fn (o Ok) name() string {
	return 'ok'
}

fn errer_name(e Errer) string {
	return e.name()
}

// `ctx.err()` may return memory reachable from its receiver and is returned, so `ctx`
// is moved to the heap; passing it on by value must read it through one dereference.
fn heaped_interface_value(src Errer) !string {
	ctx := src
	e := ctx.err()
	if e !is none {
		return e
	}
	copy := ctx
	return errer_name(ctx) + errer_name(copy)
}

fn test_heaped_interface_local_is_passed_by_value() {
	assert heaped_interface_value(Ok{})! == 'okok'
}

interface Holder {
	get() string
}

struct H {}

fn (h H) get() string {
	return 'holder'
}

// Locals are moved to the heap by name, so the second `v` follows the first one.
fn heaped_interface_after_sibling(h0 Holder, src Errer) string {
	if h0 is H {
		v := h0 as Holder
		return v.get()
	}
	v := src
	return errer_name(v)
}

fn test_heaped_interface_local_after_a_sibling_declaration() {
	assert heaped_interface_after_sibling(Holder(H{}), Ok{}) == 'holder'
	assert heaped_interface_after_sibling(unsafe { Holder(nil) }, Ok{}) == 'ok'
}
