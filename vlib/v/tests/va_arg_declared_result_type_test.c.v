@[translated]
module main

@[typedef]
struct C.va_list {}

fn C.va_start(voidptr, voidptr)
fn C.va_arg(voidptr, voidptr) voidptr
fn C.va_end(voidptr)

struct Counter {
mut:
	total u64
	calls u32
}

type Callback = fn (voidptr, int) int

fn plus_one(_ voidptr, x int) int {
	return x + 1
}

// C translated by c2v declares `fn C.va_arg(voidptr, voidptr) voidptr`; the value of
// `C.va_arg(T, ap)` still has the type `T`.
@[c2v_variadic]
fn update(op int, ...) {
	ap := C.va_list{}
	C.va_start(ap, op)
	counter := C.va_arg(&Counter, ap)
	counter.total += C.va_arg(u64, ap)
	counter.calls++
	mut out := unsafe { C.va_arg(&int, ap) }
	callback := C.va_arg(Callback, ap)
	unsafe {
		*out = callback(nil, op)
	}
	C.va_end(ap)
}

fn test_va_arg_has_the_type_of_its_type_argument() {
	mut counter := Counter{}
	mut out := 0
	update(41, voidptr(&counter), u64(5), voidptr(&out), voidptr(plus_one))
	update(1, voidptr(&counter), u64(7), voidptr(&out), voidptr(plus_one))
	assert counter.total == 12
	assert counter.calls == 2
	assert out == 2
}
