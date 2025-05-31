module main

@[typedef]
struct C.va_list {}

fn C.va_start(voidptr, voidptr)
fn C.va_arg(voidptr, voidptr) voidptr
fn C.va_end(voidptr)

fn sum(qtd int, ...) int {
	va := C.va_list{}
	mut s := 0
	C.va_start(va, qtd)
	for i := 0; i < qtd; i++ {
		s += C.va_arg(int, va)
	}
	return s
}

fn test_main() {
	assert sum(3, 4, 5, 6) == 15
}
