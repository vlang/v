module modc

#flag -I @VROOT
#flag @VROOT/impl.o
#include "header.h"
struct C.Atype {
}

// NB: [trusted] below, means that the C function, can be safely called outside unsafe{} blocks.
//
// By default, all C. functions are NOT trusted, and all V functions are by default trusted.
//
// Relatedly, if you want to mark a V function as unsafe, use [unsafe].
//
// The V compiler forces all calls of unsafe functions to be wrapped in `unsafe{...}` blocks.
struct Vtype {
pub mut:
	p &C.Atype
}

fn C.new_atype(int) voidptr

[trusted]
fn C.handle_array(voidptr, int)

fn todo_remove_me() {
	// TODO: remove this dummy function, when the vfmt bug of [trusted] after a void C function is fixed
}

[trusted]
fn C.handle_array2(voidptr, int)

fn C.destroy_atype(voidptr)

pub fn new_vtype(n int) Vtype {
	t := C.new_atype(n)
	return Vtype{t}
}

pub fn call_with_array_param(arr []Vtype) {
	mut carr := []C.Atype{}
	for t in arr {
		carr << *t.p
	}
	// TODO: make it safe
	C.handle_array(carr.data, carr.len)
	C.handle_array2(carr.data, carr.len)
}

pub fn destroy_vtype(t Vtype) {
	C.destroy_atype(t.p)
}
