#include "@VMODROOT/opaque_handle.h"

@[typedef]
pub struct C.OpaqueHandle {}

fn C.opaque_handle_get() &C.OpaqueHandle
fn C.opaque_handle_value(h &C.OpaqueHandle) int

pub type OpaqueHandle = C.OpaqueHandle

const opaque_handle = C.opaque_handle_get()
const opaque_alias_handle = unsafe { &OpaqueHandle(C.opaque_handle_get()) }

struct OpaqueUser[T] {
mut:
	value T
	seen  voidptr
}

fn (mut u OpaqueUser[T]) set_handle(h &OpaqueHandle) int {
	u.seen = voidptr(h)
	return C.opaque_handle_value(h)
}

struct OpaqueRawUser {}

fn (u OpaqueRawUser) handle_ptr(h &C.OpaqueHandle) voidptr {
	return voidptr(h)
}

// A pointer const must be passed as the pointer itself, not as the address of
// a copy of its target, which is an incomplete C type here (issue #28949).
fn test_pointer_const_of_incomplete_c_typedef_passed_to_method_param() {
	mut u := OpaqueUser[int]{}
	assert u.set_handle(opaque_handle) == 42
	assert u.seen == voidptr(opaque_handle)
	assert u.set_handle(opaque_alias_handle) == 42
	assert u.seen == voidptr(opaque_alias_handle)
	assert OpaqueRawUser{}.handle_ptr(opaque_handle) == voidptr(opaque_handle)
}
