module atomic2

/*
Implements the atomic operations. For now TCC does not support
the atomic versions on nix so it uses locks to simulate the same behavor.
On windows tcc can simulate with other atomic operations.

The @VROOT/thirdparty/stdatomic contains compability header files
for stdatomic that supports both nix, windows and c++.

This implementations should be regarded as alpha stage and be
further tested.
*/
#flag windows -I @VROOT/thirdparty/stdatomic/win
#flag linux -I @VROOT/thirdparty/stdatomic/nix
#flag darwin -I @VROOT/thirdparty/stdatomic/nix
#flag freebsd -I @VROOT/thirdparty/stdatomic/nix
#flag solaris -I @VROOT/thirdparty/stdatomic/nix

$if linux {
	$if tinyc {
		// most Linux distributions have /usr/lib/libatomic.so, but Ubuntu uses gcc version specific dir
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/8 -L/usr/lib/gcc/x86_64-linux-gnu/9 -latomic
	}
}

#include <atomic.h>

fn C.atomic_fetch_add_explicit(voidptr, i64) i64
fn C.atomic_fetch_sub_explicit(voidptr, i64) i64

[typedef]
struct C.atomic_ullong {
}

[typedef]
struct C.atomic_llong {
}

// add_u64 adds provided delta as an atomic operation
pub fn add_u64(ptr &u64, delta int) bool {
	res := C.atomic_fetch_add_explicit(&C.atomic_ullong(ptr), delta, C.NULL)
	return res == 0
}

// sub_u64 subtracts provided delta as an atomic operation
pub fn sub_u64(ptr &u64, delta int) bool {
	res := C.atomic_fetch_sub_explicit(&C.atomic_ullong(ptr), delta, C.NULL)
	return res == 0
}

// add_i64 adds provided delta as an atomic operation
pub fn add_i64(ptr &i64, delta int) bool {
	res := C.atomic_fetch_add_explicit(&C.atomic_llong(ptr), delta, C.NULL)
	return res == 0
}

// add_i64 subtracts provided delta as an atomic operation
pub fn sub_i64(ptr &i64, delta int) bool {
	res := C.atomic_fetch_sub_explicit(&C.atomic_llong(ptr), delta, C.NULL)
	return res == 0
}
