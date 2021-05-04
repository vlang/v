module atomic2

/*
Implements the atomic operations. For now TCC does not support
the atomic versions on nix so it uses locks to simulate the same behavor.
On windows tcc can simulate with other atomic operations.

The @VEXEROOT/thirdparty/stdatomic contains compability header files
for stdatomic that supports both nix, windows and c++.

This implementations should be regarded as alpha stage and be
further tested.
*/
#flag windows -I @VEXEROOT/thirdparty/stdatomic/win
#flag linux -I @VEXEROOT/thirdparty/stdatomic/nix
#flag darwin -I @VEXEROOT/thirdparty/stdatomic/nix
#flag freebsd -I @VEXEROOT/thirdparty/stdatomic/nix
#flag solaris -I @VEXEROOT/thirdparty/stdatomic/nix
$if linux {
	$if tinyc {
		// most Linux distributions have /usr/lib/libatomic.so, but Ubuntu uses gcc version specific dir
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/6
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/7
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/8
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/9
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/10
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/11
		#flag -L/usr/lib/gcc/x86_64-linux-gnu/12
		#flag -latomic
	}
}
#include <atomic.h>
// add_u64 adds provided delta as an atomic operation
pub fn add_u64(ptr &u64, delta int) bool {
	res := C.atomic_fetch_add_u64(voidptr(ptr), delta)
	return res == 0
}

// sub_u64 subtracts provided delta as an atomic operation
pub fn sub_u64(ptr &u64, delta int) bool {
	res := C.atomic_fetch_sub_u64(voidptr(ptr), delta)
	return res == 0
}

// add_i64 adds provided delta as an atomic operation
pub fn add_i64(ptr &i64, delta int) bool {
	res := C.atomic_fetch_add_u64(voidptr(ptr), delta)
	return res == 0
}

// add_i64 subtracts provided delta as an atomic operation
pub fn sub_i64(ptr &i64, delta int) bool {
	res := C.atomic_fetch_sub_u64(voidptr(ptr), delta)
	return res == 0
}

// atomic store/load operations have to be used when there might be another concurrent access
// atomicall set a value
pub fn store_u64(ptr &u64, val u64) {
	C.atomic_store_u64(voidptr(ptr), val)
}

// atomicall get a value
pub fn load_u64(ptr &u64) u64 {
	return C.atomic_load_u64(voidptr(ptr))
}

// atomicall set a value
pub fn store_i64(ptr &i64, val i64) {
	C.atomic_store_u64(voidptr(ptr), val)
}

// atomicall get a value
pub fn load_i64(ptr &i64) i64 {
	return i64(C.atomic_load_u64(voidptr(ptr)))
}
