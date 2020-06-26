module sync

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

#include "atomic.h"

fn C.atomic_fetch_add_explicit() int
fn C.atomic_fetch_sub_explicit() int

[typedef]
struct C.atomic_ullong

[typedef]
struct C.atomic_llong

pub fn add_u64(ptr &u64, delta int) bool {
	res := C.atomic_fetch_add_explicit(&C.atomic_ullong(ptr), delta, C.NULL)
	return res == 0
}

pub fn sub_u64(ptr &u64, delta int) bool {
	res := C.atomic_fetch_sub_explicit(&C.atomic_ullong(ptr), delta, C.NULL)
	return res == 0
}

// pub fn add_i64(ptr &i64, delta int) bool {
// 	$if tinyc {
// 		g_mutex.lock()
// 		ptr+=i64(delta)
// 		g_mutex.unlock()
// 		return true
// 	} $else {
// 		res := C.atomic_fetch_add_explicit(&C.atomic_llong(ptr), delta, C.NULL)
// 		return res == 0
// 	}
// }

// pub fn sub_i64(ptr &i64, delta int) bool {
// 	$if tinyc {
// 		g_mutex.lock()
// 		ptr-=i64(delta)
// 		g_mutex.unlock()
// 		return true
// 	} $else {
// 		res := C.atomic_fetch_sub_explicit(&C.atomic_llong(ptr), delta, C.NULL)
// 		return res == 0
// 	}
// }