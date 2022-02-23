// vtest retry: 4
/*
* To verify the effect of "[keep_args_alive]", this attribute may be commented out.
* However it is not guaranteed that then this test will fail.
* To provoke a failure it seems to be best to use `gcc` with optimization:
* `gcc -gc boehm -cc gcc-9 -prod test keep_args_alive_test.v`.
* Without optimization, pointer variables may remain on the stack even if
* not used any more.
*/
import rand
import sync

#flag -I@VEXEROOT/vlib/v/tests
#include "keep_args_alive_test_c.h"

fn C.atomic_load_ptr(voidptr) voidptr

fn C.atomic_store_ptr(voidptr, voidptr)

[keep_args_alive]
fn C.calc_expr_after_delay(voidptr, int, voidptr) int

fn set_vals() voidptr {
	unsafe {
		p := &int(malloc(8000000))
		q := &int(malloc(8000000))
		aa := p + 769345
		*aa = -4578
		bb := q + 572397
		*bb = 793254
		p = &int(0)
		q = &int(0)
		r := &voidptr(malloc(1000000))
		r[456] = aa
		r[7932] = bb
		aa = &int(0)
		bb = &int(0)
		return r
	}
}

fn tt(mut sem sync.Semaphore) int {
	waste_mem(10000, mut sem)
	r := &voidptr(set_vals())
	g := unsafe { C.calc_expr_after_delay(r[456], 12, r[7932]) }
	return g
}

fn waste_mem(n int, mut sem sync.Semaphore) {
	mut m := []voidptr{len: 30}
	for j := 0; n < 0 || j < n; j++ {
		i := rand.intn(30) or { 0 }
		m[i] = unsafe { malloc(10000) }
		fill := rand.intn(256) or { 0 }
		unsafe { C.memset(m[i], fill, 10000) }
		if n < 0 && sem.try_wait() {
			break
		}
	}
}

fn test_keep_args_alive_attribute() {
	mut sem := sync.new_semaphore()
	$if gcboehm ? {
		go waste_mem(-1, mut sem)
		go waste_mem(-1, mut sem)
		waste_mem(10000, mut sem)
	}
	r := &voidptr(set_vals())
	v := unsafe { C.calc_expr_after_delay(r[456], 12, r[7932]) }
	$if gcboehm ? {
		sem.post()
		sem.post()
	}
	assert v == 738318
}
