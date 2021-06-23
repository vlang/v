import os
import time
import rand
import math

struct DataObj {
mut:
	data []f64
}

struct PtrObj {
mut:
	nxt []&DataObj
}

struct PtrPtrObj {
mut:
	nxt []&PtrObj
}

const (
	log2n = 11
	n     = 1 << log2n
	n4    = f64(u64(1) << (4 * log2n))
)

fn waste_mem() {
	mut objs := PtrPtrObj{
		nxt: []&PtrObj{len: n}
	}
	for {
		sz := rand.int_in_range(10, 1000)
		mut new_obj := &PtrObj{
			nxt: []&DataObj{len: sz}
		}
		sz2 := rand.int_in_range(10, 500000)
		new_obj2 := &DataObj{
			data: []f64{len: sz2}
		}
		idx2 := rand.int_in_range(0, sz)
		new_obj.nxt[idx2] = new_obj2
		// non-equally distributed random index
		idx := int(math.sqrt(math.sqrt(rand.f64n(n4))))
		objs.nxt[idx] = new_obj
	}
}

fn main() {
	mut n_iterations := 5_000_000
	if os.args.len == 2 {
		n_iterations = os.args[1].int()
	}
	if os.args.len > 2 || n_iterations <= 0 {
		eprintln('usage:\n\t${os.args[0]} [num_iterations]')
		exit(1)
	}
	go waste_mem()
	mut last := time.sys_mono_now()
	for _ in 0 .. n_iterations {
		now := time.sys_mono_now()
		interval := now - last
		println(f64(interval) / f64(time.millisecond))
		last = now
	}
}
