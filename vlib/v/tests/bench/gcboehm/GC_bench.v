import time
import rand
import math

struct MemObj {
mut:
	nxt []&MemObj
}

const (
	log2n = 10
	n     = 1 << log2n
	n4    = f64(u64(1) << (4 * log2n))
)

fn waste_mem() {
	mut objs := MemObj{
		nxt: []&MemObj{len: n}
	}
	for {
		sz := rand.int_in_range(10, 100000)
		mut new_obj := &MemObj{
			nxt: []&MemObj{len: sz}
		}
		sz2 := rand.int_in_range(10, 100000)
		new_obj2 := &MemObj{
			nxt: []&MemObj{len: sz2}
		}
		idx2 := rand.int_in_range(0, sz / 2)
		new_obj.nxt[idx2] = new_obj2
		// non-equally distributed random index
		idx := int(math.sqrt(math.sqrt(rand.f64n(n4))))
		objs.nxt[idx] = new_obj
	}
}

fn main() {
	go waste_mem()
	mut last := time.sys_mono_now()
	for _ in 0 .. 10_000_000 {
		now := time.sys_mono_now()
		interval := now - last
		println(f64(interval) / f64(time.millisecond))
		last = now
	}
}
