// CX-free repro of the cx #14 [par] anti-scaling under -gc e (B13).
//
// Mirrors the cx map-reduce: N independent workers, each allocating many small
// boxed objects that are NOT Perceus-droppable (stored through an array-index
// target -> the store target is a USE, so the prior occupant becomes garbage the
// TRACING BACKSTOP must reclaim, not the Perceus front line). So heap_live climbs
// by every alloc until it hits next_gc (256MB) -> full STW collection. With N
// workers sharing the global trigger, the COMBINED alloc rate trips it ~N x more
// often than one worker, and every collection mach-suspends all N -> par
// anti-scales vs its own serial. This is the GC-STW-frequency cost B13 isolated
// (distinct from the allocator cacheline contention R2 already fixed).
//
//   v -gc e     -prod -cc cc -o par_reclaim_e     par_reclaim.v
//   v -gc boehm -prod -cc cc -o par_reclaim_boehm par_reclaim.v
//   ./par_reclaim_e <threads> [iters_per_thread] [ring]
import sync
import time
import os

@[heap]
struct Obj {
mut:
	a   u64
	b   u64
	pad []u8
}

fn reclaim_worker(iters int, ring int, mut wg sync.WaitGroup) {
	// live set bounded at `ring`; every store overwrites -> prior occupant is
	// garbage only the collector reclaims. heap_live climbs by objsize per alloc.
	mut buf := []&Obj{len: ring, init: &Obj{}}
	mut sink := u64(0)
	for i in 0 .. iters {
		o := &Obj{
			a:   u64(i)
			pad: []u8{len: 32}
		}
		buf[i % ring] = o // array-index store target -> not a Perceus drop site
		sink += o.a
	}
	for j in 0 .. ring {
		sink += buf[j].a // keep buf live to end
	}
	if sink == 0xdeadbeef {
		println('unreachable ${sink}')
	}
	wg.done()
}

fn arg(i int, def int) int {
	return if os.args.len > i { os.args[i].int() } else { def }
}

fn main() {
	threads := arg(1, 1)
	per := arg(2, 8_000_000)
	ring := arg(3, 4096)

	mut wg := sync.new_waitgroup()
	wg.add(threads)
	sw := time.new_stopwatch()
	for _ in 0 .. threads {
		spawn reclaim_worker(per, ring, mut wg)
	}
	wg.wait()
	el := sw.elapsed().milliseconds()
	total := i64(threads) * i64(per)
	mops := if el > 0 { f64(total) / f64(el) / 1000.0 } else { 0.0 }
	println('threads=${threads} allocs=${total} ms=${el} Mops/s=${mops:.1f}')
}
