// MP-scaling benchmark for the vgc backstop collector gate (spec §7.2 G-R2 / G-R2s).
// R2  (alloc): each thread allocates independently (no sharing) — tests whether the
//      per-thread mcache lets allocation throughput scale UP with thread count
//      (Boehm anti-scales here = cx-private issue #14).
// R2s (share): a shared live object graph that every thread reads, plus per-thread
//      transient allocation — tests that a shared graph does not make collection
//      anti-scale.
//   v -gc {none|boehm|vgc} -prod -o bench_mp bench_mp.v
//   ./bench_mp <alloc|share> <threads> [iters_per_thread]
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

__global g_pool = []&Obj{}

fn alloc_worker(iters int, mut wg sync.WaitGroup) {
	mut sink := u64(0)
	for i in 0 .. iters {
		o := &Obj{
			a:   u64(i)
			pad: []u8{len: 32}
		}
		sink += o.a
	}
	if sink == 0xdeadbeef {
		println('unreachable ${sink}')
	}
	wg.done()
}

fn share_worker(iters int, mut wg sync.WaitGroup) {
	mut sink := u64(0)
	pl := g_pool.len
	for i in 0 .. iters {
		o := g_pool[i % pl] // read the shared graph (kept live, traced each GC)
		sink += o.a
		t := &Obj{
			a:   u64(i)
			pad: []u8{len: 32}
		}
		sink += t.a // per-thread transient
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
	mode := if os.args.len > 1 { os.args[1] } else { 'alloc' }
	threads := arg(2, 4)
	per := arg(3, 2_000_000)

	if mode == 'share' {
		for i in 0 .. 50_000 {
			g_pool << &Obj{
				a:   u64(i)
				pad: []u8{len: 32}
			}
		}
	}

	mut wg := sync.new_waitgroup()
	wg.add(threads)
	sw := time.new_stopwatch()
	for _ in 0 .. threads {
		if mode == 'share' {
			spawn share_worker(per, mut wg)
		} else {
			spawn alloc_worker(per, mut wg)
		}
	}
	wg.wait()
	el := sw.elapsed().milliseconds()
	total := i64(threads) * i64(per)
	mops := if el > 0 { f64(total) / f64(el) / 1000.0 } else { 0.0 }
	println('${mode} threads=${threads} allocs=${total} ms=${el} Mops/s=${mops:.1f}')
}
