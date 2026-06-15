// Boehm multi-thread GC-throughput bench — measures the parallel-mark
// contention that makes CX's [?map [par]] slower than serial (cx-private #14).
//
// Each thread allocates+drops SCANNED objects in a tight loop, forcing frequent
// collections. Under macOS Boehm with default parallel marking, every cycle
// wakes one mark helper per core that stomps the worker threads (mach
// suspend/resume), so wall-clock and per-thread throughput collapse as threads
// are added. Pinning markers to 1 (GC_set_markers_count(1), or GC_MARKERS=1)
// removes the stomp.
//
//   v -gc boehm -prod -o boehm_mp boehm_mp_bench.v
//   for n in 1 2 4 8; do                ./boehm_mp $n 2000000; done   # default
//   for n in 1 2 4 8; do GC_MARKERS=1   ./boehm_mp $n 2000000; done   # pinned
import sync
import time
import os

@[heap]
struct Obj {
mut:
	data []u8
}

fn worker(iters int, mut wg sync.WaitGroup) {
	mut acc := u64(0)
	for i in 0 .. iters {
		mut o := &Obj{
			data: []u8{len: 128} // scanned object; previous is dropped -> GC reclaims
		}
		o.data[0] = u8(i)
		acc += o.data[0]
	}
	if acc == 0xffffffff {
		println('unreachable')
	}
	wg.done()
}

fn arg(i int, def int) int {
	return if os.args.len > i { os.args[i].int() } else { def }
}

fn main() {
	nt := arg(1, 1)
	iters := arg(2, 2_000_000)
	mut wg := sync.new_waitgroup()
	wg.add(nt)
	sw := time.new_stopwatch()
	for _ in 0 .. nt {
		spawn worker(iters, mut wg)
	}
	wg.wait()
	ms := sw.elapsed().milliseconds()
	total := i64(nt) * i64(iters)
	mps := if ms > 0 { f64(total) / f64(ms) / 1000.0 } else { 0.0 }
	per := mps / f64(nt)
	println('${nt} threads  ${ms} ms  ${mps:6.1f} M allocs/s aggregate  (${per:6.1f} per-thread)')
}
