import sync
import time
import os

@[heap]
struct Obj {
mut:
	a u64
	b u64
	c u64
	d u64
}

fn alloc_worker(iters int, mut wg sync.WaitGroup) {
	mut sink := u64(0)
	for i in 0 .. iters {
		o := &Obj{ a: u64(i) }
		sink += o.a
	}
	if sink == 0xdeadbeef { println('x ${sink}') }
	wg.done()
}

fn arg(i int, def int) int { return if os.args.len > i { os.args[i].int() } else { def } }

fn main() {
	threads := arg(1, 4)
	per := arg(2, 2_000_000)
	mut wg := sync.new_waitgroup()
	wg.add(threads)
	sw := time.new_stopwatch()
	for _ in 0 .. threads { spawn alloc_worker(per, mut wg) }
	wg.wait()
	el := sw.elapsed().milliseconds()
	total := i64(threads) * i64(per)
	mops := if el > 0 { f64(total) / f64(el) / 1000.0 } else { 0.0 }
	println('threads=${threads} ms=${el} Mops/s=${mops:.1f}')
}
