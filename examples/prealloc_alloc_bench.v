// Micro-benchmark for the bump allocator's pure allocation path.
// Build with -prealloc -gc none to measure the arena, or -gc none for libc malloc.
import time

const bench_iters = 8_000_000
const bench_size = 32

fn bench_allocs() u64 {
	mut total := u64(0)
	for i in 0 .. bench_iters {
		p := unsafe { malloc(bench_size) }
		unsafe { p[0] = u8(i) }
		total += unsafe { p[0] }
	}
	return total
}

fn main() {
	// Warm up the allocator and the branch predictor.
	_ = bench_allocs()
	mut best := u64(-1)
	for _ in 0 .. 3 {
		sw := time.new_stopwatch()
		sum := bench_allocs()
		if sum != 1_020_000_000 {
			panic('unexpected benchmark result')
		}
		us := sw.elapsed().microseconds()
		if us < best {
			best = us
		}
	}
	println('${bench_iters} x ${bench_size}B: ${best} us (${best * 1000 / bench_iters} ns/op)')
}
