import os
import rand
import rand.wyrand
import rand.musl
import rand.pcg32
import benchmark

const max_iterations = os.getenv_opt('MAX_ITERATIONS') or { '1000' }.int()

fn main() {
	mut sum := 0
	mut bigsum := u64(0)
	mut b := benchmark.start()

	C.srand(7868768)
	for _ in 0 .. max_iterations {
		sum += C.rand()
	}
	b.measure('C.rand called ${max_iterations} times, sum: ${sum}')

	rand.seed([u32(545123), 87834597])
	for _ in 0 .. max_iterations {
		bigsum += rand.u64()
	}
	b.measure('rand.u64 called ${max_iterations} times, bigsum: ${bigsum}')

	bigsum = 0
	mut wy := &wyrand.WyRandRNG{}
	wy.seed([u32(2345345), 8787671])
	for _ in 0 .. max_iterations {
		bigsum += wy.u64()
	}
	b.measure('wy.u64 called ${max_iterations} times, bigsum: ${bigsum}')

	bigsum = 0
	mut mu := &musl.MuslRNG{}
	mu.seed([u32(2345345)])
	for _ in 0 .. max_iterations {
		bigsum += mu.u64()
	}
	b.measure('mu.u64 called ${max_iterations} times, bigsum: ${bigsum}')

	bigsum = 0
	mut pc := &pcg32.PCG32RNG{}
	pc.seed([u32(2345345), 34345, 77634, 12381])
	for _ in 0 .. max_iterations {
		bigsum += pc.u64()
	}
	b.measure('pc.u64 called ${max_iterations} times, bigsum: ${bigsum}')
}
