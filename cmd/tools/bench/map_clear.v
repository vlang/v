import benchmark

fn main() {
	max_iterations := arguments()[1] or { '1_000_000' }.int()
	assert max_iterations > 0
	mut m := {
		123: 456
		789: 321
	}
	mut volatile sum := u64(0)
	mut b := benchmark.start()
	for i in 0 .. max_iterations {
		m.clear()
		m[i] = i * 2
		sum += u64(m.len)
	}
	assert m.len == 1
	b.measure('m.clear(), iterations: ${max_iterations}, sum: ${sum}')
}
