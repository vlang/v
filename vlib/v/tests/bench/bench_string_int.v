import benchmark

const maxn = 999_999

fn main() {
	mut snumbers := []string{cap: maxn}
	for i in 0 .. maxn {
		snumbers << i.str()
	}
	mut sum := i64(0)
	mut bmark := benchmark.start()
	for s in snumbers {
		sum += s.int()
	}
	bmark.measure('s.int()')
	dump(sum)
}
