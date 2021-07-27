fn one() int {
	return 1
}

fn pushf64() {
	ch <- 12.5
}

fn test_global_init() {
	intmap['two'] = 27
	key := 'two'
	assert intmap[key] == 27
	t := go pushf64()
	numberfns['one'] = one
	numberfns['two'] = fn () int {
		return 2
	}
	f := numberfns['one']
	n := f()
	assert n == 1
	m := numberfns['two']()
	assert m == 2
	got := <-ch
	assert got == 12.5
	t.wait()
	assert true
}

__global (
	intmap    map[string]int
	numberfns map[string]fn () int
	ch        chan f64
)
