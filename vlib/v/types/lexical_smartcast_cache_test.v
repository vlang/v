module types

fn lexical_smartcast_cache_worker(tc &TypeChecker, start chan bool, idx int) {
	_ := <-start
	for _ in 0 .. 10000 {
		tc.remember_lexical_smartcast_miss(idx)
		assert tc.has_lexical_smartcast_miss(idx)
		_ = tc.has_lexical_smartcast_miss(1 - idx)
	}
}

fn test_lexical_smartcast_misses_support_concurrent_checker_views() {
	tc := &TypeChecker{
		lexical_smartcast_misses: []bool{len: 2}
	}
	assert !tc.has_lexical_smartcast_miss(0)
	assert !tc.has_lexical_smartcast_miss(2)
	start := chan bool{cap: 4}
	mut workers := []thread{}
	for i in 0 .. 4 {
		workers << spawn lexical_smartcast_cache_worker(tc, start, i % 2)
	}
	for _ in 0 .. 4 { start <- true }
	workers.wait()
	assert tc.has_lexical_smartcast_miss(0)
	assert tc.has_lexical_smartcast_miss(1)
}
