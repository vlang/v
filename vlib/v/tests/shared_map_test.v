import sync

fn incr(shared foo map[string]int, key string, mut sem sync.Semaphore) {
	for _ in 0 .. 100000 {
		lock foo {
			foo[key] = foo[key] + 1
		}
	}
	sem.post()
}

fn test_shared_array() {
	shared foo := {
		'p': 10
		'q': 0
	}
	lock foo {
		foo['q'] = 20
	}
	mut sem := sync.new_semaphore()
	go incr(shared foo, 'p', mut sem)
	go incr(shared foo, 'q', mut sem)
	go incr(shared foo, 'p', mut sem)
	go incr(shared foo, 'q', mut sem)
	for _ in 0 .. 50000 {
		lock foo {
			foo['p'] -= 2
			foo['q'] += 3
		}
	}
	for _ in 0 .. 4 {
		sem.wait()
	}
	rlock foo {
		fp := foo['p']
		fq := foo['q']
		assert fp == 100010
		assert fq == 350020
	}
}

fn test_shared_init_syntax() {
	shared foo := &{
		'p':      17
		'q':      -3
		'qwertz': 10
	}
	shared bar := {
		'wer':  13.75
		'cvbn': -7.25
		'asd':  -0.0625
	}
	shared baz := &map[string]int{}
	shared qux := map[string]f64{}
	shared quux := new_map()
	lock foo {
		foo['q'] = 20
	}
	lock bar {
		bar['asd'] = 12.5
	}
	lock baz, qux, quux {
		baz['wer'] = 12
		qux['abc'] = -17.0625
		quux['tzu'] = 1.125
	}
	rlock foo, bar, baz, qux, quux {
		assert foo['q'] == 20
		assert bar['asd'] == 12.5
		assert baz['wer'] == 12
		assert qux['abc'] == -17.0625
		assert quux['tzu'] == 1.125
		assert quux['yxc'] == 9.125
	}
}

fn new_map() map[string]f64 {
	m := {
		'qwe': 34.25
		'yxc': 9.125
		'tzu': -7.5
	}
	return m
}

fn test_shared_array_iteration() {
	shared a := [12.75, -0.125, 18.5 - (1.0 + .5)]
	mut n0 := 0
	mut n1 := 0
	mut n2 := 0
	rlock a {
		for i, val in a {
			match i {
				1 {
					assert val == -0.125
					n1++
					// check for order, too:
					assert n0 == 1
				}
				0 {
					assert val == 12.75
					n0++
				}
				2 {
					assert val == 17.0
					n2++
					assert n1 == 1
				}
				else {
					// this should not happen
					assert false
				}
			}
		}
	}
	// make sure we have iterated over each of the 3 keys exactly once
	assert n0 == 1
	assert n1 == 1
	assert n2 == 1
}

fn test_shared_map_iteration() {
	shared m := {
		'qwe': 12.75
		'rtz': -0.125
		'k':   17
	}
	mut n0 := 0
	mut n1 := 0
	mut n2 := 0
	rlock m {
		for k, val in m {
			match k {
				'rtz' {
					assert val == -0.125
					n0++
				}
				'qwe' {
					assert val == 12.75
					n1++
				}
				'k' {
					assert val == 17.0
					n2++
				}
				else {
					// this should not happen
					assert false
				}
			}
		}
	}
	// make sure we have iterated over each of the 3 keys exactly once
	assert n0 == 1
	assert n1 == 1
	assert n2 == 1
}

fn test_shared_map_in() {
	shared m := {
		'qwe': 12.75
		'rtz': -0.125
		'k':   17
	}
	rlock m {
		assert 'qwe' in m
		assert 'rtz' in m
		assert 'k' in m
		assert 'zxc' !in m
	}
}
