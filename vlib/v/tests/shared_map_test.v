import sync

fn incr(shared foo map[string]int, key string, sem sync.Semaphore) {
	for _ in 0 .. 100000 {
		lock foo {
			foo[key] = foo[key] + 1
		}
	}
	sem.post()
}

fn test_shared_array() {
	shared foo := &{'p': 10, 'q': 0}
	lock foo {
		unsafe {
			foo['q'] = 20
		}
	}
	sem := sync.new_semaphore()
	go incr(shared foo, 'p', sem)
	go incr(shared foo, 'q', sem)
	go incr(shared foo, 'p', sem)
	go incr(shared foo, 'q', sem)
	for _ in 0 .. 50000 {
		lock foo {
			unsafe {
				foo['p'] -= 2
				foo['q'] += 3
			}
		}
	}
	for _ in 0..4 {
		sem.wait()
	}
	rlock foo {
		fp := unsafe { foo['p'] }
		fq := unsafe { foo['q'] }
		assert fp == 100010
		assert fq == 350020
	}
}
