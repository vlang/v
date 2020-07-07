import sync
import time

fn incr(shared foo []int, index int) {
    for _ in 0..100000 {
		lock foo {
			foo[index] = foo[index] + 1
		}
    }
	lock foo {
		foo[2]++
	}
}

fn test_shared_array(){
    shared foo := &[10, 20, 0]
    go incr(shared foo, 0)
    go incr(shared foo, 1)
    go incr(shared foo, 0)
    go incr(shared foo, 1)
	for _ in 0..50000 {
		lock foo {
			foo[0] -= 2
			foo[1] += 3
		}
	}
	mut finished_threads := 0
	for {
		rlock foo {
			finished_threads = foo[2]
		}
		if finished_threads == 4 {
			break
		}
		time.sleep_ms(100)
	}
	mut s := 0
	mut t := 0
	rlock foo {
		s = foo[0]
		t = foo[1]
	}
	assert s == 100010
	assert t == 350020
}
