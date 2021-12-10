import time

fn incr(shared foo []int, index int) {
	for _ in 0 .. 100000 {
		lock foo {
			foo[index] = foo[index] + 1
		}
	}
	lock foo {
		foo[2]++
	}
}

fn test_shared_array() {
	shared foo := &[10, 20, 0]
	go incr(shared foo, 0)
	go incr(shared foo, 1)
	go incr(shared foo, 0)
	go incr(shared foo, 1)
	for _ in 0 .. 50000 {
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
		time.sleep(100 * time.millisecond)
	}
	rlock foo {
		f0 := foo[0]
		f1 := foo[1]
		assert f0 == 100010
		assert f1 == 350020
	}
}

fn test_shared_init_syntax() {
	shared foo := &[3, 5, 6, -12]
	shared bar := [-12.5, 23.125, 6.0625, 12.5]
	shared baz := &[]int{len: 5, cap: 12}
	shared qux := []f64{len: 7}
	shared quux := new_array()
	lock foo {
		foo[2] = 20
	}
	lock bar {
		bar[3] = 12.5
	}
	lock baz, qux, quux {
		baz[3] = 12
		qux[6] = -17.0625
		quux[2] = 7.0625
	}
	rlock foo, bar, baz, qux, quux {
		assert foo[2] == 20
		assert bar[3] == 12.5
		assert baz[3] == 12
		assert qux[6] == -17.0625
		assert quux[1] == 6.25
		assert quux[2] == 7.0625
	}
}

fn new_array() []f64 {
	a := [12.5, 6.25, -3.125, 1.75]
	return a
}
