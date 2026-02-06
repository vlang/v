module sync

import time
import sync.stdatomic

fn test_waitgroup_reuse() {
	mut wg := new_waitgroup()

	wg.add(1)
	wg.done()

	wg.add(1)
	mut executed := false
	spawn fn (mut wg WaitGroup, executed voidptr) {
		defer {
			wg.done()
		}
		unsafe {
			*(&bool(executed)) = true
		}
		time.sleep(100 * time.millisecond)
		assert wg.wait_count == 1
	}(mut wg, voidptr(&executed))

	wg.wait()
	assert executed
	assert wg.wait_count == 0
}

fn test_waitgroup_no_use() {
	mut done := false
	spawn fn (done voidptr) {
		time.sleep(1 * time.second)
		if *(&bool(done)) == false {
			panic('test_waitgroup_no_use did not complete in time')
		}
	}(voidptr(&done))

	mut wg := new_waitgroup()
	wg.wait()
	done = true
}

fn test_waitgroup_go() {
	mut counter := stdatomic.new_atomic(0)
	mut wg := new_waitgroup()
	for i in 0 .. 10 {
		wg.go(fn [mut counter] () {
			counter.add(1)
		})
	}
	wg.wait()
	assert counter.load() == 10
}
