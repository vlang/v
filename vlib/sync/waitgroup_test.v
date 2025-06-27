module sync

import time

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
	mut counter := 0
	p := unsafe { &counter }
	mut wg := new_waitgroup()
	for i in 0 .. 10 {
		wg.go(fn [p] () {
			unsafe { (*p)++ }
		})
	}
	wg.wait()
	assert counter == 10
}
