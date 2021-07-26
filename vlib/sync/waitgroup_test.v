module sync

import time

fn test_waitgroup_reuse() {
	mut wg := new_waitgroup()

	wg.add(1)
	wg.done()

	wg.add(1)
	mut executed := false
	go fn (mut wg WaitGroup, executed voidptr) {
		defer {
			assert wg.wait_count == 1
			wg.done()
		}
		unsafe {
			*(&bool(executed)) = true
		}
	}(mut wg, voidptr(&executed))

	wg.wait()
	assert executed
	assert wg.wait_count == 0
}

fn test_waitgroup_no_use() {
	mut done := false
	go fn(done voidptr) {
		time.sleep(1 * time.second)
		if *(&bool(done)) == false {
			panic('test_waitgroup_no_use did not complete in time')
		}
	}(voidptr(&done))

	mut wg := new_waitgroup()
	wg.wait()
	done = true
}
