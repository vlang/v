// vtest retry: 3
// vtest flaky: true
module sync

import sync.stdatomic

fn issue_6870_worker(mut wg WaitGroup, ready chan bool, release chan bool) {
	ready <- true
	_ = <-release
	wg.add(1)
	wg.done()
	wg.done()
}

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
		sync_sleep_nanoseconds(100_000_000)
	}(mut wg, voidptr(&executed))

	wg.wait()
	assert executed
}

fn test_waitgroup_no_use() {
	done := chan bool{cap: 1}
	watchdog := spawn fn (done chan bool) {
		select {
			_ := <-done {}
			i64(10_000_000_000) {
				panic('test_waitgroup_no_use did not complete in time')
			}
		}
	}(done)

	mut wg := new_waitgroup()
	wg.wait()
	done <- true
	watchdog.wait()
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

fn test_waitgroup_add_while_waiting() {
	for _ in 0 .. 50 {
		mut wg := new_waitgroup()
		ready := chan bool{cap: 1}
		release := chan bool{cap: 1}
		wait_done := chan bool{cap: 8}
		wg.add(1)

		spawn issue_6870_worker(mut wg, ready, release)
		_ = <-ready

		for _ in 0 .. 8 {
			spawn fn [mut wg, wait_done] () {
				wg.wait()
				wait_done <- true
			}()
		}
		release <- true

		for _ in 0 .. 8 {
			select {
				_ := <-wait_done {}
				i64(2_000_000_000) {
					assert false, 'wait() missed a wakeup while work added more tasks'
				}
			}
		}
	}
}
