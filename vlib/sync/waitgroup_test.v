module sync

fn test_waitgroup_reuse() {
	mut wg := new_waitgroup()

	wg.add(1)
	wg.done()

	wg.add(1)
	mut executed := false
	go fn (mut wg WaitGroup, executed voidptr) {
		defer {
			wg.done()
		}
		unsafe {
			*(&bool(executed)) = true
		}
	}(mut wg, voidptr(&executed))

	wg.wait()
	assert executed
}
