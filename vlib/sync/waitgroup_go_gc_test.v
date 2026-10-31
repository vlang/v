module sync

// The threads behind WaitGroup.go must be registered with the collector under Boehm.
// An unregistered thread aborts the program at the first collection it triggers.
fn test_waitgroup_go_threads_survive_collections() {
	mut wg := new_waitgroup()
	for _ in 0 .. 16 {
		wg.go(fn () {
			mut keep := []string{}
			for i in 0 .. 300000 {
				keep << 'value ${i}'
			}
			assert keep.len == 300000
		})
	}
	wg.wait()
}
