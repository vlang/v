module sync

fn test_waitgroup_go_threads_survive_collections() {
	workers := 16
	values_per_worker := 16
	mut ready := new_barrier(workers)
	mut wg := new_waitgroup()
	results := chan bool{cap: workers}
	for worker in 0 .. workers {
		wg.go(fn [mut ready, results, worker, values_per_worker] () {
			values := []string{len: values_per_worker, init: 'worker ${worker} value ${index}'}
			// Keep heap data live in every worker when collection starts.
			ready.wait()
			// Boehm aborts if the worker that calls gc_collect is unregistered.
			gc_collect()
			mut matches := values.len == values_per_worker
			for i, value in values {
				matches = matches && value == 'worker ${worker} value ${i}'
			}
			results <- matches
		})
	}
	wg.wait()
	for _ in 0 .. workers {
		assert <-results
	}
}
