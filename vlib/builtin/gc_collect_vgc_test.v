// vtest vflags: -gc vgc
// vtest build: !race?
@[has_globals]
module builtin

// Consts and globals that are initialized at run time point into the vgc heap.
const vgc_test_const_text = 'const text '.repeat(20)
const vgc_test_const_map = {
	'a': 'x'.repeat(30)
	'b': 'y'.repeat(30)
}

__global vgc_test_global_strs = []string{}

// vgc_test_churn allocates short-lived strings and arrays, which reuse the slots that a
// collection freed.
@[noinline]
fn vgc_test_churn(rounds int) int {
	mut total := 0
	for i in 0 .. rounds {
		mut a := []string{cap: 4}
		for j in 0 .. 4 {
			a << 'garbage-${i}-${j}-'.repeat(3)
		}
		total += a.len
	}
	return total
}

fn test_consts_and_globals_are_roots() {
	$if vgc ? {
		for i in 0 .. 100 {
			vgc_test_global_strs << 'global-${i}-'.repeat(3)
		}
		for _ in 0 .. 3 {
			vgc_gc_start()
			vgc_test_churn(20000)
		}
		assert vgc_test_const_text == 'const text '.repeat(20)
		assert vgc_test_const_map['a'] == 'x'.repeat(30)
		assert vgc_test_const_map['b'] == 'y'.repeat(30)
		for i in 0 .. 100 {
			assert vgc_test_global_strs[i] == 'global-${i}-'.repeat(3)
		}
	}
}

fn vgc_test_thread_work(id int) int {
	mut total := 0
	for i in 0 .. 500 {
		total += '${id}-${i}-'.repeat(5).len
	}
	return total
}

fn test_threads_that_exited_are_not_scanned() {
	$if vgc ? {
		// 80 threads in total: more than the 64 cache slots, which exited threads give back.
		for round in 0 .. 10 {
			mut threads := []thread int{}
			for id in 0 .. 8 {
				threads << spawn vgc_test_thread_work(round * 8 + id)
			}
			results := threads.wait()
			// The stacks of the threads that have exited are unmapped now.
			vgc_gc_start()
			for id, total in results {
				assert total == vgc_test_thread_work(round * 8 + id)
			}
		}
		assert vgc_heap.ncaches <= 9
	}
}
