// vtest vflags: -gc vgc
// vtest build: !race?
// Every thread that allocates under vgc must get its own allocation cache. With TinyCC on
// Linux, spawned threads read the main thread's cache index and allocated from the same spans
// without a lock, which failed with `malloc_noscan(...) ... memory allocation failure`.
import sync

fn C.vgc_get_cache_idx() int

// Keep the count dynamic so compile-time string folding cannot remove the allocation.
@[noinline]
fn cache_idx_after_allocating(count int) int {
	s := 'vgc'.repeat(count)
	assert s.len == 3 * count
	return C.vgc_get_cache_idx()
}

// cache_idx_while_others_run keeps the thread alive until all threads have their cache: a
// thread that has exited gives its cache slot to the next one.
fn cache_idx_while_others_run(started &sync.WaitGroup, release chan bool) int {
	idx := cache_idx_after_allocating(40)
	mut wg := unsafe { started }
	wg.done()
	_ := <-release
	return idx
}

fn allocate_strings(id int) int {
	mut total := 0
	for round in 0 .. 20 {
		for i in 0 .. 1000 {
			total += '${id}-${round}-'.repeat(i % 7 + 1).len
		}
		if round % 5 == 0 {
			gc_collect()
		}
	}
	return total
}

fn test_spawned_threads_get_their_own_vgc_cache() {
	$if vgc ? {
		main_idx := cache_idx_after_allocating(40)
		assert main_idx >= 0
		mut threads := []thread int{}
		mut started := sync.new_waitgroup()
		started.add(4)
		release := chan bool{cap: 4}
		for _ in 0 .. 4 {
			threads << spawn cache_idx_while_others_run(started, release)
		}
		started.wait()
		for _ in 0 .. 4 {
			release <- true
		}
		idxs := threads.wait()
		for i, idx in idxs {
			assert idx >= 0
			assert idx != main_idx
			for j in 0 .. i {
				assert idx != idxs[j]
			}
		}
	}
}

fn test_threads_allocate_concurrently() {
	$if vgc ? {
		mut threads := []thread int{}
		for id in 0 .. 4 {
			threads << spawn allocate_strings(id)
		}
		totals := threads.wait()
		for id, total in totals {
			assert total == allocate_strings(id)
		}
	}
}
