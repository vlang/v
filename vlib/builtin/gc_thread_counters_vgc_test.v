// vtest vflags: -gc vgc
// vtest build: !race?
// vgc counts the live heap (its collection trigger) and the bytes allocated (gc_heap_usage()) in
// the cache of each thread, not in counters that all threads change on every allocation. The
// sums over all threads mean what the shared counters meant.
@[has_globals]
module builtin

struct VgcTestCounts {
	idx         int
	live_before u64
	live_after  u64
	alloc_after u64
}

// vgc_test_counts returns the counters of the current thread's cache slot.
fn vgc_test_counts() (u64, u64) {
	idx := C.vgc_get_cache_idx()
	return vgc_heap.caches[idx].live_bytes, vgc_heap.caches[idx].alloc_bytes
}

// vgc_test_allocate_in_thread allocates n objects of 1000 bytes without pointers (1024 bytes
// each, with their size class) in the current thread, and returns its counters around that.
fn vgc_test_allocate_in_thread(n int) VgcTestCounts {
	// The first allocation registers the thread.
	_ = vgc_malloc_noscan(1000)
	live_before, alloc_before := vgc_test_counts()
	for _ in 0 .. n {
		_ = vgc_malloc_noscan(1000)
	}
	live_after, alloc_after := vgc_test_counts()
	return VgcTestCounts{
		idx:         C.vgc_get_cache_idx()
		live_before: live_before
		live_after:  live_after
		alloc_after: alloc_after - alloc_before
	}
}

fn test_a_thread_counts_its_allocations_in_its_own_cache() {
	$if vgc ? {
		gc_disable()
		main_idx := C.vgc_get_cache_idx()
		t := spawn vgc_test_allocate_in_thread(100)
		counts := t.wait()
		gc_enable()
		assert counts.idx != main_idx
		assert counts.live_after - counts.live_before == 100 * 1024
		assert counts.alloc_after == 100 * 1000
	}
}

fn test_a_free_is_counted_by_the_thread_that_frees() {
	$if vgc ? {
		gc_disable()
		live_before, alloc_before := vgc_test_counts()
		p := vgc_malloc(1000)
		vgc_free(p)
		live_after, alloc_after := vgc_test_counts()
		gc_enable()
		assert live_after == live_before
		assert alloc_after == alloc_before + 1000
	}
}

fn test_the_counts_of_threads_that_exited_are_kept() {
	$if vgc ? {
		gc_disable()
		before := gc_heap_usage()
		live_before := vgc_heap_live()
		first := spawn vgc_test_allocate_in_thread(1000)
		a := first.wait()
		after_first := gc_heap_usage()
		live_after_first := vgc_heap_live()
		// The next thread usually gets the cache slot that the first one gave up.
		second := spawn vgc_test_allocate_in_thread(10)
		b := second.wait()
		after_second := gc_heap_usage()
		live_after_second := vgc_heap_live()
		gc_enable()
		assert a.alloc_after == 1000 * 1000
		assert b.alloc_after == 10 * 1000
		assert after_first.total_bytes >= before.total_bytes + 1000 * 1000
		assert after_first.bytes_since_gc >= before.bytes_since_gc + 1000 * 1000
		assert after_second.total_bytes >= after_first.total_bytes + 10 * 1000
		assert live_after_first >= live_before + 1000 * 1024
		assert live_after_second >= live_after_first + 10 * 1024
	}
}

// vgc_test_allocate_strings allocates about 13 MB of strings in a thread.
fn vgc_test_allocate_strings() int {
	mut total := 0
	for i in 0 .. 100000 {
		total += 'string data '.repeat(1 + i % 20).len
	}
	return total
}

fn test_the_allocations_of_a_thread_trigger_a_collection() {
	$if vgc ? {
		gc_collect()
		cycles := vgc_heap.gc_cycle
		// Lower the trigger for the test: 4 MB above the live heap.
		C.vgc_atomic_store_u64(&vgc_heap.next_gc, vgc_heap_live() + 4 * 1024 * 1024)
		t := spawn vgc_test_allocate_strings()
		assert t.wait() > 0
		assert vgc_heap.gc_cycle > cycles
	}
}

fn test_a_collection_starts_the_live_heap_and_bytes_since_gc_again() {
	$if vgc ? {
		gc_collect()
		live := vgc_heap_live()
		t := spawn vgc_test_allocate_in_thread(4000)
		t.wait()
		assert vgc_heap_live() >= live + 4000 * 1024
		gc_collect()
		// The 4 MB that the thread allocated are garbage now.
		assert vgc_heap_live() < live + 1024 * 1024
		assert gc_heap_usage().bytes_since_gc < 1024 * 1024
	}
}
