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

@[noinline]
fn vgc_test_garbage(n int, size int) {
	for _ in 0 .. n {
		_ = vgc_malloc_noscan(usize(size))
	}
}

fn test_a_span_is_not_recycled_while_a_thread_allocates_from_it() {
	$if vgc ? {
		// Two objects of a size class that nothing else uses: their span is this thread's
		// current span for that class, and it holds only garbage.
		vgc_test_garbage(2, 3000)
		span_class := int(C.vgc_size_class(3000)) * 2 + 1
		span := unsafe { vgc_heap.caches[C.vgc_get_cache_idx()].alloc[span_class] }
		assert span != unsafe { nil }
		vgc_gc_start()
		assert span.in_use
		assert span.alloc_bits != unsafe { nil }
		p := vgc_malloc_noscan(3000)
		assert vgc_find_span(p) == span
	}
}

fn vgc_test_keep_strings(id int, rounds int) int {
	mut errors := 0
	mut keep := []string{len: 2000}
	for r in 0 .. rounds {
		for i in 0 .. keep.len {
			keep[i] = '${id}-${r}-${i}-'.repeat(1 + i % 5)
		}
		for i in 0 .. keep.len {
			if keep[i] != '${id}-${r}-${i}-'.repeat(1 + i % 5) {
				errors++
			}
		}
	}
	return errors
}

fn test_threads_allocate_while_another_thread_collects() {
	$if vgc ? {
		mut threads := []thread int{}
		for id in 0 .. 4 {
			threads << spawn vgc_test_keep_strings(id, 25)
		}
		for _ in 0 .. 100 {
			vgc_gc_start()
		}
		for errors in threads.wait() {
			assert errors == 0
		}
	}
}

fn test_free_slots_of_partly_used_spans_are_reused() {
	$if vgc ? {
		// 1500 bytes: five objects per span, a size class that nothing else uses.
		mut kept := []voidptr{cap: 80}
		for i in 0 .. 400 {
			p := vgc_malloc_noscan(1500)
			if i % 5 == 0 {
				kept << p
			}
		}
		mut again := []voidptr{cap: 300}
		vgc_gc_start()
		in_use := vgc_memory_use()
		// 320 slots are free in the 80 spans that hold the kept objects.
		for _ in 0 .. 300 {
			again << vgc_malloc_noscan(1500)
		}
		assert vgc_memory_use() == in_use
		assert kept.len == 80
	}
}

fn test_gc_collect_frees_garbage() {
	$if vgc ? {
		gc_collect()
		cycles := vgc_heap.gc_cycle
		live := C.vgc_atomic_load_u64(&vgc_heap.heap_live)
		vgc_test_churn(20000)
		with_garbage := C.vgc_atomic_load_u64(&vgc_heap.heap_live)
		assert with_garbage > live + 4 * 1024 * 1024
		gc_collect()
		assert vgc_heap.gc_cycle == cycles + 1
		assert C.vgc_atomic_load_u64(&vgc_heap.heap_live) < live + 1024 * 1024
	}
}

fn test_gc_collect_does_nothing_while_the_gc_is_disabled() {
	$if vgc ? {
		assert gc_is_enabled()
		gc_disable()
		assert !gc_is_enabled()
		cycles := vgc_heap.gc_cycle
		gc_collect()
		assert vgc_heap.gc_cycle == cycles
		gc_enable()
		assert gc_is_enabled()
		gc_collect()
		assert vgc_heap.gc_cycle == cycles + 1
	}
}

fn test_pointer_free_allocations_trigger_a_collection() {
	$if vgc ? {
		cycles := vgc_heap.gc_cycle
		// Lower the trigger for the test: 4 MB above the live heap.
		C.vgc_atomic_store_u64(&vgc_heap.next_gc, C.vgc_atomic_load_u64(&vgc_heap.heap_live) +
			4 * 1024 * 1024)
		// About 13 MB of strings, and no other allocations.
		mut total := 0
		for i in 0 .. 100000 {
			total += 'string data '.repeat(1 + i % 20).len
		}
		assert total > 0
		assert vgc_heap.gc_cycle > cycles
	}
}

// vgc_test_allocate_pages allocates n garbage objects that take one page (one span) each.
@[noinline]
fn vgc_test_allocate_pages(n int) {
	for _ in 0 .. n {
		_ = vgc_malloc_noscan(8192)
	}
}

fn test_every_span_is_swept() {
	$if vgc ? {
		// More spans than the 16384 that the collector used to track.
		vgc_test_allocate_pages(17000)
		gc_collect()
		used := C.vgc_atomic_load_u64(&vgc_heap.arena_used)
		// The spans of the garbage are free again, and new objects reuse them instead of
		// carving new spans out of the arenas.
		vgc_test_allocate_pages(17000)
		assert C.vgc_atomic_load_u64(&vgc_heap.arena_used) == used
	}
}

fn test_span_metadata_takes_little_memory() {
	$if vgc ? {
		// The earlier tests made about 20000 spans. Their metadata used three pages each.
		spans_bytes := C.vgc_atomic_load_u64(&vgc_heap.arena_used)
		assert spans_bytes > 100 * 1024 * 1024
		assert vgc_heap.meta_bytes < spans_bytes / 16 + u64(vgc_meta_chunk_size)
	}
}

fn test_the_heap_stays_bounded_with_a_bounded_live_set() {
	$if vgc ? {
		gc_collect()
		base := vgc_memory_use()
		mut ring := []string{len: 4096}
		mut peak := usize(0)
		// About 100 MB of strings in total, of which about 1 MB stays live.
		for i in 0 .. 400_000 {
			ring[i % ring.len] = 'bounded live set '.repeat(1 + i % 30)
			if i % 20_000 == 0 {
				in_use := vgc_memory_use()
				if in_use > peak {
					peak = in_use
				}
				gc_collect()
			}
		}
		assert peak < base + 32 * 1024 * 1024
		assert ring[399_999 % ring.len] == 'bounded live set '.repeat(1 + 399_999 % 30)
	}
}
