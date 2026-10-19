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
