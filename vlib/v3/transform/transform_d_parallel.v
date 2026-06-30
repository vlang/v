module transform

import runtime
import v3.flat

// Parallel function-body transform. Compiled only when the build defines
// `parallel` (i.e. `-d parallel`), matching the parallel C codegen path. Each
// worker transforms a disjoint set of closure-free functions on its own cloned
// AST + forked TypeChecker, and the master folds the results back together in a
// fixed order so the build stays deterministic.

const min_parallel_transform_items = 256
const max_parallel_transform_jobs = 2

$if !windows {
	// TransformChunkArgs is the payload handed to each worker thread.
	struct TransformChunkArgs {
		worker    voidptr // &Transformer
		items_ptr voidptr // &[]FnWorkItem
	}

	// C.pthread_t declares C pthread t data used by transform.
	@[typedef]
	struct C.pthread_t {}

	// C.pthread_create declares the C pthread_create symbol used by transform.
	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int

	// C.pthread_join declares the C pthread_join symbol used by transform.
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int

	// C.pthread_attr_init declares the C pthread_attr_init symbol used by transform.
	fn C.pthread_attr_init(attr voidptr) int

	// C.pthread_attr_setstacksize declares the C pthread_attr_setstacksize symbol used by transform.
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int

	// C.pthread_attr_destroy declares the C pthread_attr_destroy symbol used by transform.
	fn C.pthread_attr_destroy(attr voidptr) int

	// transform_chunk_thread runs one worker's chunk of function bodies.
	fn transform_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &TransformChunkArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		items := unsafe { &[]FnWorkItem(a.items_ptr) }
		w.transform_pure_items_serial(*items)
		return unsafe { nil }
	}
}

// run_parallel_transform transforms the closure-free function bodies across
// threads when there is enough work, otherwise serially. Returns whether threads
// were actually used.
fn (mut t Transformer) run_parallel_transform(items []FnWorkItem, base_nodes int, base_children int) bool {
	$if windows {
		t.transform_pure_items_serial(items)
		return false
	} $else {
		n_jobs := transform_job_count(runtime.nr_jobs(), items.len)
		if items.len < min_parallel_transform_items || n_jobs <= 1 {
			t.transform_pure_items_serial(items)
			return false
		}
		mut chunks := split_work_items(items, n_jobs)
		chunk_count := chunks.len

		// chunk[0] is transformed by the master on this thread, directly against the
		// master AST — no clone. Only chunks[1..] get helper threads, each with a
		// private AST clone + forked TypeChecker. This removes one full base-AST clone
		// from the peak (each clone is ~one nodes-array; under -gc none they are never
		// freed, so they also inflate the later cgen peak) and keeps the master thread,
		// which would otherwise block in join, doing useful work.
		thread_count := chunk_count - 1
		mut workers := []voidptr{cap: thread_count}
		mut worker_asts := []voidptr{cap: thread_count}
		for _ in 0 .. thread_count {
			wast := t.clone_ast_base(base_nodes, base_children)
			worker_asts << voidptr(wast)
			wtc := t.tc.fork_for_parallel_transform(wast)
			ww := t.fork_worker(wast, wtc)
			workers << voidptr(ww)
		}
		mut args := []TransformChunkArgs{cap: thread_count}
		for ci in 0 .. thread_count {
			args << TransformChunkArgs{
				worker:    workers[ci]
				items_ptr: unsafe { voidptr(&chunks[ci + 1]) }
			}
		}

		mut thread_ids := []C.pthread_t{len: thread_count}
		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		// Transform recurses deeply on large expressions; give workers a roomy stack.
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci in 0 .. thread_count {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, transform_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		// Master transforms chunk[0] in place while the helper threads run. It only
		// touches its own functions' nodes and the master AST/TypeChecker, all disjoint
		// from the workers' clones, and never writes the shared (read-only) type tables.
		// Reset temp_counter to 0 like a freshly forked worker (transform temps are
		// function-local, so this is collision-free and matches the all-workers output).
		t.temp_counter = 0
		t.transform_pure_items_serial(chunks[0])
		for ci in 0 .. thread_count {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		// Merge helper results in fixed chunk order (chunk[0] is already in place), so
		// node numbering stays deterministic for reproducible builds.
		for ci in 0 .. thread_count {
			ww := unsafe { &Transformer(workers[ci]) }
			t.merge_worker_used_fns(ww)
			t.merge_worker(ww, chunks[ci + 1], base_nodes, base_children)
			// The worker's cloned base AST is no longer needed after merge.
			unsafe {
				mut wast := &flat.FlatAst(worker_asts[ci])
				wast.nodes.free()
				wast.children.free()
			}
		}
		return true
	}
}

// transform_job_count caps the worker count by both the runtime job count and a
// fixed ceiling (each worker clones the base AST, so more workers cost more memory).
fn transform_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_transform_jobs {
		n = max_parallel_transform_jobs
	}
	if n > n_items {
		n = n_items
	}
	return n
}
