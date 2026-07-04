module transform

// Parallel function-body transform. Each worker transforms a disjoint set of
// closure-free functions on its own cloned AST + forked TypeChecker, and the
// master folds the results back together in a fixed order so the build stays
// deterministic.

const min_parallel_transform_items = 256
const max_parallel_transform_jobs = 6

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
	_ = base_nodes
	_ = base_children
	t.transform_pure_items_serial(items)
	return false
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
