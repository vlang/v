module markused

// Parallel body-call precollection. Each worker analyzes a disjoint,
// contiguous range of fn_decl bodies with its own forked TypeChecker (private
// type_cache, so checker memoization never races) and writes into disjoint
// slots of a preallocated results array. The result content per body is a pure
// function of the (read-only) AST and checker tables, so the outcome is
// identical to the serial fallback.
import runtime
import v3.flat
import v3.types

const max_markused_jobs = 8
const min_markused_parallel_bodies = 512

$if !windows {
	// MarkusedChunkArgs is the payload handed to each worker thread. `imports`
	// is a map-header copy (shared, read-only backing) rather than a pointer:
	// the v3 self-host cgen cannot compile a `&map[K]V(ptr)` cast yet.
	struct MarkusedChunkArgs {
		collector    voidptr // &CallCollector
		body_ids_ptr voidptr // &[]int
		modules_ptr  voidptr // &[]string
		imports      map[string]string
		results_ptr  voidptr // &[]BodyCalls
		start        int
		end          int
	}

	// C.pthread_t declares C pthread t data used by markused.
	@[typedef]
	struct C.pthread_t {}

	// C.pthread_create declares the C pthread_create symbol used by markused.
	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int

	// C.pthread_join declares the C pthread_join symbol used by markused.
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int

	// C.pthread_attr_init declares the C pthread_attr_init symbol used by markused.
	fn C.pthread_attr_init(attr voidptr) int

	// C.pthread_attr_setstacksize declares the C pthread_attr_setstacksize symbol used by markused.
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int

	// C.pthread_attr_destroy declares the C pthread_attr_destroy symbol used by markused.
	fn C.pthread_attr_destroy(attr voidptr) int

	// markused_chunk_thread runs one worker's range of bodies.
	fn markused_chunk_thread(arg voidptr) voidptr {
		args := unsafe { &MarkusedChunkArgs(arg) }
		c := unsafe { &CallCollector(args.collector) }
		body_ids := unsafe { &[]int(args.body_ids_ptr) }
		modules := unsafe { &[]string(args.modules_ptr) }
		mut results := unsafe { &[]BodyCalls(args.results_ptr) }
		c.collect_bodies_range(*body_ids, *modules, args.imports, args.start, args.end, mut
			*results)
		return unsafe { nil }
	}
}

// precollect_body_calls analyzes every fn_decl body, across threads when there
// is enough work.
fn precollect_body_calls(collector CallCollector, body_ids []int, body_modules []string, imports map[string]string) []BodyCalls {
	mut results := []BodyCalls{len: body_ids.len}
	$if windows {
		collector.collect_bodies_range(body_ids, body_modules, imports, 0, body_ids.len, mut
			results)
		return results
	} $else {
		mut n_jobs := runtime.nr_jobs()
		if n_jobs > max_markused_jobs {
			n_jobs = max_markused_jobs
		}
		if body_ids.len < min_markused_parallel_bodies || n_jobs <= 1 {
			collector.collect_bodies_range(body_ids, body_modules, imports, 0, body_ids.len, mut
				results)
			return results
		}
		bounds := markused_chunk_bounds(collector.a, body_ids, n_jobs)
		thread_count := n_jobs - 1
		// Worker collectors share the read-only lookup maps but carry a forked
		// TypeChecker with a private type_cache (the only state the collectors
		// mutate through the checker).
		mut worker_collectors := []CallCollector{cap: thread_count}
		for _ in 0 .. thread_count {
			wtc := collector.tc.fork_for_parallel_transform(collector.a)
			worker_collectors << collector.fork_with_tc(wtc)
		}
		mut args := []MarkusedChunkArgs{cap: thread_count}
		for ci in 0 .. thread_count {
			args << MarkusedChunkArgs{
				collector:    unsafe { voidptr(&worker_collectors[ci]) }
				body_ids_ptr: unsafe { voidptr(&body_ids) }
				modules_ptr:  unsafe { voidptr(&body_modules) }
				imports:      imports
				results_ptr:  unsafe { voidptr(&results) }
				start:        bounds[ci + 1]
				end:          bounds[ci + 2]
			}
		}
		mut thread_ids := []C.pthread_t{len: thread_count}
		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		// The collectors recurse on deeply nested expressions; give workers a
		// roomy stack, like the transform and cgen workers.
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci in 0 .. thread_count {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, markused_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		// The master analyzes chunk 0 on this thread while the helpers run.
		collector.collect_bodies_range(body_ids, body_modules, imports, bounds[0], bounds[1], mut
			results)
		for ci in 0 .. thread_count {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		return results
	}
}

// markused_chunk_bounds splits the body list into n_jobs contiguous ranges of
// roughly equal node count. Bodies are in AST order, so the span to the next
// body id approximates each body's subtree size.
fn markused_chunk_bounds(a &flat.FlatAst, body_ids []int, n_jobs int) []int {
	mut total := i64(0)
	mut costs := []i64{cap: body_ids.len}
	for i, id in body_ids {
		next := if i + 1 < body_ids.len { body_ids[i + 1] } else { a.nodes.len }
		cost := i64(next - id)
		costs << if cost > 0 { cost } else { i64(1) }
		total += costs[i]
	}
	mut bounds := []int{cap: n_jobs + 1}
	bounds << 0
	mut acc := i64(0)
	mut chunk := 1
	for i in 0 .. body_ids.len {
		acc += costs[i]
		if chunk < n_jobs && acc >= total * i64(chunk) / i64(n_jobs) {
			bounds << i + 1
			chunk++
		}
	}
	for bounds.len < n_jobs + 1 {
		bounds << body_ids.len
	}
	return bounds
}
