module markused

// Parallel body-call precollection. Each worker analyzes a disjoint,
// contiguous range of fn_decl bodies with its own forked TypeChecker (private
// type_cache, so checker memoization never races) and writes into disjoint
// slots of a preallocated results array. The result content per body is a pure
// function of the (read-only) AST and checker tables, so the outcome is
// identical to the serial fallback.
import os
import runtime
import v3.flat
import v3.workers

const max_markused_jobs = 8
const min_markused_parallel_bodies = 512

$if !windows {
	// MarkusedChunkArgs is the payload handed to each worker thread.
	struct MarkusedChunkArgs {
		collector          voidptr // &CallCollector
		body_ids_ptr       voidptr // &[]int
		modules_ptr        voidptr // &[]string
		import_context_ptr voidptr // &[]int
		results_ptr        voidptr // &[]BodyCalls
		start              int
		end                int
	}

	// markused_chunk_thread runs one worker's range of bodies.
	fn markused_chunk_thread(arg voidptr) voidptr {
		args := unsafe { &MarkusedChunkArgs(arg) }
		c := unsafe { &CallCollector(args.collector) }
		body_ids := unsafe { &[]int(args.body_ids_ptr) }
		modules := unsafe { &[]string(args.modules_ptr) }
		import_contexts := unsafe { &[]int(args.import_context_ptr) }
		mut results := unsafe { &[]BodyCalls(args.results_ptr) }
		c.collect_bodies_range(*body_ids, *modules, *import_contexts, args.start, args.end, mut
			*results)
		return unsafe { nil }
	}
}

// precollect_body_calls analyzes every fn_decl body, across threads when there
// is enough work.
fn precollect_body_calls(collector CallCollector, body_ids []int, body_modules []string, body_import_contexts []int) []BodyCalls {
	mut results := []BodyCalls{len: body_ids.len}
	$if windows {
		collector.collect_bodies_range(body_ids, body_modules, body_import_contexts, 0,
			body_ids.len, mut results)
		return results
	} $else {
		mut ast := unsafe { collector.a }
		if isnil(ast.worker_pool) {
			ast.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		mut n_jobs := ast.worker_pool.size() + 1
		if n_jobs > max_markused_jobs {
			n_jobs = max_markused_jobs
		}
		if body_ids.len < min_markused_parallel_bodies || n_jobs <= 1 {
			collector.collect_bodies_range(body_ids, body_modules, body_import_contexts, 0,
				body_ids.len, mut results)
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
		mut master_collector := collector
		mut args := []MarkusedChunkArgs{cap: n_jobs}
		args << MarkusedChunkArgs{
			collector:          unsafe { voidptr(&master_collector) }
			body_ids_ptr:       unsafe { voidptr(&body_ids) }
			modules_ptr:        unsafe { voidptr(&body_modules) }
			import_context_ptr: unsafe { voidptr(&body_import_contexts) }
			results_ptr:        unsafe { voidptr(&results) }
			start:              bounds[0]
			end:                bounds[1]
		}
		for ci in 0 .. thread_count {
			args << MarkusedChunkArgs{
				collector:          unsafe { voidptr(&worker_collectors[ci]) }
				body_ids_ptr:       unsafe { voidptr(&body_ids) }
				modules_ptr:        unsafe { voidptr(&body_modules) }
				import_context_ptr: unsafe { voidptr(&body_import_contexts) }
				results_ptr:        unsafe { voidptr(&results) }
				start:              bounds[ci + 1]
				end:                bounds[ci + 2]
			}
		}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: n_jobs}
		for ci in 0 .. n_jobs {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        markused_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'markused:all' || fail == 'markused:${helper_idx}'
			}
		}
		ast.worker_pool.run(tasks)
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
