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

const max_markused_jobs = 17
const min_markused_parallel_bodies = 512
const scoped_markused_chunk_oversubscribe = 2
const scoped_markused_worker_batches = 4

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
		scope_enabled      bool
	mut:
		scope voidptr
	}

	// markused_chunk_thread runs one worker's range of bodies.
	@[direct_array_access]
	fn markused_chunk_thread(arg voidptr) voidptr {
		mut args := unsafe { &MarkusedChunkArgs(arg) }
		args.scope = markused_worker_scope_begin(args.scope_enabled)
		c := unsafe { &CallCollector(args.collector) }
		body_ids := unsafe { &[]int(args.body_ids_ptr) }
		modules := unsafe { &[]string(args.modules_ptr) }
		import_contexts := unsafe { &[]int(args.import_context_ptr) }
		mut results := unsafe { &[]BodyCalls(args.results_ptr) }
		if args.scope_enabled {
			c.collect_bodies_scoped_batches(*body_ids, *modules, *import_contexts, args.start,
				args.end, mut *results)
		} else {
			c.collect_bodies_range(*body_ids, *modules, *import_contexts, args.start, args.end, mut
				*results)
		}
		markused_worker_scope_leave(args.scope)
		if args.scope != unsafe { nil } {
			// Publish this worker's call lists into its persistent arena here,
			// in parallel, instead of the former serial clone loop on the
			// master; the freed scope is marked nil so the master skips it.
			for result_idx in args.start .. args.end {
				unsafe {
					(*results)[result_idx] = clone_body_calls((*results)[result_idx])
				}
			}
			markused_worker_scope_free(args.scope)
			args.scope = unsafe { nil }
		}
		return unsafe { nil }
	}
}

// collect_bodies_scoped_batches keeps call lists in the worker's result arena,
// while repeatedly releasing the much larger checker and local-analysis
// scratch state used to discover them.
@[direct_array_access]
fn (c &CallCollector) collect_bodies_scoped_batches(body_ids []int, body_modules []string, body_import_contexts []int, range_start int, range_end int, mut results []BodyCalls) {
	item_count := range_end - range_start
	if item_count <= 0 {
		return
	}
	max_batches := scoped_markused_worker_batches
	n_batches := if item_count < max_batches {
		item_count
	} else {
		max_batches
	}
	for batch_idx in 0 .. n_batches {
		start := range_start + item_count * batch_idx / n_batches
		end := range_start + item_count * (batch_idx + 1) / n_batches
		scratch_scope := markused_worker_scope_begin(true)
		batch_tc := c.tc.fork_for_parallel_transform(c.a)
		batch := c.fork_with_tc(batch_tc)
		batch.collect_bodies_range(body_ids, body_modules, body_import_contexts, start, end, mut
			results)
		markused_worker_scope_leave(scratch_scope)
		for result_idx in start .. end {
			results[result_idx] = clone_body_calls(results[result_idx])
		}
		markused_worker_scope_free(scratch_scope)
	}
}

fn markused_worker_scope_begin(enabled bool) voidptr {
	$if prealloc {
		if enabled {
			return unsafe { prealloc_scope_begin() }
		}
	}
	return unsafe { nil }
}

fn markused_worker_scope_leave(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_leave(scope) }
		}
	}
}

fn markused_worker_scope_free(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_free_after(scope) }
		}
	}
}

fn clone_body_calls(value BodyCalls) BodyCalls {
	mut calls := []string{cap: value.calls.len}
	for call in value.calls {
		calls << call.clone()
	}
	mut refs := []string{cap: value.refs.len}
	for ref in value.refs {
		refs << ref.clone()
	}
	return BodyCalls{
		calls:         calls
		refs:          refs
		uses_generics: value.uses_generics
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
		scope_workers := collector.tc.scoped_parallel_workers_enabled()
		if isnil(ast.worker_pool) && !scope_workers {
			collector.collect_bodies_range(body_ids, body_modules, body_import_contexts, 0,
				body_ids.len, mut results)
			return results
		}
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
		// Scoped chunks fork a private checker for each scratch batch, so their
		// immutable outer collector can be shared. Queue twice as many chunks as
		// cores to smooth out construct-heavy bodies whose cost is not captured
		// by source-node span.
		mut chunk_count := n_jobs
		if scope_workers {
			chunk_count *= scoped_markused_chunk_oversubscribe
			if chunk_count > body_ids.len {
				chunk_count = body_ids.len
			}
		}
		bounds := markused_chunk_bounds(collector.a, body_ids, chunk_count)
		thread_count := chunk_count - 1
		// Worker collectors share the read-only lookup maps but carry a forked
		// TypeChecker with a private type_cache (the only state the collectors
		// mutate through the checker).
		mut worker_collectors := []CallCollector{cap: thread_count}
		if !scope_workers {
			for _ in 0 .. thread_count {
				wtc := collector.tc.fork_for_parallel_transform(collector.a)
				worker_collectors << collector.fork_with_tc(wtc)
			}
		}
		mut master_collector := collector
		mut args := []MarkusedChunkArgs{cap: chunk_count}
		args << MarkusedChunkArgs{
			collector:          unsafe { voidptr(&master_collector) }
			body_ids_ptr:       unsafe { voidptr(&body_ids) }
			modules_ptr:        unsafe { voidptr(&body_modules) }
			import_context_ptr: unsafe { voidptr(&body_import_contexts) }
			results_ptr:        unsafe { voidptr(&results) }
			start:              bounds[0]
			end:                bounds[1]
			scope_enabled:      scope_workers
		}
		for ci in 0 .. thread_count {
			mut worker_collector := unsafe { voidptr(&master_collector) }
			if !scope_workers {
				worker_collector = unsafe { voidptr(&worker_collectors[ci]) }
			}
			args << MarkusedChunkArgs{
				collector:          worker_collector
				body_ids_ptr:       unsafe { voidptr(&body_ids) }
				modules_ptr:        unsafe { voidptr(&body_modules) }
				import_context_ptr: unsafe { voidptr(&body_import_contexts) }
				results_ptr:        unsafe { voidptr(&results) }
				start:              bounds[ci + 1]
				end:                bounds[ci + 2]
				scope_enabled:      scope_workers
			}
		}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        markused_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'markused:all' || fail == 'markused:${helper_idx}'
			}
		}
		ast.worker_pool.run(tasks)
		for ci in 0 .. chunk_count {
			if args[ci].scope == unsafe { nil } {
				continue
			}
			for result_idx in args[ci].start .. args[ci].end {
				results[result_idx] = clone_body_calls(results[result_idx])
			}
			markused_worker_scope_free(args[ci].scope)
		}
		return results
	}
}

// markused_chunk_bounds splits the body list into n_jobs contiguous ranges of
// roughly equal node count. Bodies are in AST order, so the span to the next
// body id approximates each body's subtree size.
@[direct_array_access]
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
