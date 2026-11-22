module markused

// Parallel body-call precollection. The pool threads take pieces of the
// fn_decl body list from one queue. Each piece is analyzed with a forked
// TypeChecker (private type_cache, so checker memoization never races) and
// fills the result slots of its bodies, which no other piece writes. The result
// content per body is a pure function of the (read-only) AST and checker
// tables, so the outcome is identical to the serial fallback.
import os
import runtime
import v.flat
import v.workers

const max_markused_jobs = 17
const min_markused_parallel_bodies = 512
// The body list is cut into about this many pieces of similar size (see
// markused_precollect_pieces).
const markused_piece_count = 256

// MarkusedPrecollectShared is what the tasks of one parallel body collection
// share. The tasks only read it, apart from the result slots of the pieces
// they take.
struct MarkusedPrecollectShared {
	collector       voidptr // &CallCollector: every piece forks it
	body_ids        []int
	modules         []string
	import_contexts []int
	// Result slots in the order they are collected, and the start of each piece
	// in that order, followed by the end of the last one.
	order        []int
	piece_starts []int
	queue        chan int
mut:
	results []BodyCalls
}

// MarkusedPieceTaskArgs is the payload of one task: a thread that collects
// pieces until the queue is empty.
struct MarkusedPieceTaskArgs {
	shared &MarkusedPrecollectShared
mut:
	scope voidptr
}

// MarkusedPrecollect is one body collection: started by
// start_precollect_body_calls, which returns while the pool is still working
// when it is allowed to, then joined by `finish` and freed by `release`.
@[heap]
struct MarkusedPrecollect {
mut:
	collector CallCollector
	shared    &MarkusedPrecollectShared = unsafe { nil }
	// One payload per task, then one for the caller (see `help`).
	args    []MarkusedPieceTaskArgs
	tasks   []workers.Task
	threads []thread
	queue   chan int
	results []BodyCalls
}

// markused_piece_thread collects pieces of the body list until none is left.
// Every piece runs on a fork of the collector's checker inside a scratch arena
// that is rewound for the next one, instead of unmapping it and faulting in a
// fresh one. The call lists it found are copied into this task's result arena,
// which the caller frees after the reachability walk.
@[direct_array_access]
fn markused_piece_thread(arg voidptr) voidptr {
	mut args := unsafe { &MarkusedPieceTaskArgs(arg) }
	mut shared := unsafe { &MarkusedPrecollectShared(voidptr(args.shared)) }
	c := unsafe { &CallCollector(shared.collector) }
	result_scope := markused_worker_scope_begin(true)
	mut scratch_scope := unsafe { nil }
	for {
		piece := <-shared.queue or { break }
		start := shared.piece_starts[piece]
		end := shared.piece_starts[piece + 1]
		if scratch_scope == unsafe { nil } || !markused_worker_scope_reenter(scratch_scope) {
			markused_worker_scope_free(scratch_scope)
			scratch_scope = markused_worker_scope_begin(true)
		}
		batch_tc := c.tc.fork_for_parallel_transform(c.a)
		batch := c.fork_with_tc(batch_tc)
		for k in start .. end {
			slot := shared.order[k]
			node := c.a.node(flat.NodeId(shared.body_ids[slot]))
			shared.results[slot] = batch.collect_body(node, shared.modules[slot], batch.imports(shared.import_contexts[slot]))
		}
		markused_worker_scope_leave(scratch_scope)
		for k in start .. end {
			slot := shared.order[k]
			shared.results[slot] = clone_body_calls(shared.results[slot])
		}
	}
	markused_worker_scope_free(scratch_scope)
	markused_worker_scope_leave(result_scope)
	args.scope = result_scope
	return unsafe { nil }
}

// markused_precollect_dispatch runs the tasks of a body collection on the pool
// from a thread of its own, so the caller can work until it needs the results.
// The tasks and everything they point to belong to the caller.
fn markused_precollect_dispatch(pool &workers.Pool, tasks []workers.Task) {
	mut running_pool := unsafe { pool }
	running_pool.run(tasks)
}

// markused_precollect_pieces cuts the body list into pieces of about
// 1/markused_piece_count of its AST nodes each. It returns the result slots in
// collection order and the piece boundaries in that order. A body as large as a
// piece is one by itself, and those come first, the largest at the head: the
// queue then ends with small pieces, and no thread starts a long body while the
// others run out of work. The other bodies keep their AST order, so a piece
// mostly holds declarations of one file.
@[direct_array_access]
fn markused_precollect_pieces(a &flat.FlatAst, body_ids []int) ([]int, []int) {
	mut total := i64(0)
	mut costs := []i64{cap: body_ids.len}
	for i, id in body_ids {
		next := if i + 1 < body_ids.len { body_ids[i + 1] } else { a.nodes.len }
		cost := i64(next - id)
		costs << if cost > 0 { cost } else { i64(1) }
		total += costs[i]
	}
	mut target := total / markused_piece_count
	if target < 1 {
		target = 1
	}
	// Sort keys of the large bodies: by cost, then the earlier body first.
	mut large := []u64{}
	for i in 0 .. body_ids.len {
		if costs[i] >= target {
			large << (u64(costs[i]) << 32) | u64(u32(0xffff_ffff) - u32(i))
		}
	}
	large.sort(a > b)
	mut order := []int{cap: body_ids.len}
	mut piece_starts := []int{cap: markused_piece_count * 2 + large.len + 2}
	for key in large {
		piece_starts << order.len
		order << int(u32(0xffff_ffff) - u32(key & 0xffff_ffff))
	}
	mut acc := i64(0)
	mut open := false
	for i in 0 .. body_ids.len {
		if costs[i] >= target {
			continue
		}
		if !open {
			piece_starts << order.len
			open = true
			acc = 0
		}
		order << i
		acc += costs[i]
		if acc >= target {
			open = false
		}
	}
	piece_starts << order.len
	return order, piece_starts
}

// MarkusedChunkArgs is the payload handed to each thread of the static split.
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
@[direct_array_access; unsafe]
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

// markused_batch_arena_keep_bytes bounds how much of a batch arena stays mapped
// for the next batch (see prealloc_scope_reenter).
const markused_batch_arena_keep_bytes = int(8) * 1024 * 1024

// markused_worker_scope_reenter makes a left batch arena current again, rewound.
// False means the caller needs a new arena.
@[unsafe]
fn markused_worker_scope_reenter(scope voidptr) bool {
	$if prealloc {
		if scope != unsafe { nil } {
			return unsafe { prealloc_scope_reenter(scope, markused_batch_arena_keep_bytes) }
		}
	}
	return false
}

@[unsafe]
fn markused_worker_scope_begin(enabled bool) voidptr {
	$if prealloc {
		if enabled {
			return unsafe { prealloc_scope_begin() }
		}
	}
	return unsafe { nil }
}

@[unsafe]
fn markused_worker_scope_leave(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_leave(scope) }
		}
	}
}

@[unsafe]
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
		call_keys:     value.call_keys.clone()
		refs:          refs
		uses_generics: value.uses_generics
	}
}

// Only eager-collect prepared bodies when helpers can offset scanning unreachable bodies.
fn markused_can_parallel_precollect(collector &CallCollector) bool {
	return collector.a.worker_count() > 0
		|| (collector.tc.scoped_parallel_workers_enabled() && runtime.nr_jobs() > 1)
}

// start_precollect_body_calls analyzes every fn_decl body, across threads when
// there is enough work. With scoped workers and `allow_async`, it returns while
// the pool threads are still collecting: until `finish` returns, the caller must
// not query the collector's checker (every piece forks it) and may only use a
// fork of its own. Otherwise the results are complete on return.
fn start_precollect_body_calls(collector CallCollector, body_ids []int, body_modules []string, body_import_contexts []int, allow_async bool) &MarkusedPrecollect {
	mut pre := &MarkusedPrecollect{
		collector: collector
		results:   []BodyCalls{len: body_ids.len}
	}
	mut ast := unsafe { collector.a }
	scope_workers := collector.tc.scoped_parallel_workers_enabled()
	if isnil(ast.worker_pool) && !scope_workers {
		collector.collect_bodies_range(body_ids, body_modules, body_import_contexts, 0,
			body_ids.len, mut pre.results)
		return pre
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
			body_ids.len, mut pre.results)
		return pre
	}
	if !scope_workers {
		precollect_body_calls_static(collector, body_ids, body_modules, body_import_contexts,
			n_jobs, &pre.results)
		return pre
	}
	// Scoped pieces fork a private checker each, so their immutable outer
	// collector can be shared.
	order, piece_starts := markused_precollect_pieces(collector.a, body_ids)
	piece_count := piece_starts.len - 1
	queue := chan int{cap: piece_count}
	for piece in 0 .. piece_count {
		queue <- piece
	}
	queue.close()
	pre.shared = &MarkusedPrecollectShared{
		collector:       unsafe { voidptr(&pre.collector) }
		body_ids:        body_ids
		modules:         body_modules
		import_contexts: body_import_contexts
		order:           order
		piece_starts:    piece_starts
		queue:           queue
		results:         pre.results
	}
	pre.queue = queue
	fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
	pre.args = []MarkusedPieceTaskArgs{cap: n_jobs + 1}
	pre.tasks = []workers.Task{cap: n_jobs}
	for _ in 0 .. n_jobs + 1 {
		pre.args << MarkusedPieceTaskArgs{
			shared: pre.shared
		}
	}
	// Without a dispatch thread the caller takes pieces as one of the tasks.
	async := allow_async && fail.len == 0
	for ci in 0 .. n_jobs {
		helper_idx := ci - 1
		pre.tasks << workers.Task{
			run:        markused_piece_thread
			arg:        unsafe { voidptr(&pre.args[ci]) }
			force_sync: (ci == 0 && !async) || fail == 'markused:all'
				|| fail == 'markused:${helper_idx}'
		}
	}
	if async {
		pre.threads << spawn markused_precollect_dispatch(ast.worker_pool, pre.tasks)
	} else {
		ast.worker_pool.run(pre.tasks)
	}
	return pre
}

// is_async reports whether pool threads may still be collecting.
fn (pre &MarkusedPrecollect) is_async() bool {
	return pre.threads.len > 0
}

// pool_busy reports whether an asynchronous collection still has pieces that
// no thread has taken. The caller's own work is free until then.
fn (mut pre MarkusedPrecollect) pool_busy() bool {
	return pre.threads.len > 0 && pre.queue.len > 0
}

// help makes the caller take pieces of an asynchronous collection, like the
// pool threads, until none is left.
fn (mut pre MarkusedPrecollect) help() {
	if pre.threads.len > 0 {
		markused_piece_thread(unsafe { voidptr(&pre.args[pre.args.len - 1]) })
	}
}

// finish waits for the pool threads and returns the call lists of every body.
// They stay valid until `release`.
fn (mut pre MarkusedPrecollect) finish() []BodyCalls {
	for pre.threads.len > 0 {
		pre.threads.pop().wait()
	}
	return pre.results
}

// release frees the arenas that hold the call lists.
fn (mut pre MarkusedPrecollect) release() {
	for mut arg in pre.args {
		markused_worker_scope_free(arg.scope)
		arg.scope = unsafe { nil }
	}
}

// precollect_body_calls_static collects contiguous ranges of bodies on worker
// collectors that each carry a forked TypeChecker with a private type_cache (the
// only state the collectors mutate through the checker). The caller's thread
// takes the first range with the collector itself.
fn precollect_body_calls_static(collector CallCollector, body_ids []int, body_modules []string, body_import_contexts []int, n_jobs int, results &[]BodyCalls) {
	mut ast := unsafe { collector.a }
	chunk_count := n_jobs
	bounds := markused_chunk_bounds(collector.a, body_ids, chunk_count)
	thread_count := chunk_count - 1
	mut worker_collectors := []CallCollector{cap: thread_count}
	for _ in 0 .. thread_count {
		wtc := collector.tc.fork_for_parallel_transform(collector.a)
		worker_collectors << collector.fork_with_tc(wtc)
	}
	mut master_collector := collector
	mut args := []MarkusedChunkArgs{cap: chunk_count}
	args << MarkusedChunkArgs{
		collector:          unsafe { voidptr(&master_collector) }
		body_ids_ptr:       unsafe { voidptr(&body_ids) }
		modules_ptr:        unsafe { voidptr(&body_modules) }
		import_context_ptr: unsafe { voidptr(&body_import_contexts) }
		results_ptr:        unsafe { voidptr(results) }
		start:              bounds[0]
		end:                bounds[1]
	}
	for ci in 0 .. thread_count {
		args << MarkusedChunkArgs{
			collector:          unsafe { voidptr(&worker_collectors[ci]) }
			body_ids_ptr:       unsafe { voidptr(&body_ids) }
			modules_ptr:        unsafe { voidptr(&body_modules) }
			import_context_ptr: unsafe { voidptr(&body_import_contexts) }
			results_ptr:        unsafe { voidptr(results) }
			start:              bounds[ci + 1]
			end:                bounds[ci + 2]
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
