module transform

import os
import runtime
import v3.flat
import v3.workers

// Parallel function-body transform. Each worker transforms a disjoint set of
// closure-free functions on its own cloned AST + forked TypeChecker, and the
// master folds the results back together in a fixed order so the build stays
// deterministic.

const min_parallel_transform_items = 256
const max_parallel_transform_jobs = 6
// Shared-base (clone-free) transform: workers share the master arrays and
// append into pre-partitioned capacity regions, so extra threads cost no
// clone memory; cap by core count only.
const max_shared_transform_jobs = 7
const scoped_transform_worker_batches = 48
const scoped_transform_master_batches = 48

$if !windows {
	// TransformChunkArgs is the payload handed to each persistent worker.
	struct TransformChunkArgs {
		worker    voidptr // &Transformer
		items_ptr voidptr // &[]FnWorkItem
	}

	// transform_chunk_thread runs one worker's chunk of function bodies.
	fn transform_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &TransformChunkArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		items := unsafe { &[]FnWorkItem(a.items_ptr) }
		w.transform_pure_items_serial(*items)
		return unsafe { nil }
	}

	// shared_chunk_thread runs one shared-base worker's chunk. No clone, no
	// chain: every worker was fully built by the master before spawning.
	fn shared_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &SharedChunkArgs(arg) }
		mut w := unsafe { &Transformer(a.worker) }
		items := unsafe { &[]FnWorkItem(a.items_ptr) }
		if w.scope_parallel_workers && (!a.is_master || w.retain_worker_results) {
			max_batches := if a.is_master {
				scoped_transform_master_batches
			} else {
				scoped_transform_worker_batches
			}
			w.transform_scoped_helper_batches(*items, max_batches)
		} else {
			w.transform_pure_items_serial(*items)
		}
		return unsafe { nil }
	}
}

// SharedChunkArgs is the payload handed to each shared-base worker thread.
struct SharedChunkArgs {
	worker    voidptr // &Transformer
	items_ptr voidptr // &[]FnWorkItem
	is_master bool
}

$if !windows {
	struct InterfaceBoxScanArgs {
		source voidptr // &Transformer
		start  int
		end    int
		file   string
		module string
	mut:
		worker voidptr // &Transformer
		scope  voidptr
	}

	fn interface_box_scan_thread(arg voidptr) voidptr {
		mut a := unsafe { &InterfaceBoxScanArgs(arg) }
		source := unsafe { &Transformer(a.source) }
		scope := transform_worker_scope_begin(source.scope_parallel_workers)
		wtc := source.tc.fork_for_parallel_transform(source.a)
		mut scan := source.fork_scan_worker(wtc)
		scan.cur_file = a.file
		scan.cur_module = a.module
		scan.tc.cur_file = a.file
		scan.tc.cur_module = a.module
		scan.interface_boxed_types_done = true
		scan.collect_interface_boxed_types_range(a.start, a.end)
		transform_worker_scope_leave(scope)
		a.worker = voidptr(scan)
		a.scope = scope
		return unsafe { nil }
	}
}

// collect_interface_boxed_types_parallel scans independent AST ranges with
// private checker context, then publishes only the small boxed-type set.
fn (mut t Transformer) collect_interface_boxed_types_parallel() bool {
	$if windows {
		return false
	} $else {
		if t.a.nodes.len < 4096 {
			return false
		}
		if isnil(t.a.worker_pool) {
			t.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		mut n_jobs := t.a.worker_pool.size() + 1
		if n_jobs > max_shared_transform_jobs {
			n_jobs = max_shared_transform_jobs
		}
		if n_jobs <= 1 {
			return false
		}
		mut bounds := []int{len: n_jobs + 1}
		for i in 0 .. n_jobs + 1 {
			bounds[i] = t.a.nodes.len * i / n_jobs
		}
		mut files := []string{len: n_jobs}
		mut modules := []string{len: n_jobs}
		mut next_bound := 0
		mut file := ''
		mut module_name := ''
		for idx, node in t.a.nodes {
			for next_bound < n_jobs && bounds[next_bound] == idx {
				files[next_bound] = file
				modules[next_bound] = module_name
				next_bound++
			}
			if node.kind == .file {
				file = node.value
				module_name = t.tc.file_modules[file] or { '' }
			} else if node.kind == .module_decl {
				module_name = node.value
			}
		}
		mut args := []InterfaceBoxScanArgs{len: n_jobs}
		mut tasks := []workers.Task{cap: n_jobs}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		t.tc.freeze_type_cache_for_forks()
		for i in 0 .. n_jobs {
			args[i] = InterfaceBoxScanArgs{
				source: voidptr(t)
				start:  bounds[i]
				end:    bounds[i + 1]
				file:   files[i]
				module: modules[i]
			}
			tasks << workers.Task{
				run:        interface_box_scan_thread
				arg:        unsafe { voidptr(&args[i]) }
				force_sync: i == 0 || fail == 'transform:all' || fail == 'transform:interface:all'
					|| fail == 'transform:interface:${i - 1}'
			}
		}
		t.a.worker_pool.run(tasks)
		t.tc.unfreeze_type_cache_after_forks()
		mut boxed_types := map[string]bool{}
		for arg in args {
			scan := unsafe { &Transformer(arg.worker) }
			for key, value in scan.interface_boxed_types {
				if value {
					boxed_types[key.clone()] = true
				}
			}
			transform_worker_scope_free(arg.scope)
		}
		t.interface_boxed_types = boxed_types.move()
		t.interface_boxed_types_done = true
		t.interface_boxed_types_frozen = true
		return true
	}
}

// promote_scoped_node_to_current copies only fields owned by `scope`. The
// caller has already left the scratch scope, so clones land in its small result
// arena and survive until the master merges this worker.
fn (mut t Transformer) promote_scoped_node_to_current(idx int, scope voidptr) {
	if idx < 0 || idx >= t.a.nodes.len {
		return
	}
	mut node := unsafe { &t.a.nodes[idx] }
	if node.value.len > 0 && (transform_scope_owns(scope, node.value.str)
		|| transform_scope_owns(t.stage_scope, node.value.str)) {
		node.value = t.promote_scoped_result_text(node.value)
	}
	if node.typ.len > 0 && (transform_scope_owns(scope, node.typ.str)
		|| transform_scope_owns(t.stage_scope, node.typ.str)) {
		node.typ = t.promote_scoped_result_text(node.typ)
	}
	old_params := node.generic_params()
	if old_params.len == 0 {
		return
	}
	mut needs_owned_params := transform_scope_owns(scope, node.payload)
		|| transform_scope_owns(scope, old_params.data)
		|| transform_scope_owns(t.stage_scope, node.payload)
		|| transform_scope_owns(t.stage_scope, old_params.data)
	if !needs_owned_params {
		for param in old_params {
			if param.len > 0 && (transform_scope_owns(scope, param.str)
				|| transform_scope_owns(t.stage_scope, param.str)) {
				needs_owned_params = true
				break
			}
		}
	}
	if !needs_owned_params {
		return
	}
	mut params := []string{cap: old_params.len}
	for param in old_params {
		if param.len > 0 && (transform_scope_owns(scope, param.str)
			|| transform_scope_owns(t.stage_scope, param.str)) {
			params << t.promote_scoped_result_text(param)
		} else {
			params << param
		}
	}
	node.set_generic_params(params)
}

fn (mut t Transformer) promote_scoped_result_text(value string) string {
	if value.len == 0 {
		return ''
	}
	// Scoped self-host workers only read the parse-time text table. Reuse those
	// compilation-owned strings before adding a worker-local canonical entry.
	if t.retain_worker_results {
		if id := t.a.text_ids[value] {
			return t.a.text_values[int(id) - 1]
		}
	}
	if canonical := t.scoped_promoted_texts[value] {
		return canonical
	}
	canonical := value.clone()
	t.scoped_promoted_texts[canonical] = canonical
	return canonical
}

// absorb_scoped_batch publishes one batch's observable state into the helper's
// result arena before its large scratch arena is released.
fn (mut t Transformer) absorb_scoped_batch(batch &Transformer, scope voidptr, new_node_start int) {
	for idx in new_node_start .. batch.a.nodes.len {
		t.promote_scoped_node_to_current(idx, scope)
	}
	for idx in batch.scoped_owned_base_nodes.keys() {
		t.promote_scoped_node_to_current(idx, scope)
		t.scoped_owned_base_log << idx
	}
	for idx in batch.scoped_owned_base_log {
		t.promote_scoped_node_to_current(idx, scope)
		t.scoped_owned_base_log << idx
	}
	for name in batch.used_fns_log {
		t.used_fns[t.promote_scoped_result_text(name)] = true
	}
	for name, req in batch.sum_eq_types {
		if name !in t.sum_eq_types {
			t.sum_eq_types[t.promote_scoped_result_text(name)] = SumEqRequest{
				module:        t.promote_scoped_result_text(req.module)
				file:          t.promote_scoped_result_text(req.file)
				helper_module: t.promote_scoped_result_text(req.helper_module)
			}
		}
	}
	for message in batch.monomorph_errors {
		t.monomorph_errors << t.promote_scoped_result_text(message)
	}
	deferred_start := t.deferred_base_writes.len
	for write in batch.deferred_base_writes {
		t.deferred_base_writes << write
	}
	t.clone_deferred_worker_writes_from(deferred_start)
	if !isnil(batch.tc.fork_overlay) {
		for idx, name in batch.tc.fork_overlay.resolved_call_names {
			owned_name := t.promote_scoped_result_text(name)
			if isnil(t.tc.fork_overlay) {
				t.set_resolved_call_entry(idx, owned_name)
			} else {
				t.tc.fork_overlay.resolved_call_names[idx] = owned_name
			}
		}
		for idx, name in batch.tc.fork_overlay.resolved_fn_values {
			owned_name := t.promote_scoped_result_text(name)
			if isnil(t.tc.fork_overlay) {
				t.set_resolved_fn_value_entry(idx, owned_name)
			} else {
				t.tc.fork_overlay.resolved_fn_values[idx] = owned_name
			}
		}
	}
	if batch.ignored_comptime_for_nodes.len > 0 {
		for idx, ignored in batch.ignored_comptime_for_nodes {
			if ignored {
				t.ignored_comptime_for_log << idx
			}
		}
	}
	t.ignored_comptime_for_log << batch.ignored_comptime_for_log
}

// transform_scoped_helper_batches keeps one worker-pool dispatch but bounds
// scratch lifetime within each helper. A fresh Transformer/TypeChecker fork per
// batch prevents caches from retaining pointers into the released arena.
fn (mut t Transformer) transform_scoped_helper_batches(items []FnWorkItem, max_batches int) {
	result_scope := transform_worker_scope_begin(true)
	mut total_cost := i64(0)
	for item in items {
		total_cost += i64(item.cost) + 1
	}
	n_batches := if items.len < max_batches {
		items.len
	} else {
		max_batches
	}
	mut start := 0
	mut consumed_cost := i64(0)
	for batch_idx in 0 .. n_batches {
		mut end := start
		target_cost := total_cost * i64(batch_idx + 1) / i64(n_batches)
		for end < items.len
			&& (batch_idx == n_batches - 1 || consumed_cost < target_cost || end == start) {
			consumed_cost += i64(items[end].cost) + 1
			end++
		}
		scratch_scope := transform_worker_scope_begin(true)
		batch_tc := t.tc.fork_for_parallel_transform(t.a)
		mut batch := t.fork_scoped_batch_worker(t.a, batch_tc)
		batch.used_fns_log_active = true
		batch.scoped_base_log_active = true
		batch.ignored_comptime_log_active = true
		new_node_start := t.a.nodes.len
		batch.transform_pure_items_serial(items[start..end])
		transform_worker_scope_leave(scratch_scope)
		t.absorb_scoped_batch(batch, scratch_scope, new_node_start)
		transform_worker_scope_free(scratch_scope)
		start = end
	}
	t.worker_scope = result_scope
	transform_worker_scope_leave(result_scope)
}

// clone_deferred_worker_writes_from moves writes queued by merge_worker out of
// a helper arena before that arena is released. In the shared-base path the
// rewritten top-level fn node is deferred until every worker has joined, so
// cloning the current master slot alone does not preserve its owned strings.
fn (mut t Transformer) clone_deferred_worker_writes_from(start int) {
	for i in start .. t.deferred_base_writes.len {
		write := t.deferred_base_writes[i]
		t.deferred_base_writes[i] = match write.kind {
			0, 1 {
				DeferredBaseWrite{
					idx:  write.idx
					kind: write.kind
					str:  t.promote_scoped_result_text(write.str)
				}
			}
			2 {
				mut params := []string{cap: write.node.generic_params().len}
				for param in write.node.generic_params() {
					params << t.promote_scoped_result_text(param)
				}
				DeferredBaseWrite{
					idx:  write.idx
					kind: write.kind
					node: flat.Node{
						value:          t.promote_scoped_result_text(write.node.value)
						typ:            t.promote_scoped_result_text(write.node.typ)
						payload:        flat.node_payload(params)
						pos:            write.node.pos
						children_start: write.node.children_start
						children_count: write.node.children_count
						kind:           write.node.kind
						op:             write.node.op
						is_mut:         write.node.is_mut
					}
				}
			}
			else {
				mut params := []string{cap: write.gparams.len}
				for param in write.gparams {
					params << t.promote_scoped_result_text(param)
				}
				DeferredBaseWrite{
					idx:     write.idx
					kind:    write.kind
					gparams: params
				}
			}
		}
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
		if isnil(t.a.worker_pool) {
			t.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		n_jobs := transform_job_count(t.a.worker_pool.size() + 1, items.len)
		if items.len < min_parallel_transform_items || n_jobs <= 1 {
			t.transform_pure_items_serial(items)
			return false
		}
		// Workers need declaration signatures while lowering calls. Snapshot them
		// before any worker can rewrite a shared-base fn_decl; lazily scanning or
		// reading declarations inside workers can otherwise observe a torn node.
		t.prepare_parallel_call_param_types()
		// Clone-free shared-base path: needs the checker's top-level index for
		// exact per-item subtree ranges, and skip_generics (the generic passes
		// scan and mutate arbitrary AST regions, which the shared design forbids).
		if t.skip_generics && !isnil(t.tc) && t.tc.top_level_idx.len > 0 {
			shared_jobs := shared_transform_job_count(t.a.worker_pool.size() + 1, items.len)
			if shared_jobs > 1 {
				return t.run_parallel_transform_shared(items, base_nodes, base_children,
					shared_jobs)
			}
		}
		// Freeze the checker's warm type cache (fully populated by the check
		// phase) as the shared read-only base for every worker fork, so workers
		// do not re-parse every type text from a cold cache; the master itself
		// writes through a private overlay for the duration of the region.
		t.tc.freeze_type_cache_for_forks()
		mut chunks := split_work_items(items, n_jobs)
		chunk_count := chunks.len

		// chunk[0] is transformed by the master on this thread, directly against the
		// master AST — no clone. Only chunks[1..] get helper threads, each with a
		// private AST clone + forked TypeChecker. This removes one full base-AST clone
		// from the peak (each clone is ~one nodes-array; under -gc none they are never
		// freed, so they also inflate the later cgen peak) and keeps the master thread,
		// which would otherwise block in join, doing useful work.
		thread_count := chunk_count - 1
		mut transform_workers := []voidptr{cap: thread_count}
		mut args := []TransformChunkArgs{cap: chunk_count}
		args << TransformChunkArgs{
			worker:    voidptr(t)
			items_ptr: unsafe { voidptr(&chunks[0]) }
		}
		for ci in 0 .. thread_count {
			wast := t.clone_ast_base(base_nodes, base_children)
			wtc := t.tc.fork_for_parallel_transform(wast)
			ww := t.fork_worker(wast, wtc)
			transform_workers << voidptr(ww)
			args << TransformChunkArgs{
				worker:    voidptr(ww)
				items_ptr: unsafe { voidptr(&chunks[ci + 1]) }
			}
		}
		t.temp_counter = 0
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        transform_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'transform:all' || fail == 'transform:${helper_idx}'
			}
		}
		any_started := t.a.worker_pool.run(tasks)
		// Merge each helper in fixed chunk order for deterministic node ids.
		for ci in 0 .. thread_count {
			ww := unsafe { &Transformer(transform_workers[ci]) }
			t.merge_worker_used_fns(ww)
			t.merge_worker(ww, chunks[ci + 1], base_nodes, base_children)
		}
		t.tc.unfreeze_type_cache_after_forks()
		return any_started
	}
}

// shared_transform_job_count caps the shared-base worker count: no clones, so
// only core count and item count matter.
fn shared_transform_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_shared_transform_jobs {
		n = max_shared_transform_jobs
	}
	if n > n_items {
		n = n_items
	}
	return n
}

// shared_region_view builds a FlatAst whose arrays alias the master's data but
// whose len/cap bound this worker's private append region [nstart, nend) /
// [cstart, cend). Base reads (ids below nstart) see the shared immutable base;
// appends land in the region, so the new node ids are already master-global.
// .nogrow turns a region overflow into a loud panic instead of a realloc that
// would free the master's live block.
fn shared_region_view(a &flat.FlatAst, nstart int, nend int, cstart int, cend int) &flat.FlatAst {
	mut nodes := []flat.Node{}
	mut children := []flat.NodeId{}
	unsafe {
		nodes.data = a.nodes.data
		nodes.len = nstart
		nodes.cap = nend
		nodes.flags.set(.nogrow)
		children.data = a.children.data
		children.len = cstart
		children.cap = cend
		children.flags.set(.nogrow)
	}
	return &flat.FlatAst{
		nodes:                nodes
		children:             children
		user_code_start:      a.user_code_start
		disabled_fns:         a.disabled_fns
		noreturn_fns:         a.noreturn_fns
		source_files:         a.source_files
		source_buffers:       a.source_buffers
		text_values:          a.text_values
		text_ids:             a.text_ids
		worker_pool:          a.worker_pool
		specialized_fn_nodes: a.specialized_fn_nodes
	}
}

// run_parallel_transform_shared is the clone-free variant of the parallel
// transform: all threads (master included) work directly on the master arrays.
// Fn subtrees are disjoint node ranges, transform only rewrites nodes inside
// the fn currently being lowered (plus appends), and stray out-of-range writes
// are intercepted (see base_write_allowed), so threads never touch each
// other's slots. Each thread appends into its own pre-partitioned capacity
// region; the master then compacts the regions into the final sequential
// layout with the same shift arithmetic merge_worker always used (a region
// start plays the role the clone base played).
fn (mut t Transformer) run_parallel_transform_shared(items []FnWorkItem, base_nodes int, base_children int, n_jobs int) bool {
	$if windows {
		t.transform_pure_items_serial(items)
		return false
	} $else {
		t.tc.freeze_type_cache_for_forks()
		mut chunks := split_work_items_ex(items, n_jobs, false)
		chunk_count := chunks.len
		thread_count := chunk_count - 1
		// Partition the reserved capacity into per-chunk append regions,
		// proportional to chunk cost (the caller reserved ~2x the expected
		// total growth for this pool).
		node_pool := t.a.nodes.cap - base_nodes
		child_pool := t.a.children.cap - base_children
		mut costs := []i64{len: chunk_count}
		mut total := i64(0)
		for ci in 0 .. chunk_count {
			for it in chunks[ci] {
				costs[ci] += i64(it.cost) + 1
			}
			total += costs[ci]
		}
		if total <= 0 {
			total = 1
		}
		mut node_starts := []int{len: chunk_count + 1}
		mut child_starts := []int{len: chunk_count + 1}
		node_starts[0] = base_nodes
		child_starts[0] = base_children
		mut acc := i64(0)
		for ci in 0 .. chunk_count {
			acc += costs[ci]
			node_starts[ci + 1] = base_nodes + int(i64(node_pool) * acc / total)
			child_starts[ci + 1] = base_children + int(i64(child_pool) * acc / total)
		}
		t.base_write_intercept = true
		t.defer_oor_writes = true
		t.shared_base_nodes = base_nodes
		setup_scope := transform_worker_scope_begin(t.scope_parallel_workers)
		mut args := []SharedChunkArgs{len: chunk_count}
		args[0] = SharedChunkArgs{
			worker:    voidptr(t)
			items_ptr: unsafe { voidptr(&chunks[0]) }
			is_master: true
		}
		for ci in 0 .. thread_count {
			view := shared_region_view(t.a, node_starts[ci + 1], node_starts[ci + 2], child_starts[
				ci + 1], child_starts[ci + 2])
			wtc := t.tc.fork_for_parallel_transform(view)
			mut ww := t.fork_worker(view, wtc)
			ww.defer_oor_writes = false
			args[ci + 1] = SharedChunkArgs{
				worker:    voidptr(ww)
				items_ptr: unsafe { voidptr(&chunks[ci + 1]) }
			}
		}
		// The master takes region 0, which is [base, node_starts[1]) — exactly
		// where compaction wants its output, so its appends need no shifting
		// and its checker-cache writes use final node ids. Bound its arrays so
		// an overflow panics instead of reallocating the shared block away
		// from under the workers.
		orig_nodes_cap := t.a.nodes.cap
		orig_children_cap := t.a.children.cap
		unsafe {
			t.a.nodes.cap = node_starts[1]
			t.a.nodes.flags.set(.nogrow)
			t.a.children.cap = child_starts[1]
			t.a.children.flags.set(.nogrow)
		}
		t.temp_counter = 0
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: chunk_count}
		for ci in 0 .. chunk_count {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        shared_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'transform:all' || fail == 'transform:${helper_idx}'
			}
		}
		transform_worker_scope_leave(setup_scope)
		any_started := t.a.worker_pool.run(tasks)
		unsafe {
			t.a.nodes.cap = orig_nodes_cap
			t.a.nodes.flags.clear(.nogrow)
			t.a.children.cap = orig_children_cap
			t.a.children.flags.clear(.nogrow)
		}
		if t.retain_worker_results && t.worker_scope != unsafe { nil } {
			mut master_base_nodes := t.scoped_owned_base_nodes.keys()
			master_base_nodes << t.scoped_owned_base_log
			for write in t.deferred_base_writes {
				master_base_nodes << write.idx
			}
			t.retained_worker_regions << ScopedTransformRegion{
				scope:      t.worker_scope
				new_start:  base_nodes
				new_end:    t.a.nodes.len
				base_nodes: master_base_nodes
			}
			for item in chunks[0] {
				if item.fn_idx >= 0 && item.fn_idx < t.transformed_fns.len {
					t.transformed_fns[item.fn_idx] = true
				}
			}
		}
		// Compact each worker region in fixed order (deterministic
		// node numbering). merge_worker treats the region start exactly like a
		// clone's base offset; compaction always moves content left, so the
		// copies never collide with unmerged regions.
		for ci in 0 .. thread_count {
			ww := unsafe { &Transformer(args[ci + 1].worker) }
			t.merge_worker_used_fns(ww)
			deferred_start := t.deferred_base_writes.len
			merged_node_start := t.a.nodes.len
			t.merge_worker(ww, chunks[ci + 1], node_starts[ci + 1], child_starts[ci + 1])
			if ww.worker_scope != unsafe { nil } && !t.retain_worker_results {
				t.clone_deferred_worker_writes_from(deferred_start)
				transform_worker_scope_free(ww.worker_scope)
			} else if ww.worker_scope != unsafe { nil } {
				mut worker_base_nodes := ww.scoped_owned_base_nodes.keys()
				worker_base_nodes << ww.scoped_owned_base_log
				for item in chunks[ci + 1] {
					worker_base_nodes << item.fn_idx
				}
				for write in ww.deferred_base_writes {
					worker_base_nodes << write.idx
				}
				t.retained_worker_regions << ScopedTransformRegion{
					scope:      ww.worker_scope
					new_start:  merged_node_start
					new_end:    t.a.nodes.len
					base_nodes: worker_base_nodes
				}
			}
		}
		if t.retain_worker_results {
			t.clone_sum_eq_types_owned()
		}
		t.base_write_intercept = false
		t.defer_oor_writes = false
		t.shared_base_nodes = -1
		t.flush_deferred_base_writes()
		if t.ignored_comptime_for_log.len > 0 {
			if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
				t.ignored_comptime_for_nodes << []bool{len: t.a.nodes.len - t.ignored_comptime_for_nodes.len}
			}
			for idx in t.ignored_comptime_for_log {
				if idx >= 0 && idx < t.ignored_comptime_for_nodes.len {
					t.ignored_comptime_for_nodes[idx] = true
				}
			}
		}
		t.tc.unfreeze_type_cache_after_forks()
		transform_worker_scope_free(setup_scope)
		return any_started
	}
}

$if !windows {
	// LateScanChunkArgs is the payload handed to each late-name-scan worker.
	struct LateScanChunkArgs {
		worker      voidptr // &Transformer
		cands_ptr   voidptr // &[]LateFnCandidate
		used        map[string]bool
		results_ptr voidptr // &[][]string
		index       int
		start       int
		end         int
	}

	// late_scan_chunk_thread runs one worker's candidate range of the late-name scan.
	fn late_scan_chunk_thread(arg voidptr) voidptr {
		args := unsafe { &LateScanChunkArgs(arg) }
		mut w := unsafe { &Transformer(args.worker) }
		cands := unsafe { &[]LateFnCandidate(args.cands_ptr) }
		mut results := unsafe { &[][]string(args.results_ptr) }
		res := w.scan_late_call_names_range(*cands, args.used, args.start, args.end)
		unsafe {
			(*results)[args.index] = res
		}
		return unsafe { nil }
	}
}

// scan_late_call_names_dispatch runs the late-name scan across threads when
// there is enough work. The scan is read-only over the merged AST (each worker
// gets a forked TypeChecker for its private memoization), and the per-range
// results are concatenated in range order and deduplicated, which reproduces
// the serial scan byte for byte.
fn (mut t Transformer) scan_late_call_names_dispatch(cands []LateFnCandidate, used map[string]bool) []string {
	$if windows {
		return t.scan_late_call_names_range(cands, used, 0, cands.len)
	} $else {
		// The scan clones no ASTs (workers share the merged AST read-only), so it
		// is not bound by the clone-memory ceiling of the transform workers.
		if isnil(t.a.worker_pool) {
			t.a.worker_pool = workers.new(runtime.nr_jobs() - 1)
		}
		mut n_jobs := t.a.worker_pool.size() + 1
		if n_jobs > 10 {
			n_jobs = 10
		}
		if n_jobs > cands.len {
			n_jobs = cands.len
		}
		if cands.len < 256 || n_jobs <= 1 {
			return t.scan_late_call_names_range(cands, used, 0, cands.len)
		}
		t.tc.freeze_type_cache_for_forks()
		bounds := late_scan_chunk_bounds(t.a, cands, n_jobs)
		thread_count := n_jobs - 1
		mut results := [][]string{len: n_jobs, init: []string{}}
		mut scan_workers := []voidptr{len: thread_count, init: unsafe { nil }}
		mut args := []LateScanChunkArgs{len: n_jobs}
		args[0] = LateScanChunkArgs{
			worker:      voidptr(t)
			cands_ptr:   unsafe { voidptr(&cands) }
			used:        used
			results_ptr: unsafe { voidptr(&results) }
			index:       0
			start:       bounds[0]
			end:         bounds[1]
		}
		for ci in 0 .. thread_count {
			// No AST clone: the scan never appends nodes. Only the checker is
			// forked (private type_cache) and the Transformer's per-function
			// context is private to the fork.
			wtc := t.tc.fork_for_parallel_transform(t.a)
			ww := t.fork_scan_worker(wtc)
			scan_workers[ci] = voidptr(ww)
			args[ci + 1] = LateScanChunkArgs{
				worker:      scan_workers[ci]
				cands_ptr:   unsafe { voidptr(&cands) }
				used:        used
				results_ptr: unsafe { voidptr(&results) }
				index:       ci + 1
				start:       bounds[ci + 1]
				end:         bounds[ci + 2]
			}
		}
		fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
		mut tasks := []workers.Task{cap: n_jobs}
		for ci in 0 .. n_jobs {
			helper_idx := ci - 1
			tasks << workers.Task{
				run:        late_scan_chunk_thread
				arg:        unsafe { voidptr(&args[ci]) }
				force_sync: ci == 0 || fail == 'transform:all' || fail == 'transform:${helper_idx}'
			}
		}
		t.a.worker_pool.run(tasks)
		t.tc.unfreeze_type_cache_after_forks()
		mut names := []string{}
		mut seen := map[string]bool{}
		for res in results {
			for name in res {
				if !seen[name] {
					seen[name] = true
					names << name
				}
			}
		}
		return names
	}
}

// late_scan_chunk_bounds splits the candidate list into n contiguous ranges of
// roughly equal node count (the span to the next candidate approximates each
// body's subtree size).
fn late_scan_chunk_bounds(a &flat.FlatAst, cands []LateFnCandidate, n int) []int {
	mut total := i64(0)
	mut costs := []i64{cap: cands.len}
	for i, cand in cands {
		next := if i + 1 < cands.len { cands[i + 1].idx } else { a.nodes.len }
		cost := i64(next - cand.idx)
		costs << if cost > 0 { cost } else { i64(1) }
		total += costs[i]
	}
	mut bounds := []int{cap: n + 1}
	bounds << 0
	mut acc := i64(0)
	mut chunk := 1
	for i in 0 .. cands.len {
		acc += costs[i]
		if chunk < n && acc >= total * i64(chunk) / i64(n) {
			bounds << i + 1
			chunk++
		}
	}
	for bounds.len < n + 1 {
		bounds << cands.len
	}
	return bounds
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
