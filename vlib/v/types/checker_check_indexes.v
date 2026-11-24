module types

import os
import v.vmod
import v.workers

// min_parallel_check_index_nodes is the size of the AST from which a parallel
// check builds its indexes on the pool (see prepare_check_indexes_parallel):
// two of them read every node, which takes a few ms in a large program.
const min_parallel_check_index_nodes = 262_144

// check_index_scan_jobs is the number of pool tasks that share the node scan.
const check_index_scan_jobs = 8

// decl_assign_bloom_mask sizes the filter of the names that the `decl_assign`s
// of an AST of `n_nodes` nodes declare.
fn decl_assign_bloom_mask(n_nodes int) u32 {
	mut nbits := u32(4096)
	for u64(nbits) < u64(n_nodes) / 4 && nbits < (u32(1) << 22) {
		nbits <<= 1
	}
	return nbits - 1
}

// scan_check_index_range adds every identifier that a `decl_assign` among the
// nodes [start, end) declares to the filter `bits`, and reports whether one of
// the nodes is a struct field with the `required` attribute.
@[direct_array_access]
fn (tc &TypeChecker) scan_check_index_range(start int, end int, mut bits []u64, mask u32) bool {
	mut has_required := false
	for i in start .. end {
		node := &tc.a.nodes[i]
		if node.kind == .decl_assign {
			for child_idx := 0; child_idx < node.children_count; child_idx += 2 {
				lhs := tc.a.node(tc.a.child(node, child_idx))
				if lhs.kind == .ident {
					const_selector_bloom_add(mut bits, mask, lhs.value)
				}
			}
		} else if !has_required && node.kind == .field_decl {
			// As struct_field_has_attr, without slicing the shared metadata.
			meta := node.generic_params()
			for attr_idx in 1 .. meta.len {
				if attribute_name_is(meta[attr_idx], 'required') {
					has_required = true
					break
				}
			}
		}
	}
	return has_required
}

// CheckIndexScanArgs is one pool task of prepare_check_indexes_parallel: the scan
// of a range of nodes.
struct CheckIndexScanArgs {
	tc    voidptr // &TypeChecker, which the task only reads
	start int
	end   int
	mask  u32
mut:
	scope        voidptr
	bits         []u64
	has_required bool
}

fn check_index_scan_thread(arg voidptr) voidptr {
	mut a := unsafe { &CheckIndexScanArgs(arg) }
	tc := unsafe { &TypeChecker(a.tc) }
	a.scope = check_worker_scope_begin(true)
	mut bits := []u64{len: int((a.mask + 1) >> 6)}
	a.has_required = tc.scan_check_index_range(a.start, a.end, mut bits, a.mask)
	a.bits = bits
	check_worker_scope_leave(a.scope)
	return unsafe { nil }
}

fn check_index_master_thread(arg voidptr) voidptr {
	mut tc := unsafe { &TypeChecker(arg) }
	tc.check_decl_indexes = tc.new_check_decl_indexes_opt(false)
	tc.prepare_check_name_filters_opt(false)
	return unsafe { nil }
}

// PreparedFnGroups is what check_duplicate_fn_declarations finds in its pass
// over the declarations, found ahead by a pool task of
// prepare_check_indexes_parallel for the AST as it was then.
@[heap]
struct PreparedFnGroups {
	tc            voidptr // &TypeChecker, which the task only reads
	top_level_len int
	nodes_len     int
mut:
	scope  voidptr
	groups map[string][]int
}

fn check_fn_groups_thread(arg voidptr) voidptr {
	mut a := unsafe { &PreparedFnGroups(arg) }
	tc := unsafe { &TypeChecker(a.tc) }
	a.scope = check_worker_scope_begin(true)
	a.groups = tc.repeated_fn_declaration_groups()
	check_worker_scope_leave(a.scope)
	return unsafe { nil }
}

// take_prepared_fn_groups returns the groups of repeated function declarations
// when prepare_check_indexes_parallel found them for the AST as it is now, or
// nil. The caller owns their arena.
fn (mut tc TypeChecker) take_prepared_fn_groups() &PreparedFnGroups {
	prepared := tc.prepared_fn_groups
	if isnil(prepared) {
		return unsafe { nil }
	}
	tc.prepared_fn_groups = unsafe { nil }
	if prepared.top_level_len != tc.top_level_idx.len || prepared.nodes_len != tc.a.nodes.len {
		check_worker_scope_free(prepared.scope)
		return unsafe { nil }
	}
	return prepared
}

// CheckFsMemoArgs is the pool task of prepare_check_indexes_parallel that asks
// the file system ahead what check_import_diagnostics asks it.
struct CheckFsMemoArgs {
	tc voidptr // &TypeChecker, which the task only reads
mut:
	memo &CheckerFsMemo = unsafe { nil }
}

fn check_fs_memo_thread(arg voidptr) voidptr {
	mut a := unsafe { &CheckFsMemoArgs(arg) }
	tc := unsafe { &TypeChecker(a.tc) }
	// The answers are kept in this thread's persistent arena, as the pass keeps
	// those it asks for itself: a path it reports may be one of them.
	a.memo = tc.prewarm_import_fs_memo()
	return unsafe { nil }
}

// prewarm_import_fs_memo answers the questions that check_import_diagnostics
// asks the file system about the directory of each file that imports a module
// (see memo_real_path, memo_vmod_root_for_file and manifest_source_root), under
// the keys it asks them with. The answers do not change during a compilation,
// and one that is missing here is asked for as before.
fn (tc &TypeChecker) prewarm_import_fs_memo() &CheckerFsMemo {
	mut memo := &CheckerFsMemo{}
	for root in tc.module_search_paths {
		if root !in memo.real_paths {
			memo.real_paths[root] = os.real_path(root)
		}
	}
	if tc.compiler_vroot.len > 0 {
		vlib_root := os.join_path(tc.compiler_vroot, 'vlib')
		if vlib_root !in memo.real_paths {
			memo.real_paths[vlib_root] = os.real_path(vlib_root)
		}
	}
	mut file := ''
	mut file_done := true
	for idx in tc.top_level_idx {
		node := &tc.a.nodes[idx]
		if node.kind == .file {
			file = node.value
			file_done = file.len == 0 || file in tc.a.cached_header_sources
			continue
		}
		if file_done || node.kind != .import_decl || node.pos.end <= node.pos.offset {
			continue
		}
		file_done = true
		dir := os.dir(file)
		if dir !in memo.real_paths {
			memo.real_paths[dir] = os.real_path(dir)
		}
		if dir in memo.vmod_roots {
			continue
		}
		vmod_root := checker_vmod_root_for_file(file)
		memo.vmod_roots[dir] = vmod_root
		if vmod_root !in memo.source_roots {
			mut source_root := ''
			if manifest := vmod.from_file(os.join_path(vmod_root, 'v.mod')) {
				source_root = os.real_path(manifest.source_root(vmod_root)).replace('\\',
					'/').trim_right('/')
			}
			memo.source_roots[vmod_root] = source_root
		}
	}
	return memo
}

// PreparedCheckItems is the work list of a parallel check (see
// collect_parallel_check_items) and its split for the dynamic dispatch, made
// ahead by a pool task of prepare_check_indexes_parallel for the AST as it was
// then. Both only read the AST and what collection and the driver set.
@[heap]
struct PreparedCheckItems {
	tc            voidptr // &TypeChecker, which the task only reads
	top_level_len int
	nodes_len     int
	costs_len     int
	pool_size     int
mut:
	scope voidptr
	// valid is false when listing the items would have completed the module
	// table, which only the check itself may do.
	valid        bool
	items        []CheckWorkItem
	final_file   string
	final_module string
	// The chunks of the dynamic dispatch for `target` chunks, or none.
	target int
	chunks [][]CheckWorkItem
}

fn check_items_thread(arg voidptr) voidptr {
	mut a := unsafe { &PreparedCheckItems(arg) }
	tc := unsafe { &TypeChecker(a.tc) }
	a.scope = check_worker_scope_begin(true)
	tc.list_parallel_check_items(mut a)
	if a.valid {
		a.target = tc.planned_dynamic_check_target(a.items, a.pool_size)
		if a.target > 0 {
			a.chunks = split_dynamic_check_items(a.items, a.target, dynamic_check_small_pct,
				dynamic_check_small_div)
		}
	}
	check_worker_scope_leave(a.scope)
	return unsafe { nil }
}

// list_parallel_check_items lists what collect_parallel_check_items lists,
// without entering the files on the checker: it tracks the file and the module
// itself, and gives up where entering a module would have written its table.
fn (tc &TypeChecker) list_parallel_check_items(mut p PreparedCheckItems) {
	blocking_import_files := tc.blocking_import_error_files()
	library_bodies_trusted := tc.selected_files_only()
	mut cur_file := ''
	mut cur_module := ''
	mut items := []CheckWorkItem{}
	mut prev_tl := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = tc.file_modules[node.value] or { '' }
			}
			.module_decl {
				cur_module = node.value
				if cur_file.len > 0 && cur_file !in tc.file_modules {
					return
				}
			}
			.fn_decl {
				if blocking_import_files[cur_file] {
					prev_tl = i
					continue
				}
				if library_bodies_trusted && !tc.diagnostic_files[cur_file] {
					prev_tl = i
					continue
				}
				span := i - prev_tl
				cost := if i < tc.fn_check_costs.len && tc.fn_check_costs[i] > 0 {
					tc.fn_check_costs[i]
				} else {
					span
				}
				items << CheckWorkItem{
					fn_idx:   i
					range_lo: prev_tl + 1
					file:     cur_file
					module:   cur_module
					cost:     cost
					rank:     i64(cost) * 1_000_000_000 - i64(i)
				}
			}
			else {}
		}
		prev_tl = i
	}
	p.items = items
	p.final_file = cur_file
	p.final_module = cur_module
	p.valid = true
}

// planned_dynamic_check_target returns the number of chunks that
// run_parallel_check splits `items` into for its dynamic dispatch with a pool of
// `pool_size` workers, or 0 when it does not dispatch dynamically.
fn (tc &TypeChecker) planned_dynamic_check_target(items []CheckWorkItem, pool_size int) int {
	if !tc.scope_parallel_check_workers || !tc.building_v_fast
		|| os.getenv('V3_TEST_PTHREAD_CREATE_FAIL').len > 0 {
		return 0
	}
	mut n_jobs := check_job_count(pool_size + 1, items.len)
	if n_jobs > max_scoped_check_jobs {
		n_jobs = max_scoped_check_jobs
	}
	n_jobs = parallel_check_jobs_for_cost(n_jobs, items)
	if items.len < tc.parallel_check_min_items || n_jobs <= 1 {
		return 0
	}
	chunk_count := int_min(n_jobs * check_chunk_oversubscribe, items.len)
	return int_min(items.len, chunk_count * dynamic_check_chunks_per_job)
}

// take_prepared_check_items returns the work list that
// prepare_check_indexes_parallel listed when it did for the AST as it is now,
// or nil. The caller owns its arena.
fn (mut tc TypeChecker) take_prepared_check_items() &PreparedCheckItems {
	prepared := tc.prepared_check_items
	if isnil(prepared) {
		return unsafe { nil }
	}
	tc.prepared_check_items = unsafe { nil }
	if !prepared.valid || prepared.top_level_len != tc.top_level_idx.len
		|| prepared.nodes_len != tc.a.nodes.len || prepared.costs_len != tc.fn_check_costs.len
		|| tc.skips_library_bodies {
		check_worker_scope_free(prepared.scope)
		return unsafe { nil }
	}
	return prepared
}

// PreparedWalkFnNames is what arm_walk_fn_short_names_listing_decls takes from
// the pool, taken ahead by prepare_check_indexes_parallel for the tables as
// they were then.
@[heap]
struct PreparedWalkFnNames {
	ret_len       int
	param_len     int
	top_level_len int
	nodes_len     int
	mask          u32
mut:
	args []WalkFnNamesArgs
}

// prepare_check_indexes_parallel builds what new_check_decl_indexes and
// prepare_check_name_filters build, with the pool reading the nodes while the
// calling thread indexes the declarations and the names. The names and the
// declarations of a reachability walk, which the pool takes later otherwise
// (see arm_walk_fn_short_names_listing_decls), are taken in the same phase. The
// tasks only read the AST and the tables that collection completed; the calling
// thread writes none of those meanwhile.
fn (mut tc TypeChecker) prepare_check_indexes_parallel(pool &workers.Pool) {
	n_nodes := tc.a.nodes.len
	mask := decl_assign_bloom_mask(n_nodes)
	n_scans := int_min(pool.size() + 1, check_index_scan_jobs)
	mut scans := []CheckIndexScanArgs{cap: n_scans}
	for job in 0 .. n_scans {
		scans << CheckIndexScanArgs{
			tc:    voidptr(tc)
			start: int(i64(n_nodes) * job / n_scans)
			end:   int(i64(n_nodes) * (job + 1) / n_scans)
			mask:  mask
		}
	}
	walk_count := tc.fn_ret_types.len + tc.fn_param_types.len
	mut walk := &PreparedWalkFnNames(unsafe { nil })
	if tc.diagnostic_files.len > 0 && tc.scope_parallel_check_workers
		&& walk_count >= min_parallel_walk_fn_names {
		walk = &PreparedWalkFnNames{
			ret_len:       tc.fn_ret_types.len
			param_len:     tc.fn_param_types.len
			top_level_len: tc.top_level_idx.len
			nodes_len:     n_nodes
			mask:          walk_fn_short_names_mask(walk_count)
			args:          []WalkFnNamesArgs{cap: 3}
		}
		for job in 0 .. 3 {
			walk.args << WalkFnNamesArgs{
				tc:          voidptr(tc)
				list_decls:  job == 2
				param_table: job == 1
				mask:        walk.mask
			}
		}
	}
	mut fn_groups := &PreparedFnGroups{
		tc:            voidptr(tc)
		top_level_len: tc.top_level_idx.len
		nodes_len:     n_nodes
	}
	mut fs_memo := &CheckFsMemoArgs{
		tc: voidptr(tc)
	}
	mut check_items := &PreparedCheckItems{
		tc:            voidptr(tc)
		top_level_len: tc.top_level_idx.len
		nodes_len:     n_nodes
		costs_len:     tc.fn_check_costs.len
		pool_size:     pool.size()
	}
	mut tasks := []workers.Task{cap: n_scans + 7}
	// Deciding whether a library body is skipped can fill a memo of the checker.
	if !tc.skips_library_bodies {
		tasks << workers.Task{
			run: check_items_thread
			arg: voidptr(check_items)
		}
	}
	tasks << workers.Task{
		run:        check_index_master_thread
		arg:        voidptr(tc)
		force_sync: true
	}
	tasks << workers.Task{
		run: check_fn_groups_thread
		arg: voidptr(fn_groups)
	}
	tasks << workers.Task{
		run: check_fs_memo_thread
		arg: voidptr(fs_memo)
	}
	for job in 0 .. n_scans {
		tasks << workers.Task{
			run: check_index_scan_thread
			arg: unsafe { voidptr(&scans[job]) }
		}
	}
	if !isnil(walk) {
		for job in 0 .. 3 {
			tasks << workers.Task{
				run: walk_fn_names_thread
				arg: unsafe { voidptr(&walk.args[job]) }
			}
		}
	}
	pool.run(tasks)
	// The filter serves the whole check: it is released with the indexes.
	scope := check_worker_scope_begin(true)
	mut bits := []u64{len: int((mask + 1) >> 6)}
	check_worker_scope_leave(scope)
	mut has_required := false
	mut scan_scopes := []voidptr{cap: n_scans}
	for scan in scans {
		for i in 0 .. bits.len {
			bits[i] |= scan.bits[i]
		}
		has_required = has_required || scan.has_required
		if scan.scope != unsafe { nil } {
			scan_scopes << scan.scope
		}
	}
	for scan_scope in scan_scopes {
		check_worker_scope_free(scan_scope)
	}
	mut indexes := tc.check_decl_indexes
	indexes.decl_assign_bloom = &ConstSelectorBloom{
		count: n_nodes
		mask:  mask
		bits:  bits
	}
	indexes.bloom_scope = scope
	tc.has_required_fields = has_required
	tc.required_fields_ast_nodes = n_nodes
	tc.prepared_walk_fn_names = walk
	tc.prepared_fn_groups = fn_groups
	tc.prepared_fs_memo = fs_memo.memo
	tc.prepared_check_items = check_items
}

// take_prepared_walk_fn_names returns the names and declarations of a
// reachability walk when prepare_check_indexes_parallel took them for the tables
// as they are now, or nil. The caller owns their arenas.
fn (mut tc TypeChecker) take_prepared_walk_fn_names(mask u32) &PreparedWalkFnNames {
	prepared := tc.prepared_walk_fn_names
	if isnil(prepared) {
		return unsafe { nil }
	}
	if prepared.ret_len != tc.fn_ret_types.len || prepared.param_len != tc.fn_param_types.len
		|| prepared.top_level_len != tc.top_level_idx.len || prepared.nodes_len != tc.a.nodes.len
		|| prepared.mask != mask || prepared.args.len != 3 {
		tc.release_prepared_walk_fn_names()
		return unsafe { nil }
	}
	tc.prepared_walk_fn_names = unsafe { nil }
	return prepared
}

// release_prepared_walk_fn_names drops the names and declarations that no
// reachability walk took.
fn (mut tc TypeChecker) release_prepared_walk_fn_names() {
	prepared := tc.prepared_walk_fn_names
	if isnil(prepared) {
		return
	}
	tc.prepared_walk_fn_names = unsafe { nil }
	for arg in prepared.args {
		check_worker_scope_free(arg.scope)
	}
}

// release_prepared_check_inputs drops what prepare_check_indexes_parallel took
// ahead and no pass of the check used.
fn (mut tc TypeChecker) release_prepared_check_inputs() {
	tc.release_prepared_walk_fn_names()
	groups := tc.prepared_fn_groups
	if !isnil(groups) {
		tc.prepared_fn_groups = unsafe { nil }
		check_worker_scope_free(groups.scope)
	}
	tc.prepared_fs_memo = unsafe { nil }
	items := tc.prepared_check_items
	if !isnil(items) {
		tc.prepared_check_items = unsafe { nil }
		check_worker_scope_free(items.scope)
	}
}
