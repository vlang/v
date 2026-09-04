module fastc

import os
import time
import v3.pref
import v3.scanner
import v3.token

struct FastcPendingReferences {
mut:
	workers    []thread map[string]bool
	references map[string]bool
}

struct FastcPendingInterfaceDispatches {
mut:
	workers    []thread string
	dispatches string
}

fn fastc_start_referenced_function_names(sources []FastcSourceFile, prefs &pref.Preferences, functions map[string]FastcFunctionSignature) FastcPendingReferences {
	if fastc_parallel_job_count(sources.len, prefs) <= 1 {
		return FastcPendingReferences{
			references: fastc_collect_referenced_function_names(sources, prefs, functions)
		}
	}
	return FastcPendingReferences{
		workers: [spawn fastc_collect_referenced_function_names(sources, prefs, functions)]
	}
}

fn fastc_wait_referenced_function_names(mut pending FastcPendingReferences) map[string]bool {
	if pending.workers.len == 0 {
		return pending.references
	}
	return pending.workers[0].wait()
}

fn fastc_start_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, interface_methods map[string]bool, used_function_names map[string]bool, selfhost bool, prefs &pref.Preferences) FastcPendingInterfaceDispatches {
	if fastc_parallel_job_count(functions.len, prefs) <= 1 {
		return FastcPendingInterfaceDispatches{
			dispatches: fastc_generate_interface_dispatches(declared_kinds, functions, interface_methods, used_function_names, selfhost)
		}
	}
	return FastcPendingInterfaceDispatches{
		workers: [
			spawn fastc_generate_interface_dispatches(declared_kinds, functions, interface_methods, used_function_names, selfhost),
		]
	}
}

fn fastc_wait_interface_dispatches(mut pending FastcPendingInterfaceDispatches) string {
	if pending.workers.len == 0 {
		return pending.dispatches
	}
	return pending.workers[0].wait()
}

fn fastc_parallel_job_count(item_count int, prefs &pref.Preferences) int {
	mut jobs := fastc_parallel_worker_limit(prefs)
	if jobs > item_count {
		jobs = item_count
	}
	if item_count < 4 {
		jobs = 1
	}
	return jobs
}

// fastc_parallel_jobs picks the worker count for a parallel phase; 1 selects
// the serial path. `-no-parallel` and VJOBS mirror the AST pipeline's
// behavior, and V3_FASTC_NO_PARALLEL forces serial for debugging and for
// byte-comparing parallel output against a serial run.
fn fastc_parallel_jobs(sources []FastcSourceFile, prefs &pref.Preferences) int {
	return fastc_parallel_job_count(sources.len, prefs)
}

fn fastc_load_source_chunk(paths []string, prefs &pref.Preferences, start int, end int) []FastcLoadedSource {
	mut loaded := []FastcLoadedSource{cap: end - start}
	for i in start .. end {
		loaded << fastc_load_source(paths[i], prefs)
	}
	return loaded
}

// fastc_memo_reader_limit caps the memo preload workers: file reads are the
// bulk of the batch and they do not scale past a few concurrent readers.
const fastc_memo_reader_limit = 6

// fastc_memo_worker runs memo preload tasks from the shared counter.
fn fastc_memo_worker(tasks []FastcMemoTask, prefs &pref.Preferences, canonical_vlib string, queue &FastcGenQueue) []FastcMemoResult {
	mut results := []FastcMemoResult{}
	for {
		slot := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if slot >= u32(tasks.len) {
			break
		}
		index := int(slot)
		results << fastc_run_memo_task(tasks[index], index, prefs, canonical_vlib)
	}
	return results
}

// fastc_run_memo_tasks runs a batch of memo tasks on at most the memo worker
// limit of threads and returns the results in task order.
fn fastc_run_memo_tasks(tasks []FastcMemoTask, prefs &pref.Preferences, canonical_vlib string) []FastcMemoResult {
	mut jobs := fastc_parallel_job_count(tasks.len, prefs)
	// Small-file reads and stats stop scaling after a few concurrent callers
	// (the kernel serializes them), and extra workers only land on slower
	// cores.
	if jobs > fastc_memo_reader_limit {
		jobs = fastc_memo_reader_limit
	}
	mut results := []FastcMemoResult{len: tasks.len}
	if jobs <= 1 {
		for index, task in tasks {
			results[index] = fastc_run_memo_task(task, index, prefs, canonical_vlib)
		}
		return results
	}
	mut queue := &FastcGenQueue{
		next: 0
	}
	mut workers := [
		spawn fastc_memo_worker(tasks, prefs, canonical_vlib, queue),
	]
	for _ in 2 .. jobs {
		workers << spawn fastc_memo_worker(tasks, prefs, canonical_vlib, queue)
	}
	first := fastc_memo_worker(tasks, prefs, canonical_vlib, queue)
	for result in first {
		results[result.index] = result
	}
	for worker in workers {
		worker_results := worker.wait()
		for result in worker_results {
			results[result.index] = result
		}
	}
	return results
}

// fastc_preload_memo lists, looks up and stats everything a resolve memo
// names in one bounded parallel batch while the main thread reads the memo's
// content blob, then materializes the files in a second batch: from the blob
// when a file's stamp still matches, by reading it otherwise.
fn fastc_preload_memo(memo_path string, memo FastcResolveMemo, prefs &pref.Preferences, canonical_vlib string, entry_paths []string, entry_real_paths []string, mut module_path_cache map[string]string, mut module_dir_files map[string][]string, mut entry_files map[string][]string, mut real_path_cache map[string]string, mut loaded map[string]FastcLoadedSource) {
	base_tasks := fastc_memo_probe_tasks(memo, entry_paths, entry_real_paths, prefs)
	mut jobs := fastc_parallel_job_count(base_tasks.len, prefs)
	if jobs > fastc_memo_reader_limit {
		jobs = fastc_memo_reader_limit
	}
	mut blob := ''
	trace := os.getenv('FASTC_BENCH_PRELOAD') != ''
	trace_sw := time.new_stopwatch()
	if jobs <= 1 {
		probe_results := fastc_run_memo_tasks(base_tasks, prefs, canonical_vlib)
		blob = fastc_read_memo_blob(memo_path, memo)
		fastc_finish_memo_preload(memo, base_tasks, probe_results, blob, prefs, canonical_vlib, trace, trace_sw, mut module_path_cache, mut module_dir_files, mut entry_files, mut real_path_cache, mut loaded)
		return
	}
	// The blob is read in ranges beside the probes, by the same workers; the
	// range tasks come first so the reads start at once.
	blob_tasks, blob_buffer := fastc_memo_blob_tasks(memo_path, memo)
	mut probe_tasks := []FastcMemoTask{cap: blob_tasks.len + base_tasks.len}
	probe_tasks << blob_tasks
	probe_tasks << base_tasks
	mut probe_results := []FastcMemoResult{len: probe_tasks.len}
	mut queue := &FastcGenQueue{
		next: 0
	}
	mut workers := [
		spawn fastc_memo_worker(probe_tasks, prefs, canonical_vlib, queue),
	]
	for _ in 2 .. jobs {
		workers << spawn fastc_memo_worker(probe_tasks, prefs, canonical_vlib, queue)
	}
	spawned_us := trace_sw.elapsed().microseconds()
	first := fastc_memo_worker(probe_tasks, prefs, canonical_vlib, queue)
	for result in first {
		probe_results[result.index] = result
	}
	own_us := trace_sw.elapsed().microseconds()
	for worker in workers {
		worker_results := worker.wait()
		for result in worker_results {
			probe_results[result.index] = result
		}
	}
	blob = fastc_memo_blob_from_results(probe_tasks, probe_results, memo, blob_buffer)
	if trace {
		eprintln('memo probe: spawned ${spawned_us} own ${own_us} joined ${trace_sw.elapsed().microseconds()} tasks=${probe_tasks.len} jobs=${jobs} blob_len=${blob.len}')
	}
	fastc_finish_memo_preload(memo, probe_tasks, probe_results, blob, prefs, canonical_vlib, trace, trace_sw, mut module_path_cache, mut module_dir_files, mut entry_files, mut real_path_cache, mut loaded)
}

// fastc_finish_memo_preload applies the probe results and materializes the
// files in a second batch.
fn fastc_finish_memo_preload(memo FastcResolveMemo, probe_tasks []FastcMemoTask, probe_results []FastcMemoResult, blob string, prefs &pref.Preferences, canonical_vlib string, trace bool, trace_sw time.StopWatch, mut module_path_cache map[string]string, mut module_dir_files map[string][]string, mut entry_files map[string][]string, mut real_path_cache map[string]string, mut loaded map[string]FastcLoadedSource) {
	fastc_apply_memo_results(probe_tasks, probe_results, prefs, mut module_path_cache, mut module_dir_files, mut entry_files, mut real_path_cache, mut loaded)
	current_stamps := fastc_memo_current_stamps(probe_tasks, probe_results, memo.files.len)
	read_tasks := fastc_memo_read_tasks(memo, current_stamps, blob)
	applied_us := trace_sw.elapsed().microseconds()
	read_results := fastc_run_memo_tasks(read_tasks, prefs, canonical_vlib)
	read_us := trace_sw.elapsed().microseconds()
	fastc_apply_memo_results(read_tasks, read_results, prefs, mut module_path_cache, mut module_dir_files, mut entry_files, mut real_path_cache, mut loaded)
	if trace {
		eprintln('memo read: applied ${applied_us} read ${read_us} done ${trace_sw.elapsed().microseconds()} tasks=${read_tasks.len}')
	}
}

// FastcPendingMemoStore keeps the memo-store wait API shared with the serial
// build. It holds no thread: the self-hosted generator only declares a thread
// type for functions that are spawned, so a `thread` field without a spawn
// would leave the generated C referring to an undeclared type.
struct FastcPendingMemoStore {
mut:
	stored bool
}

fn fastc_start_memo_store(memo_path string, previous_text string, sources []FastcSourceFile, builtin_dir string, lookup_modules []string, lookup_sources []string, prefs &pref.Preferences, module_path_cache map[string]string, module_dir_files map[string][]string, entry_paths []string, real_path_cache map[string]string, entry_files map[string][]string, preloaded map[string]FastcLoadedSource) FastcPendingMemoStore {
	// The source strings were loaded before generation. Deferring the store can pair
	// those bytes with a newer stat if a source changes while generation runs, and a
	// generation error returns before the worker is joined. Keep persistence
	// synchronous until the async store can snapshot the source versions it writes.
	fastc_store_resolve_memo(memo_path, previous_text, sources, builtin_dir, lookup_modules, lookup_sources, prefs, module_path_cache, module_dir_files, entry_paths, real_path_cache, entry_files, preloaded)
	return FastcPendingMemoStore{
		stored: true
	}
}

fn fastc_wait_memo_store(mut pending FastcPendingMemoStore) {
	pending.stored = true
}

// FastcModuleListing is one module directory listed on a worker thread.
struct FastcModuleListing {
	dir   string
	files []string
}

// fastc_list_module_load lists `dir`, or takes `listed` when `dir` is empty.
fn fastc_list_module_load(dir string, listed []string, prefs &pref.Preferences) FastcModuleListing {
	files := if dir == '' { listed } else { fastc_list_module_sources(dir, prefs) }
	return FastcModuleListing{
		dir: dir
		files: files
	}
}

// fastc_queue_source_chunks appends `files` to `pending` in read chunks of
// fastc_source_load_chunk_size paths.
fn fastc_queue_source_chunks(files []string, mut pending [][]string) {
	mut start := 0
	for start < files.len {
		mut end := start + fastc_source_load_chunk_size
		if end > files.len {
			end = files.len
		}
		pending << files[start..end]
		start = end
	}
}

// fastc_preload_sources reads and header-scans every file reachable from the
// initial queue on worker threads. Each imported module directory is listed
// on a thread and its files are read in chunks on further threads; the main
// thread schedules that work, keeping at most the phase's worker limit of
// threads running, and joins listings first (they unlock more reads) and
// then the read chunks in start order, resolving the imports of each chunk
// as it lands, so the reads of one module overlap with the discovery of the
// next. The entry files go first because their import chain is the deepest.
// The result is keyed by path, so the ordering walk that follows can use any
// traversal order; failures are kept so the walk reports them at the same
// point it would have. The listings are recorded in `module_dir_files` for
// that walk.
fn fastc_preload_sources(queue []FastcQueuedSource, prefs &pref.Preferences, canonical_vlib string, mut module_path_cache map[string]string, mut module_dir_files map[string][]string, mut real_path_cache map[string]string) map[string]FastcLoadedSource {
	mut loaded := map[string]FastcLoadedSource{}
	jobs := fastc_parallel_worker_limit(prefs)
	if jobs <= 1 {
		return loaded
	}
	mut scheduled := map[string]bool{}
	mut scheduled_dirs := map[string]bool{}
	mut entry_paths := []string{}
	mut other_paths := []string{}
	for queued in queue {
		mut path := queued.path
		if !queued.is_canonical {
			if cached := real_path_cache[path] {
				path = cached
			} else {
				path = os.real_path(path)
				real_path_cache[queued.path] = path
			}
		}
		if scheduled[path] {
			continue
		}
		if !queued.listed && !os.is_file(path) {
			continue
		}
		scheduled[path] = true
		if queued.module_name == '' {
			entry_paths << path
		} else {
			other_paths << path
		}
	}
	mut initial_paths := entry_paths.clone()
	initial_paths << other_paths
	if initial_paths.len == 0 {
		return loaded
	}
	wait_sw := time.new_stopwatch()
	mut wait_us := i64(0)
	trace := os.getenv('FASTC_BENCH_PRELOAD') != ''
	// The entry files form the first listing; its first chunk starts the
	// reader queue. (Both thread arrays start from a spawn: the self-hosted
	// generator has no empty thread-array literal.)
	mut listings := [
		spawn fastc_list_module_load('', initial_paths, prefs),
	]
	mut listing_count := 1
	mut next_listing := 1
	entry_listing := listings[0].wait()
	wait_us += wait_sw.elapsed().microseconds()
	mut pending_chunks := [][]string{}
	fastc_queue_source_chunks(entry_listing.files, mut pending_chunks)
	first_chunk := pending_chunks[0]
	mut chunk_threads := [
		spawn fastc_load_source_chunk(first_chunk, prefs, 0, first_chunk.len),
	]
	mut chunk_paths := [first_chunk]
	mut next_pending_chunk := 1
	mut next_chunk := 0
	mut pending_dirs := []string{}
	mut next_pending_dir := 0
	mut in_flight := 1
	for next_listing < listing_count || next_chunk < chunk_threads.len || next_pending_dir < pending_dirs.len || next_pending_chunk < pending_chunks.len {
		// Start queued work up to the worker limit, listings first.
		for in_flight < jobs && next_pending_dir < pending_dirs.len {
			listings << spawn fastc_list_module_load(pending_dirs[next_pending_dir], []string{}, prefs)
			listing_count++
			next_pending_dir++
			in_flight++
		}
		for in_flight < jobs && next_pending_chunk < pending_chunks.len {
			chunk := pending_chunks[next_pending_chunk]
			chunk_threads << spawn fastc_load_source_chunk(chunk, prefs, 0, chunk.len)
			chunk_paths << chunk
			next_pending_chunk++
			in_flight++
		}
		if next_listing < listing_count {
			listing_start := wait_sw.elapsed().microseconds()
			listing := listings[next_listing].wait()
			wait_us += wait_sw.elapsed().microseconds() - listing_start
			if trace {
				eprintln('preload listing join ${wait_sw.elapsed().microseconds()} waited ${wait_sw.elapsed().microseconds() - listing_start} ${listing.dir.all_after_last('/')} files=${listing.files.len}')
			}
			next_listing++
			in_flight--
			module_dir_files[listing.dir] = listing.files
			fastc_queue_source_chunks(listing.files, mut pending_chunks)
			continue
		}
		if next_chunk >= chunk_threads.len {
			continue
		}
		chunk_start := wait_sw.elapsed().microseconds()
		chunk := chunk_threads[next_chunk].wait()
		wait_us += wait_sw.elapsed().microseconds() - chunk_start
		if trace {
			eprintln('preload chunk join ${wait_sw.elapsed().microseconds()} waited ${wait_sw.elapsed().microseconds() - chunk_start}')
		}
		paths := chunk_paths[next_chunk]
		next_chunk++
		in_flight--
		for i, loaded_source in chunk {
			mut path := paths[i]
			if !prefs.building_v {
				if cached := real_path_cache[path] {
					path = cached
				} else {
					real := os.real_path(path)
					real_path_cache[path] = real
					path = real
				}
			}
			if path in loaded {
				continue
			}
			loaded[path] = loaded_source
			if loaded_source.failed {
				continue
			}
			for imported_module in fastc_header_imported_modules(loaded_source.header) {
				module_cache_key := fastc_module_cache_key(prefs, path, imported_module)
				module_dir := fastc_resolve_module_dir(module_cache_key, imported_module, path, prefs, canonical_vlib, mut module_path_cache)
				if module_dir == '' || scheduled_dirs[module_dir] {
					continue
				}
				scheduled_dirs[module_dir] = true
				pending_dirs << module_dir
			}
		}
	}
	if os.getenv('FASTC_BENCH_PHASES') != '' {
		eprintln('fastc-phase resolve.preload.detail jobs=${jobs} listings=${listing_count} chunks=${chunk_threads.len} wait_us=${wait_us}')
	}
	return loaded
}

// fastc_chunk_bounds splits the file list into contiguous chunks balanced by
// source size, returned as flattened [start, end) pairs.
fn fastc_chunk_bounds(sources []FastcSourceFile, jobs int) []int {
	mut total_size := i64(0)
	for source_file in sources {
		total_size += i64(source_file.source.len) + 1
	}
	mut bounds := []int{cap: jobs * 2}
	mut start := 0
	mut consumed := i64(0)
	for chunk_idx in 0 .. jobs {
		mut end := start
		target := total_size * i64(chunk_idx + 1) / i64(jobs)
		// Leave one file for every later chunk even when a large file keeps the
		// current chunk below its cumulative size target.
		last_available := sources.len - (jobs - chunk_idx - 1)
		for end < last_available && (chunk_idx == jobs - 1 || consumed < target || end == start) {
			consumed += i64(sources[end].source.len) + 1
			end++
		}
		bounds << start
		bounds << end
		start = end
	}
	return bounds
}

// fastc_collect_generic_method_sources indexes every generic method and free
// function remaining after source monomorphization. Scanning is independent per
// file, and chunk maps are merged in source order for deterministic duplicates.
// It also computes every file's declaration keyword flags, since this is the
// first pass over the sources, and stores them in the returned headers.
fn fastc_collect_generic_method_sources(mut sources []FastcSourceFile, prefs &pref.Preferences) map[string]FastcGenericMethodSource {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		partial := fastc_collect_generic_method_source_chunk(sources, prefs, 0, sources.len)
		fastc_apply_scan_flags(mut sources, partial.flags, 0)
		return partial.sources
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	second_thread := spawn fastc_collect_generic_method_source_chunk(sources, prefs, bounds[2], bounds[3])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_generic_method_source_chunk(sources, prefs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	mut first := fastc_collect_generic_method_source_chunk(sources, prefs, bounds[0], bounds[1])
	mut result := first.sources.move()
	fastc_apply_scan_flags(mut sources, first.flags, bounds[0])
	for chunk_idx, chunk_thread in chunk_threads {
		chunk := chunk_thread.wait()
		for key, generic in chunk.sources {
			result[key] = generic
		}
		fastc_apply_scan_flags(mut sources, chunk.flags, bounds[(chunk_idx + 1) * 2])
	}
	return result
}

// FastcIndexedIndexPartial is one file's scan flags, generic method index and
// declaration index, produced together by a worker of the combined pass.
struct FastcIndexedIndexPartial {
	index      int
	flags      FastcSourceScanFlags
	generics   map[string]FastcGenericMethodSource
	partial    FastcDeclarationPartial
	body_spans []int
}

// fastc_collect_index_worker scans one file at a time from the shared counter
// (largest files first) for its declaration flags, generic methods and
// declared names; the caller applies the results in file order.
fn fastc_collect_index_worker(sources []FastcSourceFile, prefs &pref.Preferences, order []int, queue &FastcGenQueue) []FastcIndexedIndexPartial {
	mut partials := []FastcIndexedIndexPartial{}
	for {
		slot := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if slot >= u32(order.len) {
			break
		}
		index := order[slot]
		source_file := sources[index]
		file_flags := fastc_source_scan_flags(source_file.source)
		mut generics := map[string]FastcGenericMethodSource{}
		if file_flags.has_generic_fn_syntax {
			fastc_collect_generic_methods_in_file(source_file, prefs, index, mut generics)
		}
		flagged := [
			FastcSourceFile{
				path: source_file.path
				source: source_file.source
				source_offset: source_file.source_offset
				header: fastc_header_with_scan_flags(source_file.header, file_flags)
			},
		]
		partial := fastc_collect_declaration_chunk(flagged, prefs, 0, 1)
		partials << FastcIndexedIndexPartial{
			index: index
			flags: file_flags
			generics: generics
			partial: partial
			body_spans: partial.body_spans[source_file.path] or { []int{} }
		}
	}
	return partials
}

// fastc_collect_generic_and_declaration_indexes runs the generic method scan
// and the declaration index in one parallel pass over the files: each file is
// visited once, and there is a single spawn/join round. The results are
// applied in file order, so they match the two serial scans.
fn fastc_collect_generic_and_declaration_indexes(mut sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut global_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) !map[string]FastcGenericMethodSource {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		generic_method_sources := fastc_collect_generic_method_sources(mut sources, prefs)
		fastc_collect_declaration_indexes(sources, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
		return generic_method_sources
	}
	order := fastc_file_generation_order(sources)
	mut queue := &FastcGenQueue{
		next: 0
	}
	second_thread := spawn fastc_collect_index_worker(sources, prefs, order, queue)
	mut chunk_threads := [second_thread]
	for _ in 2 .. jobs {
		chunk_thread := spawn fastc_collect_index_worker(sources, prefs, order, queue)
		chunk_threads << chunk_thread
	}
	mut partials := []FastcIndexedIndexPartial{len: sources.len}
	first := fastc_collect_index_worker(sources, prefs, order, queue)
	for indexed in first {
		partials[indexed.index] = indexed
	}
	for chunk_thread in chunk_threads {
		worker_partials := chunk_thread.wait()
		for indexed in worker_partials {
			partials[indexed.index] = indexed
		}
	}
	mut generic_method_sources := map[string]FastcGenericMethodSource{}
	for index, indexed in partials {
		source_file := sources[index]
		sources[index] = FastcSourceFile{
			path: source_file.path
			source: source_file.source
			source_offset: source_file.source_offset
			header: fastc_header_with_body_spans(fastc_header_with_scan_flags(source_file.header, indexed.flags), indexed.body_spans)
		}
		for key, generic in indexed.generics {
			generic_method_sources[key] = generic
		}
		fastc_merge_declaration_partial(indexed.partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
	}
	return generic_method_sources
}

struct FastcIndexedFileGenOutput {
	index  int
	output FastcFileGenOutput
}

// FastcFileGenWorkerResult is one worker's outputs together with the union
// of their composite and fixed-array type registrations, so the stitch pass
// merges one set per worker instead of one per file.
struct FastcFileGenWorkerResult {
	outputs           []FastcIndexedFileGenOutput
	composite_types   map[string]bool
	fixed_array_types map[string]string
}

// FastcGenQueue is a shared work-stealing counter for per-file generation. On a
// heterogeneous CPU (Apple Silicon's performance + efficiency cores) a static
// byte-weighted chunk per worker leaves the slowest efficiency core setting the
// wall time while performance cores idle; a lock-free atomic index lets every
// core keep pulling files until the queue drains, so all finish together.
struct FastcGenQueue {
mut:
	next u32
}

// fastc_file_generation_order returns file indices largest-first, so the biggest
// files are claimed before the queue tail thins out (bounding end-of-run skew).
fn fastc_file_generation_order(sources []FastcSourceFile) []int {
	mut order := []int{len: sources.len}
	for i in 0 .. sources.len {
		order[i] = i
	}
	// A few hundred files at most; insertion sort avoids a closure and keeps this
	// helper in FastC's self-hostable subset.
	for i in 1 .. order.len {
		index := order[i]
		weight := sources[index].source.len
		mut insert_at := i
		for insert_at > 0 && sources[order[insert_at - 1]].source.len < weight {
			order[insert_at] = order[insert_at - 1]
			insert_at--
		}
		order[insert_at] = index
	}
	return order
}

// fastc_generate_file_steal drains the shared queue: each worker atomically
// claims the next order slot until the queue empties. The backing source data is
// shared and read-only. It runs as a value-returning spawn: under -prealloc, V
// frees a void thread's arena on exit, so outputs travel through the return value.
fn fastc_generate_file_steal(ctx &FastcFileGenContext, sources []FastcSourceFile, order []int, queue &FastcGenQueue) FastcFileGenWorkerResult {
	mut outputs := []FastcIndexedFileGenOutput{}
	mut composite_types := map[string]bool{}
	mut fixed_array_types := map[string]string{}
	limit := u32(order.len)
	bench_files := os.getenv('FASTC_BENCH_FILES') != ''
	mut sw := time.new_stopwatch()
	for {
		// Relaxed ordering is enough: the counter only hands out unique indices,
		// and `order`/`sources` are fully initialized and read-only before the
		// workers start. fastc_atomic_fetch_add_u32 is platform-specialized (the
		// GCC/Clang/TCC builtin on Unix, InterlockedExchangeAdd on Windows).
		claimed := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if claimed >= limit {
			break
		}
		file_index := order[claimed]
		start_us := sw.elapsed().microseconds()
		output := fastc_generate_single_file(ctx, sources[file_index])
		for name, _ in output.composite_types {
			composite_types[name] = true
		}
		for name, array_type in output.fixed_array_types {
			fixed_array_types[name] = array_type
		}
		outputs << FastcIndexedFileGenOutput{
			index: file_index
			output: output
		}
		if bench_files {
			eprintln('fastc-file ${sw.elapsed().microseconds() - start_us} ${sources[file_index].source.len} ${sources[file_index].path}')
		}
	}
	return FastcFileGenWorkerResult{
		outputs: outputs
		composite_types: composite_types
		fixed_array_types: fixed_array_types
	}
}

const fastc_generation_fragment_size = 28 * 1024

fn fastc_generation_fragment_is_followed_by_comptime_else(scan scanner.Scanner) bool {
	mut lookahead := scan
	mut tok := lookahead.scan()
	for tok == .semicolon {
		tok = lookahead.scan()
	}
	if tok == .dollar {
		tok = lookahead.scan()
	}
	return tok == .key_else
}

fn fastc_source_generation_fragments(source_file FastcSourceFile, prefs &pref.Preferences) []FastcSourceFile {
	if !fastc_source_needs_fragmentation(source_file, prefs) {
		return [source_file]
	}
	part_count := (source_file.source.len + fastc_generation_fragment_size - 1) / fastc_generation_fragment_size
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut cuts := [0]
	mut brace_depth := 0
	mut paren_depth := 0
	mut bracket_depth := 0
	mut pending_cut := false
	mut next_target := source_file.source.len / part_count
	mut tok := scan.scan()
	for tok != .eof && cuts.len < part_count {
		match tok {
			.lcbr {
				brace_depth++
			}
			.rcbr {
				brace_depth--
				if brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 && scan.offset >= next_target {
					pending_cut = true
				}
			}
			.lpar {
				paren_depth++
			}
			.rpar {
				paren_depth--
			}
			.attribute {
				// The scanner consumes `@[` as one token.
				bracket_depth++
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				bracket_depth--
			}
			.semicolon {
				if pending_cut && brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 && !fastc_generation_fragment_is_followed_by_comptime_else(scan) {
					cuts << scan.offset
					next_target = source_file.source.len * cuts.len / part_count
					pending_cut = false
				}
			}
			else {}
		}
		tok = scan.scan()
	}
	if cuts.len == 1 {
		return [source_file]
	}
	cuts << source_file.source.len
	mut fragments := []FastcSourceFile{cap: cuts.len - 1}
	mut position_offset := 0
	mut position_lines := 0
	mut position_column := 0
	for i in 0 .. cuts.len - 1 {
		start := cuts[i]
		for position_offset < start {
			if source_file.source[position_offset] == `\n` {
				position_lines++
				position_column = 0
			} else {
				position_column++
			}
			position_offset++
		}
		prefix := '\n'.repeat(position_lines) + ' '.repeat(position_column)
		fragments << FastcSourceFile{
			path: source_file.path
			source: prefix + source_file.source[start..cuts[i + 1]]
			source_offset: source_file.source_offset + start - prefix.len
			header: source_file.header
		}
	}
	return fragments
}

fn fastc_source_needs_fragmentation(source_file FastcSourceFile, prefs &pref.Preferences) bool {
	return prefs.building_v && source_file.header.module_name.ends_with('fastc') && source_file.source.len > fastc_generation_fragment_size
}

struct FastcFragmentedSource {
	index     int
	fragments []FastcSourceFile
}

fn fastc_generation_fragment_chunk(candidates []FastcSourceFile, candidate_indices []int, prefs &pref.Preferences, start int, end int) []FastcFragmentedSource {
	mut result := []FastcFragmentedSource{cap: end - start}
	for i in start .. end {
		result << FastcFragmentedSource{
			index: candidate_indices[i]
			fragments: fastc_source_generation_fragments(candidates[i], prefs)
		}
	}
	return result
}

// fastc_generation_fragments splits oversized self-host sources into
// top-level fragments. Finding the cut points scans each candidate with the
// scanner, so candidates are scanned on parallel workers; the fragments are
// then spliced back in source order.
fn fastc_generation_fragments(sources []FastcSourceFile, prefs &pref.Preferences) []FastcSourceFile {
	mut candidates := []FastcSourceFile{}
	mut candidate_indices := []int{}
	for index, source_file in sources {
		if fastc_source_needs_fragmentation(source_file, prefs) {
			candidates << source_file
			candidate_indices << index
		}
	}
	mut fragments := []FastcSourceFile{cap: sources.len + candidates.len * 4}
	if candidates.len == 0 {
		fragments << sources
		return fragments
	}
	jobs := fastc_parallel_job_count(candidates.len, prefs)
	mut fragmented := []FastcFragmentedSource{cap: candidates.len}
	if jobs <= 1 {
		fragmented = fastc_generation_fragment_chunk(candidates, candidate_indices, prefs, 0, candidates.len)
	} else {
		bounds := fastc_chunk_bounds(candidates, jobs)
		second_thread := spawn fastc_generation_fragment_chunk(candidates, candidate_indices, prefs, bounds[2], bounds[3])
		mut chunk_threads := [second_thread]
		for chunk_idx in 2 .. bounds.len / 2 {
			chunk_thread := spawn fastc_generation_fragment_chunk(candidates, candidate_indices, prefs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
			chunk_threads << chunk_thread
		}
		fragmented << fastc_generation_fragment_chunk(candidates, candidate_indices, prefs, bounds[0], bounds[1])
		for chunk_thread in chunk_threads {
			fragmented << chunk_thread.wait()
		}
	}
	mut next_fragmented := 0
	for index, source_file in sources {
		if next_fragmented < fragmented.len && fragmented[next_fragmented].index == index {
			fragments << fragmented[next_fragmented].fragments
			next_fragmented++
		} else {
			fragments << source_file
		}
	}
	return fragments
}

// FastcPendingTypeDeclarations is the type phase running on a worker while
// the signatures are collected; the two only share read-only tables.
struct FastcPendingTypeDeclarations {
mut:
	workers []thread FastcTypeDeclarationResult
	result  FastcTypeDeclarationResult
}

fn fastc_start_type_declarations(sources []FastcSourceFile, type_sources map[string]string, prefs &pref.Preferences, type_source_paths map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool) FastcPendingTypeDeclarations {
	if fastc_parallel_worker_limit(prefs) <= 1 {
		return FastcPendingTypeDeclarations{
			result: fastc_run_type_declarations(sources, type_sources, prefs, type_source_paths, declared_types, declared_kinds, enum_flags, constants, public_constants)
		}
	}
	return FastcPendingTypeDeclarations{
		workers: [
			spawn fastc_run_type_declarations(sources, type_sources, prefs, type_source_paths, declared_types, declared_kinds, enum_flags, constants, public_constants),
		]
	}
}

fn fastc_wait_type_declarations(mut pending FastcPendingTypeDeclarations) !FastcTypeDeclarationResult {
	mut result := pending.result
	if pending.workers.len > 0 {
		result = pending.workers[0].wait()
	}
	if result.failed {
		return error(result.error_message)
	}
	return result
}

// FastcPendingFieldDefaults is the struct field default rendering running on
// a worker while the constants are pre-parsed.
struct FastcPendingFieldDefaults {
mut:
	workers []thread FastcFieldDefaultsResult
	result  FastcFieldDefaultsResult
}

fn fastc_start_field_defaults(source_imports map[string]map[string]string, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types map[string]string, enum_field_names map[string][]string, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, global_types map[string]string, sum_types map[string]bool) FastcPendingFieldDefaults {
	if fastc_parallel_worker_limit(prefs) <= 1 {
		return FastcPendingFieldDefaults{
			result: fastc_run_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, enum_field_names, alias_base_types, struct_fields, struct_field_info, functions, constants, public_constants, constant_types, globals, public_globals, global_types, sum_types)
		}
	}
	return FastcPendingFieldDefaults{
		workers: [
			spawn fastc_run_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, enum_field_names, alias_base_types, struct_fields, struct_field_info, functions, constants, public_constants, constant_types, globals, public_globals, global_types, sum_types),
		]
	}
}

fn fastc_wait_field_defaults(mut pending FastcPendingFieldDefaults) !FastcFieldDefaultsResult {
	mut result := pending.result
	if pending.workers.len > 0 {
		result = pending.workers[0].wait()
	}
	if result.failed {
		return error(result.error_message)
	}
	return result
}

// FastcPendingFragments is the fragmentation of the sources for parallel
// generation, running on a worker while the declaration phases proceed.
struct FastcPendingFragments {
mut:
	workers   []thread []FastcSourceFile
	fragments []FastcSourceFile
}

// fastc_start_generation_fragments starts splitting oversized sources into
// generation fragments in the background; a serial run generates whole files,
// so it gets the sources back unchanged.
fn fastc_start_generation_fragments(sources []FastcSourceFile, prefs &pref.Preferences) FastcPendingFragments {
	if fastc_parallel_job_count(sources.len, prefs) <= 1 {
		return FastcPendingFragments{
			fragments: sources
		}
	}
	return FastcPendingFragments{
		workers: [spawn fastc_generation_fragments(sources, prefs)]
	}
}

fn fastc_wait_generation_fragments(mut pending FastcPendingFragments) []FastcSourceFile {
	if pending.workers.len == 0 {
		return pending.fragments
	}
	return pending.workers[0].wait()
}

// fastc_generate_file_outputs runs per-file code generation, in parallel when
// more than one job is available. `sources` are the generation fragments from
// fastc_wait_generation_fragments. Results are restored to file order, so the
// emitted C is identical to a serial run.
fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) FastcFileGenResult {
	jobs := fastc_parallel_jobs(sources, ctx.prefs)
	if jobs <= 1 {
		mut outputs := []FastcFileGenOutput{cap: sources.len}
		for source_file in sources {
			outputs << fastc_generate_single_file(ctx, source_file)
		}
		return fastc_file_gen_result(outputs)
	}
	mut timer := fastc_new_phase_timer()
	generation_sources := sources
	order := fastc_file_generation_order(generation_sources)
	mut queue := &FastcGenQueue{
		next: 0
	}
	timer.mark('file_outputs.order')
	second_thread := spawn fastc_generate_file_steal(ctx, generation_sources, order, queue)
	mut worker_threads := [second_thread]
	for _ in 2 .. jobs {
		worker_threads << spawn fastc_generate_file_steal(ctx, generation_sources, order, queue)
	}
	mut outputs := []FastcFileGenOutput{len: generation_sources.len}
	mut composite_types := map[string]bool{}
	mut fixed_array_types := map[string]string{}
	first := fastc_generate_file_steal(ctx, generation_sources, order, queue)
	for indexed_output in first.outputs {
		outputs[indexed_output.index] = indexed_output.output
	}
	fastc_merge_worker_types(first, mut composite_types, mut fixed_array_types)
	timer.mark('file_outputs.first_chunk')
	for worker_thread in worker_threads {
		worker := worker_thread.wait()
		for indexed_output in worker.outputs {
			outputs[indexed_output.index] = indexed_output.output
		}
		fastc_merge_worker_types(worker, mut composite_types, mut fixed_array_types)
	}
	timer.mark('file_outputs.wait_chunks')
	return FastcFileGenResult{
		outputs: outputs
		composite_types: composite_types
		fixed_array_types: fixed_array_types
	}
}

fn fastc_merge_worker_types(worker FastcFileGenWorkerResult, mut composite_types map[string]bool, mut fixed_array_types map[string]string) {
	for name, _ in worker.composite_types {
		composite_types[name] = true
	}
	for name, array_type in worker.fixed_array_types {
		fixed_array_types[name] = array_type
	}
}

// FastcReferencePartial carries one chunk's function-reference scan across
// its thread boundary; partials union commutatively into program totals.
struct FastcReferencePartial {
mut:
	references           map[string]map[string]bool
	top_level_references map[string]bool
}

fn fastc_collect_reference_chunk(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, start int, end int) FastcReferencePartial {
	mut references := map[string]map[string]bool{}
	mut top_level_references := map[string]bool{}
	for idx in start .. end {
		fastc_collect_file_references(sources[idx], prefs, available_names, mut references, mut top_level_references)
	}
	return FastcReferencePartial{
		references: references
		top_level_references: top_level_references
	}
}

fn fastc_merge_reference_partial(chunk FastcReferencePartial, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	for function_name in chunk.references.keys() {
		mut combined := map[string]bool{}
		if function_name in references {
			combined = references[function_name].clone()
		}
		for referenced_name, _ in chunk.references[function_name] {
			combined[referenced_name] = true
		}
		references[function_name] = combined.clone()
	}
	for referenced_name, _ in chunk.top_level_references {
		top_level_references[referenced_name] = true
	}
}

// fastc_collect_reference_partials fans the per-file reference scan out over
// size-balanced chunks and unions the partials in file order. Reference sets
// union commutatively, so the merged result matches a serial scan exactly.
fn fastc_collect_reference_partials(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		for source_file in sources {
			fastc_collect_file_references(source_file, prefs, available_names, mut references, mut top_level_references)
		}
		return
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	second_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[2], bounds[3])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	first := fastc_collect_reference_chunk(sources, prefs, available_names, bounds[0], bounds[1])
	fastc_merge_reference_partial(first, mut references, mut top_level_references)
	for chunk_thread in chunk_threads {
		chunk := chunk_thread.wait()
		fastc_merge_reference_partial(chunk, mut references, mut top_level_references)
	}
}

struct FastcIndexedDeclarationPartial {
	index   int
	partial FastcDeclarationPartial
}

// fastc_collect_declaration_worker collects one file at a time from the shared
// counter (largest files first); the caller merges the partials in file order,
// so the result matches a serial pass exactly.
fn fastc_collect_declaration_worker(sources []FastcSourceFile, prefs &pref.Preferences, order []int, queue &FastcGenQueue) []FastcIndexedDeclarationPartial {
	mut partials := []FastcIndexedDeclarationPartial{}
	for {
		slot := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if slot >= u32(order.len) {
			break
		}
		index := order[slot]
		partials << FastcIndexedDeclarationPartial{
			index: index
			partial: fastc_collect_declaration_chunk(sources, prefs, index, index + 1)
		}
	}
	return partials
}

fn fastc_collect_declaration_indexes(sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut global_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) ! {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		partial := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
		fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
		return
	}
	order := fastc_file_generation_order(sources)
	mut queue := &FastcGenQueue{
		next: 0
	}
	second_thread := spawn fastc_collect_declaration_worker(sources, prefs, order, queue)
	mut chunk_threads := [second_thread]
	for _ in 2 .. jobs {
		chunk_thread := spawn fastc_collect_declaration_worker(sources, prefs, order, queue)
		chunk_threads << chunk_thread
	}
	mut partials := []FastcDeclarationPartial{len: sources.len}
	first := fastc_collect_declaration_worker(sources, prefs, order, queue)
	for indexed in first {
		partials[indexed.index] = indexed.partial
	}
	for chunk_thread in chunk_threads {
		worker_partials := chunk_thread.wait()
		for indexed in worker_partials {
			partials[indexed.index] = indexed.partial
		}
	}
	for partial in partials {
		fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
	}
}

struct FastcIndexedSignaturePartial {
	index   int
	partial FastcSignaturePartial
}

// fastc_collect_signature_worker collects one file at a time from the shared
// counter (largest files first); the caller merges the partials in file order.
fn fastc_collect_signature_worker(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, order []int, queue &FastcGenQueue) []FastcIndexedSignaturePartial {
	mut partials := []FastcIndexedSignaturePartial{}
	for {
		slot := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if slot >= u32(order.len) {
			break
		}
		index := order[slot]
		partials << FastcIndexedSignaturePartial{
			index: index
			partial: fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, index, index + 1)
		}
	}
	return partials
}

fn fastc_collect_signatures(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []string, mut embed_embeddeds []string) ! {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		partial := fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, 0, sources.len)
		fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
		return
	}
	order := fastc_file_generation_order(sources)
	mut queue := &FastcGenQueue{
		next: 0
	}
	second_thread := spawn fastc_collect_signature_worker(sources, prefs, declared_types, declared_type_c_names, params_structs, order, queue)
	mut chunk_threads := [second_thread]
	for _ in 2 .. jobs {
		chunk_thread := spawn fastc_collect_signature_worker(sources, prefs, declared_types, declared_type_c_names, params_structs, order, queue)
		chunk_threads << chunk_thread
	}
	mut partials := []FastcSignaturePartial{len: sources.len}
	first := fastc_collect_signature_worker(sources, prefs, declared_types, declared_type_c_names, params_structs, order, queue)
	for indexed in first {
		partials[indexed.index] = indexed.partial
	}
	for chunk_thread in chunk_threads {
		worker_partials := chunk_thread.wait()
		for indexed in worker_partials {
			partials[indexed.index] = indexed.partial
		}
	}
	// Size the program map once: growing it by doubling while thousands of
	// signatures are merged would rehash every key several times.
	functions.reserve(u32(functions.len + fastc_signature_partial_count(partials)))
	for partial in partials {
		fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	}
}

// fastc_parse_constant_file_worker parses candidate files until the shared
// counter runs past the end of `order` (largest files first).
fn fastc_parse_constant_file_worker(ctx &FastcConstantGenContext, candidates []FastcSourceFile, seed map[string]string, order []int, queue &FastcGenQueue) []FastcIndexedConstantFileResult {
	mut results := []FastcIndexedConstantFileResult{}
	bench_files := os.getenv('FASTC_BENCH_FILES') != ''
	for {
		slot := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if slot >= u32(order.len) {
			break
		}
		index := order[slot]
		file_sw := time.new_stopwatch()
		results << FastcIndexedConstantFileResult{
			index: index
			result: fastc_parse_constant_file(ctx, candidates[index], seed.clone())
		}
		if bench_files {
			eprintln('fastc-constants-file ${file_sw.elapsed().microseconds()}us ${candidates[index].source.len} bytes ${candidates[index].path}')
		}
	}
	return results
}

// fastc_parse_constant_files_parallel parses every candidate file's constants
// on parallel workers, each starting from the phase's initial constant types.
// It returns an empty list when the phase runs serially.
fn fastc_parse_constant_files_parallel(ctx &FastcConstantGenContext, candidates []FastcSourceFile, seed map[string]string) []FastcConstantFileResult {
	jobs := fastc_parallel_job_count(candidates.len, ctx.prefs)
	if jobs <= 1 {
		return []FastcConstantFileResult{}
	}
	order := fastc_file_generation_order(candidates)
	mut queue := &FastcGenQueue{
		next: 0
	}
	second_thread := spawn fastc_parse_constant_file_worker(ctx, candidates, seed, order, queue)
	mut chunk_threads := [second_thread]
	for _ in 2 .. jobs {
		chunk_thread := spawn fastc_parse_constant_file_worker(ctx, candidates, seed, order, queue)
		chunk_threads << chunk_thread
	}
	mut results := []FastcConstantFileResult{len: candidates.len}
	first_results := fastc_parse_constant_file_worker(ctx, candidates, seed, order, queue)
	for indexed in first_results {
		results[indexed.index] = indexed.result
	}
	for chunk_thread in chunk_threads {
		chunk_results := chunk_thread.wait()
		for indexed in chunk_results {
			results[indexed.index] = indexed.result
		}
	}
	return results
}
