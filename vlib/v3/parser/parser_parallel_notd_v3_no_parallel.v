module parser

import os
import runtime
import v3.flat

// Parallel file parsing. The input file list is split into contiguous,
// size-balanced chunks; each worker parses its chunk into a private Parser +
// FlatAst on its own thread, and the master folds the results back in chunk
// order, shifting node ids and children offsets by the merge position. A
// file's nodes only ever reference nodes of the same file, and chunks are
// contiguous slices of the input list merged in input order, so the merged
// nodes/children arrays are laid out exactly as a serial parse of the same
// list — the phases downstream see an identical AST either way.

const min_parallel_parse_files = 4
const min_parallel_parse_bytes = 131072
const max_parallel_parse_jobs = 8

$if !windows {
	// ParseChunkArgs is the payload handed to each worker thread.
	struct ParseChunkArgs {
		worker     voidptr // &Parser
		paths_ptr  voidptr // &[]string
		starts_ptr voidptr // &[]int (worker-local starts; the master shifts them on merge)
		start      int
		end        int
	}

	// C.pthread_t declares C pthread t data used by parser.
	@[typedef]
	struct C.pthread_t {}

	// C.pthread_create declares the C pthread_create symbol used by parser.
	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int

	// C.pthread_join declares the C pthread_join symbol used by parser.
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int

	// C.pthread_attr_init declares the C pthread_attr_init symbol used by parser.
	fn C.pthread_attr_init(attr voidptr) int

	// C.pthread_attr_setstacksize declares the C pthread_attr_setstacksize symbol used by parser.
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int

	// C.pthread_attr_destroy declares the C pthread_attr_destroy symbol used by parser.
	fn C.pthread_attr_destroy(attr voidptr) int

	// parse_chunk_thread parses one worker's contiguous range of files into the
	// worker's private FlatAst, recording each file's worker-local first node id
	// into its own preallocated slot of the shared starts array.
	fn parse_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &ParseChunkArgs(arg) }
		mut w := unsafe { &Parser(a.worker) }
		paths := unsafe { &[]string(a.paths_ptr) }
		mut starts := unsafe { &[]int(a.starts_ptr) }
		for i in a.start .. a.end {
			unsafe {
				(*starts)[i] = w.a.nodes.len
			}
			w.parse_into((*paths)[i])
		}
		return unsafe { nil }
	}
}

// parse_files_dispatch parses paths in order, appending to p.a exactly like a
// serial parse_into loop, across worker threads when there is enough work.
// Returns each file's first node id in p.a and whether threads were used.
pub fn (mut p Parser) parse_files_dispatch(paths []string, allow_parallel bool) ([]int, bool) {
	$if windows {
		return p.parse_files_with_starts(paths), false
	} $else {
		if !allow_parallel || paths.len < min_parallel_parse_files {
			return p.parse_files_with_starts(paths), false
		}
		mut sizes := []i64{cap: paths.len}
		mut total_bytes := i64(0)
		for path in paths {
			size := i64(os.file_size(path))
			sizes << size
			total_bytes += size
		}
		n_jobs := parse_job_count(runtime.nr_jobs(), paths.len)
		if n_jobs <= 1 || total_bytes < min_parallel_parse_bytes {
			return p.parse_files_with_starts(paths), false
		}
		bounds := parse_chunk_bounds(sizes, n_jobs)
		thread_count := n_jobs - 1
		mut starts := []int{len: paths.len}
		// Worker parsers are cheap to build (no AST clone: parse output is
		// per-file independent), so all of them are created up front. Each
		// pre-reserves for its chunk's source bytes to avoid growth doubling.
		mut workers := []&Parser{cap: thread_count}
		for ci in 0 .. thread_count {
			mut w := Parser.new(p.prefs)
			mut chunk_bytes := i64(0)
			for i in bounds[ci + 1] .. bounds[ci + 2] {
				chunk_bytes += sizes[i]
			}
			w.reserve_for_source(int(chunk_bytes))
			workers << w
		}
		mut args := []ParseChunkArgs{cap: thread_count}
		for ci in 0 .. thread_count {
			args << ParseChunkArgs{
				worker:     voidptr(workers[ci])
				paths_ptr:  unsafe { voidptr(&paths) }
				starts_ptr: unsafe { voidptr(&starts) }
				start:      bounds[ci + 1]
				end:        bounds[ci + 2]
			}
		}
		mut thread_ids := []C.pthread_t{len: thread_count}
		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		// The parser recurses deeply on nested expressions; give workers a
		// roomy stack, like the transform and cgen workers.
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci in 0 .. thread_count {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, parse_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		// The master parses chunk 0 straight into p.a while the helpers run —
		// no merge needed for it, and its layout matches a serial parse.
		for i in bounds[0] .. bounds[1] {
			starts[i] = p.a.nodes.len
			p.parse_into(paths[i])
		}
		// Join and merge each helper in fixed chunk order (input file order),
		// so node numbering stays deterministic and byte-identical to serial.
		for ci in 0 .. thread_count {
			C.pthread_join(thread_ids[ci], unsafe { nil })
			p.merge_parsed_worker(workers[ci], mut starts, bounds[ci + 1], bounds[ci + 2])
		}
		return starts, true
	}
}

// merge_parsed_worker appends a finished worker's parsed output to the master
// AST. Worker node ids and children offsets are 0-based (the worker started
// from an empty FlatAst), so every node-id reference moves by the master's
// node count and every children_start by the master's children count at merge
// time. Negative child slots (flat.empty_node sentinels) are left untouched.
fn (mut p Parser) merge_parsed_worker(w &Parser, mut starts []int, chunk_start int, chunk_end int) {
	node_shift := p.a.nodes.len
	child_shift := i32(p.a.children.len)
	new_children := w.a.children.len
	if new_children > 0 {
		old_len := p.a.children.len
		unsafe {
			p.a.children.grow_len(new_children)
			vmemcpy(&p.a.children[old_len], &w.a.children[0],
				new_children * int(sizeof(flat.NodeId)))
		}
		for k in old_len .. p.a.children.len {
			cid := p.a.children[k]
			if int(cid) >= 0 {
				p.a.children[k] = flat.NodeId(int(cid) + node_shift)
			}
		}
	}
	new_nodes := w.a.nodes.len
	if new_nodes > 0 {
		nodes_old_len := p.a.nodes.len
		unsafe {
			p.a.nodes.grow_len(new_nodes)
			vmemcpy(&p.a.nodes[nodes_old_len], &w.a.nodes[0], new_nodes * int(sizeof(flat.Node)))
		}
		for k in nodes_old_len .. p.a.nodes.len {
			// Only nodes with children carry a live children_start; leaving
			// childless nodes untouched keeps their zero-value fields
			// byte-identical to a serial parse.
			if p.a.nodes[k].children_count != 0 {
				p.a.nodes[k] = p.a.nodes[k].with_shifted_children(child_shift)
			}
		}
	}
	// Per-file region starts move by the merge offset.
	for i in chunk_start .. chunk_end {
		starts[i] += node_shift
	}
	// The worker validated its exports against its own files only; revalidate
	// them here against disabled fns accumulated from earlier chunks, exactly
	// like the serial parse where register_pending_export sees every previously
	// parsed file. The worker's own disabled marks are folded in afterwards, so
	// a disable in a later file keeps earlier exports — matching the serial
	// order-dependent behavior.
	for rec in w.export_records {
		if rec.name in p.a.disabled_fns || rec.qname in p.a.disabled_fns {
			continue
		}
		p.a.export_fn_names[rec.qname] = rec.value
	}
	for name, disabled in w.a.disabled_fns {
		if disabled {
			p.a.disabled_fns[name] = true
		}
	}
	for name, is_noreturn in w.a.noreturn_fns {
		if is_noreturn {
			p.a.noreturn_fns[name] = true
		}
	}
	p.parsed_v_files += w.parsed_v_files
}

// parse_job_count caps the worker count by the runtime job count, a fixed
// ceiling and the file count.
fn parse_job_count(n_runtime_jobs int, n_files int) int {
	if n_runtime_jobs <= 0 || n_files <= 0 {
		return 0
	}
	mut n := n_runtime_jobs
	if n > max_parallel_parse_jobs {
		n = max_parallel_parse_jobs
	}
	if n > n_files {
		n = n_files
	}
	return n
}

// parse_chunk_bounds splits the file list into n contiguous ranges of roughly
// equal source byte count (parse time tracks source size closely). Contiguity
// in input order is required so the ordered merge reproduces the serial layout.
fn parse_chunk_bounds(sizes []i64, n int) []int {
	mut total := i64(0)
	for size in sizes {
		total += size + 1
	}
	mut bounds := []int{cap: n + 1}
	bounds << 0
	mut acc := i64(0)
	mut chunk := 1
	for i in 0 .. sizes.len {
		acc += sizes[i] + 1
		if chunk < n && acc >= total * i64(chunk) / i64(n) {
			bounds << i + 1
			chunk++
		}
	}
	for bounds.len < n + 1 {
		bounds << sizes.len
	}
	return bounds
}
