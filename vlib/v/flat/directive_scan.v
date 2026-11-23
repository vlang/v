module flat

import v.workers

// The driver asks several questions of the C directives of a program around
// code generation: the flags they add, the native inputs they name, the
// language those need. Each answer is a pass over every node that looks at
// `.file`, `.module_decl` and `.directive` nodes, and at the comptime blocks
// that can hold a directive, and skips the rest: a few thousand nodes of the
// millions of a large program, which each pass still has to read from memory.
// The index below lists those nodes once, across the worker pool, so that the
// passes read only them.

// directive_scan_min_nodes is the tree size from which the pool builds the
// index; a smaller tree is listed by the caller's thread.
const directive_scan_min_nodes = 65536

struct DirectiveScanArgs {
	a     voidptr // &FlatAst
	start int
	end   int
mut:
	ids []i32
}

@[direct_array_access]
fn directive_scan_thread(arg voidptr) voidptr {
	mut scan := unsafe { &DirectiveScanArgs(arg) }
	a := unsafe { &FlatAst(scan.a) }
	for i in scan.start .. scan.end {
		match a.nodes[i].kind {
			.file, .module_decl, .directive, .comptime_if, .comptime_for {
				scan.ids << i32(i)
			}
			else {}
		}
	}
	return unsafe { nil }
}

// index_directive_scan_nodes lists the nodes that a scan for C directives
// reads: every `.file`, `.module_decl`, `.directive`, `.comptime_if` and
// `.comptime_for` node, in node order. The index describes the nodes as they
// are now. It stays right while nodes of those kinds only change among those
// kinds or stop being of them (a resolved comptime block becomes its branch, a
// pruned node becomes empty), because a scan reads the kind of each listed node
// again; it is ignored once the node count changes (see directive_scan_index).
// A caller drops it (drop_directive_scan_index) before any other rewrite of the
// tree. Nothing may write the nodes while this runs.
pub fn (mut a FlatAst) index_directive_scan_nodes() {
	a.drop_directive_scan_index()
	n := a.nodes.len
	if n == 0 {
		return
	}
	mut jobs := 1
	if !isnil(a.worker_pool) && n >= directive_scan_min_nodes {
		jobs = a.worker_pool.size() + 1
	}
	mut scans := []DirectiveScanArgs{cap: jobs}
	for job in 0 .. jobs {
		scans << DirectiveScanArgs{
			a:     voidptr(a)
			start: int(i64(n) * job / jobs)
			end:   int(i64(n) * (job + 1) / jobs)
		}
	}
	if jobs == 1 {
		directive_scan_thread(unsafe { voidptr(&scans[0]) })
	} else {
		mut tasks := []workers.Task{cap: jobs}
		for job in 0 .. jobs {
			tasks << workers.Task{
				run:        directive_scan_thread
				arg:        unsafe { voidptr(&scans[job]) }
				force_sync: job == 0
			}
		}
		a.worker_pool.run(tasks)
	}
	mut total := 0
	for scan in scans {
		total += scan.ids.len
	}
	mut ids := []i32{cap: total}
	for scan in scans {
		ids << scan.ids
	}
	a.directive_scan_ids = ids
	a.directive_scan_nodes = n
}

// drop_directive_scan_index forgets the index: the scans read every node again.
pub fn (mut a FlatAst) drop_directive_scan_index() {
	a.directive_scan_ids = []i32{}
	a.directive_scan_nodes = 0
}

// directive_scan_index returns the index of index_directive_scan_nodes, or an
// empty list when there is none for the nodes of the tree as they are counted
// now: the caller then reads every node.
@[inline]
pub fn (a &FlatAst) directive_scan_index() []i32 {
	if a.directive_scan_nodes == 0 || a.directive_scan_nodes != a.nodes.len {
		return []i32{}
	}
	return a.directive_scan_ids
}
