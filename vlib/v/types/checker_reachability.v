module types

import v.flat
import v.workers

const min_parallel_selected_fn_bodies = 64
const min_parallel_selected_fn_cost = 4096
const max_selected_fn_batch_bodies = 256
const max_selected_fn_batch_cost = 8192

struct SelectedFnFrontierScanArgs {
	master   voidptr
	frontier voidptr
	start    int
	end      int
mut:
	names []string
}

struct SelectedFnFrontierWorkerArgs {
	batches voidptr
	queue   chan int
}

fn selected_fn_frontier_scan_batch(arg voidptr) {
	mut args := unsafe { &SelectedFnFrontierScanArgs(arg) }
	master := unsafe { &TypeChecker(args.master) }
	frontier := unsafe { &[]SelectedFnDecl(args.frontier) }
	scratch := check_worker_scope_begin(true)
	mut worker := master.fork_for_parallel_check()
	// Program views normally share the diagnostic closure. Each scan discovers
	// only direct edges into its own set; the master combines them after join.
	worker.selected_file_called_fns = map[string]bool{}
	worker.selected_file_worklist = []string{}
	// Type resolution consults prior diagnostics for erroneous expressions.
	// This pass only reads them, and the master stays idle until all scans join.
	worker.errors = master.errors
	for i in args.start .. args.end {
		decl := unsafe { frontier[i] }
		worker.cur_file = decl.file
		worker.cur_module = decl.mod
		worker.collect_selected_file_fn_body_called_fns(worker.a.node(flat.NodeId(decl.idx)))
	}
	check_worker_scope_leave(scratch)
	// Resolution can create qualified names in the disposable arena. Publish
	// copies in the worker's persistent arena before releasing its checker.
	mut names := []string{cap: worker.selected_file_worklist.len}
	for name in worker.selected_file_worklist {
		names << name.clone()
	}
	args.names = names
	check_worker_scope_free(scratch)
	return
}

fn selected_fn_frontier_scan_thread(arg voidptr) voidptr {
	args := unsafe { &SelectedFnFrontierWorkerArgs(arg) }
	batches := unsafe { &[]SelectedFnFrontierScanArgs(args.batches) }
	for {
		i := <-args.queue or { break }
		selected_fn_frontier_scan_batch(unsafe { voidptr(&batches[i]) })
	}
	return unsafe { nil }
}

fn (tc &TypeChecker) selected_file_reachability_parallel_available() bool {
	pool := checker_worker_pool(tc.a)
	return tc.building_v_fast && tc.scope_parallel_check_workers && !isnil(pool)
		&& pool.size() > 0
}

fn (mut tc TypeChecker) collect_selected_file_frontier_parallel(frontier []SelectedFnDecl) bool {
	if !tc.selected_file_reachability_parallel_available()
		|| frontier.len < min_parallel_selected_fn_bodies {
		return false
	}
	pool := checker_worker_pool(tc.a)
	mut total_cost := i64(0)
	for decl in frontier {
		total_cost += decl.cost
	}
	if total_cost < min_parallel_selected_fn_cost {
		return false
	}
	// More than one bounded batch per worker lets the pool absorb large bodies
	// without retaining a whole reachable-program checker arena on every core.
	jobs := int_min(pool.size() + 1, max_scoped_check_jobs)
	batch_cost := int_min(max_selected_fn_batch_cost,
		int_max(int(total_cost / (jobs * 2)), min_parallel_selected_fn_cost / 2))
	mut args := []SelectedFnFrontierScanArgs{cap: frontier.len}
	mut start := 0
	for start < frontier.len {
		mut end := start
		mut cost := 0
		for end < frontier.len && end - start < max_selected_fn_batch_bodies {
			cost += frontier[end].cost
			end++
			if cost >= batch_cost {
				break
			}
		}
		args << SelectedFnFrontierScanArgs{
			master:   voidptr(tc)
			frontier: unsafe { voidptr(&frontier) }
			start:    start
			end:      end
		}
		start = end
	}
	queue := chan int{cap: args.len}
	for i in 0 .. args.len {
		queue <- i
	}
	queue.close()
	n_jobs := int_min(jobs, args.len)
	mut worker_args := []SelectedFnFrontierWorkerArgs{cap: n_jobs}
	mut tasks := []workers.Task{cap: n_jobs}
	for i in 0 .. n_jobs {
		worker_args << SelectedFnFrontierWorkerArgs{
			batches: unsafe { voidptr(&args) }
			queue:   queue
		}
		tasks << workers.Task{
			run:        selected_fn_frontier_scan_thread
			arg:        unsafe { voidptr(&worker_args[i]) }
			force_sync: i == 0
		}
	}
	pool.run(tasks)
	for arg in args {
		for name in arg.names {
			tc.enqueue_selected_file_fn(name)
		}
	}
	return true
}
