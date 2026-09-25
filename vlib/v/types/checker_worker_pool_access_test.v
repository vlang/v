module types

import os
import v.flat

// Every checker pass must reach the worker pool through checker_worker_pool or
// ensure_checker_worker_pool. Both return nil in a `v3_no_parallel` build, so
// that is what keeps such a build from fanning checker work out to threads.
fn test_checker_reaches_the_worker_pool_only_through_its_gatekeepers() {
	dir := os.dir(@FILE)
	gatekeepers := ['checker_worker_pool', 'ensure_checker_worker_pool']
	mut scanned := 0
	for entry in os.ls(dir) or { panic(err) } {
		if !entry.ends_with('.v') || entry.ends_with('_test.v') {
			continue
		}
		mut current_fn := ''
		lines := (os.read_file(os.join_path(dir, entry)) or { panic(err) }).split_into_lines()
		for i, line in lines {
			if line.starts_with('fn ') || line.starts_with('pub fn ') {
				current_fn = declared_fn_name(line)
			}
			if line.trim_space().starts_with('//') || !line.contains('.worker_pool') {
				continue
			}
			assert current_fn in gatekeepers, '${entry}:${i + 1} in `${current_fn}` uses `.worker_pool` directly'
		}
		scanned++
	}
	// Guard against the scan silently matching nothing after a move.
	assert scanned > 10
}

// In a parallel build the gatekeepers hand checker passes the driver's pool.
fn test_checker_worker_pool_is_the_ast_pool_in_parallel_builds() {
	assert !checker_serial_only()
	mut a := flat.FlatAst.new()
	assert isnil(checker_worker_pool(&a))
	a.ensure_workers(2)
	defer {
		a.close_workers()
	}
	assert voidptr(checker_worker_pool(&a)) == voidptr(a.worker_pool)
	assert voidptr(ensure_checker_worker_pool(mut a)) == voidptr(a.worker_pool)
}

// declared_fn_name returns the name declared by a top-level `fn` line, with any
// receiver and generic parameters stripped.
fn declared_fn_name(line string) string {
	mut rest := line.all_after('fn ')
	if rest.starts_with('(') {
		rest = rest.all_after(') ')
	}
	return rest.all_before('(').all_before('[').trim_space()
}
