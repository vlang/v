module builder

import v2.ast
import v2.types
// import v2.util
// import runtime

fn type_check_worker(ch_in chan []ast.File, ch_out chan string) {
	for {
		// ast_files := <- ch_in or { break }
	}
}

fn (mut b Builder) type_check_files() {
	// mod := types.new_module('main', '')
	env := types.Environment.new()
	mut checker := types.Checker.new(b.pref, b.file_set, env)
	checker.check_files(b.files)

	// mut ch_in := chan []ast.File{cap: 1000}
	// mut ch_out := chan string{cap: 1000}
	// mut worker_pool := util.WorkerPool.new[[]ast.File, string](mut ch_in, mut ch_out)	
	// for _ in 0..runtime.nr_jobs() {
	// 	worker_pool.workers << spawn type_check_worker(ch_in, ch_out)
	// }
}
