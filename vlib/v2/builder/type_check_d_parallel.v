// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.pref
import v2.token
import v2.types
import v2.util
import runtime

// CheckResult holds the result from a type checking worker
struct CheckResult {
	deferred []types.Deferred
}

// Phase 1 worker: preregister scopes and type names
fn type_check_worker_phase1(mut wp util.WorkerPool[ast.File, CheckResult], env &types.Environment, prefs &pref.Preferences, file_set &token.FileSet) {
	mut checker := types.Checker.new(prefs, file_set, env)
	for {
		file := wp.get_job() or { break }
		checker.preregister_scopes(file)
		checker.preregister_types(file)
		// Return struct deferred items (for field resolution)
		wp.push_result(CheckResult{
			deferred: checker.take_deferred()
		})
	}
}

// Phase 2 worker: full type checking
fn type_check_worker_phase2(mut wp util.WorkerPool[ast.File, CheckResult], env &types.Environment, prefs &pref.Preferences, file_set &token.FileSet) {
	mut checker := types.Checker.new(prefs, file_set, env)
	for {
		file := wp.get_job() or { break }
		checker.check_file(file)
		// Collect deferred items to be processed after all files are checked
		wp.push_result(CheckResult{
			deferred: checker.take_deferred()
		})
	}
}

fn (mut b Builder) type_check_files_parallel() &types.Environment {
	env := types.Environment.new()

	// Phase 1: Preregister all scopes and type names in parallel
	// This is the main parallelization win - registering types from many files
	// Note: Each worker has its own c_scope for C-language types. This is a
	// limitation that could be improved by making c_scope shared in Environment.
	mut wp1 := util.WorkerPool.new[ast.File, CheckResult]()
	for _ in 0 .. runtime.nr_jobs() {
		wp1.add_worker(spawn type_check_worker_phase1(mut wp1, env, b.pref, b.file_set))
	}
	for file in b.files {
		wp1.queue_job(file)
	}
	phase1_results := wp1.wait_for_results()

	// Process struct deferred items (fields) after all names registered
	// This must be done sequentially as it resolves type references
	mut checker := types.Checker.new(b.pref, b.file_set, env)
	for result in phase1_results {
		checker.add_deferred(result.deferred)
	}
	checker.process_struct_deferred()

	// Pre-register all function signatures (including methods)
	// This must happen before check_file so methods are available when called
	for file in b.files {
		checker.preregister_fn_signatures(file)
	}

	// Phase 2: Full type checking - sequential for now
	// Because deferred closures capture the checker reference,
	// they can't be safely transferred between workers.
	// Future optimization: make closures capture data instead of references.
	for file in b.files {
		checker.check_file(file)
	}

	// Process all deferred items
	checker.process_all_deferred()

	return env
}
