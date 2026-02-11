// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.pref
import v2.parser
import v2.token
import v2.util
import runtime

struct ParsingSharedState {
mut:
	file_set       &token.FileSet
	parsed_modules shared []string
}

fn (mut pstate ParsingSharedState) mark_module_as_parsed(name string) {
	lock pstate.parsed_modules {
		pstate.parsed_modules << name
	}
}

fn (mut pstate ParsingSharedState) already_parsed_module(name string) bool {
	rlock pstate.parsed_modules {
		if name in pstate.parsed_modules {
			return true
		}
	}
	return false
}

fn worker(mut wp util.WorkerPool[string, ast.File], mut pstate ParsingSharedState, prefs &pref.Preferences) {
	mut p := parser.Parser.new(prefs)
	for {
		filename := wp.get_job() or { break }
		ast_file := p.parse_file(filename, mut pstate.file_set)
		// Queue new jobs for imports before pushing result
		skip_imports := prefs.skip_imports
		if !skip_imports {
			for mod in ast_file.imports {
				if pstate.already_parsed_module(mod.name) {
					continue
				}
				pstate.mark_module_as_parsed(mod.name)
				mod_path := prefs.get_module_path(mod.name, ast_file.name)
				wp.queue_jobs(get_v_files_from_dir(mod_path))
			}
		}
		wp.push_result(ast_file)
	}
}

fn (mut b Builder) parse_files_parallel(files []string) []ast.File {
	mut pstate := &ParsingSharedState{
		file_set: b.file_set
	}

	// mut worker_pool := util.WorkerPool.new[string, ast.File](mut ch_in, mut ch_out)
	mut worker_pool := util.WorkerPool.new[string, ast.File]()
	// spawn workers
	for _ in 0 .. runtime.nr_jobs() {
		worker_pool.add_worker(spawn worker(mut worker_pool, mut pstate, b.pref))
	}
	skip_builtin := b.pref.skip_builtin
	if !skip_builtin {
		use_core_headers := b.can_use_cached_core_headers()
		// SSA/C and native backends need full core module bodies (not .vh summaries),
		// otherwise runtime helpers can be lowered to stubs.
		use_core_headers2 := if b.pref.backend in [.c, .cleanc, .x64, .arm64] {
			false
		} else {
			use_core_headers
		}
		// Parse builtin and its dependencies
		// Mark them as parsed first to prevent re-parsing via imports
		for module_path in core_cached_module_paths {
			pstate.mark_module_as_parsed(module_path)
		}
		if use_core_headers2 {
			worker_pool.queue_jobs(b.core_cached_parse_paths())
		} else {
			for module_path in core_cached_module_paths {
				worker_pool.queue_jobs(get_v_files_from_dir(b.pref.get_vlib_module_path(module_path)))
			}
		}
	}
	// parse user files
	worker_pool.queue_jobs(files)

	return worker_pool.wait_for_results()
}
