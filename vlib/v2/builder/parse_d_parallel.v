// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import sync
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
	// flat_enabled is reserved for the future thread-safe flat parallel parser.
	// The default flat pipeline currently returns before spawning workers.
	flat_enabled bool
	flat_mu      &sync.Mutex      = unsafe { nil }
	flat_builder &ast.FlatBuilder = unsafe { nil }
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

fn (mut pstate ParsingSharedState) append_to_flat(file ast.File) {
	pstate.flat_mu.lock()
	pstate.flat_builder.append_file(file)
	pstate.flat_mu.unlock()
}

fn worker(mut wp util.WorkerPool[string, ast.File], mut pstate ParsingSharedState, prefs &pref.Preferences) {
	mut p := parser.Parser.new(prefs)
	target_os := prefs.source_filter_target_os()
	allow_pkgconfig_imports := !prefs.is_cross_target()
	for {
		filename := wp.get_job() or { break }
		ast_file := p.parse_file(filename, mut pstate.file_set)
		// Queue new jobs for imports before pushing result
		skip_imports := prefs.skip_imports
		if !skip_imports {
			for mod in active_file_imports_with_options(ast_file, prefs.user_defines,
				prefs.explicit_user_defines, target_os, allow_pkgconfig_imports) {
				if pstate.already_parsed_module(mod.name) {
					continue
				}
				pstate.mark_module_as_parsed(mod.name)
				mod_path := prefs.get_module_path(mod.name, ast_file.name)
				wp.queue_jobs(get_v_files_from_dir(mod_path, prefs.user_defines, target_os))
			}
		}
		if pstate.flat_enabled {
			// In flat mode the FlatBuilder is the canonical store; push a
			// throwaway empty File over the result channel just so the pool
			// can detect job completion. parse_files_parallel discards the
			// accumulated results and derives b.files from b.flat in build().
			pstate.append_to_flat(ast_file)
			wp.push_result(ast.File{})
		} else {
			wp.push_result(ast_file)
		}
	}
}

fn (mut b Builder) parse_files_parallel(files []string) []ast.File {
	if b.flat_check_enabled {
		// FlatAst mode is the default pipeline. Keep parsing serial for now:
		// token.FileSet shares position counters and file slices that are not
		// concurrency-safe, and selector_names are keyed by those position ids.
		return b.parse_files(files)
	}
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
		// When a valid header cache exists, use lightweight .vh summaries
		// instead of fully parsing every core module source file.
		use_core_headers2 := b.can_use_cached_core_headers_for_parse()
		// Parse builtin and its dependencies
		// Mark them as parsed first to prevent re-parsing via imports
		for module_path in core_cached_module_paths {
			pstate.mark_module_as_parsed(module_path)
		}
		if use_core_headers2 {
			worker_pool.queue_jobs(b.core_cached_parse_paths())
		} else {
			target_os := b.pref.source_filter_target_os()
			for module_path in core_cached_module_paths {
				worker_pool.queue_jobs(get_v_files_from_dir(b.pref.get_vlib_module_path(module_path),
					b.pref.user_defines, target_os))
			}
		}
	}
	// parse user files
	worker_pool.queue_jobs(files)

	results := worker_pool.wait_for_results()
	return results
}
