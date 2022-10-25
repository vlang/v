// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v.pref
import v.util
import v.checker

pub type FnBackend = fn (mut b Builder)

pub fn compile(command string, pref &pref.Preferences, backend_cb FnBackend) {
	check_if_output_folder_is_writable(pref)
	// Construct the V object from command line arguments
	mut b := new_builder(pref)
	if b.should_rebuild() {
		b.rebuild(backend_cb)
	}
	b.exit_on_invalid_syntax()
	// running does not require the parsers anymore
	unsafe { b.myfree() }
	b.run_compiled_executable_and_exit()
}

fn check_if_output_folder_is_writable(pref &pref.Preferences) {
	odir := os.dir(pref.out_name)
	// When pref.out_name is just the name of an executable, i.e. `./v -o executable main.v`
	// without a folder component, just use the current folder instead:
	mut output_folder := odir
	if odir.len == pref.out_name.len {
		output_folder = os.getwd()
	}
	os.ensure_folder_is_writable(output_folder) or {
		// An early error here, is better than an unclear C error later:
		verror(err.msg())
	}
}

// Temporary, will be done by -autofree
[unsafe]
fn (mut b Builder) myfree() {
	// for file in b.parsed_files {
	// }
	unsafe { b.parsed_files.free() }
	util.free_caches()
}

fn (b &Builder) exit_on_invalid_syntax() {
	util.free_caches()
	// V should exit with an exit code of 1, when there are errors,
	// even when -silent is passed in combination to -check-syntax:
	if b.pref.only_check_syntax {
		for pf in b.parsed_files {
			if pf.errors.len > 0 {
				exit(1)
			}
		}
		if b.checker.nr_errors > 0 {
			exit(1)
		}
	}
}

fn (mut b Builder) run_compiled_executable_and_exit() {
	if b.pref.backend == .interpret {
		// the interpreted code has already ran
		return
	}
	if b.pref.skip_running {
		return
	}
	if b.pref.only_check_syntax || b.pref.check_only {
		return
	}
	if b.pref.should_output_to_stdout() {
		return
	}
	if b.pref.os == .ios {
		panic('Running iOS apps is not supported yet.')
	}
	if !(b.pref.is_test || b.pref.is_run || b.pref.is_crun) {
		exit(0)
	}
	compiled_file := os.real_path(b.pref.out_name)
	run_file := if b.pref.backend.is_js() {
		node_basename := $if windows { 'node.exe' } $else { 'node' }
		os.find_abs_path_of_executable(node_basename) or {
			panic('Could not find `$node_basename` in system path. Do you have Node.js installed?')
		}
	} else if b.pref.backend == .golang {
		go_basename := $if windows { 'go.exe' } $else { 'go' }
		os.find_abs_path_of_executable(go_basename) or {
			panic('Could not find `$go_basename` in system path. Do you have Go installed?')
		}
	} else {
		compiled_file
	}
	mut run_args := []string{cap: b.pref.run_args.len + 1}
	if b.pref.backend.is_js() {
		run_args << compiled_file
	} else if b.pref.backend == .golang {
		run_args << ['run', compiled_file]
	}
	run_args << b.pref.run_args
	mut run_process := os.new_process(run_file)
	run_process.set_args(run_args)
	if b.pref.is_verbose {
		println('running $run_process.filename with arguments $run_process.args')
	}
	// Ignore sigint and sigquit while running the compiled file,
	// so ^C doesn't prevent v from deleting the compiled file.
	// See also https://git.musl-libc.org/cgit/musl/tree/src/process/system.c
	prev_int_handler := os.signal_opt(.int, eshcb) or { serror('set .int', err) }
	mut prev_quit_handler := os.SignalHandler(eshcb)
	$if !windows { // There's no sigquit on windows
		prev_quit_handler = os.signal_opt(.quit, eshcb) or { serror('set .quit', err) }
	}
	run_process.wait()
	os.signal_opt(.int, prev_int_handler) or { serror('restore .int', err) }
	$if !windows {
		os.signal_opt(.quit, prev_quit_handler) or { serror('restore .quit', err) }
	}
	ret := run_process.code
	run_process.close()
	b.cleanup_run_executable_after_exit(compiled_file)
	exit(ret)
}

fn eshcb(_ os.Signal) {
}

[noreturn]
fn serror(reason string, e IError) {
	eprintln('could not $reason handler')
	panic(e)
}

fn (mut v Builder) cleanup_run_executable_after_exit(exefile string) {
	if v.pref.is_crun {
		return
	}
	if v.pref.reuse_tmpc {
		v.pref.vrun_elog('keeping executable: $exefile , because -keepc was passed')
		return
	}
	if !v.executable_exists {
		v.pref.vrun_elog('remove run executable: $exefile')
		os.rm(exefile) or {}
	}
}

// 'strings' => 'VROOT/vlib/strings'
// 'installed_mod' => '~/.vmodules/installed_mod'
// 'local_mod' => '/path/to/current/dir/local_mod'
pub fn (mut v Builder) set_module_lookup_paths() {
	// Module search order:
	// 0) V test files are very commonly located right inside the folder of the
	// module, which they test. Adding the parent folder of the module folder
	// with the _test.v files, *guarantees* that the tested module can be found
	// without needing to set custom options/flags.
	// 1) search in the *same* directory, as the compiled final v program source
	// (i.e. the . in `v .` or file.v in `v file.v`)
	// 2) search in the modules/ in the same directory.
	// 3) search in the provided paths
	// By default, these are what (3) contains:
	// 3.1) search in vlib/
	// 3.2) search in ~/.vmodules/ (i.e. modules installed with vpm)
	v.module_search_paths = []
	if v.pref.is_test {
		v.module_search_paths << os.dir(v.compiled_dir) // pdir of _test.v
	}
	v.module_search_paths << v.compiled_dir
	x := os.join_path(v.compiled_dir, 'modules')
	if v.pref.is_verbose {
		println('x: "$x"')
	}
	v.module_search_paths << os.join_path(v.compiled_dir, 'modules')
	v.module_search_paths << v.pref.lookup_path
	if v.pref.is_verbose {
		v.log('v.module_search_paths:')
		println(v.module_search_paths)
	}
}

pub fn (v Builder) get_builtin_files() []string {
	if v.pref.no_builtin {
		v.log('v.pref.no_builtin is true, get_builtin_files == []')
		return []
	}
	v.log('v.pref.lookup_path: $v.pref.lookup_path')
	// Lookup for built-in folder in lookup path.
	// Assumption: `builtin/` folder implies usable implementation of builtin
	for location in v.pref.lookup_path {
		if os.exists(os.join_path(location, 'builtin')) {
			mut builtin_files := []string{}
			if v.pref.backend.is_js() {
				builtin_files << v.v_files_from_dir(os.join_path(location, 'builtin',
					'js'))
			} else {
				builtin_files << v.v_files_from_dir(os.join_path(location, 'builtin'))
			}
			if v.pref.is_bare {
				builtin_files << v.v_files_from_dir(v.pref.bare_builtin_dir)
			}
			if v.pref.backend == .c {
				// TODO JavaScript backend doesn't handle os for now
				if v.pref.is_vsh && os.exists(os.join_path(location, 'os')) {
					builtin_files << v.v_files_from_dir(os.join_path(location, 'os'))
				}
			}
			return builtin_files
		}
	}
	// Panic. We couldn't find the folder.
	verror('`builtin/` not included on module lookup path.\nDid you forget to add vlib to the path? (Use @vlib for default vlib)')
}

pub fn (v &Builder) get_user_files() []string {
	if v.pref.path in ['vlib/builtin', 'vlib/strconv', 'vlib/strings', 'vlib/hash']
		|| v.pref.path.ends_with('vlib/builtin') {
		// This means we are building a builtin module with `v build-module vlib/strings` etc
		// get_builtin_files() has already added the files in this module,
		// do nothing here to avoid duplicate definition errors.
		v.log('Skipping user files.')
		return []
	}
	mut dir := v.pref.path
	v.log('get_v_files($dir)')
	// Need to store user files separately, because they have to be added after
	// libs, but we dont know	which libs need to be added yet
	mut user_files := []string{}
	// See cmd/tools/preludes/README.md for more info about what preludes are
	vroot := os.dir(pref.vexe_path())
	mut preludes_path := os.join_path(vroot, 'vlib', 'v', 'preludes')
	if v.pref.backend == .js_node {
		preludes_path = os.join_path(vroot, 'vlib', 'v', 'preludes_js')
	}
	if v.pref.trace_calls {
		user_files << os.join_path(preludes_path, 'trace_calls.v')
	}
	if v.pref.is_livemain || v.pref.is_liveshared {
		user_files << os.join_path(preludes_path, 'live.v')
	}
	if v.pref.is_livemain {
		user_files << os.join_path(preludes_path, 'live_main.v')
	}
	if v.pref.is_liveshared {
		user_files << os.join_path(preludes_path, 'live_shared.v')
	}
	if v.pref.is_test {
		user_files << os.join_path(preludes_path, 'test_runner.v')
		//
		mut v_test_runner_prelude := os.getenv('VTEST_RUNNER')
		if v.pref.test_runner != '' {
			v_test_runner_prelude = v.pref.test_runner
		}
		if v_test_runner_prelude == '' {
			v_test_runner_prelude = 'normal'
		}
		if !v_test_runner_prelude.contains('/') && !v_test_runner_prelude.contains('\\')
			&& !v_test_runner_prelude.ends_with('.v') {
			v_test_runner_prelude = os.join_path(preludes_path, 'test_runner_${v_test_runner_prelude}.v')
		}
		if !os.is_file(v_test_runner_prelude) || !os.is_readable(v_test_runner_prelude) {
			eprintln('test runner error: File $v_test_runner_prelude should be readable.')
			verror('supported test runners are: tap, json, simple, normal')
		}
		user_files << v_test_runner_prelude
	}
	if v.pref.is_test && v.pref.is_stats {
		user_files << os.join_path(preludes_path, 'tests_with_stats.v')
	}
	if v.pref.backend.is_js() && v.pref.is_stats && v.pref.is_test {
		user_files << os.join_path(preludes_path, 'stats_import.js.v')
	}
	if v.pref.is_prof {
		user_files << os.join_path(preludes_path, 'profiled_program.v')
	}
	is_test := v.pref.is_test
	mut is_internal_module_test := false
	if is_test {
		tcontent := util.read_file(dir) or { verror('$dir does not exist') }
		slines := tcontent.split_into_lines()
		for sline in slines {
			line := sline.trim_space()
			if line.len > 2 {
				if line[0] == `/` && line[1] == `/` {
					continue
				}
				if line.starts_with('module ') {
					is_internal_module_test = true
					break
				}
			}
		}
	}
	if is_internal_module_test {
		// v volt/slack_test.v: compile all .v files to get the environment
		single_test_v_file := os.real_path(dir)
		if v.pref.is_verbose {
			v.log('> Compiling an internal module _test.v file $single_test_v_file .')
			v.log('> That brings in all other ordinary .v files in the same module too .')
		}
		user_files << single_test_v_file
		dir = os.dir(single_test_v_file)
	}
	does_exist := os.exists(dir)
	if !does_exist {
		verror("$dir doesn't exist")
	}
	is_real_file := does_exist && !os.is_dir(dir)
	resolved_link := if is_real_file && os.is_link(dir) { os.real_path(dir) } else { dir }
	if is_real_file && (dir.ends_with('.v') || resolved_link.ends_with('.vsh')
		|| v.pref.raw_vsh_tmp_prefix != '' || dir.ends_with('.vv')) {
		single_v_file := if resolved_link.ends_with('.vsh') { resolved_link } else { dir }
		// Just compile one file and get parent dir
		user_files << single_v_file
		if v.pref.is_verbose {
			v.log('> just compile one file: "$single_v_file"')
		}
	} else if os.is_dir(dir) {
		if v.pref.is_verbose {
			v.log('> add all .v files from directory "$dir" ...')
		}
		// Add .v files from the directory being compiled
		user_files << v.v_files_from_dir(dir)
	} else {
		println('usage: `v file.v` or `v directory`')
		ext := os.file_ext(dir)
		println('unknown file extension `$ext`')
		exit(1)
	}
	if user_files.len == 0 {
		println('No input .v files')
		exit(1)
	}
	if v.pref.is_verbose {
		v.log('user_files: $user_files')
	}
	return user_files
}
