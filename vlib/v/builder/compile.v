// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import time
import os
import rand
import v.pref
import v.util

fn get_vtmp_folder() string {
	mut vtmp := os.getenv('VTMP')
	if vtmp.len > 0 {
		return vtmp
	}
	vtmp = os.join_path(os.temp_dir(), 'v')
	if !os.exists(vtmp) || !os.is_dir(vtmp) {
		os.mkdir_all(vtmp)
	}
	os.setenv('VTMP', vtmp, true)
	return vtmp
}

fn (mut b Builder) get_vtmp_filename(base_file_name string, postfix string) string {
	vtmp := get_vtmp_folder()
	mut uniq := ''
	if !b.pref.reuse_tmpc {
		uniq = '.$rand.u64()'
	}
	return os.real_path(os.join_path(vtmp, os.file_name(os.real_path(base_file_name)) + '$uniq$postfix'))
}

pub fn compile(command string, pref &pref.Preferences) {
	odir := os.dir(pref.out_name)
	// When pref.out_name is just the name of an executable, i.e. `./v -o executable main.v`
	// without a folder component, just use the current folder instead:
	mut output_folder := odir
	if odir.len == pref.out_name.len {
		output_folder = os.getwd()
	}
	os.is_writable_folder(output_folder) or {
		// An early error here, is better than an unclear C error later:
		verror(err)
		exit(1)
	}
	// Construct the V object from command line arguments
	mut b := new_builder(pref)
	if pref.is_verbose {
		println('builder.compile() pref:')
		// println(pref)
	}
	mut sw := time.new_stopwatch({})
	match pref.backend {
		.c { b.compile_c() }
		.js { b.compile_js() }
		.x64 { b.compile_x64() }
	}
	if pref.is_stats {
		println('compilation took: ${util.bold(sw.elapsed().milliseconds().str())} ms')
	}
	b.exit_on_invalid_syntax()
	// running does not require the parsers anymore
	unsafe {b.myfree()}
	if pref.is_test || pref.is_run {
		b.run_compiled_executable_and_exit()
	}
}

// Temporary, will be done by -autofree
[unsafe]
fn (mut b Builder) myfree() {
	// for file in b.parsed_files {
	// }
	unsafe {b.parsed_files.free()}
}

fn (b &Builder) exit_on_invalid_syntax() {
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
	if b.pref.skip_running {
		return
	}
	if b.pref.only_check_syntax {
		return
	}
	if b.pref.os == .ios && b.pref.is_ios_simulator == false {
		panic('Running iOS apps on physical devices is not yet supported. Please run in the simulator using the -simulator flag.')
	}
	if b.pref.is_verbose {
		println('============ running $b.pref.out_name ============')
	}
	if b.pref.os == .ios {
		device := '"iPhone SE (2nd generation)"'
		os.exec('xcrun simctl boot $device')
		bundle_name := b.pref.out_name.split('/').last()
		display_name := if b.pref.display_name != '' { b.pref.display_name } else { bundle_name }
		os.exec('xcrun simctl install $device ${display_name}.app')
		bundle_id := if b.pref.bundle_id != '' { b.pref.bundle_id } else { 'app.vlang.$bundle_name' }
		os.exec('xcrun simctl launch $device $bundle_id')
	} else {
		exefile := os.real_path(b.pref.out_name)
		mut cmd := '"$exefile"'
		if b.pref.backend == .js {
			jsfile := os.real_path('${b.pref.out_name}.js')
			cmd = 'node "$jsfile"'
		}
		for arg in b.pref.run_args {
			// Determine if there are spaces in the parameters
			if arg.index_byte(` `) > 0 {
				cmd += ' "' + arg + '"'
			} else {
				cmd += ' ' + arg
			}
		}
		if b.pref.is_verbose {
			println('command to run executable: $cmd')
		}
		if b.pref.is_test {
			exit(os.system(cmd))
		}
		if b.pref.is_run {
			ret := os.system(cmd)
			// TODO: make the runner wrapping as transparent as possible
			// (i.e. use execve when implemented). For now though, the runner
			// just returns the same exit code as the child process.
			exit(ret)
		}
	}
	exit(0)
}

// 'strings' => 'VROOT/vlib/strings'
// 'installed_mod' => '~/.vmodules/installed_mod'
// 'local_mod' => '/path/to/current/dir/local_mod'
fn (mut v Builder) set_module_lookup_paths() {
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
	/*
	// if v.pref.build_mode == .build_module && v.pref.path == 'vlib/builtin' { // .contains('builtin/' +  location {
	if v.pref.build_mode == .build_module && v.pref.path == 'vlib/strconv' { // .contains('builtin/' +  location {
		// We are already building builtin.o, no need to import them again
		if v.pref.is_verbose {
			println('skipping builtin modules for builtin.o')
		}
		return []
	}
	*/
	v.log('v.pref.lookup_path: $v.pref.lookup_path')
	// Lookup for built-in folder in lookup path.
	// Assumption: `builtin/` folder implies usable implementation of builtin
	for location in v.pref.lookup_path {
		if os.exists(os.join_path(location, 'builtin')) {
			mut builtin_files := []string{}
			if v.pref.is_bare {
				builtin_files << v.v_files_from_dir(os.join_path(location, 'builtin', 'bare'))
			} else if v.pref.backend == .js {
				builtin_files << v.v_files_from_dir(os.join_path(location, 'builtin', 'js'))
			} else {
				builtin_files << v.v_files_from_dir(os.join_path(location, 'builtin'))
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
	verror('`builtin/` not included on module lookup path.
Did you forget to add vlib to the path? (Use @vlib for default vlib)')
	panic('Unreachable code reached.')
}

pub fn (v &Builder) get_user_files() []string {
	if v.pref.path in ['vlib/builtin', 'vlib/strconv', 'vlib/strings', 'vlib/hash'] {
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
	preludes_path := os.join_path(vroot, 'cmd', 'tools', 'preludes')
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
		user_files << os.join_path(preludes_path, 'tests_assertions.v')
	}
	if v.pref.is_test && v.pref.is_stats {
		user_files << os.join_path(preludes_path, 'tests_with_stats.v')
	}
	if v.pref.is_prof {
		user_files << os.join_path(preludes_path, 'profiled_program.v')
	}
	is_test := dir.ends_with('_test.v')
	mut is_internal_module_test := false
	if is_test {
		tcontent := os.read_file(dir) or {
			verror('$dir does not exist')
			exit(0)
		}
		slines := tcontent.trim_space().split_into_lines()
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
		exit(1)
	}
	is_real_file := does_exist && !os.is_dir(dir)
	if is_real_file && (dir.ends_with('.v') || dir.ends_with('.vsh') || dir.ends_with('.vv')) {
		single_v_file := dir
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
