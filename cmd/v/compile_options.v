// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	compiler
	filepath
	os
	os.cmdline
	v.pref
)

//TODO Cleanup this file. This file ended up like a dump for functions that do not belong in `compiler`.
//Maybe restructure the functions below into different V files

pub fn new_v(args []string) &compiler.V {
	// Create modules dirs if they are missing
	if !os.is_dir(compiler.v_modules_path) {
		os.mkdir(compiler.v_modules_path)or{
			panic(err)
		}
		os.mkdir('$compiler.v_modules_path${filepath.separator}cache')or{
			panic(err)
		}
	}
	vroot := filepath.dir(pref.vexe_path())
	// optional, custom modules search path
	user_mod_path := cmdline.option(args, '-user_mod_path', '')
	vlib_path := cmdline.option(args, '-vlib-path', '')
	vpath := cmdline.option(args, '-vpath', '')
	target_os := cmdline.option(args, '-os', '')
	if target_os == 'msvc' {
		// notice that `-os msvc` became `-cc msvc`
		println('V error: use the flag `-cc msvc` to build using msvc')
		os.flush()
		exit(1)
	}
	mut out_name := cmdline.option(args, '-o', '')
	mut dir := args[args.len-1]//.last()
	if 'run' in args {
		args_after_run := cmdline.only_non_options( cmdline.options_after(args,['run']) )
		dir = if args_after_run.len>0 { args_after_run[0] } else { '' }
	}
	if dir == 'v.v' {
		println('looks like you are trying to build V with an old command')
		println('use `v -o v cmd/v` instead of `v -o v v.v`')
		exit(1)
	}
	if dir.ends_with(filepath.separator) {
		dir = dir.all_before_last(filepath.separator)
	}
	if dir.starts_with('.$filepath.separator') {
		dir = dir[2..]
	}
	if args.len < 2 {
		dir = ''
	}

	// build mode
	mut build_mode := pref.BuildMode.default_mode
	mut mod := ''
	joined_args := args.join(' ')
	if joined_args.contains('build module ') {
		build_mode = .build_module
		os.chdir(vroot)
		// v build module ~/v/os => os.o
		mod_path := if dir.contains('vlib') { dir.all_after('vlib' + filepath.separator) } else if dir.starts_with('.\\') || dir.starts_with('./') { dir[2..] } else if dir.starts_with(filepath.separator) { dir.all_after(filepath.separator) } else { dir }
		mod = mod_path.replace(filepath.separator, '.')
		println('Building module "${mod}" (dir="$dir")...')
		// out_name = '$TmpPath/vlib/${base}.o'
		if !out_name.ends_with('.c') {
			out_name = mod
		}
		// Cross compiling? Use separate dirs for each os
		/*
		if target_os != os.user_os() {
			os.mkdir('$TmpPath/vlib/$target_os') or { panic(err) }
			out_name = '$TmpPath/vlib/$target_os/${base}.o'
			println('target_os=$target_os user_os=${os.user_os()}')
			println('!Cross compiling $out_name')
		}
		*/

	}
	// `v -o dir/exec`, create "dir/" if it doesn't exist
	if out_name.contains(filepath.separator) {
		d := out_name.all_before_last(filepath.separator)
		if !os.is_dir(d) {
			println('creating a new directory "$d"')
			os.mkdir(d)or{
				panic(err)
			}
		}
	}

	// println('VROOT=$vroot')
	cflags := cmdline.options(args, '-cflags').join(' ')
	defines := cmdline.options(args, '-d')
	compile_defines, compile_defines_all := parse_defines( defines )

	rdir := os.realpath(dir)
	rdir_name := filepath.filename(rdir)
	if '-bare' in args {
		println('V error: use -freestanding instead of -bare')
		os.flush()
		exit(1)
	}
	is_repl := '-repl' in args
	ccompiler := cmdline.option(args, '-cc', '')
	mut prefs := &pref.Preferences{
		os: pref.os_from_string(target_os)
		is_so: '-shared' in args
		is_solive: '-solive' in args
		is_prod: '-prod' in args
		is_verbose: '-verbose' in args || '--verbose' in args
		is_debug: '-g' in args || '-cg' in args
		is_vlines: '-g' in args && !('-cg' in args)
		is_keep_c: '-keep_c' in args
		is_pretty_c: '-pretty_c' in args
		is_cache: '-cache' in args
		is_stats: '-stats' in args
		obfuscate: '-obf' in args
		is_prof: '-prof' in args
		is_live: '-live' in args
		sanitize: '-sanitize' in args
		// nofmt: '-nofmt' in args

		show_c_cmd: '-show_c_cmd' in args
		translated: 'translated' in args
		is_run: 'run' in args
		autofree: '-autofree' in args
		compress: '-compress' in args
		enable_globals: '--enable-globals' in args
		fast: '-fast' in args
		is_bare: '-freestanding' in args
		x64: '-x64' in args
		output_cross_c: '-output-cross-platform-c' in args
		prealloc: '-prealloc' in args
		is_repl: is_repl
		build_mode: build_mode
		cflags: cflags
		ccompiler: ccompiler
		building_v: !is_repl && (rdir_name == 'compiler' || rdir_name == 'v' || rdir_name == 'vfmt.v' || rdir_name == 'cmd/v' || dir.contains('vlib'))
		// is_fmt: comptime_define == 'vfmt'

		user_mod_path: user_mod_path
		vlib_path: vlib_path
		vpath: vpath
		v2: '-v2' in args
		vroot: vroot
		out_name: out_name
		path: dir
		compile_defines: compile_defines
		compile_defines_all: compile_defines_all
		mod: mod
	}
	if prefs.is_verbose || prefs.is_debug {
		println('C compiler=$prefs.ccompiler')
	}
	$if !linux {
		if prefs.is_bare && !out_name.ends_with('.c') {
			println('V error: -freestanding only works on Linux for now')
			os.flush()
			exit(1)
		}
	}
	prefs.fill_with_defaults()

	// v.exe's parent directory should contain vlib
	if !os.is_dir(prefs.vlib_path) || !os.is_dir(prefs.vlib_path + filepath.separator + 'builtin') {
		// println('vlib not found, downloading it...')
		/*
		ret := os.system('git clone --depth=1 https://github.com/vlang/v .')
		if ret != 0 {
			println('failed to `git clone` vlib')
			println('make sure you are online and have git installed')
			exit(1)
		}
		*/
		println('vlib not found. It should be next to the V executable.')
		println('Go to https://vlang.io to install V.')
		println('(os.executable=${os.executable()} vlib_path=$prefs.vlib_path vexe_path=${pref.vexe_path()}')
		exit(1)
	}

	if prefs.is_script && !os.exists(dir) {
		println('`$dir` does not exist')
		exit(1)
	}

	return compiler.new_v(prefs)
}

fn find_c_compiler_thirdparty_options(args []string) string {
	mut cflags := cmdline.options(args, '-cflags')
	$if !windows {
		cflags << '-fPIC'
	}
	if '-m32' in args {
		cflags << '-m32'
	}
	return cflags.join(' ')
}

fn parse_defines(defines []string) ([]string,[]string) {
	// '-d abc -d xyz=1 -d qwe=0' should produce:
	// compile_defines:      ['abc','xyz']
	// compile_defines_all   ['abc','xyz','qwe']
	mut compile_defines := []string
	mut compile_defines_all := []string
	for dfn in defines {
		dfn_parts := dfn.split('=')
		if dfn_parts.len == 1 {
			compile_defines << dfn
			compile_defines_all << dfn
			continue
		}
		if dfn_parts.len == 2 {
			compile_defines_all << dfn_parts[0]
			if dfn_parts[1] == '1' {
				compile_defines << dfn_parts[0]
			}
		}
	}
	return compile_defines, compile_defines_all
}
