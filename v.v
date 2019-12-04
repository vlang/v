// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	compiler
	benchmark
	os
	filepath
	//time
)

fn main() {
	//t := time.ticks()
	//defer { println(time.ticks() - t) }
	// There's no `flags` module yet, so args have to be parsed manually
	args := compiler.env_vflags_and_os_args()
	options := args.filter(it.starts_with('-'))
	//NB: commands should be explicitly set by the command line (os.args)
	//    NOT passed through VFLAGS, otherwise the naked `v` invocation for
	//    the repl does not work when you have VFLAGS with -cc or -cflags set
	//    which may be surprising to v users.
	stuff_after_executable := os.args[1..]
	commands := stuff_after_executable.filter(!it.starts_with('-'))
	
	simple_tools := ['up', 'create', 'test', 'test-compiler', 'build-tools', 'build-examples', 'build-vbinaries']
	for tool in simple_tools {
		if tool in commands {
			compiler.launch_tool('v$tool')
			return
		}
	}
  
	// Print the version and exit.
	if '-v' in options || '--version' in options || 'version' in commands {
		version_hash := compiler.vhash()
		println('V $compiler.Version $version_hash')
		return
	}
	else if '-h' in options || '--help' in options || 'help' in commands {
		println(compiler.help_text)
		return
	}
	else if 'translate' in commands {
		println('Translating C to V will be available in V 0.3')
		return
	}
	else if 'search' in commands || 'install' in commands || 'update' in commands || 'remove' in commands {
		compiler.launch_tool('vpm')
		return
	}
	else if ('get' in commands) { // obsoleted
		println('use `v install` to install modules from vpm.vlang.io ')
		return
	}
	else if 'symlink' in commands {
		compiler.create_symlink()
		return
	}
	// TODO quit if the v compiler is too old
	// u := os.file_last_mod_unix('v')
	// If there's no tmp path with current version yet, the user must be using a pre-built package
	//
	// Just fmt and exit
	else if 'fmt' in commands {
		compiler.vfmt(args)
		return
	}
	// No args? REPL
	else if 'runrepl' in commands || commands.len == 0 || (args.len == 2 && args[1] == '-') {
		compiler.launch_tool('vrepl')
		return
	}
	// Generate the docs and exit
	else if 'doc' in commands {
		vexe := os.executable()
		vdir := os.dir(os.executable())
		os.chdir(vdir)
		mod := args.last()
		os.system('$vexe build module vlib$os.path_separator' + args.last())
		txt := os.read_file(filepath.join(compiler.v_modules_path, 'vlib', '${mod}.vh')) or {
			panic(err)
		}
		println(txt)
		exit(0)
		// v.gen_doc_html_for_module(args.last())
	}
	else {
		//println('unknown command/argument\n')
		//println(compiler.help_text)
	}
	// Construct the V object from command line arguments
	mut v := compiler.new_v(args)
	if v.pref.is_verbose {
		println(args)
	}
	if 'run' in args {
		// always recompile for now, too error prone to skip recompilation otherwise
		// for example for -repl usage, especially when piping lines to v
		v.compile()
		v.run_compiled_executable_and_exit()
	}
	mut tmark := benchmark.new_benchmark()
	if v.pref.x64 {
		v.compile_x64()
	}	else {
		v.compile()
	}
	if v.pref.is_stats {
		tmark.stop()
		println( 'compilation took: ' + tmark.total_duration().str() + 'ms')
	}
	if v.pref.is_test {
		v.run_compiled_executable_and_exit()
	}
	v.finalize_compilation()
}

