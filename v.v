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

const (
	known_commands = ['run', 'build', 'version', 'doc']
	simple_tools = ['up', 'create', 'test', 'test-compiler', 'build-tools',
		 'build-examples', 'build-vbinaries']
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
	command := if os.args.len > 1 { os.args[1] } else { '' }
	// external tool
	if command in simple_tools {
		compiler.launch_tool('v' + command)
		return
	}
	// v run, v doc, etc
	if !command.starts_with('-') && !os.exists(command) {
		v_command(command, args)
	}
	// Print the version and exit.
	if '-v' in options || '--version' in options {
		version_hash := compiler.vhash()
		println('V $compiler.Version $version_hash')
		return
	}
	else if '-h' in options || '--help' in options {
		println(compiler.help_text)
		return
	}
	// No args? REPL
	else if command == '' || (args.len == 2 && args[1] == '-') {
		compiler.launch_tool('vrepl')
		return
	}
	// Construct the V object from command line arguments
	mut v := compiler.new_v(args)
	if v.pref.is_verbose {
		println(args)
	}
	if command == 'run' {
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
		println('compilation took: ' + tmark.total_duration().str() + 'ms')
	}
	if v.pref.is_test {
		v.run_compiled_executable_and_exit()
	}
	v.finalize_compilation()
}

fn v_command(command string, args []string) {
	match command {
		'', '.', 'run' {

		}
		'version' {
			println('V $compiler.Version $compiler.vhash()')
			return
		}
		'help' {
			println(compiler.help_text)
			return
		}
		'translate' {
			println('Translating C to V will be available in V 0.3 (January)')
			return
		}
		'search', 'install', 'update' {
			compiler.launch_tool('vpm')
		}
		'get' {
			println('use `v install` to install modules from vpm.vlang.io ')
			return
		}
		'symlink' {
			compiler.create_symlink()
			return
		}
		'fmt' {
			compiler.vfmt(args)
			return
		}
		'runrepl' {
			compiler.launch_tool('vrepl')
			return
		}
		'doc' {
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
			println('v $command: unknown command')
			println('Run "v help" for usage.')
			return
		}
	}
}

