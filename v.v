// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	compiler
	benchmark
	os
	filepath
	// time
)

const (
	known_commands = ['run', 'build', 'version', 'doc']
	simple_tools = ['fmt', 'up', 'create', 'test', 'test-compiler', 'build-tools', 'build-examples', 'build-vbinaries']
)

fn main() {
  is_verbose := '-verbose' in os.args || '--verbose' in os.args
	// t := time.ticks()
	// defer { println(time.ticks() - t) }
	args := compiler.env_vflags_and_os_args()
	options, command := compiler.get_v_options_and_main_command( args )
  if is_verbose {
	  eprintln('v    args: $args')
	  eprintln('v command: $command')
	  eprintln('v options: $options')
  }    
	
	// external tool
	if command in simple_tools {
		compiler.launch_tool('v' + command)
		return
	}
	// v run, v doc, etc
	if !command.starts_with('-') && !command.ends_with('.v') && !os.exists(command) {
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
	}
	else {
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
		'', '.', 'run', 'build' {
			// handled later in vlib/compiler/main.v
			return
		}
		'version' {
			println('V $compiler.Version $compiler.vhash()')
		}
		'help' {
			println(compiler.help_text)
		}
		'translate' {
			println('Translating C to V will be available in V 0.3 (January)')
		}
		'search', 'install', 'update', 'remove' {
			compiler.launch_tool('vpm')
		}
		'get' {
			println('use `v install` to install modules from vpm.vlang.io ')
		}
		'symlink' {
			compiler.create_symlink()
		}
		'runrepl' {
			compiler.launch_tool('vrepl')
		}
		'doc' {
			vexe := os.executable()
			vdir := filepath.dir(os.executable())
			os.chdir(vdir)
			mod := args.last()
			os.system('$vexe build module vlib$os.path_separator' + args.last())
			vhfile := filepath.join(compiler.v_modules_path,'vlib','${mod}.vh')
			txt := os.read_file(vhfile) or { panic(err) }
			println(txt)
			// v.gen_doc_html_for_module(args.last())
		}
		else {
			println('v $command: unknown command')
			println('Run "v help" for usage.')
		}
	}
	exit(0)
}
