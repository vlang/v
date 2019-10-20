// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	compiler
	benchmark
	//time
)

fn main() {
	//t := time.ticks()
	//defer { println(time.ticks() - t) }
	// There's no `flags` module yet, so args have to be parsed manually
	args := compiler.env_vflags_and_os_args()
	options := args.filter(it.starts_with('-'))
	commands := args.filter(!it.starts_with('-'))
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
	else if 'up' in commands {
		compiler.update_v()
		return
	}
	else if 'get' in commands {
		println('use `v install` to install modules from vpm.vlang.io ')
		return
	}
	else if 'symlink' in commands {
		compiler.create_symlink()
		return
	}
	else if 'install' in commands {
		compiler.install_v(args)
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
	else if 'test' in commands {
		compiler.test_v()
		return
	}
	// Generate the docs and exit
	else if 'doc' in commands {
		// v.gen_doc_html_for_module(args.last())
		exit(0)
	} else {
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

	// No args? REPL
	if args.len < 2 || (args.len == 2 && args[1] == '-') || 'runrepl' in args {
		compiler.run_repl()
		return
	}

	mut tmark := benchmark.new_benchmark()
	v.compile()	
	if v.pref.is_stats {
		tmark.stop()
		println( 'compilation took: ' + tmark.total_duration().str() + 'ms')
	}

	if v.pref.is_test {
		v.run_compiled_executable_and_exit()
	}

	v.finalize_compilation()
}

