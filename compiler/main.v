// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import vcompiler

import (
	benchmark
)

fn main() {
	// There's no `flags` module yet, so args have to be parsed manually
	args := vcompiler.env_vflags_and_os_args()
	// Print the version and exit.
	if '-v' in args || '--version' in args || 'version' in args {
		version_hash := vcompiler.vhash()
		println('V $vcompiler.Version $version_hash')
		return
	}
	if '-h' in args || '--help' in args || 'help' in args {
		println(vcompiler.HelpText)
		return
	}
	if 'translate' in args {
		println('Translating C to V will be available in V 0.3')
		return
	}
	if 'up' in args {
		vcompiler.update_v()
		return
	}
	if 'get' in args {
		println('use `v install` to install modules from vpm.vlang.io ')
		return
	}
	if 'symlink' in args {
		vcompiler.create_symlink()
		return
	}
	if 'install' in args {
		vcompiler.install_v(args)
		return
	}
	// TODO quit if the vcompiler is too old
	// u := os.file_last_mod_unix('v')
	// If there's no tmp path with current version yet, the user must be using a pre-built package
	//
	// Just fmt and exit
	if 'fmt' in args {
		vcompiler.vfmt(args)
		return
	}
	if 'test' in args {
		vcompiler.test_v()
		return
	}
	// Construct the V object from command line arguments
	mut v := vcompiler.new_v(args)
	v.pref.building_v = true
	if v.pref.is_verbose {
		println(args)
	}
	// Generate the docs and exit
	if 'doc' in args {
		// v.gen_doc_html_for_module(args.last())
		exit(0)
	}

	if 'run' in args {
		// always recompile for now, too error prone to skip recompilation otherwise
		// for example for -repl usage, especially when piping lines to v
		v.compile()
		v.run_compiled_executable_and_exit()
	}

	// No args? REPL
	if args.len < 2 || (args.len == 2 && args[1] == '-') || 'runrepl' in args {
		vcompiler.run_repl()
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
