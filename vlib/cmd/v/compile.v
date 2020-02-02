// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	benchmark
	compiler
)

fn compile(command string, args []string) {
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
	else if v.pref.v2 {
		v.compile2()
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
