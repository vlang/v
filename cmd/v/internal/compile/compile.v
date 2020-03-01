// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compile

import (
	benchmark
	compiler
	os
	os.cmdline
	compiler
)

pub fn compile(command string, args []string) {
	// Construct the V object from command line arguments
	parse_and_output_new_format(args)
	prefs := parse_arguments(args)
	check_for_common_mistake(args, &prefs)
	mut v := compiler.new_v(prefs)
	if v.pref.verbosity.is_higher_or_equal(.level_two) {
		println(args)
	}
	mut tmark := benchmark.new_benchmark()
	if v.pref.backend == .x64 {
		v.compile_x64()
	}
	else if v.pref.backend == .experimental {
		v.compile2()
	}
	else {
		v.compile()
	}
	if v.pref.is_stats {
		tmark.stop()
		println('compilation took: ' + tmark.total_duration().str() + 'ms')
	}
	if v.pref.is_test || v.pref.is_run {
		run_compiled_executable_and_exit(v, args)
	}
	v.finalize_compilation()
}

pub fn run_compiled_executable_and_exit(v &compiler.V, args []string) {
	if v.pref.verbosity.is_higher_or_equal(.level_two) {
		println('============ running $v.pref.out_name ============')
	}
	mut cmd := '"${v.pref.out_name}"'
	args_after_no_options := cmdline.only_non_options( cmdline.options_after(args,['run','test']) )
	if args_after_no_options.len > 1 {
		cmd += ' ' + args_after_no_options[1..].join(' ')
	}
	if v.pref.is_test {
		ret := os.system(cmd)
		if ret != 0 {
			exit(1)
		}
	}
	if v.pref.is_run {
		ret := os.system(cmd)
		// TODO: make the runner wrapping as transparent as possible
		// (i.e. use execve when implemented). For now though, the runner
		// just returns the same exit code as the child process.
		exit(ret)
	}
	exit(0)
}
