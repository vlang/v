module main

import (
	os
	testing
	benchmark
	filepath
	v.pref
)

pub const (
	v_modules_path = os.home_dir() + '.vmodules'
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	v_test_compiler(args_string.all_before('test-compiler'))
}

fn v_test_compiler(vargs string) {
	vexe := pref.vexe_path()
	parent_dir := filepath.dir(vexe)
	testing.vlib_should_be_present(parent_dir)
	// Changing the current directory is needed for some of the compiler tests,
	// compiler/tests/local_test.v and compiler/tests/repl/repl_test.v
	os.chdir(parent_dir)
	/*
	if !os.exists(parent_dir + '/v.v') {
		eprintln('v.v is missing, it must be next to the V executable')
		exit(1)
	}
	*/

	// Make sure v.c can be compiled without warnings
	$if macos {
		if os.exists('/cmd/v') {
			os.system('$vexe -o v.c cmd/v')
			if os.system('cc -Werror v.c') != 0 {
				eprintln('cc failed to build v.c without warnings')
				exit(1)
			}
			eprintln('v.c can be compiled without warnings. This is good :)')
		}
	}
	building_tools_failed := testing.v_build_failing(vargs, 'cmd/tools')
	eprintln('')
	testing.eheader('Testing all _test.v files...')
	mut compiler_test_session := testing.new_test_session(vargs)
	compiler_test_session.files << os.walk_ext(parent_dir, '_test.v')
	compiler_test_session.test()
	eprintln(compiler_test_session.benchmark.total_message('running V tests'))
	eprintln('')
	building_examples_failed := testing.v_build_failing(vargs, 'examples')
	eprintln('')
	building_live_failed := testing.v_build_failing(vargs + '-live', filepath.join('examples','hot_reload'))
	eprintln('')
	v_module_install_cmd := '$vexe install nedpals.args'
	eprintln('')
	testing.eheader('Installing a v module with: $v_module_install_cmd')
	mut vmark := benchmark.new_benchmark()
	ret := os.system(v_module_install_cmd)
	if ret != 0 {
		eprintln('failed to run v install')
	}
	if !os.exists(v_modules_path + '/nedpals/args') {
		eprintln('v failed to install a test module')
	}
	vmark.stop()
	eprintln('Installing a v module took: ' + vmark.total_duration().str() + 'ms')
	if building_tools_failed || compiler_test_session.failed || building_examples_failed || building_live_failed {
		exit(1)
	}
}
