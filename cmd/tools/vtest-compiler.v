module main

import os
import testing
import benchmark
import v.pref

fn main() {
	args_string := os.args[1..].join(' ')
	v_test_compiler(args_string.all_before('test-compiler'))
}

fn v_test_compiler(vargs string) {
	vexe := pref.vexe_path()
	parent_dir := os.dir(vexe)
	testing.vlib_should_be_present(parent_dir)
	// Changing the current directory is needed for some of the compiler tests,
	// vlib/v/tests/local_test.v and vlib/v/tests/repl/repl_test.v
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
	building_live_failed := testing.v_build_failing(vargs + '-live', os.join_path('examples',
		'hot_reload'))
	eprintln('')
	//
	testing.setup_new_vtmp_folder()
	v_module_install_cmd := '$vexe install nedpals.args'
	eprintln('')
	testing.eheader('Installing a v module with: $v_module_install_cmd')
	mut vmark := benchmark.new_benchmark()
	ret := os.system(v_module_install_cmd)
	if ret != 0 {
		eprintln('failed to run v install')
	}
	desired_path := os.join_path(pref.default_module_path, 'nedpals', 'args')
	if !(os.exists(desired_path) && os.is_dir(desired_path)) {
		eprintln('v failed to install a test module')
	}
	vmark.stop()
	eprintln('Installing a v module took: ' + vmark.total_duration().str() + 'ms')
	//
	if building_tools_failed || compiler_test_session.failed || building_examples_failed || building_live_failed {
		exit(1)
	}
}
