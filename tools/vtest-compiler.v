module main

import (
	os
	testing
	benchmark
)  

pub const (
	v_modules_path = os.home_dir() + '.vmodules'
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	v_test_compiler(args_string.all_before('test-compiler'))
}

fn v_test_compiler(vargs string){
	vexe := testing.vexe_path()
	parent_dir := os.dir(vexe)
	testing.vlib_should_be_present( parent_dir )
  
	// Changing the current directory is needed for some of the compiler tests,
	// compiler/tests/local_test.v and compiler/tests/repl/repl_test.v
	os.chdir( parent_dir )
  
	/*
	if !os.file_exists(parent_dir + '/v.v') {
		println('v.v is missing, it must be next to the V executable')
		exit(1)
	}
	*/
	
	// Make sure v.c can be compiled without warnings
	$if mac {
		if os.file_exists('/v.v') {
			os.system('$vexe -o v.c v.v')
			if os.system('cc -Werror v.c') != 0 {
				println('cc failed to build v.c without warnings')
				exit(1)
			}
			println('v.c can be compiled without warnings. This is good :)')
		}
	}
	
	building_tools_failed := testing.v_build_failing(vargs, 'tools')
	
	println('\nTesting all _test.v files...')
	mut compiler_test_session := testing.new_test_sesion( vargs )
	compiler_test_session.files << os.walk_ext(parent_dir, '_test.v')
	compiler_test_session.test()
	println( compiler_test_session.benchmark.total_message('running V tests') )

	println('')
	building_examples_failed := testing.v_build_failing(vargs, 'examples')
	
	v_module_install_cmd := '$vexe install nedpals.args'
	println('\nInstalling a v module with: $v_module_install_cmd ')
	mut vmark := benchmark.new_benchmark()
	ret := os.system(v_module_install_cmd)
	if ret != 0 {
		println('failed to run v install')
		exit(1)
	}
	if !os.file_exists(v_modules_path + '/nedpals/args') {
		println('v failed to install a test module')
		exit(1)
	}
	vmark.stop()
	println( 'Installing a v module took: ' + vmark.total_duration().str() + 'ms')
	
	if building_tools_failed || compiler_test_session.failed || building_examples_failed {
		exit(1)
	}
	
}
