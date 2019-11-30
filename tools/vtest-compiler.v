module main

import (
  os
  testing
  compiler
)  

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	v_test_compiler(args_string.all_before('test-compiler'))
}

fn v_test_compiler(args_before_test string){
	vexe := testing.vexe_path()
	parent_dir := os.dir(vexe)
	// Changing the current directory is needed for some of the compiler tests,
	// compiler/tests/local_test.v and compiler/tests/repl/repl_test.v
	os.chdir( parent_dir )
	if !os.dir_exists(parent_dir + '/vlib') {
		println('vlib/ is missing, it must be next to the V executable')
		exit(1)
	}
  
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

	println('Building v tools...')
	mut tools_session := testing.new_test_sesion( args_before_test )
	tools_v_files := os.walk_ext(parent_dir+'/tools','.v')
	tools_main_files := tools_v_files.filter(!it.contains('modules'))
	tools_session.files << tools_main_files
	tools_session.test()
	println( tools_session.benchmark.total_message('building v tools') )

	println('\nTesting all _test.v files...')
	mut compiler_test_session := testing.new_test_sesion( args_before_test )
	compiler_test_session.files << os.walk_ext(parent_dir, '_test.v')
	compiler_test_session.test()
	println( compiler_test_session.benchmark.total_message('running V tests') )
	
	println('\nBuilding examples...')
	mut example_session := testing.new_test_sesion( args_before_test )
	example_files := os.walk_ext(parent_dir+'/examples','.v')
	example_mains := example_files.filter(!it.contains('modules') && !it.contains('automaton.v') && !it.contains('some_module.v'))
	example_session.files << example_mains
	example_session.test()
	println( example_session.benchmark.total_message('building examples') )

	if tools_session.failed || compiler_test_session.failed || example_session.failed {
    exit(1)
  }

	test_vget()  
}

pub fn test_vget() {
	vexe := testing.vexe_path()
	ret := os.system('$vexe install nedpals.args')
	if ret != 0 {
		println('failed to run v install')
		exit(1)
	}
	if !os.file_exists(compiler.v_modules_path + '/nedpals/args') {
		println('v failed to install a test module')
		exit(1)
	}
	println('vget is OK')
}
