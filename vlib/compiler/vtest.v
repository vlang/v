module compiler

import (
	os
	term
	benchmark
)

struct TestSession {
mut:
	files []string
	vexe string
	vargs string
	failed bool
	benchmark benchmark.Benchmark
}

pub fn new_test_sesion(vargs string) TestSession {
	return TestSession{
		vexe: vexe_path()
		vargs: vargs
	}
}

pub fn test_v() {
	args := os.args
	if args.last() == 'test' {
		println('Usage:')
		println('   A)')
		println('      v test v  : run all v tests and build all the examples')
		println('   B)')
		println('      v test folder/ : run all v tests in the given folder.')
		println('      v -stats test folder/ : the same, but print more stats.')
		println('   C)')
		println('      v test file_test.v : run test functions in a given test file.')
		println('      v -stats test file_test.v : as above, but with more stats.')
		println('   NB: you can also give many and mixed folder/ file_test.v arguments after test.')
		println('')
		return
	}

	args_string := args[1..].join(' ')
	args_before := args_string.all_before('test ')
	args_after  := args_string.all_after('test ')

	if args_after == 'v' {
		v_test_v(args_before)
		return
	}

	mut ts := new_test_sesion(args_before)
	for targ in args_after.split(' ') {
		if os.file_exists(targ) && targ.ends_with('_test.v') {
			ts.files << targ
			continue
		}
		if os.dir_exists(targ) {
			// Fetch all tests from the directory
			ts.files << os.walk_ext( targ.trim_right(os.path_separator), '_test.v')
			continue
		}
		println('Unrecognized test file $targ .')
	}

	println('Testing...')
	ts.test()
	println('----------------------------------------------------------------------------')
	println( ts.benchmark.total_message('running V _test.v files') )
	if ts.failed {
		exit(1)
	}
}

pub fn (ts mut TestSession) test() {
	ok   := term.ok_message('OK')
	fail := term.fail_message('FAIL')
	cmd_needs_quoting := (os.user_os() == 'windows')
	show_stats := '-stats' in ts.vargs.split(' ')
	ts.benchmark = benchmark.new_benchmark()
	for dot_relative_file in ts.files {
		relative_file := dot_relative_file.replace('./', '')
		file := os.realpath( relative_file )
		tmpc_filepath := file.replace('.v', '.tmp.c')

		mut cmd := '"$ts.vexe" $ts.vargs "$file"'
		if cmd_needs_quoting { cmd = '"$cmd"' }

		ts.benchmark.step()
		if show_stats {
			println('-------------------------------------------------')
			status := os.system(cmd)
			if status == 0 {
				ts.benchmark.ok()
			}else{
				ts.benchmark.fail()
				ts.failed = true
				continue
			}
		}else{
			r := os.exec(cmd) or {
				ts.benchmark.fail()
				ts.failed = true
				println(ts.benchmark.step_message('$relative_file $fail'))
				continue
			}
			if r.exit_code != 0 {
				ts.benchmark.fail()
				ts.failed = true
				println(ts.benchmark.step_message('$relative_file $fail\n`$file`\n (\n$r.output\n)'))
			} else {
				ts.benchmark.ok()
				println(ts.benchmark.step_message('$relative_file $ok'))
			}
		}
		os.rm( tmpc_filepath )
	}
	ts.benchmark.stop()
}

pub fn v_test_v(args_before_test string){
	vexe := vexe_path()
	parent_dir := os.dir(vexe)
	// Changing the current directory is needed for some of the compiler tests,
	// compiler/tests/local_test.v and compiler/tests/repl/repl_test.v
	os.chdir( parent_dir )
	if !os.dir_exists(parent_dir + '/vlib') {
		println('vlib/ is missing, it must be next to the V executable')
		exit(1)
	}
	if !os.file_exists(parent_dir + '/v.v') {
		println('v.v is missing, it must be next to the V executable')
		exit(1)
	}
	// Make sure v.c can be compiled without warnings
	$if mac {
		os.system('$vexe -o v.c v.v')
		if os.system('cc -Werror v.c') != 0 {
			println('cc failed to build v.c without warnings')
			exit(1)
		}
		println('v.c can be compiled without warnings. This is good :)')
	}
	//
	println('Testing...')
	mut ts := new_test_sesion( args_before_test )
	ts.files << os.walk_ext(parent_dir, '_test.v')
	ts.test()
	println( ts.benchmark.total_message('running V tests') )
	//
	println('\nBuilding examples...')
	mut es := new_test_sesion( args_before_test )
	files := os.walk_ext(parent_dir+'/examples','.v')
	stable := files.filter(!it.contains('vweb'))
	es.files << stable
	es.test()
	println( es.benchmark.total_message('building examples') )
	//
	test_vget()
	if ts.failed || es.failed {
		exit(1)
	}
}

pub fn test_vget() {
	/*
	vexe := vexe_path()
	ret := os.system('$vexe install nedpals.args')
	if ret != 0 {
		println('failed to run v install')
		exit(1)
	}
	if !os.file_exists(v_modules_path + '/nedpals/args') {
		println('v failed to install a test module')
		exit(1)
	}
	println('vget is OK')
	*/
}
