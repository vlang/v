module testing

import (
	os
	term
	benchmark
	filepath
)

pub struct TestSession {
pub mut:
	files []string
	vexe string
	vargs string
	failed bool
	benchmark benchmark.Benchmark

	ok string
	fail string
}

pub fn new_test_sesion(vargs string) TestSession {
	return TestSession{
		vexe: vexe_path()
		vargs: vargs
	}
}

pub fn vexe_path() string {
	// NB: tools extracted from v require that the first
	// argument to them to be the v executable location.
	// They are usually launched by vlib/compiler/vtools.v,
	// launch_tool/1 , which provides it.
	return os.args[1]
}


pub fn (ts mut TestSession) init() {
	ts.ok   = term.ok_message('OK')
	ts.fail = term.fail_message('FAIL')
	ts.benchmark = benchmark.new_benchmark()
}

pub fn (ts mut TestSession) test() {
	ts.init()
	show_stats := '-stats' in ts.vargs.split(' ')
	for dot_relative_file in ts.files {
		relative_file := dot_relative_file.replace('./', '')
		file := os.realpath( relative_file )
		$if windows {
			if file.contains('sqlite') { continue }
		}
		$if msvc {
			if file.contains('asm') { continue }
		}
		$if tinyc {
			if file.contains('asm') { continue }
		}
		tmpc_filepath := file.replace('.v', '.tmp.c')

		cmd := '"$ts.vexe" $ts.vargs "$file"'

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
				println(ts.benchmark.step_message('$relative_file ${ts.fail}'))
				continue
			}
			if r.exit_code != 0 {
				ts.benchmark.fail()
				ts.failed = true
				println(ts.benchmark.step_message('$relative_file ${ts.fail}\n`$file`\n (\n$r.output\n)'))
			} else {
				ts.benchmark.ok()
				println(ts.benchmark.step_message('$relative_file ${ts.ok}'))
			}
		}
		os.rm( tmpc_filepath )
	}
	ts.benchmark.stop()
}

pub fn vlib_should_be_present( parent_dir string ) {
	vlib_dir := filepath.join( parent_dir, 'vlib' )
	if !os.dir_exists( vlib_dir ){
		println('$vlib_dir is missing, it must be next to the V executable')
		exit(1)
	}
}

pub fn v_build_failing(vargs string, folder string) bool {
	main_label := 'Building $folder ...'
	finish_label := 'building $folder'
	vexe := vexe_path()
	parent_dir := os.dir(vexe)
	vlib_should_be_present( parent_dir )
  
	println(main_label)
	mut session := new_test_sesion( vargs )
	files := os.walk_ext(filepath.join(parent_dir, folder),'.v')
	mains := files.filter(!it.contains('modules'))
	session.files << mains
	session.test()
	println( session.benchmark.total_message( finish_label ) )

	return session.failed
}
