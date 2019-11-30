module testing

import (
	os
	term
	benchmark
)

pub struct TestSession {
pub mut:
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

pub fn vexe_path() string {
	// NB: tools extracted from v require that the first
	// argument to them to be the v executable location.
	// They are usually launched by vlib/compiler/vtools.v,
	// launch_tool/1 , which provides it.
	return os.args[1]
}


pub fn (ts mut TestSession) test() {
	ok   := term.ok_message('OK')
	fail := term.fail_message('FAIL')
	show_stats := '-stats' in ts.vargs.split(' ')
	ts.benchmark = benchmark.new_benchmark()
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
