import os
import benchmark
import term

// TODO some logic copy pasted from valgrind_test.v and compiler_test.v, move to a module
fn test_x64() {
	$if !amd64 {
		return
	}
	if os.user_os() != 'linux' {
		eprintln('x64 tests can only be run on Linux for now.')
		exit(0)
	}
	mut bench := benchmark.new_benchmark()
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	dir := os.join_path(vroot, 'vlib/v/gen/x64/tests')
	files := os.ls(dir) or { panic(err) }
	//
	wrkdir := os.join_path(os.temp_dir(), 'vtests', 'x64')
	os.mkdir_all(wrkdir) or { panic(err) }
	os.chdir(wrkdir)
	tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no x64 tests found')
		assert false
	}
	bench.set_total_expected_steps(tests.len)
	for test in tests {
		bench.step()
		full_test_path := os.real_path(os.join_path(dir, test))
		relative_test_path := full_test_path.replace(vroot + '/', '')
		work_test_path := '$wrkdir/x.v'
		os.cp(full_test_path, work_test_path) or {}
		res_x64 := os.execute('$vexe -o exe -x64 $work_test_path')
		if res_x64.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('x64 $test failed'))
			continue
		}
		res := os.execute('./exe')
		if res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('$full_test_path failed to run'))
			eprintln(res.output)
			continue
		}
		mut expected := os.read_file('$dir/${test}.out') or { panic(err) }
		expected = expected.trim_right('\r\n').replace('\r\n', '\n')
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		found = found.trim_space()
		if expected != found {
			println(term.red('FAIL'))
			println('============')
			println('expected: "$expected" len=$expected.len')
			println('============')
			println('found:"$found" len=$found.len')
			println('============\n')
			bench.fail()
			continue
		}
		bench.ok()
		eprintln(bench.step_message_ok(relative_test_path))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('x64'))
	if bench.nfail > 0 {
		exit(1)
	}
}
