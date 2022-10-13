import os
import benchmark
import term

const is_verbose = os.getenv('VTEST_SHOW_CMD') != ''

fn test_interpret() {
	mut bench := benchmark.new_benchmark()
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)?
	dir := os.join_path(vroot, 'vlib/v/eval/testdata')
	files := os.ls(dir)?
	//
	tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no interpreter tests found')
		assert false
	}
	bench.set_total_expected_steps(tests.len)
	for test in tests {
		test_name_without_postfix := test.replace('.vv', '')
		bench.step()
		full_test_path := os.real_path(os.join_path(dir, test))
		test_file_name := os.file_name(test)
		relative_test_path := full_test_path.replace(vroot + '/', '')
		cmd := '${os.quoted_path(vexe)} interpret ${os.quoted_path(full_test_path)}'
		if is_verbose {
			println(cmd)
		}
		res := os.execute(cmd)
		if res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('$full_test_path failed to run'))
			eprintln(res.output)
			continue
		}
		mut expected := os.read_file('$dir/${test_name_without_postfix}.out')?
		expected = normalise_line_endings(expected)
		mut found := normalise_line_endings(res.output)
		found = found.trim_space()
		if expected != found {
			println(term.red('FAIL'))
			println('========================================================\n')
			println('============ expected len=$expected.len: "$expected"')
			println('============ found    len=$found.len: "$found"')
			println('========================================================\n')
			bench.fail()
			continue
		}
		bench.ok()
		eprintln(bench.step_message_ok(relative_test_path))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('native'))
	if bench.nfail > 0 {
		exit(1)
	}
}

fn normalise_line_endings(s string) string {
	return s.trim_right('\r\n').replace('\r\n', '\n')
}
