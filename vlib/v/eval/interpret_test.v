import os
import benchmark
import term
import v.util.diff

const is_verbose = os.getenv('VTEST_SHOW_CMD') != ''
const is_autofix = os.getenv('VAUTOFIX') != ''

fn test_interpret() {
	mut bench := benchmark.new_benchmark()
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)!
	dir := os.join_path(vroot, 'vlib/v/eval/testdata')
	files := os.ls(dir)!

	tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no interpreter tests found')
		assert false
	}
	bench.set_total_expected_steps(tests.len)
	mut failcmds := []string{}
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
			eprintln(bench.step_message_fail('${cmd} failed to run'))
			eprintln(res.output)
			continue
		}
		expected_file_path := '${dir}/${test_name_without_postfix}.out'
		mut expected := os.read_file(expected_file_path)!
		expected = normalise_line_endings(expected)
		mut found := normalise_line_endings(res.output)
		found = found.trim_space()
		if expected != found {
			failcmds << cmd
			bench.fail()
			eprintln(bench.step_message_fail('difference found, when running ${cmd}'))
			eprintln('='.repeat(80))
			eprintln('============    .out file: ${expected_file_path}')
			eprintln('============ expected len: ${expected.len}:\n${expected}')
			eprintln('============    found len: ${found.len}:\n${found}')
			if tdiff := diff.compare_text(expected, found) {
				eprintln('-'.repeat(80))
				eprintln('=== diff:')
				eprint(tdiff)
			}
			eprintln('='.repeat(80))
			if is_autofix {
				os.write_file(expected_file_path, found)!
			}
			continue
		}
		bench.ok()
		eprintln(bench.step_message_ok(relative_test_path))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('native'))
	if bench.nfail > 0 {
		println('You can reproduce the discovered failure cases with:')
		for fcmd in failcmds {
			println('   > ${fcmd}')
		}
		exit(1)
	}
}

fn normalise_line_endings(s string) string {
	return s.trim_right('\r\n').replace('\r\n', '\n')
}
