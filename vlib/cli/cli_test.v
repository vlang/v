module main

import v.util.diff
import term
import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn test_cli_programs() {
	testdata := os.join_path(vroot, 'vlib', 'cli', 'testdata')
	mut has_err := false
	for test in os.walk_ext(testdata, '.vv') {
		print(test + ' ')
		out_path := test.all_before_last('.vv') + '.out'
		if !os.exists(out_path) {
			println(term.red('FAIL'))
			eprintln('failed to find output file for `${test}`')
			has_err = true
			continue
		}
		expected_out := os.read_file(out_path)!.replace('\r\n', '\n')
		test_out := os.execute('${vexe} run ${test}').output.replace('\r\n', '\n')
		diff_ := diff.compare_text(expected_out, test_out)!
		if diff_ != '' {
			println(term.red('FAIL'))
			eprintln(diff_)
			has_err = true
		} else {
			println(term.green('OK'))
		}
	}
	assert !has_err
}
