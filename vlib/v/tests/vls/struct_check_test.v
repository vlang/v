import os
import term
import v.util.diff

const vroot = os.real_path(@VMODROOT)
const tmp_dir = os.real_path(os.temp_dir())

const text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'struct_text.vv')

fn testsuite_begin() {
	eprintln('testsuite_begin, text_file = ${text_file}')
}

fn testsuite_end() {
}

struct TestData {
	cmd    string
	output string
}

const test_data = [
	TestData{
		cmd:    'v -check -vls-mode ${os.quoted_path(text_file)}'
		output: '' // for a struct with `mut:` in it, should report no error
	},
]

fn test_main() {
	mut total_errors := 0

	for t in test_data {
		res := os.execute(t.cmd)
		if res.exit_code < 0 {
			println('fail execute ${t.cmd}')
			panic(res.output)
		}
		res_output := $if windows {
			res.output.replace('\r\n', '\n')
		} $else {
			res.output
		}
		if t.output != res_output {
			println('${term.red('FAIL')} ${t.cmd}')
			if diff_ := diff.compare_text(t.output, res_output) {
				println(term.header('difference:', '-'))
				println(diff_)
			} else {
				println(term.header('expected:', '-'))
				println(t.output)
				println(term.header('found:', '-'))
				println(res_output)
			}
			println(term.h_divider('-'))
			total_errors++
		} else {
			println('${term.green('OK  ')} ${t.cmd}')
		}
	}
	assert total_errors == 0
}
