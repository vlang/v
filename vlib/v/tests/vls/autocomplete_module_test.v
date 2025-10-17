import os
import term
import v.util.diff

const vroot = os.real_path(@VMODROOT)

const text_file_orig = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_text.vv')
const text_file = os.join_path(os.temp_dir(), 'sample_text.v')

fn testsuite_begin() {
	eprintln('testsuite_begin, text_file = ${text_file}')
	os.cp(text_file_orig, text_file) or { panic(err) }
}

fn testsuite_end() {
}

struct TestData {
	cmd    string
	output string
}

const test_data = [
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:18:3" ${os.quoted_path(text_file)}'
		output: '{"details" : [
{"kind":3,"label":"public_fn1","detail":"string","documentation":""},
{"kind":22,"label":"PublicStruct1","detail":"","documentation":""},
{"kind":13,"label":"PublicEnum1","detail":"","documentation":""},
{"kind":8,"label":"PublicInterface1","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias1_1","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias1_2","detail":"","documentation":""},
{"kind":21,"label":"public_const1","detail":"","documentation":""}
]}'
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
		if t.output != res.output {
			println('${term.red('FAIL')} ${t.cmd}')
			if diff_ := diff.compare_text(t.output, res.output) {
				println(term.header('difference:', '-'))
				println(diff_)
			} else {
				println(term.header('expected:', '-'))
				println(t.output)
				println(term.header('found:', '-'))
				println(res.output)
			}
			println(term.h_divider('-'))
			total_errors++
		} else {
			println('${term.green('OK  ')} ${t.cmd}')
		}
	}
	assert total_errors == 0
}
