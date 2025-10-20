import os
import term
import v.util.diff

const vroot = os.real_path(@VMODROOT)
const tmp_dir = os.real_path(os.temp_dir())

const text_file_orig = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_text.vv')
const text_file = os.join_path(tmp_dir, 'sample_text.v')
const text_file_result = $if windows { text_file.replace('\\', '/') } $else { text_file }

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
	TestData{
		cmd:    'v -w -vls-mode -check -json-errors ${os.quoted_path(text_file)}'
		output: '[
{
"path":"${text_file}",
"message":"undefined ident: `a`",
"line_nr":14,
"col":4,
"len":0
}
,
{
"path":"${text_file}",
"message":"operator `+=` not defined on left operand type `void`",
"line_nr":14,
"col":4,
"len":0
}
,
{
"path":"${text_file}",
"message":"cannot assign to `a`: expected `void`, not `int`",
"line_nr":14,
"col":9,
"len":0
}
,
{
"path":"${text_file}",
"message":"undefined ident: `s`",
"line_nr":18,
"col":2,
"len":0
}
]
'
	},
	TestData{
		cmd:    'v -check -nocolor -vls-mode ${os.quoted_path(text_file)}'
		output: '${text_file_result}:14:4: error: undefined ident: `a`
   12 | // add add `val` to `a`
   13 | fn (mut m MyS) add(val int) {
   14 |     m.a += val
      |       ^
   15 | }
   16 |
${text_file_result}:14:4: error: operator `+=` not defined on left operand type `void`
   12 | // add add `val` to `a`
   13 | fn (mut m MyS) add(val int) {
   14 |     m.a += val
      |       ^
   15 | }
   16 |
${text_file_result}:14:9: error: cannot assign to `a`: expected `void`, not `int`
   12 | // add add `val` to `a`
   13 | fn (mut m MyS) add(val int) {
   14 |     m.a += val
      |            ~~~
   15 | }
   16 |
${text_file_result}:18:2: error: undefined ident: `s`
   16 | 
   17 | fn main() {
   18 |     s.
      |     ^
   19 |     //sample_mod2.
   20 |     //mut k := MyS{}
${text_file_result}:5:8: warning: module \'sample_mod2 (v.tests.vls.sample_mod2)\' is imported but never used
    3 | 
    4 | import v.tests.vls.sample_mod1 as s
    5 | import v.tests.vls.sample_mod2
      |        ~~~~~~~~~~~~~~~~~~~~~~~
    6 | 
    7 | struct MyS{
'
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
