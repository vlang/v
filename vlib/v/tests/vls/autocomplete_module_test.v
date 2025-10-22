import os
import term
import v.util.diff

const vroot = os.real_path(@VMODROOT)
const tmp_dir = os.real_path(os.temp_dir())
const text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_text.vv')

const autocomplete_info_for_mod_sample_mod1 = '{"details" : [
{"kind":3,"label":"public_fn1","detail":"string","documentation":""},
{"kind":22,"label":"PublicStruct1","detail":"","documentation":""},
{"kind":13,"label":"PublicEnum1","detail":"","documentation":""},
{"kind":8,"label":"PublicInterface1","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias1_1","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias1_2","detail":"","documentation":""},
{"kind":21,"label":"public_const1","detail":"","documentation":""}
]}'

const autocomplete_info_for_mod_sample_mod2 = '{"details" : [
{"kind":3,"label":"public_fn2","detail":"string","documentation":""},
{"kind":22,"label":"PublicStruct2","detail":"","documentation":""},
{"kind":13,"label":"PublicEnum2","detail":"","documentation":""},
{"kind":8,"label":"PublicInterface2","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias2","detail":"","documentation":""},
{"kind":21,"label":"public_const2","detail":"","documentation":""}
]}'

const autocomplete_info_for_mod_struct = '{"details" : [
{"kind":5,"label":"a","detail":"int","documentation":""},
{"kind":5,"label":"b","detail":"string","documentation":""},
{"kind":2,"label":"add","detail":"void","documentation":""}
]}'

struct TestData {
	cmd    string
	output string
}

const test_data = [
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:19:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:20:13" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod2
	},
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:22:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_struct
	},
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:23:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:26:28" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:27:8" ${os.quoted_path(text_file)}'
		output: ''
	},
	TestData{
		cmd:    'v -check -json-errors -nocolor -vls-mode -line-info "${text_file}:28:9" ${os.quoted_path(text_file)}'
		output: 'unresolved type, maybe "builtin" was not defined. otherwise this is a bug, should never happen; please report'
	},
	TestData{
		cmd:    'v -w -vls-mode -check -json-errors ${os.quoted_path(text_file)}'
		output: '[
{
"path":"${text_file}",
"message":"unexpected token `:=`, expecting `)`",
"line_nr":26,
"col":4,
"len":0
}
,
{
"path":"${text_file}",
"message":"unexpected name `strings`, expecting `)`",
"line_nr":27,
"col":2,
"len":0
}
,
{
"path":"${text_file}",
"message":"undefined ident: ``",
"line_nr":19,
"col":3,
"len":0
}
,
{
"path":"${text_file}",
"message":"undefined ident: ``",
"line_nr":20,
"col":13,
"len":0
}
,
{
"path":"${text_file}",
"message":"undefined ident: ``",
"line_nr":23,
"col":3,
"len":0
}
,
{
"path":"${text_file}",
"message":"cannot use `main.MyS` as `string` in argument 1 to `string.all_before_last`",
"line_nr":26,
"col":2,
"len":0
}
,
{
"path":"${text_file}",
"message":"undefined ident: ``",
"line_nr":26,
"col":28,
"len":0
}
,
{
"path":"${text_file}",
"message":"`` (no value) used as value in argument 1 to `string.all_before_last`",
"line_nr":26,
"col":27,
"len":0
}
,
{
"path":"${text_file}",
"message":"`string` has no property ``",
"line_nr":26,
"col":11,
"len":0
}
,
{
"path":"${text_file}",
"message":"undefined ident: `builtin`",
"line_nr":28,
"col":2,
"len":0
}
,
{
"path":"${text_file}",
"message":"`builtin` does not return a value",
"line_nr":28,
"col":2,
"len":0
}
]
'
	},
]

fn test_main() {
	mut total_errors := 0

	dump(text_file)
	for t in test_data {
		res := os.execute(t.cmd)
		if res.exit_code < 0 {
			println('fail execute ${t.cmd}')
			panic(res.output)
		}
		res_output := $if windows {
			res.output.replace('\r\n', '\n').trim_space()
		} $else {
			res.output.trim_space()
		}
		if t.output.trim_space() != res_output {
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
