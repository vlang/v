import os
import term

const vroot = os.real_path(@VMODROOT)
const test_dir = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'multifile_gotodef')
const main_file = os.join_path(test_dir, 'main.v')
const types_file = os.join_path(test_dir, 'types.v')

struct TestCase {
	name        string
	line        int
	col         int
	expected    string
	description string
}

const test_cases = [
	TestCase{
		name:        'struct_name_cross_file'
		line:        4
		col:         12
		expected:    'multifile_gotodef/types.v:3:11'
		description: 'Go to struct definition in another file (MyStruct in types.v)'
	},
	TestCase{
		name:        'method_call_cross_file'
		line:        8
		col:         18
		expected:    'multifile_gotodef/types.v:14:21'
		description: 'Go to method definition in another file (get_value in types.v)'
	},
	TestCase{
		name:        'struct_field_cross_file'
		line:        11
		col:         22
		expected:    'multifile_gotodef/types.v:4:1'
		description: 'Go to struct field definition in another file (value in types.v)'
	},
	TestCase{
		name:        'enum_value_cross_file'
		line:        14
		col:         15
		expected:    'multifile_gotodef/types.v:9:1'
		description: 'Go to enum value definition in another file (first in types.v)'
	},
	TestCase{
		name:        'enum_short_form_cross_file'
		line:        18
		col:         4
		expected:    'multifile_gotodef/types.v:10:1'
		description: 'Go to enum value definition from short form in match (.second in types.v)'
	},
]

fn test_multifile_goto_definition() {
	mut total_errors := 0
	mut passed := 0

	// Change to vls directory so relative paths match expected output
	original_dir := os.getwd()
	vls_dir := os.join_path(vroot, 'vlib', 'v', 'tests', 'vls')
	os.chdir(vls_dir) or { panic(err) }
	defer {
		os.chdir(original_dir) or {}
	}

	for tc in test_cases {
		cmd := 'v -w -check -json-errors -nocolor -vls-mode -line-info "${main_file}:${tc.line}:gd^${tc.col}" ${os.quoted_path('multifile_gotodef')}'
		res := os.execute(cmd)

		if res.exit_code < 0 {
			println('${term.red('FAIL')} ${tc.name}: Command failed to execute')
			println('  Command: ${cmd}')
			total_errors++
			continue
		}

		res_output := $if windows {
			res.output.replace('\r\n', '\n').trim_space()
		} $else {
			res.output.trim_space()
		}

		if tc.expected != res_output {
			println('${term.red('FAIL')} ${tc.name}')
			println('  Description: ${tc.description}')
			println('  Line ${tc.line}, Column ${tc.col}')
			println('  Expected: ${tc.expected}')
			println('  Got:      ${res_output}')
			total_errors++
		} else {
			println('${term.green('OK  ')} ${tc.name}: ${tc.description}')
			passed++
		}
	}

	println('')
	println('${term.header('Summary:', '=')}')
	println('Passed: ${passed}/${test_cases.len}')
	if total_errors > 0 {
		println('${term.red('Failed:')} ${total_errors}')
	}

	assert total_errors == 0, 'Some tests failed'
}
