import os
import term
import v.util.diff

const vroot = os.real_path(@VMODROOT)
const test_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'goto_def_test_data.vv')
const mod1_text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_mod1',
	'sample.v')

struct TestCase {
	name        string
	line        int
	col         int
	expected    string
	description string
}

const test_cases = [
	TestCase{
		name:        'method_definition'
		line:        27
		col:         13
		expected:    '${test_file}:20:20'
		description: 'Go to method definition from method call'
	},
	TestCase{
		name:        'enum_value_qualified'
		line:        30
		col:         20
		expected:    '${test_file}:11:1'
		description: 'Go to enum value definition (qualified form: LocalEnum.first)'
	},
	TestCase{
		name:        'enum_value_short_form'
		line:        33
		col:         3
		expected:    '${test_file}:12:1'
		description: 'Go to enum value definition (short form: .second in match)'
	},
	TestCase{
		name:        'type_alias_cast'
		line:        37
		col:         15
		expected:    '${test_file}:16:5'
		description: 'Go to type alias definition in cast expression'
	},
	TestCase{
		name:        'sum_type_cast'
		line:        39
		col:         13
		expected:    '${test_file}:18:5'
		description: 'Go to sum type definition in cast expression'
	},
	TestCase{
		name:        'struct_init'
		line:        42
		col:         13
		expected:    '${test_file}:6:7'
		description: 'Go to struct definition from StructInit'
	},
	TestCase{
		name:        'variable_reference'
		line:        45
		col:         17
		expected:    '${test_file}:25:5'
		description: 'Go to variable definition from reference'
	},
	TestCase{
		name:        'imported_enum_value'
		line:        48
		col:         25
		expected:    '${mod1_text_file}:25:1'
		description: 'Go to imported enum value definition'
	},
	TestCase{
		name:        'field_access'
		line:        51
		col:         16
		expected:    '${test_file}:7:1'
		description: 'Go to field definition from field access'
	},
]

fn test_goto_definition() {
	mut total_errors := 0
	mut passed := 0

	for tc in test_cases {
		cmd := 'v -w -check -json-errors -nocolor -vls-mode -line-info "${test_file}:${tc.line}:gd^${tc.col}" ${os.quoted_path(test_file)}'
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
			if diff_ := diff.compare_text(tc.expected, res_output) {
				println('  Difference:')
				println(diff_)
			} else {
				println('  Expected: ${tc.expected}')
				println('  Got:      ${res_output}')
			}
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
