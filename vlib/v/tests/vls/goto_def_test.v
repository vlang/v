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
	// test_local_struct() tests
	TestCase{
		name:        'method_call'
		line:        17
		col:         13
		expected:    '${test_file}:10:20'
		description: 'Go to method definition from method call'
	},
	TestCase{
		name:        'struct_init'
		line:        20
		col:         13
		expected:    '${test_file}:6:7'
		description: 'Go to struct definition from StructInit'
	},
	TestCase{
		name:        'struct_init_field_name'
		line:        15
		col:         28
		expected:    '${test_file}:7:1'
		description: 'Go to field definition from field name in struct initialization'
	},
	TestCase{
		name:        'field_access'
		line:        21
		col:         21
		expected:    '${test_file}:7:1'
		description: 'Go to field definition from field access'
	},
	TestCase{
		name:        'variable_reference'
		line:        23
		col:         17
		expected:    '${test_file}:15:6'
		description: 'Go to variable definition from reference'
	},
	TestCase{
		name:        'field_access_via_variable'
		line:        26
		col:         17
		expected:    '${test_file}:7:1'
		description: 'Go to field definition from obj.value'
	},
	TestCase{
		name:        'method_receiver_type'
		line:        10
		col:         8
		expected:    '${test_file}:6:7'
		description: 'Go to struct definition from method receiver type (LocalStruct)'
	},
	TestCase{
		name:        'method_return_type_builtin'
		line:        10
		col:         33
		expected:    os.join_path(vroot, 'vlib', 'builtin', 'string.v') + ':45:11'
		description: 'Builtin return type (string) jumps to builtin string definition'
	},
	// test_deep_struct() tests
	TestCase{
		name:        'nested_struct_st'
		line:        49
		col:         22
		expected:    '${test_file}:38:1'
		description: 'Go to nested anonymous struct field definition (deep.st)'
	},
	TestCase{
		name:        'nested_struct_sstt'
		line:        49
		col:         25
		expected:    '${test_file}:39:2'
		description: 'Go to deeply nested anonymous struct field definition (deep.st.sstt)'
	},
	TestCase{
		name:        'nested_struct_l2'
		line:        49
		col:         30
		expected:    '${test_file}:40:3'
		description: 'Go to deeply nested struct field definition (deep.st.sstt.l2)'
	},
	TestCase{
		name:        'embedded_struct_field'
		line:        52
		col:         23
		expected:    '${test_file}:37:1'
		description: 'Go to embedded struct field declaration (deep.LocalStruct)'
	},
	TestCase{
		name:        'embedded_field_to_type'
		line:        37
		col:         2
		expected:    '${test_file}:6:7'
		description: 'Go to struct type definition from embedded field declaration'
	},
	TestCase{
		name:        'deep_2_nested_sstt_in_init'
		line:        58
		col:         21
		expected:    '${test_file}:39:2'
		description: 'Go to field definition from deep.st.sstt in struct init'
	},
	TestCase{
		name:        'deep_2_nested_l1_in_init'
		line:        60
		col:         16
		expected:    '${test_file}:42:2'
		description: 'Go to field definition from deep.st.l1 in struct init'
	},
	// test_local_enum() tests
	TestCase{
		name:        'enum_value_qualified'
		line:        72
		col:         18
		expected:    '${test_file}:66:1'
		description: 'Go to enum value definition (qualified form: LocalEnum.first)'
	},
	TestCase{
		name:        'enum_value_short_form'
		line:        75
		col:         3
		expected:    '${test_file}:67:1'
		description: 'Go to enum value definition (short form: .second in match)'
	},
	// test_local_alias() tests
	TestCase{
		name:        'type_alias_cast'
		line:        83
		col:         13
		expected:    '${test_file}:80:5'
		description: 'Go to type alias definition in cast expression'
	},
	TestCase{
		name:        'type_alias_rhs_type'
		line:        80
		col:         19
		expected:    os.join_path(vroot, 'vlib', 'builtin', 'string.v') + ':45:11'
		description: 'Go to builtin type definition from type alias RHS'
	},
	// test_local_sum() tests
	TestCase{
		name:        'sum_type_cast'
		line:        99
		col:         13
		expected:    '${test_file}:86:5'
		description: 'Go to sum type definition in cast expression'
	},
	TestCase{
		name:        'user_sum_type_rhs_type_a'
		line:        96
		col:         18
		expected:    '${test_file}:88:7'
		description: 'Go to TypeA definition from UserSum type RHS'
	},
	TestCase{
		name:        'user_sum_type_rhs_type_b'
		line:        96
		col:         26
		expected:    '${test_file}:92:7'
		description: 'Go to TypeB definition from UserSum type RHS'
	},
	// test_match_enum() tests
	TestCase{
		name:        'match_enum_short_form'
		line:        115
		col:         3
		expected:    '${test_file}:107:1'
		description: 'Go to enum value definition in match expression (short form .completion)'
	},
	TestCase{
		name:        'match_enum_variable'
		line:        114
		col:         22
		expected:    '${test_file}:113:1'
		description: 'Go to variable definition from match expression condition'
	},
	// test_imported_enum() tests
	TestCase{
		name:        'imported_enum_value'
		line:        129
		col:         25
		expected:    '${mod1_text_file}:25:1'
		description: 'Go to imported enum value definition'
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
