import strings
import os

const max_params = get_max_params()
const max_string_params = get_max_string_params()
const all_param_names = []string{len: max_params, init: '${`a` + index}'}
const all_param_values = []string{len: max_params, init: '${index + 1}'}

fn get_max_params() int {
	return 16
}

fn get_max_string_params() int {
	return 16
}

struct ReturnType {
	name         string
	init         string
	assertion    string
	no_assert_kw bool
}

const return_types = [
	ReturnType{
		name:         ''
		init:         ''
		assertion:    ''
		no_assert_kw: true
	},
	ReturnType{
		name:      'int'
		init:      '-123'
		assertion: ' == -123'
	},
	ReturnType{
		name:      'u64'
		init:      '123'
		assertion: ' == 123'
	},
	ReturnType{
		name:      'voidptr'
		init:      'voidptr(123)'
		assertion: ' == voidptr(123)'
	},
	ReturnType{
		name:      'string'
		init:      "'hello'"
		assertion: " == 'hello'"
	},
	ReturnType{
		name:         '!'
		init:         "error('an error')"
		assertion:    " or { assert err.msg() == 'an error' return }\npanic('got no error')"
		no_assert_kw: true
	},
	ReturnType{
		name:      '?string'
		init:      "'hello'"
		assertion: "? == 'hello'"
	},
	ReturnType{
		name:      'BigStruct'
		init:      'BigStruct{ a3: 56, a27: 1234, a61: 5555 }'
		assertion: ' == BigStruct{ a3: 56, a27: 1234, a61: 5555 }'
	},
]

// test_closures_with_n_args generates a new V file containing closures of `i`
// and parameters of type `typ`, to makes sure that all combinations work correctly
fn test_closures_with_n_args() {
	mut v_code := strings.new_builder(50_000)
	// Note: the type or value of the captured arg doesn't matter for this test,
	// as the entire closure context is always passed as one pointer anyways

	v_code.write_string('struct BigStruct {')
	for i in 0 .. 64 {
		v_code.write_string('\ta${i} int ')
	}
	v_code.writeln('}')

	for typ in ['u8', 'u16', 'int', 'i64', 'voidptr'] {
		for i in 0 .. max_params {
			param_names := all_param_names[..i]
			params := param_names.map('${it} ${typ}')

			mut values := all_param_values[..i].clone()
			values = values.map('${typ}(${it})')

			expected_val := '127 + ${i * (i + 1) / 2}'

			init_val := 'u64(127)'
			return_type := 'u64'

			// Note: the captured arg doesn't matter for this test, as closures always receive
			// a pointer to the entire closure context as their last argument anyways
			v_code.writeln("
fn test_big_closure_${typ}_${i}() {
	println('test_big_closure_${typ}_${i}')
	mut local := 123
	mut local_2 := 234
	mut z := ${init_val}
	c := fn [z] (${params.join(', ')}) ${return_type} {
		mut sum := z")
			for j in 0 .. i {
				v_code.writeln('\t\tsum += ${return_type}(${param_names[j]})')
			}
			v_code.writeln("
		return sum
	}
	assert c(${values.join(', ')}) == ${expected_val}
	// ensure stack wasn't overwritten:
	assert local == 123
	assert local_2 == 234
}")
		}
	}

	// handle string type separately
	for i in 0 .. max_string_params {
		param_names := all_param_names[..i]
		params := param_names.map('${it} string')

		mut values := all_param_values[..i].clone()
		values = values.map("'${it}'")

		s := all_param_values[..i].join('')
		expected_val := "'127' + '${s}'"

		init_val := "'127'"
		return_type := 'string'

		// Note: the captured arg doesn't matter for this test, as closures always receive
		// a pointer to the entire closure context as their last argument anyways
		v_code.writeln("
fn test_big_closure_string_${i}() {
	println('test_big_closure_string_${i}')
	mut local := 123
	mut local_2 := 234
	mut z := ${init_val}
	c := fn [z] (${params.join(', ')}) ${return_type} {
		mut sum := z")
		for j in 0 .. i {
			v_code.writeln('\t\tsum += ${param_names[j]}')
		}
		v_code.writeln("
		return sum
	}
	assert c(${values.join(', ')}) == ${expected_val}
	// ensure stack wasn't overwritten:
	assert local == 123
	assert local_2 == 234
}")
	}

	for return_type in return_types {
		typ := return_type.name
		styp := typ.replace_each(['?', 'option_', '!', 'result_']).to_lower_ascii()
		init := return_type.init
		assertion := return_type.assertion

		for i in 0 .. max_params {
			param_names := all_param_names[..i]
			params := param_names.map('${it} int')
			values := all_param_values[..i]

			assert_line := if !return_type.no_assert_kw {
				'assert c(${values.join(', ')}) ${assertion}'
			} else {
				'c(${values.join(', ')}) ${assertion}'
			}
			// Note: the captured arg doesn't matter for this test, as closures always receive
			// a pointer to the entire closure context as their last argument anyways
			v_code.writeln("
fn test_closure_return_${styp}_${i}() ! {
	println('test_closure_return_${styp}_${i}')
	mut local := 123
	mut local_2 := 234
	mut z := 1234
	c := fn [z] (${params.join(', ')}) ${typ} {
		return ${init}
	}
	${assert_line}
	// ensure stack wasn't overwritten:
	assert local == 123
	assert local_2 == 234
}")
		}
	}

	code := v_code.str()
	println('Compiling V code (${code.count('\n')} lines) ...')
	wrkdir := os.join_path(os.vtmp_dir(), 'closure_generator_tests')
	os.mkdir_all(wrkdir)!
	os.chdir(wrkdir)!
	full_path_to_target := os.join_path(wrkdir, 'closure_return_test.v')
	os.write_file(full_path_to_target, code)!
	vexe := os.getenv('VEXE')
	cmd := '${os.quoted_path(vexe)} -keepc -cg -showcc ${full_path_to_target}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln(res.output)
		eprintln('> failed exit code: ${res.exit_code} | cmd:\n${cmd}')
		assert false
	}
	os.chdir(os.dir(vexe)) or {}
	os.rmdir_all(wrkdir) or {}
}
