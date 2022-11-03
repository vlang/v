import strings
import os

const (
	max_params       = 16
	all_param_names  = []string{len: max_params, init: '${`a` + it}'}
	all_param_values = []string{len: max_params, init: '${it + 1}'}
)

struct ReturnType {
	name         string
	init         string
	assertion    string
	no_assert_kw bool
}

const return_types = [
	ReturnType{
		name: ''
		init: ''
		assertion: ''
		no_assert_kw: true
	},
	ReturnType{
		name: 'int'
		init: '-123'
		assertion: ' == -123'
	},
	ReturnType{
		name: 'u64'
		init: '123'
		assertion: ' == 123'
	},
	ReturnType{
		name: 'voidptr'
		init: 'voidptr(123)'
		assertion: ' == voidptr(123)'
	},
	ReturnType{
		name: 'string'
		init: "'hello'"
		assertion: " == 'hello'"
	},
	ReturnType{
		name: '?'
		init: "error('an error')"
		assertion: " or { assert err.msg() == 'an error' return }\npanic('got no error')"
		no_assert_kw: true
	},
	ReturnType{
		name: '?string'
		init: "'hello'"
		assertion: "? == 'hello'"
	},
	ReturnType{
		name: 'BigStruct'
		init: 'BigStruct{ a3: 56, a27: 1234, a61: 5555 }'
		assertion: ' == BigStruct{ a3: 56, a27: 1234, a61: 5555 }'
	},
]

// test_closures_with_n_args generates a new V file containing closures of `i`
// and parameters of type `typ`, to makes sure that all combinations work correctly
fn test_closures_with_n_args() {
	mut v_code := strings.new_builder(1024)
	// Note: the type or value of the captured arg doesn't matter for this test,
	// as the entire closure context is always passed as one pointer anyways

	v_code.write_string('struct BigStruct {')
	for i in 0 .. 64 {
		v_code.write_string('\ta$i int ')
	}
	v_code.writeln('}')

	for typ in ['byte', 'u16', 'int', 'i64', 'voidptr', 'string'] {
		for i in 0 .. max_params {
			param_names := all_param_names[..i]
			params := param_names.map('$it $typ')

			mut values := all_param_values[..i]
			if typ == 'string' {
				values = values.map("'$it'")
			} else {
				values = values.map('${typ}($it)')
			}

			mut expected_val := if typ == 'string' {
				s := all_param_values[..i].join('')
				"'127' + '$s'"
			} else {
				'127 + ${i * (i + 1) / 2}'
			}

			init_val, return_type := if typ != 'string' {
				'u64(127)', 'u64'
			} else {
				"'127'", 'string'
			}

			// Note: the captured arg doesn't matter for this test, as closures always receive
			// a pointer to the entire closure context as their last argument anyways
			v_code.writeln("
fn test_big_closure_${typ}_${i}() {
	println('test_big_closure_${typ}_$i')
	mut local := 123
	mut local_2 := 234
	mut z := $init_val
	c := fn [z] (${params.join(', ')}) $return_type {
		mut sum := z")
			for j in 0 .. i {
				if return_type == 'string' {
					v_code.writeln('\t\tsum += ${param_names[j]}')
				} else {
					v_code.writeln('\t\tsum += ${return_type}(${param_names[j]})')
				}
			}
			v_code.writeln("
		return sum
	}
	assert c(${values.join(', ')}) == $expected_val
	// ensure stack wasn't overwritten:
	assert local == 123
	assert local_2 == 234
}")
		}
	}

	for return_type in return_types {
		typ := return_type.name
		styp := typ.replace('?', 'option_').to_lower()
		init := return_type.init
		assertion := return_type.assertion

		for i in 0 .. 10 {
			param_names := all_param_names[..i]
			params := param_names.map('$it int')
			values := all_param_values[..i]

			assert_line := if !return_type.no_assert_kw {
				'assert c(${values.join(', ')}) $assertion'
			} else {
				'c(${values.join(', ')}) $assertion'
			}
			// Note: the captured arg doesn't matter for this test, as closures always receive
			// a pointer to the entire closure context as their last argument anyways
			v_code.writeln("
fn test_closure_return_${styp}_${i}() ? {
	println('test_closure_return_${styp}_$i')
	mut local := 123
	mut local_2 := 234
	mut z := 1234
	c := fn [z] (${params.join(', ')}) $typ {
		return $init
	}
	$assert_line
	// ensure stack wasn't overwritten:
	assert local == 123
	assert local_2 == 234
}")
		}
	}

	code := v_code.str()
	println('Compiling V code (${code.count('\n')} lines) ...')
	wrkdir := os.join_path(os.vtmp_dir(), 'v', 'tests', 'closures')
	os.mkdir_all(wrkdir)?
	os.chdir(wrkdir)?
	os.write_file('closure_return_test.v', code)?
	vexe := os.getenv('VEXE')
	res := os.execute('${os.quoted_path(vexe)} -keepc -cg -showcc closure_return_test.v')
	if res.exit_code != 0 {
		eprintln(res.output)
		assert false
	}
	println('Process exited with code $res.exit_code')

	os.chdir(os.dir(vexe)) or {}
	os.rmdir_all(wrkdir) or {}
}
