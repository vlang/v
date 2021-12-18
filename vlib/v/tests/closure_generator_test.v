import strings
import os

const (
	max_params       = 16
	all_param_names  = []string{len: max_params, init: '${`a` + it}'}
	all_param_values = []string{len: max_params, init: '${it + 1}'}
)

// test_closures_with_n_args generates a new V file containing closures of `i`
// and parameters of type `typ`, to makes sure that all combinations work correctly
fn test_closures_with_n_args() ? {
	mut v_code := strings.new_builder(1024)
	// NB: the type or value of the captured arg doesn't matter for this test,
	// as the entire closure context is always passed as one pointer anyways

	for typ in ['byte', 'u16', 'int', 'i64', 'voidptr', 'string'] {
		for i in 0 .. max_params {
			param_names := all_param_names[..i]
			params := param_names.map('$it $typ')

			mut values := all_param_values[..i]
			if typ == 'string' {
				values = values.map("'$it'")
			}
			values = values.map('${typ}($it)')

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

			// NB: the captured arg doesn't matter for this test, as closures always receive
			// a pointer to the entire closure context as their last argument anyways
			v_code.writeln("
	fn test_big_closure_${typ}_${i}() {
		println('test_big_closure_${typ}_$i')
		mut z := $init_val
		c := fn [z] (${params.join(', ')}) $return_type {
			mut sum := z")
			for j in 0 .. i {
				v_code.writeln('\t\tsum += ${return_type}(${param_names[j]})')
			}
			v_code.writeln('
			return sum
		}
		assert c(${values.join(', ')}) == $expected_val
	}')
		}
	}

	code := v_code.str()
	println('Compiling V code (${code.count('\n')} lines) ...')
	wrkdir := os.join_path(os.temp_dir(), 'vtests', 'closures')
	os.mkdir_all(wrkdir) ?
	os.chdir(wrkdir) ?
	os.write_file('closure_args_test.v', code) ?
	vexe := os.getenv('VEXE')
	res := os.execute('$vexe closure_args_test.v')
	if res.exit_code != 0 {
		eprintln(res.output)
		assert false
	}
	println('Process exited with code $res.exit_code')

	os.chdir(os.dir(vexe)) or {}
	os.rmdir_all(wrkdir) or {}
}
