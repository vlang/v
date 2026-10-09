import os

fn test_comptime_type_conditions_use_local_field_value_types() {
	directory := os.join_path(os.vtmp_dir(), 'comptime_local_type_${os.getpid()}')
	os.mkdir_all(directory) or { panic(err) }
	defer { os.rmdir_all(directory) or {} }
	main_path := os.join_path(directory, 'main.v')
	for value_type, value in {
		'string': "'name'"
		'bool':   'true'
		'int':    '12'
		'f64':    '1.5'
	} {
		other_type := if value_type == 'string' { 'bool' } else { 'string' }
		group_check := if value_type in ['int', 'f64'] {
			group := if value_type == 'int' { r'$int' } else { r'$float' }
			'\n$if local !is ${group} { $compile_error("lost numeric type group") }\n'
		} else {
			''
		}
		body := 'local := app.value\n$if local !is ${value_type} { $compile_error("lost local type") }\n$if local is ${other_type} { $compile_error("wrong local type") }\n$if local is ${value_type} {} $else { $compile_error("missing local type") }\n$if local !is ${other_type} {} $else { $compile_error("unexpected local type") }\n' + group_check
		for closure in [false, true] {
			statements := if closure {
				'read := fn [app] () ${value_type} {\n${body}return local\n}\nassert read() == app.value\n'
			} else {
				body + 'assert local == app.value\n'
			}
			source := 'module main\nstruct App { value ${value_type} }\nfn main() {\napp := &App{value: ${value}}\n${statements}}\n'
			os.write_file(main_path, source) or { panic(err) }
			checked := os.exec([@VEXE, '-b', 'c', '-check', main_path])
			assert checked.exit_code == 0, checked.output
			result := os.exec([@VEXE, '-b', 'c', 'run', main_path])
			assert result.exit_code == 0, result.output
		}
	}
}
