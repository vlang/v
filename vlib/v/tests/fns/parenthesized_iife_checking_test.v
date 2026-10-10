import os

fn iife_project() string {
	directory := os.join_path(os.vtmp_dir(), 'parenthesized_iife_${os.getpid()}')
	os.mkdir_all(os.join_path(directory, 'nums')) or { panic(err) }
	os.write_file(os.join_path(directory, 'v.mod'), "Module { name: 'iife' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(directory, 'nums', 'nums.v'), 'module nums\npub struct Number {\npub:\n width f64\n}\n') or {
		panic(err)
	}
	return directory
}

fn test_parenthesized_iife_checks_field_values_in_its_body() {
	directory := iife_project()
	defer { os.rmdir_all(directory) or {} }
	main_path := os.join_path(directory, 'main.v')
	module_path := [directory, '@vlib', '@vmodules'].join(os.path_delimiter)
	for value_type, value in {
		'bool':   'false'
		'string': "'12'"
	} {
		source := 'module main\nimport nums\nstruct App { checked ${value_type} }\nfn main() {\nmut app := &App{checked: ${value}}\n_ = ((fn [mut app] () f64 {\nreturn nums.Number{width: app.checked}.width\n}))()\n}\n'
		os.write_file(main_path, source) or { panic(err) }
		for options in [[]string{}, ['-check']] {
			result := os.exec([@VEXE, '-b', 'c', '-path', module_path, ...options, '-o',
				os.join_path(directory, 'invalid'), main_path])
			assert result.exit_code != 0, result.output
			assert result.output.contains('main.v:7:20: error: cannot assign to field `width`: expected `f64`, not `${value_type}`'), result.output
		}
	}
}

fn test_parenthesized_iife_preserves_valid_captures_arguments_and_returns() {
	directory := iife_project()
	defer { os.rmdir_all(directory) or {} }
	main_path := os.join_path(directory, 'main.v')
	module_path := [directory, '@vlib', '@vmodules'].join(os.path_delimiter)
	os.write_file(main_path, 'module main\nimport nums\nstruct App {\nmut:\n count int\n}\nfn main() {\nmut app := &App{count: 12}\nvalue := ((fn [mut app] (offset int) f64 {\napp.count += offset\nreturn nums.Number{width: app.count}.width\n}))(3)\nassert value == 15.0\nassert app.count == 15\n}\n') or { panic(err) }
	checked := os.exec([@VEXE, '-b', 'c', '-check', '-path', module_path, main_path])
	assert checked.exit_code == 0, checked.output
	result := os.exec([@VEXE, '-b', 'c', '-path', module_path, 'run', main_path])
	assert result.exit_code == 0, result.output
}
