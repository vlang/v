module driver

import os

const missing_test_function_message = 'a _test.v file should have *at least* one `test_` function'

struct MissingTestFunctionInput {
	name   string
	source string
	line   int = 1
}

fn test_test_files_without_active_functions_report_located_errors() {
	root := os.join_path(os.vtmp_dir(), 'v3_missing_test_function_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	inputs := [
		MissingTestFunctionInput{ name: 'empty', source: '' },
		MissingTestFunctionInput{ name: 'module_only', source: 'module main\n' },
		MissingTestFunctionInput{ name: 'type_only', source: 'module main\n\ntype Count = int\n' },
		MissingTestFunctionInput{ name: 'const_only', source: 'module main\n\nconst test_count = 1\n' },
		MissingTestFunctionInput{
			name:   'excluded'
			source: 'module main\n\n\$if missing_test_function_enabled ? {\n\tfn test_enabled() { assert true }\n}\n'
		},
		MissingTestFunctionInput{ name: 'helper', source: 'module main\n\nfn helper() {}\n', line: 3 },
	]
	for input in inputs {
		path := os.join_path(root, '${input.name}_test.v')
		os.write_file(path, input.source)!
		for serial in [false, true] {
			mut flags := []string{}
			if serial {
				flags << '-no-parallel'
			}
			output := os.join_path(root, '${input.name}_${serial}')
			result := os.exec([@VEXE, '-new-compiler', '-nocache', '-no-retry-compilation', '-nocolor',
				...flags, '-o', output, path])
			assert result.exit_code == 1, result.output
			location := '${os.real_path(path)}:${input.line}:1: error: '
			assert result.output.contains(location + missing_test_function_message), result.output
			assert result.output.count(missing_test_function_message) == 1, result.output
			assert !result.output.contains('V panic:'), result.output
		}
	}
}

fn test_active_test_functions_execute_and_keep_signature_validation() {
	root := os.join_path(os.vtmp_dir(), 'v3_active_test_function_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	active := os.join_path(root, 'active_test.v')
	os.write_file(active, 'module main

fn test_plain() {
	println("plain executed")
	assert 2 + 2 == 4
}

fn test_option() ? {
	println("option executed")
	assert true
}

fn test_result() ! {
	println("result executed")
	assert true
}
')!
	filtered := os.join_path(root, 'filtered_test.v')
	os.write_file(filtered, 'module main

\$if missing_test_function_enabled ? {
	fn test_enabled() {
		println("enabled executed")
		assert true
	}
}
')!
	for serial in [false, true] {
		mut flags := []string{}
		if serial {
			flags << '-no-parallel'
		}
		for path in [active, filtered] {
			mut defines := []string{}
			if path == filtered {
				defines << ['-d', 'missing_test_function_enabled']
			}
			output := os.join_path(root, '${os.file_name(path).all_before_last('.v')}_${serial}')
			build := os.exec([@VEXE, '-new-compiler', '-nocache', '-no-retry-compilation', '-nocolor',
				...flags, ...defines, '-o', output, path])
			assert build.exit_code == 0, build.output
			run := os.exec([output])
			assert run.exit_code == 0, run.output
			if path == active {
				for marker in ['plain executed', 'option executed', 'result executed'] {
					assert run.output.contains(marker), run.output
				}
			} else {
				assert run.output.contains('enabled executed'), run.output
			}
		}
	}
	invalid := os.join_path(root, 'invalid_test.v')
	for input in [
		MissingTestFunctionInput{
			name:   'test functions should take 0 parameters'
			source: 'module main\n\nfn test_bad(value int) {}\n'
		},
		MissingTestFunctionInput{
			name:   'test functions should either return nothing at all, or be marked to return `?` or `!`'
			source: 'module main\n\nfn test_bad() int { return 1 }\n'
		},
	] {
		os.write_file(invalid, input.source)!
		result := os.exec([@VEXE, '-new-compiler', '-nocache', '-no-retry-compilation', '-nocolor',
			'-check', invalid])
		assert result.exit_code == 1, result.output
		assert result.output.contains('${os.real_path(invalid)}:3:1: error: invalid test signature: ${input.name}'), result.output
		assert !result.output.contains(missing_test_function_message), result.output
		assert !result.output.contains('V panic:'), result.output
	}
}
