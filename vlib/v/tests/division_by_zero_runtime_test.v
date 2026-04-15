import os

const vexe = @VEXE
const vtmp_folder = os.join_path(os.vtmp_dir(), 'division_by_zero_runtime_tests')

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

fn test_indirect_integer_division_by_zero_panics_in_v() {
	output :=
		compile_and_run_program('fn divide(a int, b int) int {\n\treturn a / b\n}\n\nfn main() {\n\tprintln(divide(1, 0))\n}\n')
	assert_valid_panic_output(output, 'division by zero')
}

fn test_indirect_integer_modulo_by_zero_panics_in_v() {
	output :=
		compile_and_run_program('fn modulo(a int, b int) int {\n\treturn a % b\n}\n\nfn main() {\n\tprintln(modulo(3, 0))\n}\n')
	assert_valid_panic_output(output, 'modulo by zero')
}

fn assert_valid_panic_output(output string, message string) {
	normalized := output.replace('\r', '')
	assert normalized.contains('V panic: ${message}'), normalized
	assert !normalized.to_lower().contains('runtime error:'), normalized
}

fn compile_and_run_program(source string) string {
	os.mkdir_all(vtmp_folder) or {}
	defer {
		os.rmdir_all(vtmp_folder) or {}
	}
	source_path := os.join_path(vtmp_folder, 'division_by_zero_program.v')
	exe_path := os.join_path(vtmp_folder, 'division_by_zero_test${exe_suffix()}')
	os.write_file(source_path, source) or { panic(err) }
	compile_cmd := '${os.quoted_path(vexe)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	compilation := os.execute(compile_cmd)
	assert compilation.exit_code == 0, 'compilation failed: ${compilation.output}'
	result := os.execute(os.quoted_path(exe_path))
	assert result.exit_code != 0, 'program unexpectedly succeeded'
	return result.output
}

fn exe_suffix() string {
	return if os.user_os() == 'windows' { '.exe' } else { '' }
}
