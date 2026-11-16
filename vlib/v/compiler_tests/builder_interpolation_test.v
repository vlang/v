import os

fn test_builder_interpolation_codegen_and_runtime() {
	dir := os.join_path(os.vtmp_dir(), 'builder_interpolation_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	os.cp(os.join_path(@VMODROOT, 'vlib/v/compiler_tests/testdata/builder_interpolation.vv'), source)!
	output := os.join_path(dir, 'main.c')
	generated := os.exec([@VEXE, '-nocache', '-o', output, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(output)!
	line := code.all_after('void append_line(').all_after('void append_line(').all_before('\n}')
	assert line.contains('v3_i64_buf('), line
	assert line.contains('v3_builder_parts_alias('), line
	assert !line.contains('int__str('), line
	assert code.contains('v3_f64_exp_buf(')
	assert code.contains('v3_numeric_pad_buf(')
	for flags in [[]string{}, ['-gc', 'none'], ['-prod'], ['-autofree']] {
		mut args := [@VEXE, '-nocache']
		args << flags
		args << ['run', source]
		result := os.exec(args)
		assert result.exit_code == 0, '${args}: ${result.output}'
		assert result.output.trim_space() == 'ok', result.output
	}
}
