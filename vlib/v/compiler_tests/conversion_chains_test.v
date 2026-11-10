import os

fn test_temporary_conversion_chains() {
	dir := os.join_path(os.vtmp_dir(), 'conversion_chains_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.setenv('V3CACHE', os.join_path(dir, 'cache'), true)
	os.setenv('V3_CACHE_DISABLE_PROGRAM_EXECUTABLE', '1', true)
	os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', '1', true)
	source := os.join_path(dir, 'main.v')
	os.cp(os.join_path(@VMODROOT, 'vlib/v/compiler_tests/testdata/conversion_chains.vv'), source)!
	output := os.join_path(dir, 'main.c')
	generated := os.exec([@VEXE, '-nocache', '-o', output, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(output)!
	for name in ['fused_builder', 'fused_bytes'] {
		body := code.all_after('Array ${name}(').all_after('Array ${name}(').all_before('\n}')
		assert body.contains('__new_array_noscan('), body
		assert !body.contains('string__bytes('), body
		assert !body.contains('strings__Builder__str('), body
		assert !body.contains('Array_u8__bytestr('), body
	}
	// Repeat the default mode to exercise both a cold and a warm module cache.
	for flags in [[]string{}, []string{}, ['-nocache', '-gc', 'none'], ['-nocache', '-prod'],
		['-nocache', '-autofree']] {
		mut args := [@VEXE]
		args << flags
		args << ['run', source]
		result := os.exec(args)
		assert result.exit_code == 0, '${args}: ${result.output}'
		assert result.output.trim_space() == 'ok', result.output
	}
}
