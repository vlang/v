import os

fn bytes_lowering_function(source string, name string) string {
	marker := '${name}('
	for line in source.split_into_lines() {
		if line.len > 0 && !line[0].is_space() && line.contains(marker) && line.ends_with(' {') {
			return source.all_after(line).all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}

fn test_string_bytes_lowering_preserves_snapshots_and_effects() {
	root := os.join_path(os.vtmp_dir(), 'string_bytes_lowering_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(os.dir(@FILE), 'testdata', 'string_bytes_lowering.vv'), source)!
	generated := os.join_path(root, 'main.c')
	codegen := os.exec([@VEXE, '-new-compiler', '-nocache', '-o', generated, source])
	assert codegen.exit_code == 0, codegen.output
	c := os.read_file(generated)!
	for name in ['byte_sum', 'byte_find', 'byte_control', 'literal_loop', 'small_set', 'large_set',
		'not_set', 'empty_set', 'contains_set', 'wide_small', 'wide_large'] {
		body := bytes_lowering_function(c, name)
		assert !body.contains('string__bytes('), '${name}: ${body}'
	}
	assert bytes_lowering_function(c, 'large_set').contains('string__contains_u8(')
	for name in ['snapshot_loop', 'callback_loop', 'mutable_loop', 'scalar_alias_loop',
		'local_alias_loop', 'unsafe_function_loop'] {
		assert bytes_lowering_function(c, name).contains('string__bytes('), name
	}
	for mode in ['', '-prod', '-autofree'] {
		binary := os.join_path(root, 'program' + $if windows { '.exe' } $else { '' })
		compiler := if mode == '-prod' && @CCOMPILER in ['tinyc', 'tcc'] {
			'cc'
		} else {
			@CCOMPILER
		}
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-cc', compiler]
		if mode != '' { args << mode }
		args << ['-o', binary, source]
		compiled := os.exec(args)
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		ran := os.exec([binary])
		assert ran.exit_code == 0, '${mode}: ${ran.output}'
		assert ran.output.trim_space() == 'ok', ran.output
	}
}
