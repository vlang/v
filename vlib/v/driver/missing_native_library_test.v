module driver

import os
import v.cmdexec

fn test_missing_native_library_checks_only_positional_library_files() {
	root := os.join_path(os.vtmp_dir(), 'v native library flags ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	for suffix in ['.a', '.so', '.so.1', '.dylib', '.dll', '.lib', '.tbd'] {
		path := os.join_path(root, 'native' + suffix)
		assert v3_missing_c_link_library([path], false) or { '' } == path
		for option in ['-I', '-L', '-F', '-D', '-U', '-include', '-isystem', '-isysroot', '-o',
			'-x', '-l', '-Xlinker'] {
			assert v3_missing_c_link_library([option, path], false) == none
		}
		for prefix in ['-I', '-L', '-DNAME=', '-Wl,'] {
			assert v3_missing_c_link_library([prefix + path], false) == none
		}
		os.write_file(path, 'existing library')!
		assert v3_missing_c_link_library([path], false) == none
	}
	assert v3_missing_c_link_library(['-lmissing', '-Lmissing', 'missing.c', 'missing.o'], false) == none
}

fn test_native_library_checks_preserve_msvc_names_and_options() {
	assert v3_missing_c_link_library(['kernel32.lib'], true) == none
	assert v3_missing_c_link_library(['kernel32.lib'], false) == none
	assert v3_missing_c_link_library(['/OUT:program.dll', '/Iinclude.lib'], true) == none
	assert v3_missing_c_link_library(['C:/v/missing.lib'], true) or { '' } == 'C:/v/missing.lib'
}

fn test_missing_native_library_reports_the_required_file() {
	root := os.join_path(os.vtmp_dir(), 'v missing native library ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'main.c.v')
	output := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	os.write_file(source, 'module main\n#flag "@DIR/libmissing.a"\nfn main() {}\n')!
	compiler := $if windows { 'gcc' } $else { 'cc' }
	result := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-gc', 'none', '-cc', compiler,
		'-o', output, source], root, {
		'VFLAGS':  ''
		'VOSARGS': ''
	})
	assert result.exit_code == 1, result.output
	assert result.output.contains('C library file `'), result.output
	assert result.output.contains('libmissing.a` was not found.'), result.output
	assert result.output.contains('Build or restore this native library'), result.output
	assert !os.exists(output)
	generated_c := os.join_path(root, 'main.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-o', generated_c, source])
	assert generated.exit_code == 0, generated.output
	assert os.is_file(generated_c)
	generated_object := os.join_path(root, 'main.o')
	object := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-cc', compiler, '-o', generated_object,
		source])
	assert object.exit_code == 0, object.output
	assert os.is_file(generated_object)
	os.write_file(source, 'module main\nfn main() {}\n')!
	missing := os.join_path(root, 'another missing.a')
	for option in ['-cflags', '-ldflags', 'CFLAGS'] {
		mut args := ['-nocache', '-gc', 'none', '-cc', compiler, '-o', output]
		mut environment := {
			'VFLAGS':  ''
			'VOSARGS': ''
		}
		if option == 'CFLAGS' {
			environment['CFLAGS'] = '"${missing}"'
		} else {
			args << [option, '"${missing}"']
		}
		args << source
		failed := cmdexec.run_in_merged_with_env(@VEXE, args, root, environment)
		assert failed.exit_code == 1, failed.output
		assert failed.output.contains('C library file `${missing}` was not found.'), failed.output
	}
}
