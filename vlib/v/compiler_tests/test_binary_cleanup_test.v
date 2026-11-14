import os
import v.cmdexec

fn binary_cleanup_root(name string) !string {
	root := os.join_path(os.vtmp_dir(), 'test binary cleanup ${name} ${os.getpid()}')
	os.mkdir_all(root)!
	return root
}

fn binary_cleanup_source(body string) string {
	return 'import os
fn test_cleanup() {
	os.write_file("binary_path", os.executable())!
	assert os.is_file(os.executable())
	${body}
}
'
}

fn binary_cleanup_compile(root string, args []string) os.Result {
	old_vtmp := os.getenv_opt('VTMP')
	os.setenv('VTMP', root, true)
	defer {
		if value := old_vtmp {
			os.setenv('VTMP', value, true)
		} else {
			os.unsetenv('VTMP')
		}
	}
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	cc := $if windows { 'gcc' } $else { 'cc' }
	mut flags := ['-new-compiler', '-no-retry-compilation', '-memory-limit', '10G', '-cc', cc]
	flags << args
	return cmdexec.run_in(vexe, flags, root)
}

fn test_implicit_test_binaries_are_removed_after_success_and_failure() {
	root := binary_cleanup_root('exit')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'sample_test.v')
	marker := os.join_path(root, 'binary_path')
	for body in ['assert true', 'assert false', 'panic("cleanup panic")', 'exit(23)'] {
		text := binary_cleanup_source(body)
		os.write_file(source, text)!
		result := binary_cleanup_compile(root, ['sample_test.v'])
		if body == 'assert true' {
			assert result.exit_code == 0, result.output
		} else if body == 'exit(23)' {
			assert result.exit_code == 23, result.output
		} else {
			assert result.exit_code != 0, result.output
		}
		binary := os.read_file(marker)!.trim_space()
		assert !os.exists(binary), 'test binary remains after ${body}: ${binary}'
		assert !os.exists(binary + '.dSYM')
		assert os.read_file(source)! == text
		os.rm(marker)!
	}
}

fn test_implicit_test_binary_cleanup_with_run_stats_keepc_and_debug() {
	root := binary_cleanup_root('flags')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'sample_test.v')
	marker := os.join_path(root, 'binary_path')
	os.write_file(source, binary_cleanup_source('assert false'))!
	for flags in [['-stats'], ['-keepc'], ['-g'], ['run'], ['-keepc', 'run'], ['crun']] {
		c_files_before := os.ls(root)!.filter(it.ends_with('.tmp.c')).len
		// A previous implicit test executable is still a compiler-owned artifact.
		os.write_file(os.join_path(root, 'sample_test' + $if windows { '.exe' } $else { '' }),
			'old test artifact')!
		mut args := flags.clone()
		args << 'sample_test.v'
		result := binary_cleanup_compile(root, args)
		assert result.exit_code != 0, result.output
		binary := os.read_file(marker)!.trim_space()
		assert !os.exists(binary), 'test binary remains with ${flags}: ${binary}'
		assert !os.exists(binary + '.dSYM')
		if '-keepc' in flags {
			assert os.ls(root)!.filter(it.ends_with('.tmp.c')).len > c_files_before
		}
		os.rm(marker)!
	}
}

fn test_compile_only_test_outputs_are_preserved() {
	root := binary_cleanup_root('explicit')!
	old_show_asserts := os.getenv_opt('VTEST_SHOW_ASSERTS')
	defer {
		if value := old_show_asserts {
			os.setenv('VTEST_SHOW_ASSERTS', value, true)
		} else {
			os.unsetenv('VTEST_SHOW_ASSERTS')
		}
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'sample_test.v')
	marker := os.join_path(root, 'binary_path')
	binary := os.join_path(root, 'chosen output' + $if windows { '.exe' } $else { '' })
	os.write_file(source, binary_cleanup_source('assert true'))!
	for show_asserts in [false, true] {
		if show_asserts {
			os.setenv('VTEST_SHOW_ASSERTS', '1', true)
		} else {
			os.unsetenv('VTEST_SHOW_ASSERTS')
		}
		for flags in [[]string{}, ['-stats'], ['-stats', 'run'], ['-stats', 'crun']] {
			mut args := ['-skip-running', '-o', binary]
			args << flags
			args << source
			build := binary_cleanup_compile(root, args)
			assert build.exit_code == 0, build.output
			assert !os.exists(marker), 'compile-only test ran with ${flags}, show_asserts=${show_asserts}'
			assert os.is_file(binary)
			run := cmdexec.run_in(binary, [], root)
			assert run.exit_code == 0, run.output
			assert os.read_file(marker)!.trim_space() == os.real_path(binary)
			os.rm(marker)!
		}
	}
}

fn test_directory_test_command_removes_implicit_binary() {
	root := binary_cleanup_root('directory')!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'sample_test.v'), binary_cleanup_source('assert true'))!
	result := binary_cleanup_compile(root, ['test', root])
	assert result.exit_code == 0, result.output
	binary := os.read_file(os.join_path(root, 'binary_path'))!.trim_space()
	assert !os.exists(binary), 'directory test binary remains: ${binary}'
}

fn test_executed_explicit_test_outputs_are_removed() {
	root := binary_cleanup_root('explicit run')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'sample_test.v')
	marker := os.join_path(root, 'binary_path')
	binary := os.join_path(root, 'chosen output' + $if windows { '.exe' } $else { '' })
	for flags in [['-stats'], ['run'], ['-keepc', 'run'], ['crun']] {
		for body in ['assert true', 'assert false'] {
			os.write_file(source, binary_cleanup_source(body))!
			mut args := ['-o', binary]
			args << flags
			args << source
			result := binary_cleanup_compile(root, args)
			assert (result.exit_code == 0) == (body == 'assert true'), result.output
			assert os.read_file(marker)!.trim_space() == os.join_path(os.real_path(root),
				os.file_name(binary))
			assert !os.exists(binary), 'explicit test binary remains with ${flags}: ${binary}'
			assert !os.exists(binary + '.dSYM')
			os.rm(marker)!
		}
	}
}

fn test_kept_test_sessions_remove_binaries_after_success_and_failure() {
	root := binary_cleanup_root('kept session')!
	old_keep_session := os.getenv_opt('VTEST_KEEP_SESSION')
	os.setenv('VTEST_KEEP_SESSION', '1', true)
	defer {
		if value := old_keep_session {
			os.setenv('VTEST_KEEP_SESSION', value, true)
		} else {
			os.unsetenv('VTEST_KEEP_SESSION')
		}
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'sample_test.v')
	marker := os.join_path(root, 'binary_path')
	for flags in [[]string{}, ['-stats', '-g']] {
		for body in ['assert true', 'assert false', 'panic("cleanup panic")', 'exit(23)'] {
			os.write_file(source, binary_cleanup_source(body))!
			mut args := flags.clone()
			args << ['test', source]
			result := binary_cleanup_compile(root, args)
			assert (result.exit_code == 0) == (body == 'assert true'), result.output
			binary := os.read_file(marker)!.trim_space()
			assert !os.exists(binary), 'kept session retains test binary: ${binary}'
			assert !os.exists(binary + '.dSYM')
			assert os.is_dir(os.dir(binary)), 'session artifacts should remain'
			os.rm(marker)!
		}
	}
}
