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

fn test_explicit_test_outputs_are_preserved() {
	root := binary_cleanup_root('explicit')!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'sample_test.v')
	marker := os.join_path(root, 'binary_path')
	binary := os.join_path(root, 'chosen output' + $if windows { '.exe' } $else { '' })
	os.write_file(source, binary_cleanup_source('assert true'))!
	build := binary_cleanup_compile(root, ['-skip-running', '-o', binary, source])
	assert build.exit_code == 0, build.output
	assert !os.exists(marker)
	assert os.is_file(binary)
	run := cmdexec.run_in(binary, [], root)
	assert run.exit_code == 0, run.output
	assert os.read_file(marker)!.trim_space() == os.real_path(binary)
	os.rm(marker)!
	os.write_file(source, binary_cleanup_source('assert false'))!
	run_with_output := binary_cleanup_compile(root, ['-stats', '-o', binary, source])
	assert run_with_output.exit_code != 0, run_with_output.output
	assert os.read_file(marker)!.trim_space() == os.real_path(binary)
	assert os.is_file(binary)
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
