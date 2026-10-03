import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn test_makev_accepts_quoted_targets() {
	$if !windows {
		return
	}
	// os.exec quotes each argument; the batch target must not retain those quotes.
	result := os.exec([os.join_path(vroot, 'makev.bat'), 'help'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('makev.bat [target]'), result.output
}

fn test_vup_generates_windows_c_without_handle_type_errors() ! {
	test_root := os.join_path(os.vtmp_dir(), 'vup_windows_handles_${os.getpid()}')
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}
	// A test compiled by the compatibility compiler must still exercise the
	// default compiler, which checks C macros as integers.
	compiler := if os.base(vexe) == 'v1_fallback.exe' {
		os.join_path(vroot, 'v.exe')
	} else if os.base(vexe) == 'v1_fallback' {
		os.join_path(vroot, 'v')
	} else {
		vexe
	}
	source := os.join_path(vroot, 'cmd', 'tools', 'vup.v')
	for cc in ['msvc', 'gcc', 'tcc'] {
		c_file := os.join_path(test_root, 'vup_${cc}.c')
		// Generating C exercises the Windows checker without needing a Windows SDK.
		result := os.exec([compiler, '-new-compiler', '-g', '-gc', 'none', '-nocache', '-os', 'windows',
			'-arch', 'amd64', '-cc', cc, '-o', c_file, source])
		assert result.exit_code == 0, result.output
		assert os.is_file(c_file)
	}
}

fn write_executable(path string, content string) ! {
	os.write_file(path, content)!
	os.chmod(path, 0o755)!
}

fn test_vup_checks_primary_compiler_when_built_by_v1_fallback() ! {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'vup_primary_compiler_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}

	tool := os.join_path(test_root, 'vup')
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vup.v')])
	assert build.exit_code == 0, build.output

	git_refs := os.join_path(test_root, '.git', 'refs', 'heads')
	os.mkdir_all(git_refs)!
	os.write_file(os.join_path(test_root, '.git', 'HEAD'), 'ref: refs/heads/master\n')!
	os.write_file(os.join_path(git_refs, 'master'), 'abcdef0123456789abcdef0123456789abcdef01\n')!

	bin_dir := os.join_path(test_root, 'bin')
	os.mkdir_all(bin_dir)!
	log_file := os.join_path(test_root, 'compiler.log')
	write_executable(os.join_path(test_root, 'v'), '#!/bin/sh\n' + 'printf "%s\\n" "\$*" >> ${os.quoted_path(log_file)}\n' + 'if [ "\$1" = "version" ]; then\n' + '  echo "V 0.5.2 abcdef0"\n' + '  exit 0\n' + 'fi\n' + 'exit 1\n')!
	write_executable(os.join_path(test_root, 'v1_fallback'), '#!/bin/sh\n' + 'printf "fallback %s\\n" "\$*" >> ${os.quoted_path(log_file)}\n' + 'exit 1\n')!
	write_executable(os.join_path(bin_dir, 'git'), '#!/bin/sh\n' + 'if [ "\$1" = "pull" ]; then\n' + '  echo "Already up to date."\n' + 'fi\n' + 'exit 0\n')!
	write_executable(os.join_path(bin_dir, 'make'), '#!/bin/sh\nexit 0\n')!
	write_executable(os.join_path(bin_dir, 'gmake'), '#!/bin/sh\nexit 0\n')!

	path := '${bin_dir}:${os.getenv('PATH')}'
	result := os.exec(['env', 'PATH=' + '${path}',
		'VEXE=' + '${os.join_path(test_root, 'v1_fallback')}', '${tool}'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('V is already updated.'), result.output
	compiler_calls := os.read_file(log_file)!
	assert !compiler_calls.contains('fallback'), compiler_calls
	assert compiler_calls.trim_space().split_into_lines() == ['version', 'version'], compiler_calls
}

fn test_vup_restores_missing_primary_compiler_when_built_by_v1_fallback() ! {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'vup_missing_primary_compiler_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}

	// Give the test tool a known embedded hash so the fallback and checkout can
	// appear current while the primary compiler is missing.
	vup_source := os.read_file(os.join_path(vroot, 'cmd', 'tools', 'vup.v'))!
	assert vup_source.count('@VCURRENTHASH') == 1
	test_source := os.join_path(test_root, 'vup.v')
	os.write_file(test_source, vup_source.replace('@VCURRENTHASH', "'abcdef0'"))!
	tool := os.join_path(test_root, 'vup')
	build := os.exec([vexe, '-o', '${tool}', test_source])
	assert build.exit_code == 0, build.output

	git_refs := os.join_path(test_root, '.git', 'refs', 'heads')
	os.mkdir_all(git_refs)!
	os.write_file(os.join_path(test_root, '.git', 'HEAD'), 'ref: refs/heads/master\n')!
	os.write_file(os.join_path(git_refs, 'master'), 'abcdef0123456789abcdef0123456789abcdef01\n')!

	bin_dir := os.join_path(test_root, 'bin')
	os.mkdir_all(bin_dir)!
	make_log := os.join_path(test_root, 'make.log')
	fallback_log := os.join_path(test_root, 'fallback.log')
	write_executable(os.join_path(test_root, 'v1_fallback'), '#!/bin/sh\n' + 'printf "%s\\n" "\$*" >> ${os.quoted_path(fallback_log)}\n' + 'if [ "\$1" = "version" ]; then\n' + '  echo "V 0.5.2 abcdef0"\n' + '  exit 0\n' + 'fi\n' + 'exit 1\n')!
	write_executable(os.join_path(bin_dir, 'git'), '#!/bin/sh\n' + 'if [ "\$1" = "pull" ]; then\n' + '  echo "Already up to date."\n' + 'fi\n' + 'exit 0\n')!
	write_executable(os.join_path(bin_dir, 'make'), '#!/bin/sh\n' + 'printf "make:%s\\n" "\$*" >> ${os.quoted_path(make_log)}\n' + 'exit 0\n')!
	write_executable(os.join_path(bin_dir, 'gmake'), '#!/bin/sh\n' + 'printf "gmake:%s\\n" "\$*" >> ${os.quoted_path(make_log)}\n' + 'exit 0\n')!

	path := '${bin_dir}:${os.getenv('PATH')}'
	primary_vexe := os.join_path(os.real_path(test_root), 'v')
	result := os.exec(['env', 'PATH=' + '${path}',
		'VEXE=' + '${os.join_path(test_root, 'v1_fallback')}', '${tool}'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('`${primary_vexe}` is missing, trying `make` to restore it...'), result.output
	make_calls := os.read_file(make_log)!
	tcc_make_call := $if freebsd || openbsd || netbsd || dragonfly || solaris { 'gmake:latest_tcc' } $else { 'make:latest_tcc' }
	assert make_calls.trim_space().split_into_lines() == [tcc_make_call, 'make:'], make_calls
	assert !os.exists(fallback_log), os.read_file(fallback_log) or { '' }
}
