import os
import json2

const vls_test_vexe = @VEXE
const vls_test_vroot = os.dir(vls_test_vexe)

fn test_vls_source_update_recovers_missing_executable() ! {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'vls source recovery ${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}

	home_dir := os.join_path(test_root, 'home with spaces')
	source_dir := os.join_path(home_dir, '.vls', 'src')
	os.mkdir_all(source_dir)!

	tool_path := os.join_path(test_root, 'vls updater')
	tool_source := os.join_path(vls_test_vroot, 'cmd', 'tools', 'vls.v')
	build_result := os.exec([vls_test_vexe, '-o', tool_path, tool_source])
	assert build_result.exit_code == 0, build_result.output

	compile_log := os.join_path(test_root, 'compile arguments')
	fake_vexe := os.join_path(test_root, 'fake v')
	os.write_file(fake_vexe, '#!/bin/sh\n' + 'if [ "\$1" = "retry" ]; then\n' + '  echo "Already up to date."\n' + '  exit 0\n' + 'fi\n' + 'printf "%s\\n" "\$@" > "\$VLS_TEST_COMPILE_LOG"\n' + 'output=\n' + 'while [ "\$#" -gt 0 ]; do\n' + '  if [ "\$1" = "-o" ]; then\n' + '    shift\n' + '    output="\$1"\n' + '  fi\n' + '  shift\n' + 'done\n' + 'printf "%s\\n" "#!/bin/sh" "echo vls version test" > "\$output"\n' + 'chmod +x "\$output"\n')!
	os.chmod(fake_vexe, 0o755)!

	result := os.exec(['env', 'HOME=' + '${home_dir}', 'VEXE=' + '${fake_vexe}',
		'VLS_TEST_COMPILE_LOG=' + '${compile_log}', tool_path, '--update', '--source'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('Compiling VLS from source...'), result.output

	exec_path := os.join_path(source_dir, 'bin', 'vls')
	assert os.is_executable(exec_path)
	compile_args := os.read_file(compile_log)!.split_into_lines()
	assert compile_args.len == 5, compile_args.str()
	assert compile_args[0] == '-cc'
	assert compile_args[1] in ['cc', 'gcc', 'clang', 'msvc']
	assert compile_args[2..] == ['-o', exec_path, source_dir]
	manifest := os.read_file(os.join_path(home_dir, '.vls', 'vls.config.json'))!
	assert manifest.contains(exec_path)
}

fn test_vls_check_without_installation_reports_state() ! {
	// --check must report the state and exit 0, not raise error 101
	// with a backtrace because parse() forced language server mode
	test_root := os.join_path(os.vtmp_dir(), 'vls check ${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}
	tool_source := os.join_path(vls_test_vroot, 'cmd', 'tools', 'vls.v')
	tool_exe := $if windows { os.join_path(test_root, 'vls_check.exe') } $else { os.join_path(test_root, 'vls_check') }
	build_result := os.exec([vls_test_vexe, '-o', tool_exe, tool_source])
	assert build_result.exit_code == 0, build_result.output
	empty_home := os.join_path(test_root, 'empty home')
	os.mkdir_all(empty_home)!
	home_var := $if windows { 'USERPROFILE' } $else { 'HOME' }
	old_home := os.getenv(home_var)
	os.setenv(home_var, empty_home, true)
	defer {
		os.setenv(home_var, old_home, true)
	}
	result := os.exec([tool_exe, '--check'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('not installed'), result.output
}

fn test_vls_check_reports_manifest_and_stale_installations() ! {
	test_root := os.join_path(os.vtmp_dir(), 'vls installed check ${os.getpid()}')
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}
	tool_source := os.join_path(vls_test_vroot, 'cmd', 'tools', 'vls.v')
	tool_exe := os.join_path(test_root, 'vls_check' + $if windows { '.exe' } $else { '' })
	build_result := os.exec([vls_test_vexe, '-o', tool_exe, tool_source])
	assert build_result.exit_code == 0, build_result.output
	vls_home := os.join_path(test_root, 'home')
	os.mkdir_all(os.join_path(vls_home, '.vls'))!
	home_var := $if windows { 'USERPROFILE' } $else { 'HOME' }
	old_home := os.getenv(home_var)
	os.setenv(home_var, vls_home, true)
	defer {
		os.setenv(home_var, old_home, true)
	}
	manifest_path := os.join_path(vls_home, '.vls', 'vls.config.json')
	// The updater itself is an executable fixture; --check must not launch it.
	os.write_file(manifest_path, '{"server_path":' + json2.encode(tool_exe, escape_unicode: true, time_as_unix: true) + '}')!
	result := os.exec([tool_exe, '--check'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('installed at: ${tool_exe}'), result.output
	os.write_file(manifest_path, '{"server_path":' + json2.encode(tool_exe + '.missing', escape_unicode: true, time_as_unix: true) + '}')!
	stale := os.exec([tool_exe, '--check'])
	assert stale.exit_code == 0, stale.output
	assert stale.output.contains('not installed'), stale.output
}
