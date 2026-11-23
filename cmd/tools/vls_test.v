import os
import json2

const vls_test_vexe = @VEXE
const vls_test_vroot = os.dir(vls_test_vexe)
const vls_test_tool_dir = os.join_path(os.vtmp_dir(), 'vls updater ${os.getpid()}')
const vls_test_tool = os.join_path(vls_test_tool_dir, 'vls_check' + $if windows {
	'.exe'
} $else {
	''
})

fn testsuite_begin() {
	os.rmdir_all(vls_test_tool_dir) or {}
	os.mkdir_all(vls_test_tool_dir)!
	source := os.join_path(vls_test_vroot, 'cmd', 'tools', 'vls.v')
	build := os.exec([vls_test_vexe, '-o', vls_test_tool, source])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(vls_test_tool_dir) or {}
}

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

	compile_log := os.join_path(test_root, 'compile arguments')
	fake_vexe := os.join_path(test_root, 'fake v')
	os.write_file(fake_vexe, '#!/bin/sh\n' + 'if [ "\$1" = "retry" ]; then\n' + '  echo "Already up to date."\n' + '  exit 0\n' + 'fi\n' + 'printf "%s\\n" "\$@" > "\$VLS_TEST_COMPILE_LOG"\n' + 'output=\n' + 'while [ "\$#" -gt 0 ]; do\n' + '  if [ "\$1" = "-o" ]; then\n' + '    shift\n' + '    output="\$1"\n' + '  fi\n' + '  shift\n' + 'done\n' + 'printf "%s\\n" "#!/bin/sh" "echo vls version test" > "\$output"\n' + 'chmod +x "\$output"\n')!
	os.chmod(fake_vexe, 0o755)!

	result := os.exec(['env', 'HOME=' + '${home_dir}', 'VEXE=' + '${fake_vexe}',
		'VLS_TEST_COMPILE_LOG=' + '${compile_log}', vls_test_tool, '--update', '--source'])
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
	empty_home := os.join_path(test_root, 'empty home')
	os.mkdir_all(empty_home)!
	home_var := $if windows { 'USERPROFILE' } $else { 'HOME' }
	old_home := os.getenv(home_var)
	os.setenv(home_var, empty_home, true)
	defer {
		os.setenv(home_var, old_home, true)
	}
	result := os.exec([vls_test_tool, '--check'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('not installed'), result.output
}

fn test_vls_check_reports_manifest_and_stale_installations() ! {
	test_root := os.join_path(os.vtmp_dir(), 'vls installed check ${os.getpid()}')
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}
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
	os.write_file(manifest_path, '{"server_path":' + json2.encode(vls_test_tool,
		escape_unicode: true
		time_as_unix:   true
	) + '}')!
	result := os.exec([vls_test_tool, '--check'])
	assert result.exit_code == 0, result.output
	// --check escapes backslashes in the displayed path, including in text mode.
	displayed_tool_exe := vls_test_tool.replace(r'\', r'\\')
	assert result.output.contains('installed at: ${displayed_tool_exe}'), result.output
	os.write_file(manifest_path, '{"server_path":' + json2.encode(vls_test_tool + '.missing',
		escape_unicode: true
		time_as_unix:   true
	) + '}')!
	stale := os.exec([vls_test_tool, '--check'])
	assert stale.exit_code == 0, stale.output
	assert stale.output.contains('not installed'), stale.output
}
