import os

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
	build_result := os.execute('${os.quoted_path(vls_test_vexe)} -o ${os.quoted_path(tool_path)} ${os.quoted_path(tool_source)}')
	assert build_result.exit_code == 0, build_result.output

	compile_log := os.join_path(test_root, 'compile arguments')
	fake_vexe := os.join_path(test_root, 'fake v')
	os.write_file(fake_vexe, '#!/bin/sh\n' + 'if [ "\$1" = "retry" ]; then\n' + '  echo "Already up to date."\n' + '  exit 0\n' + 'fi\n' + 'printf "%s\\n" "\$@" > "\$VLS_TEST_COMPILE_LOG"\n' + 'output=\n' + 'while [ "\$#" -gt 0 ]; do\n' + '  if [ "\$1" = "-o" ]; then\n' + '    shift\n' + '    output="\$1"\n' + '  fi\n' + '  shift\n' + 'done\n' + 'printf "%s\\n" "#!/bin/sh" "echo vls version test" > "\$output"\n' + 'chmod +x "\$output"\n')!
	os.chmod(fake_vexe, 0o755)!

	result := os.execute('HOME=${os.quoted_path(home_dir)} VEXE=${os.quoted_path(fake_vexe)} VLS_TEST_COMPILE_LOG=${os.quoted_path(compile_log)} ${os.quoted_path(tool_path)} --update --source')
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
