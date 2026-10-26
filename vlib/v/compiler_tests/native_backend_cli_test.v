module main

import os
import v.cmdexec

fn assert_native_backend_migration(args []string) {
	result := cmdexec.run(@VEXE, args)
	assert result.exit_code == 1, result.output
	assert result.output.count('`-b native` has been replaced by `-b arm64`') == 1, result.output
	assert result.output.contains('`v -compile-backend arm64 self`'), result.output
	assert result.output.contains('`v -b arm64 -gc none file.v`'), result.output
	assert result.output.contains('native AMD64 backend is not available yet'), result.output
	assert result.output.contains('`v -b c -arch amd64 file.v`'), result.output
}

fn test_native_backend_migration_precedes_input_and_dispatch_errors() {
	missing := os.join_path(os.vtmp_dir(), 'missing_native_input_${os.getpid()}.v')
	for backend_args in [
		['-b', 'native'],
		['-backend', 'native'],
		['-b=native'],
		['-backend=native'],
	] {
		for suffix in [
			[]string{},
			[missing],
			['help', 'build-c'],
		] {
			mut args := backend_args.clone()
			args << suffix
			assert_native_backend_migration(args)
		}
		mut fmt_args := backend_args.clone()
		fmt_args << ['fmt', missing]
		fmt_result := cmdexec.run(@VEXE, fmt_args)
		assert fmt_result.exit_code == 1, fmt_result.output
		assert fmt_result.output.contains('vfmt error:'), fmt_result.output
		assert fmt_result.output.contains('does not exist'), fmt_result.output
		assert !fmt_result.output.contains('has been replaced'), fmt_result.output
	}
	assert_native_backend_migration(['-old-compiler', '-backend', 'native', 'help'])
	assert_native_backend_migration(['-b', 'c', '-backend=native', 'help'])
	assert_native_backend_migration(['-b', 'native', '-compile-backend', 'c', 'help'])
}

fn test_native_backend_migration_uses_the_last_selected_target_backend() {
	result := cmdexec.run(@VEXE, ['-b', 'native', '-backend', 'c', 'help'])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('has been replaced'), result.output
}

fn test_native_backend_migration_applies_to_vflags() {
	previous := os.getenv_opt('VFLAGS')
	os.setenv('VFLAGS', '-b native', true)
	defer {
		if value := previous {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
	}
	assert_native_backend_migration(['help'])
}

fn test_native_backend_spelling_in_completion_arguments() {
	result := cmdexec.run(@VEXE, ['complete', 'bash', 'v', '-b', 'native'])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('has been replaced'), result.output
}

fn test_native_backend_spelling_in_program_arguments_and_option_values() {
	root := os.join_path(os.vtmp_dir(), 'native_backend_cli_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	code := 'import os\nfn main() { println(os.args[1..].join(" ")) }\n'
	os.write_file(source, code)!
	program_args := ['-b', 'native', '-backend', 'native', '-b=native', '-backend=native']
	for input_args in [
		['run', source],
		['-e', code],
		['-e=${code}'],
	] {
		mut args := ['-silent']
		args << input_args
		args << program_args
		result := cmdexec.run_in(@VEXE, args, root)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == program_args.join(' '), result.output
	}
	for value in ['-b=native', '-backend=native', '-b native'] {
		result := cmdexec.run(@VEXE, ['-silent', '-cflags', value, '-check', source])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('has been replaced'), result.output
	}
}
