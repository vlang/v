module main

import os
import v.cmdexec

// These subprocess tests use the existing launcher as their compiler.

fn test_obsolete_skip_unused_is_notice_only_and_preserves_no_skip_unused() {
	root := os.join_path(os.vtmp_dir(), 'default_skip_unused_cli_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(source, 'fn unused() int { return 7 }\nfn main() {}\n')!
	for flags in [['-skip-unused'], ['-skip-unused', '-no-skip-unused'],
		['-no-skip-unused', '-skip-unused']] {
		mut args := flags.clone()
		args << ['-new-compiler', '-w', '-o', c_path, source]
		result := cmdexec.run(@VEXE, args)
		assert result.exit_code == 0, result.output
		assert result.output.contains('notice: `-skip-unused` is no longer needed'), result.output
		assert result.output.contains('unused code is skipped by default'), result.output
		assert os.read_file(c_path)!.contains('unused(') == ('-no-skip-unused' in flags), result.output
	}
}

fn test_obsolete_skip_unused_preserves_program_and_tool_arguments() {
	root := os.join_path(os.vtmp_dir(), 'skip_unused_arguments_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nfn main() { println(os.args[1]) }\n')!
	result := cmdexec.run(@VEXE, ['-new-compiler', '-silent', 'run', source, '-skip-unused'])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '-skip-unused', result.output

	completion := cmdexec.run(@VEXE, ['complete', 'bash', 'v', '-skip-unused'])
	assert completion.exit_code == 0, completion.output
	assert !completion.output.contains('-skip-unused'), completion.output
}

fn test_obsolete_skip_unused_preserves_arguments_after_command_named_input_directories() {
	root := os.join_path(os.vtmp_dir(), 'skip_unused_command_inputs_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	for input in ['run', 'crun', 'build', 'test'] {
		folder := os.join_path(root, input)
		os.mkdir_all(folder)!
		os.write_file(os.join_path(folder, 'main.v'), 'import os\nfn main() { println(os.args[1]) }\n')!
		result := cmdexec.run_in(@VEXE, ['-new-compiler', '-silent', 'run', input, '-skip-unused'], root)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '-skip-unused', result.output
	}
}

fn test_removed_usecache_flag_is_rejected_before_tool_or_compatibility_dispatch() {
	for args in [['-usecache', 'examples/hello_world.v'], ['-usecache', 'help', 'build-c'],
		['-old-compiler', '-usecache', 'help']] {
		result := cmdexec.run(@VEXE, args)
		assert result.exit_code != 0, result.output
		assert result.output.contains('unknown option `-usecache`'), result.output
		assert result.output.contains('caching is enabled by default'), result.output
		assert !result.output.contains('requires the compatibility compiler'), result.output
	}
}

fn test_removed_usecache_flag_can_still_be_a_program_argument() {
	root := os.join_path(os.vtmp_dir(), 'default_cache_cli_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nfn main() { println(os.args[1]) }\n')!
	result := cmdexec.run(@VEXE, ['-silent', 'run', source, '-usecache'])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '-usecache', result.output
}

fn test_completion_does_not_suggest_the_removed_usecache_flag() {
	result := cmdexec.run(@VEXE, ['complete', 'bash', 'v', '-usecache'])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('-usecache'), result.output
}
