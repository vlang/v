module main

import os
import v.cmdexec

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
