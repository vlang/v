module main

fn test_external_test_tool_receives_enable_globals() {
	assert external_tool_runtime_args('test', ['-enable-globals'], ['test', 'bug_test.v']) == [
		'-enable-globals',
		'test',
		'bug_test.v',
	]
}

fn test_external_test_tool_preserves_prefix_options_and_test_arguments() {
	args := [
		'-new-compiler',
		'-enable-globals',
		'-d',
		'custom_flag',
		'-cc',
		'clang',
		'test',
		'-run-only',
		'test_global',
		'tests with spaces',
		'other_test.v',
	]
	index, command := find_command(args)
	assert index == 6
	assert command == 'test'
	assert external_tool_runtime_args(command, args[..index], args[index..]) == args
}

fn test_external_test_tool_without_prefix_keeps_test_arguments() {
	args := ['test', 'bug_test.v']
	assert external_tool_runtime_args('test', []string{}, args) == args
}

fn test_race_flag_is_found_before_and_after_the_input() {
	assert race_build_requested(['-race', 'main.v'])
	assert race_build_requested(['main.v', '-race'])
	assert race_build_requested(['-race', 'run', 'main.v'])
	assert race_build_requested(['run', '-race', 'main.v'])
	assert race_build_requested(['-race', 'test', 'dir'])
	assert race_build_requested(['test', 'dir', '-race'])
	assert race_build_requested(['-o', 'out', 'main.v', '-race'])
}

fn test_race_flag_of_a_run_program_is_not_a_compiler_option() {
	assert !race_build_requested(['run', 'main.v', '-race'])
	assert !race_build_requested(['crun', 'main.v', '-race'])
	assert !race_build_requested(['script.vsh', '-race'])
	assert !race_build_requested(['-o', '-race', 'main.v'])
	assert !race_build_requested(['main.v'])
}
