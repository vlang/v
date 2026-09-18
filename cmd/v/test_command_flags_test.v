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
