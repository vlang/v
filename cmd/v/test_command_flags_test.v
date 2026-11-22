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
	assert index == 5
	assert command == 'test'
	assert external_tool_runtime_args(command, args[..index], args[index..]) == args
}

fn test_external_test_tool_without_prefix_keeps_test_arguments() {
	args := ['test', 'bug_test.v']
	assert external_tool_runtime_args('test', []string{}, args) == args
}

fn test_race_flag_is_found_before_and_after_the_input() {
	assert compiler_option_requested(['-race', 'main.v'], '-race')
	assert compiler_option_requested(['main.v', '-race'], '-race')
	assert compiler_option_requested(['-race', 'run', 'main.v'], '-race')
	assert compiler_option_requested(['run', '-race', 'main.v'], '-race')
	assert compiler_option_requested(['-race', 'test', 'dir'], '-race')
	assert compiler_option_requested(['test', 'dir', '-race'], '-race')
	assert compiler_option_requested(['-o', 'out', 'main.v', '-race'], '-race')
	// `-profile` and `-prof` take an optional file, so they do not consume a following option.
	assert compiler_option_requested(['-profile', '-race', 'main.v'], '-race')
	assert compiler_option_requested(['main.v', '-prof', '-race'], '-race')
	assert compiler_option_requested(['-profile', 'prof.txt', 'main.v', '-race'], '-race')
}

fn test_compiler_option_is_told_apart_from_a_program_argument() {
	assert compiler_option_requested(['-json-errors', 'main.v'], '-json-errors')
	assert compiler_option_requested(['-check', 'main.v', '-json-errors'], '-json-errors')
	assert compiler_option_requested(['-json-errors', 'run', 'main.v'], '-json-errors')
	assert !compiler_option_requested(['run', 'main.v', '-json-errors'], '-json-errors')
	assert !compiler_option_requested(['-o', '-json-errors', 'main.v'], '-json-errors')
	assert !compiler_option_requested(['-json-errors', 'main.v'], '-race')
}

// The driver also runs a script without the `.vsh` extension and a program read from stdin.
fn test_compiler_option_stops_at_a_raw_script_and_at_a_run_stdin_input() {
	assert !compiler_option_requested(['-raw-vsh-tmp-prefix', 'tmp', 'script', '-json-errors'],
		'-json-errors')
	assert !compiler_option_requested(['run', '-', '-json-errors'], '-json-errors')
	assert !compiler_option_requested(['crun', '-', '-json-errors'], '-json-errors')
	assert !compiler_option_requested(['-raw-vsh-tmp-prefix', 'tmp', 'script', '-race'], '-race')
	assert !compiler_option_requested(['run', '-', '-race'], '-race')
	// Before the input they are compiler options.
	assert compiler_option_requested(['-json-errors', '-raw-vsh-tmp-prefix', 'tmp', 'script'],
		'-json-errors')
	assert compiler_option_requested(['-raw-vsh-tmp-prefix', 'tmp', '-json-errors', 'script'],
		'-json-errors')
	assert compiler_option_requested(['-json-errors', 'run', '-'], '-json-errors')
	assert compiler_option_requested(['run', '-json-errors', '-'], '-json-errors')
	// A stdin program that is built, not run, takes compiler options after it too.
	assert compiler_option_requested(['-', '-json-errors'], '-json-errors')
	// `-` after `-o` is the output, not the input.
	assert compiler_option_requested(['-o', '-', 'main.v', '-json-errors'], '-json-errors')
	// The prefix only makes a script of the input that follows it.
	assert compiler_option_requested(['script', '-raw-vsh-tmp-prefix', 'tmp', '-json-errors'],
		'-json-errors')
}

fn test_race_flag_of_a_run_program_is_not_a_compiler_option() {
	assert !compiler_option_requested(['run', 'main.v', '-race'], '-race')
	assert !compiler_option_requested(['crun', 'main.v', '-race'], '-race')
	assert !compiler_option_requested(['script.vsh', '-race'], '-race')
	assert !compiler_option_requested(['-o', '-race', 'main.v'], '-race')
	assert !compiler_option_requested(['-profile', 'run', 'main.v', '-race'], '-race')
	assert !compiler_option_requested(['main.v'], '-race')
}
