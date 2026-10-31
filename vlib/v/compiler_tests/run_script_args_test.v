import os

// `v run script.vsh -flag value` must hand `-flag value` to the program. The script is
// built fresh in an empty directory, so no earlier binary can hide a lost argument list.
fn test_v_run_script_forwards_arguments_to_program() {
	root := os.join_path(os.vtmp_dir(), 'v_run_script_args_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	script := os.join_path(root, 'args.vsh')
	os.write_file(script, "println(os.args[1..].join('|'))\n") or { panic(err) }
	// `-silent` keeps the module cache messages of a cold build out of the program's output.
	result := os.exec([@VEXE, '-silent', 'run', script, '-NoPull', 'two'])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '-NoPull|two', result.output
}
