import os

fn test_inferred_conditional_returns_keep_declaring_module_and_caller_arguments() {
	project := os.join_path(os.dir(@FILE), 'project')
	output := os.join_path(os.vtmp_dir(), 'generic_conditional_return_${os.getpid()}')
	os.mkdir_all(output) or { panic(err) }
	defer { os.rmdir_all(output) or {} }
	result := os.exec([@VEXE, '-b', 'c', '-o', os.join_path(output, 'program'), 'run', project])
	assert result.exit_code == 0, result.output
}
