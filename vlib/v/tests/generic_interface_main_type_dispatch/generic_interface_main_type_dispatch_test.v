import os

fn test_generic_interface_dispatch_keeps_main_and_nested_arguments() {
	project := os.join_path(os.dir(@FILE), 'project')
	output := os.join_path(os.vtmp_dir(), 'generic_interface_dispatch_${os.getpid()}')
	os.mkdir_all(output) or { panic(err) }
	defer { os.rmdir_all(output) or {} }
	result := os.exec([@VEXE, '-b', 'c', '-o', os.join_path(output, 'program'), 'run', project])
	assert result.exit_code == 0, result.output
}
