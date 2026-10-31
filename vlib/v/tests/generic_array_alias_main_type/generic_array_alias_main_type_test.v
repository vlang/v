import os

fn test_generic_array_alias_result_keeps_the_caller_element_type() {
	project := os.join_path(os.dir(@FILE), 'project')
	output := os.join_path(os.vtmp_dir(), 'generic_array_alias_${os.getpid()}')
	os.mkdir_all(output) or { panic(err) }
	defer { os.rmdir_all(output) or {} }
	result := os.exec([@VEXE, '-b', 'c', '-o', os.join_path(output, 'program'), 'run', project])
	assert result.exit_code == 0, result.output
}

fn test_module_generic_struct_is_not_replaced_by_a_main_alias() {
	project := os.join_path(os.dir(@FILE), 'module_authority')
	output := os.join_path(os.vtmp_dir(), 'generic_module_authority_${os.getpid()}')
	os.mkdir_all(output) or { panic(err) }
	defer { os.rmdir_all(output) or {} }
	result := os.exec([@VEXE, '-b', 'c', '-o', os.join_path(output, 'program'), 'run', project])
	assert result.exit_code == 0, result.output
}

fn test_multiple_explicit_arguments_keep_the_first_return_type() {
	project := os.join_path(os.dir(@FILE), 'multiple_arguments')
	output := os.join_path(os.vtmp_dir(), 'generic_multiple_arguments_${os.getpid()}')
	os.mkdir_all(output) or { panic(err) }
	defer { os.rmdir_all(output) or {} }
	result := os.exec([@VEXE, '-b', 'c', '-o', os.join_path(output, 'program'), 'run', project])
	assert result.exit_code == 0, result.output
}
