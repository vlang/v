module main

import os
import arrays

const (
	test_path          = 'v_run_check'
	original_file_list = [
		'.git',
		'.editorconfig',
		'.gitattributes',
		'${test_path}.v',
		'v.mod',
		'.gitignore',
	]
)

fn arrays_are_equivalent(a []string, b []string) bool {
	if a.len != b.len {
		return false
	}
	for item in a {
		if item !in b {
			return false
		}
	}
	return true
}

fn test_conditional_executable_removal() ? {
	// Setup the sample project
	dir := os.join_path(os.temp_dir(), test_path)

	os.rmdir_all(dir) or {}
	os.mkdir(dir) or {}

	defer {
		os.rmdir_all(dir) or {}
	}

	os.chdir(dir)?
	os.execute_or_exit('${os.quoted_path(@VEXE)} init')

	mut executable := test_path
	$if windows {
		executable += '.exe'
	}

	new_file_list := arrays.concat(original_file_list, executable)

	assert arrays_are_equivalent(os.ls(dir)?, original_file_list)

	assert os.execute('${os.quoted_path(@VEXE)} run .').output == 'Hello World!\n'

	assert arrays_are_equivalent(os.ls(dir)?, original_file_list)

	assert os.execute('${os.quoted_path(@VEXE)} .').output == ''

	assert os.execute('./$executable').output == 'Hello World!\n'

	assert arrays_are_equivalent(os.ls(dir)?, new_file_list)

	assert os.execute('${os.quoted_path(@VEXE)} run .').output == 'Hello World!\n'

	assert arrays_are_equivalent(os.ls(dir)?, new_file_list)
}
