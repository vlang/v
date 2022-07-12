module main

import os

const test_path = 'v_run_check'

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

	original_file_list := os.ls(dir)?
	mut new_file_list := original_file_list.clone()
	new_file_list << executable

	assert os.execute('${os.quoted_path(@VEXE)} run .').output.trim_space() == 'Hello World!'

	assert arrays_are_equivalent(os.ls(dir)?, original_file_list)

	assert os.execute('${os.quoted_path(@VEXE)} .').output == ''

	assert os.execute('./$executable').output.trim_space() == 'Hello World!'

	assert arrays_are_equivalent(os.ls(dir)?, new_file_list)

	assert os.execute('${os.quoted_path(@VEXE)} run .').output.trim_space() == 'Hello World!'

	assert arrays_are_equivalent(os.ls(dir)?, new_file_list)
}
