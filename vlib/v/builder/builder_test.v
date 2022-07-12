module main

import os

const test_path = 'v_run_check'

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

	original_file_list_ := os.ls(dir)?
	dump(original_file_list_)
	assert executable !in original_file_list_

	assert os.execute('${os.quoted_path(@VEXE)} run .').output.trim_space() == 'Hello World!'
	after_run_file_list := os.ls(dir)?
	dump(after_run_file_list)
	assert executable !in after_run_file_list

	assert os.execute('${os.quoted_path(@VEXE)} .').exit_code == 0
	assert os.execute('./$executable').output.trim_space() == 'Hello World!'
	after_compilation__ := os.ls(dir)?
	dump(after_compilation__)
	assert executable in after_compilation__

	assert os.execute('${os.quoted_path(@VEXE)} run .').output.trim_space() == 'Hello World!'
	after_second_run___ := os.ls(dir)?
	dump(after_second_run___)
	assert executable in after_second_run___
}
