module main

import os

const test_path = os.join_path(os.temp_dir(), 'v', 'run_check')

const vexe = @VEXE

fn testsuite_begin() {
	os.mkdir_all(test_path) or {}
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_conditional_executable_removal() ? {
	os.chdir(test_path)?
	os.execute_or_exit('${os.quoted_path(vexe)} init')

	mut executable := 'run_check'
	$if windows {
		executable += '.exe'
	}

	original_file_list_ := os.ls(test_path)?
	dump(original_file_list_)
	assert executable !in original_file_list_

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_run_file_list := os.ls(test_path)?
	dump(after_run_file_list)
	assert executable !in after_run_file_list

	assert os.execute('${os.quoted_path(vexe)} .').exit_code == 0
	assert os.execute('./$executable').output.trim_space() == 'Hello World!'
	after_compilation__ := os.ls(test_path)?
	dump(after_compilation__)
	assert executable in after_compilation__

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_second_run___ := os.ls(test_path)?
	dump(after_second_run___)
	assert executable in after_second_run___
}
