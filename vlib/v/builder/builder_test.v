module main

import os
import encoding.iconv

const vexe = @VEXE
const test_path = os.join_path(os.vtmp_dir(), 'run_check')
const test_path2 = os.join_path(test_path, '测试目录')

fn testsuite_begin() {
	os.mkdir_all(test_path2) or {}
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_conditional_executable_removal() {
	os.chdir(test_path)!
	os.mkdir_all('src')!
	os.write_file('src/main.v', 'fn main(){\n\tprintln("Hello World!")\n}\n')!

	mut executable := 'run_check'
	$if windows {
		executable += '.exe'
	}

	original_file_list_ := os.ls(test_path)!
	dump(original_file_list_)
	assert executable !in original_file_list_

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_run_file_list := os.ls(test_path)!.filter(os.exists(it))
	dump(after_run_file_list)
	assert executable !in after_run_file_list

	assert os.execute('${os.quoted_path(vexe)} .').exit_code == 0
	assert os.execute('./${executable}').output.trim_space() == 'Hello World!'
	after_compilation__ := os.ls(test_path)!
	dump(after_compilation__)
	assert executable in after_compilation__

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_second_run___ := os.ls(test_path)!
	dump(after_second_run___)
	assert executable in after_second_run___
}

fn test_windows_ansi_path_name() {
	os.chdir(test_path2)!
	os.write_file('测试.v', 'fn main(){\n\tprintln("Hello World!")\n}\n')!

	mut executable := '测试'
	$if windows {
		executable += '.exe'
	}

	original_file_list_ := os.ls(test_path2)!
	dump(original_file_list_)
	assert executable !in original_file_list_

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_run_file_list := os.ls(test_path2)!.filter(os.exists(it))
	dump(after_run_file_list)
	assert executable !in after_run_file_list

	assert os.execute('${os.quoted_path(vexe)} . -o ${executable}').exit_code == 0
	assert os.execute('./${executable}').output.trim_space() == 'Hello World!'
	after_compilation__ := os.ls(test_path2)!
	dump(after_compilation__)
	assert executable in after_compilation__

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_second_run___ := os.ls(test_path2)!
	dump(after_second_run___)
	assert executable in after_second_run___
}
