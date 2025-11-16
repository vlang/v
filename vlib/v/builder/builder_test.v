module main

import os

const vexe = @VEXE
const test_path = os.join_path(os.vtmp_dir(), 'run_check')

fn testsuite_begin() {
	os.mkdir_all(test_path) or {}
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

fn test_file_list() {
	os.chdir(test_path)!
	os.mkdir_all('filelist')!
	os.write_file('filelist/main.v', 'module main
fn main() {
        part_a := AS{}
        part_b := BS{}
        println("\${part_a}=>\${part_b}")
}')!
	os.write_file('filelist/part_a.v', 'module main
pub struct AS{}
')!

	os.write_file('filelist/part_b.v', 'module main
pub struct BS{}
')!

	// `part_c.v` is not included in the compilation
	// or there will be a conflit definition of `struct BS`
	os.write_file('filelist/part_c.v', 'module main
pub struct BS{}
')!
	mut executable := 'filelist_check'
	$if windows {
		executable += '.exe'
	}

	original_file_list_ := os.ls(test_path)!
	dump(original_file_list_)
	assert executable !in original_file_list_

	os.execute('${os.quoted_path(vexe)} -o ${executable} filelist/main.v -file-list "filelist/part_a.v;filelist/part_b.v;"')
	after_compile_file_list := os.ls(test_path)!.filter(os.exists(it))
	dump(after_compile_file_list)
	assert executable in after_compile_file_list
	assert os.execute('./${executable}').output.trim_space() == 'AS{}=>BS{}'
}
