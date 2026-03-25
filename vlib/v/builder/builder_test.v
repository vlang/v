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

fn run_v_ok(command string) string {
	res := os.execute(command)
	if res.exit_code != 0 {
		eprintln('> failing cmd: ${command}')
		eprintln('> output:\n${res.output}')
		assert res.exit_code == 0
	}
	return res.output
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

	cmd := '${os.quoted_path(vexe)} -o ${executable} filelist/main.v -file-list "filelist/part_a.v,filelist/part_b.v,"'
	os.execute(cmd)
	after_compile_file_list := os.ls(test_path)!.filter(os.exists(it))
	dump(after_compile_file_list)
	assert executable in after_compile_file_list
	assert os.execute('./${executable}').output.trim_space() == 'AS{}=>BS{}'
}

fn test_run_explicit_src_directory_uses_project_root_lookup() {
	os.chdir(test_path)!
	project_dir := os.join_path(test_path, 'run_src_project')
	defer {
		os.chdir(test_path) or {}
	}
	os.mkdir_all(os.join_path(project_dir, 'src'))!
	os.mkdir_all(os.join_path(project_dir, 'modules', 'somemoduletwo'))!
	os.write_file(os.join_path(project_dir, 'src', 'main.v'), 'module main
import somemoduletwo

fn main() {
	println(somemoduletwo.name())
}
')!
	os.write_file(os.join_path(project_dir, 'modules', 'somemoduletwo', 'somemoduletwo.v'),
		'module somemoduletwo

pub fn name() string {
	return "somemoduletwo"
}
')!
	os.chdir(project_dir)!
	assert run_v_ok('${os.quoted_path(vexe)} run src').trim_space() == 'somemoduletwo'
	assert run_v_ok('${os.quoted_path(vexe)} run ./src').trim_space() == 'somemoduletwo'
}

fn test_thirdparty_object_build_with_multiline_cflags() {
	mut env := os.environ()
	existing_cflags := if 'CFLAGS' in env { env['CFLAGS'] } else { '' }
	env['CFLAGS'] = if existing_cflags == '' {
		'-DTHIRDPARTY_MULTILINE_1=1\n-DTHIRDPARTY_MULTILINE_2=1'
	} else {
		'${existing_cflags}\n-DTHIRDPARTY_MULTILINE_1=1\n-DTHIRDPARTY_MULTILINE_2=1'
	}
	mut p := os.new_process(vexe)
	p.set_work_folder(@VEXEROOT)
	p.set_args(['run', os.join_path(@VEXEROOT, 'vlib', 'v', 'tests', 'project_with_c_code',
		'main.v')])
	p.set_environment(env)
	p.set_redirect_stdio()
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	p.close()
	assert p.code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
}

fn test_missing_library_is_reported_without_compiler_bug_hint() {
	os.chdir(test_path)!
	os.mkdir_all('missing_library')!
	lib_name := 'v_missing_lib_25499'
	src_file := os.join_path('missing_library', 'main.v')
	os.write_file(src_file, '#flag -l${lib_name}\nfn main() {}\n')!

	res := os.execute('${os.quoted_path(vexe)} ${os.quoted_path(src_file)}')
	normalized_output := res.output.replace('\r\n', '\n')

	assert res.exit_code != 0
	assert normalized_output.contains('builder error:')
	assert normalized_output.contains('C library `${lib_name}` was not found while linking the generated program.')
	assert normalized_output.contains('Please install the corresponding development package/libraries')
	assert !normalized_output.contains('This is a V compiler bug')
}
