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
	os.write_file('main.v', 'fn main(){\n\tprintln("Hello World!")\n}\n')!

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

fn test_run_custom_base_url_uses_project_root_lookup() {
	os.chdir(test_path)!
	project_dir := os.join_path(test_path, 'run_base_url_project')
	defer {
		os.chdir(test_path) or {}
	}
	os.mkdir_all(os.join_path(project_dir, 'source', 'foo'))!
	os.mkdir_all(os.join_path(project_dir, 'source', 'modules', 'dep'))!
	os.write_file(os.join_path(project_dir, 'v.mod'),
		"Module {\n\tname: 'run_base_url_project'\n\tbase_url: 'source'\n\tdescription: ''\n\tversion: ''\n\tlicense: ''\n\tdependencies: []\n}\n")!
	os.write_file(os.join_path(project_dir, 'source', 'main.v'), 'module main
import foo
import dep

fn main() {
	println(foo.name() + "+" + dep.name())
}
')!
	os.write_file(os.join_path(project_dir, 'source', 'foo', 'foo.v'), 'module foo

pub fn name() string {
	return "foo"
}
')!
	os.write_file(os.join_path(project_dir, 'source', 'modules', 'dep', 'dep.v'), 'module dep

pub fn name() string {
	return "dep"
}
')!
	os.chdir(project_dir)!
	assert run_v_ok('${os.quoted_path(vexe)} run .').trim_space() == 'foo+dep'
	assert run_v_ok('${os.quoted_path(vexe)} run source').trim_space() == 'foo+dep'
	assert run_v_ok('${os.quoted_path(vexe)} run ./source').trim_space() == 'foo+dep'
}

fn test_removed_src_layout_error_mentions_vmod_subdirs() {
	os.chdir(test_path)!
	project_dir := os.join_path(test_path, 'run_removed_src_project')
	defer {
		os.chdir(test_path) or {}
	}
	os.mkdir_all(os.join_path(project_dir, 'src'))!
	os.write_file(os.join_path(project_dir, 'src', 'main.v'),
		'fn main() {\n\tprintln("Hello from src")\n}\n')!
	os.chdir(project_dir)!

	res := os.execute('${os.quoted_path(vexe)} run .')
	normalized_output := res.output.replace('\r\n', '\n')

	assert res.exit_code != 0
	assert normalized_output.contains('the virtual `src/` module directory is no longer supported.')
	assert !normalized_output.contains('base_url')
	assert normalized_output.contains('add `subdirs` to v.mod')
	assert normalized_output.contains("subdirs: ['admin', 'repo', 'commit', 'ci', 'security', 'ssh', 'user']")
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
	p.set_args(['run', os.join_path(@VEXEROOT, 'vlib', 'v', 'tests', 'project_with_c_code', 'main.v')])
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

fn test_run_with_obscure_source_filenames() {
	if os.user_os() == 'windows' {
		return
	}
	obscure_dir := os.join_path(test_path, 'obscure_filenames')
	os.rmdir_all(obscure_dir) or {}
	os.mkdir_all(obscure_dir)!
	source := "println('hi')\n"
	for file_name in [
		"quote's.v",
		'"hi".v',
		"'.v",
		'".v',
		'.v',
		'..v',
		'...v',
		'-.v',
		'.c.v',
		'line\nfeed.v',
	] {
		src_file := os.join_path(obscure_dir, file_name)
		os.write_file(src_file, source)!
		display_name := file_name.replace('\n', '\\n')
		res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(src_file)}')
		assert res.exit_code == 0, '${display_name}: ${res.output}'
		assert res.output.trim_space() == 'hi', '${display_name}: ${res.output}'
		assert os.read_file(src_file)! == source
	}
}

fn test_macos_2048_build_does_not_force_deployment_target() {
	$if !macos {
		return
	}
	game_path := os.join_path(@VEXEROOT, 'examples', '2048', '2048.v')
	flags_output := run_v_ok('${os.quoted_path(vexe)} -dump-c-flags - ${os.quoted_path(game_path)}')
	assert flags_output.contains('-fobjc-arc')
	assert !flags_output.contains('-mmacosx-version-min=')
}
