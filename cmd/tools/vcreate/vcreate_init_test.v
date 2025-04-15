// vtest retry: 3
import os
import v.vmod

const vroot = os.quoted_path(@VEXEROOT)
const vexe = os.quoted_path(@VEXE)
// Expect has to be installed for the test.
const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})
// Directory that contains the Expect scripts used in the test.
const expect_tests_path = os.join_path(@VEXEROOT, 'cmd', 'tools', 'vcreate', 'tests')
const test_project_dir_name = 'test_project'
// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test_project/`.
const test_path = os.join_path(os.vtmp_dir(), test_project_dir_name)

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn init_and_check() ! {
	os.chdir(test_path)!

	// Keep track of the last modified time of the main file to ensure it is not modified if it already exists.
	main_exists := os.exists('main.v')
	main_last_modified := if main_exists { os.file_last_mod_unix('main.v') } else { 0 }

	// Initialize project.
	os.execute_or_exit('${expect_exe} ${os.join_path(expect_tests_path, 'init.expect')} ${vroot}')

	x := os.execute_or_exit('${vexe} run .')
	assert x.output.trim_space() == 'Hello World!'

	if main_exists {
		assert main_last_modified == os.file_last_mod_unix('main.v')
	} else {
		assert os.read_file('main.v')! == [
			'module main\n',
			'fn main() {',
			"	println('Hello World!')",
			'}',
			'',
		].join_lines()
	}

	assert os.read_file('v.mod')! == [
		'Module {',
		"	name: '${test_project_dir_name}'",
		"	description: ''",
		"	version: '0.0.0'",
		"	license: 'MIT'",
		'	dependencies: []',
		'}',
		'',
	].join_lines()

	assert os.read_file('.gitignore')! == [
		'# Binaries for programs and plugins',
		'main',
		'${test_project_dir_name}',
		'*.exe',
		'*.exe~',
		'*.so',
		'*.dylib',
		'*.dll',
		'',
		'# Ignore binary output folders',
		'bin/',
		'',
		'# Ignore common editor/system specific metadata',
		'.DS_Store',
		'.idea/',
		'.vscode/',
		'*.iml',
		'',
		'# ENV',
		'.env',
		'',
		'# vweb and database',
		'*.db',
		'*.js',
		'',
	].join_lines()

	assert os.read_file('.gitattributes')! == [
		'* text=auto eol=lf',
		'*.bat eol=crlf',
		'',
		'*.v linguist-language=V',
		'*.vv linguist-language=V',
		'*.vsh linguist-language=V',
		'v.mod linguist-language=V',
		'.vdocignore linguist-language=ignore',
		'',
	].join_lines()

	assert os.read_file('.editorconfig')! == [
		'[*]',
		'charset = utf-8',
		'end_of_line = lf',
		'insert_final_newline = true',
		'trim_trailing_whitespace = true',
		'',
		'[*.v]',
		'indent_style = tab',
		'',
	].join_lines()
}

fn prepare_test_path() ! {
	os.rmdir_all(test_path) or {}
	os.mkdir_all(test_path) or {}
	os.chdir(test_path)!
}

fn test_v_init() {
	prepare_test_path()!
	init_and_check()!
}

fn test_v_init_in_git_dir() {
	prepare_test_path()!
	os.execute_or_exit('git init .')
	init_and_check()!
}

fn test_v_init_no_overwrite_gitignore() {
	prepare_test_path()!
	os.write_file('.gitignore', 'foo')!
	os.execute_or_exit('${expect_exe} ${os.join_path(expect_tests_path, 'init.expect')} ${vroot}')
	assert os.read_file('.gitignore')! == 'foo'
}

fn test_v_init_no_overwrite_gitattributes_and_editorconfig() {
	git_attributes_content := '*.v linguist-language=V text=auto eol=lf'
	editor_config_content := '[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.v]
indent_style = tab
'
	prepare_test_path()!
	os.write_file('.gitattributes', git_attributes_content)!
	os.write_file('.editorconfig', editor_config_content)!
	res := os.execute_or_exit('${expect_exe} ${os.join_path(expect_tests_path, 'init.expect')} ${vroot}')
	assert res.output.contains('Created binary (application) project `${test_project_dir_name}`')
	assert os.read_file('.gitattributes')! == git_attributes_content
	assert os.read_file('.editorconfig')! == editor_config_content
}

fn test_v_init_in_dir_with_invalid_mod_name_input() {
	// A project with a directory name with hyphens, which is invalid for a module name.
	dir_name_with_invalid_mod_name := 'my-proj'
	corrected_mod_name := 'my_proj'
	proj_path := os.join_path(os.vtmp_dir(), dir_name_with_invalid_mod_name)
	os.mkdir_all(proj_path) or {}
	os.chdir(proj_path)!
	os.execute_or_exit('${expect_exe} ${os.join_path(expect_tests_path, 'init_in_dir_with_invalid_mod_name.expect')} ${vroot} ${dir_name_with_invalid_mod_name} ${corrected_mod_name}')
	// Assert mod data set in `new_with_model_arg.expect`.
	mod := vmod.from_file(os.join_path(proj_path, 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == corrected_mod_name
}

fn test_v_init_with_model_arg_input() {
	prepare_test_path()!
	model := '--lib'
	res := os.execute_or_exit('${expect_exe} ${os.join_path(expect_tests_path, 'init_with_model_arg.expect')} ${vroot} ${model}')
	assert res.output.contains('Created library project `${test_project_dir_name}`'), res.output
	project_path := os.join_path(test_path)
	mod := vmod.from_file(os.join_path(project_path, 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == test_project_dir_name
	assert mod.description == 'My Awesome V Application.'
	assert mod.version == '0.0.1'
	assert mod.license == 'MIT'
	// Assert existence of a model-specific file.
	assert os.exists(os.join_path(project_path, 'tests', 'square_test.v'))
}
