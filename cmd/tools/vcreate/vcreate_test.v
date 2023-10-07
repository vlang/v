import os
import v.vmod

// Note: the following uses `test_vcreate` and NOT `vcreate_test` deliberately,
// to both avoid confusions with the name of the current test itself, and to
// avoid clashes with the postfix `_test.v`, that V uses for its own test files.
const test_path = os.join_path(os.vtmp_dir(), 'v', 'test_vcreate')

fn init_and_check() ! {
	os.chdir(test_path)!

	// if main file already exist we should not tamper it
	mut main_last_modified_time := i64(0)
	is_main_file_preexisting := os.exists('src/main.v')
	if is_main_file_preexisting == true {
		main_last_modified_time = os.file_last_mod_unix('src/main.v')
	}
	os.execute_or_exit('${os.quoted_path(@VEXE)} init')

	x := os.execute_or_exit('${os.quoted_path(@VEXE)} run .')
	assert x.exit_code == 0
	assert x.output.trim_space() == 'Hello World!'

	if is_main_file_preexisting == true {
		assert main_last_modified_time == os.file_last_mod_unix('src/main.v')
	} else {
		assert os.read_file('src/main.v')! == [
			'module main\n',
			'fn main() {',
			"	println('Hello World!')",
			'}',
			'',
		].join_lines()
	}

	assert os.read_file('v.mod')! == [
		'Module {',
		"	name: 'test_vcreate'",
		"	description: ''",
		"	version: ''",
		"	license: ''",
		'	dependencies: []',
		'}',
		'',
	].join_lines()

	assert os.read_file('.gitignore')! == [
		'# Binaries for programs and plugins',
		'main',
		'test_vcreate',
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
		'**/*.v linguist-language=V',
		'**/*.vv linguist-language=V',
		'**/*.vsh linguist-language=V',
		'**/v.mod linguist-language=V',
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
	os.write_file('.gitignore', 'blah')!
	os.execute_or_exit('${os.quoted_path(@VEXE)} init')
	assert os.read_file('.gitignore')! == 'blah'
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
	os.execute_or_exit('${os.quoted_path(@VEXE)} init')

	assert os.read_file('.gitattributes')! == git_attributes_content
	assert os.read_file('.editorconfig')! == editor_config_content
}

fn test_input() {
	$if windows {
		eprintln('Input test for windows are not yet implemented.')
		return
	}
	expect_tests_path := os.join_path(@VMODROOT, 'cmd', 'tools', 'vcreate', 'tests')

	// The expect script will create a new project in the temporary `test_path` directory.
	project_name := 'my_project'
	new_no_arg := os.execute('${os.join_path(expect_tests_path, 'new_no_arg.expect')} ${@VMODROOT} ${project_name}')
	if new_no_arg.exit_code != 0 {
		assert false, new_no_arg.output
	}
	// Assert mod data set in `new_no_arg.expect`.
	mod := vmod.decode(os.read_file(os.join_path(test_path, project_name, 'v.mod')) or {
		assert false, 'Failed reading v.mod of ${project_name}'
		return
	}) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'my_project'
	assert mod.description == 'My awesome V project.'
	assert mod.version == '0.1.0'
	assert mod.license == 'GPL'
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}
