import os

const test_path = 'vcreate_test'

fn init_and_check() ? {
	os.execute_or_exit('${os.quoted_path(@VEXE)} init')

	assert os.read_file('vcreate_test.v') ? == [
		'module main\n',
		'fn main() {',
		"	println('Hello World!')",
		'}',
		'',
	].join_lines()

	assert os.read_file('v.mod') ? == [
		'Module {',
		"	name: 'vcreate_test'",
		"	description: ''",
		"	version: ''",
		"	license: ''",
		'	dependencies: []',
		'}',
		'',
	].join_lines()

	assert os.read_file('.gitignore') ? == [
		'# Binaries for programs and plugins',
		'main',
		'vcreate_test',
		'*.exe',
		'*.exe~',
		'*.so',
		'*.dylib',
		'*.dll',
		'vls.log',
		'',
	].join_lines()

	assert os.read_file('.gitattributes') ? == [
		'*.v linguist-language=V text=auto eol=lf',
		'*.vv linguist-language=V text=auto eol=lf',
		'*.vsh linguist-language=V text=auto eol=lf',
		'**/v.mod linguist-language=V text=auto eol=lf',
		'',
	].join_lines()

	assert os.read_file('.editorconfig') ? == [
		'[*]',
		'charset = utf-8',
		'end_of_line = lf',
		'insert_final_newline = true',
		'trim_trailing_whitespace = true',
		'',
		'[*.v]',
		'indent_style = tab',
		'indent_size = 4',
		'',
	].join_lines()
}

fn test_v_init() ? {
	dir := os.join_path(os.temp_dir(), test_path)
	os.rmdir_all(dir) or {}
	os.mkdir(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	os.chdir(dir) ?

	init_and_check() ?
}

fn test_v_init_in_git_dir() ? {
	dir := os.join_path(os.temp_dir(), test_path)
	os.rmdir_all(dir) or {}
	os.mkdir(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	os.chdir(dir) ?
	os.execute_or_exit('git init .')
	init_and_check() ?
}

fn test_v_init_no_overwrite_gitignore() ? {
	dir := os.join_path(os.temp_dir(), test_path)
	os.rmdir_all(dir) or {}
	os.mkdir(dir) or {}
	os.write_file('$dir/.gitignore', 'blah') ?
	defer {
		os.rmdir_all(dir) or {}
	}
	os.chdir(dir) ?

	os.execute_or_exit('${os.quoted_path(@VEXE)} init')

	assert os.read_file('.gitignore') ? == 'blah'
}

fn test_v_init_no_overwrite_gitattributes_and_editorconfig() ? {
	git_attributes_content := '*.v linguist-language=V text=auto eol=lf'
	editor_config_content := '[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.v]
indent_style = tab
indent_size = 4
'

	dir := os.join_path(os.temp_dir(), test_path)
	os.rmdir_all(dir) or {}
	os.mkdir(dir) or {}
	os.write_file('$dir/.gitattributes', git_attributes_content) ?
	os.write_file('$dir/.editorconfig', editor_config_content) ?
	defer {
		os.rmdir_all(dir) or {}
	}
	os.chdir(dir) ?

	os.execute_or_exit('${os.quoted_path(@VEXE)} init')

	assert os.read_file('.gitattributes') ? == git_attributes_content
	assert os.read_file('.editorconfig') ? == editor_config_content
}
