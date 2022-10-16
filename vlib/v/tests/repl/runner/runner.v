module runner

import os
import v.util.diff

pub struct RunnerOptions {
pub:
	wd    string
	vexec string
	files []string
}

pub fn full_path_to_v(dirs_in int) string {
	vexe_from_env := os.getenv('VEXE')
	if vexe_from_env.len > 0 {
		return vexe_from_env
	}
	vname := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	mut path := os.executable()
	for i := 0; i < dirs_in; i++ {
		path = os.dir(path)
	}
	vexec := os.join_path(path, vname)
	/*
	args := os.args
	vreal  := os.real_path('v')
	myself := os.real_path( os.executable() )
	wd := os.getwd()
	println('args are: $args')
	println('vreal   : $vreal')
	println('myself  : $myself')
	println('wd      : $wd')
	*/
	return vexec
}

fn diff_files(file_result string, file_expected string) string {
	diffcmd := diff.find_working_diff_command() or { return err.msg() }
	return diff.color_compare_files(diffcmd, file_result, file_expected)
}

pub fn run_repl_file(wd string, vexec string, file string) !string {
	vexec_folder := os.dir(vexec) + os.path_separator
	fcontent := os.read_file(file) or { return error('Could not read repl file $file') }
	content := fcontent.replace('\r', '')
	input := content.all_before('===output===\n')
	output := content.all_after('===output===\n').trim_right('\n\r')
	fname := os.file_name(file)
	input_temporary_filename := os.real_path(os.join_path(wd, 'input_temporary_filename.txt'))
	os.write_file(input_temporary_filename, input) or { panic(err) }
	os.write_file(os.real_path(os.join_path(wd, 'original.txt')), fcontent) or { panic(err) }
	rcmd := '${os.quoted_path(vexec)} repl -replfolder ${os.quoted_path(wd)} -replprefix "${fname}." < ${os.quoted_path(input_temporary_filename)}'
	r := os.execute(rcmd)
	if r.exit_code != 0 {
		os.rm(input_temporary_filename)!
		return error('Could not execute: $rcmd')
	}
	result := r.output.replace('\r', '').replace('>>> ', '').replace('>>>', '').replace('... ',
		'').replace(wd + os.path_separator, '').replace(vexec_folder, '').replace('\\',
		'/').trim_right('\n\r')
	$if windows {
		dump(rcmd)
		dump(r.output)
		dump(result)
	}
	os.rm(input_temporary_filename)!
	if result != output {
		file_result := '${file}.result.txt'
		file_expected := '${file}.expected.txt'
		os.write_file(file_result, result) or { panic(err) }
		os.write_file(file_expected, output) or { panic(err) }
		diff := diff_files(file_expected, file_result)
		return error('Difference found in REPL file: $file
====> Expected :
|$output|
====> Got      :
|$result|
====> Diff     :
$diff
		')
	} else {
		return file.replace('./', '')
	}
}

pub fn run_prod_file(wd string, vexec string, file string) !string {
	file_expected := '${file}.expected.txt'
	f_expected_content := os.read_file(file_expected) or {
		return error('Could not read expected prod file $file_expected')
	}
	expected_content := f_expected_content.replace('\r', '')
	cmd := '${os.quoted_path(vexec)} -prod run ${os.quoted_path(file)}'
	r := os.execute(cmd)
	if r.exit_code < 0 {
		return error('Could not execute: $cmd')
	}
	if r.exit_code != 0 {
		return error('$cmd return exit code: $r.exit_code')
	}
	result := r.output.replace('\r', '')
	if result != expected_content {
		file_result := '${file}.result.txt'
		os.write_file(file_result, result) or { panic(err) }
		diff := diff_files(file_result, file_expected)
		return error('Difference found in test: $file
====> Got      :
|$result|
====> Expected :
|$expected_content|
====> Diff     :
$diff
		')
	} else {
		return 'Prod file $file is OK'
	}
}

pub fn new_options() RunnerOptions {
	vexec := full_path_to_v(5)
	mut wd := os.getwd()
	mut files := []string{}
	if os.args.len > 1 {
		files = os.args[1..]
	} else {
		os.chdir(os.dir(vexec)) or {}
		wd = os.getwd()
		files = os.walk_ext('.', '.repl')
	}
	return RunnerOptions{
		wd: wd
		vexec: vexec
		files: files
	}
}

pub fn new_prod_options() RunnerOptions {
	wd := os.getwd()
	vexec := full_path_to_v(4)
	mut files := []string{}
	if os.args.len > 1 {
		files = os.args[1..]
	} else {
		files = os.walk_ext(wd, '.prod.v')
	}
	return RunnerOptions{
		wd: wd
		vexec: vexec
		files: files
	}
}
