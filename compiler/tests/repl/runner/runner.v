module runner

import os
import time

struct RunnerOptions {
pub:
	wd string
	vexec string
	files []string
}

pub fn full_path_to_v() string {
	vname  := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	vexec := os.dir(os.dir(os.dir(os.dir( os.executable() )))) + os.PathSeparator + vname
	/*
	args := os.args
	vreal  := os.realpath('v')
	myself := os.realpath( os.executable() )
	wd := os.getwd() + os.PathSeparator
	println('args are: $args')
	println('vreal   : $vreal')
	println('myself  : $myself')
	println('wd      : $wd')
    */	
	return vexec
}

pub fn run_repl_file(wd string, vexec string, file string) string? {
	fcontent := os.read_file(file) or {	return error('Could not read file $file') }
	content := fcontent.replace('\r', '')		
	input := content.all_before('===output===\n')
	output := content.all_after('===output===\n')
	
	input_temporary_filename := 'input_temporary_filename.txt'
	os.write_file(input_temporary_filename, input)

	r := os.exec('$vexec runrepl < $input_temporary_filename') or {
		os.rm(input_temporary_filename)
		return error('Could not execute "$vexec runrepl < $input_temporary_filename" ')
	}
	os.rm(input_temporary_filename)

	result := r.output.replace('\r','').replace('>>> ', '').replace('>>>', '').replace('... ', '').all_after('Use Ctrl-C or `exit` to exit\n').replace(wd, '' )

	if result != output {
		return error('Difference found in REPL file: $file
====> Got      :
|$result|
====> Expected :
|$output|
		')
	} else {
		return 'Repl file $file is OK'
	}
}

pub fn new_options() RunnerOptions {
	wd := os.getwd() + os.PathSeparator
	vexec := full_path_to_v()
	mut files := []string
	if os.args.len > 1 {
		files = os.args.right(1)
	} else {
		files = os.walk_ext('.', '.repl')
	}
	return RunnerOptions {
		wd: wd
		vexec: vexec
		files: files
	}
}

pub fn now() i64 {
	return time.ticks()
}

pub fn tdiff_in_ms(s string, sticks i64) string {
	eticks := time.ticks()
	tdiff := (eticks - sticks)
	return '${tdiff:6d} ms | $s'
}

