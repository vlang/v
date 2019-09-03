import os

fn full_path_to_v() string {
	vname  := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	vexec := os.dir(os.dir(os.dir(os.dir( os.executable() )))) + os.PathSeparator + vname
	return vexec
}

fn test_the_v_compiler_can_be_invoked() {
	vexec := full_path_to_v()
	println('vexecutable: $vexec')
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
	assert vexec != ''

	vcmd := '$vexec --version'
	r := os.exec(vcmd) or { panic(err) }
	//println('"$vcmd" exit_code: $r.exit_code | output: $r.output')
	assert r.exit_code == 0

	vcmd_error := '$vexec nonexisting.v'
	r_error := os.exec(vcmd_error) or { panic(err) }
	//println('"$vcmd_error" exit_code: $r_error.exit_code | output: $r_error.output')
	assert r_error.exit_code == 1
	assert r_error.output == '`nonexisting.v` does not exist'
}

fn test_the_v_repl() {
	test_files := os.walk_ext('.', '.repl')
	wd := os.getwd() + os.PathSeparator
	vexec := full_path_to_v()
	
	for file in test_files {
		fcontent := os.read_file(file) or {
			assert false
			break
		}
		content := fcontent.replace('\r', '')		
		input := content.all_before('===output===\n')
		output := content.all_after('===output===\n')
		
		input_temporary_filename := 'input_temporary_filename.txt'
		os.write_file(input_temporary_filename, input)
		defer { os.rm(input_temporary_filename) }		
		r := os.exec('$vexec < $input_temporary_filename') or {
			assert false
			break
		}
		result := r.output.replace('\r','').replace('>>> ', '').replace('>>>', '').replace('... ', '').all_after('Use Ctrl-C or `exit` to exit\n').replace(wd, '' )
		assert result == output
		if result != output {
			println(file)
			println('Got : |$result|')
			println('Expected : |$output|')
		} else {
			println('Repl file $file is OK')
		}
	}
}
