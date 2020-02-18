import (
	os
	v.fmt
	filepath
	term
	v.table
	v.parser
)

fn test_fmt() {
	println('Running vfmt tests')
	vexe := os.getenv('VEXE')
	vroot := filepath.dir(vexe)
	tmpfolder := os.tmpdir()
	diff_cmd := find_working_diff_command() or {
		eprintln('No working "diff" CLI command found.')
		exit(0)
	}
	term_ok := term.ok_message('OK')
	term_fail := term.fail_message('FAIL')
	input_files := os.walk_ext('$vroot/vlib/v/fmt/tests', '_input.vv')
	for ipath in input_files {
		ifilename := filepath.filename(ipath)
		opath := ipath.replace('_input.vv', '_expected.vv')
		if !os.exists(opath) {
			eprintln('${term_fail} missing ${opath}')
			continue
		}
		expected_ocontent := os.read_file(opath) or {
			panic(err)
		}
		table := table.new_table()
		file_ast := parser.parse_file(ipath, table)
		result_ocontent := fmt.fmt(file_ast, table)
		if expected_ocontent != result_ocontent {
			eprintln('${term_fail} ${ipath} is not formatted as expected.')
			vfmt_result_file := filepath.join(tmpfolder,'vfmt_run_over_${ifilename}')
			os.write_file(vfmt_result_file, result_ocontent)
			os.system('$diff_cmd --minimal  --text   --unified=2 --show-function-line="fn " "$vfmt_result_file" "$opath" ')
			continue
		}
		eprintln('${term_ok} ${ipath}')
	}
}

fn find_working_diff_command() ?string {
	for diffcmd in ['colordiff', 'diff', 'colordiff.exe', 'diff.exe'] {
		p := os.exec('$diffcmd --version') or {
			continue
		}
		if p.exit_code == 0 {
			return diffcmd
		}
	}
	return error('no working diff command found')
}
