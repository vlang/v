import os
import term

const vexe = @VEXE
const vroot = os.dir(vexe)

fn test_check_syntax_in_silent_mode() {
	check_parsing_files_in_folder('vlib/v/parser/testdata/silent', '-silent -check-syntax')
}

fn check_parsing_files_in_folder(folder string, options string) {
	println(term.colorize(term.magenta, '> checking .vv files in folder: `${folder}`, with `${options}` ...'))
	files := os.walk_ext(os.join_path(vroot, folder), '.vv')
	for f in files {
		cmd := '${os.quoted_path(vexe)} ${options} ${os.quoted_path(f)}'
		eprintln('> cmd: ${cmd}')
		res := os.execute(cmd)
		assert res.exit_code == 1, 'Command should fail silently, but did not: `${cmd}`, output:\n${res.output}'
		assert res.output.trim_space() == '', 'there should be no output printed by comd: `${cmd}`'
	}
}
