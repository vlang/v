import os
import term

const vexe = @VEXE
const vroot = os.dir(vexe)

fn test_print_v_files_in_stdout_mode() {
	check_parsing_files_in_folder('vlib/v/parser/testdata/stdout', '-print-v-files')
}

fn test_print_v_files_in_silent_mode() {
	check_parsing_files_in_folder('vlib/v/parser/testdata/silent', '-silent -print-v-files')
}

fn test_print_watched_files_in_silent_mode__used_by_vwatch() {
	check_parsing_files_in_folder('vlib/v/parser/testdata/silent', '-silent -print-watched-files')
}

fn check_parsing_files_in_folder(folder string, options string) {
	println(term.colorize(term.magenta, '> checking .vv files in folder: `${folder}`, with `${options}` ...'))
	files := os.walk_ext(os.join_path(vroot, folder), '.vv')
	for f in files {
		cmd := '${os.quoted_path(vexe)} ${options} ${os.quoted_path(f)}'
		eprintln('> cmd: ${cmd}')
		res := os.execute(cmd)
		assert res.exit_code == 0, 'failed cmd: ${cmd}, output:\n${res.output}'
		assert res.output.split_into_lines().len > 10, 'there should be several files printed by cmd: ${cmd}'
	}
}
