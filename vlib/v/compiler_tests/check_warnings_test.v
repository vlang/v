module main

import os

fn check_output(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_warnings_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
}

// A check prints the warnings of a program without errors too: they are
// diagnostics like the others, and an editor shows nothing it is not given.
fn test_check_prints_the_warnings_of_a_program_without_errors() {
	res := check_output('clean', 'module main\n\nfn main() {\n\tunused := 1\n\tprintln(2)\n}\n')
	assert res.exit_code == 0, res.output
	assert res.output.contains('warning: unused variable: `unused`'), res.output
}

fn test_check_prints_warnings_next_to_errors() {
	res := check_output('failing', "module main\n\nfn main() {\n\tunused := 1\n\tbad := 'a' + 1\n\tprintln(bad)\n}\n")
	assert res.exit_code == 1, res.output
	assert res.output.contains('warning: unused variable: `unused`'), res.output
	assert res.output.contains('error: infix expr'), res.output
}
