module driver

import os

fn message_limit_fixture(name string) string {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	mut src := 'module main\n\nfn takes_int(x int) int {\n\treturn x\n}\n\nfn main() {\n'
	for i in 0 .. 25 {
		src += "\t_ = takes_int('s${i}')\n"
	}
	src += '}\n'
	os.write_file(os.join_path(root, 'main.v'), src) or { panic(err) }
	return root
}

fn test_errors_are_capped_at_twenty_by_default() {
	root := message_limit_fixture('message_limit_default')
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.count(' error: ') == 20, result.output
	assert result.output.contains('... and 5 more errors'), result.output
}

fn test_message_limit_can_show_more_than_twenty_errors() {
	root := message_limit_fixture('message_limit_raised')
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-message-limit', '100', '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.count(' error: ') == 25, result.output
}

fn test_message_limit_still_lowers_the_count() {
	root := message_limit_fixture('message_limit_lowered')
	defer { os.rmdir_all(root) or {} }
	result := os.exec([@VEXE, '-message-limit', '3', '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.count(' error: ') == 3, result.output
}
