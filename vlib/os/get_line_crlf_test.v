import os

fn test_get_line_strips_crlf_on_all_platforms() {
	dir := os.join_path(os.vtmp_dir(), 'get_line_crlf_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	binary := os.join_path(dir, 'read_line${if os.user_os() == 'windows' { '.exe' } else { '' }}')
	input := os.join_path(dir, 'input.txt')
	os.write_file(source, 'import os\nfn main() { println(os.get_line().bytes()); println(os.get_line().bytes()) }')!
	os.write_file(input, 'first\r\nsecond\n')!
	compiled := os.exec([@VEXE, '-nocache', '-o', binary, source])
	assert compiled.exit_code == 0, compiled.output
	mut process := os.new_process(binary)
	process.set_redirect_stdio()
	process.set_stdin_path(input)
	process.wait()
	output := process.stdout_slurp()
	assert process.code == 0
	process.close()
	assert output.replace('\r\n', '\n') == '[102, 105, 114, 115, 116]\n[115, 101, 99, 111, 110, 100]\n'
}
