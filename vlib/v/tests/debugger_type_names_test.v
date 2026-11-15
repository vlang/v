import os

fn test_dbg_shows_variable_and_receiver_type_names() {
	dir := os.join_path(os.vtmp_dir(), 'v_debugger_type_names_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	exe := os.join_path(dir, 'main.exe')
	input := os.join_path(dir, 'input.txt')
	os.write_file(source, [
		'struct Point {',
		'	x int',
		'}',
		'',
		'fn (p Point) show(label string) {',
		r'	$dbg;',
		"	println('\${label} \${p.x}')",
		'}',
		'',
		'fn main() {',
		'	a := 1',
		r'	$dbg;',
		"	Point{3}.show('x')",
		'}',
	].join_lines())!
	os.write_file(input, 'p a\nc\np label\nc\n')!
	compiled := os.exec([@VEXE, '-o', exe, source])
	assert compiled.exit_code == 0, compiled.output
	mut run := os.new_process(exe)
	defer { run.close() }
	run.set_redirect_stdio()
	run.set_stdin_path(input)
	run.wait()
	output := run.stdout_slurp() + run.stderr_slurp()
	assert run.code == 0, output
	assert output.contains('a = 1 (int)'), output
	assert output.contains('[main] (Point) show'), output
	assert output.contains('label = x (string)'), output
	assert output.contains('x 3'), output
}
