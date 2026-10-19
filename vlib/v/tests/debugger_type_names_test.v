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
	run := os.execute('${os.quoted_path(exe)} < ${os.quoted_path(input)}')
	assert run.exit_code == 0, run.output
	assert run.output.contains('a = 1 (int)'), run.output
	assert run.output.contains('[main] (Point) show'), run.output
	assert run.output.contains('label = x (string)'), run.output
	assert run.output.contains('x 3'), run.output
}
