import os

fn test_js_literal_empty_range_is_rejected() {
	vexe := os.quoted_path(@VEXE)
	work_dir := os.join_path(os.vtmp_dir(), 'js_literal_empty_range_${os.getpid()}')
	os.rmdir_all(work_dir) or {}
	os.mkdir_all(work_dir) or { panic(err) }
	defer {
		os.rmdir_all(work_dir) or {}
	}
	program := os.join_path(work_dir, 'main.v')
	os.write_file(program, 'fn main() {\n\tfor _ in 4 .. 2 {}\n}\n') or { panic(err) }
	result := os.execute('${vexe} -w -b js -check ${os.quoted_path(program)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('empty range: `4 .. 2` will never execute'), result.output
}
