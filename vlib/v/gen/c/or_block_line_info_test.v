import os

const line_info_test_vexe = @VEXE

fn test_option_propagation_panic_has_matching_line_info() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'or_block_line_info_test')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(os.real_path(tmp_dir), 'option_propagation_line_info.vv')
	os.write_file(source_path, 'struct Foo{}\n\nfn (f Foo) foo() int {\n\treturn 1\n}\n\nfn new_foo() ?Foo {\n\treturn none\n}\n\nfn main() {\n\ti := new_foo()?.foo()\n\tprintln(i)\n}\n')!
	cmd := '${os.quoted_path(line_info_test_vexe)} -g -o - ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	lines := res.output.replace('\r\n', '\n').split_into_lines()
	expected_line := '#line 12 "${source_path}"'
	expected_panic := 'builtin__panic_debug(12, builtin__tos3("${source_path}")'
	mut main_idx := -1
	for i, line in lines {
		if line.contains('void main__main(void) {') {
			main_idx = i
			break
		}
	}
	assert main_idx >= 0, res.output
	mut directive_idx := -1
	mut panic_idx := -1
	for i := main_idx + 1; i < lines.len; i++ {
		line := lines[i]
		if line == expected_line {
			directive_idx = i
		}
		if line.contains(expected_panic) {
			panic_idx = i
			break
		}
	}
	assert panic_idx > 0, res.output
	assert directive_idx == panic_idx - 1, 'expected `${expected_line}` immediately before `${expected_panic}`\n${res.output}'
}
