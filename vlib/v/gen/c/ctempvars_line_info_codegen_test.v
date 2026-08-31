import os

const ctempvars_line_info_vexe = @VEXE

fn test_ctemp_hoisting_preserves_vline_directive_spaces() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'ctempvars_line_info_test_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(os.real_path(tmp_dir), 'ctempvars_line_info.vv')
	os.write_file(source_path,
		'interface Value {}\n\nstruct Item {}\n\nfn get_value() Value {\n\treturn Item{}\n}\n\nfn main() {\n\titem := get_value() as Item\n\tprintln(item)\n}\n')!
	cmd := '${os.quoted_path(ctempvars_line_info_vexe)} -g -gc boehm -o - ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	for line in res.output.replace('\r\n', '\n').split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('#line') {
			assert trimmed.starts_with('#line '), 'malformed line directive `${trimmed}`\n${res.output}'
		}
	}
}

fn test_array_push_result_keeps_vline_directive_on_own_line() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'ctempvars_result_line_info_test_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(os.real_path(tmp_dir), 'ctempvars_result_line_info.vv')
	os.write_file(source_path,
		'fn parse() !int {\n\treturn 1\n}\n\nfn parse_all() ![]int {\n\tmut parsed := []int{}\n\tparsed << parse()!\n\treturn parsed\n}\n\nfn main() {\n\tassert parse_all()! == [1]\n}\n')!
	cmd := '${os.quoted_path(ctempvars_line_info_vexe)} -g -o - ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	assert !res.output.contains('{#line'), res.output
}
