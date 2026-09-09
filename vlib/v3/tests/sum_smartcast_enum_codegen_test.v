module v3tests

import os

fn test_sum_smartcast_enum_comparison_codegen() {
	v3 := @VEXE
	tmp := os.join_path(os.vtmp_dir(), 'v3_sum_smartcast_enum')
	os.mkdir_all(tmp)!
	source := os.join_path(tmp, 'main.v')
	os.write_file(source, "enum Kind {
	one
	two
}

type Parent = Kind | string

fn main() {
	parent := Parent(Kind.two)
	if parent is Kind && parent == .two {
		println('ok')
	}
}
")!
	result :=
		os.execute('${os.quoted_path(v3)} -silent -no-memory-limit run ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_assert_infix_runtime_values_codegen() {
	v3 := @VEXE
	tmp := os.join_path(os.vtmp_dir(), 'v3_assert_infix_runtime_values')
	os.mkdir_all(tmp)!
	source := os.join_path(tmp, 'main.v')
	os.write_file(source, 'fn main() {
	assert 5 * 5 == 77
}
')!
	result :=
		os.execute('${os.quoted_path(v3)} -silent -no-memory-limit run ${os.quoted_path(source)}')
	assert result.exit_code != 0
	assert result.output.contains('V panic: Assertion failed...'), result.output
	assert result.output.contains('left value: 5 * 5'), result.output
	assert result.output.contains('right value: 77'), result.output
}
