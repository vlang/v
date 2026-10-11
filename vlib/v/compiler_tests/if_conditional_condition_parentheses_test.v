import os

fn test_production_if_conditions_keep_required_conditional_groups() {
	root := os.join_path(os.temp_dir(), 'if_conditional_group_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn choose(enabled bool) int {
	if (match enabled { true { false } else { true } }) {
		return 0
	}
	return 1
}
fn main() {
	assert choose(false) == 0
	assert choose(true) == 1
}
')!
	for mode in ['-no-parallel', ''] {
		result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache -prod -cc clang ${mode} run ${os.quoted_path(source)}')
		assert result.exit_code == 0, '${mode}: ${result.output}'
	}
	os.write_file(source, 'fn choose(enabled bool) bool {
	if (enabled) { return true }
	return false
}
fn main() { assert choose(true) }
')!
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -prod -check ${os.quoted_path(source)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('unnecessary `()` in `if` condition'), result.output
	os.write_file(source, 'fn choose(enabled bool) bool {
	if (if enabled { true } else { false }) { return true }
	return false
}
fn main() { assert choose(true) }
')!
	nested := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -prod -check ${os.quoted_path(source)}')
	assert nested.exit_code != 0, nested.output
	assert nested.output.contains('cannot start with a parenthesized `if` expression'), nested.output
}
