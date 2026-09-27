module types

import os

fn test_returned_array_still_borrows_immutable_argument() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_scope_${os.getpid()}.v')
	os.write_file(path, 'fn borrowed(values []int) []int { return values }
fn nested(values []int) []int { return borrowed(values) }
fn main() { original := [1, 2]; mut alias := nested(original); alias[0] = 9 }
')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}
