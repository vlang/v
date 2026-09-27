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

fn test_caller_smartcast_does_not_change_callee_return_alias() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_smartcast_${os.getpid()}.v')
	os.write_file(path, 'type Source = []int | string\nfn borrowed(values []int) []int { return values.reverse() }\nfn main() { values := Source("text"); if values is string { original := [1]; mut alias := borrowed(original); alias[0] = 9 } }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_returned_array_alias_through_callee_local_receiver() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_local_receiver_${os.getpid()}.v')
	os.write_file(path, 'struct Passthrough {}
fn (p Passthrough) borrow(values []int) []int { return values }
fn nested(values []int) []int { helper := Passthrough{}; return helper.borrow(values) }
fn main() { original := [1, 2]; mut alias := nested(original); alias[0] = 9 }
')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_fresh_array_return_through_callee_local_receiver() {
	path := os.join_path(os.vtmp_dir(), 'v3_fresh_return_local_receiver_${os.getpid()}.v')
	os.write_file(path, 'struct Copier {}
fn (c Copier) copy(values []int) []int { return values.clone() }
fn nested(values []int) []int { helper := Copier{}; return helper.copy(values) }
fn main() { original := [1, 2]; mut fresh := nested(original); fresh[0] = 9 }
')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}
