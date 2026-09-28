module types

import os

fn test_returned_array_still_borrows_immutable_argument() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_scope_${os.getpid()}.v')
	os.write_file(path, 'fn borrowed(values []int) []int { return values }
fn nested(values []int) []int { return borrowed(values) }
fn main() { original := [1, 2]; mut alias := nested(original); alias[0] = 9 }
')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_caller_smartcast_does_not_change_callee_return_alias() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_smartcast_${os.getpid()}.v')
	os.write_file(path, 'type Source = []int | string\nfn borrowed(values []int) []int { return values.reverse() }\nfn main() { values := Source("text"); if values is string { original := [1]; mut alias := borrowed(original); alias[0] = 9 } }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
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
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
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
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}

fn test_local_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_local_fn_${os.getpid()}.v')
	os.write_file(path, 'fn helper(values []int) []int { return values.clone() }\nfn nested(values []int) []int { helper := fn (input []int) []int { return input }; return helper(values) }\nfn main() { original := [1, 2]; mut alias := nested(original); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_nested_function_value_does_not_shadow_later_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_nested_local_fn_${os.getpid()}.v')
	os.write_file(path, 'fn helper(values []int) []int { return values.clone() }\nfn nested(values []int) []int { if true { helper := fn (input []int) []int { return input }; _ = helper }; return helper(values) }\nfn main() { original := [1, 2]; mut fresh := nested(original); fresh[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code == 0, result.output
}

fn test_for_in_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_loop_fn_${os.getpid()}.v')
	os.write_file(path, 'fn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn nested(values []int, helpers []fn ([]int) []int) []int { for helper in helpers { return helper(values) }; return values.clone() }\nfn main() { original := [1, 2]; mut alias := nested(original, [passthrough]); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_select_receive_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_select_fn_${os.getpid()}.v')
	os.write_file(path, 'type Mapper = fn ([]int) []int\nfn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn nested(values []int, helpers chan Mapper) []int { select { helper := <-helpers { return helper(values) } }; return values.clone() }\nfn main() { helpers := chan Mapper{cap: 1}; helpers <- passthrough; original := [1, 2]; mut alias := nested(original, helpers); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_multi_return_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_multi_fn_${os.getpid()}.v')
	os.write_file(path, 'fn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn make_helpers() (int, fn ([]int) []int) { return 0, passthrough }\nfn nested(values []int) []int { _, helper := make_helpers(); return helper(values) }\nfn main() { original := [1, 2]; mut alias := nested(original); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_if_guard_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_if_guard_fn_${os.getpid()}.v')
	os.write_file(path, 'type Mapper = fn ([]int) []int\nfn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn maybe_helper() ?Mapper { return passthrough }\nfn nested(values []int) []int { if helper := maybe_helper() { return helper(values) }; return values.clone() }\nfn main() { original := [1, 2]; mut alias := nested(original); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_if_smartcast_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_if_smartcast_fn_${os.getpid()}.v')
	os.write_file(path, 'type Mapper = fn ([]int) []int\ntype MapperOrInt = Mapper | int\nfn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn nested(values []int, helper MapperOrInt) []int { if helper is Mapper { return helper(values) }; return values.clone() }\nfn main() { original := [1, 2]; mut alias := nested(original, MapperOrInt(passthrough)); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_post_if_smartcast_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_post_if_smartcast_fn_${os.getpid()}.v')
	os.write_file(path, 'type Mapper = fn ([]int) []int\ntype MapperOrInt = Mapper | int\nfn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn nested(values []int, helper MapperOrInt) []int { if helper is Mapper {} else { return values.clone() }; return helper(values) }\nfn main() { original := [1, 2]; mut alias := nested(original, MapperOrInt(Mapper(passthrough))); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}

fn test_match_smartcast_function_value_shadows_fresh_top_level_helper() {
	path := os.join_path(os.vtmp_dir(), 'v3_return_alias_match_smartcast_fn_${os.getpid()}.v')
	os.write_file(path, 'type Mapper = fn ([]int) []int\ntype MapperOrInt = Mapper | int\nfn helper(values []int) []int { return values.clone() }\nfn passthrough(values []int) []int { return values }\nfn nested(values []int, helper MapperOrInt) []int { match helper { Mapper { return helper(values) } else {} }; return values.clone() }\nfn main() { original := [1, 2]; mut alias := nested(original, MapperOrInt(passthrough)); alias[0] = 9 }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('immutable'), result.output
}
