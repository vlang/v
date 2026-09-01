import os

const missing_import_vexe = @VEXE
const missing_import_tests_dir = os.dir(@FILE)
const missing_import_v3_dir = os.dir(missing_import_tests_dir)
const missing_import_vlib_dir = os.dir(missing_import_v3_dir)
const missing_import_v3_src = os.join_path(missing_import_v3_dir, 'v3.v')

fn test_each_unresolved_module_import_is_reported() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_missing_import_diagnostics_${os.getpid()}')
	root := os.join_path(os.temp_dir(), 'v3_missing_import_project_${os.getpid()}')
	output := os.join_path(os.temp_dir(), 'v3_missing_import_output_${os.getpid()}')
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${missing_import_vexe} -gc none -path "${missing_import_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${missing_import_v3_src}')
	assert build.exit_code == 0, build.output

	os.mkdir_all(root) or { panic(err) }
	module_name := 'definitely_missing_v3_review_module'
	empty_module_name := 'empty_target_v3_review_module'
	empty_module_dir := os.join_path(root, 'modules', empty_module_name)
	os.mkdir_all(empty_module_dir) or { panic(err) }
	os.write_file(os.join_path(empty_module_dir, 'only_d_v3_review_never.v'),
		'module ${empty_module_name}\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import ${module_name}
import ${empty_module_name}

fn main() {}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'second.v'), 'module main

import ${module_name}
import ${empty_module_name}

fn helper() {}
') or {
		panic(err)
	}

	for flags in ['-no-parallel', '-building-v'] {
		result := os.execute('${v3_bin} -nocache ${flags} -o ${output} ${root}')
		assert result.exit_code != 0, result.output
		for unresolved in [module_name, empty_module_name] {
			message := 'cannot import module "${unresolved}" (not found)'
			assert result.output.count(message) == 2, result.output
		}
		assert result.output.contains('main.v'), result.output
		assert result.output.contains('second.v'), result.output
	}
}

fn test_eager_import_resolution_matches_authoritative_resolution() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_eager_import_resolution_${os.getpid()}')
	root := os.join_path(os.temp_dir(), 'v3_eager_import_project_${os.getpid()}')
	output := os.join_path(os.temp_dir(), 'v3_eager_import_output_${os.getpid()}')
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${missing_import_vexe} -gc none -path "${missing_import_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${missing_import_v3_src}')
	assert build.exit_code == 0, build.output

	precedence_root := os.join_path(root, 'precedence')
	local_arrays_dir := os.join_path(precedence_root, 'modules', 'arrays')
	os.mkdir_all(local_arrays_dir) or { panic(err) }
	os.write_file(os.join_path(local_arrays_dir, 'arrays.v'), "module arrays

pub fn eager_local_marker() string {
	return 'local arrays'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(precedence_root, 'main.v'), 'module main

import arrays

fn main() {
	println(arrays.eager_local_marker())
}
') or {
		panic(err)
	}
	precedence_build :=
		os.execute('${v3_bin} -nocache -building-v -o ${output} ${precedence_root}/main.v')
	assert precedence_build.exit_code == 0, precedence_build.output
	precedence_run := os.execute(output)
	assert precedence_run.exit_code == 0, precedence_run.output
	assert precedence_run.output.trim_space() == 'local arrays', precedence_run.output

	string_root := os.join_path(root, 'string_literal')
	bridge_dir := os.join_path(string_root, 'modules', 'eager_string_bridge')
	trap_dir := os.join_path(string_root, 'modules', 'eager_string_literal_trap')
	os.mkdir_all(bridge_dir) or { panic(err) }
	os.mkdir_all(trap_dir) or { panic(err) }
	os.write_file(os.join_path(bridge_dir, 'bridge.v'), "module eager_string_bridge

pub const text = 'before
import eager_string_literal_trap
after'
") or {
		panic(err)
	}
	os.write_file(os.join_path(trap_dir, 'trap.v'), 'module eager_string_literal_trap

this source must never be parsed
') or {
		panic(err)
	}
	os.write_file(os.join_path(string_root, 'main.v'), 'module main

import eager_string_bridge

fn main() {
	println(eager_string_bridge.text)
}
') or {
		panic(err)
	}
	string_build := os.execute('${v3_bin} -nocache -building-v -o ${output} ${string_root}/main.v')
	assert string_build.exit_code == 0, string_build.output
	string_run := os.execute(output)
	assert string_run.exit_code == 0, string_run.output
	assert string_run.output.trim_space() == 'before\nimport eager_string_literal_trap\nafter', string_run.output

	collision_root := os.join_path(root, 'suffix_collision')
	plain_bar_dir := os.join_path(collision_root, 'bar')
	dotted_bar_dir := os.join_path(collision_root, 'foo', 'bar')
	user_dir := os.join_path(collision_root, 'user')
	os.mkdir_all(plain_bar_dir) or { panic(err) }
	os.mkdir_all(dotted_bar_dir) or { panic(err) }
	os.mkdir_all(user_dir) or { panic(err) }
	os.write_file(os.join_path(plain_bar_dir, 'bar.v'), "module bar

pub fn value() string {
	return 'plain bar'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(dotted_bar_dir, 'bar.v'), "module bar

pub fn value() string {
	return 'dotted bar'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(user_dir, 'user.v'), 'module user

import foo.bar

pub fn value() string {
	return bar.value()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(collision_root, 'main.v'), 'module main

import bar
import user

fn main() {
	println(bar.value())
	println(user.value())
}
') or {
		panic(err)
	}
	collision_build :=
		os.execute('${v3_bin} -nocache -building-v -o ${output} ${collision_root}/main.v')
	assert collision_build.exit_code == 0, collision_build.output
	collision_run := os.execute(output)
	assert collision_run.exit_code == 0, collision_run.output
	assert collision_run.output.trim_space() == 'plain bar\ndotted bar', collision_run.output

	alias_root := os.join_path(root, 'module_alias')
	legacy_dir := os.join_path(alias_root, 'modules', 'legacy')
	canonical_dir := os.join_path(alias_root, 'modules', 'canonical')
	os.mkdir_all(legacy_dir) or { panic(err) }
	os.mkdir_all(canonical_dir) or { panic(err) }
	os.write_file(os.join_path(alias_root, 'v.mod'), "Module {
	name: 'eager_alias_identity'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(legacy_dir, 'alias.v'),
		"@[alias: '@VMODROOT/modules/canonical'] module legacy\n") or { panic(err) }
	os.write_file(os.join_path(canonical_dir, 'canonical.v'), 'module canonical

pub struct Value {
pub:
	n int
}

pub fn make() Value {
	return Value{
		n: 42
	}
}

pub fn read(value Value) int {
	return value.n
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(alias_root, 'main.v'), 'module main

import legacy
import canonical

fn main() {
	println(canonical.read(legacy.make()))
}
') or {
		panic(err)
	}
	alias_build := os.execute('${v3_bin} -nocache -building-v -o ${output} ${alias_root}/main.v')
	assert alias_build.exit_code == 0, alias_build.output
	alias_run := os.execute(output)
	assert alias_run.exit_code == 0, alias_run.output
	assert alias_run.output.trim_space() == '42', alias_run.output

	dotted_root := os.join_path(root, 'dotted_suffix_collision')
	a_bar_dir := os.join_path(dotted_root, 'a', 'bar')
	b_bar_dir := os.join_path(dotted_root, 'b', 'bar')
	left_dir := os.join_path(dotted_root, 'left')
	right_dir := os.join_path(dotted_root, 'right')
	os.mkdir_all(a_bar_dir) or { panic(err) }
	os.mkdir_all(b_bar_dir) or { panic(err) }
	os.mkdir_all(left_dir) or { panic(err) }
	os.mkdir_all(right_dir) or { panic(err) }
	os.write_file(os.join_path(a_bar_dir, 'bar.v'), "module bar

pub fn value() string {
	return 'a.bar'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(b_bar_dir, 'bar.v'), "module bar

pub fn value() string {
	return 'b.bar'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(left_dir, 'left.v'), 'module left

import a.bar

pub fn value() string {
	return bar.value()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(right_dir, 'right.v'), 'module right

import b.bar

pub fn value() string {
	return bar.value()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(dotted_root, 'main.v'), 'module main

import left
import right

fn main() {
	println(left.value())
	println(right.value())
}
') or {
		panic(err)
	}
	dotted_build := os.execute('${v3_bin} -nocache -building-v -o ${output} ${dotted_root}/main.v')
	assert dotted_build.exit_code == 0, dotted_build.output
	dotted_run := os.execute(output)
	assert dotted_run.exit_code == 0, dotted_run.output
	assert dotted_run.output.trim_space() == 'a.bar\nb.bar', dotted_run.output
}
