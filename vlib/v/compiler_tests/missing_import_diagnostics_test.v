import os

const missing_import_vexe = @VEXE
const missing_import_tests_dir = os.dir(@FILE)
const missing_import_v3_dir = os.dir(missing_import_tests_dir)
const missing_import_vlib_dir = os.dir(missing_import_v3_dir)
const missing_import_v3_src = os.join_path(missing_import_v3_dir, 'v.v')

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
	empty_module_dir := os.join_path(root, empty_module_name)
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

// The virtual `modules/` directory is no longer searched, so an import that it
// would have satisfied has to report the move instead of a bare "not found".
// The command has to be runnable as printed, and it may only be printed when the
// directory really holds a module this build could have used.
fn test_removed_modules_directory_reports_the_move_it_needs() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_modules_layout_hint_${os.getpid()}')
	root := os.join_path(os.temp_dir(), 'v3_modules_layout_project_${os.getpid()}')
	output := os.join_path(os.temp_dir(), 'v3_modules_layout_output_${os.getpid()}')
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(root) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${missing_import_vexe} -gc none -path "${missing_import_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${missing_import_v3_src}')
	assert build.exit_code == 0, build.output

	hint := 'the virtual `modules/` directory is no longer searched for modules.'

	// A plain import moves the module directory itself. The project directory
	// holds a space, so the printed command is only runnable when it is quoted.
	plain_root := os.join_path(root, 'plain project')
	write_modules_layout_module(plain_root, os.join_path('modules', 'helper'), 'helper')
	write_modules_layout_main(plain_root, 'helper')
	plain := os.execute('${v3_bin} -nocache -o ${output} ${os.quoted_path(os.join_path(plain_root,
		'main.v'))}')
	assert plain.exit_code != 0, plain.output
	assert plain.output.contains('cannot import module "helper" (not found)'), plain.output
	assert plain.output.contains(hint), plain.output
	plain_real := os.real_path(plain_root)
	plain_source := os.join_path(plain_real, 'modules', 'helper')
	plain_target := os.join_path(plain_real, 'helper')
	assert plain.output.contains(modules_layout_expected_move(plain_source, plain_target)), plain.output

	// A source file buried in the project does not reach the directory either:
	// the walk up from it looks for the module's path under each level, not for a
	// `modules/` directory on the way, so the project reports the move as well.
	nested_root := os.join_path(root, 'nested')
	write_modules_layout_module(nested_root, os.join_path('modules', 'helper'), 'helper')
	os.write_file(os.join_path(nested_root, 'v.mod'), "Module { name: 'nested_app' }\n") or {
		panic(err)
	}
	write_modules_layout_main(os.join_path(nested_root, 'src'), 'helper')
	nested := os.execute('${v3_bin} -nocache -o ${output} ${nested_root}/src/main.v')
	assert nested.exit_code != 0, nested.output
	assert nested.output.contains('cannot import module "helper" (not found)'), nested.output
	assert nested.output.contains(hint), nested.output
	nested_real := os.real_path(nested_root)
	assert nested.output.contains(modules_layout_expected_move(os.join_path(nested_real,
		'modules', 'helper'), os.join_path(nested_real, 'helper'))), nested.output

	// A dotted import lives several directories deep, so moving just the leaf
	// would need a destination parent that does not exist yet. Move the whole
	// top-level module tree instead, which keeps every import path intact.
	dotted_root := os.join_path(root, 'dotted')
	write_modules_layout_module(dotted_root, os.join_path('modules', 'gpu', 'agx', 'fw'),
		'fw')
	write_modules_layout_main(dotted_root, 'gpu.agx.fw')
	dotted := os.execute('${v3_bin} -nocache -o ${output} ${dotted_root}/main.v')
	assert dotted.exit_code != 0, dotted.output
	assert dotted.output.contains('cannot import module "gpu.agx.fw" (not found)'), dotted.output
	dotted_real := os.real_path(dotted_root)
	dotted_source := os.join_path(dotted_real, 'modules', 'gpu')
	dotted_target := os.join_path(dotted_real, 'gpu')
	assert dotted.output.contains(modules_layout_expected_move(dotted_source, dotted_target)), dotted.output

	// When the top-level destination is already taken, only the leaf can move,
	// and the command has to create the parents that move needs.
	taken_root := os.join_path(root, 'taken')
	write_modules_layout_module(taken_root, os.join_path('modules', 'gpu', 'agx', 'fw'),
		'fw')
	write_modules_layout_module(taken_root, os.join_path('gpu', 'other'), 'other')
	write_modules_layout_main(taken_root, 'gpu.agx.fw')
	taken := os.execute('${v3_bin} -nocache -o ${output} ${taken_root}/main.v')
	assert taken.exit_code != 0, taken.output
	taken_real := os.real_path(taken_root)
	taken_source := os.join_path(taken_real, 'modules', 'gpu', 'agx', 'fw')
	taken_target := os.join_path(taken_real, 'gpu', 'agx', 'fw')
	taken_parent := os.join_path(taken_real, 'gpu', 'agx')
	assert taken.output.contains('${modules_layout_expected_mkdir(taken_parent)} && ${modules_layout_expected_move(taken_source,
		taken_target)}'), taken.output

	// When the destination itself already exists, `mv` would move the module
	// *into* it and nest it one level deeper, so the hint has to ask for a merge
	// rather than print a command that leaves the import unresolved.
	occupied_root := os.join_path(root, 'occupied')
	write_modules_layout_module(occupied_root, os.join_path('modules', 'gpu', 'agx', 'fw'),
		'fw')
	occupied_target_dir := os.join_path(occupied_root, 'gpu', 'agx', 'fw')
	os.mkdir_all(occupied_target_dir) or { panic(err) }
	os.write_file(os.join_path(occupied_target_dir, 'fw_d_v3_layout_never.v'), 'module fw\n') or {
		panic(err)
	}
	write_modules_layout_main(occupied_root, 'gpu.agx.fw')
	occupied := os.execute('${v3_bin} -nocache -o ${output} ${occupied_root}/main.v')
	assert occupied.exit_code != 0, occupied.output
	occupied_real := os.real_path(occupied_root)
	occupied_source := os.join_path(occupied_real, 'modules', 'gpu', 'agx', 'fw')
	occupied_target := os.join_path(occupied_real, 'gpu', 'agx', 'fw')
	assert occupied.output.contains('merge ${os.quoted_path(occupied_source)} into the existing ${os.quoted_path(occupied_target)}'), occupied.output
	assert !occupied.output.contains('mv ${occupied_source} ${occupied_target}'), occupied.output

	// A directory whose only source is disabled for this build is not a module
	// the move would recover, so it must not be advertised as one.
	disabled_root := os.join_path(root, 'disabled')
	disabled_dir := os.join_path(disabled_root, 'modules', 'disabled_helper')
	os.mkdir_all(disabled_dir) or { panic(err) }
	os.write_file(os.join_path(disabled_dir, 'helper_d_v3_layout_never.v'), 'module disabled_helper\n') or {
		panic(err)
	}
	write_modules_layout_main(disabled_root, 'disabled_helper')
	disabled := os.execute('${v3_bin} -nocache -o ${output} ${disabled_root}/main.v')
	assert disabled.exit_code != 0, disabled.output
	assert disabled.output.contains('cannot import module "disabled_helper" (not found)'), disabled.output
	assert !disabled.output.contains(hint), disabled.output

	// A `modules/` directory above the importer's own project belongs to whatever
	// lives there, not to the importer. Resolution would not have taken it, so the
	// hint may not tell anyone to move another project's source either.
	foreign_root := os.join_path(root, 'foreign')
	write_modules_layout_module(foreign_root, os.join_path('modules', 'stranger'), 'stranger')
	foreign_app := os.join_path(foreign_root, 'app')
	os.mkdir_all(foreign_app) or { panic(err) }
	os.write_file(os.join_path(foreign_app, 'v.mod'), "Module { name: 'foreign_app' }\n") or {
		panic(err)
	}
	write_modules_layout_main(foreign_app, 'stranger')
	foreign := os.execute('${v3_bin} -nocache -o ${output} ${foreign_app}/main.v')
	assert foreign.exit_code != 0, foreign.output
	assert foreign.output.contains('cannot import module "stranger" (not found)'), foreign.output
	assert !foreign.output.contains(hint), foreign.output
}

// The hint quotes the paths it prints and names the tool the host actually has,
// so the expectations here have to be built the same way.
fn modules_layout_expected_move(source string, target string) string {
	$if windows {
		return 'move ${os.quoted_path(source)} ${os.quoted_path(target)}'
	} $else {
		return 'mv ${os.quoted_path(source)} ${os.quoted_path(target)}'
	}
}

fn modules_layout_expected_mkdir(dir string) string {
	$if windows {
		return 'mkdir ${os.quoted_path(dir)}'
	} $else {
		return 'mkdir -p ${os.quoted_path(dir)}'
	}
}

fn write_modules_layout_module(root string, relative string, name string) {
	dir := os.join_path(root, relative)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n') or { panic(err) }
}

fn write_modules_layout_main(root string, import_path string) {
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nimport ${import_path}\n\nfn main() {}\n') or {
		panic(err)
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
	local_arrays_dir := os.join_path(precedence_root, 'arrays')
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
	bridge_dir := os.join_path(string_root, 'eager_string_bridge')
	trap_dir := os.join_path(string_root, 'eager_string_literal_trap')
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
	legacy_dir := os.join_path(alias_root, 'legacy')
	canonical_dir := os.join_path(alias_root, 'canonical')
	os.mkdir_all(legacy_dir) or { panic(err) }
	os.mkdir_all(canonical_dir) or { panic(err) }
	os.write_file(os.join_path(alias_root, 'v.mod'), "Module {
	name: 'eager_alias_identity'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(legacy_dir, 'alias.v'),
		"@[alias: '@VMODROOT/canonical'] module legacy\n") or { panic(err) }
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
