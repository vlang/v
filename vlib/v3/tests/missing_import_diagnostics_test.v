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
}
