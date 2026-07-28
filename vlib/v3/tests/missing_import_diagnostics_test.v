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
	os.write_file(os.join_path(root, 'main.v'), 'module main

import ${module_name}

fn main() {}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'second.v'), 'module main

import ${module_name}

fn helper() {}
') or {
		panic(err)
	}

	result := os.execute('${v3_bin} -nocache -no-parallel -o ${output} ${root}')
	assert result.exit_code != 0, result.output
	message := 'cannot import module "${module_name}" (not found)'
	assert result.output.count(message) == 2, result.output
	assert result.output.contains('main.v'), result.output
	assert result.output.contains('second.v'), result.output
}
