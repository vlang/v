module driver

import os

fn test_diagnostic_module_directory_uses_the_same_fixture_mode_as_vv_files() {
	root := os.join_path(os.vtmp_dir(), 'diagnostic_fixture_input_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	module_dir := os.join_path(root, 'vlib', 'v', 'checker', 'tests', 'modules', 'sample')
	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'main.v'), 'fn main() {}')!
	assert !input_is_legacy_diagnostic_fixture(module_dir)
	os.write_file(module_dir + '.out', 'expected diagnostic')!
	assert input_is_legacy_diagnostic_fixture(module_dir)
	assert input_is_legacy_diagnostic_fixture(module_dir + os.path_separator)
	previous_dir := os.getwd()
	os.chdir(module_dir)!
	defer { os.chdir(previous_dir) or {} }
	assert input_is_legacy_diagnostic_fixture('.')
	assert input_is_legacy_diagnostic_fixture('.' + os.path_separator)
	os.chdir(previous_dir)!

	vv_path := os.join_path(root, 'vlib', 'v', 'checker', 'tests', 'sample.vv')
	os.write_file(vv_path, 'fn main() {}')!
	os.write_file(vv_path.all_before_last('.vv') + '.out', 'expected diagnostic')!
	assert input_is_legacy_diagnostic_fixture(vv_path)

	project_dir := os.join_path(root, 'application')
	os.mkdir_all(project_dir)!
	os.write_file(os.join_path(project_dir, 'main.v'), 'fn main() {}')!
	os.write_file(project_dir + '.out', 'application output')!
	assert !input_is_legacy_diagnostic_fixture(project_dir)
}
