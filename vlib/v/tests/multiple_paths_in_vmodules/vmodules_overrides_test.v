// vtest build: !sanitize-memory-gcc && !sanitized_job?
import os

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

const basepath = os.real_path(os.join_path(vroot, 'vlib', 'v', 'tests',
	'multiple_paths_in_vmodules'))

const mainvv = os.join_path(basepath, 'main.vv')

const cmd = '${os.quoted_path(vexe)} run ${os.quoted_path(mainvv)}'

const submodule_mainvv = os.join_path(basepath, 'submodule_of_third_party_main.vv')

const submodule_cmd = '${os.quoted_path(vexe)} ${os.quoted_path(submodule_mainvv)}'

const installed_module_root = os.join_path(basepath, 'path5', 'vab')

fn test_vexe_is_set() {
	assert vexe != ''
	println('vexe: ${vexe}')
}

fn test_compiling_without_vmodules_fails() {
	os.chdir(vroot) or {}
	os.setenv('VMODULES', '', true)
	dump(cmd)
	res := os.execute(cmd)
	assert res.exit_code == 1, res.output
	assert res.output.trim_space().contains('builder error: cannot import module "yyy" (not found)')
}

fn test_compiling_with_vmodules_works() {
	os.chdir(vroot) or {}
	vmpaths := ['path1', 'path2', 'path3'].map(os.join_path(basepath, it))
	os.setenv('VMODULES', vmpaths.join(os.path_delimiter), true)
	dump(os.getenv('VMODULES'))
	dump(cmd)
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == "['x', 'y', 'z']"
}

fn test_importing_third_party_submodule_works() {
	os.chdir(vroot) or {}
	os.setenv('VMODULES', os.join_path(basepath, 'path4'), true)
	dump(os.getenv('VMODULES'))
	dump(submodule_cmd)
	res := os.execute(submodule_cmd)
	assert res.exit_code == 0, res.output
}

fn test_running_installed_module_with_short_submodule_imports_works() {
	old_dir := os.getwd()
	defer {
		os.chdir(old_dir) or {}
	}
	os.chdir(installed_module_root) or {}
	os.setenv('VMODULES', os.join_path(basepath, 'path5'), true)
	run_cmd := '${os.quoted_path(vexe)} run vab.v'
	dump(os.getenv('VMODULES'))
	dump(run_cmd)
	res := os.execute(run_cmd)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'android sdk'
}
