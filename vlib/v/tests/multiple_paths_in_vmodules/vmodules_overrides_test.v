import os

const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

const basepath = os.real_path(os.join_path(vroot, 'vlib', 'v', 'tests', 'multiple_paths_in_vmodules'))

const mainvv = os.join_path(basepath, 'main.vv')

fn test_vexe_is_set() {
	assert vexe != ''
	println('vexe: ${vexe}')
}

fn test_compiling_without_vmodules_fails() {
	os.chdir(vroot) or {}
	os.setenv('VMODULES', '', true)
	cmd := '${os.quoted_path(vexe)} run ${os.quoted_path(mainvv)}'
	dump(cmd)
	res := os.execute(cmd)
	assert res.exit_code == 1
	dump(res)
	assert res.output.trim_space().contains('builder error: cannot import module "yyy" (not found)')
}

fn test_compiling_with_vmodules_works() {
	os.chdir(vroot) or {}
	vmpaths := ['path1', 'path2', 'path3'].map(os.join_path(basepath, it))
	os.setenv('VMODULES', vmpaths.join(os.path_delimiter), true)
	res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(mainvv)}')
	assert res.exit_code == 0
	assert res.output.trim_space() == "['x', 'y', 'z']"
}
