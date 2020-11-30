import os

const (
	vexe = os.getenv('VEXE')
	vroot = os.dir(vexe)
	basepath = os.real_path(os.join_path(vroot, 'vlib', 'v', 'tests', 'multiple_paths_in_vmodules'))
	mainvv = os.join_path(basepath, 'main.vv')
)

fn test_vexe_is_set() {
	assert vexe != ''
	println('vexe: $vexe')
}

fn test_compiling_without_vmodules_fails() {
	os.chdir(vroot)
	os.setenv('VMODULES', '', true)
	res := os.exec('"$vexe" run "$mainvv"') or {
		panic(err)
	}
	assert res.exit_code == 1
	assert res.output.trim_space() == 'builder error: cannot import module "yyy" (not found)'
}

fn test_compiling_with_vmodules_works() {
	os.chdir(vroot)	
	vmpaths := ['path1', 'path2', 'path3'].map(os.join_path(basepath, it))	
	os.setenv('VMODULES', vmpaths.join(os.path_delimiter), true)
	res := os.exec('"$vexe" run "$mainvv"') or {
		panic(err)
	}
	assert res.exit_code == 0
	assert res.output.trim_space() == "['x', 'y', 'z']"
}
