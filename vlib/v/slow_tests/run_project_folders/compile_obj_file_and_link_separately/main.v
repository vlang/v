module main

import os

const tfolder = os.join_path(os.vtmp_dir(), 'vobjfile_${os.getuid()}')
const vexe = os.quoted_path(@VEXE)
const gcc = os.quoted_path(os.find_abs_path_of_executable('gcc') or {
	println('This program needs `gcc` to be present.')
	println('OK')
	exit(0)
})

fn lexec(cmd string) os.Result {
	println('>>> lexec cmd: ${cmd}')
	res := os.execute_or_exit(cmd)
	return res
}

fn main() {
	os.chdir(@DIR)!
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder)!
	lexec('${vexe} -is_o -o "${tfolder}/abc.o"        -gc none -cc ${gcc} abc/')
	lexec('${gcc}        -o "${tfolder}/main_in_c.o"  -c main_in_c.c')
	lexec('${gcc}        -o "${tfolder}/program.exe"  "${tfolder}/abc.o"   "${tfolder}/main_in_c.o"')
	res := lexec('${tfolder}/program.exe')
	print(res.output)
	assert res.output.starts_with('Result of addition: 30')
	os.rmdir_all(tfolder) or {}
	println('OK')
}
