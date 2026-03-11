// vtest build: !musl? && !sanitized_job?
module main

import os
import v.slow_tests.repl.runner

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

fn test_repl_can_import_installed_submodules_from_vmodules() {
	old_vmodules := os.getenv('VMODULES')
	temp_dir := os.join_path(os.vtmp_dir(), 'v_repl_import_vmodules_submodule_${os.getpid()}')
	vmodules_dir := os.join_path(temp_dir, '.vmodules')
	module_root := os.join_path(vmodules_dir, 'importerror')
	submodule_dir := os.join_path(module_root, 'a')
	repl_dir := os.join_path(temp_dir, 'repl')
	defer {
		if old_vmodules == '' {
			os.unsetenv('VMODULES')
		} else {
			os.setenv('VMODULES', old_vmodules, true)
		}
		os.rmdir_all(temp_dir) or {}
	}
	os.rmdir_all(temp_dir) or {}
	os.mkdir_all(submodule_dir) or { panic(err) }
	os.mkdir_all(repl_dir) or { panic(err) }
	os.write_file(os.join_path(submodule_dir, 'a.v'), 'module a

pub fn testme() int {
	return 1
}
') or {
		panic(err)
	}
	os.setenv('VMODULES', vmodules_dir, true)
	vexec := runner.full_path_to_v(5)
	mut p := os.new_process(vexec)
	p.set_args(['repl', '-replfolder', repl_dir, '-replprefix', 'import_vmodules_submodule.'])
	p.set_work_folder(module_root)
	p.set_redirect_stdio()
	p.run()
	p.stdin_write('import importerror.a\na.testme()\nexit\n')
	p.wait()
	output := p.stdout_slurp()
	errors := p.stderr_slurp()
	p.close()
	assert p.code == 0, 'stdout: ${output}\nstderr: ${errors}'
	assert output.trim_space() == '1', 'stdout: ${output}\nstderr: ${errors}'
}
