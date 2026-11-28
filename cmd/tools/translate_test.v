import os

const c2v_name = 'c2v' + $if windows { '.exe' } $else { '' }

fn testsuite_begin() {
	// The installed C2V fixture is a copy of this executable.
	if os.file_name(os.executable()) != c2v_name {
		return
	}
	out := os.getenv('C2V_ARGS_OUT')
	if out == '' {
		eprintln('missing C2V_ARGS_OUT')
		exit(1)
	}
	os.write_file(out, os.args[1..].join('\n')) or {
		eprintln(err)
		exit(2)
	}
	exit(0)
}

fn test_translate_legacy_wrapper_flag_uses_wrapper_mode() {
	check_translate_wrapper(false)!
}

fn test_translate_builds_missing_c2v_binary() {
	check_translate_wrapper(true)!
}

fn check_translate_wrapper(build_missing_binary bool) ! {
	test_dir := os.join_path(os.vtmp_dir(), 'translate_test_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	vmodules_dir := os.join_path(test_dir, 'vmodules')
	c2v_dir := os.join_path(vmodules_dir, 'c2v')
	os.mkdir_all(c2v_dir)!
	c2v_bin := os.join_path(c2v_dir, c2v_name)
	if build_missing_binary {
		os.write_file(os.join_path(c2v_dir, 'fake_c2v.v'), fake_c2v_source)!
	} else {
		os.cp(os.executable(), c2v_bin)!
	}
	sample_path := os.join_path(test_dir, 'usersapi.c')
	args_out := os.join_path(test_dir, 'c2v_args.txt')
	os.write_file(sample_path, 'int usersapi_get_number_of_users(void) { return 1; }\n')!
	original_c2v_args_out := os.getenv('C2V_ARGS_OUT')
	original_vmodules := os.getenv('VMODULES')
	os.setenv('C2V_ARGS_OUT', args_out, true)
	os.setenv('VMODULES', vmodules_dir, true)
	defer {
		restore_env('C2V_ARGS_OUT', original_c2v_args_out)
		restore_env('VMODULES', original_vmodules)
	}
	mut process := os.new_process(@VEXE)
	process.set_work_folder(@VEXEROOT)
	process.set_args(['translate', '-wrapper', sample_path])
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp()
	stderr := process.stderr_slurp()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	assert stdout.contains('Compiling c2v ...') == build_missing_binary, stdout
	assert os.is_file(c2v_bin)
	args := os.read_file(args_out)!.split_into_lines()
	assert args == ['wrapper', sample_path], args.str()
}

const fake_c2v_source = "import os

fn main() {
	out := os.getenv('C2V_ARGS_OUT')
	if out == '' {
		eprintln('missing C2V_ARGS_OUT')
		exit(1)
	}
	os.write_file(out, os.args[1..].join('\\n')) or {
		eprintln(err)
		exit(2)
	}
}
"

fn restore_env(name string, value string) {
	if value == '' {
		os.unsetenv(name)
	} else {
		os.setenv(name, value, true)
	}
}
