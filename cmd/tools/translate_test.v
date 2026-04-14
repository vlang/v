import os

const qvexe = os.quoted_path(@VEXE)

fn test_translate_legacy_wrapper_flag_uses_wrapper_mode() {
	test_dir := os.join_path(os.vtmp_dir(), 'translate_test_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	vmodules_dir := os.join_path(test_dir, 'vmodules')
	compile_fake_c2v(vmodules_dir)!
	sample_path := os.join_path(test_dir, 'usersapi.c')
	args_out := os.join_path(test_dir, 'c2v_args.txt')
	os.write_file(sample_path, 'int usersapi_get_number_of_users(void) { return 1; }\n')!
	mut process := os.new_process(@VEXE)
	process.set_work_folder(@VEXEROOT)
	process.set_args(['translate', '-wrapper', sample_path])
	process.set_environment({
		'C2V_ARGS_OUT': args_out
		'VMODULES':     vmodules_dir
	})
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp()
	stderr := process.stderr_slurp()
	exit_code := process.code
	process.close()
	assert exit_code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
	args := os.read_file(args_out)!.split_into_lines()
	assert args == ['wrapper', sample_path], args.str()
}

fn compile_fake_c2v(vmodules_dir string) ! {
	c2v_dir := os.join_path(vmodules_dir, 'c2v')
	os.mkdir_all(c2v_dir)!
	fake_c2v_path := os.join_path(c2v_dir, 'fake_c2v.v')
	c2v_bin := os.join_path(c2v_dir, 'c2v' + exe_suffix())
	fake_c2v_source :=
		['import os', '', 'fn main() {', "\tout := os.getenv('C2V_ARGS_OUT')", "\tif out == '' {", "\t\teprintln('missing C2V_ARGS_OUT')", '\t\texit(1)', '\t}', "\tos.write_file(out, os.args[1..].join('\\n')) or {", '\t\teprintln(err)', '\t\texit(2)', '\t}', '}'].join('\n') +
		'\n'
	os.write_file(fake_c2v_path, fake_c2v_source)!
	res := os.execute('${qvexe} -o ${os.quoted_path(c2v_bin)} ${os.quoted_path(fake_c2v_path)}')
	assert res.exit_code == 0, res.output
}

fn exe_suffix() string {
	return if os.user_os() == 'windows' { '.exe' } else { '' }
}
