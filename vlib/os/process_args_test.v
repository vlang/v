import os

const vexe = os.getenv('VEXE')
const vroot = os.dir(vexe)

// strip_tcc_fallback_warning removes the noisy macOS-only `warning: tcc compilation
// failed, falling back to cc` line that v emits to stderr and that os.execute folds
// into the captured output. The CI path-with-spaces+comma jobs trigger this fallback,
// which would otherwise prefix the program output and break starts_with assertions.
fn strip_tcc_fallback_warning(output string) string {
	mut s := output
	for s.contains('tcc compilation failed') {
		nl := s.index('\n') or { return s }
		s = s[nl + 1..]
	}
	return s
}

fn test_v_run_simple() {
	echo_os_args := os.join_path(vroot, 'cmd/tools/test_os_args.v')
	res123 := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(echo_os_args)} 1 2 3')
	println(res123)
	assert res123.exit_code == 0
	assert strip_tcc_fallback_warning(res123.output).starts_with("['1', '2', '3']")
}

fn test_v_run_quoted_args_with_spaces() {
	echo_os_args := os.join_path(vroot, 'cmd/tools/test_os_args.v')
	res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(echo_os_args)} 1 "Learn V" 3')
	println(res)
	assert res.exit_code == 0
	assert strip_tcc_fallback_warning(res.output).starts_with("['1', 'Learn V', '3']")
}

fn test_v_run_quoted_args_with_spaces__use_os_system_to_run() {
	echo_os_args := os.join_path(vroot, 'cmd/tools/test_os_args.v')
	res :=
		os.execute('${os.quoted_path(vexe)} -use-os-system-to-run run ${os.quoted_path(echo_os_args)} 1 "Learn V" 3')
	println(res)
	assert res.exit_code == 0
	assert strip_tcc_fallback_warning(res.output).starts_with("['1', 'Learn V', '3']")
}

fn test_v_run_file_from_path_with_spaces() {
	echo_os_args := os.join_path(vroot, 'cmd/tools/test_os_args.v')
	spaced_dir := os.join_path(os.vtmp_dir(), 'v run path with spaces')
	spaced_echo_os_args := os.join_path(spaced_dir, 'test os args.v')
	os.rmdir_all(spaced_dir) or {}
	os.mkdir_all(spaced_dir)!
	defer {
		os.rmdir_all(spaced_dir) or {}
	}
	os.cp(echo_os_args, spaced_echo_os_args)!
	res :=
		os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(spaced_echo_os_args)} 1 "Learn V" 3')
	println(res)
	assert res.exit_code == 0
	assert strip_tcc_fallback_warning(res.output).starts_with("['1', 'Learn V', '3']")
}
