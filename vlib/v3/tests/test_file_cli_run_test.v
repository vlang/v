import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_test_file_cli_runner() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_test_file_cli_runner')
	build :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// test_direct_test_file_run_executes_harness validates `v3 foo_test.v` behavior.
fn test_direct_test_file_run_executes_harness() {
	v3_bin := build_v3_test_file_cli_runner()
	tmp_dir := os.join_path(os.temp_dir(), 'v3_test_file_cli_run_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	fail_src := os.join_path(tmp_dir, 'failing_test.v')
	os.write_file(fail_src, 'fn test_failure() {\n\tassert false\n}\n')!
	fail := os.execute('cd ${os.quoted_path(tmp_dir)} && ${os.quoted_path(v3_bin)} failing_test.v')
	assert fail.exit_code != 0, fail.output
	assert fail.output.contains('assert failed'), fail.output

	pass_src := os.join_path(tmp_dir, 'passing_test.v')
	os.write_file(pass_src, 'fn test_success() {\n\tassert true\n}\n')!
	pass := os.execute('cd ${os.quoted_path(tmp_dir)} && ${os.quoted_path(v3_bin)} passing_test.v')
	assert pass.exit_code == 0, pass.output
}

fn test_directory_test_command_sets_test_define_before_collecting_inputs() {
	v3_bin := build_v3_test_file_cli_runner()
	tmp_dir := os.join_path(os.temp_dir(), 'v3_test_directory_define_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	support_src := os.join_path(tmp_dir, 'support_d_test.v')
	os.write_file(support_src, 'fn test_only_value() bool {
	return true
}
')!
	test_src := os.join_path(tmp_dir, 'directory_test.v')
	os.write_file(test_src, 'fn test_test_define_and_support_file() {
	$if test {
		assert test_only_value()
	} $else {
		assert false
	}
}
')!
	bin := os.join_path(tmp_dir, 'directory_tests')
	result :=
		os.execute('${os.quoted_path(v3_bin)} test ${os.quoted_path(tmp_dir)} -o ${os.quoted_path(bin)}')
	assert result.exit_code == 0, result.output
}

fn test_directory_test_command_filters_define_suffixes() {
	v3_bin := build_v3_test_file_cli_runner()
	tmp_dir := os.join_path(os.temp_dir(), 'v3_test_directory_suffixes_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	os.write_file(os.join_path(tmp_dir, 'feature_d_ssl_test.v'), 'fn test_with_ssl() {
	$if ssl ? {
		assert true
	} $else {
		assert false
	}
}
')!
	os.write_file(os.join_path(tmp_dir, 'feature_notd_ssl_test.v'), 'fn test_without_ssl() {
	$if ssl ? {
		assert false
	} $else {
		assert true
	}
}
')!
	without_ssl_bin := os.join_path(tmp_dir, 'tests_without_ssl')
	without_ssl :=
		os.execute('${os.quoted_path(v3_bin)} test ${os.quoted_path(tmp_dir)} -o ${os.quoted_path(without_ssl_bin)}')
	assert without_ssl.exit_code == 0, without_ssl.output

	with_ssl_bin := os.join_path(tmp_dir, 'tests_with_ssl')
	with_ssl :=
		os.execute('${os.quoted_path(v3_bin)} -d ssl test ${os.quoted_path(tmp_dir)} -o ${os.quoted_path(with_ssl_bin)}')
	assert with_ssl.exit_code == 0, with_ssl.output
}

// test_run_forwards_all_args_after_input_file validates that `v3 run app.v ...`
// forwards every argument after the input file to the program, including
// `-`-prefixed flags like `--help` (which must not be swallowed as compiler flags).
fn test_run_forwards_all_args_after_input_file() {
	v3_bin := build_v3_test_file_cli_runner()
	tmp_dir := os.join_path(os.temp_dir(), 'v3_run_args_cli_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}

	app_src := os.join_path(tmp_dir, 'app.v')
	os.write_file(app_src, "import os\nfn main() {\n\tprintln(os.args[1..].join('|'))\n}\n")!
	res :=
		os.execute('cd ${os.quoted_path(tmp_dir)} && ${os.quoted_path(v3_bin)} run app.v --help pos --flag=1')
	assert res.exit_code == 0, res.output
	assert res.output.contains('--help|pos|--flag=1'), res.output
}
