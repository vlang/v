import v.pref
import v.vmod
import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn test_check_parametes() {
	// reproducing issue https://github.com/vlang/v/issues/13983
	_, cmd := pref.parse_args_and_show_errors(['help'], [''], true)
	// no command found from args
	assert cmd == ''
}

fn test_version_flag() {
	v_ver := vmod.from_file(os.join_path(vroot, 'v.mod'))!.version
	v_ver_cmd_res := os.execute_opt('${vexe} --version')!.output
	assert v_ver_cmd_res.starts_with('V ${v_ver}'), v_ver_cmd_res

	v_retry_ver_cmd_res := os.execute_opt('${vexe} retry --version')!.output
	assert v_retry_ver_cmd_res != v_ver_cmd_res

	v_git_ver_subcmd_res := os.execute_opt('${vexe} retry -- git --version')!.output
	assert v_git_ver_subcmd_res !in [v_ver_cmd_res, v_retry_ver_cmd_res]

	// Test version / verbosity toggle.
	assert os.execute_opt('${vexe} -v')!.output == v_ver_cmd_res
	assert os.execute_opt('${vexe} -cc tcc -v')!.output == v_ver_cmd_res

	example_path := os.join_path(vroot, 'examples', 'hello_world.v')
	v_verbose_cmd_res := os.execute_opt('${vexe} -v run ${example_path}')!.output
	assert v_verbose_cmd_res != v_ver_cmd_res
	assert v_verbose_cmd_res.contains('v.pref.lookup_path:')

	v_verbose_cmd_with_additional_args_res := os.execute_opt('${vexe} -g -v run ${example_path}')!.output
	assert v_verbose_cmd_with_additional_args_res != v_ver_cmd_res
	assert v_verbose_cmd_with_additional_args_res.contains('v.pref.lookup_path:')
}

fn test_v_cmds_and_flags() {
	build_cmd_res := os.execute('${vexe} build ${vroot}/examples/hello_world.v')
	assert build_cmd_res.output.trim_space() == 'Use `v ${vroot}/examples/hello_world.v` instead.'

	too_many_targets_res := os.execute('${vexe} ${vroot}/examples/hello_world.v ${vroot}/examples/fizz_buzz.v')
	assert too_many_targets_res.output.trim_space() == 'Too many targets. Specify just one target: <target.v|target_directory>.'

	unknown_arg_res := os.execute('${vexe} -xyz')
	assert unknown_arg_res.output.trim_space() == 'Unknown argument `-xyz`'

	unknown_arg_for_cmd_res := os.execute('${vexe} build-module -xyz ${vroot}/vlib/math')
	assert unknown_arg_for_cmd_res.output.trim_space() == 'Unknown argument `-xyz` for command `build-module`'

	no_run_files_res := os.execute('${vexe} run')
	assert no_run_files_res.output.trim_space() == 'v run: no v files listed'

	no_bm_files_res := os.execute('${vexe} build-module')
	assert no_bm_files_res.output.trim_space() == 'v build-module: no module specified'
}

const tfile = os.join_path(os.vtmp_dir(), 'unknown_options_output.c')

fn test_unknown_option_flags_no_run() {
	os.chdir(os.dir(@VEXE))!
	os.rm(tfile) or {}

	res1 := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} examples/hello_world.v --an-unknown-option')
	assert res1.exit_code == 1, res1.output
	assert res1.output.starts_with('Unknown argument')
	assert res1.output.contains('--an-unknown-option')
	assert !os.exists(tfile)

	res2 := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} --an-unknown-option examples/hello_world.v')
	assert res2.exit_code == 1, res2.output
	assert res2.output.starts_with('Unknown argument')
	assert res2.output.contains('--an-unknown-option')
	assert !os.exists(tfile)
}

fn test_unknown_option_flags_with_run() {
	res_run_o := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} run examples/hello_world.v --an-unknown-option')
	assert res_run_o.exit_code == 0, res_run_o.output
	assert res_run_o.output == '' // because of -o, there should not be an actual run, since compilation stopped after generating the .c file
	assert os.exists(tfile)
	os.rm(tfile) or {}

	res_run_no_o_unknown_before_run := os.execute('${os.quoted_path(@VEXE)} --an-unknown-option run examples/hello_world.v ')
	assert res_run_no_o_unknown_before_run.exit_code == 1, res_run_no_o_unknown_before_run.output
	assert res_run_no_o_unknown_before_run.output.starts_with('Unknown argument')
	assert res_run_no_o_unknown_before_run.output.contains('--an-unknown-option')
	assert !os.exists(tfile)

	res_run_no_o := os.execute('${os.quoted_path(@VEXE)} run examples/hello_world.v --an-unknown-option')
	assert res_run_no_o.exit_code == 0, res_run_no_o.output
	assert res_run_no_o.output.trim_space() == 'Hello, World!'
	assert !os.exists(tfile)
}
