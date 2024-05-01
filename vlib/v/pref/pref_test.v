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

	// tcc does not handle the symver assembly directive which is
	// a problem on FreeBSD 14
	mut compiler := 'tcc'
	$if freebsd && clang {
		compiler = 'clang'
	}

	v_verbose_cmd_with_additional_args_res := os.execute_opt('${vexe} -cc ${compiler} -v run ${example_path}')!.output
	assert v_verbose_cmd_with_additional_args_res != v_ver_cmd_res
	assert v_verbose_cmd_with_additional_args_res.contains('v.pref.lookup_path:')
}

fn test_v_cmds_and_flags() {
	build_cmd_res := os.execute('${vexe} build ${vroot}/examples/hello_world.v')
	assert build_cmd_res.output.trim_space() == 'Use `v ${vroot}/examples/hello_world.v` instead.'

	too_many_targets_res := os.execute('${vexe} ${vroot}/examples/hello_world.v ${vroot}/examples/fizz_buzz.v')
	assert too_many_targets_res.output.trim_space() == 'Too many targets. Specify just one target: <target.v|target_directory>.'

	unkown_arg_res := os.execute('${vexe} -xyz')
	assert unkown_arg_res.output.trim_space() == 'Unknown argument `-xyz`'

	unkown_arg_for_cmd_res := os.execute('${vexe} build-module -xyz ${vroot}/vlib/math')
	assert unkown_arg_for_cmd_res.output.trim_space() == 'Unknown argument `-xyz` for command `build-module`'
}
