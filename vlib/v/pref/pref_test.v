module pref

import v.vmod
import os

fn test_check_parametes() {
	// reproducing issue https://github.com/vlang/v/issues/13983
	_, cmd := parse_args_and_show_errors(['help'], [''], true)
	// no command found from args
	assert cmd == ''
}

fn test_version_falg() {
	// Vars instead of consts to prevent dupl decls in pref.
	vexe := @VEXE
	vroot := os.dir(vexe)

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

	v_verbose_cmd_with_additional_args_res := os.execute_opt('${vexe} -cc tcc -v run ${example_path}')!.output
	assert v_verbose_cmd_with_additional_args_res != v_ver_cmd_res
	assert v_verbose_cmd_with_additional_args_res.contains('v.pref.lookup_path:')
}
