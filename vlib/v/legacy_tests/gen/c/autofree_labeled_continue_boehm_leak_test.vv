import os

const vroot = os.dir(@VEXE)
const test_vexe = os.quoted_path(@VEXE)
const testdata_file = os.join_path(vroot,
	'vlib/v/gen/c/testdata/autofree_labeled_continue_boehm_leak.vv')

fn missing_boehm_leak_lib(output string) bool {
	return output.contains('ld: cannot find -lgc') || output.contains('library not found for -lgc')
		|| output.contains('gc/gc.h') || output.contains('bdw-gc')
}

fn test_autofree_labeled_continue_boehm_leak_cleanup() {
	mut exe_path := os.join_path(os.vtmp_dir(), 'autofree_labeled_continue_boehm_leak')
	$if windows {
		exe_path += '.exe'
	}
	cmd := '${test_vexe} -autofree -gc boehm_leak -o ${os.quoted_path(exe_path)} ${os.quoted_path(testdata_file)}'
	res := os.execute(cmd)
	if res.exit_code != 0 && missing_boehm_leak_lib(res.output) {
		eprintln('skipping boehm_leak labeled continue cleanup test: missing libgc')
		return
	}
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	run_res := os.execute(os.quoted_path(exe_path))
	assert run_res.exit_code == 0, run_res.output
	os.rm(exe_path) or {}
}
