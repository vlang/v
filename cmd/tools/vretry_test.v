import os

const vexe = @VEXE

fn test_retry() {
	tpath := os.join_path(os.vtmp_dir(), 'vretry_test')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}

	fail_cmd := os.execute('${vexe} retry git asdf')
	assert fail_cmd.exit_code != 0
	assert fail_cmd.output.contains('error: exceeded maximum number of retries')

	with_flags_fail_cmd := os.execute('${vexe} retry -d 0.2 -r 3 git asdf')
	assert with_flags_fail_cmd.exit_code != 0
	assert with_flags_fail_cmd.output.contains('error: exceeded maximum number of retries (3)!')

	pass_cmd := os.execute('${vexe} retry git branch')
	assert pass_cmd.exit_code == 0
	assert pass_cmd.output.contains('master')

	// Include flags on the cmd as well.
	with_falgs_pass_cmd_with_falgs := os.execute('${vexe} retry -r 3 -- git branch --list')
	assert with_falgs_pass_cmd_with_falgs.exit_code == 0
	assert with_falgs_pass_cmd_with_falgs.output == os.execute('git branch --list').output
}
