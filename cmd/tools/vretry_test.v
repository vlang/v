import os

const vexe = @VEXE

fn test_retry() {
	tpath := os.join_path(os.vtmp_dir(), 'vretry_test')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}

	fail_cmd := 'git asdf'
	if os.getenv('CI') != 'true' {
		// Skip longer running test in CI runs.
		res := os.execute('${vexe} retry ${fail_cmd}')
		assert res.exit_code != 0
		assert res.output.contains('error: exceeded maximum number of retries')
	}

	mut res := os.execute('${vexe} retry -d 0.2 -r 3 ${fail_cmd}')
	assert res.exit_code != 0
	assert res.output.contains('error: exceeded maximum number of retries (3)!')

	os.chdir(os.dir(vexe))!
	pass_cmd := 'git branch'
	res = os.execute('${vexe} retry ${pass_cmd}')
	assert res.exit_code == 0
	assert res.output == os.execute(pass_cmd).output

	// Include flags on the cmd as well.
	pass_cmd_with_flags := 'git branch --list'
	res = os.execute('${vexe} retry -r 3 -- ${pass_cmd_with_flags}')
	assert res.exit_code == 0
	assert res.output == os.execute(pass_cmd_with_flags).output
}
