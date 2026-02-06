// This test uses the script cmd/tools/check_retry.vsh
import os
import log

const vexe = @VEXE
const vroot = os.dir(vexe)

const is_ci = os.getenv('CI') != ''

fn dump_on_ci[T](x T) {
	if is_ci {
		dump(x)
	}
}

fn run(cmd string) os.Result {
	log.info('>>>  running cmd: ${cmd}')
	defer {
		log.info('>>> finished cmd: ${cmd}')
	}
	return os.execute(cmd)
}

fn test_retry() {
	log.use_stdout()
	log.warn('start...')
	defer {
		log.warn('... done')
	}
	tpath := os.join_path(os.vtmp_dir(), 'vretry_test')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}
	os.chdir(vroot)!
	fail_cmd := '${vexe} run cmd/tools/check_retry.vsh too many arguments'
	if is_ci {
		// Skip longer running test on local runs.
		res := run('${vexe} retry ${fail_cmd}')
		assert res.exit_code != 0
		assert res.output.contains('error: exceeded maximum number of retries')
	}

	mut res := run('${vexe} retry -d 0.2 -r 3 ${fail_cmd}')
	dump_on_ci(res)
	assert res.exit_code != 0
	assert res.output.contains('error: exceeded maximum number of retries (3)!')

	pass_cmd := '${vexe} run cmd/tools/check_retry.vsh'
	res = run('${vexe} retry ${pass_cmd}')
	dump_on_ci(res)
	assert res.exit_code == 0
	assert res.output == run(pass_cmd).output

	// Include flags on the cmd as well.
	pass_cmd_with_flags := '${vexe} run cmd/tools/check_retry.vsh --list -x -- -b js arguments'
	res = run('${vexe} retry -r 3 -- ${pass_cmd_with_flags}')
	dump_on_ci(res)
	assert res.exit_code == 0
	output_trimmed := res.output.trim_space()
	assert output_trimmed == "['--list', '-x', '--', '-b', 'js', 'arguments']"
}
