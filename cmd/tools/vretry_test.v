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

// The command reaches `v retry` as an argument vector, whatever quoting the caller's
// own shell already removed, and is run through a shell again. Joining that vector
// with plain spaces split a destination like `C:\Users\Jane Doe\.vmodules\markdown`
// back into two arguments, which is how `v build-tools` failed to install `markdown`
// on a checkout under such a path.
fn test_retry_keeps_arguments_that_contain_spaces_together() {
	log.use_stdout()
	tpath := os.join_path(os.vtmp_dir(), 'vretry space test ${os.getpid()}')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}
	// `v retry -- git init <dir with spaces>`: the directory has to arrive as one
	// argument, or git reports a usage error over the extra ones.
	target := os.join_path(tpath, 'a repo')
	res := run('${os.quoted_path(vexe)} retry -r 1 -- git init -q ${os.quoted_path(target)}')
	dump_on_ci(res)
	assert res.exit_code == 0, res.output
	assert os.is_dir(os.join_path(target, '.git')), 'the spaced destination was split: ${res.output}'
}

// A command can also be given as shell syntax in one argument, which is the form the
// SDL workflow uses: `v retry 'sudo apt update'`. That argument has to reach the shell
// untouched - quoting it asks for a program whose name contains spaces, which fails
// with `command not found` before the dependencies are ever installed.
fn test_retry_runs_a_command_given_as_one_shell_string() {
	log.use_stdout()
	res := run("${os.quoted_path(vexe)} retry -r 1 'echo one two three'")
	dump_on_ci(res)
	assert res.exit_code == 0, res.output
	assert !res.output.contains('command not found'), res.output
	assert res.output.trim_space().ends_with('one two three'), res.output
}

// The same form, with quoting of its own inside the string.
fn test_retry_keeps_shell_syntax_inside_one_command_string() {
	$if windows {
		return
	}
	log.use_stdout()
	res := run('${os.quoted_path(vexe)} retry -r 1 "sh -c \'echo four five\'"')
	dump_on_ci(res)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().ends_with('four five'), res.output
}

// Quoting the caller put inside a single argument has to survive too, so that a
// command like `sh -c 'echo one two'` still receives one argument after `-c`.
fn test_retry_keeps_a_quoted_argument_whole() {
	$if windows {
		return
	}
	log.use_stdout()
	res := run("${os.quoted_path(vexe)} retry -r 1 -- sh -c 'echo one two'")
	dump_on_ci(res)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().ends_with('one two'), res.output
}
