import os
import log

const vexe = @VEXE
const vroot = os.dir(vexe)
const retry_exe = os.executable()
const retry_probe = 'vretry-argv-probe'
const failing_args = ['too', 'many', 'arguments']

const is_ci = os.getenv('CI') != ''

fn testsuite_begin() {
	if os.args.len > 1 && os.args[1] == retry_probe {
		args := os.args[2..]
		println(args)
		exit(if args == failing_args { 1 } else { 0 })
	}
}

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
	return os.exec(os.split_args(cmd) or { panic(err) })
}

fn test_retry() {
	log.use_stdout()
	log.warn('start...')
	defer {
		log.warn('... done')
	}
	fail_args := [retry_exe, retry_probe, ...failing_args]
	failure_output := failing_args.str()
	// Leave the retry limit at its default while removing the wait between attempts.
	default_limit := os.exec([vexe, 'retry', '-d', '0', '--', ...fail_args])
	assert default_limit.exit_code == 1, default_limit.output
	assert default_limit.output.contains('error: exceeded maximum number of retries (10)!'), default_limit.output
	assert default_limit.output.count(failure_output) == 10, default_limit.output

	mut res := os.exec([vexe, 'retry', '-d', '0.2', '-r', '3', '--', ...fail_args])
	dump_on_ci(res)
	assert res.exit_code == 1, res.output
	assert res.output.contains('error: exceeded maximum number of retries (3)!'), res.output
	assert res.output.count(failure_output) == 3, res.output

	// Without `--`; the command is passed as separate arguments. One pre-quoted
	// string would be quoted again on Windows and reach the shell as `\"path\"`.
	res = os.exec([vexe, 'retry', retry_exe, retry_probe])
	dump_on_ci(res)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '[]', res.output

	// Include flags on the cmd as well.
	res = os.exec([vexe, 'retry', '-r', '3', '--', retry_exe, retry_probe, '--list', '-x', '--',
		'-b', 'js', 'arguments'])
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

// Wrapping an argument in quotes is not enough on Windows: a trailing backslash
// escapes the closing quote, so `C:\work space\` swallows whatever follows it. Pass
// such an argument with another one behind it and check both arrive whole. On unix
// the single-quoted form has to keep the backslash literal for the same reason.
fn test_retry_keeps_a_trailing_backslash_argument_separate() {
	log.use_stdout()
	trailing := 'with space\\'
	res := os.exec([vexe, 'retry', '-r', '1', '--', retry_exe, retry_probe, trailing, 'b'])
	dump_on_ci(res)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == [trailing, 'b'].str(), res.output
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

// Direct and launcher runs both pass the first user argument to the parser.
// Cover a first flag, the vector separator, and a real command named retry.
fn test_retry_binary_run_directly_keeps_its_first_argument() {
	log.use_stdout()
	tpath := os.join_path(os.vtmp_dir(), 'vretry direct test ${os.getpid()}')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}
	mut binary := os.join_path(tpath, 'vretry')
	$if windows {
		binary += '.exe'
	}
	build := os.exec([vexe, '-cc', @CCOMPILER, '-o', binary,
		os.join_path(vroot, 'cmd', 'tools', 'vretry.v')])
	assert build.exit_code == 0, build.output

	first_flag := os.exec([binary, '-r', '1', '--', vexe, 'version'])
	dump_on_ci(first_flag)
	assert first_flag.exit_code == 0, first_flag.output

	first_dashdash := os.exec([binary, '--', vexe, 'version'])
	dump_on_ci(first_dashdash)
	assert first_dashdash.exit_code == 0, first_dashdash.output

	limit := os.exec([binary, '-r', '2', '-d', '0', '--', 'vretry-command-that-does-not-exist'])
	dump_on_ci(limit)
	assert limit.exit_code != 0
	assert limit.output.contains('exceeded maximum number of retries (2)!'), limit.output

	// A bare `retry` is a command name, including when no options precede it.
	$if windows {
		os.write_file(os.join_path(tpath, 'retry.cmd'), '@echo direct-retry-command:%1\r\n')!
	} $else {
		command := os.join_path(tpath, 'retry')
		os.write_file(command, '#!/bin/sh\nprintf "direct-retry-command:%s\\n" "$1"\n')!
		os.chmod(command, 0o700)!
	}
	old_path := os.getenv('PATH')
	os.setenv('PATH', tpath + os.path_delimiter + old_path, true)
	defer { os.setenv('PATH', old_path, true) }
	for command_args in [[binary, 'retry', 'kept'], [vexe, 'retry', 'retry', 'kept']] {
		result := os.exec(command_args)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'direct-retry-command:kept', result.output
	}
}
