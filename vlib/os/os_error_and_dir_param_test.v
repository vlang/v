import os

const walk_root_name = 'os_error_and_dir_param_tests_${os.getpid()}'

fn walk_root() string {
	root := os.join_path(os.vtmp_dir(), walk_root_name)
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn test_sigint_to_signal_name_maps_the_posix_codes() {
	// The POSIX block of the match is shared by every platform.
	assert os.sigint_to_signal_name(1) == 'SIGHUP'
	assert os.sigint_to_signal_name(2) == 'SIGINT'
	assert os.sigint_to_signal_name(3) == 'SIGQUIT'
	assert os.sigint_to_signal_name(4) == 'SIGILL'
	assert os.sigint_to_signal_name(6) == 'SIGABRT'
	assert os.sigint_to_signal_name(8) == 'SIGFPE'
	assert os.sigint_to_signal_name(9) == 'SIGKILL'
	assert os.sigint_to_signal_name(11) == 'SIGSEGV'
	assert os.sigint_to_signal_name(13) == 'SIGPIPE'
	assert os.sigint_to_signal_name(14) == 'SIGALRM'
	assert os.sigint_to_signal_name(15) == 'SIGTERM'
	// Anything outside the known codes is reported rather than guessed.
	assert os.sigint_to_signal_name(0) == 'unknown'
	assert os.sigint_to_signal_name(-1) == 'unknown'
	assert os.sigint_to_signal_name(999) == 'unknown'
}

fn test_error_posix_defaults_the_message_to_the_posix_description() {
	err := os.error_posix(code: 2)
	assert err.code() == 2
	assert err.msg() == 'No such file or directory'
}

fn test_error_posix_keeps_an_explicit_message() {
	err := os.error_posix(msg: 'a message of my own', code: 2)
	assert err.code() == 2
	assert err.msg() == 'a message of my own'
}

fn test_error_win32_renders_a_win32_code() {
	// error_win32 panics off Windows, and a panic cannot be asserted on from a
	// test, so only the Windows side of the function has anything to call.
	$if windows {
		err := os.error_win32(code: 2)
		assert err.code() == 2
		assert err.msg() == os.get_error_msg(2)
		// An explicit message wins over the system description.
		named := os.error_win32(msg: 'my message', code: 2)
		assert named.msg() == 'my message'
		assert named.code() == 2
	}
}

fn test_error_posix_reports_every_code_as_a_message() {
	// Codes are looked up with strerror(), which answers for every input.
	for code in [0, 1, 2, 13, 22, 100] {
		assert os.error_posix(code: code).msg().len > 0
	}
}

fn test_cache_dir_honours_the_xdg_environment_variable() {
	root := walk_root()
	defer {
		os.rmdir_all(root) or {}
	}
	override := os.join_path(root, 'cache-home')
	os.mkdir_all(override) or { panic(err) }
	os.setenv('XDG_CACHE_HOME', override, true)
	defer {
		os.unsetenv('XDG_CACHE_HOME')
	}
	got := os.cache_dir()
	assert got == override
	// The directory is created even when it was missing.
	created := os.join_path(root, 'created-cache')
	os.setenv('XDG_CACHE_HOME', created, true)
	assert os.cache_dir() == created
	assert os.is_dir(created)
}

fn test_mkdir_all_creates_every_missing_component() {
	root := walk_root()
	defer {
		os.rmdir_all(root) or {}
	}
	deep := os.join_path(root, 'one', 'two', 'three')
	os.mkdir_all(deep)!
	assert os.is_dir(deep)
	assert os.is_dir(os.join_path(root, 'one'))
	// An existing folder is not an error, and a file in the way is.
	os.mkdir_all(os.join_path(root, 'one'))!
	blocker := os.join_path(root, 'blocker.txt')
	os.write_file(blocker, 'x')!
	os.mkdir_all(blocker) or {
		assert err.msg() == 'path `${blocker}` already exists, and is not a folder'
		return
	}
	assert false, 'mkdir_all() over a file should have failed'
}

fn test_mkdir_mode_is_applied_on_platforms_that_honour_it() {
	root := walk_root()
	defer {
		os.rmdir_all(root) or {}
	}
	target := os.join_path(root, 'mode')
	os.mkdir(target, mode: 0o750)!
	deep := os.join_path(root, 'nested', 'deeper')
	os.mkdir_all(deep, mode: 0o700)!

	$if !windows {
		assert os.stat(target)!.mode & u32(0o777) == 0o750
		assert os.stat(deep)!.mode & u32(0o777) == 0o700
	} $else {
		// Windows mkdir takes no mode at the C level, so the stored mode is the
		// default 0o777 with the directory bit set whatever was requested.
		assert os.stat(target)!.mode & u32(0o777) == 0o777
		assert os.stat(deep)!.mode & u32(0o777) == 0o777
	}
}

// collect_path is the callback `os.walk_with_context` expects. Its first
// parameter is an opaque context, so handing it the array to append to needs a
// cast; the unsafe block covers only that cast.
fn collect_path(context voidptr, path string) {
	mut seen := unsafe { &[]string(context) }
	seen << path
}

fn test_walk_with_context_reports_every_entry_of_a_tree() {
	root := walk_root()
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'sub', 'deep')) or { panic(err) }
	os.write_file(os.join_path(root, 'top.txt'), 't') or { panic(err) }
	os.write_file(os.join_path(root, 'sub', 'middle.txt'), 'm') or { panic(err) }
	os.write_file(os.join_path(root, 'sub', 'deep', 'leaf.txt'), 'l') or { panic(err) }

	mut seen := []string{}
	os.walk_with_context(root, voidptr(&seen), collect_path)
	mut names := seen.map(os.file_name(it))
	names.sort()
	assert names == ['deep', 'leaf.txt', 'middle.txt', 'sub', 'top.txt']

	// The starting folder itself is not reported.
	assert root !in seen

	// A path that is not a directory, and the empty path, report nothing.
	mut nothing := []string{}
	os.walk_with_context('', voidptr(&nothing), collect_path)
	os.walk_with_context(os.join_path(root, 'top.txt'), voidptr(&nothing), collect_path)
	assert nothing.len == 0
}

fn test_stdio_capture_separates_stdout_from_stderr() {
	mut cap := os.stdio_capture() or { panic(err) }
	os.log('a log line')
	println('a plain line')
	eprintln('an error line')
	stdout_lines, stderr_lines := cap.finish()
	assert stdout_lines == ['os.log: a log line\na plain line\n']
	assert stderr_lines == ['an error line\n']
}
