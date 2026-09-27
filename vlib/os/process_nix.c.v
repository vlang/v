module os

fn C.setpgid(pid i32, pgid i32) i32

// child_spawn_code_prefix is the `; code: N` separator that `IError.str()` adds.
// It is a literal, so the forked child can copy it without allocating.
const child_spawn_code_prefix = '; code: '

fn env_value_from_entries(env []string, name string) ?string {
	prefix := '${name}='
	for entry in env {
		if entry.starts_with(prefix) {
			return entry[prefix.len..]
		}
	}
	return none
}

fn (p &Process) unix_resolve_filename() !string {
	if is_abs_path(p.filename) {
		return p.filename
	}
	if p.filename.contains(path_separator) {
		if p.work_folder != '' {
			return abs_path(p.filename)
		}
		return p.filename
	}
	path := env_value_from_entries(p.env, 'PATH') or { return error_failed_to_find_executable() }
	return find_abs_path_of_executable_in_path_env(p.filename, path)
}

// UnixSpawnPlan holds everything the forked child needs, in a form it can use
// without allocating.
//
// Between `fork()` and `execve()` the child is a single thread inside a copy of
// a possibly multi threaded address space: every lock that some other thread
// held at fork time (the GC's, the allocator's, libc's) was copied in the
// locked state, and the thread that would have released it does not exist in
// the child. Anything that allocates there - resolving the executable through
// `PATH`, building the `argv`/`envp` vectors, formatting an error message with
// `eprintln` - can therefore block forever, which also hangs every parent that
// waits for the child. See vlang/v#28509.
struct UnixSpawnPlan {
	exe           string  // the already resolved path that is passed to execve
	argv          []&char // NUL terminated argument vector
	envp          []&char // NUL terminated environment vector
	resolve_error string  // when not empty, the child reports it and exits, instead of exec'ing
	stdin_error   string  // the message prefix used when the stdin file cannot be opened
	exec_error    string  // the message prefix used when execve fails
}

// unix_prepare_spawn does all the allocating work of a spawn upfront, in the
// parent process, so that the forked child only has to use async signal safe
// calls. It never fails: a failure to resolve the executable is turned into the
// message that the child prints on its (already redirected) stderr, keeping the
// previous observable behaviour of a child that exits with code 1.
fn (p &Process) unix_prepare_spawn() UnixSpawnPlan {
	mut exe := ''
	mut resolve_error := ''
	if resolved := p.unix_resolve_filename() {
		exe = resolved
	} else {
		resolve_error = '${err}\n'
	}
	mut argv := []&char{cap: p.args.len + 2}
	argv << &char(exe.str)
	for i in 0 .. p.args.len {
		argv << &char(p.args[i].str)
	}
	argv << &char(unsafe { nil })
	mut envp := []&char{cap: p.env.len + 1}
	for i in 0 .. p.env.len {
		envp << &char(p.env[i].str)
	}
	envp << &char(unsafe { nil })
	stdin_error := if p.has_stdin_path {
		'failed to open stdin file "${p.stdin_path}"'
	} else {
		''
	}
	return UnixSpawnPlan{
		exe:           exe
		argv:          argv
		envp:          envp
		resolve_error: resolve_error
		stdin_error:   stdin_error
		exec_error:    'os: failed to execute "${exe}"'
	}
}

// unix_child_write writes an already built message to the child's stderr.
// It is used between fork() and execve(), so it must not allocate.
fn unix_child_write(s string) {
	if s.len > 0 {
		unsafe { C.write(2, s.str, usize(s.len)) }
	}
}

// unix_child_report_errno writes `prefix` followed by the `; code: N` suffix
// that `IError.str()` uses, and a newline, to the child's stderr.
//
// It runs between fork() and execve(), so it only uses write() and stack
// buffers. In particular it does *not* call strerror(): that can take libc's
// locale/message locks, which are exactly the kind of lock that a fork from a
// multi threaded process can inherit already held. The errno is reported as a
// number instead; the C library's text for it is one `errno 13` lookup away,
// and a terse message is much better than a child that hangs while producing a
// nicer one.
@[direct_array_access]
fn unix_child_report_errno(prefix string) {
	code := C.errno
	unix_child_write(prefix)
	mut buf := [32]u8{}
	mut n := 0
	for ch in child_spawn_code_prefix {
		buf[n] = ch
		n++
	}
	mut digits := [16]u8{}
	mut d := 0
	mut rest := if code > 0 { code } else { 0 }
	if rest == 0 {
		digits[0] = `0`
		d = 1
	}
	for rest > 0 {
		digits[d] = u8(`0` + rest % 10)
		d++
		rest /= 10
	}
	for d > 0 {
		d--
		buf[n] = digits[d]
		n++
	}
	buf[n] = `\n`
	n++
	unsafe { C.write(2, &buf[0], usize(n)) }
}

fn (mut p Process) unix_spawn_process() int {
	// Each `C.pipe` writes two C `int` file descriptors; back them with `i32` so the
	// buffer matches the C ABI (a V `[6]int` is six 64-bit slots now that `int` is
	// 64-bit, so `pipe` would write out of bounds). The fds convert back to `int`
	// implicitly where they are stored/closed.
	mut pipeset := [6]i32{}
	if p.use_stdio_ctl {
		mut dont_care := 0
		if !p.has_stdin_path {
			dont_care = C.pipe(&pipeset[0]) // pipe read end 0 <- 1 pipe write end
		}
		dont_care = C.pipe(&pipeset[2]) // pipe read end 2 <- 3 pipe write end
		if !p.merge_stdio {
			dont_care = C.pipe(&pipeset[4]) // pipe read end 4 <- 5 pipe write end
		}
		_ = dont_care // using `_` directly on each above `pipe` fails to avoid C compiler generate an `-Wunused-result` warning
	}
	// Resolve the executable and build the C argv/envp vectors *before* forking;
	// doing it in the child would allocate, and can deadlock there.
	plan := p.unix_prepare_spawn()
	pid := fork()
	if pid != 0 {
		// This is the parent process after the fork.
		// Note: pid contains the process ID of the child process
		if p.use_stdio_ctl {
			if !p.has_stdin_path {
				p.stdio_fd[0] = pipeset[1] // store the write end of child's in
				fd_close(pipeset[0])
			}
			p.stdio_fd[1] = pipeset[2] // store the read end of child's out
			if !p.merge_stdio {
				p.stdio_fd[2] = pipeset[4] // store the read end of child's err
			}
			// close the rest of the pipe fds, the parent does not need them
			fd_close(pipeset[3])
			if !p.merge_stdio {
				fd_close(pipeset[5])
			}
		}
		return pid
	}
	//
	// Here, we are in the child process.
	// It still shares file descriptors with the parent process,
	// but it is otherwise independent and can do stuff *without*
	// affecting the parent process.
	//
	// Note: only async signal safe calls are allowed from here until execve()
	// replaces the process image - no allocation, no eprintln, and no exit()
	// (which would run the parent's atexit handlers and flush its buffered
	// stdio a second time). Use _exit() instead.
	//
	if p.use_pgroup {
		C.setpgid(0, 0)
	}
	mut stdin_fd := -1
	if p.has_stdin_path {
		stdin_fd = C.open(&char(p.stdin_path.str), o_rdonly, 0)
		if stdin_fd == -1 {
			unix_child_report_errno(plan.stdin_error)
			C._exit(1)
		}
	}
	if p.use_stdio_ctl {
		// Redirect the child standard in/out/err to the pipes that
		// were created in the parent.
		// Close the parent's pipe fds, the child do not need them:
		if !p.has_stdin_path {
			fd_close(pipeset[1])
		}
		fd_close(pipeset[2])
		if !p.merge_stdio {
			fd_close(pipeset[4])
		}
		// redirect the pipe fds to the child's in/out/err fds:
		if p.has_stdin_path {
			C.dup2(stdin_fd, 0)
		} else {
			C.dup2(pipeset[0], 0)
		}
		C.dup2(pipeset[3], 1)
		if p.merge_stdio {
			C.dup2(pipeset[3], 2)
		} else {
			C.dup2(pipeset[5], 2)
		}
		// close the pipe fdsx after the redirection
		if !p.has_stdin_path {
			fd_close(pipeset[0])
		}
		fd_close(pipeset[3])
		if !p.merge_stdio {
			fd_close(pipeset[5])
		}
	} else if p.has_stdin_path {
		C.dup2(stdin_fd, 0)
	}
	if stdin_fd > 0 {
		fd_close(stdin_fd)
	}
	if plan.resolve_error.len > 0 {
		unix_child_write(plan.resolve_error)
		C._exit(1)
	}
	if p.work_folder != '' {
		C.chdir(&char(p.work_folder.str))
	}
	C.execve(&char(plan.exe.str), plan.argv.data, plan.envp.data)
	// Note: normally execve does not return at all. If it does, it failed.
	unix_child_report_errno(plan.exec_error)
	C._exit(1)
	return 0
}

fn (mut p Process) unix_stop_process() {
	C.kill(p.pid, C.SIGSTOP)
}

fn (mut p Process) unix_resume_process() {
	C.kill(p.pid, C.SIGCONT)
}

fn (mut p Process) unix_term_process() {
	C.kill(p.pid, C.SIGTERM)
}

fn (mut p Process) unix_kill_process() {
	C.kill(p.pid, C.SIGKILL)
}

fn (mut p Process) unix_kill_pgroup() {
	C.kill(-p.pid, C.SIGKILL)
}

fn (mut p Process) unix_wait() {
	p.impl_check_pid_status(false, 0)
}

fn (mut p Process) unix_is_alive() bool {
	return p.impl_check_pid_status(true, C.WNOHANG)
}

fn (mut p Process) impl_check_pid_status(exit_early_on_ret0 bool, waitpid_options int) bool {
	mut cstatus := 0
	mut ret := -1
	$if !emscripten ? {
		ret = C.waitpid(p.pid, &cstatus, waitpid_options)
	}
	p.code = ret
	if ret == -1 {
		p.err = posix_get_error_msg(C.errno)
		return false
	}
	if exit_early_on_ret0 && ret == 0 {
		return true
	}
	mut pret, is_signaled := posix_wait4_to_exit_status(cstatus)
	if is_signaled {
		p.status = .aborted
		p.err = 'Terminated by signal ${pret:2d} (${sigint_to_signal_name(pret)})'
		pret += 128
	} else {
		p.status = .exited
	}
	p.code = pret
	return false
}

// these are here to make v_win.c/v.c generation work in all cases:
fn (mut p Process) win_spawn_process() int {
	return 0
}

fn (mut p Process) win_stop_process() {
}

fn (mut p Process) win_resume_process() {
}

fn (mut p Process) win_term_process() {
}

fn (mut p Process) win_kill_process() {
}

fn (mut p Process) win_kill_pgroup() {
}

fn (mut p Process) win_wait() {
}

fn (mut p Process) win_is_alive() bool {
	return false
}

fn (mut p Process) win_write_string(_idx int, _s string) {
}

fn (mut p Process) win_read_string(_idx int, _maxbytes int) (string, int) {
	return '', 0
}

fn (mut p Process) win_is_pending(_idx int) bool {
	return false
}

fn (mut p Process) win_slurp(_idx int) string {
	return ''
}
