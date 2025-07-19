module os

// The kind of the pipe file descriptor, that is used for communicating with the child process
pub enum ChildProcessPipeKind {
	stdin
	stdout
	stderr
}

// signal_kill kills the process, after that it is no longer running.
pub fn (mut p Process) signal_kill() {
	if p.status !in [.running, .stopped] {
		return
	}
	p._signal_kill()
	p.status = .aborted
}

// signal_term terminates the process.
pub fn (mut p Process) signal_term() {
	if p.status !in [.running, .stopped] {
		return
	}
	p._signal_term()
}

// signal_pgkill kills the whole process group.
pub fn (mut p Process) signal_pgkill() {
	if p.status !in [.running, .stopped] {
		return
	}
	p._signal_pgkill()
}

// signal_stop stops the process, you can resume it with p.signal_continue().
pub fn (mut p Process) signal_stop() {
	if p.status != .running {
		return
	}
	p._signal_stop()
	p.status = .stopped
}

// signal_continue tell a stopped process to continue/resume its work.
pub fn (mut p Process) signal_continue() {
	if p.status != .stopped {
		return
	}
	p._signal_continue()
	p.status = .running
}

// wait for a process to finish.
// Note: You have to call p.wait(), otherwise a finished process
// would get to a zombie state, and its resources will not get
// released fully, until its parent process exits.
// Note: This call will block the calling process until the child
// process is finished.
pub fn (mut p Process) wait() {
	if p.status == .not_started {
		p._spawn()
	}
	if p.status !in [.running, .stopped] {
		return
	}
	p._wait()
}

// free the OS resources associated with the process.
// Can be called multiple times, but will free the resources just once.
// This sets the process state to .closed, which is final.
pub fn (mut p Process) close() {
	if p.status in [.not_started, .closed] {
		return
	}
	p.status = .closed
	$if !windows {
		for i in 0 .. 3 {
			if p.stdio_fd[i] != 0 {
				fd_close(p.stdio_fd[i])
			}
		}
	}
}

@[unsafe]
pub fn (mut p Process) free() {
	p.close()
	unsafe {
		p.filename.free()
		p.err.free()
		p.args.free()
		p.env.free()
	}
}

// _spawn should not be called directly, but only by p.run()/p.wait().
// It encapsulates the fork/execve mechanism that allows the
// asynchronous starting of the new child process.
fn (mut p Process) _spawn() int {
	if !p.env_is_custom {
		p.env = []string{}
		current_environment := environ()
		for k, v in current_environment {
			p.env << '${k}=${v}'
		}
	}
	mut pid := 0
	$if windows {
		pid = p.win_spawn_process()
	} $else {
		pid = p.unix_spawn_process()
	}
	p.pid = pid
	p.status = .running
	return 0
}

// is_alive query whether the process is still alive.
pub fn (mut p Process) is_alive() bool {
	mut res := false
	if p.status in [.running, .stopped] {
		res = p._is_alive()
	}
	$if trace_process_is_alive ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, res: ${res}')
	}
	return res
}

//
pub fn (mut p Process) set_redirect_stdio() {
	p.use_stdio_ctl = true
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}')
	}
}

// stdin_write will write the string `s`, to the stdin pipe of the child process.
pub fn (mut p Process) stdin_write(s string) {
	p._check_redirection_call(@METHOD)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, s.len: ${s.len}, s: `${s}`')
	}
	p._write_to(.stdin, s)
}

// stdout_slurp will read from the stdout pipe.
// It will block until it either reads all the data, or until the pipe is closed (end of file).
pub fn (mut p Process) stdout_slurp() string {
	p._check_redirection_call(@METHOD)
	res := p._slurp_from(.stdout)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, res.len: ${res.len}, res: `${res}`')
	}
	return res
}

// stderr_slurp will read from the stderr pipe.
// It will block until it either reads all the data, or until the pipe is closed (end of file).
pub fn (mut p Process) stderr_slurp() string {
	p._check_redirection_call(@METHOD)
	res := p._slurp_from(.stderr)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, res.len: ${res.len}, res: `${res}`')
	}
	return res
}

// stdout_read reads a block of data, from the stdout pipe of the child process.
// It will block, if there is no data to be read. Call .is_pending() to check if
// there is data to be read, if you do not want to block.
pub fn (mut p Process) stdout_read() string {
	p._check_redirection_call(@METHOD)
	res := p._read_from(.stdout)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, res.len: ${res.len}, res: `${res}`')
	}
	return res
}

// stderr_read reads a block of data, from the stderr pipe of the child process.
// It will block, if there is no data to be read. Call .is_pending() to check if
// there is data to be read, if you do not want to block.
pub fn (mut p Process) stderr_read() string {
	p._check_redirection_call(@METHOD)
	res := p._read_from(.stderr)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, res.len: ${res.len}, res: `${res}`')
	}
	return res
}

// pipe_read reads a block of data, from the given pipe of the child process.
// It will return `none`, if there is no data to be read, *without blocking*.
pub fn (mut p Process) pipe_read(pkind ChildProcessPipeKind) ?string {
	p._check_redirection_call(@METHOD)
	if !p._is_pending(pkind) {
		$if trace_process_pipes ? {
			eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, no pending data')
		}
		return none
	}
	res := p._read_from(pkind)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, res.len: ${res.len}, res: `${res}`')
	}
	return res
}

// is_pending returns whether there is data to be read from child process's pipe corresponding to `pkind`.
// For example `if p.is_pending(.stdout) { dump( p.stdout_read() ) }` will not block indefinitely.
pub fn (mut p Process) is_pending(pkind ChildProcessPipeKind) bool {
	p._check_redirection_call(@METHOD)
	res := p._is_pending(pkind)
	$if trace_process_pipes ? {
		eprintln('${@LOCATION}, pid: ${p.pid}, status: ${p.status}, pkind: ${pkind}, res: ${res}')
	}
	return res
}

// _read_from should be called only from .stdout_read/0, .stderr_read/0 and .pipe_read/1.
fn (mut p Process) _read_from(pkind ChildProcessPipeKind) string {
	$if windows {
		s, _ := p.win_read_string(int(pkind), 4096)
		return s
	} $else {
		s, _ := fd_read(p.stdio_fd[pkind], 4096)
		return s
	}
}

// _slurp_from should be called only from stdout_slurp() and stderr_slurp().
fn (mut p Process) _slurp_from(pkind ChildProcessPipeKind) string {
	$if windows {
		return p.win_slurp(int(pkind))
	} $else {
		return fd_slurp(p.stdio_fd[pkind]).join('')
	}
}

// _write_to should be called only from stdin_write().
fn (mut p Process) _write_to(pkind ChildProcessPipeKind, s string) {
	$if windows {
		p.win_write_string(int(pkind), s)
	} $else {
		fd_write(p.stdio_fd[pkind], s)
	}
}

// _is_pending should be called only from is_pending().
fn (mut p Process) _is_pending(pkind ChildProcessPipeKind) bool {
	$if windows {
		return p.win_is_pending(int(pkind))
	} $else {
		return fd_is_pending(p.stdio_fd[pkind])
	}
	return false
}

// _check_redirection_call should be called just by stdxxx methods.
fn (mut p Process) _check_redirection_call(fn_name string) {
	if !p.use_stdio_ctl {
		panic('Call p.set_redirect_stdio() before calling p.' + fn_name)
	}
	if p.status == .not_started {
		panic('Call p.' + fn_name + '() after you have called p.run()')
	}
}

// _signal_stop should not be called directly, except by p.signal_stop.
fn (mut p Process) _signal_stop() {
	$if windows {
		p.win_stop_process()
	} $else {
		p.unix_stop_process()
	}
}

// _signal_continue should not be called directly, just by p.signal_continue.
fn (mut p Process) _signal_continue() {
	$if windows {
		p.win_resume_process()
	} $else {
		p.unix_resume_process()
	}
}

// _signal_kill should not be called directly, except by p.signal_kill.
fn (mut p Process) _signal_kill() {
	$if windows {
		p.win_kill_process()
	} $else {
		p.unix_kill_process()
	}
}

// _signal_term should not be called directly, except by p.signal_term.
fn (mut p Process) _signal_term() {
	$if windows {
		p.win_term_process()
	} $else {
		p.unix_term_process()
	}
}

// _signal_pgkill should not be called directly, except by p.signal_pgkill.
fn (mut p Process) _signal_pgkill() {
	$if windows {
		p.win_kill_pgroup()
	} $else {
		p.unix_kill_pgroup()
	}
}

// _wait should not be called directly, except by p.wait().
fn (mut p Process) _wait() {
	$if windows {
		p.win_wait()
	} $else {
		p.unix_wait()
	}
}

// _is_alive should not be called directly, except by p.is_alive().
fn (mut p Process) _is_alive() bool {
	$if windows {
		return p.win_is_alive()
	} $else {
		return p.unix_is_alive()
	}
}

// run starts the new process.
pub fn (mut p Process) run() {
	if p.status != .not_started {
		return
	}
	p._spawn()
}
