module os

// signal_kill - kills the process, after that it is no longer running
pub fn (mut p Process) signal_kill() {
	if p.status !in [.running, .stopped] {
		return
	}
	p._signal_kill()
	p.status = .aborted
	return
}

// signal_pgkill - kills the whole process group
pub fn (mut p Process) signal_pgkill() {
	if p.status !in [.running, .stopped] {
		return
	}
	p._signal_pgkill()
	return
}

// signal_stop - stops the process, you can resume it with p.signal_continue()
pub fn (mut p Process) signal_stop() {
	if p.status != .running {
		return
	}
	p._signal_stop()
	p.status = .stopped
	return
}

// signal_continue - tell a stopped process to continue/resume its work
pub fn (mut p Process) signal_continue() {
	if p.status != .stopped {
		return
	}
	p._signal_continue()
	p.status = .running
	return
}

// wait - wait for a process to finish.
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
	return
}

// close - free the OS resources associated with the process.
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

[unsafe]
pub fn (mut p Process) free() {
	p.close()
	unsafe {
		p.filename.free()
		p.err.free()
		p.args.free()
		p.env.free()
	}
}

//
// _spawn - should not be called directly, but only by p.run()/p.wait() .
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

// is_alive - query whether the process p.pid is still alive
pub fn (mut p Process) is_alive() bool {
	if p.status in [.running, .stopped] {
		return p._is_alive()
	}
	return false
}

//
pub fn (mut p Process) set_redirect_stdio() {
	p.use_stdio_ctl = true
	return
}

pub fn (mut p Process) stdin_write(s string) {
	p._check_redirection_call('stdin_write')
	$if windows {
		p.win_write_string(0, s)
	} $else {
		fd_write(p.stdio_fd[0], s)
	}
}

// will read from stdout pipe, will only return when EOF (end of file) or data
// means this will block unless there is data
pub fn (mut p Process) stdout_slurp() string {
	p._check_redirection_call('stdout_slurp')
	$if windows {
		return p.win_slurp(1)
	} $else {
		return fd_slurp(p.stdio_fd[1]).join('')
	}
}

// read from stderr pipe, wait for data or EOF
pub fn (mut p Process) stderr_slurp() string {
	p._check_redirection_call('stderr_slurp')
	$if windows {
		return p.win_slurp(2)
	} $else {
		return fd_slurp(p.stdio_fd[2]).join('')
	}
}

// read from stdout, return if data or not
pub fn (mut p Process) stdout_read() string {
	p._check_redirection_call('stdout_read')
	$if windows {
		s, _ := p.win_read_string(1, 4096)
		return s
	} $else {
		s, _ := fd_read(p.stdio_fd[1], 4096)
		return s
	}
}

pub fn (mut p Process) stderr_read() string {
	p._check_redirection_call('stderr_read')
	$if windows {
		s, _ := p.win_read_string(2, 4096)
		return s
	} $else {
		s, _ := fd_read(p.stdio_fd[2], 4096)
		return s
	}
}

// _check_redirection_call - should be called just by stdxxx methods
fn (mut p Process) _check_redirection_call(fn_name string) {
	if !p.use_stdio_ctl {
		panic('Call p.set_redirect_stdio() before calling p.${fn_name}')
	}
	if p.status == .not_started {
		panic('Call p.${fn_name}() after you have called p.run()')
	}
}

// _signal_stop - should not be called directly, except by p.signal_stop
fn (mut p Process) _signal_stop() {
	$if windows {
		p.win_stop_process()
	} $else {
		p.unix_stop_process()
	}
}

// _signal_continue - should not be called directly, just by p.signal_continue
fn (mut p Process) _signal_continue() {
	$if windows {
		p.win_resume_process()
	} $else {
		p.unix_resume_process()
	}
}

// _signal_kill - should not be called directly, except by p.signal_kill
fn (mut p Process) _signal_kill() {
	$if windows {
		p.win_kill_process()
	} $else {
		p.unix_kill_process()
	}
}

// _signal_pgkill - should not be called directly, except by p.signal_pgkill
fn (mut p Process) _signal_pgkill() {
	$if windows {
		p.win_kill_pgroup()
	} $else {
		p.unix_kill_pgroup()
	}
}

// _wait - should not be called directly, except by p.wait()
fn (mut p Process) _wait() {
	$if windows {
		p.win_wait()
	} $else {
		p.unix_wait()
	}
}

// _is_alive - should not be called directly, except by p.is_alive()
fn (mut p Process) _is_alive() bool {
	$if windows {
		return p.win_is_alive()
	} $else {
		return p.unix_is_alive()
	}
}

// run - starts the new process
pub fn (mut p Process) run() {
	if p.status != .not_started {
		return
	}
	p._spawn()
	return
}
