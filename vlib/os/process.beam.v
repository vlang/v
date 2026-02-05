// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

// BEAM backend implementation for Process methods.
// These provide placeholder implementations that compile to valid BEAM code.
// Real implementations would use Erlang's open_port or os:cmd.

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
	// Placeholder - in real impl: send kill signal via port
	p.status = .aborted
}

// signal_term terminates the process.
pub fn (mut p Process) signal_term() {
	if p.status !in [.running, .stopped] {
		return
	}
	// Placeholder
}

// signal_pgkill kills the whole process group.
pub fn (mut p Process) signal_pgkill() {
	if p.status !in [.running, .stopped] {
		return
	}
	// Placeholder
}

// signal_stop stops the process, you can resume it with p.signal_continue().
pub fn (mut p Process) signal_stop() {
	if p.status != .running {
		return
	}
	// Placeholder
	p.status = .stopped
}

// signal_continue tell a stopped process to continue/resume its work.
pub fn (mut p Process) signal_continue() {
	if p.status != .stopped {
		return
	}
	// Placeholder
	p.status = .running
}

// wait for a process to finish.
pub fn (mut p Process) wait() {
	if p.status == .not_started {
		p._spawn()
	}
	if p.status !in [.running, .stopped] {
		return
	}
	// Placeholder - in real impl: wait for port to close
	p.status = .exited
	p.code = 0
}

// free the OS resources associated with the process.
pub fn (mut p Process) close() {
	if p.status in [.not_started, .closed] {
		return
	}
	p.status = .closed
}

@[unsafe]
pub fn (mut p Process) free() {
	p.close()
}

// _spawn should not be called directly, but only by p.run()/p.wait().
fn (mut p Process) _spawn() int {
	if !p.env_is_custom {
		p.env = []string{}
		current_environment := environ()
		for k, v in current_environment {
			p.env << '${k}=${v}'
		}
	}
	// Placeholder - in real impl: would use open_port
	p.pid = 0
	p.status = .running
	return 0
}

// is_alive query whether the process is still alive.
pub fn (mut p Process) is_alive() bool {
	return p.status in [.running, .stopped]
}

// set_redirect_stdio enables stdio redirection for the process.
pub fn (mut p Process) set_redirect_stdio() {
	p.use_stdio_ctl = true
}

// stdin_write will write the string `s`, to the stdin pipe of the child process.
pub fn (mut p Process) stdin_write(s string) {
	// Placeholder
}

// stdout_slurp will read from the stdout pipe.
pub fn (mut p Process) stdout_slurp() string {
	return ''
}

// stderr_slurp will read from the stderr pipe.
pub fn (mut p Process) stderr_slurp() string {
	return ''
}

// stdout_read reads a block of data, from the stdout pipe of the child process.
pub fn (mut p Process) stdout_read() string {
	return ''
}

// stderr_read reads a block of data, from the stderr pipe of the child process.
pub fn (mut p Process) stderr_read() string {
	return ''
}

// pipe_read reads a block of data, from the given pipe of the child process.
pub fn (mut p Process) pipe_read(pkind ChildProcessPipeKind) ?string {
	return none
}

// is_pending returns whether there is data to be read from child process's pipe corresponding to `pkind`.
pub fn (mut p Process) is_pending(pkind ChildProcessPipeKind) bool {
	return false
}

// run starts the new process.
pub fn (mut p Process) run() {
	if p.status != .not_started {
		return
	}
	p._spawn()
}
