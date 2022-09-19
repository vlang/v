module os

fn C.setpgid(pid int, pgid int) int

fn (mut p Process) unix_spawn_process() int {
	mut pipeset := [6]int{}
	if p.use_stdio_ctl {
		mut dont_care := C.pipe(&pipeset[0]) // pipe read end 0 <- 1 pipe write end
		dont_care = C.pipe(&pipeset[2]) // pipe read end 2 <- 3 pipe write end
		dont_care = C.pipe(&pipeset[4]) // pipe read end 4 <- 5 pipe write end
		_ = dont_care // using `_` directly on each above `pipe` fails to avoid C compiler generate an `-Wunused-result` warning
	}
	pid := fork()
	if pid != 0 {
		// This is the parent process after the fork.
		// Note: pid contains the process ID of the child process
		if p.use_stdio_ctl {
			p.stdio_fd[0] = pipeset[1] // store the write end of child's in
			p.stdio_fd[1] = pipeset[2] // store the read end of child's out
			p.stdio_fd[2] = pipeset[4] // store the read end of child's err
			// close the rest of the pipe fds, the parent does not need them
			fd_close(pipeset[0])
			fd_close(pipeset[3])
			fd_close(pipeset[5])
		}
		return pid
	}
	//
	// Here, we are in the child process.
	// It still shares file descriptors with the parent process,
	// but it is otherwise independant and can do stuff *without*
	// affecting the parent process.
	//
	if p.use_pgroup {
		C.setpgid(0, 0)
	}
	if p.use_stdio_ctl {
		// Redirect the child standart in/out/err to the pipes that
		// were created in the parent.
		// Close the parent's pipe fds, the child do not need them:
		fd_close(pipeset[1])
		fd_close(pipeset[2])
		fd_close(pipeset[4])
		// redirect the pipe fds to the child's in/out/err fds:
		C.dup2(pipeset[0], 0)
		C.dup2(pipeset[3], 1)
		C.dup2(pipeset[5], 2)
		// close the pipe fdsx after the redirection
		fd_close(pipeset[0])
		fd_close(pipeset[3])
		fd_close(pipeset[5])
	}
	execve(p.filename, p.args, p.env) or {
		eprintln(err)
		exit(1)
	}
	return 0
}

fn (mut p Process) unix_stop_process() {
	C.kill(p.pid, C.SIGSTOP)
}

fn (mut p Process) unix_resume_process() {
	C.kill(p.pid, C.SIGCONT)
}

fn (mut p Process) unix_kill_process() {
	C.kill(p.pid, C.SIGKILL)
}

fn (mut p Process) unix_kill_pgroup() {
	C.kill(-p.pid, C.SIGKILL)
}

fn (mut p Process) unix_wait() {
	cstatus := 0
	mut ret := -1
	$if !emscripten ? {
		ret = C.waitpid(p.pid, &cstatus, 0)
	}
	if ret == -1 {
		p.err = posix_get_error_msg(C.errno)
		return
	}
	pret, is_signaled := posix_wait4_to_exit_status(cstatus)
	if is_signaled {
		p.status = .aborted
		p.err = 'Terminated by signal ${ret:2d} (${sigint_to_signal_name(pret)})'
	} else {
		p.status = .exited
	}
	p.code = pret
}

fn (mut p Process) unix_is_alive() bool {
	cstatus := 0
	mut ret := -1
	$if !emscripten ? {
		ret = C.waitpid(p.pid, &cstatus, C.WNOHANG)
	}
	if ret == -1 {
		p.err = posix_get_error_msg(C.errno)
		return false
	}
	if ret == 0 {
		return true
	}
	pret, is_signaled := posix_wait4_to_exit_status(cstatus)
	if is_signaled {
		p.status = .aborted
		p.err = 'Terminated by signal ${ret:2d} (${sigint_to_signal_name(pret)})'
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

fn (mut p Process) win_kill_process() {
}

fn (mut p Process) win_kill_pgroup() {
}

fn (mut p Process) win_wait() {
}

fn (mut p Process) win_is_alive() bool {
	return false
}

fn (mut p Process) win_write_string(idx int, s string) {
}

fn (mut p Process) win_read_string(idx int, maxbytes int) (string, int) {
	return '', 0
}

fn (mut p Process) win_slurp(idx int) string {
	return ''
}
