module os

$if (freestanding || syscalls) && !native && macos && amd64 {
	import sync.stdatomic
}

$if (freestanding || syscalls) && !native && ((linux && (amd64 || arm64)) || (macos && amd64)) {

	// Darwin has no pipe2: serialize pipe/CLOEXEC publication with Process forks.
	// Static storage cannot retain an arena allocated synchronization object.
	$if macos {
		__global fsys_process_spawn_gate stdatomic.AtomicVal[u32]
	}

	fn fsys_process_lock() {
		$if macos {
			for !fsys_process_spawn_gate.compare_and_swap(0, 1) {
				bf_nanosleep(1)
			}
		}
	}

	fn fsys_process_unlock() {
		$if macos {
			fsys_process_spawn_gate.store(0)
		}
	}

	@[direct_array_access; manualfree; no_profile; no_trace]
	fn fsys_process_close_pipes(pipes &[6]i32) {
		for i in 0 .. 6 {
			if pipes[i] >= 0 {
				bf_close(pipes[i])
			}
		}
	}

	// Keep all pipe sources above the standard streams, including when the caller
	// has closed stdin/stdout/stderr. This prevents dup2 from clobbering a source.
	@[direct_array_access; manualfree]
	fn fsys_process_pipe(fds &i32) int {
		err := bf_pipe_cloexec(fds)
		if err != 0 { return err }
		for i in 0 .. 2 {
			if unsafe { fds[i] } < 3 {
				fd, e := bf_dup_cloexec(unsafe { fds[i] }, 3)
				if e != 0 { return e }
				bf_close(unsafe { fds[i] })
				unsafe { fds[i] = i32(fd) }
			}
		}
		return 0
	}

	@[manualfree]
	fn (p &Process) fsys_free_spawn_plan(mut plan UnixSpawnPlan) {
		unsafe {
			plan.argv.free()
			plan.envp.free()
			plan.resolve_error.free()
			plan.stdin_error.free()
			plan.exec_error.free()
			if !is_abs_path(p.filename) && (p.work_folder != '' || !p.filename.contains(path_separator)) {
				plan.exe.free()
			}
		}
	}

	@[direct_array_access; manualfree]
	fn (mut p Process) fsys_spawn_process() int {
		mut plan := p.unix_prepare_spawn()
		mut pipes := [6]i32{init: -1}
		mut pipe_error := 0
		fsys_process_lock()
		if p.use_stdio_ctl {
			if !p.has_stdin_path { pipe_error = fsys_process_pipe(&pipes[0]) }
			if pipe_error == 0 { pipe_error = fsys_process_pipe(&pipes[2]) }
			if pipe_error == 0 && !p.merge_stdio { pipe_error = fsys_process_pipe(&pipes[4]) }
		}
		if pipe_error != 0 {
			fsys_process_close_pipes(&pipes)
			fsys_process_unlock()
			p.fsys_free_spawn_plan(mut plan)
			p.err = 'os: failed to create process pipes; code: ${pipe_error}'
			return -1
		}
		pid, fork_error := bf_fork_result()
		if pid == 0 {
			p.fsys_process_child(&plan, &pipes)
		}
		if fork_error != 0 {
			fsys_process_close_pipes(&pipes)
			fsys_process_unlock()
			p.fsys_free_spawn_plan(mut plan)
			p.err = 'os: failed to fork; code: ${fork_error}'
			return -1
		}
		// Set the group in both processes so an immediate parent signal_pgkill
		// cannot race the child's setpgid. An exec/exit racing this call is harmless.
		if p.use_pgroup { bf_setpgid(pid, pid) }
		if p.use_stdio_ctl {
			if !p.has_stdin_path {
				p.stdio_fd[0] = pipes[1]
				pipes[1] = -1
			}
			p.stdio_fd[1] = pipes[2]
			pipes[2] = -1
			if !p.merge_stdio {
				p.stdio_fd[2] = pipes[4]
				pipes[4] = -1
			}
		}
		fsys_process_close_pipes(&pipes)
		fsys_process_unlock()
		p.fsys_free_spawn_plan(mut plan)
		return pid
	}

	@[manualfree; no_profile; no_trace]
	fn fsys_process_write_error(data &u8, count int) {
		mut written := 0
		for written < count {
			n, e := bf_write(2, unsafe { data + written }, count - written)
			if e != 0 || n <= 0 { return }
			written += n
		}
	}

	@[manualfree; no_profile; no_trace]
	fn fsys_process_redirect(source int, target int) {
		err := bf_dup2(source, target)
		if err != 0 {
			unix_child_report_code('os: failed to redirect process stream', err)
			bf_exit(1)
		}
	}

	// No allocating V operations, cleanup defers, profiling, libc errno, or locks
	// are permitted on this path: another thread may have held them at fork time.
	@[direct_array_access; manualfree; no_profile; no_trace; noreturn]
	fn (p &Process) fsys_process_child(plan &UnixSpawnPlan, pipes &[6]i32) {
		if p.use_stdio_ctl {
			fsys_process_redirect(pipes[3], 1)
			fsys_process_redirect(if p.merge_stdio { pipes[3] } else { pipes[5] }, 2)
		}
		if p.use_pgroup {
			err := bf_setpgid(0, 0)
			if err != 0 {
				unix_child_report_code('os: failed to set process group', err)
				bf_exit(1)
			}
		}
		if p.has_stdin_path {
			fd, err := bf_openat(p.stdin_path.str, 0, 0)
			if err != 0 {
				unix_child_report_code(plan.stdin_error, err)
				bf_exit(1)
			}
			fsys_process_redirect(fd, 0)
			if fd != 0 { bf_close(fd) }
		} else if p.use_stdio_ctl {
			fsys_process_redirect(pipes[0], 0)
		}
		fsys_process_close_pipes(pipes)
		if plan.resolve_error.len > 0 {
			unix_child_write(plan.resolve_error)
			bf_exit(1)
		}
		if p.work_folder.len > 0 {
			err := bf_chdir(p.work_folder)
			if err != 0 {
				unix_child_report_code('os: failed to change process directory', err)
				bf_exit(1)
			}
		}
		_, err := bf_execve_result(&char(plan.exe.str), plan.argv.data, plan.envp.data)
		unix_child_report_code(plan.exec_error, err)
		bf_exit(1)
	}

	fn (mut p Process) fsys_process_signal(signal int, group bool) {
		err := bf_kill(if group { -p.pid } else { p.pid }, signal)
		if err != 0 { p.err = posix_get_error_msg(err) }
	}

	fn (mut p Process) fsys_check_pid_status(exit_early_on_ret0 bool, options int) bool {
		mut status := i32(0)
		mut ret := -1
		mut err := 0
		for {
			ret, err = bf_wait4_result(p.pid, &status, options, unsafe { nil })
			if err != 4 { break } // EINTR
		}
		if err != 0 {
			p.code = -1
			p.err = posix_get_error_msg(err)
			if err == 10 { // ECHILD: this pid has already been reaped elsewhere.
				p.reaped = true
				p.status = .aborted
			}
			return false
		}
		if exit_early_on_ret0 && ret == 0 { return true }
		p.reaped = true
		mut code, signaled := posix_wait4_to_exit_status(status)
		if signaled {
			p.status = .aborted
			p.err = 'Terminated by signal ${code:2d} (${sigint_to_signal_name(code)})'
			code += 128
		} else {
			p.status = .exited
		}
		p.code = code
		return false
	}
}
