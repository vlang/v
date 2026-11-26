module os

$if (freestanding || syscalls) && !native && macos && amd64 {
	import sync.stdatomic
}

$if (freestanding || syscalls) && !native && ((linux && (amd64 || arm64)) || (macos && amd64)) {
	#include <errno.h>

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
			plan.search_paths.free()
			plan.resolve_error.free()
			plan.stdin_error.free()
			plan.exec_error.free()
			if plan.exe.str != p.filename.str {
				plan.exe.free()
			}
		}
	}

	@[direct_array_access; manualfree]
	fn (mut p Process) fsys_spawn_process(inherit_stdin bool) int {
		mut plan := p.unix_prepare_spawn_plan(inherit_stdin)
		mut pipes := [6]i32{init: -1}
		mut pipe_error := 0
		mut error_pipe := [2]i32{init: -1}
		fsys_process_lock()
		if p.use_stdio_ctl {
			if !p.has_stdin_path && !inherit_stdin { pipe_error = fsys_process_pipe(&pipes[0]) }
			if pipe_error == 0 { pipe_error = fsys_process_pipe(&pipes[2]) }
			if pipe_error == 0 && !p.merge_stdio { pipe_error = fsys_process_pipe(&pipes[4]) }
		}
		if pipe_error == 0 && inherit_stdin { pipe_error = fsys_process_pipe(&error_pipe[0]) }
		if pipe_error != 0 {
			if error_pipe[0] >= 0 { bf_close(error_pipe[0]) }
			if error_pipe[1] >= 0 { bf_close(error_pipe[1]) }
			fsys_process_close_pipes(&pipes)
			fsys_process_unlock()
			p.fsys_free_spawn_plan(mut plan)
			p.err = 'os: failed to create process pipes; code: ${pipe_error}'
			return -1
		}
		pid, fork_error := bf_fork_result()
		if pid == 0 {
			if error_pipe[0] >= 0 { bf_close(error_pipe[0]) }
			p.fsys_process_child(&plan, &pipes, inherit_stdin, error_pipe[1])
		}
		if fork_error != 0 {
			if error_pipe[0] >= 0 { bf_close(error_pipe[0]) }
			if error_pipe[1] >= 0 { bf_close(error_pipe[1]) }
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
			if !p.has_stdin_path && !inherit_stdin {
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
		if error_pipe[1] >= 0 { bf_close(error_pipe[1]) }
		if error_pipe[0] >= 0 {
			error_code := fsys_process_exec_error(error_pipe[0])
			if error_code != 0 {
				p.err = 'os: failed to execute process; code: ${error_code}'
			}
		}
		return pid
	}

	// This pipe closes automatically at exec. A failed child reports before
	// writing diagnostics, so a long diagnostic cannot fill stdout and deadlock.
	@[manualfree]
	fn fsys_process_exec_error(fd int) int {
		defer { bf_close(fd) }
		mut error := i32(0)
		mut received := 0
		for received < 4 {
			n, code := bf_read(fd, unsafe { voidptr(uint(&error) + uint(received)) }, 4 - received)
			if code == 4 { continue }
			if code != 0 { return code }
			if n == 0 { return if received == 0 { 0 } else { 5 } }
			received += n
		}
		return int(error)
	}

	@[manualfree; no_profile; no_trace; noreturn]
	fn fsys_process_child_fail(message string, code int, fd int) {
		if fd >= 0 {
			value := i32(code)
			mut written := 0
			for written < 4 {
				n, error := bf_write(fd, unsafe { voidptr(uint(&value) + uint(written)) }, 4 - written)
				if error == 4 { continue }
				if error != 0 || n == 0 { break }
				written += n
			}
			bf_close(fd)
		}
		unix_child_report_code(message, code)
		bf_exit(1)
	}

	@[manualfree]
	fn fsys_execute_shell(command string) Result {
		mut arguments := ['/bin/sh', '-c', command]
		// The vector borrows command; only fsys_exec_capture's copies are owned.
		defer { unsafe { (&array(&arguments)).free() } }
		return fsys_exec_capture(arguments, command)
	}

	@[manualfree]
	fn fsys_exec_capture(args []string, label string) Result {
		mut process := new_process(args[0].clone())
		mut arguments := []string{cap: args.len - 1}
		for i in 1 .. args.len { arguments << args[i].clone() }
		process.set_args(arguments)
		process.set_redirect_stdio_merged()
		defer {
			process.close()
			for mut value in process.env {
				unsafe { value.free() }
			}
			for mut value in process.args {
				unsafe { value.free() }
			}
			// All strings are owned by this capture. Erase the vectors' element
			// type so explicit cleanup is shallow in every compiler memory mode.
			unsafe {
				process.filename.free()
				process.err.free()
				(&array(&process.env)).free()
				(&array(&process.args)).free()
				free(process)
			}
		}
		if process._spawn(true) < 0 {
			return Result{ exit_code: -1, output: 'exec("${label}") failed' }
		}
		failed := process.err.len > 0
		mut chunks := fd_slurp(process.stdio_fd[1])
		output := chunks.join('')
		for mut value in chunks {
			unsafe { value.free() }
		}
		unsafe { (&array(&chunks)).free() }
		process.wait()
		if failed || process.code < 0 {
			unsafe { output.free() }
			return Result{ exit_code: -1, output: 'exec("${label}") failed' }
		}
		// Keep os.exec's existing signal-number convention; Process itself uses 128+n.
		code := if process.status == .aborted { process.code - 128 } else { process.code }
		return Result{ exit_code: code, output: output }
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
	fn fsys_process_redirect(source int, target int, error_fd int) {
		err := bf_dup2(source, target)
		if err != 0 {
			fsys_process_child_fail('os: failed to redirect process stream', err, error_fd)
		}
	}

	// No allocating V operations, cleanup defers, profiling, libc errno, or locks
	// are permitted on this path: another thread may have held them at fork time.
	@[direct_array_access; manualfree; no_profile; no_trace; noreturn]
	fn (p &Process) fsys_process_child(plan &UnixSpawnPlan, pipes &[6]i32, inherit_stdin bool, error_fd int) {
		if p.use_stdio_ctl {
			fsys_process_redirect(pipes[3], 1, error_fd)
			fsys_process_redirect(if p.merge_stdio { pipes[3] } else { pipes[5] }, 2, error_fd)
		}
		if p.use_pgroup {
			err := bf_setpgid(0, 0)
			if err != 0 {
				fsys_process_child_fail('os: failed to set process group', err, error_fd)
			}
		}
		if p.has_stdin_path {
			fd, err := bf_openat(p.stdin_path.str, 0, 0)
			if err != 0 {
				fsys_process_child_fail(plan.stdin_error, err, error_fd)
			}
			fsys_process_redirect(fd, 0, error_fd)
			if fd != 0 { bf_close(fd) }
		} else if p.use_stdio_ctl && !inherit_stdin {
			fsys_process_redirect(pipes[0], 0, error_fd)
		}
		fsys_process_close_pipes(pipes)
		if plan.resolve_error.len > 0 {
			fsys_process_child_fail(plan.resolve_error, 2, error_fd)
		}
		if p.work_folder.len > 0 {
			err := bf_chdir(p.work_folder)
			if err != 0 {
				fsys_process_child_fail('os: failed to change process directory', err, error_fd)
			}
		}
		if plan.search_paths.len > 0 {
			mut denied := false
			for i in 0 .. plan.search_paths.len {
				_, code := bf_execve_result(&char(plan.search_paths[i].str), plan.argv.data, plan.envp.data)
				if code == C.EACCES { denied = true }
				// execvp continues past inaccessible entries, absent interpreters,
				// non-directories, and transient errors from network filesystems.
				if code != C.ENOENT && code != C.EACCES && code != C.ENOTDIR && code != C.ENODEV
					&& code != C.ETIMEDOUT && code != C.ESTALE {
					fsys_process_child_fail(plan.exec_error, code, error_fd)
				}
			}
			fsys_process_child_fail(plan.exec_error, if denied {
				int(C.EACCES)
			} else {
				int(C.ENOENT)
			}, error_fd)
		}
		_, err := bf_execve_result(&char(plan.exe.str), plan.argv.data, plan.envp.data)
		fsys_process_child_fail(plan.exec_error, err, error_fd)
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
