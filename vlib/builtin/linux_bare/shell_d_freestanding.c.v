@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_shell_abi.h"
	fn C.v_shell_stream_open(i32, uint) uint
	fn C.v_shell_stream_close(uint) i32

	@[heap]
	struct BareShellPipe {
	mut:
		stream uint
		fd     i32
		pid    i32
		next   &BareShellPipe = unsafe { nil }
	}
	__global bare_shell_pipes &BareShellPipe
	__global bare_shell_pipe_mutex u32
	__global bare_shell_signal_mutex u32
	__global bare_shell_signal_users i32
	__global bare_shell_int_action BareNativeSignalAction
	__global bare_shell_quit_action BareNativeSignalAction

	struct BareShellCancel {
		pid     i32
		mask    u64
		signals bool
	}
	fn bare_shell_cancel_cleanup(argument uint) {
		state := unsafe { &BareShellCancel(argument) }
		if state.pid > 0 {
			sys_call2(u64(C.SYS_kill), u64(state.pid), u64(C.SIGKILL))
			bare_shell_wait(state.pid)
		}
		if state.signals {
			sys_call4(u64(C.SYS_rt_sigprocmask), u64(C.SIG_SETMASK), u64(&state.mask), 0, 8)
			bare_shell_signal_release()
		}
	}
	fn bare_shell_wait_cancel(pid i32) i32 {
		mut status := i32(0)
		for {
			result := bare_cancel_syscall(i64(C.SYS_wait4), uint(pid), uint(&status), 0, 0, 0, 0)
			if result >= 0 { return status }
			if result != -i64(C.EINTR) { return i32(bare_native_result(u64(result))) }
		}
		return -1
	}

	fn bare_shell_wait(pid i32) i32 {
		mut status := i32(0)
		for {
			result := bare_native_result(sys_call4(u64(C.SYS_wait4), u64(pid), u64(&status), 0, 0))
			if result >= 0 { return status }
			if bf_native_errno_cell != i32(C.EINTR) { return -1 }
		}
		return -1
	}

	@[noreturn]
	fn bare_shell_exec(command uint) {
		arguments := [uint(c'sh'), uint(c'-c'), uint(c'--'), command, uint(0)]!
		sys_call3(bf_nr_execve, u64(c'/bin/sh'), u64(&arguments[0]), u64(C.v_native_environment()))
		sys_call1(u64(C.SYS_exit_group), 127)
		for {}
	}

	fn bare_shell_signal_release() {
		bare_compat_lock(&bare_shell_signal_mutex)
		bare_shell_signal_users--
		if bare_shell_signal_users == 0 {
			sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGINT), u64(&bare_shell_int_action), 0, 8)
			sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGQUIT), u64(&bare_shell_quit_action), 0, 8)
		}
		bare_compat_unlock(&bare_shell_signal_mutex)
	}

	fn bare_shell_signal_acquire() i32 {
		bare_compat_lock(&bare_shell_signal_mutex)
		if bare_shell_signal_users == 0 {
			ignore := BareNativeSignalAction{ handler: 1 }
			if bare_native_result(sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGINT), u64(&ignore), u64(&bare_shell_int_action), 8)) < 0 {
				bare_compat_unlock(&bare_shell_signal_mutex)
				return -1
			}
			if bare_native_result(sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGQUIT), u64(&ignore), u64(&bare_shell_quit_action), 8)) < 0 {
				sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGINT), u64(&bare_shell_int_action), 0, 8)
				bare_compat_unlock(&bare_shell_signal_mutex)
				return -1
			}
		}
		bare_shell_signal_users++
		bare_compat_unlock(&bare_shell_signal_mutex)
		return 0
	}

	@[export: 'v_shell_system'; markused]
	fn bare_shell_system(command uint) i32 {
		if command == 0 { return if bare_shell_system(uint(c'exit 0')) == 0 { 1 } else { 0 } }
		bare_pthread_testcancel()
		saved := bf_native_errno_cell
		if bare_shell_signal_acquire() < 0 { return -1 }
		block := u64(1) << (i32(C.SIGCHLD) - 1)
		mut old_mask := u64(0)
		if bare_native_result(sys_call4(u64(C.SYS_rt_sigprocmask), 0, u64(&block), u64(&old_mask), 8)) < 0 {
			bare_shell_signal_release()
			return -1
		}
		pid := bare_native_fork()
		if pid == 0 {
			default_action := BareNativeSignalAction{}
			if bare_shell_int_action.handler != 1 {
				sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGINT), u64(&default_action), 0, 8)
			}
			if bare_shell_quit_action.handler != 1 {
				sys_call4(u64(C.SYS_rt_sigaction), u64(C.SIGQUIT), u64(&default_action), 0, 8)
			}
			sys_call4(u64(C.SYS_rt_sigprocmask), 2, u64(&old_mask), 0, 8)
			bare_shell_exec(command)
		}
		state := BareShellCancel{ pid: pid, mask: old_mask, signals: true }
		mut cleanup := BareCancelCleanup{}
		bare_cancel_cleanup_push(mut cleanup, uint(bare_shell_cancel_cleanup), uint(&state))
		status := if pid < 0 { i32(-1) } else { bare_shell_wait_cancel(pid) }
		bare_cancel_cleanup_pop(&cleanup)
		error := bf_native_errno_cell
		sys_call4(u64(C.SYS_rt_sigprocmask), 2, u64(&old_mask), 0, 8)
		bare_shell_signal_release()
		bf_native_errno_cell = if status < 0 { error } else { saved }
		return status
	}

	@[export: 'v_shell_popen'; markused]
	fn bare_shell_popen(command uint, mode uint) uint {
		if command == 0 || mode == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return 0
		}
		mut reading := false
		mut writing := false
		mut close_on_exec := false
		mut at := mode
		for unsafe { *(&u8(at)) } != 0 {
			match unsafe { *(&u8(at)) } {
				`r` { reading = true }
				`w` { writing = true }
				`e` { close_on_exec = true }
				else {
					bf_native_errno_cell = i32(C.EINVAL)
					return 0
				}
			}
			at++
		}
		if reading == writing {
			bf_native_errno_cell = i32(C.EINVAL)
			return 0
		}
		mut record := unsafe { &BareShellPipe(__calloc(1, sizeof(BareShellPipe))) }
		if record == unsafe { nil } { return 0 }
		mut descriptors := [2]i32{}
		if bare_native_result(sys_call2(u64(C.SYS_pipe2), u64(&descriptors[0]), u64(C.O_CLOEXEC))) < 0 {
			unsafe { __free(record) }
			return 0
		}
		parent_fd := descriptors[if reading { 0 } else { 1 }]
		child_fd := descriptors[if reading { 1 } else { 0 }]
		stream := C.v_shell_stream_open(parent_fd, if reading { uint(c'r') } else { uint(c'w') })
		if stream == 0 {
			error := bf_native_errno_cell
			sys_call1(u64(C.SYS_close), u64(parent_fd))
			sys_call1(u64(C.SYS_close), u64(child_fd))
			unsafe { __free(record) }
			bf_native_errno_cell = error
			return 0
		}
		bare_compat_lock(&bare_shell_pipe_mutex)
		pid := bare_native_fork()
		if pid == 0 {
			sys_call1(u64(C.SYS_close), u64(parent_fd))
			mut previous := bare_shell_pipes
			for previous != unsafe { nil } {
				sys_call1(u64(C.SYS_close), u64(previous.fd))
				previous = previous.next
			}
			target := if reading { i32(1) } else { i32(0) }
			if child_fd == target {
				if i64(sys_call3(u64(C.SYS_fcntl), u64(target), u64(C.F_SETFD), 0)) < 0 {
					sys_call1(u64(C.SYS_exit_group), 127)
				}
			} else {
				if i64(sys_call3(u64(C.SYS_dup3), u64(child_fd), u64(target), 0)) < 0 {
					sys_call1(u64(C.SYS_exit_group), 127)
				}
				sys_call1(u64(C.SYS_close), u64(child_fd))
			}
			bare_shell_exec(command)
		}
		error := bf_native_errno_cell
		sys_call1(u64(C.SYS_close), u64(child_fd))
		if pid < 0 {
			bare_compat_unlock(&bare_shell_pipe_mutex)
			C.v_shell_stream_close(stream)
			unsafe { __free(record) }
			bf_native_errno_cell = error
			return 0
		}
		if !close_on_exec && bare_native_result(sys_call3(u64(C.SYS_fcntl), u64(parent_fd), u64(C.F_SETFD), 0)) < 0 {
			failure := bf_native_errno_cell
			C.v_shell_stream_close(stream)
			bare_compat_unlock(&bare_shell_pipe_mutex)
			sys_call2(u64(C.SYS_kill), u64(pid), u64(C.SIGKILL))
			bare_shell_wait(pid)
			unsafe { __free(record) }
			bf_native_errno_cell = failure
			return 0
		}
		record.stream = stream
		record.fd = parent_fd
		record.pid = pid
		record.next = bare_shell_pipes
		bare_shell_pipes = record
		bare_compat_unlock(&bare_shell_pipe_mutex)
		return stream
	}

	@[export: 'v_shell_pclose'; markused]
	fn bare_shell_pclose(stream uint) i32 {
		bare_pthread_testcancel()
		saved := bf_native_errno_cell
		bare_compat_lock(&bare_shell_pipe_mutex)
		mut previous := unsafe { &BareShellPipe(nil) }
		mut record := bare_shell_pipes
		for record != unsafe { nil } && record.stream != stream {
			previous = record
			record = record.next
		}
		if record == unsafe { nil } {
			bare_compat_unlock(&bare_shell_pipe_mutex)
			bf_native_errno_cell = i32(C.ECHILD)
			return -1
		}
		closed := C.v_shell_stream_close(stream)
		close_error := bf_native_errno_cell
		if previous == unsafe { nil } {
			bare_shell_pipes = record.next
		} else {
			previous.next = record.next
		}
		pid := record.pid
		unsafe { __free(record) }
		bare_compat_unlock(&bare_shell_pipe_mutex)
		state := BareShellCancel{ pid: pid }
		mut cleanup := BareCancelCleanup{}
		bare_cancel_cleanup_push(mut cleanup, uint(bare_shell_cancel_cleanup), uint(&state))
		status := bare_shell_wait_cancel(pid)
		bare_cancel_cleanup_pop(&cleanup)
		if status < 0 { return -1 }
		bf_native_errno_cell = if closed < 0 { close_error } else { saved }
		return if closed < 0 { i32(-1) } else { status }
	}
}
