@[has_globals]
module driver

import os

$if native ? {

	// This is the native counterpart of run_signals_nix.c. Signal callbacks and
	// the forked observer use only stack storage, globals, and async-signal-safe
	// libc calls. In particular, neither path allocates V arrays or strings.
	fn C.sigaction(signal i32, action voidptr, previous voidptr) i32
	fn C.sigprocmask(how i32, mask voidptr, previous voidptr) i32
	fn C.siginterrupt(signal i32, interrupt i32) i32
	fn C.getpgrp() i32
	fn C.getpgid(pid i32) i32
	fn C.tcgetpgrp(fd i32) i32
	fn C.tcsetpgrp(fd i32, group i32) i32
	fn C.waitpid(pid i32, status voidptr, options i32) i32

	const native_run_tstp = $if linux { i32(20) } $else { i32(18) }
	const native_run_stop = $if linux { i32(19) } $else { i32(17) }
	const native_run_cont = $if linux { i32(18) } $else { i32(19) }
	const native_run_block = $if linux { i32(0) } $else { i32(1) }
	const native_run_unblock = $if linux { i32(1) } $else { i32(2) }
	const native_run_setmask = $if linux { i32(2) } $else { i32(3) }

	// libc owns the sigaction layout. Both native Unix targets fit in this
	// reservation, whose u64 fields provide the required pointer alignment.
	struct NativeRunAction {
	mut:
		words [64]u64
	}

	// Linux uses 128 bytes and Darwin uses the first four bytes. All signals used
	// here fit in the first word on both supported little-endian native targets.
	struct NativeRunMask {
	mut:
		words [16]u64
	}

	__global (
		volatile native_run_owner_pid               i32
		volatile native_run_child_pid               i32
		volatile native_run_received_signal         i32
		volatile native_run_termination_signal      i32
		volatile native_run_forwarded_stop          i32
		volatile native_run_bridging_stop           i32
		volatile native_run_bridging_interrupt      i32
		volatile native_run_observer_pid            i32
		volatile native_run_observer_forwarded_int  i32
		volatile native_run_observer_forwarded_quit i32
		volatile native_run_observer_forwarded_stop i32
		volatile native_run_observer_notice_read    = i32(-1)
		volatile native_run_observer_notice_write   = i32(-1)
		native_run_observed_stop                    bool
		native_run_observer_control                 = i32(-1)
		native_run_observer_stopped                 bool
		native_run_owner_group                      i32
		native_run_terminal_fd                      = i32(-1)
		native_run_handoff_terminal                 bool
		native_run_previous                         [7]NativeRunAction
	)

	fn native_run_fcntl(fd i32, command i32, value i32) i32 {
		return C.fcntl(fd, command, unsafe { voidptr(i64(value)) })
	}

	fn native_run_signal_number(index int) i32 {
		return match index {
			0 { i32(15) }
			1 { i32(1) }
			2 { i32(2) }
			3 { i32(3) }
			4 { native_run_tstp }
			5 { i32(21) }
			else { i32(22) }
		}
	}

	fn native_run_block_signals(mut previous NativeRunMask) i32 {
		mut mask := NativeRunMask{}
		for index in 0 .. 7 {
			mask.words[0] |= u64(1) << (native_run_signal_number(index) - 1)
		}
		return C.sigprocmask(native_run_block, &mask, &previous)
	}

	fn native_run_restore_mask(previous &NativeRunMask) {
		C.sigprocmask(native_run_setmask, previous, unsafe { nil })
	}

	fn native_run_send_child_signal(signal_number i32) {
		if native_run_observer_pid > 0 && native_run_observer_notice_write >= 0 {
			mut announced := false
			if signal_number == 2 {
				announced = native_run_observer_forwarded_int != 0
				native_run_observer_forwarded_int = 1
			} else if signal_number == 3 {
				announced = native_run_observer_forwarded_quit != 0
				native_run_observer_forwarded_quit = 1
			} else if signal_number == native_run_tstp {
				announced = native_run_observer_forwarded_stop != 0
				native_run_observer_forwarded_stop = 1
			} else {
				announced = true
			}
			if !announced {
				notice := u8(signal_number)
				for C.write(native_run_observer_notice_write, &notice, 1) < 0 && C.errno == 4 {
				}
			}
		}
		if native_run_child_pid > 0 {
			C.kill(-native_run_child_pid, signal_number)
		}
	}

	fn native_run_forward_signal(signal_number i32) {
		saved_errno := C.errno
		is_stop := signal_number == native_run_tstp || signal_number == 21 || signal_number == 22
		if C.getpid() != native_run_owner_pid {
			if is_stop {
				C.kill(C.getpid(), native_run_stop)
				C.errno = saved_errno
				return
			}
			C._exit(128 + signal_number)
		}
		if is_stop && signal_number == native_run_bridging_stop {
			native_run_bridging_stop = 0
			C.errno = saved_errno
			return
		}
		if signal_number == native_run_bridging_interrupt {
			native_run_bridging_interrupt = 0
			C.errno = saved_errno
			return
		}
		if is_stop {
			native_run_forwarded_stop = signal_number
		}
		native_run_received_signal = signal_number
		if signal_number == 1 || signal_number == 15 {
			native_run_termination_signal = signal_number
		}
		native_run_send_child_signal(signal_number)
		C.errno = saved_errno
	}

	fn native_run_install_signal(signal_number i32, previous &NativeRunAction) i32 {
		if C.sigaction(signal_number, unsafe { nil }, previous) != 0 {
			return -1
		}
		// Preserve inherited ignores without temporarily discarding pending signals.
		handler := C.signal(signal_number, voidptr(native_run_forward_signal))
		if handler == unsafe { voidptr(i64(-1)) } {
			return -1
		}
		if handler == unsafe { voidptr(1) } {
			return C.sigaction(signal_number, previous, unsafe { nil })
		}
		if C.siginterrupt(signal_number, 0) != 0 {
			saved_errno := C.errno
			C.sigaction(signal_number, previous, unsafe { nil })
			C.errno = saved_errno
			return -1
		}
		return 0
	}

	fn native_run_set_foreground(group i32) i32 {
		mut mask := NativeRunMask{}
		mask.words[0] = u64(1) << 21 // SIGTTOU
		mut previous := NativeRunMask{}
		if C.sigprocmask(native_run_block, &mask, &previous) != 0 {
			return -1
		}
		result := C.tcsetpgrp(native_run_terminal_fd, group)
		saved_errno := C.errno
		native_run_restore_mask(&previous)
		C.errno = saved_errno
		return result
	}

	fn native_run_restore_terminal() {
		if native_run_terminal_fd >= 0
			&& C.tcgetpgrp(native_run_terminal_fd) == native_run_child_pid {
			native_run_set_foreground(native_run_owner_group)
		}
	}

	fn native_run_foreground_child() i32 {
		if native_run_handoff_terminal && native_run_terminal_fd >= 0
			&& C.tcgetpgrp(native_run_terminal_fd) == native_run_owner_group {
			group := C.getpgid(native_run_child_pid)
			if group < 0 {
				return -1
			}
			return native_run_set_foreground(group)
		}
		return 0
	}

	fn native_run_observer_interrupt(signal_number i32) {
		mut notices := [3]u8{}
		mut forwarded := false
		for {
			count := C.read(native_run_observer_notice_read, &notices[0], 3)
			if count < 0 && C.errno == 4 {
				continue
			}
			if count <= 0 {
				break
			}
			for i in 0 .. int(count) {
				if notices[i] == u8(signal_number) {
					forwarded = true
				}
			}
		}
		C._exit((if forwarded { 64 } else { 128 }) + signal_number)
	}

	fn native_run_start_observer(previous_mask &NativeRunMask) i32 {
		if !native_run_handoff_terminal || C.getpgid(native_run_child_pid) != native_run_child_pid {
			return 0
		}
		mut control := [2]i32{}
		if C.pipe(&control[0]) != 0 {
			return -1
		}
		if native_run_fcntl(control[0], 2, 1) < 0 || native_run_fcntl(control[1], 2, 1) < 0 {
			C.close(control[0])
			C.close(control[1])
			return -1
		}
		mut notices := [2]i32{}
		if C.pipe(&notices[0]) != 0 {
			C.close(control[0])
			C.close(control[1])
			return -1
		}
		flags := native_run_fcntl(notices[0], 3, 0)
		nonblocking := $if linux { i32(2048) } $else { i32(4) }
		if flags < 0 || native_run_fcntl(notices[0], 4, flags | nonblocking) < 0
			|| native_run_fcntl(notices[0], 2, 1) < 0 || native_run_fcntl(notices[1], 2, 1) < 0 {
			C.close(control[0])
			C.close(control[1])
			C.close(notices[0])
			C.close(notices[1])
			return -1
		}
		observer := C.fork()
		if observer < 0 {
			C.close(control[0])
			C.close(control[1])
			C.close(notices[0])
			C.close(notices[1])
			return -1
		}
		if observer == 0 {
			C.close(control[1])
			C.close(notices[1])
			native_run_observer_notice_read = notices[0]
			if C.setpgid(0, native_run_child_pid) != 0 {
				C._exit(125)
			}
			for i in 0 .. 4 {
				signal_number := match i {
					0 { i32(1) }
					1 { i32(15) }
					2 { i32(21) }
					else { i32(22) }
				}
				if C.signal(signal_number, unsafe { voidptr(1) }) == unsafe { voidptr(i64(-1)) } {
					C._exit(125)
				}
			}
			for i in 0 .. 3 {
				signal_number := if i < 2 { i32(i + 2) } else { native_run_tstp }
				previous := C.signal(signal_number, voidptr(native_run_observer_interrupt))
				if previous == unsafe { voidptr(i64(-1)) } {
					C._exit(125)
				}
				if previous == unsafe { voidptr(1) } {
					C.signal(signal_number, unsafe { voidptr(1) })
				}
			}
			if native_run_terminal_fd >= 0 {
				C.close(native_run_terminal_fd)
			}
			for fd in 0 .. 3 {
				if fd != control[0] && fd != notices[0] {
					C.close(fd)
				}
			}
			if C.sigprocmask(native_run_setmask, previous_mask, unsafe { nil }) != 0 {
				C._exit(125)
			}
			mut byte := u8(0)
			for {
				count := C.read(control[0], &byte, 1)
				if count < 0 && C.errno == 4 {
					continue
				}
				if count <= 0 {
					C._exit(if count < 0 { 125 } else { 0 })
				}
			}
		}
		C.close(control[0])
		if C.setpgid(observer, native_run_child_pid) != 0
			&& C.getpgid(observer) != native_run_child_pid {
			C.close(control[1])
			C.close(notices[0])
			C.close(notices[1])
			C.kill(observer, 9)
			for i32(C.waitpid(observer, unsafe { nil }, 0)) < 0 && C.errno == 4 {
			}
			return -1
		}
		native_run_observer_pid = observer
		native_run_observer_control = control[1]
		// Retain a reader so late forwarding cannot raise SIGPIPE after observer exit.
		native_run_observer_notice_read = notices[0]
		native_run_observer_notice_write = notices[1]
		native_run_observer_stopped = false
		native_run_observer_forwarded_int = 0
		native_run_observer_forwarded_quit = 0
		native_run_observer_forwarded_stop = 0
		return 0
	}

	fn native_run_close_notices() {
		if native_run_observer_notice_read >= 0 {
			C.close(native_run_observer_notice_read)
		}
		if native_run_observer_notice_write >= 0 {
			C.close(native_run_observer_notice_write)
		}
		native_run_observer_notice_read = -1
		native_run_observer_notice_write = -1
	}

	fn native_run_observer_exit(status i32) i32 {
		mut previous_mask := NativeRunMask{}
		if native_run_block_signals(mut previous_mask) != 0 {
			return -1
		}
		exited := status & 0x7f == 0
		exit_code := (status >> 8) & 0xff
		signal_number := if exited { exit_code - 128 } else { i32(0) }
		native_run_observer_pid = 0
		native_run_observer_stopped = false
		if native_run_observer_control >= 0 {
			C.close(native_run_observer_control)
		}
		native_run_observer_control = -1
		native_run_close_notices()
		if signal_number == native_run_tstp {
			native_run_observed_stop = true
		}
		if signal_number == 2 || signal_number == 3 {
			mut selected := NativeRunMask{}
			selected.words[0] = u64(1) << (signal_number - 1)
			native_run_bridging_interrupt = signal_number
			C.kill(-native_run_owner_group, signal_number)
			C.sigprocmask(native_run_unblock, &selected, unsafe { nil })
		}
		native_run_restore_mask(&previous_mask)
		return if exited && exit_code == 125 { -1 } else { 0 }
	}

	fn native_run_close_observer() i32 {
		if native_run_observer_control >= 0 {
			C.close(native_run_observer_control)
		}
		native_run_observer_control = -1
		if native_run_observer_pid > 0 && native_run_observer_stopped {
			C.kill(native_run_observer_pid, native_run_cont)
		}
		for native_run_observer_pid > 0 {
			mut status := i32(0)
			waited := i32(C.waitpid(native_run_observer_pid, &status, 2)) // WUNTRACED
			if waited < 0 {
				if C.errno == 4 {
					continue
				}
				native_run_observer_pid = 0
				native_run_observer_stopped = false
				native_run_close_notices()
				return -1
			}
			if status & 0xff == 0x7f {
				C.kill(waited, native_run_cont)
				continue
			}
			return native_run_observer_exit(status)
		}
		return 0
	}

	fn native_run_signals_begin() i32 {
		mut previous_mask := NativeRunMask{}
		if native_run_block_signals(mut previous_mask) != 0 {
			return -1
		}
		native_run_owner_pid = C.getpid()
		native_run_owner_group = C.getpgrp()
		native_run_child_pid = 0
		native_run_received_signal = 0
		native_run_termination_signal = 0
		native_run_forwarded_stop = 0
		native_run_observed_stop = false
		native_run_bridging_stop = 0
		native_run_bridging_interrupt = 0
		native_run_observer_pid = 0
		native_run_observer_control = -1
		native_run_observer_stopped = false
		native_run_terminal_fd = C.open(c'/dev/tty', 2) // O_RDWR
		native_run_handoff_terminal = native_run_terminal_fd >= 0 && C.tcgetpgrp(0) >= 0
		if native_run_terminal_fd >= 0 && native_run_fcntl(native_run_terminal_fd, 2, 1) < 0 {
			saved_errno := C.errno
			C.close(native_run_terminal_fd)
			native_run_terminal_fd = -1
			native_run_restore_mask(&previous_mask)
			C.errno = saved_errno
			return -1
		}
		mut installed := 0
		for installed < 7
			&& native_run_install_signal(native_run_signal_number(installed), &native_run_previous[installed]) == 0 {
			installed++
		}
		result := if installed == 7 { i32(0) } else { i32(-1) }
		saved_errno := C.errno
		if result != 0 {
			for installed > 0 {
				installed--
				C.sigaction(native_run_signal_number(installed), &native_run_previous[installed], unsafe { nil })
			}
			if native_run_terminal_fd >= 0 {
				C.close(native_run_terminal_fd)
			}
			native_run_terminal_fd = -1
		}
		native_run_restore_mask(&previous_mask)
		C.errno = saved_errno
		return result
	}

	fn native_run_signals_set_pid(child_pid i32) i32 {
		mut previous_mask := NativeRunMask{}
		if native_run_block_signals(mut previous_mask) != 0 {
			return -1
		}
		mut result := i32(0)
		if child_pid <= 0 {
			result = -1
		} else {
			for attempt in 0 .. 2 {
				if C.setpgid(child_pid, child_pid) == 0 {
					break
				}
				set_error := C.errno
				group := C.getpgid(child_pid)
				if group == child_pid || set_error == 3 || (group < 0 && C.errno == 3) {
					break
				}
				if set_error == 1 && attempt == 0 {
					continue
				}
				result = -1
				break
			}
		}
		if result == 0 {
			native_run_child_pid = child_pid
			result = native_run_start_observer(&previous_mask)
			if result == 0 {
				result = native_run_foreground_child()
			}
			if result != 0 && C.kill(child_pid, 0) < 0 && C.errno == 3 {
				result = 0
			}
			if result == 0 {
				signal_number := native_run_received_signal
				if signal_number != 0 {
					native_run_send_child_signal(signal_number)
				}
			}
		}
		saved_errno := C.errno
		native_run_restore_mask(&previous_mask)
		C.errno = saved_errno
		return result
	}

	fn native_run_signal_child(signal_number i32) i32 {
		if C.kill(-native_run_child_pid, signal_number) == 0
			&& C.getpgid(native_run_child_pid) == native_run_child_pid {
			return 0
		}
		return C.kill(native_run_child_pid, signal_number)
	}

	fn native_run_suspend_owner(stop_signal i32, forwarded_stop i32) i32 {
		native_run_restore_terminal()
		if native_run_terminal_fd >= 0 && forwarded_stop == 0
			&& (stop_signal == native_run_tstp || stop_signal == 21 || stop_signal == 22) {
			native_run_bridging_stop = stop_signal
			C.kill(-native_run_owner_group, stop_signal)
		}
		mut previous_mask := NativeRunMask{}
		for {
			native_run_forwarded_stop = 0
			C.kill(native_run_owner_pid, native_run_stop)
			if native_run_block_signals(mut previous_mask) != 0 {
				return -1
			}
			if native_run_forwarded_stop == 0 {
				break
			}
			native_run_restore_mask(&previous_mask)
		}
		mut result := native_run_foreground_child()
		if result == 0 {
			result = native_run_signal_child(native_run_cont)
		}
		saved_errno := C.errno
		native_run_restore_mask(&previous_mask)
		C.errno = saved_errno
		return if result != 0 { -1 } else { 0 }
	}

	fn native_run_signals_kill(child_pid i32) {
		if child_pid > 0 && native_run_child_pid == child_pid {
			C.kill(-child_pid, 9)
		}
		if child_pid > 0 {
			C.kill(child_pid, 9)
		}
	}

	fn native_run_signals_wait(mut code i32, mut signal_number i32) i32 {
		if native_run_child_pid <= 0 {
			return -1
		}
		for {
			mut status := i32(0)
			target := if native_run_observer_pid > 0 { i32(-1) } else { native_run_child_pid }
			waited := i32(C.waitpid(target, &status, 2)) // WUNTRACED
			if waited < 0 {
				if C.errno == 4 {
					continue
				}
				return -1
			}
			if waited == native_run_observer_pid {
				if status & 0xff == 0x7f {
					native_run_observer_stopped = true
					continue
				}
				if native_run_observer_exit(status) != 0 {
					return -1
				}
				if native_run_observed_stop {
					if native_run_signal_child(native_run_stop) != 0 {
						return -1
					}
					continue
				}
				mut previous_mask := NativeRunMask{}
				if native_run_block_signals(mut previous_mask) != 0 {
					return -1
				}
				result := native_run_start_observer(&previous_mask)
				native_run_restore_mask(&previous_mask)
				if result != 0 {
					return -1
				}
				continue
			}
			if waited != native_run_child_pid {
				continue
			}
			if status & 0xff == 0x7f {
				stopped_by := (status >> 8) & 0xff
				if (stopped_by == 21 || stopped_by == 22) && native_run_forwarded_stop == 0
					&& !native_run_observed_stop && native_run_handoff_terminal
					&& native_run_terminal_fd >= 0
					&& C.tcgetpgrp(native_run_terminal_fd) == native_run_child_pid {
					C.kill(-native_run_child_pid, native_run_cont)
					continue
				}
				forwarded_stop := native_run_forwarded_stop
				native_run_forwarded_stop = 0
				if native_run_terminal_fd < 0 && forwarded_stop == 0 {
					continue
				}
				stop_signal := if native_run_observed_stop { native_run_tstp } else { stopped_by }
				if native_run_close_observer() != 0 {
					return -1
				}
				native_run_observed_stop = false
				if native_run_suspend_owner(stop_signal, forwarded_stop) != 0 {
					return -1
				}
				mut previous_mask := NativeRunMask{}
				if native_run_block_signals(mut previous_mask) != 0 {
					return -1
				}
				result := native_run_start_observer(&previous_mask)
				native_run_restore_mask(&previous_mask)
				if result != 0 {
					return -1
				}
				continue
			}
			native_run_restore_terminal()
			if native_run_close_observer() != 0 {
				return -1
			}
			signal_number = if status & 0x7f != 0 { status & 0x7f } else { i32(0) }
			code = if signal_number != 0 { 128 + signal_number } else { (status >> 8) & 0xff }
			return 0
		}
		return -1
	}

	fn native_run_signals_finish() i32 {
		mut previous_mask := NativeRunMask{}
		if native_run_block_signals(mut previous_mask) != 0 {
			return -1
		}
		native_run_restore_terminal()
		native_run_close_observer()
		if native_run_terminal_fd >= 0 {
			C.close(native_run_terminal_fd)
		}
		native_run_terminal_fd = -1
		native_run_child_pid = 0
		received_signal := native_run_termination_signal
		for index := 6; index >= 0; index-- {
			C.sigaction(native_run_signal_number(index), &native_run_previous[index], unsafe { nil })
		}
		native_run_restore_mask(&previous_mask)
		return received_signal
	}

	fn wait_run_process(mut process os.Process) !int {
		if native_run_signals_begin() != 0 {
			return error('could not install run signal handlers')
		}
		process.use_pgroup = true
		process.run()
		if process.pid <= 0 {
			process.status = .aborted
			process.close()
			native_run_signals_finish()
			return error('could not start run process group')
		}
		mut code := i32(0)
		mut signal_number := i32(0)
		if native_run_signals_set_pid(i32(process.pid)) != 0
			|| native_run_signals_wait(mut code, mut signal_number) != 0 {
			native_run_signals_kill(i32(process.pid))
			process.wait()
			native_run_signals_finish()
			return error('could not wait for run process group')
		}
		process.code = int(code)
		process.status = if signal_number != 0 { .aborted } else { .exited }
		if signal_number != 0 {
			process.err = 'Terminated by signal ${signal_number}'
		}
		process.close()
		return native_run_signals_finish()
	}
}
