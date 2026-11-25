module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_signal_abi.h"
	fn C.v_signal_action_get(uint, uint)
	fn C.v_signal_action_set(uint, uint)
	fn C.v_signal_restorer() uint
	fn C.v_signal_description_buffer() uint
	fn C.v_signal_mask_set(uint, u64)
	fn C.v_signal_queue_info(uint, i32, i32, u32, uint)

	@[export: 'v_native_sigaltstack'; markused]
	fn bare_native_sigaltstack(input uint, output uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_sigaltstack), u64(input), u64(output))))
	}

	@[export: 'v_native_raise'; markused]
	fn bare_native_raise(number i32) i32 {
		if number == 32 || number == 33 { return i32(bare_native_result(u64(-i64(C.EINVAL)))) }
		return i32(bare_native_result(sys_call3(u64(C.SYS_tgkill), sys_call0(u64(C.SYS_getpid)),
			sys_call0(u64(C.SYS_gettid)), u64(number))))
	}

	@[export: 'v_native_pthread_kill'; markused]
	fn bare_native_pthread_kill(handle uint, number i32) i32 {
		if number < 0 || number > 64 || number == 32 || number == 33 { return i32(C.EINVAL) }
		pid := sys_call0(u64(C.SYS_getpid))
		// A synchronous handler may call pthread APIs. Never hold the target's
		// metadata lock when delivering a signal to the calling thread.
		if handle == uint(bare_pthread_current_get()) {
			return i32(-i64(sys_call3(u64(C.SYS_tgkill), pid, sys_call0(u64(C.SYS_gettid)), u64(number))))
		}
		mut thread := unsafe { &BarePthread(handle) }
		bare_compat_lock(&thread.metadata_lock)
		defer { bare_compat_unlock(&thread.metadata_lock) }
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return 0 }
		return i32(-i64(sys_call3(u64(C.SYS_tgkill), pid, u64(tid), u64(number))))
	}

	struct BareNativeSignalAction {
	mut:
		handler  uint
		flags    uint
		restorer uint
		mask     u64
	}
	@[export: 'v_native_sigaction'; markused]
	fn bare_native_sigaction(number i32, input uint, output uint) i32 {
		if number < 1 || number > 64 || number == 32 || number == i32(C.SIGKILL) || number == i32(C.SIGSTOP) {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		mut action := BareNativeSignalAction{}
		mut previous := BareNativeSignalAction{}
		if input != 0 {
			C.v_signal_action_get(input, uint(&action))
			$if amd64 {
				if action.flags & 0x04000000 == 0 || action.restorer == 0 {
					action.flags |= 0x04000000
					action.restorer = C.v_signal_restorer()
				}
			}
		}
		result := bare_native_result(sys_call4(u64(C.SYS_rt_sigaction), u64(number), if input != 0 {
			u64(&action)
		} else {
			u64(0)
		}, if output != 0 { u64(&previous) } else { u64(0) }, 8))
		if result >= 0 && output != 0 { C.v_signal_action_set(output, uint(&previous)) }
		return i32(result)
	}
	@[export: 'v_native_signal'; markused]
	fn bare_native_signal(number i32, handler uint) uint {
		if number < 1 || number > 64 || number == 32 || number == i32(C.SIGKILL) || number == i32(C.SIGSTOP) || handler == ~uint(0) {
			bf_native_errno_cell = i32(C.EINVAL)
			return ~uint(0)
		}
		mut action := BareNativeSignalAction{ handler: handler, flags: uint(C.SA_RESTART), mask: u64(1) << (number - 1) }
		mut previous := BareNativeSignalAction{}
		$if amd64 {
			action.flags |= 0x04000000
			action.restorer = C.v_signal_restorer()
		}
		if bare_native_result(sys_call4(u64(C.SYS_rt_sigaction), u64(number), u64(&action), u64(&previous), 8)) < 0 {
			return ~uint(0)
		}
		return previous.handler
	}
	@[export: 'v_native_sigpending'; markused]
	fn bare_native_sigpending(output uint) i32 {
		if output == 0 {
			bf_native_errno_cell = i32(C.EFAULT)
			return -1
		}
		mut mask := u64(0)
		result := bare_native_result(sys_call2(u64(C.SYS_rt_sigpending), u64(&mask), 8))
		if result == 0 { C.v_signal_mask_set(output, mask) }
		return i32(result)
	}

	@[export: 'v_native_sigtimedwait'; markused]
	fn bare_native_sigtimedwait(set uint, info uint, timeout uint) i32 {
		return i32(bare_native_result(sys_call4(u64(C.SYS_rt_sigtimedwait), u64(set), u64(info), u64(timeout), 8)))
	}

	@[export: 'v_native_sigwait'; markused]
	fn bare_native_sigwait(set uint, output uint) i32 {
		saved := bf_native_errno_cell
		if output == 0 { return i32(C.EINVAL) }
		for {
			result := bare_native_sigtimedwait(set, 0, 0)
			failure := bf_native_errno_cell
			bf_native_errno_cell = saved
			if result >= 0 {
				unsafe { *(&i32(output)) = result }
				return 0
			}
			if failure != i32(C.EINTR) { return failure }
		}
		return 0
	}

	@[export: 'v_native_sigsuspend'; markused]
	fn bare_native_sigsuspend(set uint) i32 {
		return i32(bare_native_result(sys_call2(u64(C.SYS_rt_sigsuspend), u64(set), 8)))
	}

	@[export: 'v_native_sigqueue'; markused]
	fn bare_native_sigqueue(pid i32, number i32, value uint) i32 {
		mut info := [16]u64{}
		C.v_signal_queue_info(uint(&info[0]), number, i32(sys_call0(u64(C.SYS_getpid))), u32(sys_getuid()), value)
		return i32(bare_native_result(sys_call3(u64(C.SYS_rt_sigqueueinfo), u64(pid), u64(number), u64(&info[0]))))
	}
	@[export: 'v_native_strsignal'; markused]
	fn bare_native_strsignal(number i32) uint {
		known := match number {
			1 { uint(c'Hangup') }
			2 { uint(c'Interrupt') }
			3 { uint(c'Quit') }
			4 { uint(c'Illegal instruction') }
			5 { uint(c'Trace/breakpoint trap') }
			6 { uint(c'Aborted') }
			7 { uint(c'Bus error') }
			8 { uint(c'Floating point exception') }
			9 { uint(c'Killed') }
			10 { uint(c'User defined signal 1') }
			11 { uint(c'Segmentation fault') }
			12 { uint(c'User defined signal 2') }
			13 { uint(c'Broken pipe') }
			14 { uint(c'Alarm clock') }
			15 { uint(c'Terminated') }
			16 { uint(c'Stack fault') }
			17 { uint(c'Child exited') }
			18 { uint(c'Continued') }
			19 { uint(c'Stopped (signal)') }
			20 { uint(c'Stopped') }
			21 { uint(c'Stopped (tty input)') }
			22 { uint(c'Stopped (tty output)') }
			23 { uint(c'Urgent I/O condition') }
			24 { uint(c'CPU time limit exceeded') }
			25 { uint(c'File size limit exceeded') }
			26 { uint(c'Virtual timer expired') }
			27 { uint(c'Profiling timer expired') }
			28 { uint(c'Window changed') }
			29 { uint(c'I/O possible') }
			30 { uint(c'Power failure') }
			31 { uint(c'Bad system call') }
			else { uint(0) }
		}
		if known != 0 { return known }
		output := C.v_signal_description_buffer()
		realtime := number >= 34 && number <= 64
		prefix := if realtime { uint(c'Real-time signal ') } else { uint(c'Unknown signal ') }
		mut position := bare_string_strlcpy(output, prefix, 96)
		mut value := if realtime { i64(number - 34) } else { i64(number) }
		if value < 0 {
			unsafe { *(&u8(output + position)) = `-` }
			position++
			value = -value
		}
		mut digits := [10]u8{}
		mut count := 0
		for {
			digits[count] = u8(value % 10) + `0`
			count++
			value /= 10
			if value == 0 { break }
		}
		for i in 0 .. count {
			unsafe { *(&u8(output + position + uint(i))) = digits[count - i - 1] }
		}
		unsafe { *(&u8(output + position + uint(count))) = 0 }
		return output
	}
}
