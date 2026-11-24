module macos_raw_signal

// Test-only XNU signal entry shared by the thread and clock fixtures. The
// input wire layout and six entry registers match the frozen native C oracle.
$if freestanding && macos && amd64 && !native {
	pub type Handler = fn (i32, voidptr, voidptr)

	struct SignalAction {
		handler    Handler
		trampoline fn ()
		mask       u32
		flags      i32
	}

	fn C.v_macos_kernel_bsd_call6(number u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64, result &u64) i32

	// install_usr1 installs a test handler with XNU's sigreturn validation token.
	@[no_profile; no_trace]
	pub fn install_usr1(handler Handler, restart bool) i32 {
		action := SignalAction{
			handler:    handler
			trampoline: signal_entry
			mask:       0
			flags:      if restart { i32(0x402) } else { i32(0x400) }
		}
		if sizeof(SignalAction) != 24 { return 22 }
		return C.v_macos_kernel_bsd_call6(46, 30, u64(&action), 0, 0, 0, 0, unsafe { nil })
	}

	// mask reads the current uthread mask without changing process-wide masks.
	@[no_profile; no_trace]
	pub fn mask() (u32, i32) {
		mut old := u32(0)
		code := C.v_macos_kernel_bsd_call6(329, 3, 0, u64(&old), 0, 0, 0, unsafe { nil })
		return old, code
	}

	// set_mask replaces only the caller's uthread mask.
	@[no_profile; no_trace]
	pub fn set_mask(value u32) i32 {
		return C.v_macos_kernel_bsd_call6(329, 3, u64(&value), 0, 0, 0, 0, unsafe { nil })
	}

	// send_usr1 targets a live thread send right; ESRCH is the native value 3.
	@[no_profile; no_trace]
	pub fn send_usr1(thread u32) i32 {
		return C.v_macos_kernel_bsd_call6(328, thread, 30, 0, 0, 0, 0, unsafe { nil })
	}

	@[_naked; no_profile; no_trace]
	fn signal_entry() {
		asm volatile amd64 {
			push rbp
			mov rbp, rsp
			mov rax, rdi
			mov rbx, r8
			mov r12, r9
			mov edi, edx
			mov rsi, rcx
			mov rdx, r8
			call rax
			mov rdi, rbx
			mov esi, 30
			mov rdx, r12
			mov eax, 33554616
			syscall
			ud2
		}
	}
}
