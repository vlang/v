module builtin

$if amd64 && !native {
	#include <signal.h>

	type BareMacSignalHandler = fn (i32)

	// XNU's input includes the userspace return trampoline. The old-action
	// result has the public sigaction layout and omits that pointer.
	struct BareMacSignalAction {
		handler    BareMacSignalHandler = unsafe { nil } // SIG_DFL is zero.
		trampoline fn ()                = unsafe { nil }
		mask       u32
		flags      i32
	}

	struct BareMacPreviousSignalAction {
		handler BareMacSignalHandler = unsafe { nil } // The old action can be SIG_DFL.
		mask    u32
		flags   i32
	}

	// Keep the SDK's function-pointer ABI in the exported wrapper, rather
	// than the erased pointer used by the general builtin C declaration.
	fn C.signal(signal i32, handler BareMacSignalHandler) BareMacSignalHandler

	@[export: 'signal'; markused; no_profile; no_trace]
	fn bare_macos_signal(signal i32, handler BareMacSignalHandler) BareMacSignalHandler {
		action := BareMacSignalAction{
			handler:    handler
			trampoline: bare_macos_signal_entry
			// SA_RESTART and XNU's token-validated sigreturn convention.
			flags:      0x402
		}
		mut previous := BareMacPreviousSignalAction{}
		code := mac_kernel_bsd_call6(46, u64(signal), u64(&action), u64(&previous), 0,
			0, 0, unsafe { nil })
		if code != 0 {
			C.errno = code
			return unsafe { BareMacSignalHandler(C.SIG_ERR) }
		}
		return previous.handler
	}

	// Darwin enters with catcher/style/signal/info/ucontext/token in the six
	// argument registers. sigreturn restores the interrupted register state;
	// keep its context and validation token in callee-saved registers while
	// invoking the ordinary one-argument signal handler.
	@[_naked; no_profile; no_trace]
	fn bare_macos_signal_entry() {
		asm volatile amd64 {
			push rbp
			mov rbp, rsp
			mov rax, rdi
			mov rbx, r8
			mov r12, r9
			mov edi, edx
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
