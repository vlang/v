module builtin

const (
	sighup    = 0x1
	sigint    = 0x2
	sigquit   = 0x3
	sigill    = 0x4
	sigtrap   = 0x5
	sigabrt   = 0x6
	sigemt    = 0x7
	sigfpe    = 0x8
	sigkill   = 0x9
	sigbus    = 0xa
	sigsegv   = 0xb
	sigsys    = 0xc
	sigpipe   = 0xd
	sigalrm   = 0xe
	sigterm   = 0xf
	sigurg    = 0x10
	sigstop   = 0x11
	sigtstp   = 0x12
	sigcont   = 0x13
	sigchld   = 0x14
	sigttin   = 0x15
	sigttou   = 0x16
	sigio     = 0x17
	sigxcpu   = 0x18
	sigxfsz   = 0x19
	sigvtalrm = 0x1a
	sigprof   = 0x1b
	sigwinch  = 0x1c
	siginfo   = 0x1d
	sigusr1   = 0x1e
	sigusr2   = 0x1f
)

const (
	segv_maperr = 0x1
	segv_accerr = 0x2
)

[typedef]
union C.sigval {
	sival_int int
	sival_ptr voidptr
}

[typedef]
struct C.siginfo_t {
	si_signo  int      // signal number
	si_errno  int      // errno association
	si_code   int      // signal code
	si_pid    C.pid_t  // sending process
	si_uid    C.uid_t  // sender's ruid
	si_status int      // exit value
	si_addr   voidptr  // faulting instruction
	si_value  C.sigval // signal value
	si_band   i64      // band event for SIGPOLL
	__pad     [7]u64   // reserved for future Use
}

// sigaction_handler is the default signal handler for signals.
// It panics on SIGSEGV and SIGBUS.
//
// It is needed to process signals and give understandable error
// messages along with the stacktrace.
//
// See `Gen.gen_signal_handler()`
[markused]
fn sigaction_handler(sig int, info &C.siginfo_t, context voidptr) {
	match info.si_signo {
		sigsegv, sigbus {
			panic_mem()
		}
		else {}
	}
}
