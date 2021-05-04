module os

#include <signal.h>

pub enum Signal {
	hup = 1
	int
	quit
	ill
	trap
	abrt
	bus
	fpe
	kill
	usr1
	segv
	usr2
	pipe
	alrm
	term
	stkflt
	chld
	cont
	stop
	tstp
	ttin
	ttou
	urg
	xcpu
	xfsz
	vtalrm
	prof
	winch
	poll
	pwr
	sys
}

type SignalHandler = fn (Signal)

fn C.signal(signal int, handlercb SignalHandler) voidptr

[deprecated: 'use os.signal_opt() instead']
[deprecated_after: '2021-05-18']
pub fn signal(signum int, handler voidptr) voidptr {
	return voidptr(signal_opt(Signal(signum), handler) or { C.SIG_ERR })
}

// signal will assign `handler` callback to be called when `signum` signal is received.
pub fn signal_opt(signum Signal, handler SignalHandler) ?SignalHandler {
	C.errno = 0
	prev_handler := C.signal(int(signum), handler)
	if prev_handler == C.SIG_ERR {
		// errno isn't correctly set on Windows, but EINVAL is this only possible value it can take anyway
		return error_with_code(posix_get_error_msg(C.EINVAL), C.EINVAL)
	}
	return SignalHandler(prev_handler)
}
