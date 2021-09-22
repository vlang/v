module os

#include <signal.h>

// os.Signal - enumerate possible POSIX signals and
// their integer codes.
// NB: the integer codes are given here explicitly,
// to make it easier to lookup, without needing to
// consult man pages / signal.h .

pub enum Signal {
	hup = 1
	int = 2
	quit = 3
	ill = 4
	trap = 5
	abrt = 6
	bus = 7
	fpe = 8
	kill = 9
	usr1 = 10
	segv = 11
	usr2 = 12
	pipe = 13
	alrm = 14
	term = 15
	stkflt = 16
	chld = 17
	cont = 18
	stop = 19
	tstp = 20
	ttin = 21
	ttou = 22
	urg = 23
	xcpu = 24
	xfsz = 25
	vtalrm = 26
	prof = 27
	winch = 28
	poll = 29
	pwr = 30
	sys = 31
}

type SignalHandler = fn (Signal)

fn C.signal(signal int, handlercb SignalHandler) voidptr

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
