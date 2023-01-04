module os

// os.Signal - enumerate possible POSIX signals and
// their integer codes.
// Note: the integer codes are given here explicitly,
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
