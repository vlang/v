module os

// os.Signal - enumerate possible POSIX signals and
// their integer codes.
// Note: the integer codes are given here explicitly,
// to make it easier to lookup, without needing to
// consult man pages / signal.h .

pub enum Signal {
	hup    = 1  // hangup
	int    = 2  // interrupt from keyboard
	quit   = 3  // quit from keyboard
	ill    = 4  // illegal instruction
	trap   = 5  // trace trap
	abrt   = 6  // abort
	bus    = 7  // bus error
	fpe    = 8  // floating point exception
	kill   = 9  // kill signal
	usr1   = 10 // user-defined signal 1
	segv   = 11 // segmentation violation
	usr2   = 12 // user-defined signal 2
	pipe   = 13 // write on a pipe with no one to read it
	alrm   = 14 // alarm clock
	term   = 15 // software termination signal
	stkflt = 16 // stack fault
	chld   = 17 // child process has stopped or exited
	cont   = 18 // continue executing, if stopped
	stop   = 19 // stop executing (cannot be caught or ignored)
	tstp   = 20 // stop executing, can be caught and ignored
	ttin   = 21 // terminal input for background process
	ttou   = 22 // terminal output for background process
	urg    = 23 // urgent condition on socket
	xcpu   = 24 // CPU time limit exceeded
	xfsz   = 25 // file size limit exceeded
	vtalrm = 26 // virtual time alarm
	prof   = 27 // profiling timer alarm
	winch  = 28 // window size change
	poll   = 29 // pollable event occurred
	pwr    = 30 // power failure
	sys    = 31 // bad system call
}

pub type SignalHandler = fn (Signal)
