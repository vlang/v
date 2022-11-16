module os

// - ProcessState.not_started - the process has not yet started
// - ProcessState.running - the process is currently running
// - ProcessState.stopped - the process was running, but was stopped temporarily
// - ProcessState.exited - the process has finished/exited
// - ProcessState.aborted - the process was terminated by a signal
// - ProcessState.closed - the process resources like opened file descriptors were freed/discarded, final state.
pub enum ProcessState {
	not_started
	running
	stopped
	exited
	aborted
	closed
}

[heap]
pub struct Process {
pub:
	filename string // the process's command file path
pub mut:
	pid  int // the PID of the process
	code int = -1
	// the exit code of the process, != -1 *only* when status is .exited *and* the process was not aborted
	status ProcessState = .not_started
	// the current status of the process
	err           string   // if the process fails, contains the reason why
	args          []string // the arguments that the command takes
	env_is_custom bool     // true, when the environment was customized with .set_environment
	env           []string // the environment with which the process was started  (list of 'var=val')
	use_stdio_ctl bool     // when true, then you can use p.stdin_write(), p.stdout_slurp() and p.stderr_slurp()
	use_pgroup    bool     // when true, the process will create a new process group, enabling .signal_pgkill()
	stdio_fd      [3]int   // the stdio file descriptors for the child process, used only by the nix implementation
	wdata         voidptr  // the WProcess; used only by the windows implementation
}

// new_process - create a new process descriptor
// Note: new does NOT start the new process.
// That is done because you may want to customize it first,
// by calling different set_ methods on it.
// In order to start it, call p.run() or p.wait()
pub fn new_process(filename string) &Process {
	return &Process{
		filename: filename
		stdio_fd: [-1, -1, -1]!
	}
}

// set_args - set the arguments for the new process
pub fn (mut p Process) set_args(pargs []string) {
	if p.status != .not_started {
		return
	}
	p.args = pargs
	return
}

// set_environment - set a custom environment variable mapping for the new process
pub fn (mut p Process) set_environment(envs map[string]string) {
	if p.status != .not_started {
		return
	}
	p.env_is_custom = true
	p.env = []string{}
	for k, v in envs {
		p.env << '${k}=${v}'
	}
	return
}
