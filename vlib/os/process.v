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

@[heap]
pub struct Process {
pub mut:
	 // the process's command file path
	filename string
	// the PID of the process
	pid      int
	// the exit code of the process, != -1 *only* when status is .exited *and* the process was not aborted
	code     int = -1
	// the current status of the process
	status ProcessState = .not_started
	// if the process fails, contains the reason why
	err              string
	// the arguments that the command takes
	args             []string
	// the initial working folder of the process. When '', reuse the same folder as the parent process.
	work_folder      string
	// true, when the environment was customized with .set_environment
	env_is_custom    bool
	// the environment with which the process was started  (list of 'var=val')
	env              []string
	// when true, then you can use p.stdin_write(), p.stdout_slurp() and p.stderr_slurp()
	use_stdio_ctl    bool
	// when true, the process will create a new process group, enabling .signal_pgkill()
	use_pgroup       bool
	// the stdio file descriptors for the child process, used only by the nix implementation
	stdio_fd         [3]int
	// the WProcess; used only by the windows implementation
	wdata            voidptr
	// sets a value indicating whether to start the process in a new window, The default is false; used only by the windows implementation
	create_no_window bool
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

// set_work_folder - set the initial working folder for the new process
// If you do not set it, it will reuse the current working folder of the parent process.
pub fn (mut p Process) set_work_folder(path string) {
	if p.status != .not_started {
		return
	}
	p.work_folder = real_path(path)
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
