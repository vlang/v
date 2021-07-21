module os_js

#const $child_process = require('child_process')

// ProcessState.not_started - the process has not yet started
// ProcessState.running - the process is currently running
// ProcessState.stopped - the process was running, but was stopped temporarily
// ProcessState.exited - the process has finished/exited
// ProcessState.aborted - the process was terminated by a signal
// ProcessState.closed - the process resources like opened file descriptors were freed/discarded, final state.
pub enum ProcessState {
	not_started
	running
	stopped
	exited
	aborted
	closed
}

// todo(playX): fix reference member access in JS backend
// [heap]
pub struct Process {
pub:
	filename string
pub mut:
	pid    voidptr
	code   int = -1
	status ProcessState = .not_started
	// the current status of the process
	err           string   // if the process fails, contains the reason why
	args          []string // the arguments that the command takes
	env_is_custom bool     // true, when the environment was customized with .set_environment
	env           []string // the environment with which the process was started  (list of 'var=val')
	use_stdio_ctl bool     // when true, then you can use p.stdin_write(), p.stdout_slurp() and p.stderr_slurp()
	use_pgroup    bool     // when true, the process will create a new process group, enabling .signal_pgkill()
	stdio_fd      [3]int   // the stdio file descriptors for the child process, used only by the nix implementation
}

// new_process - create a new process descriptor
// NB: new does NOT start the new process.
// That is done because you may want to customize it first,
// by calling different set_ methods on it.
// In order to start it, call p.run() or p.wait()
pub fn new_process(filename string) Process {
	return Process{
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
		p.env << '$k=$v'
	}
	return
}

fn (mut p Process) spawn_internal() {
	#p.pid = $child_process.spawn(
	#p.filename+'',
	#p.args.arr.map((x) => x.valueOf() + ''),
	#{
	#env: (p.env_is_custom ? p.env : $process.env),
	#})
	#p.pid.on('error', function (err) { builtin.panic('Failed to start subprocess') })

	p.status = .running
	// todo(playX): stderr,stdin
	if p.use_stdio_ctl {
		#p.pid.stdout.pipe(process.stdout)
		#p.pid.stdin.pipe(process.stdin)
		#p.pid.stderr.pipe(process.stderr)
	}
}

pub fn (mut p Process) run() {
	if p.status != .not_started {
		return
	}
	p.spawn_internal()
	return
}

pub fn (mut p Process) signal_kill() {
	if p.status !in [.running, .stopped] {
		return
	}
	#p.pid.kill('SIGKILL');

	p.status = .aborted
}

pub fn (mut p Process) signal_stop() {
	if p.status !in [.running, .stopped] {
		return
	}
	#p.pid.kill('SIGSTOP');

	p.status = .aborted
}

pub fn (mut p Process) signal_continue() {
	if p.status != .stopped {
		return
	}
	#p.pid.kill('SIGCONT');

	p.status = .running
	return
}

pub fn (mut p Process) wait() {
	if p.status == .not_started {
		p.spawn_internal()
	}
	if p.status !in [.running, .stopped] {
		return
	}

	p.wait_internal()
	return
}

fn (mut p Process) wait_internal() {
	#p.pid.on('exit', function (code) { console.log(code) })
}

pub fn (mut p Process) set_redirect_stdio() {
	p.use_stdio_ctl = true
	return
}

pub fn (mut p Process) stdin_write(s string) {
	p.check_redirection_call('stdin_write')
	#p.pid.stdin.write(s)
}

// todo(playX): probably does not work

// will read from stdout pipe, will only return when EOF (end of file) or data
// means this will block unless there is data
pub fn (mut p Process) stdout_slurp() string {
	p.check_redirection_call('stdout_slurp')
	mut res := ''
	#p.pid.stdout.on('data', function (data) { res = new builtin.string(data) })

	return res
}

// _check_redirection_call - should be called just by stdxxx methods
fn (mut p Process) check_redirection_call(fn_name string) {
	if !p.use_stdio_ctl {
		panic('Call p.set_redirect_stdio() before calling p.$fn_name')
	}
	if p.status == .not_started {
		panic('Call p.${fn_name}() after you have called p.run()')
	}
}
