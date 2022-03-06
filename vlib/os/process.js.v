module os

$if js_node {
	#const $child_process = require('child_process')
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

fn (mut p Process) spawn_internal() {
	#p.val.pid = $child_process.spawn(
	#p.val.filename+'',
	#p.val.args.arr.map((x) => x.valueOf() + ''),
	#{
	#env: (p.val.env_is_custom ? p.val.env : $process.env),
	#})
	#p.val.pid.on('error', function (err) { builtin.panic('Failed to start subprocess') })

	p.status = .running
	if p.use_stdio_ctl {
		#p.val.pid.stdout.pipe(process.stdout)
		#p.val.pid.stdin.pipe(process.stdin)
		#p.val.pid.stderr.pipe(process.stderr)
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
	#p.val.pid.kill('SIGKILL');

	p.status = .aborted
}

pub fn (mut p Process) signal_stop() {
	if p.status !in [.running, .stopped] {
		return
	}
	#p.val.pid.kill('SIGSTOP');

	p.status = .aborted
}

pub fn (mut p Process) signal_continue() {
	if p.status != .stopped {
		return
	}
	#p.val.pid.kill('SIGCONT');

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
	#p.val.pid.on('exit', function (code) { console.log(code) })
}

pub fn (mut p Process) set_redirect_stdio() {
	p.use_stdio_ctl = true
	return
}

pub fn (mut p Process) stdin_write(s string) {
	p.check_redirection_call('stdin_write')
	#p.val.pid.stdin.write(s)
}

pub fn (mut p Process) stdin_resume() {
	#p.val.pid.stdin.resume()
}

// todo(playX): probably does not work

// will read from stdout pipe, will only return when EOF (end of file) or data
// means this will block unless there is data
pub fn (mut p Process) stdout_slurp() string {
	p.check_redirection_call('stdout_slurp')
	mut res := ''
	#p.val.pid.stdout.on('data', function (data) { res = new string(data) })

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

pub fn (mut p Process) close() {
	// no-op on JS backend
}
