module os

fn (mut p Process) win_spawn_process() int {
	eprintln('TODO implement waiting for a process on windows')
	return 12345
}

fn (mut p Process) win_stop_process() {
	eprintln('TODO implement stopping a process on windows')
}

fn (mut p Process) win_resume_process() {
	eprintln('TODO implement resuming a process on windows')
}

fn (mut p Process) win_kill_process() {
	eprintln('TODO implement killing a process on windows')
}

fn (mut p Process) win_wait() {
	eprintln('TODO implement waiting for a process on windows')
	p.status = .exited
	p.code = 0
}

fn (mut p Process) win_is_alive() bool {
	eprintln('TODO implement checking whether the process is still alive on windows')
	return false
}

//
// these are here to make v_win.c/v.c generation work in all cases:
fn (mut p Process) unix_spawn_process() int {
	return 0
}

fn (mut p Process) unix_stop_process() {
}

fn (mut p Process) unix_resume_process() {
}

fn (mut p Process) unix_kill_process() {
}

fn (mut p Process) unix_wait() {
}

fn (mut p Process) unix_is_alive() bool {
	return false
}
