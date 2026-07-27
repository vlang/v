module main

import os
import time

const child_poll_interval = 5 * time.millisecond
const child_gate_timeout = 5 * time.second

fn main() {
	if os.args.len < 3 {
		panic('watchdog child mode is required')
	}
	match os.args[1] {
		'clean' { run_clean() or { panic(err) } }
		'failure' { run_failure() or { panic(err) } }
		'root' { run_root() or { panic(err) } }
		'root_exits' { run_root_exits() or { panic(err) } }
		'descendant' { run_descendant() or { panic(err) } }
		else { panic('unknown watchdog child mode `${os.args[1]}`') }
	}
}

fn run_failure() ! {
	if os.args.len != 3 {
		return error('failure requires a gate path')
	}
	await_watchdog_gate(os.args[2])!
	println('watchdog intentional failure')
	exit(37)
}

fn run_clean() ! {
	if os.args.len != 3 {
		return error('clean requires a gate path')
	}
	await_watchdog_gate(os.args[2])!
	payload := 'watchdog-output-drain-'.repeat(16)
	for index in 0 .. 512 {
		println('stdout:${index}:${payload}')
		eprintln('stderr:${index}:${payload}')
	}
	println('{"probe":"watchdog_self_test","status":"PASS","cleanup":"complete"}')
}

fn run_root() ! {
	if os.args.len != 5 {
		return error('root requires gate, pid, and started paths')
	}
	spawn_watchdog_descendant(os.args[2], os.args[3], os.args[4])!
	for {
		time.sleep(time.second)
	}
}

fn run_root_exits() ! {
	if os.args.len != 5 {
		return error('root_exits requires gate, pid, and started paths')
	}
	spawn_watchdog_descendant(os.args[2], os.args[3], os.args[4])!
	deadline := time.now().add(child_gate_timeout)
	for !os.exists(os.args[4]) {
		if time.now() >= deadline {
			return error('watchdog descendant did not report startup')
		}
		time.sleep(child_poll_interval)
	}
}

fn spawn_watchdog_descendant(gate_path string, pid_path string, started_path string) ! {
	await_watchdog_gate(gate_path)!
	mut child := os.new_process(os.executable())
	child.set_args(['descendant', started_path])
	child.run()
	if child.pid <= 0 {
		return error('failed to spawn watchdog descendant')
	}
	os.write_file(pid_path, '${child.pid}\n')!
}

fn await_watchdog_gate(gate_path string) ! {
	deadline := time.now().add(child_gate_timeout)
	for !os.exists(gate_path) {
		if time.now() >= deadline {
			return error('watchdog parent did not release the start gate')
		}
		time.sleep(child_poll_interval)
	}
}

fn run_descendant() ! {
	if os.args.len != 3 {
		return error('descendant requires a started path')
	}
	os.write_file(os.args[2], '${os.getpid()}\n')!
	for {
		time.sleep(time.second)
	}
}
