import os
import time

fn test_getpid() {
	pid := os.getpid()
	eprintln('current pid: $pid')
	assert pid != 0
}

fn test_run() {
	if os.user_os() == 'windows' {
		return
	}
	//
	mut p := os.new_process('/bin/sleep')
	p.set_args(['0.2'])
	p.run()
	assert p.status == .running
	assert p.pid > 0
	assert p.pid != os.getpid()
	mut i := 0
	for {
		if !p.is_alive() {
			break
		}
		os.system('ps -opid= -oppid= -ouser= -onice= -of= -ovsz= -orss= -otime= -oargs= -p $p.pid')
		time.sleep_ms(50)
		i++
	}
	p.wait()
	assert p.code == 0
	assert p.status == .exited
	//
	eprintln('polling iterations: $i')
	assert i < 20
}

fn test_wait() {
	if os.user_os() == 'windows' {
		return
	}
	mut p := os.new_process('/bin/date')
	p.wait()
	assert p.pid != os.getpid()
	assert p.code == 0
	assert p.status == .exited
}

fn test_slurping_output() {
	if os.user_os() == 'windows' {
		return
	}
	mut p := os.new_process('/bin/date')
	p.set_redirect_stdio()
	p.wait()
	assert p.code == 0
	assert p.status == .exited
	output := p.stdout_slurp().trim_space()
	errors := p.stderr_slurp().trim_space()
	eprintln('p output: "$output"')
	eprintln('p errors: "$errors"')
}
