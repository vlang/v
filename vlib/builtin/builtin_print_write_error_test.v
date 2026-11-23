import os
import time

const broken_stdout_probe_arg = '--vtest-broken-stdout'

fn testsuite_begin() {
	$if !windows {
		if os.args.len == 2 && os.args[1] == broken_stdout_probe_arg {
			print_to_broken_stdout()
			exit(0)
		}
	}
}

fn print_to_broken_stdout() {
	mut broken := os.pipe() or { panic(err) }
	saved_stdout := os.fd_dup(1)
	defer {
		os.fd_dup2(saved_stdout, 1)
		os.fd_close(saved_stdout)
		broken.close()
	}
	if os.fd_dup2(broken.read_fd, 1) == -1 {
		panic('could not replace stdout with the read end of the pipe')
	}
	os.fd_close(broken.read_fd)
	broken.read_fd = -1
	println('broken stdout should not hang')
}

fn test_println_does_not_hang_on_failed_stdout_write() {
	$if windows {
		return
	}
	mut p := os.new_process(os.executable())
	p.set_args([broken_stdout_probe_arg])
	p.set_redirect_stdio()
	p.run()
	defer {
		if p.is_alive() {
			p.signal_kill()
			p.wait()
		}
		p.close()
	}
	max_wait_iterations := $if s390x || rv64 { 1200 } $else { 300 }
	for _ in 0 .. max_wait_iterations {
		if !p.is_alive() {
			break
		}
		time.sleep(50 * time.millisecond)
	}
	if p.is_alive() {
		p.signal_kill()
		p.wait()
		stdout := p.stdout_slurp()
		stderr := p.stderr_slurp()
		assert false, 'println hung on a failed stdout write\nstdout:\n${stdout}\nstderr:\n${stderr}'
	}
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	assert p.code == 0, 'child exited with ${p.code}\nstdout:\n${stdout}\nstderr:\n${stderr}'
	assert stdout == '', stdout
	assert stderr == '', stderr
}
