import os
import time

const broken_stdout_child_source = r'
import os

fn main() {
	mut broken := os.pipe() or { panic(err) }
	saved_stdout := os.fd_dup(1)
	defer {
		os.fd_dup2(saved_stdout, 1)
		os.fd_close(saved_stdout)
		broken.close()
	}
	assert os.fd_dup2(broken.read_fd, 1) != -1
	os.fd_close(broken.read_fd)
	broken.read_fd = -1
	println("broken stdout should not hang")
}
'

fn test_println_does_not_hang_on_failed_stdout_write() {
	mut p := os.new_process(@VEXE)
	p.set_args(['-e', broken_stdout_child_source])
	p.set_redirect_stdio()
	p.run()
	defer {
		if p.is_alive() {
			p.signal_kill()
			p.wait()
		}
		p.close()
	}
	for _ in 0 .. 300 {
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
