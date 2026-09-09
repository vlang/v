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
	$if windows {
		return
	}
	stamp := time.now().unix_milli()
	child_source_path := os.join_path(os.vtmp_dir(), 'broken_stdout_child_${stamp}.v')
	child_binary_path := os.join_path(os.vtmp_dir(), 'broken_stdout_child_${stamp}.bin')
	os.write_file(child_source_path, broken_stdout_child_source)!
	defer {
		os.rm(child_source_path) or {}
		os.rm(child_binary_path) or {}
	}
	compile_cmd := '${os.quoted_path(@VEXE)} -o ${os.quoted_path(child_binary_path)} ${os.quoted_path(child_source_path)}'
	compile_result := os.execute(compile_cmd)
	assert compile_result.exit_code == 0, 'child compilation failed\ncommand: ${compile_cmd}\noutput:\n${compile_result.output}'
	mut p := os.new_process(child_binary_path)
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
