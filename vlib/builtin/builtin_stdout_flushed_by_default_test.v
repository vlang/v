import os
import time

const ready_marker = 'ready\n'

fn test_stdout_is_flushed_by_default() {
	mut cap := os.stdio_capture()!
	println('ready')
	mut pending := false
	for _ in 0 .. 100 {
		if os.fd_is_pending(cap.stdout.read_fd) {
			pending = true
			break
		}
		time.sleep(10 * time.millisecond)
	}
	cap.stop()
	stdout := cap.stdout.slurp().join('')
	stderr := cap.stderr.slurp().join('')
	cap.close()
	assert pending
	assert stdout == ready_marker, stdout
	assert stderr == ''
}
