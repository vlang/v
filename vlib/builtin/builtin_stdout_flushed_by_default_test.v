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

fn redirected_stdout_bytes(snippet string) ![]u8 {
	stamp := time.now().unix_milli()
	source_path := os.join_path(os.vtmp_dir(), 'redirected_stdout_${stamp}.v')
	output_path := os.join_path(os.vtmp_dir(), 'redirected_stdout_${stamp}.bin')
	os.write_file(source_path, 'fn main() {\n\t${snippet}\n}\n')!
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	cmd := '${os.quoted_path(@VEXE)} run ${os.quoted_path(source_path)} > ${os.quoted_path(output_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, 'command failed: ${cmd}\noutput:\n${res.output}'
	return os.read_bytes(output_path)!
}

fn assert_redirected_stdout_bytes(snippet string, expected []u8) ! {
	bytes := redirected_stdout_bytes(snippet)!
	assert bytes == expected, 'expected ${expected}, got ${bytes}'
}

fn test_redirected_stdout_preserves_newline_bytes() ! {
	assert_redirected_stdout_bytes(r"print('\n')", [u8(`\n`)])!
	assert_redirected_stdout_bytes(r"print('\r\n')", [u8(`\r`), `\n`])!
}
