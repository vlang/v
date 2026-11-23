import os
import time

const ready_marker = 'ready\n'
const redirected_stdout_probe_arg = '--vtest-redirected-stdout'

fn testsuite_begin() {
	if os.args.len != 3 || os.args[1] != redirected_stdout_probe_arg {
		return
	}
	match os.args[2] {
		'lf' { print('\n') }
		'crlf' { print('\r\n') }
		else { exit(2) }
	}
	exit(0)
}

fn test_stdout_is_flushed_by_default() {
	$if windows {
		return
	}
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

fn redirected_stdout_bytes(mode string) ![]u8 {
	output_path := os.join_path(os.vtmp_dir(), 'redirected_stdout_${os.getpid()}_${mode}.bin')
	defer {
		os.rm(output_path) or {}
	}
	res := os.exec(['sh', '-c', '"\${1}" "\${2}" "\${3}" > "\${4}"', 'redirected-stdout',
		os.executable(), redirected_stdout_probe_arg, mode, output_path])
	assert res.exit_code == 0, '${mode} probe failed: ${res.output}'
	return os.read_bytes(output_path)!
}

fn assert_redirected_stdout_bytes(mode string, expected []u8) ! {
	bytes := redirected_stdout_bytes(mode)!
	assert bytes == expected, 'expected ${expected}, got ${bytes}'
}

fn test_redirected_stdout_preserves_newline_bytes() ! {
	assert_redirected_stdout_bytes('lf', [u8(`\n`)])!
	assert_redirected_stdout_bytes('crlf', [u8(`\r`), `\n`])!
}
