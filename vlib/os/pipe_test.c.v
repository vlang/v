module main

import os

fn test_pipe_basic() {
	println('=== Testing basic pipe functionality ===')

	mut pipe := os.pipe()!
	defer {
		pipe.close()
	}

	test_data := 'Hello, Pipe!'
	bytes_written := pipe.write(test_data.bytes()) or {
		eprintln('Failed to write to pipe: ${err}')
		return
	}
	println('Written ${bytes_written} bytes to pipe')

	mut buffer := []u8{len: 1024}
	bytes_read := pipe.read(mut buffer) or {
		eprintln('Failed to read from pipe: ${err}')
		return
	}

	received_data := buffer[0..bytes_read].bytestr()
	println('Read ${bytes_read} bytes from pipe: "${received_data}"')

	assert bytes_written == bytes_read
	assert test_data == received_data
	println('✓ Basic pipe test passed')
	println('')
}

fn test_pipe_large_data() {
	println('=== Testing pipe with large data ===')

	mut pipe := os.pipe()!
	defer {
		pipe.close()
	}

	large_data := 'X'.repeat(1000)
	bytes_written := pipe.write(large_data.bytes()) or {
		eprintln('Failed to write large data to pipe: ${err}')
		return
	}
	println('Written ${bytes_written} bytes of large data to pipe')

	mut buffer := []u8{len: 1024}
	bytes_read := pipe.read(mut buffer) or {
		eprintln('Failed to read large data from pipe: ${err}')
		return
	}

	assert bytes_written == bytes_read
	assert bytes_read == 1000
	println('✓ Large data pipe test passed')
	println('')
}

// Note: following tests can only passed by `v pipe_test.c.v` not `v test pipe_test.c.v`
/*
fn test_capture_stdout() {
	println('=== Testing stdout capture ===')

	mut cap := os.Capture.new()

	cap.stdout_stderr_capture_start() or {
		eprintln('Failed to start capture: ${err}')
		return
	}

	test_output := 'This is stdout test message'
	println(test_output)
	println('Another stdout line')

	stdout, stderr := cap.stdout_stderr_capture_stop() or {
		eprintln('Failed to stop capture: ${err}')
		return
	}

	println('Captured stdout: "${stdout}"')
	println('Captured stderr: "${stderr}"')

	assert stdout.contains(test_output)
	assert stdout.contains('Another stdout line')
	assert stderr == ''
	println('✓ Stdout capture test passed')
	println('')
}

fn test_capture_stderr() {
	println('=== Testing stderr capture ===')

	mut cap := os.Capture.new()

	cap.stdout_stderr_capture_start() or {
		eprintln('Failed to start capture: ${err}')
		return
	}

	test_error := 'This is stderr test message'
	eprintln(test_error)
	eprintln('Another stderr line')

	stdout, stderr := cap.stdout_stderr_capture_stop() or {
		eprintln('Failed to stop capture: ${err}')
		return
	}

	println('Captured stdout: "${stdout}"')
	println('Captured stderr: "${stderr}"')

	assert stderr.contains(test_error)
	assert stderr.contains('Another stderr line')
	assert stdout == ''
	println('✓ Stderr capture test passed')
	println('')
}

fn test_capture_both() {
	println('=== Testing both stdout and stderr capture ===')

	mut cap := os.Capture.new()

	cap.stdout_stderr_capture_start() or {
		eprintln('Failed to start capture: ${err}')
		return
	}

	stdout_msg := 'Standard output message'
	stderr_msg := 'Standard error message'

	println(stdout_msg)
	eprintln(stderr_msg)
	println('More stdout')
	eprintln('More stderr')

	stdout, stderr := cap.stdout_stderr_capture_stop() or {
		eprintln('Failed to stop capture: ${err}')
		return
	}

	println('Captured stdout: "${stdout}"')
	println('Captured stderr: "${stderr}"')

	assert stdout.contains(stdout_msg)
	assert stdout.contains('More stdout')
	assert stderr.contains(stderr_msg)
	assert stderr.contains('More stderr')
	assert !stdout.contains(stderr_msg)
	assert !stderr.contains(stdout_msg)
	println('✓ Both stdout and stderr capture test passed')
	println('')
}

fn test_capture_restoration() {
	println('=== Testing output restoration after capture ===')

	original_stdout_msg := 'Before capture - stdout'
	original_stderr_msg := 'Before capture - stderr'

	println(original_stdout_msg)
	eprintln(original_stderr_msg)

	mut cap := os.Capture.new()

	cap.stdout_stderr_capture_start() or {
		eprintln('Failed to start capture: ${err}')
		return
	}

	captured_stdout_msg := 'During capture - stdout'
	captured_stderr_msg := 'During capture - stderr'

	println(captured_stdout_msg)
	eprintln(captured_stderr_msg)

	stdout, stderr := cap.stdout_stderr_capture_stop() or {
		eprintln('Failed to stop capture: ${err}')
		return
	}

	after_capture_stdout_msg := 'After capture - stdout'
	after_capture_stderr_msg := 'After capture - stderr'

	println(after_capture_stdout_msg)
	eprintln(after_capture_stderr_msg)

	assert stdout.contains(captured_stdout_msg)
	assert stderr.contains(captured_stderr_msg)
	assert !stdout.contains(original_stdout_msg)
	assert !stderr.contains(original_stderr_msg)

	println('✓ Output restoration test passed')
	println('')
}
*/
