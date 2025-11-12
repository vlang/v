module os

import strings

$if windows {
	fn C._dup(fd int) int
	fn C._dup2(fd1 int, fd2 int) int
	fn C._pipe(fds &int, size u32, mode int) int
	fn C._close(fd int) int
} $else {
	fn C.dup(fd int) int
}

// Pipe represents a bidirectional communication channel
@[noinit]
pub struct Pipe {
mut:
	read_fd  int = -1
	write_fd int = -1
}

// pipe creates a new pipe for inter-process communication
pub fn pipe() !Pipe {
	mut fds := [2]int{}
	$if windows {
		if C._pipe(&fds[0], 4096, 0) == -1 {
			return error('Failed to create pipe')
		}
	} $else {
		if C.pipe(&fds[0]) == -1 {
			return error('Failed to create pipe')
		}
	}

	return Pipe{
		read_fd:  fds[0]
		write_fd: fds[1]
	}
}

// close closes the pipe and releases associated resources
pub fn (mut p Pipe) close() {
	if p.read_fd != -1 {
		$if windows {
			C._close(p.read_fd)
		} $else {
			C.close(p.read_fd)
		}
		p.read_fd = -1
	}
	if p.write_fd != -1 {
		$if windows {
			C._close(p.write_fd)
		} $else {
			C.close(p.write_fd)
		}
		p.write_fd = -1
	}
}

// read reads data from the pipe into the provided buffer
pub fn (p &Pipe) read(mut buffer []u8) !int {
	result := C.read(p.read_fd, buffer.data, buffer.len)
	if result == -1 {
		return error('Read failed')
	}
	return result
}

// write writes data from the buffer to the pipe
pub fn (p &Pipe) write(buffer []u8) !int {
	result := C.write(p.write_fd, buffer.data, buffer.len)
	if result == -1 {
		return error('Write failed')
	}
	return result
}

// Capture manages redirection of standard output and error streams
@[noinit]
pub struct Capture {
mut:
	stdout_pipe        &Pipe = unsafe { nil }
	stderr_pipe        &Pipe = unsafe { nil }
	original_stdout_fd int   = -1
	original_stderr_fd int   = -1
}

// Capture.new creates a new `Capture`
pub fn Capture.new() Capture {
	return Capture{}
}

// stdout_stderr_capture_start starts capturing stdout and stderr by redirecting them to pipes
pub fn (mut c Capture) stdout_stderr_capture_start() ! {
	mut pipe_stdout := pipe()!
	mut pipe_stderr := pipe()!
	$if windows {
		c.original_stdout_fd = C._dup(1) // Save stdout
		c.original_stderr_fd = C._dup(2) // Save stderr
		if C._dup2(pipe_stdout.write_fd, 1) == -1 {
			pipe_stdout.close()
			pipe_stderr.close()
			return error('Failed to redirect stdout')
		}
		if C._dup2(pipe_stderr.write_fd, 2) == -1 {
			C._dup2(c.original_stdout_fd, 1) // Restore stdout
			pipe_stdout.close()
			pipe_stderr.close()
			return error('Failed to redirect stderr')
		}
		// Close original write ends (duplicated by dup2)
		C._close(pipe_stdout.write_fd)
		C._close(pipe_stderr.write_fd)
	} $else {
		c.original_stdout_fd = C.dup(1) // Save stdout
		c.original_stderr_fd = C.dup(2) // Save stderr
		if C.dup2(pipe_stdout.write_fd, 1) == -1 {
			pipe_stdout.close()
			pipe_stderr.close()
			return error('Failed to redirect stdout')
		}
		if C.dup2(pipe_stderr.write_fd, 2) == -1 {
			C.dup2(c.original_stdout_fd, 1) // Restore stdout
			pipe_stdout.close()
			pipe_stderr.close()
			return error('Failed to redirect stderr')
		}
		// Close original write ends (duplicated by dup2)
		C.close(pipe_stdout.write_fd)
		C.close(pipe_stderr.write_fd)
	}

	pipe_stdout.write_fd = -1
	pipe_stderr.write_fd = -1

	// Store pipes for later reading
	c.stdout_pipe = &pipe_stdout
	c.stderr_pipe = &pipe_stderr
}

// stdout_stderr_capture_stop stops capturing and returns the captured stdout and stderr content
pub fn (mut c Capture) stdout_stderr_capture_stop() !(string, string) {
	mut stdout_result := strings.new_builder(1024)
	mut stderr_result := strings.new_builder(1024)

	$if windows {
		// Restore original stdout and stderr
		if c.original_stdout_fd != -1 {
			C._dup2(c.original_stdout_fd, 1)
			C._close(c.original_stdout_fd)
			c.original_stdout_fd = -1
		}

		if c.original_stderr_fd != -1 {
			C._dup2(c.original_stderr_fd, 2)
			C._close(c.original_stderr_fd)
			c.original_stderr_fd = -1
		}
	} $else {
		// Restore original stdout and stderr
		if c.original_stdout_fd != -1 {
			C.dup2(c.original_stdout_fd, 1)
			C.close(c.original_stdout_fd)
			c.original_stdout_fd = -1
		}

		if c.original_stderr_fd != -1 {
			C.dup2(c.original_stderr_fd, 2)
			C.close(c.original_stderr_fd)
			c.original_stderr_fd = -1
		}
	}

	// Read all available data from stdout pipe
	if !isnil(c.stdout_pipe) {
		mut buffer := []u8{len: 1024}
		for {
			n := c.stdout_pipe.read(mut buffer) or { break }
			if n == 0 {
				break
			}
			stdout_result.write(buffer[0..n])!
		}
		c.stdout_pipe.close()
		c.stdout_pipe = unsafe { nil }
	}

	// Read all available data from stderr pipe
	if !isnil(c.stderr_pipe) {
		mut buffer := []u8{len: 1024}
		for {
			n := c.stderr_pipe.read(mut buffer) or { break }
			if n == 0 {
				break
			}
			stderr_result.write(buffer[0..n])!
		}
		c.stderr_pipe.close()
		c.stderr_pipe = unsafe { nil }
	}

	return stdout_result.str(), stderr_result.str()
}
