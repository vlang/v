module os

fn C._dup(fd int) int
fn C._dup2(fd1 int, fd2 int) int
fn C._pipe(fds &int, size u32, mode int) int
fn C.dup(fd int) int

const fd_stdout = $if windows { 1 } $else { C.STDOUT_FILENO }
const fd_stderr = $if windows { 2 } $else { C.STDERR_FILENO }

// fd_dup duplicates a file descriptor
pub fn fd_dup(fd int) int {
	return $if windows { C._dup(fd) } $else { C.dup(fd) }
}

// fd_dup2 duplicates file descriptor `fd1` to descriptor number `fd2`
// If `fd2` is already open, it is closed first before being reused.
// Returns the new file descriptor on success, or -1 on error.
pub fn fd_dup2(fd1 int, fd2 int) int {
	return $if windows { C._dup2(fd1, fd2) } $else { C.dup2(fd1, fd2) }
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
		if C._pipe(&fds[0], 0, 0) == -1 {
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
		fd_close(p.read_fd)
		p.read_fd = -1
	}
	if p.write_fd != -1 {
		fd_close(p.write_fd)
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

// slurp reads all data from the pipe until EOF
pub fn (mut p Pipe) slurp() []string {
	// Close write end to send EOF signal to the pipe
	fd_close(p.write_fd)
	p.write_fd = -1
	result := fd_slurp(p.read_fd)
	fd_close(p.read_fd)
	p.read_fd = -1
	return result
}

// IOCapture manages redirection of standard output and error streams
@[noinit]
pub struct IOCapture {
pub mut:
	stdout Pipe
	stderr Pipe
mut:
	original_stdout_fd int = -1
	original_stderr_fd int = -1
}

// stdio_capture starts capturing stdout and stderr by redirecting them to pipes
// Example:
// ```v
//    	mut cap := os.stdio_capture()!
//    	println('hello println')
//    	eprintln('hello eprintln')
//    	cap.stop()
//    	sout := cap.stdout.slurp()
//    	serr := cap.stderr.slurp()
//    	cap.close()
// ```
// or
// ```v
//    	mut cap := os.stdio_capture()!
//    	println('hello println')
//    	eprintln('hello eprintln')
//    	sout, serr := cap.finish()
// ```
pub fn stdio_capture() !IOCapture {
	mut c := IOCapture{}
	mut pipe_stdout := pipe()!
	mut pipe_stderr := pipe()!

	// Save original file descriptors
	c.original_stdout_fd = fd_dup(fd_stdout)
	c.original_stderr_fd = fd_dup(fd_stderr)

	// Redirect stdout to pipe
	if fd_dup2(pipe_stdout.write_fd, fd_stdout) == -1 {
		pipe_stdout.close()
		pipe_stderr.close()
		return error('Failed to redirect stdout')
	}

	// Redirect stderr to pipe
	if fd_dup2(pipe_stderr.write_fd, fd_stderr) == -1 {
		fd_dup2(c.original_stdout_fd, fd_stdout) // Restore stdout
		pipe_stdout.close()
		pipe_stderr.close()
		return error('Failed to redirect stderr')
	}

	// Close original write ends (duplicated by dup2)
	fd_close(pipe_stdout.write_fd)
	fd_close(pipe_stderr.write_fd)

	pipe_stdout.write_fd = -1
	pipe_stderr.write_fd = -1

	// Store pipes for later reading
	c.stdout = pipe_stdout
	c.stderr = pipe_stderr
	return c
}

// stop restores the original stdout and stderr streams
// This should be called to resume normal console output
pub fn (mut c IOCapture) stop() {
	// Restore original stdout
	if c.original_stdout_fd != -1 {
		fd_dup2(c.original_stdout_fd, fd_stdout)
		fd_close(c.original_stdout_fd)
		c.original_stdout_fd = -1
	}

	// Restore original stderr
	if c.original_stderr_fd != -1 {
		fd_dup2(c.original_stderr_fd, fd_stderr)
		fd_close(c.original_stderr_fd)
		c.original_stderr_fd = -1
	}
}

// close releases all resources associated with the capture
pub fn (mut c IOCapture) close() {
	c.stdout.close()
	c.stderr.close()
}

// finish stops capturing, reads all captured data, and releases resources
pub fn (mut c IOCapture) finish() ([]string, []string) {
	c.stop()
	stdout_str := c.stdout.slurp()
	stderr_str := c.stderr.slurp()
	c.close()
	return stdout_str, stderr_str
}
