module os

import strings

// Command represents a running shell command, that the parent process
// wishes to monitor for output on its stdout pipe.
pub struct Command {
mut:
	f voidptr
pub mut:
	eof       bool
	exit_code int
pub:
	path            string
	redirect_stdout bool
}

// start_new_command will create a new os.Command, and start it right away.
// The command will represent a process running the passed shell command `cmd`,
// in a way that you can later call c.read_line() to intercept the new child process
// output line by line, streaming while the command is running.
// See also c.eof, c.read_line(), c.close() and c.exit_code .
@[deprecated: 'use os.start_new_command_args with an argument array to avoid shell injection']
pub fn start_new_command(cmd string) !Command {
	mut res := Command{
		path: cmd
	}
	res.start_shell()!
	return res
}

// start_new_command_args starts a program with literal arguments and merged output.
// Use read_line(), eof, close(), and exit_code to stream its output.
// Standard input is empty; use Process for interactive input.
pub fn start_new_command_args(args []string) !CommandArgs {
	if args.len == 0 {
		return error('start_new_command_args requires at least one argument')
	}
	filename := find_abs_path_of_executable(args[0])!
	mut process := new_process(filename)
	process.set_args(args[1..])
	process.expand_environment = false
	process.set_redirect_stdio_merged()
	process.set_stdin_path(if user_os() == 'windows' { 'NUL' } else { '/dev/null' })
	process.run()
	return CommandArgs{
		path:    args[0]
		process: process
	}
}

// start will start the command. Use start_new_command/1 instead.
@[deprecated: 'use os.start_new_command_args with an argument array to avoid shell injection']
@[manualfree]
pub fn (mut c Command) start() ! {
	c.start_shell()!
}

@[manualfree]
fn (mut c Command) start_shell() ! {
	pcmd := c.path + ' 2>&1'
	defer {
		unsafe { pcmd.free() }
	}
	c.f = vpopen(pcmd)
	if isnil(c.f) {
		return error('exec("${c.path}") failed')
	}
}

// read_line returns a single line from the stdout of the running command.
// Note: c.eof will be set to true, if the command ended while a line was read.
// The returned line will contain all of the accumulated output before the process ended.
// In practice, that often means, you will get a single '' and c.eof == true at the end.
@[manualfree]
pub fn (mut c Command) read_line() string {
	buf := [4096]u8{}
	mut res := strings.new_builder(1024)
	defer { unsafe { res.free() } }
	unsafe {
		bufbp := &u8(&buf[0])
		for C.fgets(&char(bufbp), 4096, c.f) != 0 {
			race_file_read()
			len := vstrlen(bufbp)
			for i in 0 .. len {
				if bufbp[i] == `\n` {
					res.write_ptr(bufbp, i)
					final := res.str()
					return final
				}
			}
			res.write_ptr(bufbp, len)
		}
	}
	$if race ? {
		// The end of the output is a read that did not fail too.
		if C.feof(c.f) != 0 {
			race_file_read()
		}
	}
	c.eof = true
	final := res.str()
	return final
}

// close will close the pipe to the command, and wait for the command to finish,
// then set .exit_code according to how its final process status.
pub fn (mut c Command) close() ! {
	c.exit_code = vpclose(c.f)
	c.f = unsafe { nil }
	if c.exit_code == 127 {
		return error_with_code('error', 127)
	}
}

// CommandArgs streams merged output from a program started with literal arguments.
pub struct CommandArgs {
mut:
	process &Process = unsafe { nil }
	pending string
pub mut:
	eof       bool
	exit_code int
pub:
	path string
}

// read_line waits for the next output line without its newline, setting eof when the pipe closes.
@[manualfree]
pub fn (mut c CommandArgs) read_line() string {
	for {
		if newline := c.pending.index('\n') {
			line := c.pending[..newline]
			c.pending = c.pending[newline + 1..]
			return line
		}
		mut chunk := ''
		$if windows {
			wdata := unsafe { &WProcess(c.process.wdata) }
			mut buf := [4096]u8{}
			mut bytes_read := u32(0)
			// ReadFile waits for data or a closed pipe; an empty PeekNamedPipe is not EOF.
			if C.ReadFile(wdata.child_stdout_read, &buf[0], u32(buf.len), voidptr(&bytes_read), 0) {
				chunk = decode_windows_captured_output(buf[..int(bytes_read)].bytestr())
			}
		} $else {
			chunk = c.process._read_from(.stdout)
		}
		if chunk.len == 0 {
			c.eof = true
			line := c.pending
			c.pending = ''
			return line
		}
		c.pending += chunk
	}
}

// close closes the output pipe, waits for the program, and records its exit code.
pub fn (mut c CommandArgs) close() ! {
	$if windows {
		// Closing the reader lets a writer exit even when output was not drained.
		mut wdata := unsafe { &WProcess(c.process.wdata) }
		close_valid_handle(&wdata.child_stdout_read)
	}
	c.process.close()
	c.process.wait()
	c.exit_code = c.process.code
	c.process.close()
}
