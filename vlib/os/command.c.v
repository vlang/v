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
pub fn start_new_command(cmd string) !Command {
	mut res := Command{
		path: cmd
	}
	res.start()!
	return res
}

// start will start the command. Use start_new_command/1 instead.
@[manualfree]
pub fn (mut c Command) start() ! {
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
		bufbp := &buf[0]
		for C.fgets(&char(bufbp), 4096, c.f) != 0 {
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
