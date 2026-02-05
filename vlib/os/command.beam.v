// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

// BEAM backend implementation for Command.
// These provide placeholder implementations that compile to valid BEAM code.
// Real implementations would use Erlang's os:cmd or open_port.

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
pub fn start_new_command(cmd string) !Command {
	mut res := Command{
		path: cmd
	}
	res.start()!
	return res
}

// start will start the command.
pub fn (mut c Command) start() ! {
	// Placeholder - in real impl: would use os:cmd or open_port
	// For BEAM, this is a stub that doesn't actually run commands
}

// read_line returns a single line from the stdout of the running command.
pub fn (mut c Command) read_line() string {
	c.eof = true
	return ''
}

// close will close the pipe to the command, and wait for the command to finish.
pub fn (mut c Command) close() ! {
	c.exit_code = 0
}
