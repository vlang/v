module main

import os

// Stream merged stdout and stderr from commands with literal arguments.

fn exec(args []string) string {
	mut out := ''
	mut line := ''
	mut cmd := os.start_new_command_args(args) or { panic(err) }
	defer {
		cmd.close() or { panic(err) }
		assert cmd.exit_code == 0
	}

	for {
		line = cmd.read_line()
		println(line)
		out += line
		if cmd.eof {
			return out
		}
	}
	return out
}

fn main() {
	exec(['find', os.temp_dir()])
	stdout := exec(['echo', 'to stdout'])
	assert stdout == 'to stdout'
	// Shell syntax is passed explicitly to a shell; CommandArgs merges both output streams.
	stderr := exec(['sh', '-c', 'echo to stderr >&2'])
	println("'${stderr}'")
	assert stderr == 'to stderr'
}
