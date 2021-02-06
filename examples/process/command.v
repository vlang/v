module main

import os

// basic example which shows how to use the Command function

fn exec(path string) string {
	mut out := ''
	mut line := ''
	mut cmd := os.Command{
		path: path
	}
	cmd.start() or { panic(err) }

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
	mut out := ''
	exec("bash -c 'find /tmp/'")
	out = exec('echo to stdout')
	out = exec('echo to stderr 1>&2')
	println("'$out'")
	// THIS DOES NOT WORK, is error, it goes to stderror of the command I run
	assert out == 'to stderr'
}
