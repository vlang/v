module main
import time
import os

// basic example which shows how to use the Command function

fn exec(args []string) string {
	mut out := ""
	mut line := ""
	mut line_err := ""
	mut cmd := os.new_process("/bin/bash")
	cmd.set_args(args)
	cmd.set_redirect_stdio()
	cmd.run()

	//to make sure we can see returncode and is_alive state
	time.sleep_ms(10)

	if cmd.is_alive() {
		for {

			// line = cmd.stdout_slurp() //I can't figure out what this does and difference with stdout_read			
			//if I use it it blocks

			line = cmd.stdout_read()
			println(line)

			line_err = cmd.stderr_read() //IF WE CALL STDERR_READ will block

			// we need a mechanism which allows us to check if stderr/stdout has data or it should never block

			// line_err = cmd.stderr_slurp()

			// println(line_err)
			out += line
			out += line_err
			if ! cmd.is_alive() {
				line = cmd.stdout_slurp()
				break
			}
		}
	}
	if cmd.pid > 0 {
		print(out)
		println("ERROR:")
		println(cmd)
		println(cmd.stderr_read())
	}
	return out
}

fn main() {
	mut out := ''
	// out = exec(["-c","find /"]) 
	out = exec(["-c","find /tmp/"]) //DOES NOT WORK???? why not, it works in execve?


	//TODO: 

	// out = exec("echo to stdout")
	// out = exec("echo to stderr 1>&2")
	// println("'$out'")
	// assert out == 'to stderr'
}
